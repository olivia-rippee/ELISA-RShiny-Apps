library(shiny)
library(tidyverse)
library(DT)

# -------------------------------------------------
# Helper functions
# -------------------------------------------------
format_metric <- function(metric, x) {
  if (metric %in% c("Lower_ParmA","Upper_ParmA","Lower_ParmB","Upper_ParmB")) {
    formatC(x, format = "f", digits = 1)
  } else if (metric %in% c("CV_ParmA","CV_ParmB","CV_RP")) {
    paste0(formatC(x, format = "f", digits = 2), "%")
  } else if (metric == "SampleSize") {
    as.character(x)
  } else {
    formatC(x, format = "f", digits = 3)}}

cv <- function(x) {
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100}

conf_level <- 0.90
z_score <- qnorm(1 - (1 - conf_level) / 2)


# -------------------------------------------------
# UI
# -------------------------------------------------
ui <- fluidPage(
  titlePanel("ELISA Analysis – Parallelism"),
  fileInput("dilution_file", "Upload Dilution CSV"),
  fileInput("layout_file", "Upload Layout CSV"),
  fileInput("serialtesting_file", "Upload Serial Testing CSV"),
  radioButtons(
    "parallelism_scope",
    "Parallelism scope:",
    choices = c(
      "Only plates with parallelism in plateID" = "parallelism_only",
      "All plates" = "all")),
  actionButton("run", "Run Analysis", class = "btn-primary"),
  actionButton("clear", "Clear"),
  hr(),
  conditionalPanel(
    condition = "input.run > 0",
    uiOutput("parallelism_ui")))


# -------------------------------------------------
# Server
# -------------------------------------------------
server <- function(input, output, session) {
  
  observeEvent(input$clear, session$reload())
  
  # -------------------
  # Data 
  # -------------------
  data_all <- eventReactive(input$run, {
    req(input$serialtesting_file, input$dilution_file, input$layout_file)
      
    serial_testing <- read.csv(input$serialtesting_file$datapath, stringsAsFactors = FALSE)
    
    serial_testing <- serial_testing %>%
      mutate(
        serial = case_when(
          grepl("-120", serialID, TRUE) ~ "120",
          grepl("SERA", serialID, TRUE) ~ "SerA",
          grepl("SERB", serialID, TRUE) ~ "SerB",
          grepl("PC",   serialID, TRUE) ~ "PC",
          grepl("NC",   serialID, TRUE) ~ "NC",
          grepl("MR",   serialID, TRUE) ~ "MR",
          TRUE ~ NA_character_),
        serial = factor(serial, levels = c("120","SerA","SerB","PC","NC","MR")))
    
    parallelism_plates <- serial_testing %>%
      {if (input$parallelism_scope == "parallelism_only") {
        filter(., grepl("Parallelism", plateID, ignore.case = TRUE),
               serial %in% c("SerA","SerB","PC"))
      } else {.}} %>%
      arrange(serialID)
    
    # Dilution + layout
    # -------------------
    dilution <- read.csv(input$dilution_file$datapath, stringsAsFactors = FALSE)
    layout   <- read.csv(input$layout_file$datapath, stringsAsFactors = FALSE)
    
    dilution <- dilution %>%
      group_by(plateID) %>%
      mutate(RowLetter = LETTERS[row_number()]) %>%
      ungroup()
    
    layout <- layout %>%
      group_by(plateID) %>%
      mutate(RowLetter = LETTERS[row_number()]) %>%
      ungroup()
    
    dilution_long <- dilution %>%
      pivot_longer(starts_with("X"), names_to = "Column", values_to = "Dilution") %>%
      mutate(Column = as.integer(sub("X", "", Column)))
    
    layout_long <- layout %>%
      pivot_longer(starts_with("X"), names_to = "Column", values_to = "serialID") %>%
      mutate(Column = as.integer(sub("X", "", Column)))
    
    well_mapping <- layout_long %>%
      left_join(dilution_long, by = c("plateID","RowLetter","Column")) %>%
      mutate(
        serial = case_when(
          grepl("-120", serialID, TRUE) ~ "120",
          grepl("SERA", serialID, TRUE) ~ "SerA",
          grepl("SERB", serialID, TRUE) ~ "SerB",
          grepl("PC",   serialID, TRUE) ~ "PC",
          grepl("NC",   serialID, TRUE) ~ "NC",
          grepl("MR",   serialID, TRUE) ~ "MR",
          TRUE ~ NA_character_),
        serial = factor(serial, levels = c("120","SerA","SerB","PC","NC","MR")))
    
    start_dilutions <- well_mapping %>%
      group_by(plateID, serialID) %>%
      slice_min(Dilution, n = 1, with_ties = FALSE) %>%
      ungroup()
    
    list(
      parallelism = if (!is.null(parallelism_plates)) {
        parallelism_plates %>%
          left_join(start_dilutions, by = c("plateID","serialID","serial")) %>%
          mutate(dilution_group = paste0(serial, " ", Dilution))
      } else NULL)})
  
  # -------------------------------------------------
  # Parallelism UI
  # -------------------------------------------------
  output$parallelism_ui <- renderUI({
    req(input$run)
    tagList(
      h2("Parallelism"),
      h3("All Plate IDs"),
      DTOutput("parallelism_plate_ids"),
      hr(),
      h3("Parallelism - All Starting Dilutions"),
      DTOutput("parallelism_all"),
      br(),
      h3("Parallelism - Combined Serial A + Serial B"),
      DTOutput("parallelism_combined"),
      br(),
      h3("Parallelism - By Serial and Starting Dilution"),
      DTOutput("parallelism_by_serial"))})
  
  # -------------------------------------------------
  # Parallelism tables
  # -------------------------------------------------
  output$parallelism_plate_ids <- renderDT({
    datatable(
      data_all()$parallelism %>% distinct(plateID) %>% arrange(plateID),
      options = list(dom = "t"),
      rownames = FALSE)})
  
  output$parallelism_all <- renderDT({
    df <- data_all()$parallelism
    serials <- unique(df$serial)
    tbl <- df %>%
      group_by(serial) %>%
      summarise(
        AvgParmA = mean(ParmA_ratio, na.rm=TRUE),
        StdevParmA = sd(ParmA_ratio, na.rm=TRUE),
        CV_ParmA = cv(ParmA_ratio),
        AvgParmB = mean(ParmB_ratio, na.rm=TRUE),
        StdevParmB = sd(ParmB_ratio, na.rm=TRUE),
        CV_ParmB = cv(ParmB_ratio),
        AvgRP = mean(rp, na.rm=TRUE),
        StdevRP = sd(rp, na.rm=TRUE),
        CV_RP = cv(rp),
        SampleSize = sum(!is.na(ParmA_ratio) & !is.na(ParmB_ratio)),
        MarginError_ParmA = z_score * StdevParmA / sqrt(SampleSize),
        Lower_ParmA = AvgParmA - MarginError_ParmA,
        Upper_ParmA = AvgParmA + MarginError_ParmA,
        MarginError_ParmB = z_score * StdevParmB / sqrt(SampleSize),
        Lower_ParmB = AvgParmB - MarginError_ParmB,
        Upper_ParmB = AvgParmB + MarginError_ParmB,
        .groups="drop") %>%
      pivot_longer(-serial, names_to="Metric", values_to="Value") %>%
      pivot_wider(names_from=serial, values_from=Value) %>%
      mutate(across(-Metric, ~ mapply(format_metric, Metric, .x))) %>%
      bind_rows(tibble(Metric="CI", !!!setNames(rep(paste0(conf_level*100,"%"), length(serials)), serials)))
    
    datatable(tbl, options=list(dom="t", scrollX=TRUE), rownames = FALSE)})
  
  output$parallelism_combined <- renderDT({
    df <- data_all()$parallelism %>% filter(serial %in% c("SerA","SerB"))
    detected_dilutions <- sort(unique(df$Dilution))
    
    summarize_block <- function(x) {
      SampleSize <- sum(!is.na(x$ParmA_ratio) & !is.na(x$ParmB_ratio))
      AvgParmA <- mean(x$ParmA_ratio, na.rm=TRUE)
      StdevParmA <- sd(x$ParmA_ratio, na.rm=TRUE)
      AvgParmB <- mean(x$ParmB_ratio, na.rm=TRUE)
      StdevParmB <- sd(x$ParmB_ratio, na.rm=TRUE)
      tibble(
        AvgParmA, StdevParmA, CV_ParmA=cv(x$ParmA_ratio),
        AvgParmB, StdevParmB, CV_ParmB=cv(x$ParmB_ratio),
        SampleSize,
        MarginError_ParmA = z_score*StdevParmA/sqrt(SampleSize),
        Lower_ParmA = AvgParmA - z_score*StdevParmA/sqrt(SampleSize),
        Upper_ParmA = AvgParmA + z_score*StdevParmA/sqrt(SampleSize),
        MarginError_ParmB = z_score*StdevParmB/sqrt(SampleSize),
        Lower_ParmB = AvgParmB - z_score*StdevParmB/sqrt(SampleSize),
        Upper_ParmB = AvgParmB + z_score*StdevParmB/sqrt(SampleSize))}
    
    blocks <- c(list("SerA+SerB" = summarize_block(df)),
        setNames(lapply(detected_dilutions, 
           function(d) summarize_block(filter(df, Dilution == d))),
           paste0("SerA+SerB ", detected_dilutions)))
    
    tbl <- bind_cols(Metric = names(blocks[[1]]),
           lapply(blocks, unlist)) %>%
      mutate(across(-Metric, ~ mapply(format_metric, Metric, .x))) %>%
      bind_rows(tibble(Metric="CI", 
                       !!!setNames(rep(paste0(conf_level*100,"%"), ncol(.)-1), names(.)[-1])))
    
    datatable(tbl, options=list(dom="t", scrollX=TRUE), rownames = FALSE)})
  
  output$parallelism_by_serial <- renderDT({
    df <- data_all()$parallelism %>%
      filter(serial %in% c("SerA","SerB")) %>%
      group_by(serial, Dilution) %>%
      summarise(
        AvgParmA = mean(ParmA_ratio, na.rm=TRUE),
        StdevParmA = sd(ParmA_ratio, na.rm=TRUE),
        CV_ParmA = cv(ParmA_ratio),
        AvgParmB = mean(ParmB_ratio, na.rm=TRUE),
        StdevParmB = sd(ParmB_ratio, na.rm=TRUE),
        CV_ParmB = cv(ParmB_ratio),
        AvgRP = mean(rp, na.rm=TRUE),
        StdevRP = sd(rp, na.rm=TRUE),
        CV_RP = cv(rp),
        SampleSize = sum(!is.na(ParmA_ratio) & !is.na(ParmB_ratio)),
        MarginError_ParmA = z_score * StdevParmA / sqrt(SampleSize),
        Lower_ParmA = AvgParmA - MarginError_ParmA,
        Upper_ParmA = AvgParmA + MarginError_ParmA,
        MarginError_ParmB = z_score * StdevParmB / sqrt(SampleSize),
        Lower_ParmB = AvgParmB - MarginError_ParmB,
        Upper_ParmB = AvgParmB + z_score*StdevParmB/sqrt(SampleSize),
        .groups="drop") %>%
      unite(Group, serial, Dilution, sep=" ") %>%
      pivot_longer(-Group, names_to="Metric", values_to="Value") %>%
      pivot_wider(names_from=Group, values_from=Value) %>%
      mutate(across(-Metric, ~ mapply(format_metric, Metric, .x))) %>%
      bind_rows(tibble(Metric="CI", !!!setNames(rep(paste0(conf_level*100,"%"), ncol(.)-1), names(.)[-1])))
    
    datatable(df, options=list(dom="t", scrollX=TRUE), rownames = FALSE)})}

# -------------------------------------------------
# Run app
# -------------------------------------------------
shinyApp(ui, server)
