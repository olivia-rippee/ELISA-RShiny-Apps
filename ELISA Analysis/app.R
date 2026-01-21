library(shiny)
library(tidyverse)
library(DT)
library(flextable)

# -------------------------------------------------
# Helper functions
# -------------------------------------------------
cv <- function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100

format_metric <- function(metric, x) {
  if (metric %in% c("Lower_ParmA","Upper_ParmA","Lower_ParmB","Upper_ParmB")) {
    formatC(x, format = "f", digits = 1)
  } else if (metric %in% c("CV_ParmA","CV_ParmB","CV_RP")) {
    paste0(formatC(x, format = "f", digits = 2), "%")
  } else if (metric == "SampleSize") {
    as.character(x)
  } else {
    formatC(x, format = "f", digits = 3)}}

conf_level <- 0.90
z_score <- qnorm(1 - (1 - conf_level) / 2)


make_ruggedness_table <- function(data, type = c("min","max")) {
  type <- match.arg(type)
  data %>%
    group_by(serial) %>%
    summarise(
      AvgParmA = mean(ParmA_ratio, na.rm = TRUE),
      StdevParmA = sd(ParmA_ratio, na.rm = TRUE),
      CV_ParmA = StdevParmA / AvgParmA * 100,
      
      AvgParmB = mean(ParmB_ratio, na.rm = TRUE),
      StdevParmB = sd(ParmB_ratio, na.rm = TRUE),
      CV_ParmB = StdevParmB / AvgParmB * 100,
      
      AvgRP   = mean(rp, na.rm = TRUE),
      StdevRP = sd(rp, na.rm = TRUE),
      CV_RP   = StdevRP / AvgRP * 100,
      
      SampleSize = sum(!is.na(ParmA_ratio) & !is.na(ParmB_ratio)),
      MarginError_ParmA = z_score * StdevParmA / sqrt(SampleSize),
      Lower_ParmA = AvgParmA - MarginError_ParmA,
      Upper_ParmA = AvgParmA + MarginError_ParmA,
      MarginError_ParmB = z_score * StdevParmB / sqrt(SampleSize),
      Lower_ParmB = AvgParmB - MarginError_ParmB,
      Upper_ParmB = AvgParmB + MarginError_ParmB,
      .groups = "drop") %>%
    pivot_longer(-serial, names_to="Metric", values_to="Value") %>%
    pivot_wider(names_from=serial, values_from=Value) %>%
    mutate(across(-Metric, ~ mapply(format_metric, Metric, .x))) %>%
    {ci_row <- tibble(Metric="CI", !!!setNames(rep(paste0(conf_level*100,"%"), ncol(.)-1), names(.)[-1]))
      bind_rows(., ci_row)}}


make_ruggedness_min_table <- function(data) {
  req(nrow(data) > 0)
  ruggedness_serials <- unique(data$serial)
  ruggedness_min_summary <- data %>%
    group_by(serial) %>%
    summarise(
      AvgParmA = mean(ParmA_ratio, na.rm = TRUE),
      StdevParmA = sd(ParmA_ratio, na.rm = TRUE),
      CV_ParmA = StdevParmA / AvgParmA * 100,
      AvgParmB = mean(ParmB_ratio, na.rm = TRUE),
      StdevParmB = sd(ParmB_ratio, na.rm = TRUE),
      CV_ParmB = StdevParmB / AvgParmB * 100,
      AvgRP   = mean(rp, na.rm = TRUE),
      StdevRP = sd(rp, na.rm = TRUE),
      CV_RP   = StdevRP / AvgRP * 100,
      SampleSize = sum(!is.na(ParmA_ratio) & !is.na(ParmB_ratio)),
      MarginError_ParmA = z_score * StdevParmA / sqrt(SampleSize),
      Lower_ParmA = AvgParmA - MarginError_ParmA,
      Upper_ParmA = AvgParmA + MarginError_ParmA,
      MarginError_ParmB = z_score * StdevParmB / sqrt(SampleSize),
      Lower_ParmB = AvgParmB - MarginError_ParmB,
      Upper_ParmB = AvgParmB + MarginError_ParmB,
      .groups = "drop")
  
  ruggedness_min <- ruggedness_min_summary %>%
    pivot_longer(-serial, names_to = "Metric", values_to = "Value") %>%
    pivot_wider(names_from = serial, values_from = Value)

  ruggedness_min <- ruggedness_min %>%
    mutate(across(-Metric, ~ case_when(
      Metric %in% c("Lower_ParmA","Upper_ParmA","Lower_ParmB","Upper_ParmB") ~ formatC(.x, format = "f", digits = 1),
      Metric %in% c("CV_ParmA","CV_ParmB","CV_RP") ~ paste0(formatC(.x, format = "f", digits = 2), "%"),
      Metric == "SampleSize" ~ as.character(.x),
      TRUE ~ formatC(.x, format = "f", digits = 3))))
  
  ci_row <- tibble(Metric = "CI",
    !!!setNames(rep(paste0(conf_level * 100, "%"), ncol(ruggedness_min) - 1), names(ruggedness_min)[-1]))
  
  bind_rows(ruggedness_min, ci_row) %>%
    arrange(factor(Metric, levels = c(
      "AvgParmA","StdevParmA","CV_ParmA",
      "AvgParmB","StdevParmB","CV_ParmB",
      "AvgRP","StdevRP","CV_RP", "SampleSize","CI",
      "MarginError_ParmA","Lower_ParmA","Upper_ParmA",
      "MarginError_ParmB","Lower_ParmB","Upper_ParmB")))}

make_ruggedness_max_table <- function(data) {
  req(nrow(data) > 0)
  ruggedness_serials <- unique(data$serial)
  ruggedness_max_summary <- data %>%
    group_by(serial) %>%
    summarise(
      AvgParmA = mean(ParmA_ratio, na.rm = TRUE),
      StdevParmA = sd(ParmA_ratio, na.rm = TRUE),
      CV_ParmA = StdevParmA / AvgParmA * 100,
      AvgParmB = mean(ParmB_ratio, na.rm = TRUE),
      StdevParmB = sd(ParmB_ratio, na.rm = TRUE),
      CV_ParmB = StdevParmB / AvgParmB * 100,
      AvgRP   = mean(rp, na.rm = TRUE),
      StdevRP = sd(rp, na.rm = TRUE),
      CV_RP   = StdevRP / AvgRP * 100,
      SampleSize = sum(!is.na(ParmA_ratio) & !is.na(ParmB_ratio)),
      MarginError_ParmA = z_score * StdevParmA / sqrt(SampleSize),
      Lower_ParmA = AvgParmA - MarginError_ParmA,
      Upper_ParmA = AvgParmA + MarginError_ParmA,
      MarginError_ParmB = z_score * StdevParmB / sqrt(SampleSize),
      Lower_ParmB = AvgParmB - MarginError_ParmB,
      Upper_ParmB = AvgParmB + MarginError_ParmB,
      .groups = "drop")
  
  ruggedness_max <- ruggedness_max_summary %>%
    pivot_longer(-serial, names_to = "Metric", values_to = "Value") %>%
    pivot_wider(names_from = serial, values_from = Value)
  
  ruggedness_max <- ruggedness_max %>%
    mutate(across(-Metric, ~ case_when(
      Metric %in% c("Lower_ParmA","Upper_ParmA","Lower_ParmB","Upper_ParmB") ~ formatC(.x, format = "f", digits = 1),
      Metric %in% c("CV_ParmA","CV_ParmB","CV_RP") ~ paste0(formatC(.x, format = "f", digits = 2), "%"),
      Metric == "SampleSize" ~ as.character(.x),
      TRUE ~ formatC(.x, format = "f", digits = 3))))
  
  ci_row <- tibble(Metric = "CI",
    !!!setNames(rep(paste0(conf_level * 100, "%"), ncol(ruggedness_max) - 1), names(ruggedness_max)[-1]))
  
  bind_rows(ruggedness_max, ci_row) %>%
    arrange(factor(Metric, levels = c(
      "AvgParmA","StdevParmA","CV_ParmA",
      "AvgParmB","StdevParmB","CV_ParmB",
      "AvgRP","StdevRP","CV_RP", "SampleSize","CI",
      "MarginError_ParmA","Lower_ParmA","Upper_ParmA",
      "MarginError_ParmB","Lower_ParmB","Upper_ParmB")))}


# -------------------------------------------------
# UI
# -------------------------------------------------
ui <- fluidPage(
  titlePanel("ELISA Analysis – Parallelism & Ruggedness"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("dilution_file", "Upload Dilution CSV"),
      fileInput("layout_file", "Upload Layout CSV"),
      fileInput("serialtesting_file", "Upload SerialTesting CSV"),
      actionButton("run", "Run Analysis", class = "btn-primary"),
      actionButton("clear", "Clear")),
    
    mainPanel(
      uiOutput("parallelism_ui"),
      hr(),
      uiOutput("ruggedness_ui"))))

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
    
    # Parallelism plates
    # -------------------
    parallelism_plates <- serial_testing %>%
      filter(grepl("Parallelism", plateID, TRUE),
             serial %in% c("SerA","SerB","PC")) %>%
      arrange(serialID)
    
    # Ruggedness plates
    # -------------------
    ruggedness_plates <- serial_testing %>%
      filter(grepl("min|max|ruggedness", plateID, TRUE),
             serial %in% c("120","SerB","PC")) %>%
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
      parallelism = parallelism_plates %>%
        left_join(start_dilutions, by = c("plateID","serialID","serial")) %>%
        mutate(dilution_group = paste0(serial, " ", Dilution)),
      ruggedness = ruggedness_plates,
      ruggedness_min = ruggedness_plates %>% filter(grepl("min", plateID, TRUE)),
      ruggedness_max = ruggedness_plates %>% filter(grepl("max", plateID, TRUE)))})
  
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
  # Ruggedness UI
  # -------------------------------------------------
  output$ruggedness_ui <- renderUI({
    req(input$run)
    
    df_min <- data_all()$ruggedness_min
    df_max <- data_all()$ruggedness_max
    
    min_has_temp <- nrow(df_min %>% filter(grepl("temp", plateID, ignore.case = TRUE))) > 0
    min_has_time <- nrow(df_min %>% filter(grepl("time", plateID, ignore.case = TRUE))) > 0
    
    max_has_temp <- nrow(df_max %>% filter(grepl("temp", plateID, ignore.case = TRUE))) > 0
    max_has_time <- nrow(df_max %>% filter(grepl("time", plateID, ignore.case = TRUE))) > 0
    
    tagList(
      h2("Ruggedness"),
      h3("All Plate IDs"),
      DTOutput("ruggedness_plate_ids"),
      hr(),
      h3("Ruggedness - All Plates"),
      DTOutput("ruggedness_all"),
      hr(),
      h3("Ruggedness – Min Plates"),
      if (min_has_temp) DTOutput("ruggedness_min_temp_table"),
      if (min_has_time) DTOutput("ruggedness_min_time_table"),
      if (!min_has_temp && !min_has_time) DTOutput("ruggedness_min_table"),
      hr(),
      h3("Ruggedness – Max Plates"),
      if (max_has_temp) DTOutput("ruggedness_max_temp_table"),
      if (max_has_time) DTOutput("ruggedness_max_time_table"),
      if (!max_has_temp && !max_has_time) DTOutput("ruggedness_max_table"))})
  
  
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
    
    datatable(df, options=list(dom="t", scrollX=TRUE), rownames = FALSE)})
  
  # -------------------------------------------------
  # Ruggedness tables
  # -------------------------------------------------
  output$ruggedness_all <- renderDT({
    df <- data_all()$ruggedness
    serials <- unique(df$serial)
    
    # Serial-level
    serial_summary <- df %>%
      group_by(serial) %>%
      summarise(
        AvgParmA = mean(ParmA_ratio, na.rm=TRUE),
        StdevParmA = sd(ParmA_ratio, na.rm=TRUE),
        CV_ParmA = StdevParmA / AvgParmA * 100,
        AvgParmB = mean(ParmB_ratio, na.rm=TRUE),
        StdevParmB = sd(ParmB_ratio, na.rm=TRUE),
        CV_ParmB = StdevParmB / AvgParmB * 100,
        AvgRP = mean(rp, na.rm=TRUE),
        StdevRP = sd(rp, na.rm=TRUE),
        CV_RP = StdevRP / AvgRP * 100,
        SampleSize = sum(!is.na(ParmA_ratio) & !is.na(ParmB_ratio)),
        MarginError_ParmA = z_score * StdevParmA / sqrt(SampleSize),
        Lower_ParmA = AvgParmA - MarginError_ParmA,
        Upper_ParmA = AvgParmA + MarginError_ParmA,
        MarginError_ParmB = z_score * StdevParmB / sqrt(SampleSize),
        Lower_ParmB = AvgParmB - MarginError_ParmB,
        Upper_ParmB = AvgParmB + MarginError_ParmB,
        .groups="drop")
    
    # Avg serial results
    avg_serials <- serial_summary %>%
      summarise(
        AvgParmA = mean(AvgParmA, na.rm=TRUE),
        StdevParmA = mean(StdevParmA, na.rm=TRUE),
        CV_ParmA = StdevParmA / AvgParmA * 100,
        AvgParmB = mean(AvgParmB, na.rm=TRUE),
        StdevParmB = mean(StdevParmB, na.rm=TRUE),
        CV_ParmB = StdevParmB / AvgParmB * 100,
        AvgRP = mean(AvgRP, na.rm=TRUE),
        StdevRP = mean(StdevRP, na.rm=TRUE),
        CV_RP = StdevRP / AvgRP * 100,
        SampleSize = sum(SampleSize),
        MarginError_ParmA = z_score * StdevParmA / sqrt(SampleSize),
        Lower_ParmA = AvgParmA - MarginError_ParmA,
        Upper_ParmA = AvgParmA + MarginError_ParmA,
        MarginError_ParmB = z_score * StdevParmB / sqrt(SampleSize),
        Lower_ParmB = AvgParmB - MarginError_ParmB,
        Upper_ParmB = AvgParmB + MarginError_ParmB) %>%
      pivot_longer(everything(), names_to="Metric", values_to="Avg_Serials")
    
    # All data (not by serial)
    all_plates <- df %>%
      summarise(
        AvgParmA = mean(ParmA_ratio, na.rm=TRUE),
        StdevParmA = sd(ParmA_ratio, na.rm=TRUE),
        CV_ParmA = StdevParmA / AvgParmA * 100,
        AvgParmB = mean(ParmB_ratio, na.rm=TRUE),
        StdevParmB = sd(ParmB_ratio, na.rm=TRUE),
        CV_ParmB = StdevParmB / AvgParmB * 100,
        AvgRP = mean(rp, na.rm=TRUE),
        StdevRP = sd(rp, na.rm=TRUE),
        CV_RP = StdevRP / AvgRP * 100,
        SampleSize = sum(!is.na(ParmA_ratio) & !is.na(ParmB_ratio)),
        MarginError_ParmA = z_score * StdevParmA / sqrt(SampleSize),
        Lower_ParmA = AvgParmA - MarginError_ParmA,
        Upper_ParmA = AvgParmA + MarginError_ParmA,
        MarginError_ParmB = z_score * StdevParmB / sqrt(SampleSize),
        Lower_ParmB = AvgParmB - MarginError_ParmB,
        Upper_ParmB = AvgParmB + MarginError_ParmB) %>%
      pivot_longer(everything(), names_to="Metric", values_to="All")
    
    tbl <- serial_summary %>%
      slice(match(serials, serial)) %>%
      pivot_longer(-serial, names_to="Metric", values_to="Value") %>%
      pivot_wider(names_from=serial, values_from=Value) %>%
      left_join(all_plates, by="Metric") %>%
      left_join(avg_serials, by="Metric")
    
    tbl <- tbl %>%
      mutate(across(-Metric, ~ mapply(format_metric, Metric, .x)))
    
    ci_row <- tibble(Metric="CI",
                     !!!setNames(rep(paste0(conf_level*100,"%"), ncol(tbl)-1), names(tbl)[-1]))
    tbl <- bind_rows(tbl, ci_row)
    
    datatable(tbl, options=list(dom="t", scrollX=TRUE), rownames=FALSE)})
  
  output$ruggedness_plate_ids <- renderDT({
    datatable(
      data_all()$ruggedness %>% distinct(plateID) %>% arrange(plateID),
      options = list(dom = "t"),
      rownames = FALSE)})
  
  output$ruggedness_min_table <- renderDT({
    df <- data_all()$ruggedness_min
    datatable(
      make_ruggedness_min_table(df),
      options = list(scrollX = TRUE, dom = "t"), 
      rownames=FALSE)})
  
  output$ruggedness_min_temp_table <- renderDT({
    df <- data_all()$ruggedness_min %>% filter(grepl("temp", plateID, ignore.case = TRUE))
    datatable(
      make_ruggedness_min_table(df),
      options = list(scrollX = TRUE, dom = "t"),
      rownames=FALSE,
      caption = "Min Temp")})
  
  output$ruggedness_min_time_table <- renderDT({
    df <- data_all()$ruggedness_min %>% filter(grepl("time", plateID, ignore.case = TRUE))
    datatable(
      make_ruggedness_min_table(df),
      options = list(scrollX = TRUE, dom = "t"),
      rownames=FALSE,
      caption = "Min Time")})
  
  output$ruggedness_max_table <- renderDT({
    df <- data_all()$ruggedness_max
    datatable(
      make_ruggedness_max_table(df),
      options = list(scrollX = TRUE, dom = "t"),
      rownames=FALSE)})
  
  output$ruggedness_max_temp_table <- renderDT({
    df <- data_all()$ruggedness_max %>% filter(grepl("temp", plateID, ignore.case = TRUE))
    datatable(
      make_ruggedness_max_table(df),
      options = list(scrollX = TRUE, dom = "t"),
      rownames=FALSE,
      caption = "Max Temp")})
  
  output$ruggedness_max_time_table <- renderDT({
    df <- data_all()$ruggedness_max %>% filter(grepl("time", plateID, ignore.case = TRUE))
    datatable(
      make_ruggedness_max_table(df),
      options = list(scrollX = TRUE, dom = "t"),
      rownames=FALSE,
      caption = "Max Time")})}


# -------------------------------------------------
# Run app
# -------------------------------------------------
shinyApp(ui, server)
