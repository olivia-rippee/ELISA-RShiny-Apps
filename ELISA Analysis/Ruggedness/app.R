library(shiny)
library(tidyverse)
library(DT)

# -------------------------------------------------
# Helper functions
# -------------------------------------------------
cv <- function(x) {sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100}

format_metric <- function(metric, x) {
  if (metric %in% c("Lower_ParmA","Upper_ParmA","Lower_ParmB","Upper_ParmB")) {
    formatC(x, format = "f", digits = 1)
  } else if (metric %in% c("CV_ParmA","CV_ParmB","CV_RP")) {
    paste0(formatC(x, format = "f", digits = 2), "%")
  } else if (metric == "SampleSize") {
    as.character(x)
  } else {
    formatC(x, format = "f", digits = 3)}}

make_ruggedness_min_table <- function(data, conf_level, z_score) {
  if (nrow(data) == 0) return(NULL)
  ruggedness_serials <- unique(data$serial)
  ruggedness_min_summary <- data %>%
    group_by(serial) %>%
    summarise(
      AvgParmA = mean(ParmA_ratio, na.rm = TRUE),
      StdevParmA = sd(ParmA_ratio, na.rm = TRUE),
      CV_ParmA = ifelse(AvgParmA == 0, NA, StdevParmA / AvgParmA * 100),
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

make_ruggedness_max_table <- function(data, conf_level, z_score) {
  if (nrow(data) == 0) return(NULL)
  ruggedness_serials <- unique(data$serial)
  ruggedness_max_summary <- data %>%
    group_by(serial) %>%
    summarise(
      AvgParmA = mean(ParmA_ratio, na.rm = TRUE),
      StdevParmA = sd(ParmA_ratio, na.rm = TRUE),
      CV_ParmA = ifelse(AvgParmA == 0, NA, StdevParmA / AvgParmA * 100),
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
  titlePanel("ELISA Analysis – Ruggedness"),
  fileInput("serialtesting_file", "Upload Serial Testing CSV"),
  
  radioButtons(
    "ruggedness_scope",
    "Ruggedness scope:",
    choices = c(
      "Only plates with ruggedness, min, or max in plateID" = "ruggedness_only",
      "All plates" = "all")),
  
  numericInput(
    "conf_level",
    "Confidence level:",
    value = 0.90,
    min = 0.80,
    max = 0.99,
    step = 0.01),
  uiOutput("conf_warning"),
  
  actionButton("run", "Run Analysis", class = "btn-primary"),
  actionButton("clear", "Clear"),
  hr(),
  
  conditionalPanel(
    condition = "input.run > 0",
    uiOutput("ruggedness_ui")))


# -------------------------------------------------
# Server
# -------------------------------------------------
server <- function(input, output, session) {
  
  # -----------------------------
  # Valid confidence level cap
  # -----------------------------
  output$conf_warning <- renderUI({
    req(input$conf_level)
    
    if (input$conf_level < 0.80 || input$conf_level > 0.99) {
      div(
        style = "color: #d9534f; font-size: 13px; margin-top: 4px; margin-bottom: 10px;",
        "Confidence level must be between 0.80 and 0.99")
    } else {NULL}})
  
  # -----------------------------
  # UI visibility toggle
  # -----------------------------
  show_output <- reactiveVal(FALSE)
  output$show_output <- reactive({show_output()})
  outputOptions(output, "show_output", suspendWhenHidden = FALSE)
  
  # Run -> show output
  observeEvent(input$run, {show_output(TRUE)})
  
  # Clear -> hide output
  observeEvent(input$clear, {show_output(FALSE)})
  
  # -----------------------------
  # Z-score reactive to confidence level
  # -----------------------------
  z_score <- reactive({
    req(input$conf_level)
    
    validate(
      need(input$conf_level >= 0.80 && input$conf_level <= 0.99,
           "Invalid confidence level"))
    
    qnorm(1 - (1 - input$conf_level) / 2)})
  
  # -------------------
  # Data 
  # -------------------
  data_all <- eventReactive(input$run, {
    req(input$serialtesting_file)
    
    # Ruggedness plates
    # -------------------
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
    
    ruggedness_plates <- serial_testing %>%
      {if (input$ruggedness_scope == "ruggedness_only") {
        filter(., grepl("min|max|ruggedness", plateID, ignore.case = TRUE),
               serial %in% c("120","SerB","PC"))
      } else {.}} %>%
      arrange(serialID)
    
    ruggedness_min <- ruggedness_plates %>% filter(grepl("min", plateID, ignore.case = TRUE))
    ruggedness_max <- ruggedness_plates %>% filter(grepl("max", plateID, ignore.case = TRUE))
    
    list(
      ruggedness = ruggedness_plates,
      ruggedness_min = ruggedness_min,
      ruggedness_max = ruggedness_max)})
  
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
        CV_ParmA = ifelse(AvgParmA == 0, NA, StdevParmA / AvgParmA * 100),
        AvgParmB = mean(ParmB_ratio, na.rm=TRUE),
        StdevParmB = sd(ParmB_ratio, na.rm=TRUE),
        CV_ParmB = StdevParmB / AvgParmB * 100,
        AvgRP = mean(rp, na.rm=TRUE),
        StdevRP = sd(rp, na.rm=TRUE),
        CV_RP = StdevRP / AvgRP * 100,
        SampleSize = sum(!is.na(ParmA_ratio) & !is.na(ParmB_ratio)),
        MarginError_ParmA = z_score() * StdevParmA / sqrt(SampleSize),
        Lower_ParmA = AvgParmA - MarginError_ParmA,
        Upper_ParmA = AvgParmA + MarginError_ParmA,
        MarginError_ParmB = z_score() * StdevParmB / sqrt(SampleSize),
        Lower_ParmB = AvgParmB - MarginError_ParmB,
        Upper_ParmB = AvgParmB + MarginError_ParmB,
        .groups="drop")
    
    # Avg serial results
    avg_serials <- serial_summary %>%
      summarise(
        AvgParmA = mean(AvgParmA, na.rm=TRUE),
        StdevParmA = mean(StdevParmA, na.rm=TRUE),
        CV_ParmA = ifelse(AvgParmA == 0, NA, StdevParmA / AvgParmA * 100),
        AvgParmB = mean(AvgParmB, na.rm=TRUE),
        StdevParmB = mean(StdevParmB, na.rm=TRUE),
        CV_ParmB = StdevParmB / AvgParmB * 100,
        AvgRP = mean(AvgRP, na.rm=TRUE),
        StdevRP = mean(StdevRP, na.rm=TRUE),
        CV_RP = StdevRP / AvgRP * 100,
        SampleSize = sum(SampleSize),
        MarginError_ParmA = z_score() * StdevParmA / sqrt(SampleSize),
        Lower_ParmA = AvgParmA - MarginError_ParmA,
        Upper_ParmA = AvgParmA + MarginError_ParmA,
        MarginError_ParmB = z_score() * StdevParmB / sqrt(SampleSize),
        Lower_ParmB = AvgParmB - MarginError_ParmB,
        Upper_ParmB = AvgParmB + MarginError_ParmB) %>%
      pivot_longer(everything(), names_to="Metric", values_to="Avg_Serials")
    
    # All data (not by serial)
    all_plates <- df %>%
      summarise(
        AvgParmA = mean(ParmA_ratio, na.rm=TRUE),
        StdevParmA = sd(ParmA_ratio, na.rm=TRUE),
        CV_ParmA = ifelse(AvgParmA == 0, NA, StdevParmA / AvgParmA * 100),
        AvgParmB = mean(ParmB_ratio, na.rm=TRUE),
        StdevParmB = sd(ParmB_ratio, na.rm=TRUE),
        CV_ParmB = StdevParmB / AvgParmB * 100,
        AvgRP = mean(rp, na.rm=TRUE),
        StdevRP = sd(rp, na.rm=TRUE),
        CV_RP = StdevRP / AvgRP * 100,
        SampleSize = sum(!is.na(ParmA_ratio) & !is.na(ParmB_ratio)),
        MarginError_ParmA = z_score() * StdevParmA / sqrt(SampleSize),
        Lower_ParmA = AvgParmA - MarginError_ParmA,
        Upper_ParmA = AvgParmA + MarginError_ParmA,
        MarginError_ParmB = z_score() * StdevParmB / sqrt(SampleSize),
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
                     !!!setNames(rep(paste0(input$conf_level*100,"%"), ncol(tbl)-1), names(tbl)[-1]))
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
      make_ruggedness_min_table(df, input$conf_level, z_score()),
      options = list(scrollX = TRUE, dom = "t"), 
      rownames=FALSE)})
  
  output$ruggedness_min_temp_table <- renderDT({
    df <- data_all()$ruggedness_min %>% filter(grepl("temp", plateID, ignore.case = TRUE))
    datatable(
      make_ruggedness_min_table(df, input$conf_level, z_score()),
      options = list(scrollX = TRUE, dom = "t"),
      rownames=FALSE,
      caption = "Min Temp")})
  
  output$ruggedness_min_time_table <- renderDT({
    df <- data_all()$ruggedness_min %>% filter(grepl("time", plateID, ignore.case = TRUE))
    datatable(
      make_ruggedness_min_table(df, input$conf_level, z_score()),
      options = list(scrollX = TRUE, dom = "t"),
      rownames=FALSE,
      caption = "Min Time")})
  
  output$ruggedness_max_table <- renderDT({
    df <- data_all()$ruggedness_max
    datatable(
      make_ruggedness_max_table(df, input$conf_level, z_score()),
      options = list(scrollX = TRUE, dom = "t"),
      rownames=FALSE)})
  
  output$ruggedness_max_temp_table <- renderDT({
    df <- data_all()$ruggedness_max %>% filter(grepl("temp", plateID, ignore.case = TRUE))
    datatable(
      make_ruggedness_max_table(df, input$conf_level, z_score()),
      options = list(scrollX = TRUE, dom = "t"),
      rownames=FALSE,
      caption = "Max Temp")})
  
  output$ruggedness_max_time_table <- renderDT({
    df <- data_all()$ruggedness_max %>% filter(grepl("time", plateID, ignore.case = TRUE))
    datatable(
      make_ruggedness_max_table(df, input$conf_level, z_score()),
      options = list(scrollX = TRUE, dom = "t"),
      rownames=FALSE,
      caption = "Max Time")})}


# -------------------------------------------------
# Run app
# -------------------------------------------------
shinyApp(ui, server)
