library(shiny)
library(lubridate)
library(dplyr)

# Step sequences
step_sequences <- list(
  Capture = c("Capture", "Block", "Antigen", "Detector", "Conjugate", "Substrate", "Stop/Read"),
  `Direct Bind` = c("Antigen", "Block", "Antibody", "Conjugate", "Substrate", "Stop/Read"),
  `CpG Hybridization` = c("Capture", "Block", "Detector+Antigen", "Conjugate", "Substrate", "Stop/Read"))

# Default durations (hr, min, transfer)
default_durations <- list(
  
  Capture = list(
    Capture = c(hr = 3, min = 0, transfer = 0),
    Block = c(hr = 2, min = 0, transfer = 0),
    Antigen = c(hr = 18, min = 0, transfer = 0),
    Detector = c(hr = 3, min = 0, transfer = 0),
    Conjugate = c(hr = 0, min = 45, transfer = 0),
    Substrate = c(hr = 0, min = 10, transfer = NA),
    `Stop/Read` = c(hr = 0, min = 5, transfer = NA)),
  
  `Direct Bind` = list(
    Antigen = c(hr = 21, min = 0, transfer = 0),
    Block = c(hr = 1, min = 0, transfer = 0),
    Antibody = c(hr = 4, min = 0, transfer = 0),
    Conjugate = c(hr = 1, min = 0, transfer = 0),
    Substrate = c(hr = 0, min = 10, transfer = NA),
    `Stop/Read` = c(hr = 0, min = 5, transfer = NA)),
  
  `CpG Hybridization` = list(
    Capture = c(hr = 19, min = 0, transfer = 0),
    Block = c(hr = 2, min = 0, transfer = 0),
    `Detector+Antigen` = c(hr = 1, min = 30, transfer = 0),
    Conjugate = c(hr = 1, min = 0, transfer = 0),
    Substrate = c(hr = 0, min = 40, transfer = NA),
    `Stop/Read` = c(hr = 0, min = 5, transfer = NA)))

ui <- fluidPage(
  titlePanel("ELISA Timepoints Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput("start_day", "Start Day:", value = Sys.Date()),
      textInput("start_time", "Start Time (HH:MM, 24-hour):", value = "08:00"),
      
      tags$br(),
      h4("Enter duration and transfer time for each step:"),
      
      # Correct tabsetPanel with proper list of tabPanel()
      do.call(tabsetPanel,
              c(list(id = "tab_steps"),
                lapply(names(step_sequences), function(tab_name) {
                  tabPanel(tab_name,
                           do.call(tagList, lapply(step_sequences[[tab_name]], function(step) {
                             tagList(
                               tags$br(),
                               h4(strong(step)),
                               fluidRow(
                                 column(4, numericInput(paste0(tab_name, "_", step, "_hr"), "Hr:",
                                                        value = default_durations[[tab_name]][[step]]["hr"],
                                                        min = 0)),
                                 column(4, numericInput(paste0(tab_name, "_", step, "_min"), "Min:",
                                                        value = default_durations[[tab_name]][[step]]["min"],
                                                        min = 0, max = 59)),
                                 if (!step %in% c("Substrate", "Stop/Read"))
                                   column(4, numericInput(paste0(tab_name, "_", step, "_transfer"), "Transfer (min):",
                                                          value = default_durations[[tab_name]][[step]]["transfer"],
                                                          min = 0))),
                               tags$hr())})))}))),
    
    actionButton("calc", "Calculate Timings", class = "btn-primary"),
    actionButton("clear", "Clear Table", class = "btn-secondary"),
    actionButton("refresh", "Refresh Page", class = "btn-secondary")),
  
  mainPanel(
    h3("Step Schedule"),
    tableOutput("schedule"))))

server <- function(input, output, session) {
  # Independent reactiveVals for each module
  schedule_data <- reactiveValues(
    Capture = NULL, `Direct Bind` = NULL, `CpG Hybridization` = NULL)
  
  observeEvent(input$calc, {
    current_tab <- input$tab_steps
    steps <- step_sequences[[current_tab]]
    
    # Compute durations in minutes
    durations_min <- sapply(steps, function(s) {
      hr <- input[[paste0(current_tab, "_", s, "_hr")]]
      min <- input[[paste0(current_tab, "_", s, "_min")]]
      hr*60 + min})
    
    transfers_min <- sapply(steps, function(s) {
      tr <- input[[paste0(current_tab, "_", s, "_transfer")]]
      if (is.null(tr)) 0 else tr})
    
    start_datetime <- as.POSIXct(
      paste(input$start_day, input$start_time),
      format = "%Y-%m-%d %H:%M",
      tz = Sys.timezone())
    
    df <- data.frame(Step=character(),
                     Duration=character(),
                     Start_Time=character(),
                     End_Time=character(),
                     stringsAsFactors=FALSE)
    
    current_time <- start_datetime
    
    for (i in seq_along(steps)) {
      step_name <- steps[i]
      step_duration <- durations_min[i]
      
      step_start <- current_time
      step_end <- current_time + minutes(step_duration)
      
      df <- rbind(df,
                  data.frame(
                    Step = step_name,
                    Duration = sprintf("%02d hr %02d min", step_duration %/% 60, step_duration %% 60),
                    Start_Time = format(step_start, "%A, %d %b %Y @ %H:%M"),
                    End_Time = format(step_end, "%A, %d %b %Y @ %H:%M"),
                    stringsAsFactors = FALSE))
      
      current_time <- step_end + minutes(transfers_min[i])}
    
    schedule_data[[current_tab]] <- df})
  
  observeEvent(input$clear, {
    current_tab <- input$tab_steps
    schedule_data[[current_tab]] <- NULL})
  
  observeEvent(input$refresh, {
    updateDateInput(session, "start_day", value = Sys.Date())
    updateTextInput(session, "start_time", value = "08:00")
    
    # Reset defaults for all tabs independently
    for (tab_name in names(step_sequences)) {
      for (step in step_sequences[[tab_name]]) {
        updateNumericInput(session, paste0(tab_name, "_", step, "_hr"), 
                           value = default_durations[[tab_name]][[step]]["hr"])
        updateNumericInput(session, paste0(tab_name, "_", step, "_min"), 
                           value = default_durations[[tab_name]][[step]]["min"])
        if (!step %in% c("Substrate", "Stop/Read")) {
          updateNumericInput(session, paste0(tab_name, "_", step, "_transfer"), 
                             value = default_durations[[tab_name]][[step]]["transfer"])}}}
    
    # Clear all schedules
    for (tab_name in names(step_sequences)) {
      schedule_data[[tab_name]] <- NULL}})
  
  output$schedule <- renderTable({
    current_tab <- input$tab_steps
    schedule_data[[current_tab]]}, bordered = TRUE, striped = TRUE, hover = TRUE,
  sanitize.text.function = function(x) x)}

shinyApp(ui, server)
