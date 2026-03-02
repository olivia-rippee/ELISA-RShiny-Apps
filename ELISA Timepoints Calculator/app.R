library(shiny)
library(lubridate)
library(dplyr)

ui <- fluidPage(
  titlePanel("ELISA Timepoints Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput("start_day", "Start Day:", value = Sys.Date()),
      textInput("start_time", "Start Time (HH:MM, 24-hour):", value = "08:00"),
      
      tags$br(),
      h4("Enter duration and transfer time for each step:"),
      
      do.call(tagList, lapply(
        c("Capture", "Block", "Antigen", "Detector", "Conjugate", "Substrate", "Stop/Read"),
        function(step) {
          tagList(
            tags$br(),
            h4(strong(step)),
            fluidRow(
              column(4, numericInput(paste0(step, "_hr"), "Hr:",
                                     value = ifelse(step == "Capture", 3,
                                             ifelse(step == "Block", 2,
                                             ifelse(step == "Antigen", 18,
                                             ifelse(step == "Detector", 3,
                                             ifelse(step == "Conjugate", 0,
                                             ifelse(step == "Substrate", 0, 0)))))),
                                     min = 0)),
              column(4, numericInput(paste0(step, "_min"), "Min:",
                                     value = ifelse(step == "Conjugate", 45,
                                                    ifelse(step == "Substrate", 10,
                                                           ifelse(step == "Stop/Read", 5, 0))),
                                     min = 0, max = 59)),
              if (!(step %in% c("Substrate", "Stop/Read")))
                column(4, numericInput(paste0(step, "_transfer"), "Transfer (min):",
                                       value = 0, min = 0))),
            tags$hr())})),
      
      actionButton("calc", "Calculate Timings", class = "btn-primary"),
      actionButton("clear", "Clear Table", class = "btn-secondary"),
      actionButton("refresh", "Refresh Page", class = "btn-secondary")),
    
    mainPanel(
      h3("Step Schedule"),
      tableOutput("schedule"))))

server <- function(input, output, session) {
  schedule_data <- reactiveVal(NULL)
  
  observeEvent(input$calc, {
    start_datetime <- as.POSIXct(
      paste(input$start_day, input$start_time),
      format = "%Y-%m-%d %H:%M",
      tz = Sys.timezone())
    
    steps <- c("Capture", "Block", "Antigen", "Detector", "Conjugate", "Substrate", "Stop/Read")
    
    durations_min <- sapply(steps,
                            function(s) input[[paste0(s, "_hr")]]*60 + input[[paste0(s, "_min")]])
    
    transfers_min <- sapply(steps,
                            function(s) {
                              transfer_val <- input[[paste0(s, "_transfer")]]
                              if (is.null(transfer_val)) 0 else transfer_val})
    
    df <- data.frame(Step = character(),
                     Duration = character(),
                     Start_Time = character(),
                     End_Time = character(),
                     stringsAsFactors = FALSE)
    
    current_time <- start_datetime
    
    for (i in seq_along(steps)) {
      step_name <- steps[i]
      step_duration <- durations_min[i]
      
      step_start <- current_time
      step_end   <- current_time + minutes(step_duration)
      
      df <- rbind(df,
        data.frame(
          Step = step_name,
          Duration = sprintf("%02d hr %02d min", step_duration %/% 60, step_duration %% 60),
          Start_Time = format(step_start, "%A, %d %b %Y @ %H:%M"),
          End_Time = format(step_end, "%A, %d %b %Y @ %H:%M"),
          stringsAsFactors = FALSE))
      
      current_time <- step_end + minutes(transfers_min[i])}
    
    schedule_data(df)})
  
  observeEvent(input$clear, {
    schedule_data(NULL)})
  
  observeEvent(input$refresh, {
    updateDateInput(session, "start_day", value = Sys.Date())
    updateTextInput(session, "start_time", value = "08:00")
    
    steps <- c("Capture", "Block", "Antigen", "Detector", "Conjugate", "Substrate", "Stop/Read")
    
    for (step in steps) {
      default_hr <- switch(step,
                           "Capture" = 3,
                           "Block" = 2,
                           "Antigen" = 18,
                           "Detector" = 3,
                           "Conjugate" = 0,
                           "Substrate" = 0,
                           "Stop/Read" = 0)
      updateNumericInput(session, paste0(step, "_hr"), value = default_hr)
      
      default_min <- switch(step,
                            "Conjugate" = 45,
                            "Substrate" = 10,
                            "Stop/Read" = 5,
                            0)
      updateNumericInput(session, paste0(step, "_min"), value = default_min)
      
      if (!(step %in% c("Substrate", "Stop/Read"))) {
        default_transfer <- 0
        updateNumericInput(session, paste0(step, "_transfer"), value = default_transfer)}}
    
    schedule_data(NULL)})
  
  output$schedule <- renderTable({
    schedule_data()},
  bordered = TRUE,
  striped = TRUE,
  hover = TRUE,
  sanitize.text.function = function(x) x)}


shinyApp(ui, server)
