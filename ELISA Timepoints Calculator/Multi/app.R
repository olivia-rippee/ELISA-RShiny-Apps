library(shiny)
library(lubridate)
library(dplyr)
library(DT)

# ------------------
# Step Definitions
# ------------------
step_sequences <- list(
  Capture = c("Capture", "Block", "Antigen", "Detector", "Conjugate", "Substrate", "Stop/Read"),
  `Direct Bind` = c("Antigen", "Block", "Antibody", "Conjugate", "Substrate", "Stop/Read"),
  `CpG Hybridization` = c("Capture", "Block", "Thermocycle", "Detector+Antigen", "Conjugate", "Substrate", "Stop/Read"))

# Default durations
default_durations <- list(
  Capture = list(
    Capture = c(hr=3, min=0, transfer=0),
    Block = c(hr=2, min=0, transfer=0),
    Antigen = c(hr=18, min=0, transfer=0),
    Detector = c(hr=3, min=0, transfer=0),
    Conjugate = c(hr=0, min=45, transfer=0),
    Substrate = c(hr=0, min=10, transfer=NA),
    `Stop/Read` = c(hr=0, min=5, transfer=NA)),
  `Direct Bind` = list(
    Antigen = c(hr=21, min=0, transfer=0),
    Block = c(hr=1, min=0, transfer=0),
    Antibody = c(hr=4, min=0, transfer=0),
    Conjugate = c(hr=1, min=0, transfer=0),
    Substrate = c(hr=0, min=10, transfer=NA),
    `Stop/Read` = c(hr=0, min=5, transfer=NA)),
  `CpG Hybridization` = list(
    Capture = c(hr=19, min=0, transfer=0),
    Block = c(hr=2, min=0, transfer=0),
    Thermocycle = c(hr=0, min=30, transfer=NA),
    `Detector+Antigen` = c(hr=1, min=30, transfer=0),
    Conjugate = c(hr=1, min=0, transfer=0),
    Substrate = c(hr=0, min=40, transfer=NA),
    `Stop/Read` = c(hr=0, min=5, transfer=NA)))

# ------------------
# UI
# ------------------
ui <- fluidPage(
  titlePanel("Multi-ELISA Scheduler"),
  sidebarLayout(
    sidebarPanel(
      numericInput("num_assays", "Number of assays:", 1, min=1, max=10),
      uiOutput("assay_sections"),
      actionButton("calc", "Calculate", class="btn-primary"),
      actionButton("clear", "Clear Table", class="btn-secondary"),
      actionButton("refresh", "Refresh Page", class = "btn-secondary"),
      br(),br(),
      downloadButton("downloadICS", "Export to Outlook")),
    mainPanel(DTOutput("schedule"))),
  tags$script(HTML("
  Shiny.addCustomMessageHandler('refreshPage', function(message) {
    location.reload();});")))

# ------------------
# Server
# ------------------
server <- function(input, output, session) {
  
  schedule_data <- reactiveVal(NULL)
  
  # Generate sections for each assay
  # ---------------------------------
  output$assay_sections <- renderUI({
    lapply(1:input$num_assays, function(i){
      assay_type_choices <- names(step_sequences)
      tagList(
        tags$hr(),tags$br(),
        tags$h2(paste("Assay", i), style="color:#1F3B5F; margin-bottom:15px;"),
        textInput(paste0("assay_name_", i),"Assay label:", value=paste("Assay", i)),
        selectInput(paste0("assay_type_", i),"Assay type:",
                    choices=assay_type_choices, selected=assay_type_choices[1]),
        dateInput(paste0("start_date_", i),"Start date:", value=Sys.Date()),
        textInput(paste0("start_time_", i),"Start time (HH:MM):", value="08:00"),
        uiOutput(paste0("custom_times_", i)))})})
  
  # Custom time inputs per assay
  # -----------------------------
  observe({
    for(i in 1:max(1,input$num_assays)){
      local({ii <- i
        output[[paste0("custom_times_", ii)]] <- renderUI({
          assay_type <- input[[paste0("assay_type_", ii)]]
          if(is.null(assay_type)) return(NULL)
          steps <- step_sequences[[assay_type]]
          tagList(
            lapply(steps, function(step){
              tagList(tags$div(tags$h4(
                    style="margin-top:15px; margin-bottom:5px; color:#2C3E50; font-weight:bold;", 
                    step),
                  fluidRow(
                    column(4, numericInput(paste0("hr_", ii,"_", step), "Hr",
                                           value=default_durations[[assay_type]][[step]]["hr"], min=0)),
                    column(4, numericInput(paste0("min_", ii,"_", step), "Min",
                                           value=default_durations[[assay_type]][[step]]["min"], min=0, max=59)),
                    if(!step %in% c("Substrate",  "Stop/Read"))
                      column(4, numericInput(paste0("transfer_", ii,"_", step), "Transfer (min)",
                                             value=default_durations[[assay_type]][[step]]["transfer"], min=0)))))}))})})}})
  
  # Calculate schedule
  # -------------------
  observeEvent(input$calc,{
    all_steps <- data.frame()
    
    for(i in 1:input$num_assays){
      assay_label <- input[[paste0("assay_name_", i)]]
      assay_type <- input[[paste0("assay_type_", i)]]
      steps <- step_sequences[[assay_type]]
      current_time <- as.POSIXct(paste(input[[paste0("start_date_", i)]],
                                       input[[paste0("start_time_", i)]]),format="%Y-%m-%d %H:%M")
      
      for(s in steps){
        hr <- input[[paste0("hr_", i,"_", s)]]
        mn <- input[[paste0("min_", i,"_", s)]]
        tr <- input[[paste0("transfer_", i,"_", s)]]
        if(is.null(tr)) tr <- 0
        
        duration_min <- hr*60 + mn
        step_start <- current_time
        step_end <- current_time + minutes(duration_min)
        
        all_steps <- rbind(all_steps, data.frame(
          AssayLabel=assay_label,
          Step=s,
          StartTime=step_start,
          EndTime=step_end,
          AssayNumber=i))
        
        current_time <- step_end + minutes(tr)}}
    
    # Overlap detection: start times within 15 minutes, comparing across assays only
    # ------------------
    overlaps <- sapply(1:nrow(all_steps), function(i){
      other_steps <- all_steps[all_steps$AssayNumber != all_steps$AssayNumber[i], ]
      any(abs(difftime(all_steps$StartTime[i], other_steps$StartTime, units="mins")) < 15)})
    all_steps$Overlap <- ifelse(overlaps,"⚠️",  "")
    
    # Format for table
    all_steps$Start <- format(all_steps$StartTime, "%A, %d %b %Y @ %H:%M")
    all_steps$End   <- format(all_steps$EndTime,   "%A, %d %b %Y @ %H:%M")
    
    schedule_data(all_steps)})
  
  # Clear table
  # ------------------
  observeEvent(input$clear,{
    schedule_data(NULL)})
  
  # Refresh page
  # ------------------
  observeEvent(input$refresh, {
    session$sendCustomMessage("refreshPage", list())})
  
  # Render table with colored conflicts
  # ------------------------------------
  output$schedule <- renderDT({
    df <- schedule_data()
    if(is.null(df)) return(NULL)
    
    datatable(
      df[,c("AssayLabel", "Step", "Start", "End", "Overlap")],
      options = list(pageLength = 15, dom = 't', ordering = FALSE),
      rownames = FALSE,
      class = "table table-bordered table-striped table-hover") %>%
      formatStyle("Overlap", target = "row",
        backgroundColor = styleEqual("⚠️", "#F9D6D5"))})

  # Export ICS
  # -----------
  output$downloadICS <- downloadHandler(
    filename=function(){paste0("ELISA_schedule_", Sys.Date(), ".ics")},
    content=function(file){
      df <- schedule_data()
      if(is.null(df)) return(NULL)
      
      ics_lines <- c("BEGIN:VCALENDAR", "VERSION:2.0", "PRODID:-//YourApp//ELISA Scheduler//EN")
      
      for(i in 1:nrow(df)){
        event_name <- paste0(df$AssayLabel[i]," - ", df$Step[i])
        start_utc <- format(df$StartTime[i], "%Y%m%dT%H%M%S")
        end_utc <- format(df$StartTime[i] + minutes(30), "%Y%m%dT%H%M%S")
        ics_event <- c("BEGIN:VEVENT",
                       paste0("SUMMARY:", event_name),
                       paste0("DTSTART:", start_utc),
                       paste0("DTEND:", end_utc),
                       paste0("DESCRIPTION:Step: ", df$Step[i]),
                       "END:VEVENT")
        ics_lines <- c(ics_lines,ics_event)}
      ics_lines <- c(ics_lines,"END:VCALENDAR")
      writeLines(ics_lines,file)})}

# ------------------
# Run App
# ------------------
shinyApp(ui, server)
