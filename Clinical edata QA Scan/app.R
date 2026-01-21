library(shiny)
library(shinyjs)
library(DT)
library(readr)
library(readxl)
library(dplyr)
library(digest)


# -----------------------
# UI
# -----------------------
ui <- fluidPage(useShinyjs(),
  titlePanel("Clinical edata QA Scan"),
  p("Check your clinical edata for consistent animal IDs between two files (CSV or Excel)."),
  br(),
  fluidRow(
    column(6, fileInput("file1", "Upload File 1 (CSV/XLSX)")),
    column(6, fileInput("file2", "Upload File 2 (CSV/XLSX)"))),
  br(),
  actionButton("run", "Run QA Check", class = "btn-primary"),
  actionButton("clear", "Clear", class = "btn-secondary"),
  br(), br(),
  uiOutput("results_ui"))


# -----------------------
# Server
# -----------------------
server <- function(input, output, session) {
  read_file <- function(path) {
    ext <- tolower(tools::file_ext(path))
    if (ext %in% c("csv", "txt")) {
      df <- read_csv(path, show_col_types = FALSE)
    } else if (ext %in% c("xls", "xlsx")) {
      df <- read_excel(path)
    } else {stop("Unsupported file type") }
    names(df) <- trimws(names(df))
    df}
  
  # Store uploaded files in reactiveValues so they can be cleared
  rv <- reactiveValues(file1 = NULL, file2 = NULL)
  
  observeEvent(input$file1, { rv$file1 <- read_file(input$file1$datapath) })
  observeEvent(input$file2, { rv$file2 <- read_file(input$file2$datapath) })
  
  observeEvent(input$run, {
    files <- list(file1 = rv$file1, file2 = rv$file2)
    
    # Check that both files are uploaded
    # -----------------------------------
    if (is.null(files$file1) || is.null(files$file2)) {
      showModal(modalDialog(
        title = "Missing file(s)",
        "Please upload both files.",
        easyClose = TRUE))
      return()}
    
    # Check if the same file is selected
    # -----------------------------------
    hash1 <- digest::digest(file = input$file1$datapath)
    hash2 <- digest::digest(file = input$file2$datapath)
    if (hash1 == hash2) {
      showModal(modalDialog(
        title = "Same file",
        "You have uploaded the same file twice. Please select two different files.",
        easyClose = TRUE))
      return()}
    
    # Check column existence
    # -----------------------
    if (!"animalID" %in% names(files$file1)) {
      showModal(modalDialog(
        title = "Column missing",
        "File 1 does not contain a column named 'animalID'.",
        easyClose = TRUE))
      return()}
    
    if (!"animalID" %in% names(files$file2)) {
      showModal(modalDialog(
        title = "Column missing",
        "File 2 does not contain a column named 'animalID'.",
        easyClose = TRUE))
      return()}
    
    # Extract animal IDs
    # -------------------
    ids1 <- unique(files$file1$animalID)
    output$ids1_table <- renderDT({
      datatable(
        data.frame(animalID = ids1),
        options = list(pageLength = 10),
        rownames = FALSE)})
    
    ids2 <- unique(files$file2$animalID)
    output$ids2_table <- renderDT({
      datatable(
        data.frame(animalID = ids2),
        options = list(pageLength = 10),
        rownames = FALSE)})
    
    # Compare
    # ---------
    missing_in_file2 <- setdiff(ids1, ids2)
    missing_in_file1 <- setdiff(ids2, ids1)
    same_count <- length(ids1) == length(ids2)
    
    # Render results
    # ---------------
    output$results_ui <- renderUI({
      tagList(
        tags$hr(style = "border-top: 3px solid #999; margin: 30px 0;"),
        h3("All Animal IDs"),
        br(),
        
        # Output all IDs for reference
        # -----------------------------
        fluidRow(column(6,
          h4("Animal IDs found in File 1"),
          hr(),
          DTOutput("ids1_table")),
                 column(6,
          h4("Animal IDs found in File 2"),
          hr(),
          DTOutput("ids2_table"))),
        tags$hr(style = "border-top: 3px solid #999; margin: 30px 0;"),
        
        # Comparison
        # -----------
        h3("Comparison"),
        p(paste("File 1 has", length(ids1), "unique animal IDs and File 2 has", length(ids2), "unique animal IDs."), 
          if (same_count) "Both files have the same number of IDs." else "The files have different numbers of IDs."),
        br(),
        
        # IDs missing from one file
        # --------------------------
        fluidRow(column(6,
          h4("IDs in File 1 but not in File 2"),
          hr(),
          if (length(missing_in_file2) == 0) "None" else DTOutput("missing2_table")),
                 column(6,
          h4("IDs in File 2 but not in File 1"),
          hr(),
          if (length(missing_in_file1) == 0) "None" else DTOutput("missing1_table"))))  })
    
    output$missing1_table <- renderDT({data.frame(animalID = missing_in_file1)}, rownames = FALSE)
    output$missing2_table <- renderDT({data.frame(animalID = missing_in_file2)}, rownames = FALSE) })
  
  # Clear button
  # ---------------
  observeEvent(input$clear, {
    # Reset file inputs visually
    reset("file1")
    reset("file2")
    
    # Reset reactiveValues
    rv$file1 <- NULL
    rv$file2 <- NULL
    
    # Clear results UI
    output$results_ui <- renderUI({})
    
    # Clear all table outputs explicitly
    output$ids1_table <- renderDT({})
    output$ids2_table <- renderDT({})
    output$missing1_table <- renderDT({})
    output$missing2_table <- renderDT({})
    
    # Remove any open modals
    removeModal()  }) }


# -----------------------
# Run App
# -----------------------
shinyApp(ui, server)
