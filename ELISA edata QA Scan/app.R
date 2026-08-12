library(shiny)
library(shinyjs)
library(tidyverse)
library(DT)

# -----------------------
# Helper functions
# -----------------------
collapse_rows <- function(rows) {
  if (length(rows) == 0) return("")
  runs <- split(rows, cumsum(c(1, diff(rows) != 1)))
  collapsed <- sapply(runs, function(run) {
    if (length(run) == 1) as.character(run)
    else paste0(run[1], "-", run[length(run)])})
  paste(collapsed, collapse = ", ")}

detect_mislabels <- function(target_file, target_name, all_files) {
  target_ids <- unique(target_file$plateID)
  other_ids <- unique(unlist(lapply(all_files[names(all_files) != target_name], function(df) df$plateID)))
  mislabels <- target_ids[!(target_ids %in% other_ids)]
  if (length(mislabels) == 0) return(NULL)
  entries <- lapply(mislabels, function(plateid) {
    rows <- which(target_file$plateID == plateid) + 1
    data.frame(File = target_name, Row = collapse_rows(rows), PlateID = plateid, stringsAsFactors = FALSE)})
  do.call(rbind, entries)}

get_layout_values <- function(layout_df, plateid) {
  layout_df %>%
    filter(plateID == plateid) %>%
    select(-plateID) %>%
    unlist(use.names = FALSE) %>%
    unique() %>%
    na.omit()}

check_plate_serials <- function(plateid, layout, serialtesting) {
  # Identify Layout well columns 1-12
  well_cols <- names(layout)[grepl("^(X)?([1-9]|1[0-2])$", names(layout))]
  
  # SerialTesting is the source of truth
  serials <- serialtesting %>%
    filter(plateID == plateid) %>%
    pull(serialID) %>%
    unique() %>%
    na.omit()
  
  # Get all values from Layout wells 1-12
  layout_vals <- layout %>%
    filter(plateID == plateid) %>%
    select(all_of(well_cols)) %>%
    unlist(use.names = FALSE) %>%
    unique() %>%
    na.omit()
  
  # Check each SerialTesting serialID against Layout
  tibble(
    PlateID = plateid,
    Serial = serials,
    Missing_In = sapply(serials, function(s) {
      if (!(s %in% layout_vals)) {"Layout"
      } else {""}})) %>%
    filter(Missing_In != "")}

# -----------------------
# UI
# -----------------------
ui <- fluidPage(useShinyjs(),
  titlePanel("ELISA edata QA Scan"),
  p("Check your ELISA edata for consistent plate labeling and matching serials between files prior to submission to CVB."),
  br(),
  fluidRow(column(4, fileInput("dilution", "Upload Dilution (CSV)")),
           column(4, fileInput("layout", "Upload Layout (CSV)")),
  fluidRow(column(4, fileInput("od", "Upload OD (CSV)")) ),
         # column(4, fileInput("plateinfo", "Upload PlateInfo (CSV)")),
           column(4, fileInput("serialtesting", "Upload SerialTesting (CSV)")) ),
  br(),
  actionButton("run", "Run QA Check", class = "btn-primary"),
  actionButton("clear", "Clear", class = "btn-secondary"),
  br(), br(),
  uiOutput("results_ui"))


# -----------------------
# Server
# -----------------------
server <- function(input, output, session) {
  files_data <- reactiveVal(list())
  
  plateIDs_r <- reactiveVal(NULL)
  mislabels_any_r <- reactiveVal(NULL)
  mislabels_all_r <- reactiveVal(NULL)
  serial_mismatch_r <- reactiveVal(NULL)
  
  observe({files <- list(
      dilution = if (!is.null(input$dilution)) read_csv(input$dilution$datapath) else NULL,
      layout = if (!is.null(input$layout)) read_csv(input$layout$datapath) else NULL,
      od = if (!is.null(input$od)) read_csv(input$od$datapath) else NULL,
      # plateinfo = if (!is.null(input$plateinfo)) read_csv(input$plateinfo$datapath) else NULL,
      serialtesting = if (!is.null(input$serialtesting)) read_csv(input$serialtesting$datapath) else NULL)
    files_data(files) })
  
  observeEvent(input$run, {
    files <- files_data()
    if (length(files) == 0 || all(sapply(files, is.null))) {
      showModal(modalDialog(title = "No files",
        "Please upload files.",
        easyClose = TRUE))
      return() }
    
    
    # All Plate IDs
    # -----------------------
    platelist <- unique(unlist(lapply(
      files,
      function(df) if (!is.null(df)) df$plateID else character(0))))
    
    n <- length(platelist)
    half <- ceiling(n / 2)
    
    plate_df <- data.frame(
      PlateID_1 = platelist[1:half],
      PlateID_2 = c(platelist[(half + 1):n], rep(NA, max(0, half * 2 - n))),
      stringsAsFactors = FALSE)
    
    plateIDs_r(
      datatable(plate_df, colnames = c("PlateID", "PlateID"),
        options = list(pageLength = 10), rownames = FALSE))
    
    
    # Mislabels in any file
    # -----------------------
    mislabels_list <- mapply(
      detect_mislabels,
      target_file = files,
      target_name = names(files),
      MoreArgs = list(all_files = files),
      SIMPLIFY = FALSE)
    detected_any <- do.call(rbind, Filter(Negate(is.null), mislabels_list))
    mislabels_any_r(detected_any)
    
    
    # Mislabels in all files
    # -----------------------
    presence_list <- lapply(files, function(df) if (!is.null(df)) platelist %in% df$plateID else logical(length(platelist)))
    presence_matrix <- do.call(cbind, presence_list)
    rownames(presence_matrix) <- platelist
    incomplete_ids <- rownames(presence_matrix)[rowSums(presence_matrix) < length(files)]
    df_mislabels <- NULL
    if (length(incomplete_ids) > 0) {
      df_mislabels <- lapply(incomplete_ids, function(plateid) {
        entries <- lapply(names(files), function(filename) {
          df <- files[[filename]]
          if (!is.null(df)) {rows <- which(df$plateID == plateid) + 1
          if (length(rows) > 0)
            data.frame(File = filename, Row = collapse_rows(rows), PlateID = plateid) }})
        do.call(rbind, Filter(Negate(is.null), entries)) })
      df_mislabels <- do.call(rbind, df_mislabels) }
    mislabels_all_r(df_mislabels)
    
    
    # Serial check
    # -----------------------
    serials_missing <- NULL
    serial_check_message <- NULL
    
    if (!is.null(files$layout) && !is.null(files$serialtesting)) {
      
      # SerialTesting is the source of truth
      plate_ids <- unique(files$serialtesting$plateID)
      
      serials_missing <- map_dfr(
        plate_ids,
        check_plate_serials,
        layout = files$layout,
        serialtesting = files$serialtesting)
      
      if (nrow(serials_missing) == 0) {
        serial_check_message <-
          "All serialID values from SerialTesting were found in Layout."}
      
    } else {serial_check_message <- "Serial check skipped. Layout or SerialTesting file missing."}
    
    serial_mismatch_r(list(data = serials_missing, message = serial_check_message))
    
    # Render results UI
    # -----------------------
    output$results_ui <- renderUI({
      tagList(hr(),
        h3("All Plate IDs"),
        p("List of all plateIDs found in the files."),
        DTOutput("plateIDs_table"),
        hr(),
        
        h3("Plate IDs Not in Any Other File"),
        p("List of plate IDs only found in one file (e.g. typos)."),
        if (is.null(mislabels_any_r()) || nrow(mislabels_any_r()) == 0) {
          HTML("No mismatches found.")
        } else {DTOutput("mislabels_any_table")},
        hr(),
        
        h3("Plate IDs Not in Every File"),
        p("List of plate IDs that are missing from at least one file (e.g. unfinished entries)."),
        if (is.null(mislabels_all_r()) || nrow(mislabels_all_r()) == 0) {
          HTML("No mismatches found.")
        } else {DTOutput("mislabels_all_table")},
        hr(),
        
        h3("Serial Check"),
        p("List of serials found in Serial Testing that are not in Layout."),
        if (!is.null(serial_mismatch_r()$message)) {
          HTML(serial_mismatch_r()$message)
        } else {DTOutput("serial_mismatch_table") } ) })
    
    
    # Render tables
    # -----------------------
    output$plateIDs_table <- renderDT({plateIDs_r() })
    output$mislabels_any_table <- renderDT({datatable(mislabels_any_r(), rownames = FALSE)})
    output$mislabels_all_table <- renderDT({datatable(mislabels_all_r(), rownames = FALSE)})
    output$serial_mismatch_table <- renderDT({datatable(serial_mismatch_r()$data, rownames = FALSE)}) })
  
  
  # Clear Button
  # -----------------------
  observeEvent(input$clear, {
    reset("dilution")
    reset("layout")
    reset("od")
    # reset("plateinfo")
    reset("serialtesting")
    files_data(list())
    plateIDs_r(NULL)
    mislabels_any_r(NULL)
    mislabels_all_r(NULL)
    serial_mismatch_r(NULL)
    output$results_ui <- renderUI({}) }) }


# -----------------------
# Run App
# -----------------------
shinyApp(ui, server)
