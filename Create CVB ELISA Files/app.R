library(shiny)
library(openxlsx)

# -------------------------------
# Helper function
# -------------------------------
make_plate_df <- function() {
  cols <- c(as.character(1:12), "plateID")
  df <- as.data.frame(
    matrix(nrow = 0, ncol = length(cols)),
    stringsAsFactors = FALSE)
  colnames(df) <- cols
  df}


# -------------------------------
# UI
# -------------------------------
ui <- fluidPage(
  titlePanel("ELISA File Generator"),
  tabsetPanel(
    
    # 3PL Tab
    # --------
    tabPanel("3PL",
             sidebarLayout(
               sidebarPanel(
                 h4("CSV File Names (editable)"),
                 br(),
                 textInput("filename_dilution", "Dilution file", "Dilution.csv"),
                 textInput("filename_layout", "Layout file", "Layout.csv"),
                 textInput("filename_od", "OD file", "OD.csv"),
                 textInput("filename_plateinfo", "PlateInfo file", "PlateInfo.csv"),
                 textInput("filename_serial", "SerialTesting file", "SerialTesting.csv"),
                 br(),
                 downloadButton("download_files", "Download Files", class="btn-primary")),
               mainPanel(
                 h4("Files that will be created:"),
                 verbatimTextOutput("preview")))),
    
    # Pseudolinear with masking range inputs
    # ----------------------------------------
    tabPanel("Pseudolinear",
             sidebarLayout(
               sidebarPanel(
                 h4("Excel File Name (editable)"),
                 br(),
                 textInput("filename_masking", "Masking file", "MaskingRanges.xlsx"),
                 br(),
                 # Inputs for the 3 row ranges (MR)
                 h5("MR Ranges (Rows)"),
                 textInput("row1", "MR Row 1 range", "3-6"),
                 textInput("row2", "MR Row 2 range", "3-7"),
                 textInput("row3", "MR Row 3 range", "4-7"),
                 br(),
                 # Inputs for the 3 column ranges (PC)
                 h5("PC Ranges (Column)"),
                 textInput("col1", "PC Column 1 range", "5-8"),
                 textInput("col2", "PC Column 2 range", "5-9"),
                 textInput("col3", "PC Column 3 range", "6-9"),
                 br(),
                 downloadButton("download_masking", "Download File", class="btn-primary")),
               mainPanel(
                 h4("Preview of the Masking Ranges file:"),
                 verbatimTextOutput("masking_preview"))))))

# -------------------------------
# Server
# -------------------------------
server <- function(input, output, session) {
  
  # 3PL Tab
  # ---------
  output$preview <- renderText({
    paste(
      input$filename_dilution,
      input$filename_layout,
      input$filename_od,
      input$filename_plateinfo,
      input$filename_serial,
      sep = "\n")})
  
  output$download_files <- downloadHandler(
    filename = function() { paste0("ELISA_files_", Sys.Date(), ".zip") },
    content = function(file) {
      tmpdir <- tempdir()
      old_wd <- setwd(tmpdir)
      on.exit(setwd(old_wd), add = TRUE)
      
      add_csv <- function(x) if (!grepl("\\.csv$", x, ignore.case = TRUE)) paste0(x, ".csv") else x
      
      filename_dilution <- add_csv(input$filename_dilution)
      filename_layout   <- add_csv(input$filename_layout)
      filename_od       <- add_csv(input$filename_od)
      filename_plateinfo<- add_csv(input$filename_plateinfo)
      filename_serial   <- add_csv(input$filename_serial)
      
      write.csv(make_plate_df(), filename_dilution, row.names = FALSE, quote = FALSE)
      write.csv(make_plate_df(), filename_layout, row.names = FALSE, quote = FALSE)
      write.csv(make_plate_df(), filename_od, row.names = FALSE, quote = FALSE)
      
      PlateInfo <- data.frame(
        plateID = character(),
        date = character(),
        tech = character(),
        plate_role = character(),
        stringsAsFactors = FALSE)
      write.csv(PlateInfo, filename_plateinfo, row.names = FALSE, quote = FALSE)
      
      SerialTesting <- data.frame(
        plateID = character(),
        serialID = character(),
        ParmB_ratio = numeric(),
        ParmA_ratio = numeric(),
        rp = numeric(),
        avgBlank = numeric(),
        avgBlankedNegativeCtrl = numeric(),
        stringsAsFactors = FALSE)
      write.csv(SerialTesting, filename_serial, row.names = FALSE, quote = FALSE)
      
      zip(zipfile = file, files = c(filename_dilution, filename_layout, filename_od,
                                    filename_plateinfo, filename_serial))})
  
  # Pseudolinear Tab
  # -----------------
  create_full_template <- reactive({
    rows <- c(input$row1, input$row2, input$row3)
    cols <- c(input$col1, input$col2, input$col3)
    
    n_tables <- 4
    block_width <- 4
    total_cols <- n_tables * block_width + (n_tables - 1)
    
    mat <- matrix("", nrow = 8, ncol = total_cols)
    
    mat[1,1] <- "Plate ID"
    mat[2,1] <- "Avg Blank"
    
    main_titles <- c("PC", "PC", "SERIAL", "SERIAL")
    sub_titles  <- c("Slope Ratio", "RP", "Slope Ratio", "RP")
    
    for (i in seq_len(n_tables)) {
      start_col <- (i - 1) * (block_width + 1) + 1
      mat[3, start_col] <- main_titles[i]
      mat[4, start_col] <- sub_titles[i]
      mat[4, (start_col+1):(start_col+3)] <- cols
      mat[5, start_col] <- "MR"
      mat[6, start_col] <- rows[1]
      mat[7, start_col] <- rows[2]
      mat[8, start_col] <- rows[3]}
    
    empty_row <- matrix("", nrow = 1, ncol = total_cols)
    rows_list <- list()
    for (i in 1:5) {
      rows_list[[length(rows_list)+1]] <- mat
      if (i < 5) rows_list[[length(rows_list)+1]] <- empty_row}
    
    as.data.frame(do.call(rbind, rows_list), stringsAsFactors = FALSE)})
  
  output$masking_preview <- renderPrint({
    df <- create_full_template()
    print(utils::head(df, 40))})
  
  output$download_masking <- downloadHandler(
    filename = function() {
      fname <- input$filename_masking
      if (!grepl("\\.xlsx$", fname, ignore.case = TRUE)) paste0(fname, ".xlsx") else fname},
    content = function(file) {
      df <- create_full_template()
      
      wb <- createWorkbook()
      addWorksheet(wb, "Masking")
      writeData(wb, "Masking", df, colNames = FALSE)
      saveWorkbook(wb, file, overwrite = TRUE)})}

# -------------------------------
# Run app
# -------------------------------
shinyApp(ui, server)
