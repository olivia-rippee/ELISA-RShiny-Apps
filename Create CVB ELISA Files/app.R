library(shiny)

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
  titlePanel("Generate Blank ELISA edata CSV Files"),
  sidebarLayout(
    sidebarPanel(
      h4("File names (editable)"),
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
      verbatimTextOutput("preview"))))

# -------------------------------
# Server
# -------------------------------
server <- function(input, output, session) {
  output$preview <- renderText({
    paste(
      input$filename_dilution,
      input$filename_layout,
      input$filename_od,
      input$filename_plateinfo,
      input$filename_serial,
      sep = "\n")})
  
  # Download handler
  output$download_files <- downloadHandler(
    filename = function() {
      paste0("ELISA_files_", Sys.Date(), ".zip")},
    content = function(file) {
      tmpdir <- tempdir()
      old_wd <- setwd(tmpdir)
      on.exit(setwd(old_wd), add = TRUE)
      add_csv <- function(x) {
        if (!grepl("\\.csv$", x, ignore.case = TRUE)) {
          paste0(x, ".csv")
        } else {x}}

      filename_dilution <- add_csv(input$filename_dilution)
      filename_layout <- add_csv(input$filename_layout)
      filename_od   <- add_csv(input$filename_od)
      filename_plateinfo <- add_csv(input$filename_plateinfo)
      filename_serial <- add_csv(input$filename_serial)
      
      write.csv(make_plate_df(), filename_dilution,
                row.names = FALSE, quote = FALSE)
      
      write.csv(make_plate_df(), filename_layout,
                row.names = FALSE, quote = FALSE)
      
      write.csv(make_plate_df(), filename_od,
                row.names = FALSE, quote = FALSE)
      
      PlateInfo <- data.frame(
        plateID = character(),
        date = character(),
        tech = character(),
        plate_role = character(),
        stringsAsFactors = FALSE)
      write.csv(PlateInfo, filename_plateinfo,
                row.names = FALSE, quote = FALSE)
      
      SerialTesting <- data.frame(
        plateID = character(),
        serialID = character(),
        ParmB_ratio = numeric(),
        ParmA_ratio = numeric(),
        rp = numeric(),
        avgBlank = numeric(),
        avgBlankedNegativeCtrl = numeric(),
        stringsAsFactors = FALSE)
      write.csv(SerialTesting, filename_serial,
                row.names = FALSE, quote = FALSE)
      
      zip(zipfile = file, files = c(filename_dilution, filename_layout, filename_od,
                                    filename_plateinfo, filename_serial))})}

# -------------------------------
# Run app
# -------------------------------
shinyApp(ui = ui, server = server)
