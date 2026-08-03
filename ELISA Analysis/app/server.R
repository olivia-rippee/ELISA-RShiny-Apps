server <- function(input, output, session) {
  
  observeEvent(input$clear, session$reload())
  
  # z-score and confidence level input
  z_score <- reactive({
    qnorm(1 - (1 - input$conf_level) / 2)})
  
  conf_level <- reactive({
    input$conf_level})
  
  # Confidence level validation
  # -----------------------------
  output$conf_warning <- renderUI({
    req(input$analyses)
    
    if ("parallelism" %in% input$analyses || "ruggedness" %in% input$analyses) {
      req(input$conf_level)
      
      if (input$conf_level < 0.80 || input$conf_level > 0.99) {
        div(
          style = "color:red;",
          "Confidence level must be between 80% and 99%."
        )
      }
    }
  })
  
  # Dynamic z-score for CI calculations
  z_score <- reactive({
    req(input$conf_level)
    qnorm(1 - (1 - input$conf_level) / 2)
  })
  
  # ------------------------------------
  # User specifies which serial is which
  # ------------------------------------
  serial_testing_raw <- reactive({
    req(input$serialtesting_file)
    read_uploaded_file(input$serialtesting_file)
  })
  
  serial_mapping_valid <- reactive({
    req(serial_testing_raw())
    
    vals <- c(
      input$PC_serial,
      input$SerA_serial,
      input$SerB_serial,
      input$`120_serial`)
    
    # must all be selected
    if (any(is.null(vals) | vals == "")) {
      return(FALSE)}
    
    # must be unique
    length(unique(vals)) == 4})
  
  output$serial_mapping_warning <- renderText({
    req(serial_testing_raw())
    
    vals <- c(input$PC_serial, input$SerA_serial, input$SerB_serial, input$`120_serial`)
    
    if (any(is.null(vals) | vals == "")) {
      return("Please select all serial mappings.")}
    
    if (length(unique(vals)) != 4) {
      return("Each selection must be unique (no duplicates).")}})
  
  output$serial_mapping_ui <- renderUI({
    req(serial_testing_raw())
    serial_choices <- sort(unique(serial_testing_raw()$serialID))
    
    tagList(
      h5(strong(style = "width:100%;", "Select Serial IDs")),
      
      selectInput("PC_serial", "Positive Control", choices = serial_choices),
      selectInput("SerA_serial", "Serial A", choices = serial_choices),
      selectInput("SerB_serial", "Serial B", choices = serial_choices),
      selectInput("120_serial", "120%", choices = serial_choices),
      
      textOutput("serial_mapping_warning"),
      br())})
  
  # -------------------
  # Data 
  # -------------------
  data_all <- eventReactive(input$run, {
    
    if (any(input$analyses %in% c("parallelism","ruggedness"))) {
      req(serial_mapping_valid())}
    
    mapping <- list(
      PC   = input$PC_serial,
      SerA = input$SerA_serial,
      SerB = input$SerB_serial,
      X120 = input$`120_serial`)
    
    out <- list()
    
    if ("uniformity" %in% input$analyses) {
      
      req(input$od_file)
      
      out <- c(
        out,
        process_uniformity(
          od_file = input$od_file,
          scope = input$uniformity_scope
        )
      )
    }
    
    if ("parallelism" %in% input$analyses) {
      req(
        input$serialtesting_file,
        input$dilution_file,
        input$layout_file)
      
      out <- c(
        out,
        process_parallelism(
          serial_testing = input$serialtesting_file,
          dilution = input$dilution_file,
          layout = input$layout_file,
          mapping = mapping,
          scope = input$parallelism_scope
        )
      )
      
    }
    
    if ("ruggedness" %in% input$analyses) {
      req(input$serialtesting_file)
      
      out <- c(
        out,
        process_ruggedness(
          serial_testing = input$serialtesting_file,
          mapping = mapping,
          scope = input$ruggedness_scope
        )
      )
      
    }
    
    out})
  
  # -------------------------------------------------
  # Uniformity UI
  # -------------------------------------------------
  output$uniformity_ui <- renderUI({
    req(input$run)
    tagList(
      h2("Uniformity"),
      h3("All Plate IDs"),
      DTOutput("uniformity_plate_ids"),
      h3("Uniformity (Average Over All Plates)"),
      plotOutput("heatmap_all", height="500px"),
      hr(),
      h3("Uniformity - Heatmap by Plate"),
      uiOutput("heatmap_by_plate"),
      hr(),
      h3("Uniformity - Statistics Table"),
      DTOutput("stats_table"),
      hr(),
      h3("Uniformity - Average Line Graphs"),
      plotOutput("line_plots"))})
  
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
  
  uniformity_plots(
    input,
    output,
    session,
    data_all
  )
  
  
  parallelism_tables(
    input,
    output,
    session,
    data_all,
    z_score(),
    conf_level()
  )
  
  
  ruggedness_tables(
    input,
    output,
    session,
    data_all,
    z_score(),
    conf_level()
  )}
