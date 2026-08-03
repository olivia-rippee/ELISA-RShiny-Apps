ui <- fluidPage(
  titlePanel("ELISA Analysis – Uniformity, Parallelism, and Ruggedness"),
  
  fluidRow(column(12,
    checkboxGroupInput("analyses", "Select analyses to run:",
                       choices = c("Uniformity"  = "uniformity", "Parallelism" = "parallelism", "Ruggedness"  = "ruggedness")),
    br(),
    
    conditionalPanel(
      condition = "input.analyses.includes('parallelism') || input.analyses.includes('ruggedness')",
      
      numericInput(
        "conf_level",
        "Confidence level:",
        value = 0.90,
        min = 0.80,
        max = 0.99,
        step = 0.01),
      
      uiOutput("conf_warning"),
      br()),
    
    conditionalPanel(
      condition = "input.analyses.includes('parallelism')",
      
      # Dilution
      h5(strong(style = "width:100%;", "Upload Dilution File (.csv or .xlsx) with columns 1-12 and plateID")),
      fileInput("dilution_file", label = NULL, accept = c(".csv", ".xlsx")),
      
      # Layout
      h5(strong(style = "width:100%;", "Upload Layout File (.csv or .xlsx) with columns 1-12 and plateID")),
      fileInput("layout_file", label = NULL, accept = c(".csv", ".xlsx"))),
    
    
    conditionalPanel(
      condition = "input.analyses.includes('uniformity')",
      
      # OD
      h5(strong(style = "width:100%;", "Upload OD File (.csv or .xlsx) with columns 1-12 and plateID")),
      fileInput("od_file", label = NULL, accept = c(".csv", ".xlsx"))),
    
    
    conditionalPanel(
      condition = "input.analyses.includes('parallelism') || input.analyses.includes('ruggedness')",
      
      # Serial testing
      h5(strong(style = "width:100%;", "Upload SerialTesting File (.csv or .xlsx) 
  with columns columns plateID, serialID, ParmA_ratio, ParmB_ratio, and rp")),
      p("Analysis requires a 120%, Serial A, Serial B, and Positive Control."),
      fileInput("serialtesting_file", label = NULL, accept = c(".csv", ".xlsx")),
      uiOutput("serial_mapping_ui")),
    
    actionButton("run", "Run Analysis", class = "btn-primary"),
    actionButton("clear", "Clear"))),
  
  hr(),
  
  tabsetPanel(
    tabPanel("Uniformity", 
             conditionalPanel(
               condition = "input.analyses.includes('uniformity')",
               radioButtons(inputId = "uniformity_scope", 
                            label = "Uniformity analysis scope:", 
                            choices = c(
                              "Only plates with uniformity in plateID" = "uniformity_only", 
                              "All plates" = "all"),
                            selected = "uniformity_only"),
               uiOutput("uniformity_ui")),
             conditionalPanel(
               condition = "!input.analyses.includes('uniformity')", 
               h4("Uniformity not selected."))),
    
    tabPanel("Parallelism", 
             conditionalPanel(
               condition = "input.analyses.includes('parallelism')",
               radioButtons(inputId = "parallelism_scope", 
                            label = "Parallelism analysis scope:", 
                            choices = c(
                              "Only plates with parallelism in plateID" = "parallelism_only",
                              "All plates" = "all"), 
                            selected = "parallelism_only"),
               uiOutput("parallelism_ui")),
             conditionalPanel(
               condition = "!input.analyses.includes('parallelism')", 
               h4("Parallelism not selected."))),
    
    tabPanel("Ruggedness", 
             conditionalPanel(
               condition = "input.analyses.includes('ruggedness')",
               radioButtons(inputId = "ruggedness_scope", 
                            label = "Ruggedness analysis scope:", 
                            choices = c(
                              "Only plates with ruggedness, min, or max in plateID" = "ruggedness_only",
                              "All plates" = "all"), 
                            selected = "ruggedness_only"),
               uiOutput("ruggedness_ui")),
             conditionalPanel(
               condition = "!input.analyses.includes('ruggedness')", 
               h4("Ruggedness not selected.")))))