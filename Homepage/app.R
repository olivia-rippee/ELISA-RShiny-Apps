library(shiny)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "lumen"),
  
  tags$head(
    tags$style(HTML("
    .card-header {
      font-weight: 700;}"))),
  
  titlePanel("Kemin R Shiny App Homepage"),
  br(),
  
  h3("RShiny Apps"),
  p("Created by OR"),
  
  layout_columns(
    col_widths = c(4, 4, 4),

    card(
      min_height = 250,
      card_header("ELISA Timepoints Calculator"),
      p("Plan your ELISA timepoints."),
      card_footer(
        tags$a(
          "Launch App",
          href = "https://oliviarippee-elisa-timepoints-calculator.share.connect.posit.cloud",
          target = "_blank",
          class = "btn btn-primary w-100"))),

    card(
      min_height = 250,
      card_header("ELISA edata QA Scan"),
      p("Verify plateIDs and serials between files."),
      card_footer(
        tags$a(
          "Launch App",
          href = "https://oliviarippee-elisa-edata-qa-scan.share.connect.posit.cloud/",
          target = "_blank",
          class = "btn btn-primary w-100"))),

    card(
      min_height = 250,
      card_header("ELISA Analysis"),
      p("Generate summary statistics for parallelism and ruggedness."),
      card_footer(
        tags$a(
          "Launch App",
          href = "https://oliviarippee-elisa-parallelism-ruggedness.share.connect.posit.cloud/",
          target = "_blank",
          class = "btn btn-primary w-100"))),
    
    #    card(
    #      min_height = 250,
    #      card_header("Generate Blank ELISA CVB Files"),
    #      p("Create blank Dilution, Layout, OD, PlateInfo, and SerialTesting csv files."),
    #      card_footer(
    #        tags$a(
    #          "Launch App",
    #          href = "https://oliviarippee-cvb-elisa-files.share.connect.posit.cloud/",
    #          target = "_blank",
    #          class = "btn btn-primary w-100"))),

    card(
      min_height = 250,
      card_header("Clinical edata QA Scan"),
      p("Verify animal IDs between files."),
      card_footer(
        tags$a(
          "Launch App",
          href = "https://oliviarippee-clinical-edata-qa-scan.share.connect.posit.cloud/",
          target = "_blank",
          class = "btn btn-primary w-100")))),

    br(),
    h3("Useful External Websites"),
    br(),

    layout_columns(
      col_widths = c(4, 4, 4),

    card(
      card_footer(
        tags$a(
          "Dilution Factor Calculator",
          href = "https://www.physiologyweb.com/calculators/dilution_factor_calculator_no_unit.html",
          target = "_blank",
          class = "btn btn-info w-100"))),
    
    card(
      card_footer(
        tags$a(
          "Percent Solutions Calculator",
          href = "https://www.physiologyweb.com/calculators/percent_solutions_calculator.html",
          target = "_blank",
          class = "btn btn-info w-100"))),
    
    card(
      card_footer(
        tags$a(
          "Time Duration Calculator",
          href = "https://www.calculator.net/time-duration-calculator.html",
          target = "_blank",
          class = "btn btn-info w-100")))))

server <- function(input, output, session) {}

shinyApp(ui, server)

