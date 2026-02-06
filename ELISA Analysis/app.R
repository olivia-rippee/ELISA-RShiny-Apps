library(shiny)
library(tidyverse)
library(DT)
library(flextable)
library(patchwork)
library(gridExtra)

# -------------------------------------------------
# Helper functions
# -------------------------------------------------
cv <- function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100

format_metric <- function(metric, x) {
  if (metric %in% c("Lower_ParmA","Upper_ParmA","Lower_ParmB","Upper_ParmB")) {
    formatC(x, format = "f", digits = 1)
  } else if (metric %in% c("CV_ParmA","CV_ParmB","CV_RP")) {
    paste0(formatC(x, format = "f", digits = 2), "%")
  } else if (metric == "SampleSize") {
    as.character(x)
  } else {
    formatC(x, format = "f", digits = 3)}}

conf_level <- 0.90
z_score <- qnorm(1 - (1 - conf_level) / 2)

make_ruggedness_table <- function(data, type = c("min","max")) {
  type <- match.arg(type)
  data %>%
    group_by(serial) %>%
    summarise(
      AvgParmA = mean(ParmA_ratio, na.rm = TRUE),
      StdevParmA = sd(ParmA_ratio, na.rm = TRUE),
      CV_ParmA = StdevParmA / AvgParmA * 100,
      
      AvgParmB = mean(ParmB_ratio, na.rm = TRUE),
      StdevParmB = sd(ParmB_ratio, na.rm = TRUE),
      CV_ParmB = StdevParmB / AvgParmB * 100,
      
      AvgRP   = mean(rp, na.rm = TRUE),
      StdevRP = sd(rp, na.rm = TRUE),
      CV_RP   = StdevRP / AvgRP * 100,
      
      SampleSize = sum(!is.na(ParmA_ratio) & !is.na(ParmB_ratio)),
      MarginError_ParmA = z_score * StdevParmA / sqrt(SampleSize),
      Lower_ParmA = AvgParmA - MarginError_ParmA,
      Upper_ParmA = AvgParmA + MarginError_ParmA,
      MarginError_ParmB = z_score * StdevParmB / sqrt(SampleSize),
      Lower_ParmB = AvgParmB - MarginError_ParmB,
      Upper_ParmB = AvgParmB + MarginError_ParmB,
      .groups = "drop") %>%
    pivot_longer(-serial, names_to="Metric", values_to="Value") %>%
    pivot_wider(names_from=serial, values_from=Value) %>%
    mutate(across(-Metric, ~ mapply(format_metric, Metric, .x))) %>%
    {ci_row <- tibble(Metric="CI", !!!setNames(rep(paste0(conf_level*100,"%"), ncol(.)-1), names(.)[-1]))
      bind_rows(., ci_row)}}

make_ruggedness_min_table <- function(data) {
  req(nrow(data) > 0)
  ruggedness_serials <- unique(data$serial)
  ruggedness_min_summary <- data %>%
    group_by(serial) %>%
    summarise(
      AvgParmA = mean(ParmA_ratio, na.rm = TRUE),
      StdevParmA = sd(ParmA_ratio, na.rm = TRUE),
      CV_ParmA = StdevParmA / AvgParmA * 100,
      AvgParmB = mean(ParmB_ratio, na.rm = TRUE),
      StdevParmB = sd(ParmB_ratio, na.rm = TRUE),
      CV_ParmB = StdevParmB / AvgParmB * 100,
      AvgRP   = mean(rp, na.rm = TRUE),
      StdevRP = sd(rp, na.rm = TRUE),
      CV_RP   = StdevRP / AvgRP * 100,
      SampleSize = sum(!is.na(ParmA_ratio) & !is.na(ParmB_ratio)),
      MarginError_ParmA = z_score * StdevParmA / sqrt(SampleSize),
      Lower_ParmA = AvgParmA - MarginError_ParmA,
      Upper_ParmA = AvgParmA + MarginError_ParmA,
      MarginError_ParmB = z_score * StdevParmB / sqrt(SampleSize),
      Lower_ParmB = AvgParmB - MarginError_ParmB,
      Upper_ParmB = AvgParmB + MarginError_ParmB,
      .groups = "drop")
  
  ruggedness_min <- ruggedness_min_summary %>%
    pivot_longer(-serial, names_to = "Metric", values_to = "Value") %>%
    pivot_wider(names_from = serial, values_from = Value)

  ruggedness_min <- ruggedness_min %>%
    mutate(across(-Metric, ~ case_when(
      Metric %in% c("Lower_ParmA","Upper_ParmA","Lower_ParmB","Upper_ParmB") ~ formatC(.x, format = "f", digits = 1),
      Metric %in% c("CV_ParmA","CV_ParmB","CV_RP") ~ paste0(formatC(.x, format = "f", digits = 2), "%"),
      Metric == "SampleSize" ~ as.character(.x),
      TRUE ~ formatC(.x, format = "f", digits = 3))))
  
  ci_row <- tibble(Metric = "CI",
    !!!setNames(rep(paste0(conf_level * 100, "%"), ncol(ruggedness_min) - 1), names(ruggedness_min)[-1]))
  
  bind_rows(ruggedness_min, ci_row) %>%
    arrange(factor(Metric, levels = c(
      "AvgParmA","StdevParmA","CV_ParmA",
      "AvgParmB","StdevParmB","CV_ParmB",
      "AvgRP","StdevRP","CV_RP", "SampleSize","CI",
      "MarginError_ParmA","Lower_ParmA","Upper_ParmA",
      "MarginError_ParmB","Lower_ParmB","Upper_ParmB")))}

make_ruggedness_max_table <- function(data) {
  req(nrow(data) > 0)
  ruggedness_serials <- unique(data$serial)
  ruggedness_max_summary <- data %>%
    group_by(serial) %>%
    summarise(
      AvgParmA = mean(ParmA_ratio, na.rm = TRUE),
      StdevParmA = sd(ParmA_ratio, na.rm = TRUE),
      CV_ParmA = StdevParmA / AvgParmA * 100,
      AvgParmB = mean(ParmB_ratio, na.rm = TRUE),
      StdevParmB = sd(ParmB_ratio, na.rm = TRUE),
      CV_ParmB = StdevParmB / AvgParmB * 100,
      AvgRP   = mean(rp, na.rm = TRUE),
      StdevRP = sd(rp, na.rm = TRUE),
      CV_RP   = StdevRP / AvgRP * 100,
      SampleSize = sum(!is.na(ParmA_ratio) & !is.na(ParmB_ratio)),
      MarginError_ParmA = z_score * StdevParmA / sqrt(SampleSize),
      Lower_ParmA = AvgParmA - MarginError_ParmA,
      Upper_ParmA = AvgParmA + MarginError_ParmA,
      MarginError_ParmB = z_score * StdevParmB / sqrt(SampleSize),
      Lower_ParmB = AvgParmB - MarginError_ParmB,
      Upper_ParmB = AvgParmB + MarginError_ParmB,
      .groups = "drop")
  
  ruggedness_max <- ruggedness_max_summary %>%
    pivot_longer(-serial, names_to = "Metric", values_to = "Value") %>%
    pivot_wider(names_from = serial, values_from = Value)
  
  ruggedness_max <- ruggedness_max %>%
    mutate(across(-Metric, ~ case_when(
      Metric %in% c("Lower_ParmA","Upper_ParmA","Lower_ParmB","Upper_ParmB") ~ formatC(.x, format = "f", digits = 1),
      Metric %in% c("CV_ParmA","CV_ParmB","CV_RP") ~ paste0(formatC(.x, format = "f", digits = 2), "%"),
      Metric == "SampleSize" ~ as.character(.x),
      TRUE ~ formatC(.x, format = "f", digits = 3))))
  
  ci_row <- tibble(Metric = "CI",
    !!!setNames(rep(paste0(conf_level * 100, "%"), ncol(ruggedness_max) - 1), names(ruggedness_max)[-1]))
  
  bind_rows(ruggedness_max, ci_row) %>%
    arrange(factor(Metric, levels = c(
      "AvgParmA","StdevParmA","CV_ParmA",
      "AvgParmB","StdevParmB","CV_ParmB",
      "AvgRP","StdevRP","CV_RP", "SampleSize","CI",
      "MarginError_ParmA","Lower_ParmA","Upper_ParmA",
      "MarginError_ParmB","Lower_ParmB","Upper_ParmB")))}

format_plate_stats <- function(stats_row) {
  paste0(
    "Q25: ", round(stats_row$Q25, 3),
    "   Median: ", round(stats_row$Median, 3),
    "     Q75: ", round(stats_row$Q75, 3), "\n",
    "CV: ", round(stats_row$CV, 1), "%   ",
    "Inner CV: ", round(stats_row$Inner_CV, 1), "%   ",
    "Max/Min: ", round(stats_row$Ratio_Max_Min, 1), "%   ",
    "Inner Max/Min: ", round(stats_row$Inner_Ratio_Max_Min, 1), "%")}


# -------------------------------------------------
# UI
# -------------------------------------------------
ui <- fluidPage(
  titlePanel("ELISA Analysis – Uniformity, Parallelism, and Ruggedness"),
  
  fluidRow(column(12,
    checkboxGroupInput("analyses", "Select analyses to run:",
                    choices = c("Uniformity"  = "uniformity", "Parallelism" = "parallelism", "Ruggedness"  = "ruggedness")),
                  
    conditionalPanel(
      condition = "input.analyses.includes('parallelism')",
      fileInput("dilution_file", "Upload Dilution CSV"),
      fileInput("layout_file", "Upload Layout CSV")),
                  
    conditionalPanel(
      condition = "input.analyses.includes('uniformity')",
      fileInput("od_file", "Upload OD CSV")),
                  
    conditionalPanel(
      condition = "input.analyses.includes('parallelism') || input.analyses.includes('ruggedness')",
      fileInput("serialtesting_file", "Upload SerialTesting CSV")),
                  
    actionButton("run", "Run Analysis", class = "btn-primary"),
    actionButton("clear", "Clear"))),
  
  hr(),
  
  tabsetPanel(
    tabPanel("Uniformity",
             conditionalPanel(
               condition = "input.analyses.includes('uniformity')",
               uiOutput("uniformity_ui")),
             conditionalPanel(
               condition = "!input.analyses.includes('uniformity')",
               h4("Uniformity not selected."))),
    
    tabPanel("Parallelism",
             conditionalPanel(
               condition = "input.analyses.includes('parallelism')",
               uiOutput("parallelism_ui")),
             conditionalPanel(
               condition = "!input.analyses.includes('parallelism')",
               h4("Parallelism not selected."))),
    
    tabPanel("Ruggedness",
             conditionalPanel(
               condition = "input.analyses.includes('ruggedness')",
               uiOutput("ruggedness_ui")),
             conditionalPanel(
               condition = "!input.analyses.includes('ruggedness')",
               h4("Ruggedness not selected.")))))


# -------------------------------------------------
# Server
# -------------------------------------------------
server <- function(input, output, session) {
  
  observeEvent(input$clear, session$reload())
  
  # -------------------
  # Data 
  # -------------------
  data_all <- eventReactive(input$run, {
    
    req(input$analyses)
    
    # -------------------
    # Conditional file requirements
    # -------------------
    if ("uniformity" %in% input$analyses) {
      req(input$od_file)}
    
    if ("parallelism" %in% input$analyses) {
      req(input$serialtesting_file, input$dilution_file, input$layout_file)}
    
    if ("ruggedness" %in% input$analyses) {
      req(input$serialtesting_file)}
    
    
    # Uniformity plates
    # -------------------
    if ("uniformity" %in% input$analyses) {
      
      ODs <- read.csv(input$od_file$datapath, stringsAsFactors = FALSE)
      colnames(ODs) <- sub("^X(?=\\d)", "", colnames(ODs), perl = TRUE)
      
      uniformity_plates <- ODs %>%
        filter(grepl("Uniformity", plateID, ignore.case = TRUE)) %>%
        group_by(plateID) %>%
        mutate(Row = LETTERS[row_number()]) %>%
        ungroup()
      
      uniformity_long <- uniformity_plates %>%
        pivot_longer(`1`:`12`, names_to = "Col", values_to = "OD") %>%
        mutate(
          Row = as.character(Row),
          Col = as.character(Col),
          OD  = as.numeric(OD))
      
      row_levels <- c(sort(unique(uniformity_long$Row)), "Avg")
      col_levels <- c(sort(as.numeric(unique(uniformity_long$Col))), "Avg") |> as.character()
      
      row_avgs <- uniformity_long %>%
        group_by(plateID, Row) %>%
        summarise(OD = mean(OD, na.rm = TRUE), Col = "Avg", .groups = "drop")
      
      col_avgs <- uniformity_long %>%
        group_by(plateID, Col) %>%
        summarise(OD = mean(OD, na.rm = TRUE), Row = "Avg", .groups = "drop")
      
      overall_avg_plate <- uniformity_long %>%
        group_by(plateID) %>%
        summarise(Row = "Avg", Col = "Avg", OD = mean(OD, na.rm = TRUE), .groups = "drop")
      
      heatmap_df <- bind_rows(uniformity_long, row_avgs, col_avgs, overall_avg_plate) %>%
        mutate(
          Row = factor(Row, levels = rev(row_levels)),
          Col = factor(Col, levels = col_levels))
      
      heatmap_list <- split(heatmap_df, heatmap_df$plateID)
      
      avg_well <- uniformity_long %>%
        group_by(plateID, Row, Col) %>%
        summarise(OD = mean(OD, na.rm = TRUE), .groups = "drop")
      
      row_avg_all <- avg_well %>%
        group_by(plateID, Row) %>%
        summarise(OD = mean(OD), Col = "Avg", .groups = "drop")
      
      col_avg_all <- avg_well %>%
        group_by(plateID, Col) %>%
        summarise(OD = mean(OD), Row = "Avg", .groups = "drop")
      
      overall_avg_all <- avg_well %>%
        group_by(plateID) %>%
        summarise(Row = "Avg", Col = "Avg", OD = mean(OD), .groups = "drop")
      
      heatmap_avg <- bind_rows(avg_well, row_avg_all, col_avg_all, overall_avg_all) %>%
        mutate(
          Row = factor(Row, levels = rev(row_levels)),
          Col = factor(Col, levels = col_levels))
      
      heatmap_all_plates <- heatmap_avg %>%
        group_by(Row, Col) %>%
        summarise(OD = mean(OD, na.rm = TRUE), .groups = "drop") %>%
        mutate(
          Row = factor(Row, levels = rev(row_levels)),
          Col = factor(Col, levels = col_levels),
          plateID = "All Plates")
      
      metrics_df <- uniformity_long %>%
        mutate(
          Row_num = match(Row, LETTERS),
          Col_num = as.numeric(Col))
      
      inner_wells <- metrics_df %>%
        filter(Row_num %in% 2:7, Col_num %in% 2:11)
      
      uniformity_metrics <- metrics_df %>%
        group_by(plateID) %>%
        summarise(
          Q25 = quantile(OD, 0.25, na.rm = TRUE),
          Median = quantile(OD, 0.50, na.rm = TRUE),
          Q75 = quantile(OD, 0.75, na.rm = TRUE),
          CV = cv(OD),
          Inner_CV = cv(inner_wells$OD[inner_wells$plateID == unique(plateID)]),
          Ratio_Max_Min = max(OD) / min(OD) * 100,
          Inner_Ratio_Max_Min =
            max(inner_wells$OD[inner_wells$plateID == unique(plateID)]) /
            min(inner_wells$OD[inner_wells$plateID == unique(plateID)]) * 100,
          .groups = "drop")
      
      overall_metrics <- metrics_df %>%
        summarise(
          plateID = "All Plates",
          Q25 = quantile(OD, 0.25),
          Median = quantile(OD, 0.50),
          Q75 = quantile(OD, 0.75),
          CV = cv(OD),
          Inner_CV = cv(inner_wells$OD),
          Ratio_Max_Min = max(OD) / min(OD) * 100,
          Inner_Ratio_Max_Min =
            max(inner_wells$OD) / min(inner_wells$OD) * 100)
      
      uniformity_metrics <- bind_rows(uniformity_metrics, overall_metrics)
    
    } else {
      uniformity_plates <- heatmap_df <- heatmap_list <- heatmap_avg <-
        heatmap_all_plates <- uniformity_metrics <- NULL}
    
    
    # Parallelism plates
    # -------------------
    if ("parallelism" %in% input$analyses) {
      
      serial_testing <- read.csv(input$serialtesting_file$datapath, stringsAsFactors = FALSE)
    
      serial_testing <- serial_testing %>%
        mutate(
          serial = case_when(
            grepl("-120", serialID, TRUE) ~ "120",
            grepl("SERA", serialID, TRUE) ~ "SerA",
            grepl("SERB", serialID, TRUE) ~ "SerB",
            grepl("PC",   serialID, TRUE) ~ "PC",
            grepl("NC",   serialID, TRUE) ~ "NC",
            grepl("MR",   serialID, TRUE) ~ "MR",
            TRUE ~ NA_character_),
          serial = factor(serial, levels = c("120","SerA","SerB","PC","NC","MR")))
      
      parallelism_plates <- serial_testing %>%
        filter(grepl("Parallelism", plateID, TRUE),
               serial %in% c("SerA","SerB","PC")) %>%
        arrange(serialID)
      
      # Dilution + layout
      # -------------------
      dilution <- read.csv(input$dilution_file$datapath, stringsAsFactors = FALSE)
      layout   <- read.csv(input$layout_file$datapath, stringsAsFactors = FALSE)
      
      dilution <- dilution %>%
        group_by(plateID) %>%
        mutate(RowLetter = LETTERS[row_number()]) %>%
        ungroup()
      
      layout <- layout %>%
        group_by(plateID) %>%
        mutate(RowLetter = LETTERS[row_number()]) %>%
        ungroup()
      
      dilution_long <- dilution %>%
        pivot_longer(starts_with("X"), names_to = "Column", values_to = "Dilution") %>%
        mutate(Column = as.integer(sub("X", "", Column)))
      
      layout_long <- layout %>%
        pivot_longer(starts_with("X"), names_to = "Column", values_to = "serialID") %>%
        mutate(Column = as.integer(sub("X", "", Column)))
      
      well_mapping <- layout_long %>%
        left_join(dilution_long, by = c("plateID","RowLetter","Column")) %>%
        mutate(
          serial = case_when(
            grepl("-120", serialID, TRUE) ~ "120",
            grepl("SERA", serialID, TRUE) ~ "SerA",
            grepl("SERB", serialID, TRUE) ~ "SerB",
            grepl("PC",   serialID, TRUE) ~ "PC",
            grepl("NC",   serialID, TRUE) ~ "NC",
            grepl("MR",   serialID, TRUE) ~ "MR",
            TRUE ~ NA_character_),
          serial = factor(serial, levels = c("120","SerA","SerB","PC","NC","MR")))
      
      start_dilutions <- well_mapping %>%
        group_by(plateID, serialID) %>%
        slice_min(Dilution, n = 1, with_ties = FALSE) %>%
        ungroup()
      
    } else {parallelism_plates <- start_dilutions <- NULL}
    
    
    # Ruggedness plates
    # -------------------
    if ("ruggedness" %in% input$analyses) {
      serial_testing <- read.csv(input$serialtesting_file$datapath, stringsAsFactors = FALSE)
      
      serial_testing <- serial_testing %>%
        mutate(
          serial = case_when(
            grepl("-120", serialID, TRUE) ~ "120",
            grepl("SERA", serialID, TRUE) ~ "SerA",
            grepl("SERB", serialID, TRUE) ~ "SerB",
            grepl("PC",   serialID, TRUE) ~ "PC",
            grepl("NC",   serialID, TRUE) ~ "NC",
            grepl("MR",   serialID, TRUE) ~ "MR",
            TRUE ~ NA_character_),
          serial = factor(serial, levels = c("120","SerA","SerB","PC","NC","MR")))
      
      ruggedness_plates <- serial_testing %>%
        filter(grepl("min|max|ruggedness", plateID, TRUE),
               serial %in% c("120","SerB","PC")) %>%
        arrange(serialID)
      
      ruggedness_min <- ruggedness_plates %>% filter(grepl("min", plateID, TRUE))
      ruggedness_max <- ruggedness_plates %>% filter(grepl("max", plateID, TRUE))
      
    } else {ruggedness_plates <- ruggedness_min <- ruggedness_max <- NULL}
    
  
    list(
      uniformity_plates = uniformity_plates,
      heatmap_df = heatmap_df,
      heatmap_list = heatmap_list,
      heatmap_avg = heatmap_avg,
      heatmap_all_plates = heatmap_all_plates,
      uniformity_metrics = uniformity_metrics,
      
      parallelism = if (!is.null(parallelism_plates)) {
        parallelism_plates %>%
          left_join(start_dilutions, by = c("plateID","serialID","serial")) %>%
          mutate(dilution_group = paste0(serial, " ", Dilution))
      } else NULL,
      
      ruggedness = ruggedness_plates,
      ruggedness_min = ruggedness_min,
      ruggedness_max = ruggedness_max)})
  
  # -------------------------------------------------
  # Uniformity UI
  # -------------------------------------------------
  output$uniformity_ui <- renderUI({
    req(input$run)
    tagList(
      h2("Uniformity"),
      h3("All Plate IDs"),
      DTOutput("uniformity_plate_ids"),
      h3("Uniformity - Heatmap All Plates"),
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
  
  # -------------------------------------------------
  # Uniformity plots
  # -------------------------------------------------
  
  output$uniformity_plate_ids <- renderDT({
    datatable(
      data_all()$uniformity_plates %>% distinct(plateID) %>% arrange(plateID),
      options = list(dom = "t"),
      rownames = FALSE)})
  
  # Heatmap Avg All Plates
  # -----------------------------
  output$heatmap_all <- renderPlot({
    df_avg <- data_all()$heatmap_all_plates
    stats <- data_all()$uniformity_metrics %>%
      filter(plateID == "All Plates") %>%
      slice(1)
    
    heatmap_plot <- ggplot(df_avg, aes(Col, Row, fill = OD)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(OD, 3)), size = 5) +
      geom_hline(yintercept = 1.5, linewidth = 1) +
      geom_vline(xintercept = 12.5, linewidth = 1) +
      coord_fixed() +
      scale_fill_gradient(low = "steelblue", high = "orange2") +
      scale_x_discrete(position = "top", expand = c(0,0)) +
      scale_y_discrete(expand = c(0,0)) +
      labs(title = "All Plates", fill = "Avg OD") +
      theme_minimal() +
      theme(panel.grid = element_blank())
    
    stats_plot <- ggplot() +
      annotate("text", x = 0, y = 1,
               label = format_plate_stats(stats),
               hjust = 0, vjust = 1, size = 5) +
      theme_void() +
      xlim(0,1) + ylim(0,1)
    
    heatmap_plot / stats_plot + plot_layout(heights = c(10,2))})
  
  
  # Heatmap By Plate
  # -----------------------------
  output$heatmap_by_plate <- renderUI({
    req(data_all())
    lapply(names(data_all()$heatmap_list), function(p) {
      plotOutput(paste0("hm_", p), height = "400px")}) %>% 
      tagList()})
  
  observe({
    req(data_all())
    hm_list <- data_all()$heatmap_list
    
    lapply(names(hm_list), function(p) {
      local({
        plate <- p
        
        output[[paste0("hm_", plate)]] <- renderPlot({
          df <- hm_list[[plate]]
          
          stats <- data_all()$uniformity_metrics %>%
            filter(plateID == plate) %>%
            slice(1)
          
          heatmap_plot <- ggplot(df, aes(Col, Row, fill = OD)) +
            geom_tile(color = "white") +
            geom_text(aes(label = round(OD, 3)), size = 5) +
            geom_hline(yintercept = 1.5, linewidth = 1) +
            geom_vline(xintercept = 12.5, linewidth = 1) +
            coord_fixed() +
            scale_fill_gradient(low = "steelblue", high = "orange2") +
            scale_x_discrete(position = "top", expand = c(0, 0)) +
            scale_y_discrete(expand = c(0, 0)) +
            labs(title = plate, fill = "OD") +
            theme_minimal() +
            theme(panel.grid = element_blank())
          
          stats_plot <- ggplot() +
            annotate("text", x = 0, y = 1,
              label = format_plate_stats(stats),
              hjust = 0, vjust = 1,  size = 5) +
            theme_void() +
            xlim(0, 1) + ylim(0, 1)
          
          heatmap_plot / stats_plot +
            plot_layout(heights = c(10, 2))})})})})
  
  
  # Statistics Table
  # -----------------------------
  output$stats_table <- renderDT({
    datatable(
      data_all()$uniformity_metrics %>%
        mutate(
          across(where(is.numeric), \(x) round(x, 3)),
          across(
            c(CV, Inner_CV, Ratio_Max_Min, Inner_Ratio_Max_Min),
            \(x) paste0(round(x, 1), "%"))),
      options = list(dom = "t"),
      rownames = FALSE)})
  
  
  # Line Graphs Avg OD by Row/Column
  # -----------------------------
  output$line_plots <- renderPlot({
    heatmap_df <- data_all()$heatmap_df
    row_levels <- levels(heatmap_df$Row)
    col_levels <- levels(heatmap_df$Col)
    
    line_graphs_data <- heatmap_df %>%
      filter(Row == "Avg" | Col == "Avg") %>%
      mutate(
        Row = factor(Row, levels = row_levels),
        Col_num = as.numeric(Col),
        Col_num = ifelse(
          is.na(Col_num),
          max(Col_num, na.rm = TRUE) + 1,
          Col_num))
    
    plate_ids <- unique(line_graphs_data$plateID)
    
    if (!"All Plates" %in% plate_ids) {
      
      plate_ids <- c("All Plates", plate_ids)
      
      all_plate_summary <- heatmap_df %>%
        filter(Row == "Avg" | Col == "Avg") %>%
        group_by(Row, Col) %>%
        summarise(OD = mean(OD, na.rm = TRUE), .groups = "drop") %>%
        mutate(
          Row = factor(Row, levels = row_levels),
          Col_num = as.numeric(Col),
          Col_num = ifelse(
            is.na(Col_num),
            max(Col_num, na.rm = TRUE) + 1,
            Col_num),
          plateID = "All Plates")
      
      line_graphs_data <- bind_rows(line_graphs_data, all_plate_summary)}
    
    plate_plots <- lapply(plate_ids, function(pid) {
      
      df <- filter(line_graphs_data, plateID == pid)
      
      p_col <- ggplot(
        df %>% filter(Col != "Avg"),
        aes(x = Col, y = OD, group = Row, color = Row)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        coord_cartesian(ylim = c(0, 2)) +
        scale_x_discrete(limits = col_levels) +
        labs(title = paste(pid, "OD by Column"),
          x = "Column", y = "Average OD") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "none")
      
      p_row <- ggplot(
        df %>% filter(Row != "Avg"),
        aes(x = Row, y = OD, group = Col, color = Col)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        coord_cartesian(ylim = c(0, 2)) +
        scale_x_discrete(limits = rev(row_levels)) +
        labs(title = paste(pid, "OD by Row"),
          x = "Row", y = "Average OD") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "none")

      p_col + p_row + plot_layout(ncol = 2)})
    
    wrap_plots(plate_plots, ncol = 1)},
  
    height = function() {
      plates <- length(unique(data_all()$heatmap_df$plateID)) + 1
      plates * 400})
  
  # -------------------------------------------------
  # Parallelism tables
  # -------------------------------------------------
  output$parallelism_plate_ids <- renderDT({
    datatable(
      data_all()$parallelism %>% distinct(plateID) %>% arrange(plateID),
      options = list(dom = "t"),
      rownames = FALSE)})
  
  output$parallelism_all <- renderDT({
    df <- data_all()$parallelism
    serials <- unique(df$serial)
    tbl <- df %>%
      group_by(serial) %>%
      summarise(
        AvgParmA = mean(ParmA_ratio, na.rm=TRUE),
        StdevParmA = sd(ParmA_ratio, na.rm=TRUE),
        CV_ParmA = cv(ParmA_ratio),
        AvgParmB = mean(ParmB_ratio, na.rm=TRUE),
        StdevParmB = sd(ParmB_ratio, na.rm=TRUE),
        CV_ParmB = cv(ParmB_ratio),
        AvgRP = mean(rp, na.rm=TRUE),
        StdevRP = sd(rp, na.rm=TRUE),
        CV_RP = cv(rp),
        SampleSize = sum(!is.na(ParmA_ratio) & !is.na(ParmB_ratio)),
        MarginError_ParmA = z_score * StdevParmA / sqrt(SampleSize),
        Lower_ParmA = AvgParmA - MarginError_ParmA,
        Upper_ParmA = AvgParmA + MarginError_ParmA,
        MarginError_ParmB = z_score * StdevParmB / sqrt(SampleSize),
        Lower_ParmB = AvgParmB - MarginError_ParmB,
        Upper_ParmB = AvgParmB + MarginError_ParmB,
        .groups="drop") %>%
      pivot_longer(-serial, names_to="Metric", values_to="Value") %>%
      pivot_wider(names_from=serial, values_from=Value) %>%
      mutate(across(-Metric, ~ mapply(format_metric, Metric, .x))) %>%
      bind_rows(tibble(Metric="CI", !!!setNames(rep(paste0(conf_level*100,"%"), length(serials)), serials)))
    
    datatable(tbl, options=list(dom="t", scrollX=TRUE), rownames = FALSE)})
  
  output$parallelism_combined <- renderDT({
    df <- data_all()$parallelism %>% filter(serial %in% c("SerA","SerB"))
    detected_dilutions <- sort(unique(df$Dilution))
    
    summarize_block <- function(x) {
      SampleSize <- sum(!is.na(x$ParmA_ratio) & !is.na(x$ParmB_ratio))
      AvgParmA <- mean(x$ParmA_ratio, na.rm=TRUE)
      StdevParmA <- sd(x$ParmA_ratio, na.rm=TRUE)
      AvgParmB <- mean(x$ParmB_ratio, na.rm=TRUE)
      StdevParmB <- sd(x$ParmB_ratio, na.rm=TRUE)
      tibble(
        AvgParmA, StdevParmA, CV_ParmA=cv(x$ParmA_ratio),
        AvgParmB, StdevParmB, CV_ParmB=cv(x$ParmB_ratio),
        SampleSize,
        MarginError_ParmA = z_score*StdevParmA/sqrt(SampleSize),
        Lower_ParmA = AvgParmA - z_score*StdevParmA/sqrt(SampleSize),
        Upper_ParmA = AvgParmA + z_score*StdevParmA/sqrt(SampleSize),
        MarginError_ParmB = z_score*StdevParmB/sqrt(SampleSize),
        Lower_ParmB = AvgParmB - z_score*StdevParmB/sqrt(SampleSize),
        Upper_ParmB = AvgParmB + z_score*StdevParmB/sqrt(SampleSize))}
    
    blocks <- c(list("SerA+SerB" = summarize_block(df)),
      setNames(lapply(detected_dilutions, 
        function(d) summarize_block(filter(df, Dilution == d))),
        paste0("SerA+SerB ", detected_dilutions)))
    
    tbl <- bind_cols(Metric = names(blocks[[1]]),
      lapply(blocks, unlist)) %>%
      mutate(across(-Metric, ~ mapply(format_metric, Metric, .x))) %>%
      bind_rows(tibble(Metric="CI", 
                       !!!setNames(rep(paste0(conf_level*100,"%"), ncol(.)-1), names(.)[-1])))
    
    datatable(tbl, options=list(dom="t", scrollX=TRUE), rownames = FALSE)})
  
  output$parallelism_by_serial <- renderDT({
    df <- data_all()$parallelism %>%
      filter(serial %in% c("SerA","SerB")) %>%
      group_by(serial, Dilution) %>%
      summarise(
        AvgParmA = mean(ParmA_ratio, na.rm=TRUE),
        StdevParmA = sd(ParmA_ratio, na.rm=TRUE),
        CV_ParmA = cv(ParmA_ratio),
        AvgParmB = mean(ParmB_ratio, na.rm=TRUE),
        StdevParmB = sd(ParmB_ratio, na.rm=TRUE),
        CV_ParmB = cv(ParmB_ratio),
        AvgRP = mean(rp, na.rm=TRUE),
        StdevRP = sd(rp, na.rm=TRUE),
        CV_RP = cv(rp),
        SampleSize = sum(!is.na(ParmA_ratio) & !is.na(ParmB_ratio)),
        MarginError_ParmA = z_score * StdevParmA / sqrt(SampleSize),
        Lower_ParmA = AvgParmA - MarginError_ParmA,
        Upper_ParmA = AvgParmA + MarginError_ParmA,
        MarginError_ParmB = z_score * StdevParmB / sqrt(SampleSize),
        Lower_ParmB = AvgParmB - MarginError_ParmB,
        Upper_ParmB = AvgParmB + z_score*StdevParmB/sqrt(SampleSize),
        .groups="drop") %>%
      unite(Group, serial, Dilution, sep=" ") %>%
      pivot_longer(-Group, names_to="Metric", values_to="Value") %>%
      pivot_wider(names_from=Group, values_from=Value) %>%
      mutate(across(-Metric, ~ mapply(format_metric, Metric, .x))) %>%
      bind_rows(tibble(Metric="CI", !!!setNames(rep(paste0(conf_level*100,"%"), ncol(.)-1), names(.)[-1])))
    
    datatable(df, options=list(dom="t", scrollX=TRUE), rownames = FALSE)})
  
  # -------------------------------------------------
  # Ruggedness tables
  # -------------------------------------------------
  output$ruggedness_all <- renderDT({
    df <- data_all()$ruggedness
    serials <- unique(df$serial)
    
    # Serial-level
    serial_summary <- df %>%
      group_by(serial) %>%
      summarise(
        AvgParmA = mean(ParmA_ratio, na.rm=TRUE),
        StdevParmA = sd(ParmA_ratio, na.rm=TRUE),
        CV_ParmA = StdevParmA / AvgParmA * 100,
        AvgParmB = mean(ParmB_ratio, na.rm=TRUE),
        StdevParmB = sd(ParmB_ratio, na.rm=TRUE),
        CV_ParmB = StdevParmB / AvgParmB * 100,
        AvgRP = mean(rp, na.rm=TRUE),
        StdevRP = sd(rp, na.rm=TRUE),
        CV_RP = StdevRP / AvgRP * 100,
        SampleSize = sum(!is.na(ParmA_ratio) & !is.na(ParmB_ratio)),
        MarginError_ParmA = z_score * StdevParmA / sqrt(SampleSize),
        Lower_ParmA = AvgParmA - MarginError_ParmA,
        Upper_ParmA = AvgParmA + MarginError_ParmA,
        MarginError_ParmB = z_score * StdevParmB / sqrt(SampleSize),
        Lower_ParmB = AvgParmB - MarginError_ParmB,
        Upper_ParmB = AvgParmB + MarginError_ParmB,
        .groups="drop")
    
    # Avg serial results
    avg_serials <- serial_summary %>%
      summarise(
        AvgParmA = mean(AvgParmA, na.rm=TRUE),
        StdevParmA = mean(StdevParmA, na.rm=TRUE),
        CV_ParmA = StdevParmA / AvgParmA * 100,
        AvgParmB = mean(AvgParmB, na.rm=TRUE),
        StdevParmB = mean(StdevParmB, na.rm=TRUE),
        CV_ParmB = StdevParmB / AvgParmB * 100,
        AvgRP = mean(AvgRP, na.rm=TRUE),
        StdevRP = mean(StdevRP, na.rm=TRUE),
        CV_RP = StdevRP / AvgRP * 100,
        SampleSize = sum(SampleSize),
        MarginError_ParmA = z_score * StdevParmA / sqrt(SampleSize),
        Lower_ParmA = AvgParmA - MarginError_ParmA,
        Upper_ParmA = AvgParmA + MarginError_ParmA,
        MarginError_ParmB = z_score * StdevParmB / sqrt(SampleSize),
        Lower_ParmB = AvgParmB - MarginError_ParmB,
        Upper_ParmB = AvgParmB + MarginError_ParmB) %>%
      pivot_longer(everything(), names_to="Metric", values_to="Avg_Serials")
    
    # All data (not by serial)
    all_plates <- df %>%
      summarise(
        AvgParmA = mean(ParmA_ratio, na.rm=TRUE),
        StdevParmA = sd(ParmA_ratio, na.rm=TRUE),
        CV_ParmA = StdevParmA / AvgParmA * 100,
        AvgParmB = mean(ParmB_ratio, na.rm=TRUE),
        StdevParmB = sd(ParmB_ratio, na.rm=TRUE),
        CV_ParmB = StdevParmB / AvgParmB * 100,
        AvgRP = mean(rp, na.rm=TRUE),
        StdevRP = sd(rp, na.rm=TRUE),
        CV_RP = StdevRP / AvgRP * 100,
        SampleSize = sum(!is.na(ParmA_ratio) & !is.na(ParmB_ratio)),
        MarginError_ParmA = z_score * StdevParmA / sqrt(SampleSize),
        Lower_ParmA = AvgParmA - MarginError_ParmA,
        Upper_ParmA = AvgParmA + MarginError_ParmA,
        MarginError_ParmB = z_score * StdevParmB / sqrt(SampleSize),
        Lower_ParmB = AvgParmB - MarginError_ParmB,
        Upper_ParmB = AvgParmB + MarginError_ParmB) %>%
      pivot_longer(everything(), names_to="Metric", values_to="All")
    
    tbl <- serial_summary %>%
      slice(match(serials, serial)) %>%
      pivot_longer(-serial, names_to="Metric", values_to="Value") %>%
      pivot_wider(names_from=serial, values_from=Value) %>%
      left_join(all_plates, by="Metric") %>%
      left_join(avg_serials, by="Metric")
    
    tbl <- tbl %>%
      mutate(across(-Metric, ~ mapply(format_metric, Metric, .x)))
    
    ci_row <- tibble(Metric="CI",
                     !!!setNames(rep(paste0(conf_level*100,"%"), ncol(tbl)-1), names(tbl)[-1]))
    tbl <- bind_rows(tbl, ci_row)
    
    datatable(tbl, options=list(dom="t", scrollX=TRUE), rownames=FALSE)})
  
  output$ruggedness_plate_ids <- renderDT({
    datatable(
      data_all()$ruggedness %>% distinct(plateID) %>% arrange(plateID),
      options = list(dom = "t"),
      rownames = FALSE)})
  
  output$ruggedness_min_table <- renderDT({
    df <- data_all()$ruggedness_min
    datatable(
      make_ruggedness_min_table(df),
      options = list(scrollX = TRUE, dom = "t"), 
      rownames=FALSE)})
  
  output$ruggedness_min_temp_table <- renderDT({
    df <- data_all()$ruggedness_min %>% filter(grepl("temp", plateID, ignore.case = TRUE))
    datatable(
      make_ruggedness_min_table(df),
      options = list(scrollX = TRUE, dom = "t"),
      rownames=FALSE,
      caption = "Min Temp")})
  
  output$ruggedness_min_time_table <- renderDT({
    df <- data_all()$ruggedness_min %>% filter(grepl("time", plateID, ignore.case = TRUE))
    datatable(
      make_ruggedness_min_table(df),
      options = list(scrollX = TRUE, dom = "t"),
      rownames=FALSE,
      caption = "Min Time")})
  
  output$ruggedness_max_table <- renderDT({
    df <- data_all()$ruggedness_max
    datatable(
      make_ruggedness_max_table(df),
      options = list(scrollX = TRUE, dom = "t"),
      rownames=FALSE)})
  
  output$ruggedness_max_temp_table <- renderDT({
    df <- data_all()$ruggedness_max %>% filter(grepl("temp", plateID, ignore.case = TRUE))
    datatable(
      make_ruggedness_max_table(df),
      options = list(scrollX = TRUE, dom = "t"),
      rownames=FALSE,
      caption = "Max Temp")})
  
  output$ruggedness_max_time_table <- renderDT({
    df <- data_all()$ruggedness_max %>% filter(grepl("time", plateID, ignore.case = TRUE))
    datatable(
      make_ruggedness_max_table(df),
      options = list(scrollX = TRUE, dom = "t"),
      rownames=FALSE,
      caption = "Max Time")})}


# -------------------------------------------------
# Run app
# -------------------------------------------------
shinyApp(ui, server)

