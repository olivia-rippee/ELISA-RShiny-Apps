library(shiny)
library(tidyverse)
library(DT)
library(patchwork)

# -------------------------------------------------
# Helper functions
# -------------------------------------------------
read_uploaded_file <- function(file_input) {
  file_ext <- tolower(tools::file_ext(file_input$name))
  
  df <- if (file_ext == "csv") {
    read.csv(file_input$datapath, stringsAsFactors = FALSE)
  } else if (file_ext %in% c("xlsx", "xls")) {
    as.data.frame(read_excel(file_input$datapath),
                  stringsAsFactors = FALSE)
  } else {
    stop("Unsupported file type. Please upload a CSV or Excel file.")}
  
  # Standardize column names
  names(df) <- make.names(names(df))
  df}

cv <- function(x) {sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100}

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
  titlePanel("ELISA Analysis - Uniformity"),
  
  # OD
  h5(strong(style = "width:100%;", "Upload OD File (.csv or .xlsx) with columns 1-12 and plateID")),
  fileInput("od_file", label = NULL, accept = c(".csv", ".xlsx")),

  radioButtons(
    "uniformity_scope",
    "Uniformity scope:",
    choices = c(
      "Only plates with uniformity in plateID" = "uniformity_only",
      "All plates" = "all"),
    selected = "uniformity_only"),
  actionButton("run", "Run Analysis", class = "btn-primary"),
  actionButton("clear", "Clear"),
  hr(),
  
  conditionalPanel(
    condition = "input.run > 0",
    h3("Plate IDs"),
    DTOutput("plate_ids"),
    hr(),
    h3("Heatmap (Average Over All Plates)"),
    plotOutput("heatmap_all", height = "500px"),
    hr(),
    h3("Heatmap by Plate"),
    uiOutput("heatmap_by_plate"),
    hr(),
    h3("Statistics Table"),
    DTOutput("stats_table"),
    hr(),
    h3("Row / Column Line Graphs"),
    plotOutput("line_plots", height = "800px")))

# -------------------------------------------------
# Server
# -------------------------------------------------
server <- function(input, output, session) {
  
  observeEvent(input$clear, session$reload())
  
  # -----------------------------
  # Reactive data
  # -----------------------------
  data_all <- eventReactive(input$run, {
    req(input$od_file)
    
    ODs <- read_uploaded_file(input$od_file)
    colnames(ODs) <- sub("^X(?=\\d)", "", colnames(ODs), perl = TRUE)
    
    # Optional filtering
    uniformity_plates <- ODs %>%
      { if (input$uniformity_scope == "uniformity_only") {
        filter(., grepl("uniformity", plateID, ignore.case = TRUE))
      } else .} %>%
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
    
    # -----------------------------
    # Heatmap components
    # -----------------------------
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
    
    # -----------------------------
    # All plates heatmap
    # -----------------------------
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
    
    # -----------------------------
    # Metrics
    # -----------------------------
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
    
    # Overall row
    overall_metrics <- metrics_df %>%
      summarise(
        plateID = "All Plates",
        Q25 = quantile(OD, 0.25, na.rm = TRUE),
        Median = quantile(OD, 0.50, na.rm = TRUE),
        Q75 = quantile(OD, 0.75, na.rm = TRUE),
        CV = cv(OD),
        Inner_CV = cv(inner_wells$OD),
        Ratio_Max_Min = max(OD) / min(OD) * 100,
        Inner_Ratio_Max_Min = max(inner_wells$OD) / min(inner_wells$OD) * 100)
    
    uniformity_metrics <- bind_rows(uniformity_metrics, overall_metrics)
    
    list(
      plates = uniformity_plates,
      heatmap_df = heatmap_df,
      heatmap_list = heatmap_list,
      heatmap_all = heatmap_all_plates,
      metrics = uniformity_metrics)})
  
  # -----------------------------
  # Plate IDs
  # -----------------------------
  output$plate_ids <- renderDT({
    datatable(
      data_all()$plates %>% distinct(plateID),
      options = list(dom = "t"),
      rownames = FALSE)})
  
  # -----------------------------
  # Heatmap ALL
  # -----------------------------
  output$heatmap_all <- renderPlot({
    df <- data_all()$heatmap_all
    
    stats <- data_all()$metrics %>%
      filter(plateID == "All Plates") %>%
      slice(1)
    
    p1 <- ggplot(df, aes(Col, Row, fill = OD)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(OD, 2)), size = 5) +
      geom_hline(yintercept = 1.5, linewidth = 1) +
      geom_vline(xintercept = 12.5, linewidth = 1) +
      coord_fixed() +
      scale_fill_gradient(low = "steelblue", high = "orange") +
      scale_x_discrete(position = "top", expand = c(0,0)) +
      scale_y_discrete(expand = c(0,0)) +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 14),
            plot.title = element_text(size = 16)) +
      labs(title = "All Plates")
    
    p2 <- ggplot() +
      annotate("text", x = 0, y = 1,
               label = format_plate_stats(stats),
               hjust = 0, vjust = 1, size = 5) +
      theme_void()
    
    p1 / p2 + plot_layout(heights = c(10, 2))})
  
  # -----------------------------
  # Heatmap by plate
  # -----------------------------
  output$heatmap_by_plate <- renderUI({
    req(data_all())
    
    lapply(names(data_all()$heatmap_list), function(p) {
      plotOutput(paste0("hm_", p), height = "500px")}) %>% tagList()})
  
  observe({
    req(data_all())
    hm_list <- data_all()$heatmap_list
    
    lapply(names(hm_list), function(p) {
      local({
        plate <- p
        
        output[[paste0("hm_", plate)]] <- renderPlot({
          
          df <- hm_list[[plate]]
          
          stats <- data_all()$metrics %>%
            filter(plateID == plate) %>%
            slice(1)
          
          p1 <- ggplot(df, aes(Col, Row, fill = OD)) +
            geom_tile(color = "white") +
            geom_text(aes(label = round(OD, 2)), size = 5) +
            geom_hline(yintercept = 1.5, linewidth = 1) +
            geom_vline(xintercept = 12.5, linewidth = 1) +
            coord_fixed() +
            scale_fill_gradient(low = "steelblue", high = "orange2") +
            scale_x_discrete(position = "top", expand = c(0, 0)) +
            scale_y_discrete(expand = c(0, 0)) +
            labs(title = plate, fill = "OD") +
            theme_minimal() +
            theme(panel.grid = element_blank(),
                  axis.text.x = element_text(size = 14),
                  axis.text.y = element_text(size = 14),
                  axis.title.x = element_text(size = 16),
                  axis.title.y = element_text(size = 16),
                  legend.text = element_text(size = 12),
                  legend.title = element_text(size = 14))
          
          p2 <- ggplot() +
            annotate("text", x = 0, y = 1,
                     label = format_plate_stats(stats),
                     hjust = 0, vjust = 1) +
            theme_void()
          
          p1 / p2 + plot_layout(heights = c(10, 2))})})})})
  
  # -----------------------------
  # Stats table
  # -----------------------------
  output$stats_table <- renderDT({
    datatable(
      data_all()$metrics %>%
        mutate(across(where(is.numeric), ~ round(.x, 3)),
               across(
                 c(CV, Inner_CV, Ratio_Max_Min, Inner_Ratio_Max_Min),
                 \(x) paste0(round(x, 1), "%"))),
      options = list(dom = "t"),
      rownames = FALSE)})
  
  # -----------------------------
  # Line plots
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
        scale_x_discrete(limits = setdiff(col_levels, "Avg")) +
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
        scale_x_discrete(limits = rev(setdiff(row_levels, "Avg"))) +
        labs(title = paste(pid, "OD by Row"),
             x = "Row", y = "Average OD") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "none")
      
      p_col + p_row + plot_layout(ncol = 2)})
    
    wrap_plots(plate_plots, ncol = 1)},
    
    height = function() {
      plates <- length(unique(data_all()$heatmap_df$plateID)) + 1
      plates * 400})}

# -------------------------------------------------
# Run app
# -------------------------------------------------
shinyApp(ui, server)
