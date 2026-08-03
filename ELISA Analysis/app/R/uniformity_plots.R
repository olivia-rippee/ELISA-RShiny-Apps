uniformity_plots <- function(input, output, session, data_all) {

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
      geom_text(aes(label = round(OD, 2)), size = 5) +
      geom_hline(yintercept = 1.5, linewidth = 1) +
      geom_vline(xintercept = 12.5, linewidth = 1) +
      coord_fixed() +
      scale_fill_gradient(low = "steelblue", high = "orange2") +
      scale_x_discrete(position = "top", expand = c(0,0)) +
      scale_y_discrete(expand = c(0,0)) +
      labs(title = "All Plates") +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 14),
            plot.title = element_text(size = 16))
    
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
      plotOutput(paste0("hm_", p), height = "500px")}) %>% 
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
                  legend.title = element_text(size = 14),
                  plot.title = element_text(size = 16))
          
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
      filter((Row == "Avg" | Col == "Avg") & !(Row == "Avg" & Col == "Avg")) %>%
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
      plates * 400})}