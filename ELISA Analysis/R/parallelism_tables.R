parallelism_tables <- function(input, output, session, data_all, z_score, conf_level) {
  
  # --------------------------------------------------
  # All Plate IDs
  # --------------------------------------------------
  output$parallelism_plate_ids <- renderDT({
    datatable(
      data_all()$parallelism %>%
        distinct(plateID) %>%
        arrange(plateID),
      options = list(dom = "t"),
      rownames = FALSE)})
  
  # --------------------------------------------------
  # Parallelism - All Starting Dilutions
  # --------------------------------------------------
  output$parallelism_all <- renderDT({
    df <- data_all()$parallelism
    
    tbl <- df %>%
      group_by(serial) %>%
      summarise(
        AvgParmA = mean(ParmA_ratio, na.rm = TRUE),
        StdevParmA = sd(ParmA_ratio, na.rm = TRUE),
        CV_ParmA = cv(ParmA_ratio),
        
        AvgParmB = mean(ParmB_ratio, na.rm = TRUE),
        StdevParmB = sd(ParmB_ratio, na.rm = TRUE),
        CV_ParmB = cv(ParmB_ratio),
        
        AvgRP = mean(rp, na.rm = TRUE),
        StdevRP = sd(rp, na.rm = TRUE),
        CV_RP = cv(rp),
        
        nA = sum(!is.na(ParmA_ratio)),
        nB = sum(!is.na(ParmB_ratio)),
        .groups = "drop") %>%
      
      mutate(
        MarginError_ParmA = z_score() * StdevParmA / sqrt(nA),
        Lower_ParmA = AvgParmA - MarginError_ParmA,
        Upper_ParmA = AvgParmA + MarginError_ParmA,
        
        MarginError_ParmB = z_score() * StdevParmB / sqrt(nB),
        Lower_ParmB = AvgParmB - MarginError_ParmB,
        Upper_ParmB = AvgParmB + MarginError_ParmB,
        
        SampleSize = pmin(nA, nB)) %>%
      
      select(serial, AvgParmA, StdevParmA, CV_ParmA,
        AvgParmB, StdevParmB, CV_ParmB,
        AvgRP, StdevRP, CV_RP, SampleSize,
        MarginError_ParmA, Lower_ParmA, Upper_ParmA,
        MarginError_ParmB, Lower_ParmB, Upper_ParmB) %>%
      
      pivot_longer(cols = -serial, names_to = "Metric", values_to = "Value") %>%
      pivot_wider(names_from = serial, values_from = Value) %>%
      mutate(across(-Metric, ~ mapply(format_metric, Metric, .x)))
    
    # Add confidence level row
    ci <- tibble(Metric = "CI")
    
    for (nm in names(tbl)[-1]) {
      ci[[nm]] <- paste0(conf_level() * 100, "%")}
    
    tbl <- bind_rows(tbl, ci)

  DT::datatable(tbl, rownames = FALSE,
    options = list(paging = FALSE, searching = FALSE, ordering = FALSE,
      info = FALSE, scrollX = TRUE, dom = "t"))}) 
  
  # --------------------------------------------------
  # Parallelism - Combined Serial A + Serial B
  # --------------------------------------------------
  output$parallelism_combined <- renderDT({
    
    df <- data_all()$parallelism %>%
      filter(serial %in% c("SerA", "SerB"))
    
    detected_dilutions <- sort(unique(na.omit(df$Dilution)))
    
    summarize_block <- function(x) {
      
      SampleSize <- sum(!is.na(x$ParmA_ratio) & !is.na(x$ParmB_ratio))
      
      AvgParmA <- mean(x$ParmA_ratio, na.rm = TRUE)
      StdevParmA <- sd(x$ParmA_ratio, na.rm = TRUE)
      
      AvgParmB <- mean(x$ParmB_ratio, na.rm = TRUE)
      StdevParmB <- sd(x$ParmB_ratio, na.rm = TRUE)
      
      tibble(
        AvgParmA = AvgParmA,
        StdevParmA = StdevParmA,
        CV_ParmA = cv(x$ParmA_ratio),
        
        AvgParmB = AvgParmB,
        StdevParmB = StdevParmB,
        CV_ParmB = cv(x$ParmB_ratio),
        
        SampleSize = SampleSize,
        
        MarginError_ParmA = z_score() * StdevParmA / sqrt(SampleSize),
        Lower_ParmA = AvgParmA - z_score() * StdevParmA / sqrt(SampleSize),
        Upper_ParmA = AvgParmA + z_score() * StdevParmA / sqrt(SampleSize),
        
        MarginError_ParmB = z_score() * StdevParmB / sqrt(SampleSize),
        Lower_ParmB = AvgParmB - z_score() * StdevParmB / sqrt(SampleSize),
        Upper_ParmB = AvgParmB + z_score() * StdevParmB / sqrt(SampleSize))}
    
    # Overall combined SerA + SerB
    blocks <- list("SerA+SerB" = summarize_block(df))
    
    # Separate dilution results
    if (length(detected_dilutions) > 0) {
      
      dilution_blocks <- setNames(
        lapply(detected_dilutions, function(d) {
          dilution_df <- filter(df, !is.na(Dilution), Dilution == d)
          summarize_block(dilution_df)}),
        paste0("SerA+SerB ", detected_dilutions))
      
      # Remove dilution-specific column when every serial is the same dilution
      dilution_blocks <- dilution_blocks[
        vapply(detected_dilutions, function(d) {
          nrow(filter(df, !is.na(Dilution), Dilution == d)) < nrow(df)
        }, logical(1))]
      
      blocks <- c(blocks, dilution_blocks)}
    
    # Convert summaries to table
    tbl <- bind_rows(lapply(names(blocks), function(nm) {
        
      blocks[[nm]] %>%
          pivot_longer(cols = everything(), names_to = "Metric", values_to = "Value") %>%
          mutate(Group = nm)})) %>%
      
      pivot_wider(names_from = Group, values_from = Value) %>%
      mutate(across(-Metric, ~ mapply(format_metric, Metric, .x)))
    
    # Add confidence level row
    ci <- tibble(Metric = "CI")
    
    for (nm in names(tbl)[-1]) {
      ci[[nm]] <- paste0(conf_level() * 100, "%")}
    
    tbl <- bind_rows(tbl, ci)
    
    datatable(tbl, rownames = FALSE,
      options = list(dom = "t", scrollX = TRUE, paging = TRUE,
        pageLength = 25, lengthChange = FALSE))})
  
  # --------------------------------------------------
  # Parallelism - By Serial and Starting Dilution
  # --------------------------------------------------
  output$parallelism_by_serial <- renderDT({
    
    df <- data_all()$parallelism %>%
      filter(serial %in% c("SerA", "SerB"), !is.na(Dilution))
    
    req(nrow(df) > 0)
    
    tbl <- df %>%
      group_by(serial, Dilution) %>%
      summarise(
        AvgParmA = mean(ParmA_ratio, na.rm = TRUE),
        StdevParmA = sd(ParmA_ratio, na.rm = TRUE),
        CV_ParmA = cv(ParmA_ratio),
        
        AvgParmB = mean(ParmB_ratio, na.rm = TRUE),
        StdevParmB = sd(ParmB_ratio, na.rm = TRUE),
        CV_ParmB = cv(ParmB_ratio),
        
        AvgRP = mean(rp, na.rm = TRUE),
        StdevRP = sd(rp, na.rm = TRUE),
        CV_RP = cv(rp),
        
        n = sum(!is.na(ParmA_ratio) & !is.na(ParmB_ratio)),
        .groups = "drop") %>%
      
      mutate(
        MarginError_ParmA = z_score() * StdevParmA / sqrt(n),
        Lower_ParmA = AvgParmA - MarginError_ParmA,
        Upper_ParmA = AvgParmA + MarginError_ParmA,
        
        MarginError_ParmB = z_score() * StdevParmB / sqrt(n),
        Lower_ParmB = AvgParmB - MarginError_ParmB,
        Upper_ParmB = AvgParmB + MarginError_ParmB,
        
        SampleSize = n,
        Group = paste(serial, Dilution)) %>%
      
      select(Group, AvgParmA, StdevParmA, CV_ParmA,
        AvgParmB, StdevParmB, CV_ParmB,
        AvgRP, StdevRP, CV_RP, SampleSize,
        MarginError_ParmA, Lower_ParmA, Upper_ParmA,
        MarginError_ParmB, Lower_ParmB, Upper_ParmB) %>%
      
      pivot_longer(cols = -Group, names_to = "Metric", values_to = "Value") %>%
      pivot_wider(names_from = Group, values_from = Value) %>%
      mutate(across(-Metric, ~ mapply(format_metric, Metric, .x)))
    
    # Add confidence level row
    ci <- tibble(Metric = "CI")
    
    for (nm in names(tbl)[-1]) {
      ci[[nm]] <- paste0(conf_level() * 100, "%")}
    
    tbl <- bind_rows(tbl, ci)
    
    datatable(tbl, rownames = FALSE,
      options = list(dom = "t", scrollX = TRUE, paging = TRUE,
        pageLength = 25, lengthChange = FALSE))})}
