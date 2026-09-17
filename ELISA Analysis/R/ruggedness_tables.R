make_ruggedness_table <- function(data, z_score, conf_level) {
  req(nrow(data) > 0)
  
  ruggedness_summary <- data %>%
    group_by(serial) %>%
    summarise(
      AvgParmA = mean(ParmA_ratio, na.rm = TRUE),
      StdevParmA = sd(ParmA_ratio, na.rm = TRUE),
      CV_ParmA = ifelse(AvgParmA == 0, NA, StdevParmA / AvgParmA * 100),
      AvgParmB = mean(ParmB_ratio, na.rm = TRUE),
      StdevParmB = sd(ParmB_ratio, na.rm = TRUE),
      CV_ParmB = StdevParmB / AvgParmB * 100,
      AvgRP = mean(rp, na.rm = TRUE),
      StdevRP = sd(rp, na.rm = TRUE),
      CV_RP = StdevRP / AvgRP * 100,
      
      SampleSize = sum(!is.na(ParmA_ratio) & !is.na(ParmB_ratio)),
      MarginError_ParmA =  z_score * StdevParmA / sqrt(SampleSize),
      Lower_ParmA = AvgParmA - MarginError_ParmA,
      Upper_ParmA = AvgParmA + MarginError_ParmA,
      MarginError_ParmB =  z_score * StdevParmB / sqrt(SampleSize),
      Lower_ParmB = AvgParmB - MarginError_ParmB,
      Upper_ParmB = AvgParmB + MarginError_ParmB,
      .groups = "drop")
  
  ruggedness_table <- ruggedness_summary %>%
    pivot_longer(-serial, names_to = "Metric", values_to = "Value") %>%
    pivot_wider(names_from = serial, values_from = Value)
  
  ruggedness_table <- ruggedness_table %>%
    mutate(across(-Metric,
        ~case_when(Metric %in% c("Lower_ParmA", "Upper_ParmA", "Lower_ParmB", "Upper_ParmB"
          ) ~ formatC(.x, format = "f", digits = 1),
          
          Metric %in% c("CV_ParmA", "CV_ParmB", "CV_RP"
          ) ~ paste0(formatC(.x, format = "f", digits = 2), "%"),
          
          Metric == "SampleSize" ~ as.character(.x),
          
          TRUE ~ formatC(.x, format = "f", digits = 3))))
  
  ci_row <- tibble(
    Metric = "CI",
    !!!setNames(
      rep(paste0(conf_level * 100, "%"), ncol(ruggedness_table) - 1),
      names(ruggedness_table)[-1]))
  
  bind_rows(ruggedness_table, ci_row) %>%
    arrange(factor(Metric,
        levels = c("AvgParmA", "StdevParmA", "CV_ParmA", "AvgParmB", "StdevParmB", "CV_ParmB", "AvgRP", "StdevRP", "CV_RP",
          "SampleSize", "CI", "MarginError_ParmA", "Lower_ParmA", "Upper_ParmA", "MarginError_ParmB", "Lower_ParmB", "Upper_ParmB")))}

ruggedness_tables <- function(input, output, session, data_all, z_score, conf_level){
  
  output$ruggedness_plate_ids <- renderDT({
    datatable(
      data_all()$ruggedness %>% distinct(plateID) %>% arrange(plateID),
      options = list(dom = "t"),
      rownames = FALSE)})
  
  output$ruggedness_min_table <- renderDT({
    df <- data_all()$ruggedness_min
    datatable(
      make_ruggedness_table(df, z_score = z_score, conf_level = conf_level),
      options = list(scrollX = TRUE, dom = "t"), 
      rownames=FALSE)})
  
  output$ruggedness_min_temp_table <- renderDT({
    df <- data_all()$ruggedness_min %>% filter(grepl("temp", plateID, ignore.case = TRUE))
    datatable(
      make_ruggedness_table(df, z_score = z_score, conf_level = conf_level),
      options = list(scrollX = TRUE, dom = "t"),
      rownames=FALSE,
      caption = "Min Temp")})
  
  output$ruggedness_min_time_table <- renderDT({
    df <- data_all()$ruggedness_min %>% filter(grepl("time", plateID, ignore.case = TRUE))
    datatable(
      make_ruggedness_table(df, z_score = z_score, conf_level = conf_level),
      options = list(scrollX = TRUE, dom = "t"),
      rownames=FALSE,
      caption = "Min Time")})
  
  output$ruggedness_max_table <- renderDT({
    df <- data_all()$ruggedness_max
    datatable(
      make_ruggedness_table(df, z_score = z_score, conf_level = conf_level),
      options = list(scrollX = TRUE, dom = "t"),
      rownames=FALSE)})
  
  output$ruggedness_max_temp_table <- renderDT({
    df <- data_all()$ruggedness_max %>% filter(grepl("temp", plateID, ignore.case = TRUE))
    datatable(
      make_ruggedness_table(df, z_score = z_score, conf_level = conf_level),
      options = list(scrollX = TRUE, dom = "t"),
      rownames=FALSE,
      caption = "Max Temp")})
  
  output$ruggedness_max_time_table <- renderDT({
    df <- data_all()$ruggedness_max %>% filter(grepl("time", plateID, ignore.case = TRUE))
    datatable(
      make_ruggedness_table(df, z_score = z_score, conf_level = conf_level),
      options = list(scrollX = TRUE, dom = "t"),
      rownames=FALSE,
      caption = "Max Time")})
  
  output$ruggedness_all <- renderDT({
    df <- data_all()$ruggedness
    serials <- unique(df$serial)
    
    # Serial-level
    serial_summary <- df %>%
      group_by(serial) %>%
      summarise(
        AvgParmA = mean(ParmA_ratio, na.rm=TRUE),
        StdevParmA = sd(ParmA_ratio, na.rm=TRUE),
        CV_ParmA = ifelse(AvgParmA == 0, NA, StdevParmA / AvgParmA * 100),
        AvgParmB = mean(ParmB_ratio, na.rm=TRUE),
        StdevParmB = sd(ParmB_ratio, na.rm=TRUE),
        CV_ParmB = StdevParmB / AvgParmB * 100,
        AvgRP = mean(rp, na.rm=TRUE),
        StdevRP = sd(rp, na.rm=TRUE),
        CV_RP = StdevRP / AvgRP * 100,
        SampleSize = sum(!is.na(ParmA_ratio) & !is.na(ParmB_ratio)),
        MarginError_ParmA =  z_score * StdevParmA / sqrt(SampleSize),
        Lower_ParmA = AvgParmA - MarginError_ParmA,
        Upper_ParmA = AvgParmA + MarginError_ParmA,
        MarginError_ParmB =  z_score * StdevParmB / sqrt(SampleSize),
        Lower_ParmB = AvgParmB - MarginError_ParmB,
        Upper_ParmB = AvgParmB + MarginError_ParmB,
        .groups="drop")
    
    # Avg serial results
    avg_serials <- serial_summary %>%
      summarise(
        AvgParmA = mean(AvgParmA, na.rm=TRUE),
        StdevParmA = mean(StdevParmA, na.rm=TRUE),
        CV_ParmA = ifelse(AvgParmA == 0, NA, StdevParmA / AvgParmA * 100),
        AvgParmB = mean(AvgParmB, na.rm=TRUE),
        StdevParmB = mean(StdevParmB, na.rm=TRUE),
        CV_ParmB = StdevParmB / AvgParmB * 100,
        AvgRP = mean(AvgRP, na.rm=TRUE),
        StdevRP = mean(StdevRP, na.rm=TRUE),
        CV_RP = StdevRP / AvgRP * 100,
        SampleSize = sum(SampleSize),
        MarginError_ParmA =  z_score * StdevParmA / sqrt(SampleSize),
        Lower_ParmA = AvgParmA - MarginError_ParmA,
        Upper_ParmA = AvgParmA + MarginError_ParmA,
        MarginError_ParmB =  z_score * StdevParmB / sqrt(SampleSize),
        Lower_ParmB = AvgParmB - MarginError_ParmB,
        Upper_ParmB = AvgParmB + MarginError_ParmB) %>%
      pivot_longer(everything(), names_to="Metric", values_to="Avg_Serials")
    
    # All data (not by serial)
    all_plates <- df %>%
      summarise(
        AvgParmA = mean(ParmA_ratio, na.rm=TRUE),
        StdevParmA = sd(ParmA_ratio, na.rm=TRUE),
        CV_ParmA = ifelse(AvgParmA == 0, NA, StdevParmA / AvgParmA * 100),
        AvgParmB = mean(ParmB_ratio, na.rm=TRUE),
        StdevParmB = sd(ParmB_ratio, na.rm=TRUE),
        CV_ParmB = StdevParmB / AvgParmB * 100,
        AvgRP = mean(rp, na.rm=TRUE),
        StdevRP = sd(rp, na.rm=TRUE),
        CV_RP = StdevRP / AvgRP * 100,
        SampleSize = sum(!is.na(ParmA_ratio) & !is.na(ParmB_ratio)),
        MarginError_ParmA =  z_score * StdevParmA / sqrt(SampleSize),
        Lower_ParmA = AvgParmA - MarginError_ParmA,
        Upper_ParmA = AvgParmA + MarginError_ParmA,
        MarginError_ParmB =  z_score * StdevParmB / sqrt(SampleSize),
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
  }
  
