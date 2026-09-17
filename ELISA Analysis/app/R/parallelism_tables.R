parallelism_tables <- function(input, output, session, data_all, z_score, conf_level){
  
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
        MarginError_ParmA =   z_score * StdevParmA / sqrt(SampleSize),
        Lower_ParmA = AvgParmA - MarginError_ParmA,
        Upper_ParmA = AvgParmA + MarginError_ParmA,
        MarginError_ParmB =   z_score * StdevParmB / sqrt(SampleSize),
        Lower_ParmB = AvgParmB - MarginError_ParmB,
        Upper_ParmB = AvgParmB + MarginError_ParmB,
        .groups="drop") %>%
      pivot_longer(-serial, names_to="Metric", values_to="Value") %>%
      pivot_wider(names_from=serial, values_from=Value) %>%
      mutate(across(-Metric, ~ mapply(format_metric, Metric, .x))) %>%
      bind_rows(tibble(Metric="CI", !!!setNames(rep(paste0(conf_level*100,"%"), length(serials)), serials)))
    
    datatable(tbl, options=list(dom="t", scrollX=TRUE), rownames = FALSE)})
  
  output$parallelism_combined <- renderDT({
    df <- data_all()$parallelism %>%
      filter(serial %in% c("SerA", "SerB"))
    
    # Only use detected starting dilutions
    # NA means no additional starting dilution was detected
    detected_dilutions <- sort(unique(na.omit(df$Dilution)))
    
    summarize_block <- function(x) {
      SampleSize <- sum(!is.na(x$ParmA_ratio) & !is.na(x$ParmB_ratio))
      AvgParmA   <- mean(x$ParmA_ratio, na.rm = TRUE)
      StdevParmA <- sd(x$ParmA_ratio, na.rm = TRUE)
      AvgParmB   <- mean(x$ParmB_ratio, na.rm = TRUE)
      StdevParmB <- sd(x$ParmB_ratio, na.rm = TRUE)
      
      tibble(AvgParmA, StdevParmA, CV_ParmA = cv(x$ParmA_ratio),
        AvgParmB, StdevParmB, CV_ParmB = cv(x$ParmB_ratio),
        SampleSize,
        
        MarginError_ParmA = z_score * StdevParmA / sqrt(SampleSize),
        Lower_ParmA = AvgParmA - z_score * StdevParmA / sqrt(SampleSize),
        Upper_ParmA = AvgParmA + z_score * StdevParmA / sqrt(SampleSize),
        
        MarginError_ParmB = z_score * StdevParmB / sqrt(SampleSize),
        Lower_ParmB = AvgParmB - z_score * StdevParmB / sqrt(SampleSize),
        Upper_ParmB = AvgParmB + z_score * StdevParmB / sqrt(SampleSize))}
    
    # Always calculate the overall SerA + SerB result
    blocks <- list("SerA+SerB" = summarize_block(df))
    
    # Only add dilution-specific columns when starting dilutions were detected
    if (length(detected_dilutions) > 0) {
      
      dilution_blocks <- setNames(
        lapply(detected_dilutions, function(d) {
            summarize_block(filter(df, !is.na(Dilution), Dilution == d))}),
        paste0("SerA+SerB ", detected_dilutions))
      
      blocks <- c(blocks, dilution_blocks)}
    
    tbl <- bind_cols(Metric = names(blocks[[1]]), lapply(blocks, unlist)) %>%
      mutate(across(-Metric,  ~ mapply(format_metric, Metric, .x))) %>%
      bind_rows(tibble(Metric = "CI", !!!setNames(
            rep(paste0(conf_level * 100, "%"), ncol(.) - 1), names(.)[-1])))
    
    datatable(tbl, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)})
  

  output$parallelism_by_serial <- renderDT({
    df <- data_all()$parallelism %>%
      filter(serial %in% c("SerA","SerB"), !is.na(Dilution))
    
    req(nrow(df) > 0)
    
    tbl <- df %>%
      group_by(serial, Dilution) %>%
      summarise(
        AvgParmA=mean(ParmA_ratio,na.rm=TRUE), StdevParmA=sd(ParmA_ratio,na.rm=TRUE), CV_ParmA=cv(ParmA_ratio),
        AvgParmB=mean(ParmB_ratio,na.rm=TRUE), StdevParmB=sd(ParmB_ratio,na.rm=TRUE), CV_ParmB=cv(ParmB_ratio),
        AvgRP=mean(rp,na.rm=TRUE), StdevRP=sd(rp,na.rm=TRUE), CV_RP=cv(rp),
        SampleSize=sum(!is.na(ParmA_ratio)&!is.na(ParmB_ratio)), .groups="drop") %>%
      mutate(
        MarginError_ParmA=z_score*StdevParmA/sqrt(SampleSize),
        Lower_ParmA=AvgParmA-MarginError_ParmA, Upper_ParmA=AvgParmA+MarginError_ParmA,
        MarginError_ParmB=z_score*StdevParmB/sqrt(SampleSize),
        Lower_ParmB=AvgParmB-MarginError_ParmB, Upper_ParmB=AvgParmB+MarginError_ParmB,
        Group=paste(serial,Dilution)) %>%
      select(Group, AvgParmA:Upper_ParmB) %>%
      pivot_longer(cols=-Group, names_to="Metric", values_to="Value") %>%
      pivot_wider(names_from=Group, values_from=Value) %>%
      mutate(across(-Metric, ~mapply(format_metric, Metric, .x)))
    
    ci <- tibble(Metric="CI")
    for (nm in names(tbl)[-1]) ci[[nm]] <- paste0(conf_level*100, "%")
    tbl <- bind_rows(tbl, ci)
    
    datatable(tbl, options=list(dom="t", scrollX=TRUE), rownames=FALSE)})
}
