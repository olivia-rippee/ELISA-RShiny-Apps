process_uniformity <- function(od_file, scope) {

  ODs <- read_uploaded_file(od_file)
  colnames(ODs) <- sub("^X(?=\\d)", "", colnames(ODs), perl = TRUE)
  
  # Conditionally filter based on user choice
  uniformity_plates <- ODs %>%
    {if (scope == "uniformity_only") {
      filter(., grepl("uniformity", plateID, ignore.case = TRUE))
    } else {.}} %>%
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
  col_levels <- c(sort(as.numeric(unique(uniformity_long$Col))), "Avg") %>% as.character()
  
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

  list(
    uniformity_plates = uniformity_plates,
    heatmap_df = heatmap_df,
    heatmap_list = heatmap_list,
    heatmap_avg = heatmap_avg,
    heatmap_all_plates = heatmap_all_plates,
    uniformity_metrics = uniformity_metrics)}


process_parallelism <- function(serial_testing, dilution, layout, mapping, scope) {
  
  # Read files
  # -----------
  serial_testing <- read_uploaded_file(serial_testing)
  dilution <- read_uploaded_file(dilution)
  layout <- read_uploaded_file(layout)
  
  serial_testing <- map_serials(serial_testing, mapping)
  
  # Plate list
  # ----------
  parallelism_plates <- serial_testing %>%
    {if (scope == "parallelism_only") {
      filter(., grepl("Parallelism", plateID, ignore.case = TRUE),
             serial %in% c("SerA","SerB","PC"))
    } else {.}} %>%
    arrange(serialID)
  
  # Dilution + layout
  # -------------------
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
    left_join(
      dilution_long,
      by = c("plateID", "RowLetter", "Column")
    ) %>%
    map_serials(mapping)
  
  start_dilutions <- well_mapping %>%
    group_by(plateID, serialID, serial) %>%
    slice_min(Dilution, n = 1, with_ties = FALSE) %>%
    ungroup()

  list(
    parallelism = if (!is.null(parallelism_plates)) {
      parallelism_plates %>%
        left_join(start_dilutions, by = c("plateID","serialID","serial")) %>%
        mutate(dilution_group = paste0(serial, " ", Dilution))
    } else NULL,
    start_dilutions = start_dilutions)}


process_ruggedness <- function(serial_testing, mapping, scope) {
  serial_testing <- read_uploaded_file(serial_testing)
  
  serial_testing <- map_serials(serial_testing, mapping)
  
  ruggedness_plates <- serial_testing %>%
    {if (scope == "ruggedness_only") {
      filter(., grepl("min|max|ruggedness", plateID, ignore.case = TRUE),
             serial %in% c("120","SerB","PC"))
    } else {.}} %>%
    arrange(serialID)
  
  ruggedness_min <- ruggedness_plates %>% filter(grepl("min", plateID, ignore.case = TRUE))
  ruggedness_max <- ruggedness_plates %>% filter(grepl("max", plateID, ignore.case = TRUE))

list(
  ruggedness = ruggedness_plates,
  ruggedness_min = ruggedness_min,
  ruggedness_max = ruggedness_max)}
