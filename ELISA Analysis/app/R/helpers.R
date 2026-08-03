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

format_metric <- function(metric, x) {
  if (metric %in% c("Lower_ParmA","Upper_ParmA","Lower_ParmB","Upper_ParmB")) {
    formatC(x, format = "f", digits = 1)
  } else if (metric %in% c("CV_ParmA","CV_ParmB","CV_RP")) {
    paste0(formatC(x, format = "f", digits = 2), "%")
  } else if (metric == "SampleSize") {
    as.character(x)
  } else {
    formatC(x, format = "f", digits = 3)}}

format_plate_stats <- function(stats_row) {
  paste0(
    "Q25: ", round(stats_row$Q25, 3),
    "   Median: ", round(stats_row$Median, 3),
    "     Q75: ", round(stats_row$Q75, 3), "\n",
    "CV: ", round(stats_row$CV, 1), "%   ",
    "Inner CV: ", round(stats_row$Inner_CV, 1), "%   ",
    "Max/Min: ", round(stats_row$Ratio_Max_Min, 1), "%   ",
    "Inner Max/Min: ", round(stats_row$Inner_Ratio_Max_Min, 1), "%")}

map_serials <- function(df, mapping) {
  df %>%
    mutate(
      serial = case_when(
        serialID == mapping$X120 ~ "120",
        serialID == mapping$SerA ~ "SerA",
        serialID == mapping$SerB ~ "SerB",
        serialID == mapping$PC   ~ "PC",
        TRUE ~ NA_character_),
      serial = factor(serial,
                      levels = c("120","SerA","SerB","PC")))}