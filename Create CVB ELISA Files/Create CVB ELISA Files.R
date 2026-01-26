# ----------------------------------------------------------------------------------------------------
# Instructions: copy-paste file directory in setwd(). Change \ to / if needed. Run the whole script.
# ----------------------------------------------------------------------------------------------------

# Copy-paste directory for files here. Make sure to change \ to /.
setwd("")


# Function to create empty plate-style data frame
# ------------------------------------------------
make_plate_df <- function() {
  cols <- c(as.character(1:12), "plateID")
  df <- as.data.frame(
    matrix(nrow = 0, ncol = length(cols)),
    stringsAsFactors = FALSE)
  colnames(df) <- cols
  df}


# Generate empty files. You can change file names here or in File Explorer later.
# -------------------------------------------------------------------------------

# Dilution
# ----------------------------
Dilution <- make_plate_df()
write.csv(Dilution, "Dilution.csv", row.names = FALSE, quote = FALSE)

# Layout
# ----------------------------
Layout <- make_plate_df()
write.csv(Layout, "Layout.csv", row.names = FALSE, quote = FALSE)

# OD
# ----------------------------
OD <- make_plate_df()
write.csv(OD, "OD.csv", row.names = FALSE, quote = FALSE)

# PlateInfo
# ----------------------------
PlateInfo <- data.frame(
  plateID = character(),
  date = character(),
  tech = character(),
  plate_role = character(),
  stringsAsFactors = FALSE)

write.csv(PlateInfo, "PlateInfo.csv", row.names = FALSE, quote = FALSE)


# SerialTesting
# ----------------------------
SerialTesting <- data.frame(
  plateID = character(),
  serialID = character(),
  ParmB_ratio  = numeric(),
  ParmA_ratio = numeric(),
  rp = numeric(),
  avgBlank= numeric(),
  avgBlankedNegativeCtrl = numeric(),
  stringsAsFactors = FALSE)

write.csv(SerialTesting, "SerialTesting.csv", row.names = FALSE, quote = FALSE)

