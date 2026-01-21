
# Publish/redeploy an R Shiny app
# --------------------------------

setwd("C:/Users/ORippee/OneDrive - Kemin Industries/R&D - Olivia/R Code/FolderofInterest")

rsconnect::deployApp(appDir = ".", appMode = "shiny",  appPrimaryDoc = "app.R")
