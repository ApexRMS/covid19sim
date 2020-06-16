#
# covid19-daily.R: Script to update data and forecasts daily
#
# Developed by ApexRMS
#
# Input: Set the runDate in covid-19-constants.R. If not specified then will default to today
# Outputs:
#   Plot of the moving average of growth rates
#   Template .ssim library file
#   Simulated .ssim library file
#   Updated CSV files of deaths and forecasts in git repo

# Setup -------------------------
library(rstudioapi)

# Set the working directory to the script's folder (works only in RStudio)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("covid19-local.R")
source("covid19-constants.R")

# Run analysis -----------------------------
source("covid19-analysis.R")

# Plot growth rates in Shiny - run app.R --------------

# Create SyncroSim template -----------------------------
source("covid19-ssim-template.R")


# Run SyncroSim template -----------------------------
source("covid19-ssim-run.R")

