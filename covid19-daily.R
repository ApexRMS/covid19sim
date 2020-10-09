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
source("covid19-local.R")
source("covid19-constants.R")

# Check that both data repos are synced to the same date ----------------
if(
  read.delim(paste0(dataFolder, "/update_time.txt"), header=F) %>%
  pull %>%
  as.Date !=
  read_csv(paste0(dataWorldFolder,"/time_series_covid19_deaths_global.csv"), n_max = 0) %>%
  names() %>%
  tail(1) %>%
  as.Date(format = "%m/%d/%y")
)
  stop("The John Hopkins and Isha Berry databases are not synced to the same date. Please wait until both are up-to-date.")

# Run analysis -----------------------------
source("covid19-analysis.R")

# Plot growth rates in Shiny - run app.R --------------

# Create SyncroSim template -----------------------------
source("covid19-ssim-template.R")


# Run SyncroSim template -----------------------------
source("covid19-ssim-run.R")

