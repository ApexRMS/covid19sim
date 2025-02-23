# A script to update the daily forecast and rebuild the shiny app
#
# - Please ensure setup.R has been run at least once since cloning repository
#   before running this update script!

# Load all packages -----------------------------------------------------------
# This is done first to ensure all packages are installed

message("Loading packages...")

packages <- c("tidyverse",
              "magrittr",
              "lubridate",
              "zoo",
              "rsyncrosim",
              "rsconnect",
              "shiny",
              "plotly",
              "RColorBrewer",
              "shinyWidgets",
              "cowplot",
              "grid",
              "scales",
              "shinythemes",
              "shinyBS",
              "rvest",
              "thematic")

tryCatch(
  invisible(lapply(packages, library, character.only = T, quietly = T)),
  error = function(cond){
    stop(paste0(cond,
                "\n\nOne or more package returned errors,",
                " please run setup.R if you haven't alread."))
  }
)

message("Done loading packages!\n")

# Source header files ---------------------------------------------------------
source("headers/local.R")
source("headers/constants.R")
source("headers/ihme.R")

# Update external data sources ------------------------------------------------

message("Updating internal and external data sources...")

if(!dir.exists(dataFolder) | !dir.exists(dataWorldFolder))
  stop("External data source folders not found. Please run setup.R")

system(str_c("git pull --quiet"))
system(str_c("git -C ", dataFolder, " pull --quiet"))
system(str_c("git -C ", dataWorldFolder, " pull --quiet"))

message("Done updating external data sources!\n")

# Check that both external sources are in sync --------------------------------
if(
  read.delim(paste0(dataFolder, "/update_time.txt"), header=F) %>%
  pull %>%
  as.Date !=
  read_csv(paste0(dataWorldFolder,"/time_series_covid19_deaths_global.csv"),
           n_max = 0) %>%
  names() %>%
  tail(1) %>%
  as.Date(format = "%m/%d/%y")
)
  stop(paste0("The John Hopkins and Isha Berry databases are not synced to ",
              "the same date. Please wait until both are up-to-date."))

# Make forecast ---------------------------------------------------------------

message("Preparing model inputs...")
source("forecast/analysis.R")
message("Done preparing model inputs!\n")

# Prepare Syncrosim template --------------------------------------------------

message("Preparing Syncrosim template...")
source("forecast/ssim-template.R")
message("Done preparing Syncrosim template!\n")

# Make forecast ---------------------------------------------------------------

message("Running forecast...")
source("forecast/ssim-run.R")
message("Done running forecast!\n")

# Update IHME model data for shiny app ----------------------------------------

message("Fetching IHME data for shiny app...")
updateIHME()
message("Done fetching IHME data!\n")

# Build and launch shiny app locally ------------------------------------------

message("Building shiny app...")
source("shiny/build.R")
message("Done building shiny app!\n")

runApp("shiny/covid19canada/app.R")
