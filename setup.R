# A script to setup the necessary packages, directories, and external data
# sources to run Covid-19 forecasts and generate shiny applets with the data

# Install missing packages ----------------------------------------------------

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
              "rvest")

githubPackages <- c("thematic")

## Install and load all missing packages available on CRAN
## Adapted from: Vikram Baliga 
## (https://vbaliga.github.io/verify-that-r-packages-are-installed-and-loaded/)
invisible(
  lapply(packages,
         function(x) {
           if (!require(x, character.only = TRUE, quietly = TRUE)) {
             install.packages(x, dependencies = TRUE)
             library(x, character.only = TRUE, quietly = TRUE)
           }
         })
)

## Repeat for packages only available on GitHub
invisible(
  lapply(githubPackages,
         function(x) {
           if (!require(x, character.only = TRUE, quietly = TRUE)) {
             remotes::install_github("rstudio/thematic")
             library(x, character.only = TRUE, quietly = TRUE)
           }
         })
)

# Prepare directories ---------------------------------------------------------

directories <- c("external_data",
                 "ssim",
                 "ssim/data",
                 "ssim/runs",
                 "ssim/templates",
                 "shiny/IHME",
                 "shiny/covid19canada/data")

directories %>%
  walk(dir.create, showWarnings = F)

# Clone external data ---------------------------------------------------------

if(!dir.exists("external_data/canada"))
  system("git clone --quiet \\
         https://github.com/ishaberry/Covid19Canada external_data/canada")

if(!dir.exists("external_data/world"))
  system("git clone --quiet \\
         https://github.com/CSSEGISandData/COVID-19 external_data/world")

# Download IHME data for shiny app --------------------------------------------

# Scrape the list of IHME model archives using `rvest`
ihmeURLs <- "http://www.healthdata.org/covid/data-downloads" %>%
  read_html %>%                        # Pull html for the downloads page
  html_nodes(css = "a") %>%            # Extract hyperlink tags
  html_attr(name = "href") %>%         # Extract the urls
  str_subset("2020-.*zip") %>%         # Only keep urls for data archives
  set_names(.,                         # Name the urls by the date
            str_extract(., "202\\d-\\d\\d-\\d\\d") %>% # \d matches a digit 
            str_replace_all("-", "_")) # Covert Y-M-D to Y_M_D

# Source functions used for downloading IHME data
source("headers/ihme.R")

# Download, unpack, and standardize past IHME archives if the folder is empty
if(dir("shiny/IHME") %>% length == 0)
  ihmeURLs %>%
    iwalk(downloadIHME)

# The latest IHME archive is listed separately; check that that is up to date
updateIHME()
