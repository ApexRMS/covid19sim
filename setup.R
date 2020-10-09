# A script to setup the necessary packages, directories, and external data
# sources to run Covid-19 forecasts and generate shiny applets with the data

# Install missing packages ----------------------------------------------------

packages <- c("tidyverse",
              "magrittr",
              "lubridate",
              "zoo",
              "rsyncrosim",
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
  html_nodes(css = "a") %>%            # Extract "a" nodes
  html_attr(name = "href") %>%         # Extract the urls
  str_subset("2020-.*zip") %>%         # Only keep urls for data archives
  set_names(.,                         # Name the urls by the date
            str_extract(., "202\\d-\\d\\d-\\d\\d") %>% # \d matches a digit 
              str_replace_all("-", "_"))

# Store some helpful directory / file names
ihmeDir <- "shiny/IHME/"
ihmeZip <- str_c(ihmeDir, "temp.zip")

# Download, unpack, and standardize the IHME archives if the folder is empty
if(dir(ihmeDir) %>% length == 0)
  ihmeURLs %>%
    iwalk(function(url, folder){
        # Download, unzip, and remove zip file
        download.file(url, ihmeZip, quiet = T)
        unzip(ihmeZip, exdir = ihmeDir)
        file.remove(ihmeZip)
        # Rename the extracted folder to yyyy_mmm_dd format (ie `folder`)
        # - Early archives all used the same name without dates
        if(file.exists(str_c(ihmeDir, "ihme-covid19")))
          file.rename(str_c(ihmeDir, "ihme-covid19"), str_c(ihmeDir, folder))
        # - Others add irregular suffixes
        #   - ".+" matches any suffix after the standarized date format
        if(dir(ihmeDir, pattern = "202\\d_\\d\\d_\\d\\d.+") %>% length > 0){
          file.rename(dir(ihmeDir, pattern = "202\\d_\\d\\d_\\d\\d.+",
                          full.names = T),
                      str_c(ihmeDir, folder))
        }
        # Finally remove and rename files within the folder as needed
        # - suppressWarnings is used as these files aren't always present
        suppressWarnings({
          file.remove(str_c(ihmeDir, folder, "/", 
                          c("readme.txt",
                            "IHME_COVID_19_Data_Release_Information_Sheet.pdf",
                            "Best_mask_hospitalization_all_locs.csv",
                            "Worse_hospitalization_all_locs.csv")))
          file.rename(str_c(ihmeDir, folder, "/",
                            "Reference_hospitalization_all_locs.csv"),
                      str_c(ihmeDir, folder, "/",
                            "Hospitalization_all_locs.csv"))
        })
        print(folder)
      }
    )

