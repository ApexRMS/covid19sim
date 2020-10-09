library(tidyverse)
library(rvest)

# Function to download, unpack, and standardize IHME model archives
#   - url is the location of the compresed archive
#   - folderName is generally the date of the record in yyyy_mm_dd format
downloadIHME <- function(url, folderName, ihmeDir = "shiny/IHME/"){
  ihmeZip <- str_c(ihmeDir, "temp.zip")

  # Download, unzip, and remove zip file
  # - also store the original extracted folder name for renaming
  download.file(url, ihmeZip)
  oldfolderName <- unzip(ihmeZip, exdir = ihmeDir %>% str_sub(1,-2)) %>%
    `[`(1) %>%
    str_split("/") %>%
    unlist %>%
    str_subset("202\\d|ihme")
  file.remove(ihmeZip)

  # Rename the extracted folder
  file.rename(str_c(ihmeDir, oldfolderName), str_c(ihmeDir, folderName))

  # Finally remove and rename files within the folder as needed
  # - suppressWarnings is used as these files aren't always present
  suppressWarnings({
    file.remove(str_c(ihmeDir, folderName, "/", 
                    c("readme.txt",
                      "IHME_COVID_19_Data_Release_Information_Sheet.pdf",
                      "IHME_COVID_19_Data_Release_Information_Sheet_II.pdf",
                      "Best_mask_hospitalization_all_locs.csv",
                      "Worse_hospitalization_all_locs.csv")))
    file.rename(str_c(ihmeDir, folderName, "/",
                      "Reference_hospitalization_all_locs.csv"),
                str_c(ihmeDir, folderName, "/",
                      "Hospitalization_all_locs.csv"))
    file.rename(str_c(ihmeDir, folderName, "/",
                      "ihme-covid19_all_locs.csv"),
                str_c(ihmeDir, folderName, "/",
                      "Hospitalization_all_locs.csv"))
  })
  invisible()
}

# Function to download the most recent IHME model
updateIHME <- function(ihmeDir = "shiny/IHME/"){
  # Parse the date of the most recent update   
  latestDate <- "http://www.healthdata.org/covid/data-downloads" %>%
    read_html %>%                        # Pull html for downloads page
    html_nodes(css = "p") %>%            # Extract paragraph tags
    html_text %>%                        # Convert to text
    str_subset("This page was last updated") %>% # Extract udpate note
    str_extract("\\w+ \\d+, 202\\d") %>% # Extract just the date
    as.Date(format = "%B %d, %Y") %>%    # Parse date
    format("%Y_%m_%d")                   # Convert to string

  # Download if the corresponding folder does not exist
  if(!dir.exists(str_c(ihmeDir, latestDate)))
    downloadIHME(
      "https://ihmecovid19storage.blob.core.windows.net/latest/ihme-covid19.zip",
      latestDate,
      ihmeDir)
  invisible()
}
