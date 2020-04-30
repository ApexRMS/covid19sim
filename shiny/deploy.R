# Shiny application for SyncroSim epidemic package COVID-19 output.
# Copyright ? 2007-2020 Apex Resource Management Solutions Ltd. (ApexRMS). All rights reserved.
# The TERMS OF USE and END USER LICENSE AGREEMENT for this software can be found in the LICENSE file.

# This script copies the most up-to-date model outputs from the "data" folder to the "shiny/covid19canada/data" folder

# Get output files from "data" folder
outputFiles <- list.files("data/", pattern="model-output")

# Confirm there are exactly 4 output files
if(!length(outputFiles) == 4){stop(paste("There are", length(outputFiles), "output files, instead of 4"))}

# Copy files to "shiny/covid19canada/data" folder
file.copy(paste0("data/", outputFiles), "shiny/covid19canada/data", overwrite=T)
