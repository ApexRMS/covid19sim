#
# forecast/ssim-run.R: Run SyncroSim "epidemic" package library and sends output to CSV
#
# Developed by ApexRMS
#
# Input: covid19-canada-yyyy-mm-dd.sim SyncroSim library file, as generated by the script forecast/ssim-template.R
# Outputs:
#   covid19-canada-run-yyyy-mm-dd.ssim - a copy of the input library file with results
#   appended summary output to various CSV files

# Setup -------------------------

library(rsyncrosim)
library(tidyverse)
library(lubridate)

# ******************* SET THIS DATE BEFORE RUNNING **************
# This should be the day after the last death data
# runDate = "2020-04-29"
# runDate = today()
# ***************************************************************
source("headers/local.R")
source("headers/constants.R")


# Set constants -------------------------

scenarioNamePrefix = "Current measures: "
scenarioNameSuffix = " (base fatality)"

libraryName = "covid19-canada"
packageName = "epidemic"
numberJobs = 7

# Backup the output files
file.copy(paste0(outputFolder, "/", "deaths-cumulative-model-output.csv"), 
          paste0(runFolder, "/", "deaths-cumulative-model-output.csv"), overwrite=T)
file.copy(paste0(outputFolder, "/", "deaths-daily-model-output.csv"), 
          paste0(runFolder, "/", "deaths-daily-model-output.csv"), overwrite=T)
file.copy(paste0(outputFolder, "/", "infected-cumulative-model-output.csv"), 
          paste0(runFolder, "/", "infected-cumulative-model-output.csv"), overwrite=T)
file.copy(paste0(outputFolder, "/", "infected-daily-model-output.csv"), 
          paste0(runFolder, "/", "infected-daily-model-output.csv"), overwrite=T)

# Create a copy of the template library to work with in the website folder
sourceFileName = paste0(templateFolder, "/", libraryName, ".ssim")
libraryFileName = paste0(runFolder, "/", libraryName, "-run.ssim")
file.copy(sourceFileName, libraryFileName, overwrite = T)

# Delete previous temp folder - otherwise old cached chart results remain
tempFolder = paste0(libraryFileName,".temp")
if (dir.exists(file.path(tempFolder))) {
  unlink(tempFolder, recursive = TRUE)
}

# Start a SyncroSim session
packagePrefix = paste0(packageName, "_")  # Used to reference internal table names in SyncroSim
mySession = rsyncrosim::session()

# Open copy of library to run for output
myLibrary = ssimLibrary(libraryFileName, session = mySession, package = "epidemic")

# Generate a list of the scenario names to run
scenarioNames = paste0(scenarioNamePrefix, jurisdictionsModel, scenarioNameSuffix)


# Run scenarios --------------------------------------

# Clear object gathering all the output
if (exists("deaths_cumulative_all")) { remove(deaths_cumulative_all)}
if (exists("deaths_daily_all")) { remove(deaths_daily_all)}
if (exists("infected_cumulative_all")) { remove(infected_cumulative_all)}
if (exists("infected_daily_all")) { remove(infected_daily_all)}

# Repeat for all jurisdictions
for (name in scenarioNames) {
  # name = scenarioNames[4]
  
  # Run the scenario
  myScenario = scenario(myLibrary, name)
  myResultsScenario = run(myScenario, jobs=numberJobs)
  
  # For reference get a list of all the datasheets in the library
  allDatasheetNames = datasheet(myResultsScenario, summary=T)

  # Get the raw output datasheets
  datasheetName = paste0(packagePrefix, "OutputDeath")
  outputDeathRaw = datasheet(myResultsScenario, datasheetName)
  datasheetName = paste0(packagePrefix, "OutputInfected")
  outputInfectedRaw = datasheet(myResultsScenario, datasheetName)
  
  # Summarize the raw output

  deaths_daily = outputDeathRaw %>%
  mutate(Date = str_sub(Date,1,10)) %>%
  group_by(Jurisdiction, Date) %>%
  summarize(mean = round(mean(Value),4),
            lower = round(quantile(Value, 0.025),4),
            upper = round(quantile(Value, 0.975),4)) %>%
  mutate(lower = ifelse(mean==lower,"",as.character(lower))) %>%
  mutate(upper = ifelse(mean==upper,"",as.character(upper))) %>%
  rename(Mean=mean, Upper=upper, Lower=lower) %>%
  mutate(date_model_run = date(runDate))
  
  deaths_cumulative = outputDeathRaw %>%
    mutate(Date = str_sub(Date,1,10)) %>%
    group_by(Jurisdiction, Date) %>%
    summarize(mean = round(mean(CumulativeValue),4),
              lower = round(quantile(CumulativeValue, 0.025),4),
              upper = round(quantile(CumulativeValue, 0.975),4)) %>%
    mutate(lower = ifelse(mean==lower,"",as.character(lower))) %>%
    mutate(upper = ifelse(mean==upper,"",as.character(upper))) %>%
    rename(Mean=mean, Upper=upper, Lower=lower) %>%
    mutate(date_model_run = date(runDate))
  
  infected_daily = outputInfectedRaw %>%
    mutate(Date = str_sub(Date,1,10)) %>%
    group_by(Jurisdiction, Date) %>%
    summarize(mean = round(mean(Value),0),
              lower = round(quantile(Value, 0.025),0),
              upper = round(quantile(Value, 0.975),0)) %>%
    rename(Mean=mean, Upper=upper, Lower=lower) %>%
    mutate(date_model_run = date(runDate))
  
  infected_cumulative = outputInfectedRaw %>%
    mutate(Date = str_sub(Date,1,10)) %>%
    group_by(Jurisdiction, Date) %>%
    summarize(mean = round(mean(CumulativeValue),0),
              lower = round(quantile(CumulativeValue, 0.025),0),
              upper = round(quantile(CumulativeValue, 0.975),0)) %>%
    rename(Mean=mean, Upper=upper, Lower=lower) %>%
    mutate(date_model_run = date(runDate))
  
  # Add to output generated for previous jurisdictions
  if (exists("deaths_cumulative_all")) {
    deaths_cumulative_all = bind_rows(deaths_cumulative_all, deaths_cumulative)
    deaths_daily_all = bind_rows(deaths_daily_all, deaths_daily)
    infected_cumulative_all = bind_rows(infected_cumulative_all, infected_cumulative)
    infected_daily_all = bind_rows(infected_daily_all, infected_daily)
  } else {
    deaths_cumulative_all = deaths_cumulative
    deaths_daily_all = deaths_daily
    infected_cumulative_all = infected_cumulative
    infected_daily_all = infected_daily
  }
}

# Write output ---------------------------------------------

# Append summary output to existing CSV file
write_csv(deaths_cumulative_all, append=T, paste0(outputFolder, "/", "deaths-cumulative-model-output.csv"))
write_csv(deaths_daily_all, append=T, paste0(outputFolder, "/", "deaths-daily-model-output.csv"))
write_csv(infected_cumulative_all, append=T, paste0(outputFolder, "/", "infected-cumulative-model-output.csv"))
write_csv(infected_daily_all, append=T, paste0(outputFolder, "/", "infected-daily-model-output.csv"))

# Check output
deathsCheck = read.csv(paste0(outputFolder, "/", "deaths-cumulative-model-output.csv"), stringsAsFactors=F)
lastDate = max(deathsCheck$date_model_run)
priorDate = runDate - ddays(1)
runDateRecords = nrow(filter(deathsCheck, date_model_run == runDate))
priorDateRecords = nrow(filter(deathsCheck, date_model_run == priorDate))

print(paste0("runDate: ", runDate))
print(paste0("lastDate: ", lastDate))
print(paste0("priorDateRecords: ", priorDateRecords))
print(paste0("runDateRecords: ", runDateRecords))
