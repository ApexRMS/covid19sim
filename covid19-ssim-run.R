#
# covid19-ssim-run.R: Runs library and sends output to CSV
#
# Takes as input c:\temp\covid19-canada-2020-04-17.sim
# *** Need to set the runDate below to the date of the file above ***
# Runs current measures base scenarios
# Exports the results to csv files in c:\temp\website
#
# Developed by ApexRMS

# ******************* SET THIS DATE BEFORE RUNNING **************
# runDate = "2020-04-19"
runDate = now()
# ***************************************************************
# Setup -------------------------

library(rsyncrosim)
library(tidyverse)
library(lubridate)
library(rstudioapi)

# Set the working directory to the script's folder (works only in RStudio)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("covid19-local.R")
source("covid19-constants.R")


# Set constants -------------------------

scenarioNamePrefix = "Current measures: "
scenarioNameSuffix = " (base fatality)"

libraryName = "covid19-canada"
packageName = "epidemic"
numberJobs = 7

# Backup the output files
file.copy(paste0(outputFolder, "/", "deaths-cumulative-model-output.csv"), 
          paste0(runFolder, "/", "deaths-cumulative-model-output-bak.csv"), overwrite=T)
file.copy(paste0(outputFolder, "/", "deaths-daily-model-output.csv"), 
          paste0(runFolder, "/", "deaths-daily-model-output-bak.csv"), overwrite=T)
file.copy(paste0(outputFolder, "/", "infected-cumulative-model-output.csv"), 
          paste0(runFolder, "/", "infected-cumulative-model-output-bak.csv"), overwrite=T)
file.copy(paste0(outputFolder, "/", "infected-daily-model-output.csv"), 
          paste0(runFolder, "/", "infected-daily-model-output-bak.csv"), overwrite=T)

# Create a copy of the template library to work with in the website folder
sourceFileName = paste0(templateFolder, "/", libraryName, ".ssim")
libraryFileName = paste0(runFolder, "/", libraryName, "-run.ssim")
file.copy(sourceFileName, libraryFileName)

# Start a SyncroSim session
packagePrefix = paste0(packageName, "_")  # Used to reference internal table names in SyncroSim
mySession = session()

# Open copy of library to run for output
myLibrary = ssimLibrary(libraryFileName, session = mySession)

# Generate a list of the scenario names to run
scenarioNames = paste0(scenarioNamePrefix, jurisdictionsModel, scenarioNameSuffix)


# Run scenarios --------------------------------------

# Clear object gathering all the output
if (exists("deaths_cumulative")) { remove(deaths_cumulative)}
if (exists("deaths_daily")) { remove(deaths_daily)}
if (exists("infected_cumulative")) { remove(infected_cumulative)}
if (exists("infected_daily")) { remove(infected_daily)}

# Repeat for all jurisdictions
for (name in scenarioNames) {
  # name = scenarioNames[1]
  
  # Run the scenario
  myScenario = scenario(myLibrary, name)
  # myResultsScenario = scenario(myLibrary, 41)
  myResultsScenario = run(myScenario, jobs=numberJobs)
  
  allDatasheetNames = datasheet(myResultsScenario, summary=T)
  datasheetName = paste0(packagePrefix, "OutputDeath")
  outputDeathRaw = datasheet(myResultsScenario, datasheetName)
  datasheetName = paste0(packagePrefix, "OutputInfected")
  outputInfectedRaw = datasheet(myResultsScenario, datasheetName)
  
  deaths_daily = outputDeathRaw %>%
    mutate(Date = str_sub(Date,1,10)) %>%
    group_by(Jurisdiction, Date) %>%
    summarize(mean = round(mean(Value),0),
              lower = round(quantile(Value, 0.025),0),
              upper = round(quantile(Value, 0.975),0)) %>%
    mutate(lower = ifelse(mean==lower,"",as.character(lower))) %>%
    mutate(upper = ifelse(mean==upper,"",as.character(upper))) %>%
    rename(Mean=mean, Upper=upper, Lower=lower) %>%
    mutate(date_model_run = date(runDate))
  
  deaths_cumulative = outputDeathRaw %>%
    mutate(Date = str_sub(Date,1,10)) %>%
    group_by(Jurisdiction, Date) %>%
    summarize(mean = round(mean(CumulativeValue),0),
              lower = round(quantile(CumulativeValue, 0.025),0),
              upper = round(quantile(CumulativeValue, 0.975),0)) %>%
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
  
  # Add output to previously generated output
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

# Remove country suffix from province jurisdiction name
# deaths_cumulative_all = deaths_cumulative_all %>%
#   ungroup %>%
#   mutate(Jurisdiction = str_replace(Jurisdiction,"Canada - ", ""))
# deaths_daily_all = deaths_daily_all %>%
#   ungroup %>%
#   mutate(Jurisdiction = str_replace(Jurisdiction,"Canada - ", ""))
# infected_cumulative_all = infected_cumulative_all %>%
#   ungroup %>%
#   mutate(Jurisdiction = str_replace(Jurisdiction,"Canada - ", ""))
# infected_daily_all = infected_daily_all %>%
#   ungroup %>%
#   mutate(Jurisdiction = str_replace(Jurisdiction,"Canada - ", ""))

# Write to file
write_csv(deaths_cumulative_all, append=T, paste0(outputFolder, "/", "deaths-cumulative-model-output.csv"))
write_csv(deaths_daily_all, append=T, paste0(outputFolder, "/", "deaths-daily-model-output.csv"))
write_csv(infected_cumulative_all, append=T, paste0(outputFolder, "/", "infected-cumulative-model-output.csv"))
write_csv(infected_daily_all, append=T, paste0(outputFolder, "/", "infected-daily-model-output.csv"))
