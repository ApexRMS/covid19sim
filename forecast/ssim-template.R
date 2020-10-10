#
# forecast/ssim-template.R: Preparing a template SyncroSim library for use with the "epidemic" package 
#
# Developed by ApexRMS

# Input: CSV output from the script forecast/analysis.R plus other input CSV files
# Output: covid19-canada-yyyy-mm-dd.ssim - SyncroSim library with corresponding model inputs
#
# devtools::install("C:/gitprojects/rsyncrosim")
library(rsyncrosim)
library(tidyverse)
library(lubridate)

# ******************* SET THIS DATE BEFORE RUNNING **************
# This should be the day after the last death data
# runDate = "2020-04-25"
# runDate = today()
# ***************************************************************

# Setup -------------------------
source("headers/local.R")
source("headers/constants.R")

# TODO: automatically pull from git data repos

# Set constants

packageName = "epidemic"
libraryName = "covid19-canada.ssim"
owner = "ApexRMS"

# Modelling dates
startDate = "2020-02-12"           # First date of simulation - 
lastNoControlDate = "2020-03-14"   # Last date for which there are no controls in the No Control scenario
daysPastDeaths = 14                # Number of days to simulate forward after the last day with deaths

# Number of realizations
numRealizations = 1000

# Fatality scenarios to be included in the template library
# fatalityScenarios = c(2)
fatalityScenarios = c(1)
fatalityScenarioNames = c(" (base fatality)", " (high fatality)")
highFatalityMultiplier = 1.5

# Control scenarios to be included in the template library
#controlScenarios = c(2)
controlScenarios = c(1)
controlScenarioNames = c("Current measures: ", "No control: ")

# Load Data -------------------------

# Load and summarize the population data
filename = paste0(inputFolder, "/deaths-output.csv")
populationData = read.csv(filename, stringsAsFactors = F) %>%
  filter(jurisdiction %in% jurisdictionsModel) %>%
  select(jurisdiction, country_region, province_state, population) %>%
  group_by(jurisdiction, country_region, province_state) %>%
  summarize(population = mean(population)) %>%
  ungroup %>%
  mutate(name = province_state)
# populationData$name[populationData$name%in% c("")] = populationData$country_region[populationData$name%in% c("")]
populationData$name = populationData$jurisdiction

# Generate dataframe of all the jurisdictions & names
jurisdictions = populationData %>%
  select(jurisdiction, name) %>%
  mutate(folderId=0)


# Library & Project Datafeeds ------------------------------------

# Start a SyncroSim session
packagePrefix = paste0(packageName, "_")  # Used to reference internal table names in SyncroSim
mySession = session()  # Uses default session (works on Windows) - otherwise use: session("C:/My SyncroSim install folder")

# Create a new library for the SyncroSim epidemic package
fileName = paste0(templateFolder, "/", libraryName)
myLibrary = ssimLibrary(fileName, session = mySession, package = packageName, overwrite = T)

# Set the library properties
name(myLibrary) = paste0("COVID-19 Canada (", date(runDate), ")")
owner(myLibrary) = owner
description(myLibrary) = paste0("COVID-19 simulation scenarios generated on ", runDate,
                                ". \n \nAdditional details on the approach used can be found at www.apexrms.com/covid19")

# Turn library multiprocessing on
datasheetName = "core_Multiprocessing"
myDatasheet = datasheet(myLibrary, name = datasheetName)
myDatasheet$EnableMultiprocessing = TRUE
myDatasheet$MaximumJobs=5
saveDatasheet(myLibrary, myDatasheet, name = datasheetName)

# Load the default project for this library - default project always named "Definitions"
myProject = project(myLibrary, "Definitions") 

# Set the project properties
owner(myProject) = owner
description(myProject) = "Currently includes only those provinces with at least 5 deaths for a period of 7 days or more"

# Add Jurisdictions
myJurisdictions = jurisdictions %>%
  rename(Name = jurisdiction) %>%
  select(Name)
datasheetName = paste0(packagePrefix, "Jurisdiction")
saveDatasheet(myProject, as.data.frame(myJurisdictions), name = datasheetName)

# Add pre-configured Charts
filename = paste0(inputFolder, "/chart-ssim.csv")
myCharts = read.csv(filename)
datasheetName = "corestime_Charts"
saveDatasheet(myProject, myCharts, "corestime_Charts")

#TODO: make the Chart read-only

# Create a folder in the library for the Extra scenarios (using command function to call SyncroSim console)
# args = list(create=NULL, folder=NULL, lib=filepath(myLibrary), name="Other Scenarios", tpid=projectId(myProject))
# return = command(args, session=mySession)
# otherFolderId = as.numeric(strsplit(return, ": ")[[1]][2])
# 
# # Create a folder in the library for each jurisdiction (using SyncroSim command function)
# for (jurName in jurisdictions$jurisdiction){
#   args = list(create=NULL, folder=NULL, lib=filepath(myLibrary), name=jurName, tfid=otherFolderId)
#   return = command(args, session=mySession)
#   folderId = as.numeric(strsplit(return, ": ")[[1]][2])
#   jurisdictions$folderId[jurisdictions$name==jurName]=folderId
# }


# Scenario Datafeeds ------------------------------

# Create a scenario for each combination of jurisdiction, control scenario and fatality level scenario
for (jur in jurisdictions$jurisdiction){
  for (control in controlScenarios) {
    for (fatality in fatalityScenarios) {
      # Loop testing: jur = jurisdictions$jurisdiction[1]; control = controlScenarios[1]; fatality = fatalityScenarios[1]
      
      jurName = jurisdictions$name[jurisdictions$jurisdiction==jur]
      
      # Create a new scenario and set the owner
      scenarioName = paste0(controlScenarioNames[control],jurName,fatalityScenarioNames[fatality])
      myScenario = scenario(myProject, scenario = scenarioName)
      owner(myScenario) = owner
      
      # Put the scenario in the jurisdiction's library folder
      folderId = jurisdictions$folderId[jurisdictions$jurisdiction==jur]
      if (!((control==1) & (fatality==1))) {
        args = list(move=NULL, scenario=NULL, lib=filepath(myLibrary), sid=scenarioId(myScenario), tfid=folderId)
        command(args, session=mySession)
      }

      # Build up a scenario description
      description = paste0("Death data downloaded from https://github.com/ishaberry/Covid19Canada on ", runDate, ".")
      if (control == 2) {
        description = paste(description,
            paste0("Only death data up to and including ", as.character(ymd(lastNoControlDate) + days(infectionPeriod)), " are included. ",
                   "This reflects the last date on which death data is associated with the period prior to public health measures."), sep="\n")
        description = paste(description,
            paste0("Growth rate of infections after last date with deaths is set to the average growth rate of deaths up to and including deaths on ",
                   as.character(ymd(lastNoControlDate) + days(infectionPeriod)), "."), sep="\n")
      }
      if (control == 1) {
        paste(jurisdictionsGrowthReferenceList[[jur]], collapse = ', ')
        refCountriesString = paste
        description = paste(description,
            paste0("Growth rate after last date with deaths is set to the observed distribution of rates from ",
            paste(jurisdictionsGrowthReferenceList[[jur]], collapse = ', '),"."), sep="\n")
        description = paste(description,
            paste0("Growth rates for other countries downloaded from https://github.com/CSSEGISandData/COVID-19 on ", runDate, "."), sep="\n")
      }
      if (fatality == 1) {
        description = paste(description,
            "Fatality rate is unadjusted age-weighted value from Verity et al (2020).", sep="\n")
      }
      if (fatality == 2) {
        description = paste(description,
            "Fatality rate is 50% higher than the age-weighted value from Verity et al (2020).", sep="\n")
      }
      description = paste(description, "Additional details on the model parameterization can be found at www.apexrms.com/covid19", sep="\n")
      description(myScenario) = description

      # Deaths --------------------------------------------------------------
      filename = paste0(inputFolder, "/deaths-output.csv")
      deathData = read.csv(filename, stringsAsFactors=F)
      deathData = filter(deathData, jurisdiction == jur) %>%
        select(date, jurisdiction, deaths_daily)
      
      endDate = as.character(max(as.Date(deathData$date)) + daysPastDeaths)
      
      if (control == 2){
        # No control scenario: only include actual deaths attributable to infections up to end of no control date
        deathData = filter(deathData, ymd(date) <= ymd(lastNoControlDate) + days(infectionPeriod))
      }
      
      myDatasheet = rename(deathData, Timestep = date, Jurisdiction = jurisdiction, Value = deaths_daily)
      datasheetName = paste0(packagePrefix, "ActualDeath")
      saveDatasheet(myScenario, myDatasheet, name = datasheetName)
      
      # Run Control -------------------------------------------------------------------------
      datasheetName = paste0(packagePrefix, "RunControl")
      myDatasheet = datasheet(myScenario, name = datasheetName)
      myDatasheet$StartDate = as.character(myDatasheet$StartDate)  # Bug in package - returns a logical
      myDatasheet$EndDate = as.character(myDatasheet$EndDate)  # Bug in package - returns a logical
      myDatasheet = add_row(myDatasheet,
                                      MinimumIteration=1,
                                      MaximumIteration=numRealizations, 
                                      StartDate=startDate,
                                      EndDate=endDate)
      saveDatasheet(myScenario, myDatasheet, name = datasheetName)
      
      datasheetName = paste0(packagePrefix, "RuntimeJurisdiction")
      myDatasheet = datasheet(myScenario, name = datasheetName)
      myDatasheet = add_row(myDatasheet, Jurisdiction = jur)
      saveDatasheet(myScenario, myDatasheet, name = datasheetName)
      
      # Population ------------------------------------------------------------------------
      datasheetName = "Population"
      datasheetName = paste0(packagePrefix, "Population")
      myDatasheet = populationData %>%
        filter(jurisdiction == jur) %>%
        select(jurisdiction, population) %>%
        rename(Jurisdiction=jurisdiction, TotalSize=population)
      saveDatasheet(myScenario, as.data.frame(myDatasheet), name = datasheetName)
      
      # Fatality --------------------------------------------------------
      fileName = paste0(inputFolder, "/fatality-canada-ssim.csv")
      datasheetName = paste0(packagePrefix, "FatalityRate")
      myDatasheet = read.csv(fileName)
      myDatasheet = filter(myDatasheet, Jurisdiction == jur)
      if (fatality == 2) {
        # Fatality scenario 2 has a high fatality multiplier
        myDatasheet$Value = myDatasheet$Value * highFatalityMultiplier
      }
      saveDatasheet(myScenario, myDatasheet, name = datasheetName)
      
      # Attack Rate -----------------------------------------------------
      fileName = paste0(inputFolder, "/attack-rate-canada-ssim.csv")
      datasheetName = paste0(packagePrefix, "AttackRate")
      myDatasheet = read.csv(fileName)
      saveDatasheet(myScenario, myDatasheet, name = datasheetName)
      
      # Incubation Period -----------------------------------------------------
      fileName = paste0(inputFolder, "/incubation-period-ssim.csv")
      datasheetName = paste0(packagePrefix, "IncubationPeriod")
      myDatasheet = read.csv(fileName)
      saveDatasheet(myScenario, myDatasheet, name = datasheetName)
      
      # Symptom Period -----------------------------------------------------
      fileName = paste0(inputFolder, "/symptom-period-ssim.csv")
      datasheetName = paste0(packagePrefix, "SymptomPeriod")
      myDatasheet = read.csv(fileName)
      saveDatasheet(myScenario, myDatasheet, name = datasheetName)
      
      # Model Type -------------------------------------------------------------------------
      modelType = "Exponential"
      if (control == 2){
        # No control scenario: use the logistic model
        modelType="Logistic"
        }
      datasheetName = paste0(packagePrefix, "ModelType")
      myDatasheet = datasheet(myScenario, name = datasheetName)
      myDatasheet = add_row(myDatasheet, ModelType=modelType)
      saveDatasheet(myScenario, myDatasheet, name = datasheetName)
      
      # Growth -------------------------------------------------------------
      datasheetName = paste0(packagePrefix, "GrowthRate")
      if (control == 2){
        # No control scenario: growth rate using regression model
        myDatasheet = datasheet(myScenario, name = datasheetName, optional = F, empty = T)
        fileName = paste0(inputFolder, "/deaths-regression-output.csv")
        growthData = read.csv(fileName)
        growthData = filter(growthData, jurisdiction == jur, regression == "reg1")
        growthRate = growthData$slope[1]-1
        myDatasheet = addRow(myDatasheet, list(growthRate))
        
      } else {
        # Current measures scenario: growth rate sampled from other countries
        myDatasheet = datasheet(myScenario, name = datasheetName, optional = F)
        fileName = paste0(growthFolder, "/growth-output.csv")
        growthData = read.csv(fileName)
        growthData$date = as.character(growthData$date)
        growthData = filter(growthData, jurisdiction == jur)
        tempData = rename(growthData, Iteration=iteration, Timestep=date, Jurisdiction=jurisdiction, Value=rate) %>%
          select(Iteration, Timestep, Jurisdiction, Value)
        myDatasheet = bind_rows(myDatasheet, tempData)
        
        # for (iter in seq(1,numRealizations)) {
        #   # Sample from growth rates from other countries for each realization
        #   # iter = 1
        #   len = length(jurisdictionsGrowthReferenceList[[jur]])
        #   index = sample(1:len,1)
        #   refJur = jurisdictionsGrowthReferenceList[[jur]][index]
        #   growthDataFiltered = filter(growthData, scenario == refJur)
        #   tempData = data.frame(Iteration=iter, Timestep = growthDataFiltered$date, Jurisdiction=as.character(jur), Value=growthDataFiltered$rate)
        #   myDatasheet = rbind(myDatasheet, tempData)
        # }
      }
      saveDatasheet(myScenario, myDatasheet, name = datasheetName)
      print(name(myScenario))
      
      # Run scenario
      # run(myScenario)
      # readOnly(myScenario) = T
      
    } # next fatality
  } # next control
} # next jur
