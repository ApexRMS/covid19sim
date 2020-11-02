#
# forecast/analysis.R: Preparing model inputs for use in the SyncroSim "epidemic" package 
#
# Developed by ApexRMS
#
# Inputs:
#  International daily COVID-19 death data downloaded from https://github.com/CSSEGISandData/COVID-19
#  Canadian COVID-19 death data downloaded from https://github.com/ishaberry/Covid19Canada
#  Infection fatality rates from Verity et al (2020)
#  Canadian population data (from Statistics Canada)
#  World population data (from United Nations)
# Outputs: various CSV files summarizing the data (as required by the script forecast/ssim-template.R)

# Setup -------------------------

library(tidyverse)
library(lubridate)
library(zoo)

# ******************* SET THIS DATE BEFORE RUNNING **************
# This should be the day after the last death data
# runDate = "2020-04-25"
# runDate = today()
# ***************************************************************

source("headers/local.R")
source("headers/constants.R")

# Load data -------------------------

# Load world mortality data
mortalityDataWorldRaw = read_csv(paste0(dataWorldFolder, "/", "time_series_covid19_deaths_global.csv"))

# Load U.S. mortality data
mortalityDataUSRaw = read_csv(paste0(dataWorldFolder, "/", "time_series_covid19_deaths_US.csv"))

# Load world population data (& remove Canada and US data)
populationWorld = read_csv(paste0(inputFolder, "/", "population_world.csv"))
populationWorld = populationWorld %>% 
  rename(country_region="Country Name", population="2018") %>%
  select(country_region, population) %>%
  mutate(jurisdiction = country_region, province_state="") %>%
  filter (!(country_region %in% c("Canada", "United States"))) %>%
  select(jurisdiction, country_region, province_state, population)

# Rename problematic countries
populationWorld = mutate(populationWorld, jurisdiction = str_replace(jurisdiction, "Korea, Rep.", "South Korea"))
populationWorld = mutate(populationWorld, country_region = str_replace(country_region, "Korea, Rep.", "South Korea"))

# Load U.S. population data
populationUS = read_csv(paste0(inputFolder, "/", "population_us.csv"))
populationUS = populationUS %>% 
  rename(province_state="NAME", population="POPESTIMATE2019") %>%
  select(province_state, population) %>%
  filter (!(province_state %in% c("Northeast Region", "Midwest Region", "West Region", "South Region"))) %>%
  mutate(country_region="United States") %>%
  mutate(jurisdiction = paste0(country_region, " - ", province_state)) %>%
  select(jurisdiction, country_region, province_state, population)
populationUS$jurisdiction[populationUS$province_state=="United States"] = "United States"
populationUS$province_state[populationUS$province_state=="United States"] = ""

# Load Canada total population data (Q1 2020 data from https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710000901)
populationCanada = read_csv(paste0(inputFolder, "/", "population_canada.csv"))
populationCanada = select(populationCanada, -code)
populationTotal = sum(populationCanada$population)
populationCanada = populationCanada %>%
  mutate(country_region = "Canada", province_state = province) %>%
  mutate(jurisdiction = paste0(country_region, " - ", province_state)) %>%
  select(jurisdiction, country_region, province_state, population) %>%
  add_row(jurisdiction="Canada", country_region="Canada", province_state="", population = populationTotal)

# Join the population datasets
population = bind_rows(populationWorld, populationUS, populationCanada)

# Load Canada detailed population data (by age)
populationDetailCanadaRaw = read_csv(paste0(inputFolder, "/", "population_detail_canada.csv"))

# Load fatality data (from Verity et al 2020)
fatalityRates = read_csv(paste0(inputFolder, "/", "fatality-rates.csv")) %>%
  rename(fatality_mean=mean, fatality_lower=lower, fatality_upper=upper)
# fatalityRatesCases = read_csv(paste0(inputFolder, "/", "fatality-rates-cases.csv")) %>%
#   rename(fatality_cases_mean=mean, fatality_cases_lower=lower, fatality_cases_upper=upper)

# Load Canada mortality data (From https://github.com/ishaberry/Covid19Canada)
mortalityDataRaw = read_csv(paste0(dataFolder, "/", "mortality.csv"))
mortalityDataRaw$province[mortalityDataRaw$province=="NL"] = "Newfoundland and Labrador"
mortalityDataRaw$province[mortalityDataRaw$province=="BC"] = "British Columbia"
mortalityDataRaw$province[mortalityDataRaw$province=="NWT"] = "Northwest Territories"
mortalityDataRaw$province[mortalityDataRaw$province=="PEI"] = "Prince Edward Island"
# mortalityDataRaw$province = factor(mortalityDataRaw$province, levels(population$province) )


# Population & fatality ---------------

# Compute weighted mean fatality for Canadian jurisdictions
# fatalityRatesCases = select(fatalityRatesCases, -age_min, -age_max)
# fatalityRates = left_join(fatalityRates, fatalityRatesCases)

populationDetailCanada = populationDetailCanadaRaw %>% 
  filter(Sex=="Both sexes") %>%
  mutate(Age_num=as.numeric(sub("\\ .*", "", `Age group`))) %>%
  mutate(age_group = cut(`Age_num`,
                         breaks=c(-Inf, fatalityRates$age_max),
                         labels=fatalityRates$age_group)) %>%
  mutate(age_group = as.character(age_group)) %>%
  select(`GEO`, `age_group`, `VALUE`) %>%
  group_by(`GEO`, `age_group`) %>%
  summarise(population = sum(VALUE)) %>%
  rename(jurisdiction=GEO) %>%
  left_join(fatalityRates, by="age_group")


fatalityRatesCanada <- populationDetailCanada %>%  
  group_by(jurisdiction) %>%
  summarize(fatality_mean=weighted.mean(fatality_mean, population),
            fatality_mean_lower=weighted.mean(fatality_lower, population),
            fatality_mean_upper=weighted.mean(fatality_upper, population))
            # fatality_cases_mean=weighted.mean(fatality_cases_mean, population),
            # fatality_cases_mean_lower=weighted.mean(fatality_cases_lower, population),
            # fatality_cases_mean_upper=weighted.mean(fatality_cases_upper, population))

# Write to file
write_csv(fatalityRatesCanada, paste0(outputFolder, "/", "fatality-canada-output.csv"))
write_csv(populationDetailCanada, paste0(outputFolder, "/", "population-detail-canada-output.csv"))


# Deaths -------------------

# Setup world data
deathsWorld = mortalityDataWorldRaw %>%
  select(-Lat, -Long) %>%
  rename(province_state="Province/State", country_region = "Country/Region") %>%
  pivot_longer(c(-province_state, -country_region), names_to = "date", values_to = "deaths_cumulative") %>%
  mutate(date = mdy(date))

# Rename problematic countries (e.g. Korea,South)
deathsWorld = mutate(deathsWorld, country_region = str_replace(country_region, "Korea, South", "South Korea"))

# Extract the canadian world data (to check against detailed canadian data)
deathsWorldCanada = filter(deathsWorld, country_region=="Canada")         
deathsWorld = filter(deathsWorld, country_region!="Canada")

# Summarize world data at country level
deathsWorld = deathsWorld %>%
  group_by(country_region, date) %>%
  summarize(deaths_cumulative = sum(deaths_cumulative)) %>%
  mutate(jurisdiction = country_region, province_state="")

# Join in world population
deathsWorld = select(deathsWorld, jurisdiction, country_region, province_state, date, deaths_cumulative)

# Setup U.S. data by state
deathsUS = mortalityDataUSRaw %>%
  select(-UID, -iso2, -iso3, -code3, -FIPS, -Lat, -Long_, -Combined_Key) %>%
  rename(county=Admin2, province_state=Province_State, country_region = Country_Region, population=Population) %>%
  pivot_longer(c(-county, -province_state, -country_region, -population), names_to = "date", values_to = "deaths_cumulative") %>%
  mutate(date = mdy(date), country_region="United States") %>%
  group_by(province_state, country_region, date) %>%
  summarize(deaths_cumulative = sum(deaths_cumulative)) %>%
  mutate(jurisdiction = paste0(country_region, " - ", province_state)) %>%
  select(jurisdiction, country_region, province_state, date, deaths_cumulative)
deathsUS$jurisdiction = str_replace_all(deathsUS$jurisdiction, " - NA", "")
deathsUS$province_state[is.na(deathsUS$province_state[])] = ""

# Calculate entire US totals
deathsUSTotal = deathsUS %>%
  group_by(date) %>%
  summarize(deaths_cumulative = sum(deaths_cumulative))
deathsUSTotal = deathsUSTotal %>%
  mutate(jurisdiction="United States", country_region="United States", province_state="")

# Combine U.S. state and total data
deathsUS = bind_rows(deathsUSTotal, deathsUS) %>%
  select(jurisdiction, country_region, province_state, date, deaths_cumulative) %>%
  arrange(jurisdiction)

# Calculate Canadian deaths by date, age and province
deathsDetailedCanada = mortalityDataRaw
deathsDetailedCanada$age=recode(deathsDetailedCanada$age, .default="Other",
                                "0-9"     = "0-9",
                                "10-19"   = "10-19",
                                "20-29"   = "20-29",
                                "30-39"   = "30-39",
                                "40-49"   = "40-49",
                                "50-59"   = "50-59",
                                "60-69"   = "60-69",
                                "70-79"   = "70-79",
                                "80-89"   = "80+",
                                "90-99"   = "80+",
                                "100-109" = "80+",
                                ">50"     = "50-59",
                                "61"      = "60-69",
                                "65"      = "60-69",
                                ">65"     = "60-69",
                                ">70"     = "70-79",
                                "78"      = "70-79",
                                "79"      = "70-79",
                                ">80"     = "80+",
                                "82"      = "80+",
                                "83"      = "80+",
                                "85"      = "80+",
                                "90-99"   = "80+",
                                "91"      = "80+",
                                "92"      = "80+",
                                "93"      = "80+",
                                "94"      = "80+",
                                "104"     = "80+",
                                "Not Reported" = "Not Reported")
unique(deathsDetailedCanada$age)
# if("Other" %in% unique(deathsDetailedCanada$age)) warning("******* There are Other ages to fix *********")
deathsDetailedCanada$age=recode(deathsDetailedCanada$age, "Not Reported" = "")
deathsDetailedCanada$age=recode(deathsDetailedCanada$age, "Other" = "")
unique(deathsDetailedCanada$age)

deathsDetailedCanada = deathsDetailedCanada %>%
  select(province, age, date=date_death_report) %>%
  mutate(date = dmy(date)) %>%
  rename(province_state = province) %>%
  mutate(deaths_daily=1) %>%
  group_by(province_state, date, age) %>%
  summarize(deaths_daily = sum(deaths_daily))

# Convert Canada detailed data into world format
deathsDetailedCanada = deathsDetailedCanada %>%
  ungroup %>%
  mutate(country_region="Canada", jurisdiction = paste0("Canada - ", province_state)) %>%
  select(jurisdiction, country_region, province_state, date, age, deaths_daily)

# Calculate summary of detailed deaths (by age) for entire country
deathsDetailedCanadaTotal = deathsDetailedCanada %>%
  group_by(age, date) %>%
  summarize(deaths_daily=sum(deaths_daily)) %>%
  mutate(province_state = "", jurisdiction = "Canada", country_region = "Canada") %>%
  select(jurisdiction, country_region, province_state, date, age, deaths_daily) %>%
  arrange(date, age)

# Join provincial and national Canadian data
deathsDetailedCanada = bind_rows(deathsDetailedCanada, deathsDetailedCanadaTotal) %>%
  arrange(jurisdiction)

# Write detailed deaths (with age) to file
write_csv(deathsDetailedCanada, paste0(outputFolder, "/", "deaths-detailed-canada-output.csv"))

# Drop age
deathsCanada = deathsDetailedCanada %>%
  group_by(province_state, date) %>%
  summarize(deaths_daily = sum(deaths_daily))

# Create a template with all dates for all provinces
firstDeathDate = min(deathsCanada$date)
lastDeathDate = max(deathsCanada$date)
allDeathDates = as_date(c(firstDeathDate:lastDeathDate))
allDeathProvinces = (populationCanada %>%
                       filter(province_state !="") %>%
                       select(province_state))$province_state
provincesAllDeathDates = crossing(allDeathProvinces, allDeathDates) %>%
  rename(province_state=allDeathProvinces, date=allDeathDates)

# Now join the actual summary data into the template
deathsCanada = provincesAllDeathDates %>%
  left_join(deathsCanada)
deathsCanada$deaths_daily[is.na(deathsCanada$deaths_daily[])] = 0

# Add the cumulative deaths
deathsCanada = deathsCanada %>%
  group_by(province_state, date) %>%
  summarize(deaths_daily = sum(deaths_daily)) %>%
  mutate(deaths_cumulative = cumsum(deaths_daily))

# Convert Canada data into world format
deathsCanada = deathsCanada %>%
  ungroup %>%
  mutate(country_region="Canada", jurisdiction = paste0("Canada - ", province_state)) %>%
  select(jurisdiction, country_region, province_state, date, deaths_daily, deaths_cumulative)

# Calculate summary for entire country
deathsCanadaTotal = deathsCanada %>%
  group_by(country_region, date) %>%
  summarize(deaths_daily=sum(deaths_daily), deaths_cumulative = sum(deaths_cumulative)) %>%
  mutate(province_state = "", jurisdiction = country_region) %>%
  select(jurisdiction, country_region, province_state, date, deaths_daily, deaths_cumulative)

# Join provincial and national Canadian data
deathsCanada = bind_rows(deathsCanada, deathsCanadaTotal) %>%
  arrange(jurisdiction)

# Backcalculate daily deaths for World and US
deathsWorldLagged = deathsWorld %>%
  select(jurisdiction, date, deaths_cumulative) %>%
  group_by(jurisdiction) %>%
  arrange(jurisdiction, date) %>%
  mutate(deaths_cumulative_lag = dplyr::lag(deaths_cumulative, n = 1, default = 0)) %>%
  mutate(deaths_daily = deaths_cumulative - deaths_cumulative_lag)
deathsWorld = left_join(deathsWorld, deathsWorldLagged) %>%
  select(jurisdiction, country_region, province_state, date, deaths_daily, deaths_cumulative)

deathsUSLagged = deathsUS %>%
  select(jurisdiction, date, deaths_cumulative) %>%
  group_by(jurisdiction) %>%
  arrange(jurisdiction, date) %>%
  mutate(deaths_cumulative_lag = dplyr::lag(deaths_cumulative, n = 1, default = 0)) %>%
  mutate(deaths_daily = deaths_cumulative - deaths_cumulative_lag)
deathsUS = left_join(deathsUS, deathsUSLagged) %>%
  select(jurisdiction, country_region, province_state, date, deaths_daily, deaths_cumulative)

# Join world, US and Canada
deaths = bind_rows(deathsCanada, deathsUS, deathsWorld)

# Remove deaths not occuring prior to runDate
deaths = deaths %>% filter(date < runDate)

# Add population data
deaths = left_join(deaths, population) %>%
  select(jurisdiction, country_region, province_state, population, date, deaths_daily, deaths_cumulative)

# Add days since threshold deaths for each jurisdiction
deathsStartDates = deaths %>%
  filter(deaths_cumulative >= deathsCumulativeRegMin) %>%
  group_by(jurisdiction) %>%
  summarize(date_start=min(date))
deaths = left_join(deaths, deathsStartDates) %>%
  mutate(day_model = date - date_start) %>%
  mutate(day_model = ifelse(day_model<0, NA, day_model)) %>%
  select(-date_start)

# Calculate log of cumulative deaths for days greater than threshold
deaths = mutate(deaths, deaths_cumulative_log = ifelse(day_model>=0, log10(deaths_cumulative), NA))

# Calculate daily growth rate, including moving averages
deaths = mutate(deaths, deaths_growth = ifelse(day_model>=0, deaths_daily / (deaths_cumulative - deaths_daily), NA)) %>%
  group_by(jurisdiction)%>%
  mutate(deaths_growth_ma3=rollapply(deaths_growth,3,mean,align='right',fill=NA)) %>%
  mutate(deaths_growth_ma5=rollapply(deaths_growth,5,mean,align='right',fill=NA)) %>%
  mutate(deaths_growth_ma7=rollapply(deaths_growth,7,mean,align='right',fill=NA)) %>%
  mutate(deaths_growth_ma7=rollapply(deaths_growth,9,mean,align='right',fill=NA))

# Write to file
write_csv(deaths, paste0(outputFolder, "/", "deaths-output.csv"))

# Compare data by province from world and canadian sources
deathComparison = deathsWorldCanada %>%
  rename(deaths_cumulative_world = deaths_cumulative) %>%
  left_join(deathsCanada) %>%
  mutate(diff = deaths_cumulative_world - deaths_cumulative) %>%
  arrange(diff)

# Check recent Ontario data - note that our Canadian data is always now ahead of Ontario data reports online
deathsOntarioRecent = filter(deathComparison, province_state=="Ontario", date>="2020-03-29") %>% arrange(date)

# Death growth scenarios -----------------------

# Function to generate new growth rate scenarios, given target and reference jurisdictions
generateGrowthScenario  <- function(jur, jurRef) {
  # jur = string for jurisdiction for which to generate scenario
  # jurRef = string for jurisdiction to act as the reference scenario in future
  # jur="Canada - British Columbia"; jurRef="South Korea"
  
  # Extract jurisdiction's actual daily growth rates (ignoring first day)
  # growthActual = deaths %>%
  #   filter(jurisdiction == jur, day_model > 8, deaths_growth_ma7 >= 0) %>%
  #   rename(rate=deaths_growth_ma7) %>%
  #   select(jurisdiction, date, day_model, rate)

  # NEW WAY: Set the last actual rate to be the 3-day moving average on one of the last 5 days of actual values
  # This introduces some uncertainty in the actual growth rate for the last day of actual values
  # Should eventually replace this with a proper estimate of the growth rate distribution
  growthActual = deaths %>%
    filter(jurisdiction == jur, day_model > 4) %>%
    rename(rate=deaths_growth_ma3) %>%
    select(jurisdiction, date, day_model, rate)
  lastDayActual = max(growthActual$day_model)
  lastDateActual = growthActual$date[growthActual$day_model==lastDayActual]
  lastDayActualSample = lastDayActual - sample(0:5,1)   # Go back 5-days when using a 3-day moving average
  # END OF NEW WAY
  
  # OLD WAY: Set the last actual rate to be the 7-day moving average on the last day of actual rates
  # growthActual = deaths %>%
  #   filter(jurisdiction == jur, day_model > 0) %>%
  #   rename(rate=deaths_growth_ma7) %>%
  #   select(jurisdiction, date, day_model, rate)
  # lastDayActual = max(growthActual$day_model)
  # lastDateActual = growthActual$date[growthActual$day_model==lastDayActual]
  # lastDayActualSample = lastDayActual
  # END OF OLD WAY

  lastRateActualSample = growthActual$rate[growthActual$day_model == lastDayActualSample]
  
  # Extract reference jurisdiction's 7-day moving average growth rates
  growthRef = deaths %>%
    filter(jurisdiction == jurRef, day_model > 8, deaths_growth_ma7 >= 0) %>%
    rename(rate=deaths_growth_ma7) %>%
    select(jurisdiction, date, day_model, rate)

  # To find the first reference day, find the first day moving forward with a growth rate within 5% of the actual growth rate, but not greater
  # If none of the days moving forward meet this criteria, repeat the search moving backwards in time
  # If again there are no results, take the first day with a lower growth rate
  # Finally, if all reference growth rates > current actual rate, set reference dat to last possible

  # Generate lists to perform search
  growthRateMatchingError <- 0.05 # Match growth rates within 5%
  growthRefLowerTest <- growthRef %>% filter(rate <= lastRateActualSample)
  growthRefStrictTest <- growthRefLowerTest %>%
    filter(rate >= lastRateActualSample * (1 - growthRateMatchingError))
  growthRefStrictTestAfter <- growthRefStrictTest %>%
    filter(day_model >= lastDayActual)
  growthRefStrictTestBefore <- growthRefStrictTest %>%
    filter(day_model < lastDayActual)

  # Perform search
  firstDayRef <- case_when(
    nrow(growthRefStrictTestAfter) > 0  ~ min(growthRefStrictTestAfter$day_model),
    nrow(growthRefStrictTestBefore) > 0 ~ max(growthRefStrictTestBefore$day_model),
    nrow(growthRefLowerTest) > 0        ~ min(growthRefLowerTest$day_model),
    TRUE                                ~ max(growthRef$day_model)
  )
  firstDateRef = growthRef$date[growthRef$day_model==firstDayRef]

  # Extract all the reference rates that start on this day
  growthRefLower = growthRef %>% filter(day_model >= firstDayRef)
  
  # Shift the day & date of these reference rates to start on the day after the jurisdiction's actuals
  growthFuture = growthRefLower %>%
    mutate(day_model = day_model - (firstDayRef - lastDayActual) + 1,
           date = date - days(firstDateRef) + days(lastDateActual) + 1)
  
  # Join the reference rates to the actual rates to generate the future scenario
  growthScenario = bind_rows(growthActual, growthFuture)
  
  # Now shift the date from date of death back to date of infection
  growthScenario = growthScenario %>% mutate(date = date - infectionPeriod)
  
  return(growthScenario)
}

# Clear object gathering all the growth scenarios
if (exists("allGrowthScenarios")) { remove(allGrowthScenarios) }

for (jur in jurisdictionsModel) {
  # Loop over every jurisdiction to be included in the model
  print(paste0("Jurisdiction: ", jur))
  for (iter in seq(1,1000)) {
    
    # Randomly select a reference jurisdiction for each iteration
    jurRef = sample(jurisdictionsGrowthReferenceList[[jur]],1)

    # Generate new scenario
    growthScenario = generateGrowthScenario(jur, jurRef)
    
    # Fix up the scenario, jurisdiction and source fields
    growthScenario = growthScenario %>% 
      rename(source = jurisdiction) %>%
      mutate(scenario = jurRef, jurisdiction=jur, iteration=iter) %>%
      select(scenario, jurisdiction, source, date, day_model, iteration, rate)
    
    # Add scenario to previously generated scenarios
    if (exists("allGrowthScenarios")) {
      allGrowthScenarios = bind_rows(allGrowthScenarios, growthScenario)
    } else {
      allGrowthScenarios = growthScenario
    }
  }  # next iter
}  # next jur

# Write to file
write_csv(allGrowthScenarios, paste0(growthFolder, "/", "growth-output.csv"))


# Death regression ----------------

# Fit the model N(t) = N(0)r^t to the cumulative deaths for Canada and major provinces
# Fit for each jurisdiction starting on the first day number of deaths >= threshold
# Regression for y = ab^x: # Fit log(y) = a* + b*x, where a=10a* and b=10b*

for (period in c("pre", "post", "all")) {
  # Repeat regression for each period
  
  # period = "all"  # For testing
  
  # Setup saving console regression output to file for this period
  suffix = paste0("-", period)
  sink(paste0(outputFolder, "/", "deaths-regression-output", suffix, ".txt"))
  
  # Clear object gathering all the model results for this period
  if (exists("deathsRegAll")) { remove(deathsRegAll) }
  
  for (jur in jurisdictionsRegression) {
    # Repeat regresssion for each jurisdiction
    
    # jur="Canada"   # For testing
    
    # Remove initial dates when there were too few deaths
    deathsReg = filter(deaths, jurisdiction==jur, deaths_cumulative >= deathsCumulativeRegMin)
    
    # Get the first and last date of the full period (i.e. with all data)
    deathsRegInitialDate = min(deathsReg$date)
    deathsRegFinalDate = max(deathsReg$date)
    
    # Shorten the period based on breakpoints if period set to pre or post
    if (period %in% c("pre")) { deathsRegFinalDate = interventionDate$date[interventionDate$jurisdiction==jur] - 1 + infectionPeriod }
    if (period %in% c("post")) { deathsRegInitialDate = interventionDate$date[interventionDate$jurisdiction==jur] + infectionPeriod }
    
    # Restrict dates to the period of interest (i.e pre, post or all)
    deathsReg = filter(deaths, jurisdiction==jur, date >= deathsRegInitialDate, date <= deathsRegFinalDate)
    
    print(jur)
    print(paste0("Period: ",period))
    print(paste0("deathsRegInitialDate: ",deathsRegInitialDate))
    print(paste0("deathsRegFinalDate: ",deathsRegFinalDate))
    
    # Linear regression
    lm = lm(deaths_cumulative_log~day_model, data=deathsReg)
    print(summary(lm))
    
    # Regression coefficients transformed back to original growth model
    intercept = summary(lm)$coefficients[1,1]
    slope = summary(lm)$coefficients[2,1]
    deathsGrowthInitTotal = 10^intercept
    deathsGrowthRateTotal = 10^slope
    
    print(paste0("deathsGrowthInitTotal: ",deathsGrowthInitTotal))
    print(paste0("deathsGrowthRateTotal: ", deathsGrowthRateTotal))
    print("******************************************************")
    
    # Add model projections back into regression dataset
    deathsReg = mutate(deathsReg, deaths_cumulative_log_model = intercept + (slope*day_model),
                       deaths_cumulative_model = deathsGrowthInitTotal * (deathsGrowthRateTotal^day_model)) %>%
      select(jurisdiction, date, day_model, deaths_cumulative_log, deaths_cumulative_log_model, deaths_cumulative_model)
    
    # Add the projections to a list accumulating all the provincial model projections
    if (exists("deathsRegAll")) {
      deathsRegAll = bind_rows(deathsRegAll, deathsReg)
    } else {
      deathsRegAll = deathsReg
    }
  }  # Next jurisdiction
  
  # Add model projections back into regression dataset for this period
  deathsPeriod = left_join(deaths, deathsRegAll)
  
  # Stop saving regression output to file for this period
  sink()
  
  # Write regression output to file for this period
  suffix = paste0("-", period)
  write_csv(deathsPeriod, paste0(outputFolder, "/", "deaths-output", suffix, ".csv"))
  
}  # Next period

