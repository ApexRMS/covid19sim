#
# headers/constants.R: Constants for analysis and plotting of Covid-19 data in Canada
#
# Developed by ApexRMS

library(tidyverse)
library(lubridate)

# ******************* SET THIS DATE BEFORE RUNNING **************
# This should be the day after the last death data
# runDate = as_date("2020-08-17")
runDate = today()
# ***************************************************************


deathsCumulativeRegMin = 5  # Minimum cumulative number of deaths for death growth rate regression
casesCumulativeRegMin = 15  # Minimum cumulative number of cases for cases growth rate regression
infectionPeriod = 23        # Delay between infection and death (for analysis)

pcDenominatorDeaths = 1000000    # Denominator in the per capita calculations for deaths
pcDenominatorCases = 100000    # Denominator in the per capita calculations for cases
pcDenominatorTests= 1000 # Denominator in the per capita calculations for tests

# Jurisdictions to include in the model
jurisdictionsModel = c("Canada", 
                       "Canada - Alberta",
                       "Canada - British Columbia",
                       "Canada - Manitoba",
                       "Canada - Nova Scotia",
                       "Canada - Ontario",
                       "Canada - Quebec")

# Jurisdictions to be used for reference in the growth rate scenarios
jurisdictionsGrowthReference = c("Australia", "France", "Italy", "Japan", "South Korea", "United States",
                                 "United States - California", "United States - Kansas", 
                                 "United States - Washington", "United States - Michigan",
                                 "Germany", "Greece", "Thailand", "Singapore", "New Zealand", "Sweden",
                                 "Ecuador", "Bangladesh", "Belgium", "Iraq", "Finland", "Netherlands",
                                 "United States - Wisconsin", "United States - Illinois")
jurisdictionsGrowthReferenceCA = c("Iraq", "Belgium", "United States - Michigan", "Ecuador")
jurisdictionsGrowthReferenceQC = c("United States - Wisconsin", "United States - Illinois", "Belgium", "United States - Michigan")
jurisdictionsGrowthReferenceON = c("Iraq", "Belgium", "United States - Michigan", "Ecuador")
jurisdictionsGrowthReferenceBC = c("Ecuador", "Finland", "Netherlands", "Sweden")
jurisdictionsGrowthReferenceAB = c("Ecuador", "Finland", "Netherlands", "Sweden")
jurisdictionsGrowthReferenceMB = c("Iraq", "Belgium", "United States - Michigan", "Bangladesh")
jurisdictionsGrowthReferenceNS = c("Australia", "Singapore", "Djibouti", "New Zealand")

jurisdictionsGrowthReferenceList = list("Canada" = jurisdictionsGrowthReferenceCA,
                                    "Canada - Alberta" = jurisdictionsGrowthReferenceAB,
                                    "Canada - British Columbia" = jurisdictionsGrowthReferenceBC,
                                    "Canada - Manitoba" = jurisdictionsGrowthReferenceMB,
                                    "Canada - Nova Scotia" = jurisdictionsGrowthReferenceNS,
                                    "Canada - Ontario" = jurisdictionsGrowthReferenceON,
                                    "Canada - Quebec" = jurisdictionsGrowthReferenceQC)

# Jurisdictions to be included in plots
# jurisdictionsFocalWorld = c("Italy", "Spain", "France", "South Korea", "Canada")
# jurisdictionsFocalWorldUS = c("Italy", "Spain", "France", "South Korea", "United States", "Canada")
# jurisdictionsFocalCanada = c("Canada", "Canada - Quebec", "Canada - Ontario", "Canada - Alberta", "Canada - British Columbia")

# Jurisdictions to be included in growth regressions
jurisdictionsRegression = c("Italy", "Spain", "France", "South Korea",
                            "Canada", "Canada - Quebec", "Canada - Ontario", "Canada - Alberta", "Canada - British Columbia")

# Start dates of the post-intervention period by jurisdiction for growth regressions
interventionDate = tribble(~jurisdiction, ~date,
                          "Canada",                     ymd("2020-03-14"),
                          "Canada - Alberta",           ymd("2020-03-14"),
                          "Canada - British Columbia",  ymd("2020-03-14"),
                          "Canada - Ontario",           ymd("2020-03-14"),
                          "Canada - Quebec",            ymd("2020-03-14"),
                          "France",                     ymd("2020-03-15"),
                          "Italy",                      ymd("2020-03-08"),
                          "South Korea",               ymd("2020-02-23"),
                          "Spain",                      ymd("2020-03-12"))

