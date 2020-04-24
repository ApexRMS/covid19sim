#
# covid19-constants.R: Constants for analysis and plotting of Covid-19 data in Canada
#
# Developed by ApexRMS

library(lubridate)

deathsCumulativeRegMin = 5  # Minimum cumulative number of deaths for death growth rate regression
casesCumulativeRegMin = 15  # Minimum cumulative number of cases for cases growth rate regression
infectionPeriod = 23        # Delay between infection and death (for analysis)

pcDenominatorDeaths = 1000000    # Denominator in the per capita calculations for deaths
pcDenominatorCases = 100000    # Denominator in the per capita calculations for cases
pcDenominatorTests= 1000 # Denominator in the per capita calculations for tests

# Jurisdictions to be included in plots
jurisdictionsFocalWorld = c("Italy", "Spain", "France", "South Korea", "Canada")
jurisdictionsFocalWorldUS = c("Italy", "Spain", "France", "South Korea", "United States", "Canada")
jurisdictionsFocalCanada = c("Canada", "Canada - Quebec", "Canada - Ontario", "Canada - Alberta", "Canada - British Columbia")

# Jurisdictions to be used for reference in the growth rate scenarios
jurisdictionsGrowthReference = c("Italy", "Spain", "France", "South Korea", "United States")   # Complete list (for analysis)
jurisdictionsGrowthReference1 = c("Italy", "Spain", "France", "South Korea", "United States")  # Partial list (for library sampling)
jurisdictionsGrowthReference2 = c("Italy", "Spain", "France", "South Korea")                   # Another partial list

jurisdictionsGrowthReferenceList = list("Canada" = jurisdictionsGrowthReference1,
                                    "Canada - Alberta" = jurisdictionsGrowthReference2,
                                    "Canada - British Columbia" = jurisdictionsGrowthReference2,
                                    "Canada - Ontario" = jurisdictionsGrowthReference1,
                                    "Canada - Quebec" = jurisdictionsGrowthReference1)
# Jurisdictions to be included in growth regressions
jurisdictionsRegression = c("Italy", "Spain", "France", "South Korea",
                            "Canada", "Canada - Quebec", "Canada - Ontario", "Canada - Alberta", "Canada - British Columbia")

# Jurisdictions to include in the model
#jurisdictionsModel = c("Canada")
jurisdictionsModel = jurisdictionsFocalCanada

# Start dates of the post-intervention period by jurisdiction fro regressions
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

