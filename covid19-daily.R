#
# covid19-daily.R: Script to update data and forecasts daily
#
# Developed by ApexRMS
#
# Input: Set the runDate in covid-19-constants.R. If not specified then will default to today
# Outputs:
#   Plot of the moving average of growth rates
#   Template .ssim library file
#   Simulated .ssim library file
#   Updated CSV files of deaths and forecasts in git repo

# Setup -------------------------
library(rstudioapi)

# Set the working directory to the script's folder (works only in RStudio)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("covid19-local.R")
source("covid19-constants.R")

# Run analysis -----------------------------
source("covid19-analysis.R")


# Plot growth rates ------------------------

deaths <- read_csv(paste0(outputFolder, "/", "deaths-output.csv"))
whiteTheme <- theme(
  panel.background = element_rect(fill = NA),
  panel.border = element_rect(fill = NA, color = "grey75"),
  axis.ticks = element_line(color = "grey55"),
  panel.grid.major = element_line(color = "grey95", size = 0.2),
  panel.grid.minor = element_line(color = "grey95", size = 0.2),
  plot.title = element_text(hjust=0.5, size=18),
  axis.title = element_text(size=18),
  strip.text = element_text(size=18),
  axis.text = element_text(size=14),
  legend.key = element_rect(fill = NA),
  legend.text = element_text(size=14),
  legend.title = element_blank())

# Plot first set of jurisdictions
minDayModel = 40
plotData = mutate(deaths, deaths_growth_ma7 = deaths_growth_ma7 * 100) %>%
  filter(jurisdiction %in% jurisdictionsGrowth1, day_model>=minDayModel)
plotDataRef = mutate(deaths, deaths_growth_ma7 = deaths_growth_ma7 * 100) %>%
  filter(jurisdiction %in% jurisdictionsGrowthReference1, day_model>=minDayModel)
p <- plotData %>%
  ggplot(aes(x=day_model, y=deaths_growth_ma7, group=jurisdiction)) +
  geom_line(data=plotData, aes(color=jurisdiction), size=1.5) +
  geom_line(data=plotDataRef, aes(color=jurisdiction), size=0.5) +
  scale_x_continuous("Days since 5 total deaths") +
  scale_y_continuous("% daily growth (7-day moving avg.)") + 
  whiteTheme
p
ggsave(paste0(plotFolder, "/deaths_growth_ma7_1_", runDate, ".png"), p, width=10, height=5, dpi=300)

# Plot second set of jurisdictions
minDayModel = 40
plotData = mutate(deaths, deaths_growth_ma7 = deaths_growth_ma7 * 100) %>%
  filter(jurisdiction %in% jurisdictionsGrowth2, day_model>=minDayModel)
plotDataRef = mutate(deaths, deaths_growth_ma7 = deaths_growth_ma7 * 100) %>%
  filter(jurisdiction %in% jurisdictionsGrowthReference2, day_model>=minDayModel)
p <- plotData %>%
  ggplot(aes(x=day_model, y=deaths_growth_ma7, group=jurisdiction)) +
  geom_line(data=plotData, aes(color=jurisdiction), size=1.5) +
  geom_line(data=plotDataRef, aes(color=jurisdiction), size=0.5) +
  scale_x_continuous("Days since 5 total deaths") +
  scale_y_continuous("% daily growth (7-day moving avg.)") + 
  whiteTheme
p
ggsave(paste0(plotFolder, "/deaths_growth_ma7_2_", runDate, ".png"), p, width=10, height=5, dpi=300)

# Plot third set of jurisdictions
minDayModel = 9
plotData = mutate(deaths, deaths_growth_ma7 = deaths_growth_ma7 * 100) %>%
  filter(jurisdiction %in% jurisdictionsGrowth3, day_model>=minDayModel)
plotDataRef = mutate(deaths, deaths_growth_ma7 = deaths_growth_ma7 * 100) %>%
  filter(jurisdiction %in% jurisdictionsGrowthReference3, day_model>=minDayModel)
p <- plotData %>%
  ggplot(aes(x=day_model, y=deaths_growth_ma7, group=jurisdiction)) +
  geom_line(data=plotData, aes(color=jurisdiction), size=1.5) +
  geom_line(data=plotDataRef, aes(color=jurisdiction), size=0.5) +
  scale_x_continuous("Days since 5 total deaths") +
  scale_y_continuous("% daily growth (7-day moving avg.)") + 
  whiteTheme
p
ggsave(paste0(plotFolder, "/deaths_growth_ma7_3_", runDate, ".png"), p, width=10, height=5, dpi=300)



# Shiny growth rates - run app.R --------------

# Create SyncroSim template -----------------------------
source("covid19-ssim-template.R")


# Run SyncroSim template -----------------------------
source("covid19-ssim-run.R")

