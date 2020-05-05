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

# Define colours for all focal jurisdictions
brewer.pal(n = 8, name = "Dark2")
jurisdictionLineColor <- c("Canada"="red3", "France"="darkorchid3", "Italy"="goldenrod1", "South Korea"="dodgerblue4", "Spain"="darkolivegreen4", "Canada - Alberta"="#666666", "Canada - British Columbia"="#A6761D", "Canada - Ontario"="#1B9E77", "Canada - Quebec"="#D95F02")
jurisdictionLineColor <- c("Canada"="red3", "United States"="black", "France"="darkorchid3", "Italy"="goldenrod1", "South Korea"="dodgerblue4", "Spain"="darkolivegreen4", "Canada - Alberta"="#666666", "Canada - British Columbia"="#A6761D", "Canada - Ontario"="#1B9E77", "Canada - Quebec"="#D95F02")
#jurisdictionLineColor <- c("Canada"="red3", "France"="#C4961A", "Italy"="#F4EDCA", "South Korea"="#D16103", "Spain"="#C3D7A4", "Canada - Alberta"="#666666", "Canada - British Columbia"="#A6761D", "Canada - Ontario"="#1B9E77", "Canada - Quebec"="#D95F02")
jurisdictionLineType <- c("Canada"="solid", "United States"="longdash", "France"="longdash",  "Italy"="longdash", "South Korea"="longdash", "Spain"="longdash", "Canada - Alberta"="solid", "Canada - British Columbia"="solid", "Canada - Ontario"="solid", "Canada - Quebec"="solid" )
jurisdictionLineSize <- c("Canada"=1, "United States"=0.5, "France"=0.5,  "Italy"=0.5, "South Korea"=0.5, "Spain"=0.5, "Canada - Alberta"=1, "Canada - British Columbia"=1, "Canada - Ontario"=1, "Canada - Quebec"=1)
jurisdictionLabels <- c("Canada"="Canada", "United States"="United States", "France"="France", "Italy"="Italy", "South Korea"="South Korea", "Spain"="Spain", "Canada - Alberta"="Alberta", "Canada - British Columbia"="British Columbia", "Canada - Ontario"="Ontario", "Canada - Quebec"="Quebec")

# Define labels for canadian focal jurisdictions
jurisdictionFocalCanadaLabels <- c("Canada"="Canada", "Canada - Alberta"="Alberta", "Canada - British Columbia"="British Columbia", "Canada - Ontario"="Ontario", "Canada - Quebec"="Quebec")

plotData = mutate(deaths, deaths_growth_ma7 = deaths_growth_ma7 * 100) %>%
  filter(jurisdiction %in% c(jurisdictionsFocalWorldUS, jurisdictionsFocalCanada), day_model>=9)

p <- plotData %>%
  ggplot(aes(x=day_model, y=deaths_growth_ma7, group=jurisdiction)) +
  geom_line(aes(color=jurisdiction, linetype=jurisdiction, size=jurisdiction)) +
  #  geom_point(aes(color=jurisdiction), size=1.5) +
  scale_color_manual(values=jurisdictionLineColor, labels = jurisdictionLabels) +
  scale_linetype_manual(values=jurisdictionLineType, labels = jurisdictionLabels) +
  scale_size_manual(values = jurisdictionLineSize, labels = jurisdictionLabels) +
  scale_x_continuous("Days since 5 total deaths") +
  scale_y_continuous("% daily growth (7-day moving avg.)") + 
  whiteTheme
p
ggsave(paste0(plotFolder, "/deaths_growth_ma7_world.png"), p, width=10, height=5, dpi=300)


# Create SyncroSim template -----------------------------
source("covid19-ssim-template.R")


# Run SyncroSim template -----------------------------
source("covid19-ssim-run.R")

