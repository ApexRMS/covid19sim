# Shiny application for SyncroSim epidemic package COVID-19 output.
# Copyright ? 2007-2020 Apex Resource Management Solutions Ltd. (ApexRMS). All rights reserved.
# The TERMS OF USE and END USER LICENSE AGREEMENT for this software can be found in the LICENSE file.

#### Workspace ####
# Packages
library(tidyverse)
library(magrittr)
library(shiny)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(shinyWidgets)
library(cowplot)

# Input parameters
brewer.pal(n = 8, name = "Dark2")
jurisdictionLineColor <- c("Canada"="red3","Canada - Alberta"="#666666", "Canada - British Columbia"="#A6761D", "Canada - Ontario"="#1B9E77", "Canada - Quebec"="#D95F02", "Observed" = "white", "Modeled" = "black")

# Output files
outputFiles <- list.files("data")

# Load data
dailyDeaths <- read.csv(paste0("data/", outputFiles[which(grepl("deaths-daily", outputFiles))])) %>%
  mutate(Date = as.Date(Date), date_model_run = as.Date(date_model_run)) %>%
  mutate(Metric = "Daily Deaths")

cumulativeDeaths <- read.csv(paste0("data/", outputFiles[which(grepl("deaths-cumulative", outputFiles))])) %>%
  mutate(Date = as.Date(Date), date_model_run = as.Date(date_model_run)) %>%
  mutate(Metric = "Cumulative Deaths")

dailyInfected <- read.csv(paste0("data/", outputFiles[which(grepl("infected-daily", outputFiles))])) %>%
  mutate(Date = as.Date(Date), date_model_run = as.Date(date_model_run)) %>%
  mutate(Metric = "Daily Infections")

cumulativeInfected <- read.csv(paste0("data/", outputFiles[which(grepl("infected-cumulative", outputFiles))])) %>%
  mutate(Date = as.Date(Date), date_model_run = as.Date(date_model_run)) %>%
  mutate(Metric = "Cumulative Infections")

# Format data
data <- bind_rows(dailyDeaths, dailyInfected, cumulativeDeaths, cumulativeInfected) %>%
  mutate(DataType = ifelse(is.na(Lower), "Observed", "Modeled")) %>%
  mutate(DataType = ordered(DataType, level=c("Observed", "Modeled"))) %>%
  mutate(Metric = ordered(Metric, levels=c("Daily Infections", "Daily Deaths", "Cumulative Infections", "Cumulative Deaths")))

#### Helpers ####
forecastDates <- sort(unique(data$date_model_run))
minDate <- min(data$Date)
maxDate <- max(data$Date)
jurisdictions <- sort(unique(data$Jurisdiction))
whiteTheme <- theme(panel.background = element_rect(fill = NA),
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

#### UI ####
ui <- fluidPage(
  
  br(),
  
  sidebarLayout(
    
    sidebarPanel(width=3, 
      selectInput("forecastDate", 
        label = "Forecast Date",
         choices = forecastDates,
          selected = max(forecastDates)),
      
      checkboxGroupInput("juris", 
                         label = "Jurisdictions",
                         choices = jurisdictions,
                         selected = jurisdictions[1]),
      
      materialSwitch("logY",
                     label = strong("Log Y axis"), 
                     value = F,
                     status = "primary",
                     width="100%"),
      
      p("The projections shown on this page are made using the", a(href="https://syncrosim.com/", "SyncroSim", target="_blank"), "model framework."),
      
      fluidRow(column(12, align="center", offset = 0,
                      actionButton(inputId='modelDetails',
                                   label="Model Details",
                                   icon = icon("info-circle"),
                                   onclick ="window.open('http://www.apexrms.com/covid19/', '_blank')"),
                      tags$style(type='text/css', "#button {vertical-align- middle; height- 50px; width- 100%; font-size- 30px; justify-content: center;}"),
                      br(),
                      br())),
      
      p(strong("Note that the simulation results presented here are simply a demonstration of the model framework, and should not be considered actual predictions for any of these jurisdictions."))),
    
    mainPanel(
      
      titlePanel(h2("COVID-19 Forecasts using SyncroSim", align="center")),
      
      fluidRow(column(12, align="center",
                              
        plotOutput("chart", width="100%", height="430px"),
        
        sliderInput("range", width="100%", label = "Date Range",
          min = minDate, max = maxDate, value = c(minDate, maxDate), 
          step = 1))))
  )
)

#### Server ####
server <- function(input, output) {
  
  output$chart <- renderPlot({
    
    # Subset data based on user inputs
    dataSubset <- data %>%
      filter(Jurisdiction %in% input$juris) %>% # Only keep jurisdictions of interest
      filter(!((DataType == "Observed") & (!date_model_run == max(date_model_run)))) %>% # Remove observations for all but the most recent model
      filter(!((DataType == "Modeled") & (!date_model_run == input$forecastDate))) %>% # Remove predictions for all but the model run of interest
      filter(Date >= input$range[1] & Date <= input$range[2]) # Only keep dates of interest
    
    # Produce main plot (without legend)
    plot <- ggplot(dataSubset, aes(x=Date, y=Mean, color=Jurisdiction)) + 
      geom_ribbon(data=dataSubset[which(dataSubset$DataType=="Modeled"),], aes(ymin=Lower, ymax=Upper, fill=Jurisdiction), alpha=0.3, color=NA, show.legend=F) +
      geom_line(aes(linetype=DataType), size=1) +
      scale_linetype_manual(values=c("Observed"="solid", "Modeled"="dotted"), labels=c("Observed", 'Modeled (95% Confidence Interval)')) +
      scale_color_manual(values=jurisdictionLineColor) +
      scale_fill_manual(values=jurisdictionLineColor) +
      guides(color=F, linetype=F) +
      scale_y_continuous(name="Number of people", labels=scales::label_comma(), trans=ifelse(input$logY, "log10", "identity")) +
      whiteTheme +
      facet_wrap(vars(Metric), scales="free_y") +
      theme(axis.title.x = element_blank(),
            plot.margin=unit(c(10,0,0,0),"pt"))
    
    # Produce legend for Jurisdictions
    jurisdictionLegend <- ggplot(dataSubset, aes(x=Date, y=Mean, color=Jurisdiction)) + 
      geom_line(size=1) +
      scale_color_manual(values=jurisdictionLineColor) +
      whiteTheme +
      theme(legend.position = "top", 
            legend.justification = "left",
            legend.margin=margin())
    
    jurisdictionLegend <- get_legend(jurisdictionLegend)
    
    # Produce legend for data types
    typeLegend <- ggplot(dataSubset, aes(x=Date, y=Mean)) + 
      geom_ribbon(aes(ymin=Lower, ymax=Upper, fill=DataType), alpha=0.3, color=NA) +
      geom_line(aes(linetype=DataType), size=1) +
      scale_linetype_manual(values=c("Observed"="solid", "Modeled"="dotted"), labels=c("Observed"="Observed", "Modeled"='Modeled (95% Confidence Interval)')) +
      scale_fill_manual(values=jurisdictionLineColor, labels=c("Observed"="Observed", "Modeled"='Modeled (95% Confidence Interval)')) +
      whiteTheme +
      theme(legend.position = "top", 
            legend.justification = "left",
            legend.margin=margin())
    
    typeLegend <- get_legend(typeLegend)
    
    # Combine plot and legends
    p <- plot_grid(jurisdictionLegend, typeLegend, plot, ncol=1, rel_heights = c(1,1,15))
    return(p)
  })

}

#### Run Shiny app ####
shinyApp(ui, server)
