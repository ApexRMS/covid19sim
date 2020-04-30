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

# Working directory
setwd("shiny/covid19canada")

# Input parameters
source("helpers.R")

# Output files
outputFiles <- list.files("data")

# Load data
deaths_daily <- read.csv(paste0("data/", outputFiles[which(grepl("deaths-daily", outputFiles))]))
deaths_cumulative <- read.csv(paste0("data/", outputFiles[which(grepl("deaths-cumulative", outputFiles))]))
infected_daily <- read.csv(paste0("data/", outputFiles[which(grepl("infected-daily", outputFiles))]))
infected_cumulative <- read.csv(paste0("data/", outputFiles[which(grepl("infected-cumulative", outputFiles))]))

#### .. ####
minDate = get_min_date()
maxDate = get_max_date()
Jurisdictions = get_jurisdictions()
dailyInfected = get_infected_daily_data()
cumulativeInfected = get_infected_cumulative_data()
dailyDeaths = get_deaths_daily_data()
cumulativeDeaths = get_deaths_cumulative_data()

#### Create UI ####
ui <- fluidPage(
  
  titlePanel("SyncroSim COVID-19"), 
  
  sidebarLayout(
    
    sidebarPanel(width = 4, 
      selectInput("jur", 
        label = "Jurisdiction",
         choices = Jurisdictions,
          selected = Jurisdictions[1])),
    
    mainPanel(fluidRow(column(8,align="center",
      plotOutput("chart"),
        sliderInput("range", width="100%", label = "Date Range",
          min = minDate, max = maxDate, value = c(minDate, maxDate), 
          step = 1))))
  )
)

#### Create server ####
server <- function(input, output) {
  
  output$chart <- renderPlot({
    
    d1 = subset_jur_time(dailyInfected, input$jur, input$range[1],input$range[2])
    d2 = subset_jur_time(cumulativeInfected, input$jur, input$range[1],input$range[2])
    d3 = subset_jur_time(dailyDeaths, input$jur, input$range[1],input$range[2])
    d4 = subset_jur_time(cumulativeDeaths, input$jur, input$range[1],input$range[2]) 
    
    p = ggplot(d1, aes(x=as.Date(Date), y=Mean)) + geom_line(color='red') + scale_x_date(date_labels = "%m-%d") + xlab("") 
    
    return(p)
  })

}

#### Run Shiny app ####
shinyApp(ui, server)
