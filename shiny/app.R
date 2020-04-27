# Shiny application for SyncroSim epidemic package COVID-19 output.
# Copyright © 2007-2020 Apex Resource Management Solutions Ltd. (ApexRMS). All rights reserved.
# The TERMS OF USE and END USER LICENSE AGREEMENT for this software can be found in the LICENSE file.

library(shiny)
library(ggplot2)

source("helpers.R")

minDate = get_min_date()
maxDate = get_max_date()
Jurisdictions = get_jurisdictions()
dailyInfected = get_infected_daily_data()
cumulativeInfected = get_infected_cumulative_data()
dailyDeaths = get_deaths_daily_data()
cumulativeDeaths = get_deaths_cumulative_data()

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

shinyApp(ui, server)
