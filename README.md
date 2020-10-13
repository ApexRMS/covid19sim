# Covid-19 Forecasting with R and SyncroSim

This repository contains code and data used to generate a demonstration model of
COVID-19 infections and deaths in Canada for the [epidemic Package](https://github.com/ApexRMS/epidemic)
in the [SyncroSim](http://www.syncrosim.com) modeling framework. Forecasts are
made both on a national level and on a provincial level for six provinces.
Current and past forecasts can be viewed and compared to case data and forecasts
made by the Institute for Health Metrics and Evaluation
([IHME](http://www.healthdata.org/covid)) on the accompanying [shinyapp](https://apexrms.shinyapps.io/covid19canada/).
Code for generating the shiny app is also maintained in this repository.

## Running forecasts locally

### Dependencies

To run forecasts and build the shiny app locally, you first require a working
installation of R, git, and [SyncroSim](https://syncrosim.com/download/). While
Rstudio is not necessary, it is recommended if you are unfamiliar with running R
scripts from the commandline. This project is tested and maintained with R
version 4.0.2, but should work with R version 4.0.0 or newer.

### Setup

Begin by cloning this repository and open the `covid19sim.Rproj` project in
Rstudio to ensure your R session loads the correct working directory. Open the
`setup.R` script in the root directory and either source the file or run the
file line-by-line. This script will install any missing R packages, clone
external data sources used to generate the forecast, and pull in the IHME
forecasts displayed in the shiny app. Finally, open SyncroSim and ensure the
`epidemic` package is installed.

This setup procedure only needs to be run once after cloning the repository.

### Daily forecasts

To generate a forecast, first ensure that you are in the correct working
directory. The easiest way to do this is to open the `covid19sim.Rproj` project
file in RStudio. From here, open the `update-forecast.R` and either hit the "Run
App" button in the top-right corner, or run the file line-by-line. This script
will check that internal and external data sources are up-to-date, perpare the
model inputs, and build a local shiny app to visualize the results.

ApexRMS runs this forecast daily and archives past forecast results in this
repository for posterity. Try playing around with the Forecast Dates in the
[shiny app](https://apexrms.shinyapps.io/covid19canada/) to see these results.
