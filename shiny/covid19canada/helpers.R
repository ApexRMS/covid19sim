# Shiny application for SyncroSim epidemic package COVID-19 output.
# Copyright ? 2007-2020 Apex Resource Management Solutions Ltd. (ApexRMS). All rights reserved.
# The TERMS OF USE and END USER LICENSE AGREEMENT for this software can be found in the LICENSE file.

.get_data_dir <- function(){
  
  d = paste(getwd(), DATA_DIR_NAME, sep = "/")
  
  if (!dir.exists((d))){
    stop(paste("The data directory is missing : ", d))
  }
  
  return(d)
}

.get_data_file_name <- function(fileName){
  
  f = paste(.get_data_dir(), fileName, sep = "/")
  
  if (!file.exists(f)){
    stop(paste("The data file is missing : ", f))    
  }
  
  return(f)
}

get_infected_daily_data <- function(){
  
  df = read.csv(.get_data_file_name(INFECTED_DAILY_FILE_NAME))
  return(df[as.Date(df$date_model_run) == max(as.Date(df$date_model_run)),])
}

get_infected_cumulative_data <- function(){
  
  df = read.csv(.get_data_file_name(INFECTED_CUMULATIVE_FILE_NAME))
  return(df[as.Date(df$date_model_run) == max(as.Date(df$date_model_run)),])
}

get_deaths_daily_data <- function(){
  
  df = read.csv(.get_data_file_name(DEATHS_DAILY_FILE_NAME))
  return(df[as.Date(df$date_model_run) == max(as.Date(df$date_model_run)),])
}

get_deaths_cumulative_data <- function(){
  
  df = read.csv(.get_data_file_name(DEATHS_CUMULATIVE_FILE_NAME))
  return(df[as.Date(df$date_model_run) == max(as.Date(df$date_model_run)),])
}

get_min_date <- function(){
  
  df = get_infected_daily_data()
  return(min(as.Date(df$Date)))
}

get_max_date <- function(){
  
  df = get_infected_daily_data()
  return(max(as.Date(df$Date)))  
}

get_jurisdictions <- function(){
  
  df = get_infected_daily_data()
  return(unique(df[["Jurisdiction"]]))
}

subset_jur_time <- function(data, jur, start, end){
  
  return(data[
    data$Jurisdiction==jur & 
    as.Date(data$Date) >= start & 
    as.Date(data$Date) <= end,
    ])
}
