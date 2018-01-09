#########################################################
#  Code to update sensor data (full dataset)  

#     >haven't done anything with merging to location yet

#########################################################

# function for downloading all the sensors data without specific requirements.
fetchAllSensorsData = function() {
  
  #Using getMESA_data.R download all data and combine into a single dataframe
  setwd("X:/Production_code")
  source("getMESA_data.R", local = TRUE)
  loc_names <- suppressWarnings(suppressMessages(datafeed_get_files()))
  sensor_data <- datafeed_download_file(loc_names)
  sensor_data <- as.data.frame(sensor_data)

  #output data
  return(sensor_data)
}

#Get all data (Takes awhile)
sensor_data<-fetchAllSensorsData()


#write csv (takes awhile)
write.csv(x = sensor_data,file = "X:/Data/rawdata/sensor_data.csv" )
