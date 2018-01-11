################################################################
#  Code to update sensor data (full dataset) and add site id's #
################################################################

#------------------------------------------------------------------------------------------------------------#
# function for downloading all the sensors data without specific requirements.
#------------------------------------------------------------------------------------------------------------#
fetchAllSensorsData <- function() {
  
  #Using getMESA_data.R download all data and combine into a single dataframe
  setwd("X:/Production_code")
  source("getMESA_data.R", local = TRUE)
  loc_names <- suppressWarnings(suppressMessages(datafeed_get_files()))
  sensor_data <- datafeed_download_file(loc_names)
  sensor_data <- as.data.frame(sensor_data)

  #output data
  return(sensor_data)
}

#------------------------------------------------------------------------------------------------------------#
#Function to load the raw sensor location data from the Access Database (calls on the script "GetAccessTables.R" )
#------------------------------------------------------------------------------------------------------------#
fetchSensorLocationData <- function(){
  ## set the 32-bit script location
  pathIn32BitRScript <- "H:/Sensor_QC/GetAccessTables.R"
  ## run the 32 bit script GetAcessTables.R
  system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ",pathIn32BitRScript))
  ## Set the path for loading the rda file created from GetAccessTables.R 
  pathOutUpAccdb <- "H:/Sensor_QC/"
  ## load the sensor location table we just created
  load(paste0(pathOutUpAccdb,"sensor_location_data.rda"))
  
  return(sensorlocations)
}

#------------------------------------------------------------------------------------------------------------#
#Function to merge the sensor data and location data in order to add the location to each sensor observation
#------------------------------------------------------------------------------------------------------------#
addLocations<-function(sensordat,locationdat){
  
    #convert start/end times from location data to UTC
    utc_start_time<-  as.POSIXct(locationdat$local_start_time)
    attr(utc_start_time,"tzone")<-"UTC"
    utc_end_time<-  as.POSIXct(locationdat$local_end_time)
    
    #fill in all end times for current locations as the current time
    utc_end_time[is.na(utc_end_time)]<-  Sys.time()
    attr(utc_end_time,"tzone")<-"UTC"
  
    #Add column to sensor data to fill in with site id's
    sensordat["site_id"]<-rep(NA,nrow(sensordat))
    
    #loop accross location file entries and add site_id to sensor data
    for(i in 1:nrow(locationdat)){
      #indicator that correct monitor ID and within the time range
      time_ind<- sensordat$monitor==as.character(locationdat$monitor_id[i]) & sensordat$date >  utc_start_time[i] & sensordat$date <  utc_end_time[i] 
      #add site_id to sensor data that match the monitor/time frame
      sensordat$site_id[time_ind]<-as.character(locationdat$site_id[i])
    }
  
    #mean(is.na(sensordat$site_id))   
    # as of 1/11/18, only 89% completeness for site_id (some times don't fall into a given time frame)

  return(sensordat)
}


#------------------------------------------------------------------------------------------------------------#
# Execute functions and write csv
#------------------------------------------------------------------------------------------------------------#

#Get all sensor data (Takes awhile)
sensor_data<-fetchAllSensorsData()

#Get sensor location data
sensor_locations<-fetchSensorLocationData()

#Add site locations to sensor data
combined_data<-addLocations(sensor_data,sensor_locations)

#remove index column
combined_data[,"X1"]<-NULL

#write csv (takes awhile)
write.csv(x = combined_data,file = "X:/Data/rawdata/sensor_data.csv",row.names = F)
