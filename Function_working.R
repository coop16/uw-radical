# analyzeData - analyze correlation between one station and one sensor
# Input:
# station - station name
# sensor - sensor name
# pollutantStation - pollutant name in station data
# pollutantSensor - pollutant name in sensor data
# startdate - start date of analyzed period in local timezone
# enddate - end date of analyzed period in local timezone
# removeOutliers - if we want to remove outliers before analysis. default value: TRUE
# byDay - if we want to aggregate data by day. Default value: FALSE
# Assumption: sensor and station were collocated at specific date range
# Assumption: pullutant names have the same semantic 
# Output: result - list of following items:
#                  mergedData
#                  summary of linear regression
#             TODO: to download all required libraries that can be needed for this code.
#             install.packages("excel.link")
#             library("excel.link")
analyzeData <- function(station, sensor, pollutantStation, pollutantSensor, startdate, enddate, removeOutliers = TRUE, byDay = FALSE) {
  if(startdate > enddate) {
    print("Startdate is later than enddate");
    return(NULL);
  }
  
  # fetch data for station
  print("Fetching station data");
  stationData = fetchStationData(station, pollutantStation, startdate, enddate)
  # fetch data for sensor
  print("Fetching sensor data");
  sensorData = fetchSensorData(sensor, pollutantSensor, startdate, enddate)
  # TODO! Investigate misalignment of dates
  
  # TODO analyze
  # Clean data
  ## TODO add sizes of stationData, sensorData to print out
  print("Start cleaning data");
  cleanData(stationData, sensorData, removeOutliers)
  
  mergedData = merge(stationData, sensorData, by = "Date")
  # plot(mergedData[,2], ylim = c(-150,150))
  # lines(mergedData[,2])
  # lines(mergedData[,3])
  
  # run linear regression
  fit<-lm(formula = paste(colnames(mergedData)[2], "~", colnames(mergedData)[3], sep = ""), data=mergedData)
  
  # TODO: update output comment on function signature
  result = list(station = station,
                sensor = sensor,
                pollutantStation = pollutantStation,
                pollutantSensor = pollutantSensor,
                startdate = startdate,
                enddate = enddate,
                removeOutliers = removeOutliers, 
                byDay = byDay, 
                mergedData = mergedData, 
                summary = summary(fit))
  
  
  return(result);
}

# cleanData - cleans data of outliers and other factors (TODO)
# Input:
# stationData - data of station to clean
# sensorData - data of sensor to clean
# removeOutliers - if we want to remove outliers
# Output: void
# Side effect: sensorData and stationData are cleaned from outliers
cleanData <- function(stationData, sensorData, removeOutliers) {
  if(removeOutliers) {
    print("TODO Removing outliers")
    # TODO detect outliers
    # TODO: remove outliers
  } else {
    print("keeping outliers");
  }
  
}

# Input: fetchStationData - gets data for specific station between dates
# station - station name
# pollutant - pollutant name
# startdate - start date of analyzed period
# enddate - end date of analyzed period
# byDay - if we want to aggregate data by day. Default value: FALSE
# Output: station data
fetchStationData <- function(station, pollutant, startdate, enddate, byDay = FALSE) {
  # TODO: get file for all dates
  # TODO: automatic obtain file for dates
  # TODO: to add NO+NO2 = NOX coloumn
  
  #### Reading data of Seattle stations from period of 06.01.17 ##### 
  stationData = read.csv("H:/My data/RSea_stations_06012017_08312017.csv", header=T)
  
  #### Convert ObsDateTime to date field ObsDateTimeAsDate
  stationData$ObsDateTimeAsDate = as.POSIXct(stationData$ObsDateTime, format = "%m/%d/%Y %H:%M")
  # remember index of column that we added for date
  dateColIndex = grep("ObsDateTimeAsDate", colnames(stationData), value=FALSE)
  
  # get only relevant station
  if(pollutant == "NO")
    filterString = paste(station, pollutant, "ppb", sep = ".")
  else 
    filterString = paste(station, pollutant, "", sep = ".")
  stationData = stationData[, c(grep(filterString, colnames(stationData), value=FALSE), dateColIndex)]
  
  # get only relevant dates
  stationData = subset(stationData, ObsDateTimeAsDate >= startdate
                       & ObsDateTimeAsDate <= endDate)
  
  stationData = setNames(stationData[, c(2,1)], c("Date", generatePollutantColumnName("Station", pollutant)))
  
  # stationData is not accessible after functions finishes
  return(stationData)
}

# Input: fetchSensorData - gets data for specific sensor between dates (TODO)
# sensor - sensor name
# startdate - start date of analyzed period
# pollutant - pollutant name
# enddate - end date of analyzed period
# byDay - if we want to aggregate data by day. Default value: FALSE
# Output: sensor data
fetchSensorData <- function(sensor, pollutant, startdate, enddate, byDay = FALSE) {
  currDir <- getwd() 
  setwd("X:/Production_code")
  source("X:/Production_code/getMESA_data.R", local = TRUE)
  source("X:/Production_code/getMESA_data_Kairos.R", local = TRUE)
  sensor.loc <- fread("X://Data//Sensor Location table.csv")
  loc_names <- suppressWarnings(suppressMessages(sensor_data <- datafeed_get_files()))
  sensor_data <- datafeed_download_file(loc_names)
  sensor_data <- data.table(sensor_data)
  setwd(currDir)
  
  # TODO: get file for all dates
  # TODO: automatic obtain file for dates
  
  #### Reading data of sensor from period of 06.01.17 #####
  # TODO change comment above
  sensorData = sensor_data 
  
  #### Calculating  the manufacture Formula for sensors
  
  sensorData$CO_est <- (((sensorData$CO_we-270)-(sensorData$CO_aux-340))/420)*1000
  sensorData$NO_est <- (((sensorData$NO_we-545)-(sensorData$NO_aux-510))/520)*1000
  sensorData$NO2_est <- (((sensorData$NO2_we-225)-(sensorData$NO2_aux-245))/309)*1000
  sensorData$O3_est <- (((sensorData$O3_we-260)-(sensorData$O3_aux-300))/298)*1000

  
  
  # get only relevant station
  sensorData = subset(sensorData, monitor == sensor)
  
  # remember new index of date column after we removed not needed columns
  sensorData = sensorData[,c(pollutant, "date")]
  
  # get only relevant dates
  # sensorData = subset(sensorData, date >= startdate
                     # &  date <= endDate)
  
  # Aggregate data  
  if(byDay) { # by day
    # Create aggregation column by day for comparison with Stations
    sensorData$date = as.POSIXct(as.character(sensorData$date - 28800, format="%Y-%m-%d"), format="%Y-%m-%d")
  } else { # by hour
    # Create aggregation column by hour for comparison with Stations
    sensorData$date = as.POSIXct(as.character(sensorData$date, format="%Y-%m-%d %H"), format="%Y-%m-%d %H") - 28800
  }
  aggregatedData = aggregate(x = sensorData, by = list(sensorData$date), FUN = mean, data=sensorData)
  sensorData = setNames(aggregatedData[, c(3,2)], c("Date", generatePollutantColumnName("Sensor", pollutant)))
  
  # stationData is not accessible after functions finishes
  return(sensorData) 
}

# Generates column name based on pollutant and source
# source - "Station" or "Sensor"
# pollutant - pollutant name
generatePollutantColumnName <- function(source, pollutant) {
  return(paste(source, pollutant, sep = "."))
}


# adds data analysis to excel file
# TODO update result definition
# Input: result - list of following items:
#                  mergedData
#                  summary of linear regression
#        sheetTitle
addOutputDataToExcel <- function(res, sheetTitle) {
  # create new workbook (file)
  Input_parameters<-c(res[1:8])
  data<-res$mergedData
  fit<-lm(formula = paste(colnames(data)[2], "~", colnames(data)[3], sep = ""), data=data)
  QQplot<-qqPlot(fit, main="QQ Plot")
  resultsData <-data.frame(Input_parameters, data, formatLinearRegressionSummary, QQplot)
  setwd("H:/My data")
  write.xlsx(resultsData, "Calib.Analysis")
  
  p = plot(res$mergedData[,2], 
           ylim = c(min(min(res$mergedData[,2], na.rm = TRUE), min(res$mergedData[,3], na.rm = TRUE)),
                    max(max(res$mergedData[,2], na.rm = TRUE), max(res$mergedData[,3], na.rm = TRUE))),
           type="l", col="red")
  lines(res$mergedData[,3], col = "blue")
  y.plot=current.graphics()
  xl[g3] = y.plot
#TODO: to add vif when it will be more than one covariate in regression
  #vif(fit)
  
  
  
  
  xl.workbook.add()
  # add new sheet
  xl.sheet.add(sheetTitle)
  # take range cells
  xls=xl.get.excel()
  # point specific location on sheet
  rng=xls[["Activesheet"]]$Cells(1,1)
  # Output input parameters (1:8) - look at "result"
  nxt=xl.write(result[1:8], rng)
  
  # Output merged data (nxt +1 => where it stopped +1 row)
  rng=rng$Offset(nxt[1]+1,0)
  nxt=xl.write(result$mergedData, rng)
  
  # put plot into Excel (ignore NA when looking at ylim)(2 =station)(3=sensors)
  p = plot(result$mergedData[,2], 
           ylim = c(min(min(result$mergedData[,2], na.rm = TRUE), min(result$mergedData[,3], na.rm = TRUE)),
                    max(max(result$mergedData[,2], na.rm = TRUE), max(result$mergedData[,3], na.rm = TRUE))),
           type="l", col="red")
  lines(result$mergedData[,3], col = "blue")
  y.plot=current.graphics()
  xl[g3] = y.plot
  
  summary = result$summary
  #  nxt=xl.write(formatLinearRegressionSummary(summary), rng, row.names=TRUE, col.names=TRUE)
  
  
  ## TODO get name from arguments
  #xl.workbook.save("H:/My data/tmp/outputDataTest1.xlsx")
  #xl.workbook.close()
}

# test function for output linear regression to csv file
formatLinearRegressionSummary <- function(summary){ 
  res<-c(paste(as.character(summary$call),collapse=" "), 
         summary$coefficients[1], 
         summary$coefficients[2], 
         summary$coefficients[2,2], 
         summary$r.squared, 
         summary$adj.r.squared, 
         summary$fstatistic) 
  # names(res)<-c("call","intercept","slope","slope.SE","r.squared","Adj. r.squared", 
  #               "F-statistic","numdf","dendf") 
  return(res)
}  
