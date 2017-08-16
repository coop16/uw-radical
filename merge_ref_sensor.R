################################
#merge sensor data
#Author: Elena AUstin
#Date: June 30th, 2017
#Version: 1.0
#Name: merge_ref_sensor.R
##############################
library(data.table)
library(tidyr)
library(ggplot2)
library(lubridate)

setwd("X://Data//calibration")

load("reference_data.RData")

#Download Kairos Data files

source("getMESA_data.R")

### Load Data ###
source('X://Production_code//config.R', encoding = 'UTF8')
source('X://Production_code//getMESA_data_Kairos.r', encoding = 'UTF8') # get sensor data from Kairos
# suppressWarnings(suppressMessages(sensor_data <- getMESA_data(start.date = DATE.RANGE[1], stop.date = DATE.RANGE[2])))

# sensor_data <- getMESA_data(start.date = as.POSIXct("2017-06-30"), stop.date = as.POSIXct("2017-07-01"))
sensor_data <- get_colo("NYBG")
dataK <- data.table(sensor_data)
# dataK <- data.table(sensor_data$wide)

dataK[,c("i.sensor.2","i.sensor.1")]=NULL
dataK[,dateUTC:=as.POSIXct(format(date, tz="UTC"),tz="UTC")]

dataK[,start_time:=as.POSIXct(dateUTC)]
dataK[,end_time:=as.POSIXct(dateUTC)]


#merge with location file
setkey(dataK, monitor, start_time, end_time)

dataK<-dataK[monitor%in%unique(sensor.loc$monitor),]

dataK<-foverlaps(sensor.loc, dataK[!is.na(dataK$date),], by.y=c("monitor","start_time","end_time"),
                by.x=c("monitor","start_timeUTC","end_timeUTC"), nomatch=0)


dataK[,c("i.start_time", "i.end_time", "start_time","end_time","sensor")]<-NULL

#take the mean of duplicated timepoints CHECK THIS
#end.result<-end.result[,lapply(.SD, mean, na.rm=T), by=c("dateUTC","sitename","units","pollutant","tz_r")]

end.result_wide<-dcast(end.result, dateUTC+sitename~pollutant+units, value.var="concentration", 
                       fun.aggregate= function(x) mean(x, na.rm=T))

#create hourly files of the sensor data
#we can add completness criteria here!
dataK[,hours:=round_date(dateUTC,"1 hour")]
dataK[,c("i.sensor")]=NULL
dataK_hourly<-dataK[,lapply(.SD, mean, na.rm=T), by=c("hours","monitor","siteid","sitename","tz_r"),
                    .SDcols=names(dataK)[unlist(dataK[,lapply(.SD,is.numeric)])]]


attributes(dataK_hourly$hours)$tz="UTC"

#merge
dataK_hourly[,hours:=as.POSIXct(hours, tz="UTC")]
end.result_wide<-data.table(end.result_wide)
setkey(dataK_hourly, hours, sitename)
setkey(end.result_wide, dateUTC,sitename)

merged<-end.result_wide[dataK_hourly]
  
ggplot(merged[tags%in%c("Plantower1_pm2_5_mass")], aes(x=Pm25Neph_ugm3, y=value))+
  geom_point()


ggplot(merged) + geom_point(aes(x=Pm25Neph_ugm3, y = Plantower2_pm2_5_mass))
