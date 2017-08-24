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

source("X://Production_code//getMESA_data.R")

### Load Data ###
source('X://Production_code//config.R', encoding = 'UTF8')
#source('X://Production_code//getMESA_data_Kairos.r', encoding = 'UTF8') # get sensor data from Kairos
sensor.loc <- fread("X://Data//Sensor Location table.csv")

loc_names <- suppressWarnings(suppressMessages(sensor_data <- datafeed_get_files()))

##get all data files
#sensor_data <- datafeed_download_file(loc_names)
#sensor_data <- data.table(sensor_data)

#Get data collocated at Beacon Hill
sensor_data <- get_colo("BeaconHill")
dataK <- data.table(sensor_data)

#dataK[,c("i.sensor.2","i.sensor.1")]=NULL

#Format date/time 
dataK[,dateUTC:=as.POSIXct(format(date, tz="UTC"),tz="UTC")]
dataK[,start_time:=as.POSIXct(dateUTC)]
dataK[,end_time:=as.POSIXct(dateUTC)]

#formatdates sensor.loc
#find AM/PM entries and parse datetime into UTC
sensor.loc[grep("PM", start_time), start_timeUTC := format(as.POSIXct(start_time, format="%m/%d/%Y %I:%M%p" ,tz=tz_r),tz="UTC"), by=tz_r ]
sensor.loc[grep("AM", start_time), start_timeUTC := format(as.POSIXct(start_time, format="%m/%d/%Y %I:%M%p" ,tz=tz_r),tz="UTC"), by=tz_r ]
sensor.loc[c(grep("PM", start_time, invert=T),
            grep("AM", start_time, invert=T)), 
           start_timeUTC := format(as.POSIXct(start_time, format="%m/%d/%Y %H:%M" ,tz=tz_r),tz="UTC"), by=tz_r ]

#replace empty end times
sensor.loc[(end_time) %in% "", end_time := "01/01/2888 10:10:10"]

sensor.loc[grep("PM", end_time), end_timeUTC := format(as.POSIXct(end_time, format="%m/%d/%Y %I:%M%p" ,tz=tz_r),tz="UTC"), by=tz_r ]
sensor.loc[grep("AM", end_time), end_timeUTC := format(as.POSIXct(end_time, format="%m/%d/%Y %I:%M%p" ,tz=tz_r),tz="UTC"), by=tz_r ]
sensor.loc[c(grep("PM", end_time, invert=T),
             grep("AM", end_time, invert=T)), 
           end_timeUTC := format(as.POSIXct(end_time, format="%m/%d/%Y %H:%M" ,tz=tz_r),tz="UTC"), by=tz_r ]
sensor.loc[,start_timeUTC := as.POSIXct(start_timeUTC)]
sensor.loc[,end_timeUTC := as.POSIXct(end_timeUTC)]

#merge with location file
setkey(dataK, monitor, start_time, end_time)
setkey(sensor.loc, start_timeUTC, end_timeUTC)

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
