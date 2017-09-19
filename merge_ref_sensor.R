################################
#merge sensor data
#Author: Elena AUstin
#Date: June 30th, 2017
#Version: 1.0
#Name: merge_ref_sensor.R
##############################

library(pacman)

p_load("data.table", "tidyr", "ggplot2","lubridate","ggthemes", "mgcv","lme4")

#load("reference_data.RData")

#Download Kairos Data files

source("X://Production_code//getMESA_data.R")

### Load Data ###
source('X://Production_code//config.R', encoding = 'UTF8')
#source('X://Production_code//getMESA_data_Kairos.r', encoding = 'UTF8') # get sensor data from Kairos
sensor.loc <- fread("X://Data//Sensor Location table_editedElena.csv")

loc_names <- suppressWarnings(suppressMessages(sensor_data <- datafeed_get_files()))

##get all data files
#sensor_data <- datafeed_download_file(loc_names)
#sensor_data <- data.table(sensor_data)

#Get data collocated at Beacon Hill
sensor_data <- get_colo("BeaconHill")
dataK <- copy(data.table(sensor_data))

#get reference data values
ref_data<- data.table(read.csv("X://Data//Calibration//formatted_ref_data//manually_formatted//BH_20170616.csv"))
#correct timezone
ref_data[,datetime_local := as.POSIXct(datetime, format="%m/%d/%Y %H:%M", tz="America/Los_Angeles")]
ref_data[,dateUTC := as.POSIXct(format(datetime_local, tz="UTC"), tz="UTC")]

#dataK[,c("i.sensor.2","i.sensor.1")]=NULL

#Format date/time 
dataK[,dateUTC:=as.POSIXct(format(date, tz="UTC"),tz="UTC")]
dataK[,start_timeUTC:=as.POSIXct(dateUTC)]
dataK[,end_timeUTC:=as.POSIXct(dateUTC)]
dataK[, datehourUTC := as.POSIXct(format(dateUTC, "%Y-%m-%d %H:00:00"), tz="UTC")]

#formatdates sensor.loc
#find AM/PM entries and parse datetime into UTC
# sensor.loc[grep("PM", start_time), start_timeUTC := format(as.POSIXct(start_time, format="%m/%d/%Y %I:%M%p" ,tz=tz_r),tz="UTC"), by=tz_r ]
# sensor.loc[grep("AM", start_time), start_timeUTC := format(as.POSIXct(start_time, format="%m/%d/%Y %I:%M%p" ,tz=tz_r),tz="UTC"), by=tz_r ]
# sensor.loc[c(grep("PM", start_time, invert=T),
#             grep("AM", start_time, invert=T)), 
#            start_timeUTC := format(as.POSIXct(start_time, format="%m/%d/%Y %H:%M" ,tz=tz_r),tz="UTC"), by=tz_r ]

sensor.loc[, start_timeUTC := format(as.POSIXct(start_time,tz=tz_r),tz="UTC"), by=tz_r ]

#replace empty end times
sensor.loc[(end_time) %in% "", end_time := "2888-01-01 10:10:10"]

# sensor.loc[grep("PM", end_time), end_timeUTC := format(as.POSIXct(end_time, format="%m/%d/%Y %I:%M%p" ,tz=tz_r),tz="UTC"), by=tz_r ]
# sensor.loc[grep("AM", end_time), end_timeUTC := format(as.POSIXct(end_time, format="%m/%d/%Y %I:%M%p" ,tz=tz_r),tz="UTC"), by=tz_r ]
# sensor.loc[c(grep("PM", end_time, invert=T),
#              grep("AM", end_time, invert=T)), 
#            end_timeUTC := format(as.POSIXct(end_time, format="%m/%d/%Y %H:%M" ,tz=tz_r),tz="UTC"), by=tz_r ]

sensor.loc[,end_timeUTC := format(as.POSIXct(end_time,tz=tz_r),tz="UTC"), by=tz_r ]

sensor.loc[,start_timeUTC := as.POSIXct(start_timeUTC)]
sensor.loc[,end_timeUTC := as.POSIXct(end_timeUTC)]

#use only beacon hill locations
sensor.loc = sensor.loc[sitename=="BeaconHill",]

#find start_time errors:

sensor.loc[start_timeUTC>end_timeUTC,]

#merge with location file
setkey(dataK, monitor, start_timeUTC, end_timeUTC)
setkey(sensor.loc, start_timeUTC, end_timeUTC)

dataK_sub <-dataK[monitor%in%unique(sensor.loc$monitor),]

dataK_sub <- copy(foverlaps(sensor.loc, dataK[!is.na(dataK$date),], by.y=c("monitor","start_timeUTC","end_timeUTC"),
                by.x=c("monitor","start_timeUTC","end_timeUTC"), nomatch=0))


dataK_sub[,c("i.start_time", "i.end_time", "start_time","end_time","sensor", "start_timeUTC","end_timeUTC")]<-NULL

#get hourly data for sensors

value_columns = c("Plantower1_pm0_3_count", "Plantower1_pm0_5_count", "Plantower1_pm1_count",  
                  "Plantower1_pm2_5_count", "Plantower1_pm5_count",   "Plantower1_pm10_count",  "Plantower1_pm1_mass",    "Plantower1_pm2_5_mass", 
                  "Plantower1_pm10_mass",   "Plantower2_pm0_3_count", "Plantower2_pm0_5_count", "Plantower2_pm1_count",   "Plantower2_pm2_5_count",
                  "Plantower2_pm5_count",   "Plantower2_pm10_count",  "Plantower2_pm1_mass",    "Plantower2_pm2_5_mass",  "Plantower2_pm10_mass",  
                  "Temp_val",               "RH_val",                 "CO_aux",                 "CO_we",                  "NO_aux",                
                  "NO_we",                  "NO2_aux",                "NO2_we",                 "O3_aux",                 "O3_we",                 
                  "S1_val",                 "S2_val",                 "CO_sensor",              "NO_sensor",              "NO2_sensor",            
                  "O3_sensor")

#summarize hourly by hour
dataK_sub_hourly = dataK_sub[, lapply(.SD, FUN = function(x) {
  ifelse(length(x)>5, mean(x, na.rm=T), -9999)}), .SDcols=value_columns , 
  by = c("datehourUTC", "monitor")]

#count -9999 values
dataK_sub_hourly = dataK_sub_hourly[,lapply(.SD, FUN = function(x) {
  ifelse(x==-9999, NA, x)})]

dataK_sub_hourly[,datehourUTC := as.POSIXct(datehourUTC, origin= "1970-01-01")]


#merge beacon_hill sensors with beacon hill data

setkey(dataK_sub_hourly, datehourUTC)
setkey(ref_data, dateUTC)

merged_ref_sensor<- dataK_sub_hourly[ref_data]

merged_ref_sensor <- merged_ref_sensor[datehourUTC>=min(dataK_sub_hourly$datehourUTC),]

#remove empty data

P = ggplot(merged_ref_sensor, aes(datetime_local, Plantower1_pm2_5_mass, color="mycol1"))+
  geom_point(size=0.5, alpha=0.5 ) +
  geom_point(aes(datetime_local, Plantower2_pm2_5_mass,color = "mycol1"), size=0.5, alpha=0.5 )+
  geom_line(aes(datetime_local, BeaconHill_Pm25TeomFEM_ugm3 , color="mycol2"))  +
  scale_color_manual(name="PM2.5 Mass", values=c(mycol1="black",mycol2="red"), labels=c("Community Monitors", "Beacon Hill Regulatory")) +
  xlab("") + ylab (expression(paste(
    PM[2.5], " Concentration ( ",
    mu, "g/", m^3,
    " )", sep="")))
 
  P + theme_igray(14) + theme(legend.position="bottom")
  
merged_ref_sensor[,perc_error_plant_mass2.5 := (Plantower1_pm2_5_mass - Plantower2_pm2_5_mass)/rowMeans(.SD)*100, 
                  .SDcols=c("Plantower1_pm2_5_mass", "Plantower2_pm2_5_mass")]

hist(merged_ref_sensor$perc_error_plant_mass2.5, xlab=("Percent Error (%) Mass 2.5 Plantower"),main="")

merged_ref_sensor[,abs_dif_plant_mass2.5 := abs(Plantower1_pm2_5_mass - Plantower2_pm2_5_mass)]

hist(merged_ref_sensor$abs_dif_plant_mass2.5, xlab=("Absolute Difference Mass 2.5 Plantower"),main="")


merged_ref_sensor[,plantower_pm2_5_mass := rowMeans(.SD), .SDcols=c("Plantower1_pm2_5_mass", "Plantower2_pm2_5_mass")]

#plot sensor values vs Reference values

ggplot(merged_ref_sensor, aes(BeaconHill_Pm25TeomFEM_ugm3, plantower_pm2_5_mass)) + geom_point() +theme_igray(14) +
  xlab(expression(paste(
    PM[2.5], " Beacon Hill ( ",
    mu, "g/", m^3,
    " )"))) +
      ylab(expression(paste(
        PM[2.5], "Mean Plantower ( ",
        mu, "g/", m^3,
        " )" )))
        
merged_ref_sensor[, cor(BeaconHill_Pm25TeomFEM_ugm3, Plantower1_pm2_5_mass, use= "pairwise.complete.obs"), by="monitor"]

ggplot(merged_ref_sensor[monitor%in%"ACT15"], aes(BeaconHill_Pm25TeomFEM_ugm3, plantower_pm2_5_mass)) + geom_point() +theme_igray(14) +
  xlab(expression(paste(
    PM[2.5], " Beacon Hill ( ",
    mu, "g/", m^3,
    " )"))) +
  ylab(expression(paste(
    PM[2.5], "Mean Plantower ( ",
    mu, "g/", m^3,
    " )" )))

ggplot(merged_ref_sensor[monitor%in%"ACT16"], aes(BeaconHill_Pm25TeomFEM_ugm3, plantower_pm2_5_mass)) + geom_point() +theme_igray(14) +
  xlab(expression(paste(
    PM[2.5], " Beacon Hill ( ",
    mu, "g/", m^3,
    " )"))) +
  ylab(expression(paste(
    PM[2.5], "Mean Plantower ( ",
    mu, "g/", m^3,
    " )" )))

lmfunction <- function(x, y) {
  lm(y~x)
}

merged_ref_sensor[,logrefPM25 := log(BeaconHill_Pm25TeomFEM_ugm3)]
merged_ref_sensor[!is.finite(logrefPM25),logrefPM25 := NA]

merged_ref_sensor[,logSensorPM25 := log(plantower_pm2_5_mass)]
merged_ref_sensor[!is.finite(logSensorPM25),logSensorPM25 := NA]

lme.PM25 <- lmer(BeaconHill_Pm25TeomFEM_ugm3 ~  plantower_pm2_5_mass + S1_val +
                   (1 + plantower_pm2_5_mass|monitor), data=merged_ref_sensor,na.action=na.exclude)

lme.PM25 <- lmer(BeaconHill_Pm25TeomFEM_ugm3 ~  plantower_pm2_5_mass +
                   (1 + plantower_pm2_5_mass|monitor), data=merged_ref_sensor,na.action=na.exclude)

plot((merged_ref_sensor$BeaconHill_Pm25TeomFEM_ugm3), (predict(lme.PM25)))
abline(0,1,col="red")

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
