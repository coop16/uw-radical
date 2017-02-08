library(data.table)

# get raw colocation data
setwd('/pacific/rad')

names<-list.files(path="data/rawdata", full.names = T)
names_cal<-names[grep("calibration",names)]

calibration_list<-lapply(names_cal, FUN = function(x) {
  temp<-read.csv(x)
  temp<-data.table(temp)
  temp[,sensor := strsplit(x,"_")[[1]][1]]
  temp[,Unix.Time:=as.numeric(as.character(Unix.Time))]
  temp<-temp[!is.na(Unix.Time)]
  temp[,datetimeUTC:=as.POSIXct(Julian.Time, format="%m/%d/%y %H:%M:%S",tz="UTC")]
  temp
})



calibration<-rbindlist(calibration_list)

write.csv(calibration, "calibration_data.csv", row.names=F)

#this code is for time averaging
# sensors <- data.table(read.csv("allboxes_colocation1.csv"))
#
# mean_function <- function (x) {
#   x<-as.numeric(as.character(x))
#   if (sum(!is.na(x))>=40){
#     mean(x,na.rm=T)} else -999}
#
# sensors[,datetime:=as.POSIXct(datetime,format="%Y-%m-%d %H:%M:%S")]
# sensors.hour<-sensors[,lapply(.SD, mean_function) ,
#                       by=list(format(datetime,"%Y-%m-%d %H"),sensor)]
# #replace missing values with NA
# sensors.hour[sensors.hour==-999]=NA
#
# sensors.hour[,datetime:=as.POSIXct(format, format="%Y-%m-%d %H")]
#
# setkey(sensors.hour,datetime)
# setkey(ARBw,datetime)
#
# all<-sensors.hour[ARBw]
# all<-all[!is.na(sensor)]
#
# write.csv(all,"hourly_colocation1_all.csv",row.names=F)

#this code is for applying the factory calibrations to the gas sensor readings
#all sensor data (time resolved)
#sensors<-data.table(read.csv("allboxes_colocation1.csv"))

# calvals_CO<-fread("SY_GasSensorList_CO.csv")
# calvals_NO<-fread("SY_GasSensorList_NO.csv")
# calvals_NO2<-fread("SY_GasSensorList_NO2.csv")
# calvals_O3<-fread("SY_GasSensorList_O3.csv")
#
# setkey(all,sensor)
# setkey(calvals_CO,sensor)
# setkey(calvals_NO,sensor)
# setkey(calvals_NO2,sensor)
# setkey(calvals_O3,sensor)
#
# all<-calvals_CO[all]
# all<-calvals_NO[all]
# all<-calvals_NO2[all]
# all<-calvals_O3[all]
#
# #adjustment based on factory cal
# all[,CO_sensor := CO_we-CO_aux]
# all[,NO_sensor := NO_we-NO_aux]
# all[,NO2_sensor := NO2_we-NO2_aux]
# all[,O3_sensor := O3_we-O3_aux]
# all[,pm25_sensor := Dylos_Bin_1 - Dylos_Bin_3]
# all[,pm10_sensor := Dylos_Bin_1 - Dylos_Bin_4]

#
# write.csv(all,"hourly_colocation1_all.csv",row.names=F)
