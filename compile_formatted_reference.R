
# ============================================ #
# Libraries 
# ============================================ #

library(data.table)

# ============================================ #
# Functions 
# ============================================ #

R.scan <- function(char.vec, splitter, component){
  if (is.na(char.vec)){temp <- NA}
  else{
    temp <- rep(NA, length(char.vec))
    for (i in 1:length(char.vec)){
      temp[i] <- strsplit(char.vec[i],splitter)[[1]][component]
    }
  }
  temp
}

get.format <- function(x){
  n.x <- length(x)
  hour <- strtoi(R.scan(x, ":",1))
  mins <- R.scan(x, ":",2)
  secs <- R.scan(x, ":",3)
  hour.bool <- sum(!is.na(hour)) > 0
  min.bool <- sum(!is.na(mins)) > 0
  sec.bool <- sum(!is.na(secs)) > 0
  hr.max <- max(hour, na.rm=TRUE)
  am.pm.bool <- sum(grepl("AM|PM|A.M.|P.M.", x)) > 0
  if (hr.max > 12) { f <- "%H"}
  else { f <- "%I" }
  if (min.bool){
    f <- paste0(f, ":%M")
    if (sec.bool){
      f <- paste0(f, ":%S")
  } }
  if (am.pm.bool){ f <- paste(f, "%p") }
  paste("%m/%d/%Y", f)
}

# ============================================ #
# Load data
# ============================================ #

timezone_names <- c("US/Pacific"="America/Los_Angeles",
                    "US/Central"="America/Chicago",
                    "US/Eastern"="America/New_York")


ref.files <- list.files("X:\\Data\\calibration\\formatted_ref_data\\manually_formatted")

# ============================================ #
# Read & aggregate reference data
# ============================================ #

result <- list()
k <- 1
for (i in ref.files){
  temp <- read.csv(paste0("X:\\Data\\calibration\\formatted_ref_data\\manually_formatted\\", i), stringsAsFactors = FALSE)
  if (i == "STP_gas_201705.csv"){ 1 }
  else{
    temp2 <- temp
    temp2$date  <- as.Date(as.character(temp2$date), "%m/%d/%Y")
    temp2$time  <- toupper(as.character(temp2$time))
    temp2$time  <- strptime(paste(temp$date, temp$time), format = get.format(temp2$time))
    time.format <- median(difftime(temp2$time[-1], temp2$time[-length(temp2$time)], units = "mins"), na.rm = TRUE)
    temp2$interval <- rep(time.format, length(temp2$time))
    colnames1 <- R.scan(as.character(names(temp)),"_",1)
    colnames2 <- R.scan(as.character(names(temp)),"_",2) 
    colnames3 <- R.scan(as.character(names(temp)),"_",3)
    col.to.do <- names(temp)[colnames2 %in% c("CO","NO","NO2","NOx","O3","PM10","PM25",
                                              "Pm25uv","LightScatEc","Pm25Neph","Pm25TeomFEM",
                                              "SO2","NOy-NO","TPM25-1405","VisualRange","PM25raw","temp","Temp",
                                              "Pm25MetOneBam","PAH","Neph","Langan2-CO","Langan1-CO","CO2","rh","BC")]
    for (j in col.to.do){
      new.result <- cbind.data.frame(temp2$date, 
                                   temp2$time, 
                                   temp2$interval, 
                                   temp2[,j], 
                                   rep(colnames2[names(temp) == j], length(temp2[, j])),
                                   rep(colnames3[names(temp) == j], length(temp2[, j])),
                                   rep(colnames1[names(temp) == j], length(temp2[, j])))
      names(new.result) <- c("date","time","interval","concentration","pollutant","units","location")
      new.result[,c("pollutant")] <- as.character(new.result[,c("pollutant")])
      new.result[,c("units")] <- as.character(new.result[,c("units")])
      new.result[,c("sitename")] <- as.character(new.result[,c("location")])
      new.result[,c("concentration")] <- as.numeric(new.result[,c("concentration")])
      result[[k]] <- new.result
      k <- k + 1 
    } 
} }
end.result <- do.call(rbind.data.frame, result)

#wide format end.result
end.result<-data.table(end.result)

#correct tz display
lookuptz<-unique(sensor.loc[,tz_r,by=sitename])
lookuptz[,sitename:=as.character(sitename)]
setkey(lookuptz, sitename)
setkey(end.result, location)

end.result<-lookuptz[end.result]
#remove empty time points 
#Amanda: there are missing time values in this data
end.result<-end.result[!is.na(time)]
attributes(end.result$time)$tzone<-"UTC"
end.result[tz_r%in%"America/New_York",dateUTC:= time+5*60*60]
end.result[tz_r%in%"America/Chicago",dateUTC:= time+6*60*60]
end.result[tz_r%in%"America/Los_Angeles",dateUTC:= time+8*60*60]
#remove missing rows
end.result<-end.result[!is.na(concentration)]

# Check location names:
# ---------------------
unique(end.result$location)[!(unique(end.result$location) %in% unique(sensor.loc$sitename))]

# Location x Pollutant:
# ---------------------
table(end.result[,c("sitename","pollutant")])

# Pollutant x Units
# -----------------
table(end.result[,c("pollutant","units")])

# ============================================ #
# Read & format monitor-reference lookup table
# ============================================ #

sensor.loc <- read.csv("X:\\Data\\SensorLocationTable.csv")

# Clean up sensor.loc table

# Re-format start and end times
sensor.loc$end_time<-as.POSIXct(as.character(sensor.loc$end_time),
                                format="%m/%d/%Y %H:%M")
sensor.loc$start_time<-as.POSIXct(as.character(sensor.loc$start_time),
                                format="%m/%d/%Y %H:%M")


sensor.loc<-data.table(sensor.loc, stringsAsFactors=T)


#correct R names of time zones
sensor.loc[,tz_r:=timezone_names[as.character(tz_r)]]

#temp time variable
sensor.loc[!is.finite(end_time)]$end_time<-as.POSIXct("2111-06-1 12:12")

sensor.loc[,start_timeUTC:= as.POSIXct(unlist(lapply(1:nrow(sensor.loc),
                                FUN = function(x)
                                {temp=as.POSIXct(start_time[x], 
                                                 format="%m/%d/%Y %H:%M", 
                                                 tz=tz_r[x])
                                 format(temp, tz="UTC") }
                                )))]

sensor.loc[,end_timeUTC:= as.POSIXct(unlist(lapply(1:nrow(sensor.loc),
                                          FUN = function(x)
                                          {temp=as.POSIXct(end_time[x], 
                                                           format="%m/%d/%Y %H:%M", 
                                                           tz=tz_r[x])
                                          format(temp, tz="UTC") }
                                          )))]
attributes(sensor.loc$end_timeUTC)$tzone="UTC"
attributes(sensor.loc$start_timeUTC)$tzone="UTC"

setkey(sensor.loc, monitor, start_timeUTC, end_timeUTC)
sensor.loc[,c("sensor","start_time","end_time","tz")]=NULL

save(end.result,sensor.loc, file = "X:\\Data\\calibration\\reference_data.RData")


