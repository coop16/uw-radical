library(gdata)
source("Q:\\eac_database\\code\\AmandaFunctions.R")
current.wd <- getwd()

setwd("X:\\Data\\calibration\\NYC_reference_data\\")

sitenames <- R.scan(list.files()[3:6],"_",1)
filelist  <- list.files()[3:6]

#
# Site -> Location Match-up
# -------------------------
siteloc <- read.xls("X:\\Data\\SensorLocationTable.xlsx")
siteloc$start_time <- as.POSIXct(strptime( gsub("0.00","0",as.character(siteloc$start_time)), format="%Y-%m-%d %H:%M", tz="EST"))
siteloc$end_time   <- as.POSIXct(strptime( gsub("0.00","0",as.character(siteloc$end_time)), format="%Y-%m-%d %H:%M", tz="EST"))

#sitename <- list()
#rawdata <- list()
#column_names <- list()
#temp <- c("sitename" = sitename, "rawdata" = rawdata, "column_names" = column_names)


#result <- list()
#temp   <- list()
#clean.data <- as.data.frame(matrix(NA, nrow=0, ncol = 5))
#names(clean.data) <- c("datetime", "O3", "NO2", "CO", "PM25", "NO")
#for (i in 1:length(filelist)){
#  temp <- list("sitename" = sitenames[i], 
#                 "rawdata"  = as.data.frame(read.xls(filelist[i], skip = 4, header = FALSE)),
#                  "column_names" = read.xls(filelist[i], header = FALSE)[2,] )
#  temp$rawdata$datetime <- strptime( as.character(temp$rawdata$V1), format="%m/%d/%Y %I:%M %p", tz="EST")
#  temp2 <- as.data.frame(matrix(-99.99, nrow=dim(temp$rawdata)[1], ncol=4))
#  names(temp2) <- names(clean.data)[-1]
#  data.miss <- names(temp2)[!(names(temp2) %in% names(temp$rawdata))]
#  temp$rawdata[,data.miss] <- temp2[data.miss]
#  result[[i]] <- temp
#  clean.data <- rbind.data.frame(clean.data, temp$rawdata[,c("datetime","O3","NO2","CO","PM25", "NO")])
#}

clean.data <- as.data.frame(matrix(NA, nrow=0, ncol = 7))
names(clean.data) <- c("datetime", "sitename", "O3", "NO2", "CO", "PM25", "NO")
clean.data$datetime <- as.Date(clean.data$datetime)
clean.data$O3 <- as.numeric(clean.data$O3)
clean.data$PM25 <- as.numeric(clean.data$PM25)
clean.data$NO2 <- as.numeric(clean.data$NO2)
clean.data$CO <- as.numeric(clean.data$CO)
clean.data$NO <- as.numeric(clean.data$NO)
clean.data$sitename <- as.character(clean.data$sitename)
for (i in 1:length(filelist)){
  temp <- read.xls(filelist[i], stringsAsFactors = FALSE)
  temp$datetime <- as.POSIXct(strptime( as.character(temp$date1), format="%m/%d/%Y %I:%M %p", tz="EST"))
  temp$sitename <- rep(sitenames[i], length(temp$datetime))
  clean.data <- rbind.data.frame(clean.data, temp[,c("datetime","sitename","O3","NO2","CO","PM25", "NO")])
}
 clean.data$O3 <- as.numeric(clean.data$O3)
 clean.data$PM25 <- as.numeric(clean.data$PM25)
 clean.data$NO2 <- as.numeric(clean.data$NO2)
 clean.data$CO <- as.numeric(clean.data$CO)
 clean.data$NO <- as.numeric(clean.data$NO)
 summary(clean.data)

setwd(current.wd)
