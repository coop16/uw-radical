###########################################
# getMESA_data.R
#
# Function (getMESA_data) for getting data from Kairos
# specify EITHER nday: number of days back to get data (starting with today)
# OR date_start and date_end to get a date range
# Returns a list with 3 positions.
# $parsed is the raw kairos data
# $long is the data in long format
# $wide is the data in wide format
#
#
#
# Elena Austin
# 9/6/2016
###########################################

setwd("H:/projects/uw-radical")
source('config.r') #load api configuration

#check if necessary packages are installed
unavailable <-
  setdiff(c("curl", "jsonlite","data.table","reshape2", "grid", "gridExtra", "ggplot2", "stringr"),
          rownames(installed.packages()))
#install missing packages
if(length(unavailable)>0)
  install.packages(unavailable,repos="https://cran.fhcrc.org/")

#load required libraries
library(curl)
library(jsonlite)
library(data.table)
library(reshape2)
library(ggplot2)
library(grid)
library(gridExtra)
library(stringr)


#Download data
h <- new_handle()

prep_query <- function (u_name, date_start, date_end) {
  #Prepare a data structure to send in JSON format
  data1 = paste0("{\"start_absolute\": ",date_start,", \"end_absolute\": ",date_end,", \"metrics\" : [",
                "{\"name\": \"", u_name,"\", \"tags\": {\"Plantower1\":\"pm0_3_count\"}, \"aggregators\": [{\"name\": \"avg\", \"align_sampling\": \"true\", \"sampling\": {\"value\": 1, \"unit\": \"seconds\"}}]}, ",
                "{\"name\": \"", u_name,"\", \"tags\": {\"Plantower1\":\"pm0_5_count\"}, \"aggregators\": [{\"name\": \"avg\", \"align_sampling\": \"true\", \"sampling\": {\"value\": 1, \"unit\": \"seconds\"}}]}, ",
                "{\"name\": \"", u_name,"\", \"tags\": {\"Plantower1\":\"pm1_mass\"}, \"aggregators\": [{\"name\": \"avg\", \"align_sampling\": \"true\", \"sampling\": {\"value\": 1, \"unit\": \"seconds\"}}]}, ",
                "{\"name\": \"", u_name,"\", \"tags\": {\"Plantower1\":\"pm2_5_mass\"}, \"aggregators\": [{\"name\": \"avg\", \"align_sampling\": \"true\", \"sampling\": {\"value\": 1, \"unit\": \"seconds\"}}]}, ",
                "{\"name\": \"", u_name,"\", \"tags\": {\"Plantower1\":\"pm10_mass\"}, \"aggregators\": [{\"name\": \"avg\", \"align_sampling\": \"true\", \"sampling\": {\"value\": 1, \"unit\": \"seconds\"}}]}, ",
                "{\"name\": \"", u_name,"\", \"tags\": {\"Plantower1\":\"pm10_count\"}, \"aggregators\": [{\"name\": \"avg\", \"align_sampling\": \"true\", \"sampling\": {\"value\": 1, \"unit\": \"seconds\"}}]}, ",
                "{\"name\": \"", u_name,"\", \"tags\": {\"Plantower1\":\"pm1_count\"}, \"aggregators\": [{\"name\": \"avg\", \"align_sampling\": \"true\", \"sampling\": {\"value\": 1, \"unit\": \"seconds\"}}]}, ",
                "{\"name\": \"", u_name,"\", \"tags\": {\"Plantower2\":\"pm0_3_count\"}, \"aggregators\": [{\"name\": \"avg\", \"align_sampling\": \"true\", \"sampling\": {\"value\": 1, \"unit\": \"seconds\"}}]}, ",
                "{\"name\": \"", u_name,"\", \"tags\": {\"Plantower2\":\"pm0_5_count\"}, \"aggregators\": [{\"name\": \"avg\", \"align_sampling\": \"true\", \"sampling\": {\"value\": 1, \"unit\": \"seconds\"}}]}, ",
                "{\"name\": \"", u_name,"\", \"tags\": {\"Plantower2\":\"pm1_mass\"}, \"aggregators\": [{\"name\": \"avg\", \"align_sampling\": \"true\", \"sampling\": {\"value\": 1, \"unit\": \"seconds\"}}]}, ",
                "{\"name\": \"", u_name,"\", \"tags\": {\"Plantower2\":\"pm2_5_mass\"}, \"aggregators\": [{\"name\": \"avg\", \"align_sampling\": \"true\", \"sampling\": {\"value\": 1, \"unit\": \"seconds\"}}]}, ",
                "{\"name\": \"", u_name,"\", \"tags\": {\"Plantower2\":\"pm10_mass\"}, \"aggregators\": [{\"name\": \"avg\", \"align_sampling\": \"true\", \"sampling\": {\"value\": 1, \"unit\": \"seconds\"}}]}, ",
                "{\"name\": \"", u_name,"\", \"tags\": {\"Plantower2\":\"pm10_count\"}, \"aggregators\": [{\"name\": \"avg\", \"align_sampling\": \"true\", \"sampling\": {\"value\": 1, \"unit\": \"seconds\"}}]}, ",
                "{\"name\": \"", u_name,"\", \"tags\": {\"Plantower2\":\"pm1_count\"}, \"aggregators\": [{\"name\": \"avg\", \"align_sampling\": \"true\", \"sampling\": {\"value\": 1, \"unit\": \"seconds\"}}]}, ",
                "{\"name\": \"", u_name,"\", \"tags\": {\"Temp\":\"val\"}, \"aggregators\": [{\"name\": \"avg\", \"align_sampling\": \"true\", \"sampling\": {\"value\": 1, \"unit\": \"seconds\"}}]}, ",
                "{\"name\": \"", u_name,"\", \"tags\": {\"RH\":\"val\"}, \"aggregators\": [{\"name\": \"avg\", \"align_sampling\": \"true\", \"sampling\": {\"value\": 1, \"unit\": \"seconds\"}}]}, ",
                "{\"name\": \"", u_name,"\", \"tags\": {\"CO\":\"aux\"}, \"aggregators\": [{\"name\": \"avg\", \"align_sampling\": \"true\", \"sampling\": {\"value\": 1, \"unit\": \"seconds\"}}]}, ",
                "{\"name\": \"", u_name,"\", \"tags\": {\"CO\":\"we\"}, \"aggregators\": [{\"name\": \"avg\", \"align_sampling\": \"true\", \"sampling\": {\"value\": 1, \"unit\": \"seconds\"}}]}, ",
                "{\"name\": \"", u_name,"\", \"tags\": {\"NO\":\"aux\"}, \"aggregators\": [{\"name\": \"avg\", \"align_sampling\": \"true\", \"sampling\": {\"value\": 1, \"unit\": \"seconds\"}}]}, ",
                "{\"name\": \"", u_name,"\", \"tags\": {\"NO\":\"we\"}, \"aggregators\": [{\"name\": \"avg\", \"align_sampling\": \"true\", \"sampling\": {\"value\": 1, \"unit\": \"seconds\"}}]}, ",
                "{\"name\": \"", u_name,"\", \"tags\": {\"NO2\":\"aux\"}, \"aggregators\": [{\"name\": \"avg\", \"align_sampling\": \"true\", \"sampling\": {\"value\": 1, \"unit\": \"seconds\"}}]}, ",
                "{\"name\": \"", u_name,"\", \"tags\": {\"NO2\":\"we\"}, \"aggregators\": [{\"name\": \"avg\", \"align_sampling\": \"true\", \"sampling\": {\"value\": 1, \"unit\": \"seconds\"}}]}, ",
                "{\"name\": \"", u_name,"\", \"tags\": {\"O3\":\"aux\"}, \"aggregators\": [{\"name\": \"avg\", \"align_sampling\": \"true\", \"sampling\": {\"value\": 1, \"unit\": \"seconds\"}}]}, ",
                "{\"name\": \"", u_name,"\", \"tags\": {\"O3\":\"we\"}, \"aggregators\": [{\"name\": \"avg\", \"align_sampling\": \"true\", \"sampling\": {\"value\": 1, \"unit\": \"seconds\"}}]}, ",
                "{\"name\": \"", u_name,"\", \"tags\": {\"S1\":\"val\"}, \"aggregators\": [{\"name\": \"avg\", \"align_sampling\": \"true\", \"sampling\": {\"value\": 1, \"unit\": \"seconds\"}}]}, ",
                "{\"name\": \"", u_name,"\", \"tags\": {\"S2\":\"val\"}, \"aggregators\": [{\"name\": \"avg\", \"align_sampling\": \"true\", \"sampling\": {\"value\": 1, \"unit\": \"seconds\"}}]}]}"
  )
  data1
}


get_metricnames <- function(re = NA) {
  handle_setopt(h, URL = paste0(kairos.api, "metricnames"))
  handle_setopt(h, CAINFO = kairos.cert)
  #Get password
  handle_setopt(h, USERPWD = kairos.pwd)
  #handle_setopt(h, POSTFIELDS = db_query)
  handle_setopt(h, POST = 0)
  req <- curl_fetch_memory(paste0(kairos.api, "metricnames"), handle = h)
  metrics <- fromJSON(rawToChar(req$content))$results
  if(!is.na(re)){
    return(str_subset(metrics, re))
  }else{
    return(metrics)
  }
}

send_query <- function(db_query) {
  handle_setopt(h, URL = paste0(kairos.api, "datapoints/query"))
  handle_setopt(h, CAINFO = kairos.cert)
  #Get password
  handle_setopt(h, USERPWD = kairos.pwd)
  handle_setopt(h, POSTFIELDS = db_query)
  handle_setopt(h, POST = 1)
  req <- curl_fetch_memory(paste0(kairos.api, "datapoints/query"), handle = h)
  req
}


getMESA_data <- function(nday=NA, start.date=NA, stop.date=NA) {
  community_units <- get_metricnames('MESA[0-9]+')

  if (!is.na(nday)){
    #yesterday midnight (multiply by 1000 to match python date representation
    start.date<- as.numeric(trunc(Sys.time()-3600*24*nday,"days"))*1000
    stop.date<-as.numeric(trunc(Sys.time(),"days")-1)*1000
  } else {
    start.date <- as.numeric(start.date)*1000
    stop.date <- as.numeric(stop.date)*1000
  }

  #get all dylos values (function)
  get_vals <- function (dyl, start=start.date, stop=stop.date){
    req <- send_query(prep_query(dyl,start,stop))
    json.res <- fromJSON(rawToChar(req$content))
  }

  #get json file from server
  json.list <- lapply(community_units, FUN = function (dyl) get_vals(dyl))


  #parse to r list
  parsed_data_list <- lapply(json.list, FUN = function(x) {lapply(x$queries$results,
                                                             FUN = function (y) {
                                                               temp<-as.data.table(y$values[[1]])
                                                               if (ncol(temp)==2){
                                                                 setnames(temp,c("date","value"))
                                                                 temp$monitor<-y$name
                                                                 #tag and tag name
                                                                  temp$tags<-
                                                                   paste0(names
                                                                          (unlist(y$tags)),"_",unlist(y$tags))
                                                               } else {temp =
                                                                  data.table(NULL)}
                                                               temp
                                                             })})

  # get sample size
  lapply(json.list, FUN = function(lx) {lx$queries$sample_size})

  #convert list to data.table
  parsed_data<-rbindlist(lapply(parsed_data_list,rbindlist))

  #wide format (fill in empty tag values)
  wide_parsed <-dcast.data.table(parsed_data, date+monitor~tags, value.var = "value")

  #convert to date format
  wide_parsed$date<-format(
    as.POSIXct(wide_parsed$date/1000,origin="1970-01-01 00:00:00"),
    "%Y-%m-%d %H:%M:%S")

  wide_parsed$date<-as.POSIXct(wide_parsed$date,
                               format="%Y-%m-%d %H:%M:%S",tz="America/Los_Angeles")

  calvals_CO<-fread("MESA_GasSensorList_CO.csv")
  calvals_NO<-fread("MESA_GasSensorList_NO.csv")
  calvals_NO2<-fread("MESA_GasSensorList_NO2.csv")
  calvals_O3<-fread("MESA_GasSensorList_O3.csv")

  setkey(wide_parsed,'monitor')
  setkey(calvals_CO,'Monitor Name')
  setkey(calvals_NO,'Monitor Name')
  setkey(calvals_NO2,'Monitor Name')
  setkey(calvals_O3,'Monitor Name')

  # merge in calibration values based on factory results
  wide_parsed<-calvals_CO[wide_parsed]
  wide_parsed<-calvals_NO[wide_parsed]
  wide_parsed<-calvals_NO2[wide_parsed]
  wide_parsed<-calvals_O3[wide_parsed]

  #adjustment based on factory cal
  wide_parsed[,CO_sensor := ((CO_we - WE_Zero_total_CO)
      - (CO_aux - AUX_Zero_total_CO))/ WE_sens_CO_mv.ppb]
  wide_parsed[,NO_sensor := ((NO_we - WE_Zero_total_NO)
      - (NO_aux - AUX_Zero_total_NO))/ WE_sens_NO_mv.ppb]
  wide_parsed[,NO2_sensor := ((NO2_we - WE_Zero_total_NO2)
      - (NO2_aux - AUX_Zero_total_NO2))/ WE_sens_NO2_mv.ppb]
  wide_parsed[,O3_sensor := ((O3_we - WE_Zero_total_O3)
      - (O3_aux - AUX_Zero_total_O3))/ WE_sens_O3_mv.ppb]

  # wide_parsed[,CO_sensor := CO_we-CO_aux]
  # wide_parsed[,NO_sensor := NO_we-NO_aux]
  # wide_parsed[,NO2_sensor := NO2_we-NO2_aux]
  # wide_parsed[,O3_sensor := O3_we-O3_aux]
  #wide_parsed[,pm25_sensor := Dylos_bin1 - Dylos_bin3]
  #wide_parsed[,pm10_sensor := Dylos_bin1 - Dylos_bin4]

  setnames(wide_parsed,"Monitor Name","monitor")

  #convert back to long preserving NA values so that any missing tag values can be identified
  long_parsed <- melt(wide_parsed, id.vars= c("date","monitor"), variable.name="tags")

  #order monitors as factors
  wide_parsed$monitor<-factor(wide_parsed$monitor, levels=paste0("SY",1:14))

  #format date
  wide_parsed[,date:=as.POSIXct(date, tz="America/Los_Angeles")]

  list(parsed=parsed_data, long=long_parsed, wide=wide_parsed)

}
