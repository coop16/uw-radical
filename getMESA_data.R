library(httr)
library(dplyr)
library(stringr)

datafeed_get_files <- function(){
    source('config.r')
    http.response <- GET(datafeed.url, authenticate(datafeed.usr, datafeed.pwd))
    if( http.response$status_code == 200 ){
        content(http.response, 'text', encoding = 'UTF-8') %>% str_extract_all('(?<=a href=\\")MESA[0-9]+\\.csv') %>% unlist()
    }else{
        stop('DOWNLOAD FAILED: Initial Connection')
    }
}

datafeed_download_file <- function(fnames, quiet = F){
    if(length(fnames) > 1){
        lapply(fnames, function(x){datafeed_download_file(x, quiet = quiet)}) %>% bind_rows() %>% factory_gas_calibrations()
    }else{
        source('config.r')
        http.response <- GET(paste(datafeed.url, fnames, sep = '/'), authenticate(datafeed.usr, datafeed.pwd))
        if( http.response$status_code == 200 ){
            if( !quiet ){ message(fnames) }
            read.csv(text=content(http.response, 'text', encoding = 'UTF-8')) %>%
            mutate(
                date = as.POSIXct(date, format="%Y-%m-%d %H:%M:%S",tz="America/Los_Angeles")
            ) %>%
            factory_gas_calibrations()
        }else{
            stop(paste0('DOWNLOAD FAILED: ', fnames))
        }
    }
}

datafeed_download_all <- function(quiet = F){
    datafeed.files <- datafeed_get_files() # Find files to download
    lapply(datafeed.files, function(x){datafeed_download_file(x, quiet = quiet)}) %>% bind_rows() %>% factory_gas_calibrations()# Download each csv and combine
}

factory_gas_calibrations <- function(df){
    # load & merge calibration values
    calvals_CO  <- read.csv("MESA_GasSensorList_CO.csv",  stringsAsFactors = F)
    calvals_NO  <- read.csv("MESA_GasSensorList_NO.csv",  stringsAsFactors = F)
    calvals_NO2 <- read.csv("MESA_GasSensorList_NO2.csv", stringsAsFactors = F)
    calvals_O3  <- read.csv("MESA_GasSensorList_O3.csv",  stringsAsFactors = F)
    calvals <- calvals_CO %>%
        full_join(calvals_NO,  by = 'Monitor.Name') %>%
        full_join(calvals_NO2, by = 'Monitor.Name') %>%
        full_join(calvals_O3,  by = 'Monitor.Name') %>%
        select(-matches('sensor'))
    df <- df %>% left_join(calvals, by = c('monitor' = 'Monitor.Name'))

    # calculate sensor values based on factory calibration values
    df <- df %>%
        mutate(
            CO_sensor = ((CO_we - WE_Zero_total_CO) - (CO_aux - AUX_Zero_total_CO))/ WE_sens_CO_mv.ppb,
            NO_sensor = ((NO_we - WE_Zero_total_NO) - (NO_aux - AUX_Zero_total_NO))/ WE_sens_NO_mv.ppb,
            NO2_sensor = ((NO2_we - WE_Zero_total_NO2) - (NO2_aux - AUX_Zero_total_NO2))/ WE_sens_NO2_mv.ppb,
            O3_sensor = ((O3_we - WE_Zero_total_O3) - (O3_aux - AUX_Zero_total_O3))/ WE_sens_O3_mv.ppb
        )
    df <- df[,!(names(df) %in% names(calvals))] # drop calibration values
    df
}

getMESA_data <- function(){
    datafeed_download_all()
}
