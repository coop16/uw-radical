options(scipen = 999)

library(readxl)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)

setwd('H:/projects/uw-radical')

source('config.r')
source('calibration/load_O3_ref.r')
source('calibration/load_DE_ref.r')
load(paste(shared.drive, 'Data/calibration/sensor_calibration_data_01Feb17_16Feb17.rdata', sep = '/'))
#tz(mesa.data$wide$date) <- 'etc/UTC'

### Prepare sensor data ###
average.sensor.measurements <- function(df, avg_period){
    df %>%
        mutate(time.bin = floor_date(date, unit = avg_period)) %>%
        select(-date) %>%
        group_by(sensor, time.hourly) %>%
        summarize_if(is.numeric, function(x){mean(x, na.rm=T)})
}
average.ref.measurements <- function(df, avg_period){
    df %>%
        mutate(
            time.bin = floor_date(time, unit = avg_period)
        ) %>%
        select(-time) %>%
        group_by(time.hourly) %>%
        summarize_if(is.numeric, function(x){mean(x, na.rm=T)})
}

sensor_data_30min <- average.sensor.measurements(mesa.data$wide, '30 minutes')
sensor_data_10min <- average.sensor.measurements(mesa.data$wide, '10 minutes')
sensor_data_05min <- average.sensor.measurements(mesa.data$wide, '5 minutes')

ref_data_30min <- average.ref.measurements(ref_data, '30 minutes')
ref_data_10min <- average.ref.measurements(ref_data, '10 minutes')
ref_data_05min <- average.ref.measurements(ref_data, '5 minutes')

ref_o3_30min <- average.ref.measurements(ref_o3, '30 minutes')
ref_o3_10min <- average.ref.measurements(ref_o3, '10 minutes')
ref_o3_05min <- average.ref.measurements(ref_o3, '5 minutes')






### Compare stuff!!! ###
compare <- sensor_data_10min  %>%
    ungroup() %>%
    inner_join(ref_data_10min, by = 'time.bin', suffix = c('.sensor', '.ref')) %>%
    arrange(time.bin) %>%
    mutate(sensor = factor(sensor))

ref_col <- c(
    'CO_sensor'             = 'ref.co.1',
    'O3_sensor'             = 'ref.o3',
    'NO_sensor'             = 'ref.no',
    'NO2_sensor'            = 'ref.no2',
    'pm.shinyei1'           = 'ref.neph',
    'pm.shinyei2'           = 'ref.neph',
    'Plantower1_pm2_5_mass' = 'ref.neph',
    'Plantower2_pm2_5_mass' = 'ref.neph'
)



### Show timeline ###
rec_times <- rbind(
    mesa.data$wide %>% select(sensor, time = date),
    ref_data %>% filter(!is.na(ref.no2)) %>% transmute(sensor = 'DE LAB', time),
    ref_o3 %>% transmute(sensor = 'O3 LAB', time)
)
plt <- ggplot(rec_times) + geom_point(aes(y = sensor, x = time))


### make scatterplot ###
make.scatterplot <- function(compare, col, ref_col){
    r2 <- format(round(cor(compare[ref_col], compare[col], use = 'pairwise')**2, 2) , nsmall=2)
    plt <- ggplot(compare) +
        geom_point(aes_string(x = ref_col, y = col, color = 'sensor')) +
        geom_smooth(aes_string(x = ref_col, y = col), method = 'lm', se = F) +
        ggtitle(paste(col,'(10 min avgs)\nr2=',r2)) +
        theme_minimal() +
        scale_color_discrete(drop = F)
    return(plt)
}
plts <- list()
for( col in names(sensor_data_10min) ){
    if (!(col %in% c('sensor', 'time'))){
        if(col %in% names(ref_col)){
            plts[[col]] <- make.scatterplot(compare, col, ref_col[col])
        }
    }
}

#for(col in names(plts)){ ggsave(paste0('X:/Documentation/calibration/sp_mesa1-5_',col,'.png'), plts[[col]], width = 5, height = 5) }
