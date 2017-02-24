options(scipen = 999)
setwd('H:/projects/uw-radical')


### Get and load necessary packages ###
list.of.packages <- c('stringr', 'lubridate', 'dplyr', 'ggplot2', 'stringr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,'Package'])]
if(length(new.packages)) install.packages(new.packages)
sapply(list.of.packages, library, character.only = T) -> .shh


### Load Data ###
source('config.r', encoding = 'UTF8')
source('calibration/load_O3_ref.r', encoding = 'UTF8')
source('calibration/load_DE_ref.r', encoding = 'UTF8')
load(paste(shared.drive, 'Data/calibration/sensor_calibration_data_01Feb17_16Feb17.rdata', sep = '/'))


### Prepare sensor data ###
average.sensor.measurements <- function(df, avg_period){
    df %>%
        mutate(time.bin = floor_date(date, unit = avg_period)) %>%
        select(-date) %>%
        group_by(sensor, time.bin) %>%
        summarize_if(is.numeric, function(x){mean(x, na.rm=T)})
}
average.ref.measurements <- function(df, avg_period){
    df %>%
        mutate(
            time.bin = floor_date(time, unit = avg_period)
        ) %>%
        select(-time) %>%
        group_by(time.bin) %>%
        summarize_if(is.numeric, function(x){mean(x, na.rm=T)})
}

sensor_data_30min <- average.sensor.measurements(mesa.data$wide, '30 minutes')
sensor_data_10min <- average.sensor.measurements(mesa.data$wide, '10 minutes')

ref_data_30min <- average.ref.measurements(ref_data, '30 minutes')
ref_data_10min <- average.ref.measurements(ref_data, '10 minutes')

ref_o3_30min <- average.ref.measurements(ref_o3, '30 minutes')
ref_o3_10min <- average.ref.measurements(ref_o3, '10 minutes')


### Compare stuff!!! ###
compare <- sensor_data_30min  %>%
    ungroup() %>%
    inner_join(ref_data_30min, by = 'time.bin', suffix = c('.sensor', '.ref')) %>%
    arrange(time.bin) %>%
    mutate(sensor = factor(sensor))

ref_col <- c(
    'CO_sensor'             = 'ref.co.1',
    #'O3_sensor'             = 'ref.o3',
    'NO_sensor'             = 'ref.no',
    'NO2_sensor'            = 'ref.no2',
    'S1_val'           = 'ref.neph',
    'S2_val'           = 'ref.neph',
    'Plantower1_pm2_5_mass' = 'ref.neph',
    'Plantower2_pm2_5_mass' = 'ref.neph'
)


### Calculate comparison statistics ###
sensor.calibrations <- list()
for( col in names(ref_col) ){
  if( sum(is.na(compare[,col])) & sum(is.na(compare[,ref_col[col]])) ){
    sensor.calibrations[[col]] <- lm(paste(col, '~', ref_col[col], '+ sensor'), data = compare)
  }
}


### Show timeline ###
rec_times <- rbind(
    mesa.data$wide %>% filter(!is.na(Plantower1_pm2_5_mass)) %>% select(sensor, time = date),
    ref_data %>% transmute(sensor = 'DE LAB', time),
    ref_o3 %>% transmute(sensor = 'O3 LAB', time)
)
plt <- ggplot(rec_times) + geom_point(aes(y = sensor, x = time))


### make scatterplot ###
make.scatterplot <- function(compare, col, ref_col){
    plt <- ggplot(compare) +
        geom_point(aes_string(x = ref_col, y = col, color = 'sensor')) +
        geom_smooth(aes_string(x = ref_col, y = col, color = 'sensor'), method = 'lm', se = F) +
        ggtitle(paste(col,'(30 min avgs)')) +
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
