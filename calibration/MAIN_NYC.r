### SET OPTIONS HERE ###
WK.DIR = 'H:/projects/uw-radical/'
DATE.RANGE = c(as.POSIXct('2017-03-06 00:00 PST'), as.POSIXct('2017-03-09 23:59 PST'))
MONITORS.OF.INTEREST = 1:99 # which monitors (MESA#) do you want?
########################

# Get reference data
# ------------------
### Load Data ###
source('config.r', encoding = 'UTF8')
print('loading kairos');source('getMESA_data_Kairos.r', encoding = 'UTF8') # get sensor data from Kairos
suppressWarnings(suppressMessages(sensor_data <- getMESA_data(start.date = DATE.RANGE[1], stop.date = DATE.RANGE[2])))
sensor_data <- sensor_data$wide %>% filter(sensor %in% paste0('MESA', MONITORS.OF.INTEREST))
source('calibration/load_NYC_ref.r', encoding = 'UTF8') # load raw files from folders like rad/calibration/MESA_NLk_Colocation_Data_[date]/

### Get set up and load necessary packages ###
options(scipen = 999)
setwd(WK.DIR)
list.of.packages <- c('stringr', 'lubridate', 'dplyr', 'reshape2', 'gdata')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,'Package'])]
if(length(new.packages)) install.packages(new.packages)
suppressWarnings(suppressMessages(sapply(list.of.packages, library, character.only = T) -> .shh))


### Load Data ###
source('config.r', encoding = 'UTF8')
source('getMESA_data_Kairos.r', encoding = 'UTF8') # get sensor data from Kairos
suppressWarnings(suppressMessages(sensor_data <- getMESA_data(start.date = DATE.RANGE[1], stop.date = DATE.RANGE[2])))
sensor_data <- sensor_data$wide %>% filter(sensor %in% paste0('MESA', MONITORS.OF.INTEREST))

### Prepare sensor data ###
average.sensor.measurements <- function(df, avg_period){
    df %>%
        mutate(time.bin = floor_date(date, unit = avg_period)) %>%
        group_by(sensor, time.bin) %>%
        summarize_if(is.numeric, function(x){mean(x, na.rm=T)}) %>%
        ungroup()
}
average.ref.measurements <- function(df, avg_period){
    df %>%
        mutate(time.bin = floor_date(datetime, unit = avg_period)) %>%
        group_by(sitename, time.bin) %>%
        summarize_if(is.numeric, function(x){mean(x, na.rm=T)})
}

sensor_data_10min <- average.sensor.measurements(sensor_data, '10 minutes')
ref_data_10min <- average.ref.measurements(clean.data, '10 minutes')

### load calibrations ###
load(paste0(shared.drive, 'Data/calibration/calibration_results.rdata')) #loads sensor.calibrations

### Compare stuff!!! ###
compare <- sensor_data_10min  %>%
    left_join(ref_data_10min, by = 'time.bin') %>%
    arrange(time.bin) %>%
    mutate(sensor = factor(sensor))
#%>%
#    select(time.bin, sensor, CO_sensor, O3_sensor, NO_sensor, NO2_sensor, S1_val, S2_val, matches('Plantower'), matches('ref\\.'))
compare <- compare[,names(compare) %in% c("time.bin", "sensor","S1_val","S2_val","CO","NO","NO2","PM25","O3","sitename","Temp_val") |
  grepl("sensor",names(compare)) | grepl("Plantower",names(compare))]


ref_col <- c(
    'CO_sensor'             = 'CO',
    'O3_sensor'             = 'O3',
    'NO_sensor'             = 'NO',
    'NO2_sensor'            = 'NO2',
    'S1_val'                = 'PM25',
    'S2_val'                = 'PM25',
    'Plantower1_pm2_5_mass' = 'PM25',
    'Plantower2_pm2_5_mass' = 'PM25'
)


### Calculate comparison statistics ###
sensor.calibrations <- list()
for( col in names(ref_col) ){ # make a lm for each sensor & pollutant
    sensor.calibrations[[col]] <- list()
    for( sensor in unique(compare$sensor) ){
        temp <- na.omit(compare[compare$sensor == sensor, c("sensor","sitename","time.bin","Temp_val",col, ref_col[col])])
        temp2 <- merge(temp, siteloc, by=c("sensor","sitename"))
        temp3 <- temp2[as.POSIXlt(temp2$start_time, "GMT") <= as.POSIXlt(temp2$time.bin, "GMT") &
                       as.POSIXlt(temp2$time.bin, "GMT") <= as.POSIXlt(temp2$end_time, "GMT"),]
        if( dim(temp3)[1] > 0 ){
            # are there any measures to compare?
            sensor.calibrations[[col]][[sensor]] <- lm(paste(ref_col[col], '~ Temp_val + ', col), data = temp3)
        }
    }
}
sensor.calibrations <- unlist(sensor.calibrations, recursive = F)

results <- data.frame(
    sensor    = names(sensor.calibrations) %>% str_extract('MESA[0-9]+$'),
    poll      = names(sensor.calibrations) %>% str_extract('^.*(?=\\.)'),
    r2        = unlist(lapply(sensor.calibrations, function(fit){summary(fit)$r.squared})),
    ct        = unlist(lapply(sensor.calibrations, function(fit){length(fitted.values(fit))})),
    pct_error = unlist(lapply(sensor.calibrations, function(fit){mean(resid(fit)/fitted.values(fit))})),
    intercept = unlist(lapply(sensor.calibrations, function(fit){coef(fit)[1]})),
    slope     = unlist(lapply(sensor.calibrations, function(fit){coef(fit)[2]}))
) %>% arrange(sensor, poll)
write.csv(results, paste0(shared.drive, 'Documentation/calibration/calibration_results', as.character(Sys.Date()), '.csv'), row.names =F)


### Make scatterplots ###
make.scatterplot <- function(compare, col, ref_col){
    plt <- ggplot(compare) +
        geom_point(aes_string(x = ref_col, y = col, color = 'sensor')) +
        geom_smooth(aes_string(x = ref_col, y = col, color = 'sensor'), method = 'lm', se = F) +
        ggtitle(paste(col,'(10 min avgs)')) +
        theme_minimal() +
        scale_color_discrete(drop = F)
    return(plt)
}
get.calib <- function(sensors, values, poll_name, calib_tbl){
  result <- rep(NA, length(sensors))
  for (i in unique(sensors)){
    result[sensors == i] <- calib_tbl[calib_tbl$sensor == i & calib_tbl$poll == poll_name,"intercept"] +
                            calib_tbl[calib_tbl$sensor == i & calib_tbl$poll == poll_name,"slope"]*values[sensors == i]
  }
  result
}
uw.calib <- read.csv("X:\\Documentation\\calibration\\calibration_results2017-03-02.csv", stringsAsFactors = FALSE)
plts <- list()
for( col in names(sensor_data_10min) ){
    if (!(col %in% c('sensor', 'time')) & col %in% unique(uw.calib$poll)){
        if(col %in% names(ref_col)){
            compare["calib_val"] <- get.calib(as.character(compare[["sensor"]]), compare[[col]], col, uw.calib)
#            plts[[col]] <- make.scatterplot(compare, "calib_val", ref_col[col])
            plts[[col]] <- make.scatterplot(compare, col, ref_col[col])
        }
    }
}

suppressWarnings(
    for(col in names(plts)){ ggsave(paste0(shared.drive, 'Documentation/calibration/calibration_scatters', as.character(Sys.Date()),'_', col,'.png'), plts[[col]], width = 5, height = 5) }
)

print(paste0('Done! View results in ', shared.drive, 'Documentation/calibration/'))
