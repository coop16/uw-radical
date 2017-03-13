### Load Ozone Reference Data ###
load_schedule <- function(){
    schedule_file <- list.files(paste(shared.drive, 'Data/calibration', sep = '/'), pattern = 'Calibration_Tracking', full.names = T)[1]
    schedule_data <- read.csv(schedule_file, stringsAsFactors = F, check.names = F) %>%
        transmute(
            sensor = Monitor_id,
            exposure_start = mdy_hm(Start_date_time, tz = 'America/Los_Angeles'),
            exposure_end   = mdy_hm(End_date_time,   tz = 'America/Los_Angeles'),
            location = `Location (e.g., diesel chamber)`
            )

    # longify schedule, interpretting ranges (e.g. "1-2, 4-5" -> MESA1, MESA2, MESA4, MESA5)
    schedule_data_long <- list()
    for(row in 1:nrow(schedule_data)){
        schedule_data_long[[row]] <- cbind(
                sensor = paste0('MESA',
                    eval(
                        parse(text = paste("c(", gsub("\\-", ":", str_extract(schedule_data[row,'sensor'], '(?<=MESA ).*')), ")"))
                        )
                    ),
                schedule_data[row, 2:ncol(schedule_data)],
                row.names = NULL
            )
    }
    schedule_data_long <- do.call(rbind, schedule_data_long)

    return(schedule_data_long)
}

schedule_data <- load_schedule()
