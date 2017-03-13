### Load Ozone Reference Data ###
if(!'readxl' %in% installed.packages()[,'Package']) install.packages('readxl')
library(readxl)

load_nyc <- function(){
    ref_files <- list.files(paste(shared.drive, 'Data/calibration/NYC_reference_data', sep = '/'), full.names = T)
    ref_data <- lapply(ref_files, function(x){
        read_excel(x, skip = 4, na = '<Samp') %>%
        mutate(
            time = mdy_hms(`Date & Time`, tz = 'America/New_York')
            )
    })
    return(ref_data)
}

ref_nyc <- load_nyc()
