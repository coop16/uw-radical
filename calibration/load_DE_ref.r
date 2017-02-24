### Prepare Reference Data ###
load.de <- function(de.dir){
    # find relevant files
    de.files <- list.files(de.dir, full.names = T)
    main.file <- str_subset(de.files, 'Mobile_DAS.*\\.txt$')[1]
    caps.file <- str_subset(de.files, 'CAPS-NO2.*\\.dat$')[1]
    langan.files <- str_subset(de.files, 'LanganCO.*\\.csv')

    read.de.main <- function(main.file){
        main.data <- read.table(main.file, header = T, sep = '\t', check.names = F, na.strings = c('NA', 'NaN', ''), stringsAsFactors = F) %>%
            transmute(
                time = mdy_hms(`Computer Time Stamp`, tz = 'America/Los_Angeles'),
                ref.no = `2BTech NO, NO conc. (ppb)`,
                ref.temp = `Precon HS-2000 Temp (°C)`,
                ref.rh = `Precon HS-2000 RH (%)`,
                ref.co2 = `SenseAir CO2 conc. (ppm)`,
                ref.particle.ct = `P-Trak particle count`,
                ref.neph = if_else(`Nephelometer scat. coeff. 2E-4 range` < .95*2.101e-04, `Nephelometer scat. coeff. 2E-4 range`, `Nephelometer scat. coeff. 1E-3 range`), #take high-range channel if more sensitive channel exceeds 95% of max
                ref.o3 = `Optec O3 conc. (mg/m³)` * 24.45/48 #convert to ppb
                )
    }

    read.de.caps <- function(caps.file){
        caps.data <- read.table(caps.file, header = T, sep = ',', check.names = F, na.strings = c('NA', 'NaN', ''), stringsAsFactors = F, comment.char = '%') %>%
            transmute(
                time = ymd_hms(Timestamp, tz = 'America/Los_Angeles'),
                ref.no2 = NO2
                )
    }

    read.de.langan <- function(langan.file){
        langan.data <- read.table(langan.file, header = T, sep = ',', check.names = F, na.strings = c('NA', 'NaN', ''), stringsAsFactors = F, skip = 1) %>%
            mutate(time = mdy_hms(lag(`Date Time, GMT-08:00`), tz = 'America/Los_Angeles')) %>% #co timestamps are end of sample time and should be lagged
            filter(!is.na(time)) %>% #toss the first 30 sec because of lag
            select(
                time,
                ref.co = matches('CO# [0-9]+')
                ) %>%
            mutate(ref.co = ref.co - 1) #adjust for offset
    }


    # Load correct files
    de.main.raw <- read.de.main(main.file)
    de.caps.raw <- read.de.caps(caps.file)
    de.langan.raw <- lapply(langan.files, read.de.langan)


    # Bin each time period by 5 minute intervals, average, and join
    time.avg <- function(df, unit = '5 minutes'){
        df %>%
            mutate(time = floor_date(time, unit = '5 minutes')) %>%
            group_by(time) %>%
            summarize_if(is.numeric, function(x){mean(x, na.rm=T)})
        }

    de.data <- time.avg(de.main.raw) %>%
        full_join(time.avg(de.caps.raw), by = 'time') %>%
        full_join(time.avg(de.langan.raw[[1]]), by = 'time') %>%
        full_join(time.avg(de.langan.raw[[2]]), by = 'time', suffix = c('.1', '.2'))

}

de.dirs <- str_subset(list.dirs(paste(shared.drive, '/Data/calibration', sep = '/')), 'Colocation_Data_[0-9]+')
ref_data <- list()
for(de.dir in de.dirs) ref_data[[de.dir]] <- load.de(de.dir)
ref_data <- do.call(rbind, ref_data)
