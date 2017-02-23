library(readxl)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)

### Prepare Reference Data ###

load_DE <- function(){
  # Read in data
  ref_files <- list.files(paste(shared.drive, 'Data/calibration/', sep = '/'), pattern = 'ref_analyzers_201702[0-9]+\\.csv$', full.names = T)

  ref_data <- lapply(ref_files, function(x){
      read_csv(x, na = c('','NA','NaN')) %>%
          select(time = `Computer Time Stamp`,
                 ref.no = `2BTech NO, NO conc. (ppb)`,
                 ref.no2 = `CAPS 10-s avg NO2 (ppb)`,
                 ref.bc = `AE51 BC (ng/m3)`,
                 ref.co.1 = `Lngn1 unaj mnfld CO, ppm`,
                 ref.co.2 = `Lngn2 exp rm unadj CO, ppm`,
                 ref.co2 = `SenseAir CO2 conc. (ppm)`,
                 ref.o3 = `Optec O3 conc. (mg/m3)`,
                 ref.particle.ct = `P-Trak prtcl count (pt/cm3)`,
                 #ref.neph = `Neph intDL b-scat (m^-1)`
                 ref.neph = `Nephelometer scat. coeff. 1E-3 range`
          )
  })
  ref_data <- do.call(rbind, ref_data)
  ref_data <- ref_data %>% mutate(
      time = mdy_hm(time, tz = 'America/Los_Angeles'),
      ref.no = as.numeric(ref.no),
      ref.no2 = as.numeric(ref.no2),
      ref.o3 = ref.o3 * 24.45/48 #convert to ppb
  )
  }

ref_data <- load_DE()
