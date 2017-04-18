list.of.packages <- c('shiny', 'ggplot2', 'dplyr', 'tidyr', 'lubridate')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,'Package'])]
if(length(new.packages)) install.packages(new.packages)
suppressWarnings(suppressMessages(sapply(list.of.packages, library, character.only = T) -> .shh))

shinyUI(pageWithSidebar(

  headerPanel("MESA Air Dashboard"),

  sidebarPanel(
    "This is an experimental data viewer for live MESA Air data. Data shown is raw, has not been QA'ed, and has only had preliminary, manufacturer-provided calibrations applied. The final data used in health research will be averaged over longer time periods and will be adjusted to reflect in-house calibration exercises.",
    checkboxGroupInput('channel',
      'Channel',
      c('Plantower1_pm2_5_mass', 'Plantower2_pm2_5_mass', 'RH_val', 'S1_val', 'S2_val', 'Temp_val', 'CO_sensor', 'NO_sensor', 'NO2_sensor', 'O3_sensor'),
      selected = 'Plantower1_pm2_5_mass'
      ),
    sliderInput('starttime',
      'Start time (hours ago)',
      min = 0, max = 72, value = 24, step = 1
      ),
    sliderInput('stoptime',
      'Stop time (hours ago)',
      min = 0, max = 72, value = 0, step = 1
      ),
    selectInput('site',
        'Site',
        names(site_lookup),
        selected = 'New York'
        )
      #,plotOutput('tlplot')
    ),

  mainPanel(
    plotOutput('tsplot')
  )


))
