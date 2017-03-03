library(shiny)
library(ggplot2)

shinyUI(pageWithSidebar(

  headerPanel("Radical Dashboard"),

  sidebarPanel(
    checkboxGroupInput('channel',
      'Channel',
      c('Plantower1_pm2_5_mass', 'Plantower2_pm2_5_mass', 'RH_val', 'S1_val', 'S2_val', 'Temp_val', 'CO_sensor', 'NO_sensor', 'NO2_sensor', 'O3_sensor'),
      selected = 'Plantower1_pm2_5_mass'
      ),
    sliderInput('starttime',
      'Start time (hours ago)',
      min = 1, max = 24, value = 24, step = 6
      ),
    sliderInput('stoptime',
      'Stop time (hours ago)',
      min = 0, max = 24, value = 0, step = 6
      ),
    selectInput('site',
        'Site',
        c('NYC'),
        selected = 'NYC'
        )
      #,plotOutput('tlplot')
    ),

  mainPanel(
    plotOutput('tsplot')
  )


))
