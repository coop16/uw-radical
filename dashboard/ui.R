library(shiny)
library(ggplot2)

shinyUI(pageWithSidebar(

  headerPanel("Radical Dashboard"),

  sidebarPanel(
    checkboxGroupInput('y',
      'Channel',
      c('Plantower1_pm2_5_mass', 'Plantower2_pm2_5_mass', 'RH_val', 'S1_val', 'S2_val', 'Temp_val', 'CO_sensor', 'NO_sensor', 'NO2_sensor', 'O3_sensor'),
      selected = 'Plantower1_pm2_5_mass'
      ),
      plotOutput('tlplot')
    ),

  mainPanel(
    plotOutput('tsplot')
  )


))
