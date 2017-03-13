library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

setwd('..')
source('getMESA_data.R')

shinyServer(function(input, output) {
  site_lookup = list(
      'ALL' = str_extract(datafeed_get_files(), '.*(?=\\.csv)'),
      'NYC' = c('MESA9', 'MESA10', 'MESA11', 'MESA13', 'MESA14', 'MESA16')
      )
  dataset <- reactivePoll(30 * 1000, NULL, # check every 30 seconds whether it's time for an update
    checkFunc = function(){floor_date(Sys.time() - 30, unit = '5 mins')}, # server updates every 5 minutes on the 5's -- we'll wait 30 extra seconds to allow for clock diffs
    valueFunc = function(){
        datafeed_download_file(paste0(site_lookup[[input$site]], '.csv')) %>% gather(tags, value, -monitor, -date, na.rm = T) %>% filter(date > Sys.time() - 72*60*60)
        }
    )

  # Timeseries plot
  output$tsplot <- renderPlot({
    ts.plt <- ggplot(
        dataset()[which(dataset()$tags %in% input$channel),]
      ) +
      geom_line(aes_string(x='date', y='as.numeric(value)', color = 'tags')) +
      geom_point(aes_string(x='date', y='as.numeric(value)', color = 'tags')) +
      facet_wrap(~monitor, ncol = 1) +
      theme_minimal() +
      xlab('time') +
      ylab('val') +
      coord_cartesian(xlim = c(Sys.time() - (input$starttime*60*60), Sys.time() - (input$stoptime*60*60))) +
      ggtitle(paste(input$channel, 'as of', as.character(max(dataset()$date)))) +
      theme(legend.position="top", strip.text = element_text(size=14, face="bold"))


    print(ts.plt)
  }, height = 1500)
  # output$tlplot <- renderPlot({
  #   # Traffic Light plot
  #
  #   last.24h <- dataset() %>%
  #     filter(date > max(date) - (24*60*60)) %>%
  #     filter(tags %in% c('Plantower1_pm2_5_mass', 'Plantower2_pm2_5_mass', 'RH_val', 'S1_val', 'S2_val', 'Temp_val', 'CO_sensor', 'NO_sensor', 'NO2_sensor', 'O3_sensor')) %>%
  #     mutate(ok = is.na(value))
  #   last.pass <- last.24h %>% filter(date == max(date))
  #
  #   last24h.ok <- last.24h %>% group_by(monitor) %>% summarize(ok.last24h = sum(ok) > 0)
  #   last.pass.ok <- last.pass %>% group_by(monitor) %>% summarize(ok.lastpass = sum(ok) > 0)
  #
  #   status <- dataset() %>%
  #     dplyr::select(monitor) %>%
  #     distinct() %>%
  #     left_join(last24h.ok) %>%
  #     left_join(last.pass.ok) %>%
  #     mutate(
  #       ok.last24h = ok.last24h & !is.na(ok.last24h),
  #       ok.lastpass = ok.lastpass & !is.na(ok.lastpass)
  #     ) %>%
  #     transmute(
  #       monitor,
  #       status = factor(ok.last24h + ok.lastpass)
  #     ) %>%
  #     arrange(monitor)
  #
  #   tl.plt <- ggplot(status) +
  #     geom_point(aes(x = '', y = monitor), color = 'black', size = 10) +
  #     geom_point(aes(x = '', y = monitor, color = status), size = 8) +
  #     xlab('') +
  #     theme_minimal() +
  #     scale_color_manual(values = c('red', 'yellow', 'green'), guide = F)
  #
  #   print(tl.plt)
  #
  # })
  renderText

})
