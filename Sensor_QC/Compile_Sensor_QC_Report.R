################################
# Script to compile QC Report  #
################################

#set working directory to where QC report Rmarkdown file is
setwd("X:/Production_code/sensor_QC/")

#define report file name including report date (Monday after the monitoring period) and save to X:/Sensor_QC_Reports/
reportname<-paste('X:/SensorQC_Reports/Sensor_QC_Report_',format(Sys.Date()- as.numeric(Sys.Date()-1+4)%%7,'%m_%d_%Y'),'.html', sep='')

#Compile document and save with file name indicating report date
rmarkdown::render('Sensor_QC_Report.Rmd', output_file = reportname )


