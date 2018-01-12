################################################################################
#  This script is used so that you can get around a bug connecting to an Access 
#  database using R 64 bit.  From the main document, we can say we want to run 
#  this script using 32 bit, and then save an Rdata file that we can load from the 
#  main script.  Therefore, if we are running the main script in R 64 bit,
#  we avoid having to manually set R to work in 32bit (which requires a restart)
#  prior to connecting to the access database.

#see: https://stackoverflow.com/questions/13070706/how-to-connect-r-with-access-database-in-64-bit-window
################################################################################

library(RODBC) 

## set the database path
inCopyDbPath <- "X:/Data/RAD_monitor_database.accdb"

## connect to the database
conAccdb <- odbcConnectAccess2007(inCopyDbPath) 

## Fetch the sensor location table from the database. Just select the whole table
sensorlocations <- sqlFetch (conAccdb,"sensor_location_tbl")

## Save the tables
save(sensorlocations, file = "X:/Data/rawdata/sensor_location_data.rda")

close(conAccdb)