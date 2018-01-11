library(readr)
library(data.table)


#-----------------------------------------------------------------------------------------------------------------#
#Get subset of data from last week 
#   >Default is Monday through Sunday (ending on midnight (UTC) of most recent Sunday)
#       >Note that midnight UTC is late afternoon/evening PST
#   > If set "fromLastMonday=F, then will go through previous night rather than previous Sunday night
#-----------------------------------------------------------------------------------------------------------------#
getLastWeek <- function(fromLastMonday=T){

  #get current time and date (Converted to Coordinated Universal Time)
  currTime<-Sys.time()
  attr(currTime, "tzone") <- "UTC"
  currDate<-Sys.Date()
  attr(currDate, "tzone") <- "UTC"
  
  #function to get previous monday from a date string
  lastMonday <- function(x){x - as.numeric(x-1+4)%%7}
  
  if(fromLastMonday){
    querydate<-lastMonday(currDate)
  }else{
    querydate<-currDate
    
  }
  
  #get subset of data from past week 
  lastweekdat<-subset(x = fulldat,as.Date(as.POSIXlt(date)) >= (querydate-7) & as.Date(as.POSIXlt(date)) < querydate  )

  # Drop index (first column) and CO_sensor,NO_sensor, NO2_sensor, O3_sensor (last 4 columns)
  lastweekdat <- lastweekdat[,2:(ncol(lastweekdat)-4)]
  
  return(lastweekdat)
}

#-----------------------------------------------------------------------------------------------------------------#
#Get list of all monitors in (in full dataset or past week)
#-----------------------------------------------------------------------------------------------------------------#

getMonitorList<-function(dat){

#get list of monitors (from full dataset or last week dataset depending on which is the input)
monitorlist<-unique(dat$monitor)

#ordered by study and number
actmon<-monitorlist[grepl(pattern = "ACT",x = monitorlist)]
actmon.index<-as.numeric(substring(actmon,4) )
actmonsorted<-paste0("ACT",sort(actmon.index) )
mesamon<-monitorlist[grepl(pattern = "MESA",x = monitorlist)]
mesamon.index<-as.numeric(substring(mesamon,5) )
mesamonsorted<-paste0("MESA",sort(mesamon.index) )
sortedMonitors<-c(actmonsorted,mesamonsorted)

return(sortedMonitors)

}


#-----------------------------------------------------------------------------------------------------------------#
#Get monitors without any data for the past week
#-----------------------------------------------------------------------------------------------------------------#

getMissingMonitors<-function(monlist,weekdata){
  
  #get list of monitors which we have data for in past week
  wkmonlist<-unique(weekdata$monitor)
  
  #get list of monitors we have data for ever, but not for this week
  missingmonitors<-setdiff(monlist,wkmonlist)
  
  return(missingmonitors)
}

#-----------------------------------------------------------------------------------------------------------------#
#Get all completeness data for a single monitor (by day)
#-----------------------------------------------------------------------------------------------------------------#
getMonitorCompleteness<-function(mon,weekdata,fulldata){
  
  #get maximum sample size by any monitor for the week
  numbercompletepermonitor <- aggregate(x = weekdata, by = list(weekdata$monitor), FUN = function(x) sum(!is.na(x)))
  maxcomplete<-max(numbercompletepermonitor[,-1])  
  
  #get maximum sample size by any monitor for each day
  daylist<-unique(as.Date(as.POSIXlt(weekdata$date)) )
  maxDayObs<-rep(NA,7)
  for(i in 1:7){
    dayData<-subset(weekdata,as.Date(as.POSIXlt(date))==daylist[i])
    numbercompletepermonitor_day<- aggregate(x = dayData, by = list(dayData$monitor), FUN = function(x) sum(!is.na(x)))
    maxDayObs[i]<-max(numbercompletepermonitor_day[,-1])  
  }
  
  #get subset of data for the particular monitor  
  mdat<-subset(weekdata,monitor==mon)
  
  #if have no data for monitor don't proceed
  if(dim(mdat)[1]==0){
    return("No Data Collected for this monitor last week.")
  }else{
    
    #get complete sensor observations for each day
    dailycomplete = aggregate(x = mdat, by = list(as.Date(as.POSIXlt(mdat$date)) ), FUN = function(x) sum(!is.na(x)))
    
    #get maximum observations possible for each day with data observed (should only vary by 1 or two at most per day)
    maxDayObs_mon<-maxDayObs[daylist %in% unique(as.Date(as.POSIXlt(mdat$date)))]

    #get proportion complete for each day
    dailycompleteprop <- dailycomplete
    for(i in 1:dim(dailycompleteprop)[1] ){
      dailycompleteprop[i,-1]<-round( dailycompleteprop[i,-1]/maxDayObs_mon[i] ,digits=2)
    }
    
    #complete sensor observations for entire week
    weekcomplete<-c(apply(dailycomplete[,4:dim(dailycomplete)[2]],2,sum) )
    
    #Row for sample size (maximum observations we could observe if all complete)
    possibleSampleSize<-c(maxcomplete,maxDayObs_mon)
    
    #create table with # complete in each cell and column for whole week
    dailyAllInfo<-cbind(weekcomplete,t(dailycomplete[,4:length(dailycomplete)]))
    
    #add row with sample size
    dailyAllInfo<-rbind("Sample Size"=possibleSampleSize,dailyAllInfo)
    
    #Add percentages to each cell in table (besides first row)
    for(i in 1:dim(dailyAllInfo)[2] ){
      dailyAllInfo[-1,i]<-paste0(dailyAllInfo[-1,i]," (", round(as.numeric(dailyAllInfo[-1,i])/as.numeric(dailyAllInfo[1,i]) , digits=2)*100 , "%)" )
    }
    
    #add more formatting
    outputtab<-as.data.frame(cbind(rownames(dailyAllInfo),dailyAllInfo) )
    colnames(outputtab)<-c(" ","Week",as.character(daylist[daylist %in% unique(as.Date(as.POSIXlt(mdat$date)))]) )
    
    #return table with all info
    return(outputtab)
  }
}

#-----------------------------------------------------------------------------------------------------------------#
#get basic summary data for all monitors 
#-----------------------------------------------------------------------------------------------------------------#
getBasicSummary<-function(weekdata,monlist){
  
  #get number complete for each monitor
  numbercomplete <- aggregate(x = weekdata, by = list(weekdata$monitor), FUN = function(x) sum(!is.na(x)))
  
  #maximum number of observations for previous week by any monitor
  maxcomplete<-max(numbercomplete[,-1])

  #average completeness proportion for all Plantower pm measures
  plantowerProp<-round(apply(numbercomplete[,4:21]/maxcomplete,1,mean) ,digits=2)
  
  #average completeness proportion for Shinyei measures
  shinyeiProp<-round(apply(numbercomplete[,32:33]/maxcomplete,1,mean) ,digits=2)
  
  #average completeness proportion for all gas measures
  gasProp<-round(apply(numbercomplete[,24:31]/maxcomplete,1,mean) ,digits=2)  
  
  #average completeness proportion across all sensor measures
  aveProp<-round(apply(numbercomplete[,4:33]/maxcomplete,1,mean) ,digits=2)
  
  #minimum proportion complete for measure of a given monitor
  minProp<-round(apply(numbercomplete[,4:33]/maxcomplete,1,min) ,digits=2)
  
  #create table of basic summary information
  #sample size, Plantower completeness, Shinyei completeness, Gas completeness, Total average completeness, Minimum Sensor Completeness
  #only output monitors with at least one sensor with below 90% completeness
  basicSummaryTab<-cbind(numbercomplete$Group.1,maxcomplete,aveProp,minProp,plantowerProp,shinyeiProp,gasProp)
  orderedtab<-as.data.table(basicSummaryTab)
  setkey(orderedtab,"minProp")    
  smalltab<-orderedtab[orderedtab$minProp<.9,]
  colnames(smalltab)<-c("Monitor","Sample Size","Ave. Sensor Completeness","Min. Sensor Completeness","Ave. Plantower Completeness","Ave. Shinyei Completeness","Ave. Gas Completeness")
  
  return(smalltab)
}



