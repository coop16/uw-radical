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
#Get monitors without data for at least one day in past week
#-----------------------------------------------------------------------------------------------------------------#

getMissingMonitors<-function(monlist,weekdata){
  
  library(data.table)
  
  #create empty vector
  daysofweek<-rep(NA,length(monlist) )
  
  #fill in number of days with data for each monitor
  for(i in 1:length(monlist)){
    #get subset of data for the particular monitor  
    mdat<-subset(weekdata,monitor==monlist[i])
    daysofweek[i]<-length(unique(as.Date(as.POSIXlt(mdat$date))))
  }
  
  daysofweek.df<-as.data.frame(cbind(monlist,daysofweek))
  missdays<-subset(daysofweek.df,daysofweek!=7)  
  ordered<-setkey(as.data.table(missdays),'daysofweek')
  colnames(ordered)<-c("Monitor","Number of Days with Data ")
  return(ordered)
}

#-----------------------------------------------------------------------------------------------------------------#
#Get all completeness data for a single monitor (by day)
#-----------------------------------------------------------------------------------------------------------------#
getMonitorCompleteness<-function(mon,weekdata){
  
  #get subset of data for the particular monitor  
  mdat<-subset(weekdata,monitor==mon)
  
  #if have no data for monitor don't proceed
  if(dim(mdat)[1]==0){
    return("No Data Collected for this monitor last week.")
  }else{
    
    #get sample size, # complete and proportion complete for each day
    dailycount = aggregate(x = mdat, by = list(  as.Date(as.POSIXlt(mdat$date))   ), FUN = length)
    dailycomplete = aggregate(x = mdat, by = list(as.Date(as.POSIXlt(mdat$date)) ), FUN = function(x) sum(!is.na(x)))
    dailycompleteprop = aggregate(x = mdat, by = list(as.Date(as.POSIXlt(mdat$date)) ), FUN = function(x) round(mean(!is.na(x)),digits = 3  ) )
    
    #add row for week total to each count table (we'll deal with proportion table later )
    newdailycount<-rbind(apply(dailycount[,-1],2,sum) ,dailycount[-1])
    row.names(newdailycount)<-c("Week",as.character(dailycount$Group.1))
    newdailycomplete<-rbind(apply(dailycomplete[,-1],2,sum) ,dailycomplete[-1])
    row.names(newdailycomplete)<-c("Week",as.character(dailycomplete$Group.1))

    #create table that shows "sample size , # complete (percent complete)" in each cell
      #just make copy of a table with correct dimensions
      dailyAllInfo<-newdailycount[,3:dim(newdailycount)[2]] 
      
      #fill in table
      for(i in 3:dim(newdailycount)[2]){
        dailyAllInfo[colnames(newdailycount)[i] ]<-paste0(newdailycount[,i]," , ",newdailycomplete[,i]," (",round(newdailycomplete[,i]/newdailycount[,i],digits=2)*100,"%)")
      }
    
    #return table with all info
    return(t(as.data.frame(dailyAllInfo)))
  }
}

#-----------------------------------------------------------------------------------------------------------------#
#get basic summary data for all monitors 
#-----------------------------------------------------------------------------------------------------------------#
getBasicSummary<-function(weekdata){

  #get number complete and number na for each sensor for each monitor
  totalcount<- aggregate(x = weekdata, by = list(weekdata$monitor), FUN = length)
  numbercomplete <- aggregate(x = weekdata, by = list(weekdata$monitor), FUN = function(x) sum(!is.na(x)))

  #sample size for monitor (all sensors)
  nAllSensors<-apply(totalcount[,4:33],1,sum)

  #average completeness proportion for all pm measures
  pmProp<-round(apply(numbercomplete[,4:21]/totalcount[,4:21],1,mean) ,digits=2)
  
  #average completeness proportion for all gas measures
  gasProp<-round(apply(numbercomplete[,24:31]/totalcount[,24:31],1,mean) ,digits=2)  
  
  #average completeness proportion across all sensor measures
  aveProp<-round(apply(numbercomplete[,4:33]/totalcount[,4:33],1,mean) ,digits=2)
  
  #minimum proportion complete for measure of a given monitor
  minProp<-round(apply(numbercomplete[,4:33]/totalcount[,4:33],1,min) ,digits=2)
  
  #create table of basic summary information
  #sample size, PM completeness, Gas completeness, Total average completeness, Minimum Sensor Completeness
  #only output monitors with at least one sensor with below 90% completeness
  basicSummaryTab<-cbind(totalcount$Group.1,nAllSensors,pmProp,gasProp,aveProp,minProp)
  orderedtab<-as.data.table(basicSummaryTab)
  setkey(orderedtab,"minProp")    
  smalltab<-orderedtab[orderedtab$minProp<.9,]
  colnames(smalltab)<-c("Monitor","Sample Size","PM Completeness","Gas Completeness","Ave. Completeness","Min. Sensor Completeness")
  
  return(smalltab)
}



