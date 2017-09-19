# library(data.table)
# library(ggplot2)
# require(dtplyr) 
# library(bit64)

identify_burn <- function(datacalraw) {
  if(!"sensor"%in%colnames(datacalraw))
    datacalraw$sensor<-"boxa"
  #criteria for identifying peaks
  indexvalsNO<-datacalraw[,list("val"=.I[NO_we>3000&!is.na(NO_we)]), by=sensor]
  indexvalsNO<-indexvalsNO[!val-c(1, head(val,-1))==1]
  indexvalsCO<-datacalraw[,list("val"=.I[CO_we>3000&!is.na(CO_we)]), by=sensor]
  indexvalsCO<-indexvalsCO[!val-c(1, head(val,-1))==1]
  
  #index values
  indexvals<-unique(rbindlist(list(indexvalsCO, indexvalsNO)))
  indexvals$datetime<-datacalraw[indexvals$val]$datetime
  indexvals[,stopdate:=datetime+8*60*60]
  
  datacalraw[,stopdate:=datetime]
  
  setkey(datacalraw, datetime, stopdate)
  setkey(indexvals, datetime, stopdate)
  
  #identify all datapoints within the range
  remove<-foverlaps(indexvals, datacalraw)
  
  setkey(remove, datetime, sensor)
  setkey(datacalraw, datetime, sensor)
  
  datacalraw<- setDT(anti_join(datacalraw, remove))
  
  if("Dylos_Bin_1"%in%names(datacalraw)){
    datacalraw<-datacalraw[as.numeric(as.character(Dylos_Bin_1))<=100000,]
    datacalraw<-datacalraw[as.numeric(as.character(Dylos_Bin_2))<=100000,]
    datacalraw<-datacalraw[as.numeric(as.character(Dylos_Bin_3))<=100000,]
    datacalraw<-datacalraw[as.numeric(as.character(Dylos_Bin_4))<=100000,]
    datacalraw<-datacalraw[as.numeric(as.character(Dylos_Bin_1))>=0,]
    datacalraw<-datacalraw[as.numeric(as.character(Dylos_Bin_2))>=0,]
    datacalraw<-datacalraw[as.numeric(as.character(Dylos_Bin_3))>=0,]
    datacalraw<-datacalraw[as.numeric(as.character(Dylos_Bin_4))>=0,]
  }
  if("Dylos_bin1"%in%names(datacalraw)){
    datacalraw<-datacalraw[as.numeric(as.character(Dylos_bin1))<=100000,]}
  
 datacalraw[,sensor:=NULL]
  datacalraw
}