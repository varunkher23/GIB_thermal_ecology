acceleration_data_from_movebank=function(study,animalName,date_start,date_end,step_length,creds){

library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(janitor)
library(tidyjson)
library(move)
  
dates=seq.Date(from=as.Date(date_start),to=as.Date(date_end),by = step_length)
test_data_new=data.frame()
test_data_temp=data.frame()
##### Retrieve acceleration data from movebank ####
  {
    for (i in 1:length(dates)) {
      timestamp_start=paste(str_replace_all(dates[i],"-",""),"0530000",sep="")
      timestamp_end=paste(str_replace_all(string = dates[i]+step_length,"-",""),"0530599",sep="")
      
      continue=F
      tryCatch({
        test_data_temp=getMovebankNonLocationData(study=study, animalName = animalName, ### Name of the bird
                                                   login=creds,timestamp_start=timestamp_start,timestamp_end=timestamp_end)
        test_data_new=rbind(test_data_new,test_data_temp)%>%unique.data.frame()
        print(paste("Accelaration data for",dates[i],"to",dates[i]+step_length,"downloaded",sep=" "))
      }, 
      error=function(e){continue <- TRUE})
      
      if (continue==T) {
        print(paste("Data for row",i," not downloaded"))
        next
      } 
    }
    
    if (nrow(test_data_new) > 0) {
      test_data_new=test_data_new%>%
        mutate(timestamp_IST=with_tz(as.POSIXct(timestamp),tzone="Asia/Calcutta"))%>%
        mutate(Date=as.Date(timestamp_IST))%>%
        filter(Date>=date_start)%>%filter(Date<=date_end)
    }
  }
  return(test_data_new)
}
