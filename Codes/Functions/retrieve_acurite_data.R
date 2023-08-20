temperature_data_from_acurite=function(date_start,date_end){
  library(jsonlite)
  library(tidyjson)
  library(httr)
  library(janitor)
  library(tidyverse)
  library(lubridate)
  
  dates_all=seq.Date(from=as.Date(date_start)- 1,to=as.Date(date_end),by = 1)
  weather_data_new=data.frame()
  weather_data_temp=data.frame()
  for (i in 1:length(dates_all)) {
    date = dates_all[i]
    rm(weather_data_temp)
    URL=paste("https://dataapi.myacurite.com/mar-sensor-readings/24C86E0C3772-00000440-5in1WS/5m-summaries/",date,".json",sep="")
    res = GET(URL)
    continue=F
    tryCatch({
      weather_data_temp = fromJSON(rawToChar(res$content))%>%
        set_names(c("temperature","rain","accumulated_rain","wind_average","humidity", "wind","wind_direction","feels_like","dew_point","heat_index","wind_chill","pressure"))%>%
        as.data.frame()%>%clean_names()%>%
        rename(timestamp_UTC=temperature_happened_at)%>%
        dplyr::select(-contains("happened_at"))%>%
        mutate(timestamp_UTC=as.POSIXct(str_replace(timestamp_UTC,pattern = "T",replacement = " "),"UTC"))%>%
        mutate(timestamp_IST=with_tz(timestamp_UTC,"Asia/Calcutta"))%>%
        unnest(names_sep = "_")
      weather_data_new=rbind(weather_data_new,weather_data_temp)
      print(paste("Temperature data for",dates_all[i],"downloaded",sep=" "))
    }, 
    error=function(e){continue <- TRUE})
    
    if (continue==T) {
      print(paste("Temperature data for",dates_all[i],"not downloaded",sep=" "))
      next
    } 
  } ### For loop close
  weather_data_new=weather_data_new%>%
    filter(with_tz(timestamp_IST,"Asia/Calcutta")>with_tz(date_start,"Asia/Calcutta"))%>%
    filter(with_tz(timestamp_IST,"Asia/Calcutta") <= with_tz(date_end,"Asia/Calcutta")+86400)
  return(weather_data_new)
}  

temperature_data_from_acurite_specific=function(dates){
  library(jsonlite)
  library(tidyjson)
  library(httr)
  library(janitor)
  library(tidyverse)
  library(lubridate)
  
  dates_all=dates
  weather_data_new=data.frame()
  weather_data_temp=data.frame()
  for (i in 1:length(dates_all)) {
    date = dates_all[i]
    rm(weather_data_temp)
    URL=paste("https://dataapi.myacurite.com/mar-sensor-readings/24C86E0C3772-00000440-5in1WS/5m-summaries/",date,".json",sep="")
    res = GET(URL)
    continue=F
    tryCatch({
      weather_data_temp = fromJSON(rawToChar(res$content))%>%
        set_names(c("temperature","rain","accumulated_rain","wind_average","humidity", "wind","wind_direction","feels_like","dew_point","heat_index","wind_chill","pressure"))%>%
        as.data.frame()%>%clean_names()%>%
        rename(timestamp_UTC=temperature_happened_at)%>%
        dplyr::select(-contains("happened_at"))%>%
        mutate(timestamp_UTC=as.POSIXct(str_replace(timestamp_UTC,pattern = "T",replacement = " "),"UTC"))%>%
        mutate(timestamp_IST=with_tz(timestamp_UTC,"Asia/Calcutta"))%>%
        unnest(names_sep = "_")
      weather_data_new=rbind(weather_data_new,weather_data_temp)
      print(paste("Temperature data for",dates_all[i],"downloaded",sep=" "))
    }, 
    error=function(e){continue <- TRUE})
    
    if (continue==T) {
      print(paste("Temperature data for",dates_all[i],"not downloaded",sep=" "))
      next
    } 
  } ### For loop close
  return(weather_data_new)
}  
