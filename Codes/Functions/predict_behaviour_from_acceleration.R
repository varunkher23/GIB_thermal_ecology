predict_behaviour_from_accelerometer=function(test_data,rf_model, return = "behaviour_only"){
  library(tidyverse)
  library(moveACC)
  library(randomForest)
  
  activity_summary=data.frame()
  dates=unique(test_data$Date)
  for (j in 1:length(dates)) {
    date_i=as.Date(dates[j])
    test_data_1 =  test_data%>%filter(Date == as.Date(date_i))
    sampling_schedule=BurstSamplingScedule(test_data_1)%>%
      mutate(sampling_duration_hrs= ((as.numeric(MedianSampleRateSecs) / as.numeric(burstDurationSecs))) *  ### Inverse of proportion sampled
               (as.numeric(numberOfBursts)/3600))%>%
      mutate(burstDurationSecs=as.numeric(as.character(burstDurationSecs) ))%>%
      mutate(sampling_proportion=(burstDurationSecs/as.numeric(MedianSampleRateSecs)))
    if (sampling_schedule$MedianSampleRateSecs < 300 & is.na(sampling_schedule$MedianSampleRateSecs)==F ) {
      sensitiv_test <- data.frame(TagID=c("8650","5949","5947","5948","8652"), sensitivity="low")%>%mutate(TagID=as.integer(TagID))
      transfDF_test <- TransformRawACC(test_data_1, sensitivity.settings=sensitiv_test, units="g")
      statsDF_test <- ACCstats(df=transfDF_test)%>%clean_names()%>%dplyr::select(-individual_id,-tag_id,-timestamp)
      test_data_1 = test_data_1%>%left_join(statsDF_test)%>%mutate(tag_local_identifier=as.integer(tag_local_identifier))
      test_data_1$rf_Fine_Beh=test_data_1%>%dplyr::select(avg_x:odba_median)%>%predict(rf_model,newdata=.)
      test_data_1$activity_time=sampling_schedule$MedianSampleRateSecs
      activity_summary=rbind(activity_summary,test_data_1)
    }
  }
  
  if (return == "full") {
    return(activity_summary)
  }else {
    return(dplyr::select(activity_summary,event_id,rf_Fine_Beh))
  }
}