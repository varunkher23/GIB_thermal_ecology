library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(janitor)
library(tidyjson)
library(move)
library(move)
library(moveACC)
library(randomForest)
library(RColorBrewer)
source("Codes/Functions/retrieve_acurite_data.R")
source("Codes/Functions/retrieve_acceleration_data.R")
source("Codes/Functions/predict_behaviour_from_acceleration.R")

creds <-  movebankLogin(username = "Jhala",password = "Tigercell2018!")
birds=move::getMovebankAnimals(study = "GIB_Jhala_Dutta_Rajasthan_2019", login = creds)%>%
  dplyr::select(tag_local_identifier,local_identifier)%>%
  unique.data.frame()
rf_step2=read_rds("Models/rf_overall_fine_6class")



##### Retrieve acceleration data from movebank ####

acc_data=acceleration_data_from_movebank(study="GIB_Jhala_Dutta_Rajasthan_2019",
                                                animalName = "GIB 5949 Rkvy Out 310319", 
                                                date_start = as.Date("2022-06-15"),
                                                date_end=as.Date("2023-03-28"),step_length=16,
                                                creds=creds)%>%
  mutate(timestamp_round=round_date(timestamp_IST,unit = '5 min'))


activity_summary=predict_behaviour_from_accelerometer(test_data = acc_data,rf_model = rf_step2)

##### Retrieve weather data from acurite ####
weather_data=temperature_data_from_acurite_specific(dates = unique(acc_data$Date))%>%
  rename(timestamp_ws_IST=timestamp_IST)%>%
  mutate(UID=row_number())%>%
  mutate(timestamp_round=round_date(timestamp_ws_IST, unit = "5 min"))%>%
  unique.data.frame()

data_combined=acc_data%>%left_join(weather_data)%>%left_join(activity_summary)%>%unique.data.frame()%>%
  filter(is.na(temperature_raw_values_C)==F)

dates_to_keep=data_combined%>%mutate(hour=hour(timestamp_IST))%>%group_by(Date)%>%
  summarise(n_hrs=length(unique(hour)))%>%ungroup()%>%
  filter(n_hrs>4)

data_combined%>%
  group_by(Date,timestamp_IST,event_id)%>%
  summarise(rf_Fine_Beh=unique(rf_Fine_Beh),temperature_raw_values_C=mean(temperature_raw_values_C))%>%
  filter(Date %in% dates_to_keep$Date)%>%
  unique.data.frame()%>%
  mutate(activity=ifelse(rf_Fine_Beh =="Standing" | rf_Fine_Beh == "Sitting", as.numeric(0),as.numeric(1)))%>%
  mutate(hour=hour(timestamp_IST))%>%
  filter(hour > 5 | hour < 20)%>%
  mutate(month=month(timestamp_IST))%>%
  mutate(Season=ifelse(month < 4 | month > 10, as.character("Winter"),as.character("Summer")))%>%
  group_by(Season,Date,hour)%>%
  summarise(activity_mean=sum(activity)/length(event_id),temperature_mean=mean(temperature_raw_values_C, na.rm=T),
            n=length(event_id))%>%
  filter(n>14)%>%
  ggplot(aes(x=temperature_mean,y=activity_mean,group=Date))+
  geom_point(aes(colour = Season),alpha=1/3)+
  geom_line(aes(x=temperature_mean,y=activity_mean,group=Date,color=Season),stat="smooth",
            alpha=0.9,method = "lm",inherit.aes = F, se=F,linetype="dashed")+
  scale_y_continuous(limits = c(0,1))+
  theme_minimal()+
  geom_smooth(aes(x=temperature_mean,y=activity_mean, colour=Season),se=T,inherit.aes = F)+
  geom_smooth(aes(x=temperature_mean,y=activity_mean),se=T,inherit.aes = F)+
  ggtitle("Activity against mean hourly temperature")