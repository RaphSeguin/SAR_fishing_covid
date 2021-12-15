#' One data set per year
#'
#' 
#' @param data  Output from the 5_filter_shipping
#' 
#' @return One data frame for each year
#'
#' @export
#' 
#' 


sep_morning_evening = function(SAR_data_final,SAR_data_2017,SAR_data_2018,SAR_data_2019,SAR_data_2020){
  
SAR_data_final$Time = format(as.POSIXct(SAR_data_final$detecttime), format = "%H:%M:%S")
SAR_morning_final = SAR_data_final %>% filter(Time < "12:00:00")%>% group_by(Month) %>%
  mutate(ObsMonth = n()) %>%
  ungroup()
save(SAR_morning_final, file = "output/SAR_morning_final.Rdata")

SAR_data_2017$Time = format(as.POSIXct(SAR_data_2017$detecttime), format = "%H:%M:%S")
SAR_morning_2017 = SAR_data_2017 %>% filter(Time < "12:00:00")   %>% group_by(Month) %>%
  mutate(ObsMonth = n()) %>%
  ungroup()
save(SAR_morning_2017, file = "output/SAR_morning_2017.Rdata")
SAR_data_2018$Time = format(as.POSIXct(SAR_data_2018$detecttime), format = "%H:%M:%S")
SAR_morning_2018 = SAR_data_2018 %>% filter(Time < "12:00:00")%>% group_by(Month) %>%
  mutate(ObsMonth = n()) %>%
  ungroup()
save(SAR_morning_2018, file = "output/SAR_morning_2018.Rdata")
SAR_data_2019$Time = format(as.POSIXct(SAR_data_2019$detecttime), format = "%H:%M:%S")
SAR_morning_2019 = SAR_data_2019 %>% filter(Time < "12:00:00")%>% group_by(Month) %>%
  mutate(ObsMonth = n()) %>%
  ungroup()
save(SAR_morning_2019, file = "output/SAR_morning_2019.Rdata")
SAR_data_2020$Time = format(as.POSIXct(SAR_data_2020$detecttime), format = "%H:%M:%S")
SAR_morning_2020 = SAR_data_2020 %>% filter(Time < "12:00:00")%>% group_by(Month) %>%
  mutate(ObsMonth = n()) %>%
  ungroup()
save(SAR_morning_2020, file = "output/SAR_morning_2020.Rdata")


SAR_data_final$Time = format(as.POSIXct(SAR_data_final$detecttime), format = "%H:%M:%S")
SAR_evening_final = SAR_data_final %>% filter(Time > "12:00:00")%>% group_by(Month) %>%
  mutate(ObsMonth = n()) %>%
  ungroup()
save(SAR_evening_final, file = "output/SAR_evening_final.Rdata")

SAR_data_2017$Time = format(as.POSIXct(SAR_data_2017$detecttime), format = "%H:%M:%S")
SAR_evening_2017 = SAR_data_2017 %>% filter(Time > "12:00:00")%>% group_by(Month) %>%
  mutate(ObsMonth = n()) %>%
  ungroup()
save(SAR_evening_2017, file = "output/SAR_evening_2017.Rdata")
SAR_data_2018$Time = format(as.POSIXct(SAR_data_2018$detecttime), format = "%H:%M:%S")
SAR_evening_2018 = SAR_data_2018 %>% filter(Time > "12:00:00")%>% group_by(Month) %>%
  mutate(ObsMonth = n()) %>%
  ungroup()
save(SAR_evening_2018, file = "output/SAR_evening_2018.Rdata")
SAR_data_2019$Time = format(as.POSIXct(SAR_data_2019$detecttime), format = "%H:%M:%S")
SAR_evening_2019 = SAR_data_2019 %>% filter(Time > "12:00:00")%>% group_by(Month) %>%
  mutate(ObsMonth = n()) %>%
  ungroup()
save(SAR_evening_2019, file = "output/SAR_evening_2019.Rdata")
SAR_data_2020$Time = format(as.POSIXct(SAR_data_2020$detecttime), format = "%H:%M:%S")
SAR_evening_2020 = SAR_data_2020 %>% filter(Time > "12:00:00")%>% group_by(Month) %>%
  mutate(ObsMonth = n()) %>%
  ungroup()
save(SAR_evening_2020, file = "output/SAR_evening_2020.Rdata")

}
