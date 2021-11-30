#' Cleans up data of SAR observations, removing outliers false positives etc
#'
#' 
#' @param data  THe output from the 1_ZEE_intersection function
#' 
#' @return Dataframe with cleaned data
#'
#' @export
#' 



data_clean = function(SAR_data_ZEE){
  
  
  #General informations on data
  SAR_data_clean = SAR_data_ZEE %>%
    #Delete false positives
    filter(reliability != 3) %>%
    #Only ships length longer than 15
    filter(length > 15) %>%
    #General temporal info on data
    mutate(Year = as.factor(ifelse(str_detect(detecttime,"2020"),"2020",ifelse(str_detect(detecttime,"2019"),"2019",ifelse(str_detect(detecttime,"2018"),"2018",ifelse(str_detect(detecttime,"2017"),"2017",ifelse(str_detect(detecttime,"2021"),"2021","2016")))))),
           Month = as.factor(ifelse(str_detect(detecttime,"-03-"),"March",ifelse(str_detect(detecttime,"-04-"),"April",ifelse(str_detect(detecttime,"-05-"),"May",ifelse(str_detect(detecttime,"-06-"),"June",ifelse(str_detect(detecttime,"-07-"),"July",
                                                                                                                                                                                                                     ifelse(str_detect(detecttime,"-08-"),"August",ifelse(str_detect(detecttime,"-09-"),"September",ifelse(str_detect(detecttime,"-10-"),"October",ifelse(str_detect(detecttime,"-11-"),"November","February")))))))))),
           MonthNum = as.factor(ifelse(str_detect(detecttime,"-03-"),"03",ifelse(str_detect(detecttime,"-04-"),"04",ifelse(str_detect(detecttime,"-05-"),"05",ifelse(str_detect(detecttime,"-06-"),"06",ifelse(str_detect(detecttime,"-07-"),"07",
                                                                                                                                                                                                               ifelse(str_detect(detecttime,"-08-"),"08",ifelse(str_detect(detecttime,"-09-"),"09",ifelse(str_detect(detecttime,"-10-"),"10",ifelse(str_detect(detecttime,"-11-"),"11","02")))))))))),
           Day = as.factor(gsub( " .*$", "", detecttime))) %>%
    #Number of shots taken for each day
    group_by(Day) %>%
    mutate(Img = n_distinct(detecttime)) %>%
    ungroup() %>%
    #Number of shots taken each Month
    group_by(Year,Month) %>%
    mutate(ImgMonth = n_distinct(detecttime)) %>%
    ungroup() %>%
    #Number of shots taken each year
    group_by(Year) %>%
    mutate(ImgYear = n_distinct(detecttime)) %>%
    ungroup() %>%
    #And number of observations per day 
    group_by(Day) %>%
    mutate(ObsDay = n()) %>%
    ungroup() %>%
    #Number of observations per image
    group_by(detecttime) %>%
    mutate(ObsImg  = n()) %>%
    ungroup() %>%
    filter(Month != "February") %>%
    filter(Year != 2016) %>%
    filter(Year != 2021)
  
  #Remove outliers by deleting all days where number of observations is greater than 95% of observed values
  SAR_data_clean = SAR_data_clean %>% filter(ObsDay < quantile(SAR_data_clean$ObsDay,0.95))
  
  SAR_data_clean$Month = factor(SAR_data_clean$Month,levels = c("March","April","May","June","July","August","September","October","November"))
  SAR_data_clean$MonthNum = as.Date(paste0("2017-",SAR_data_clean$MonthNum,"-01"))
  
  save(SAR_data_clean,file="output/data_all.Rdata")
  
  return(SAR_data_clean)
  
  
}