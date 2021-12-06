#' One data set per year
#'
#' 
#' @param data  Output from the 5_filter_shipping
#' 
#' @return One data frame for each year
#'
#' @export
#' 


sep_year = function(SAR_data_final){
  
  #DATA PER YEAR
  SAR_data_2017 = SAR_data_final %>%
    filter(Year == "2017") %>%
    #Number of osbervations per month
    group_by(Month) %>%
    mutate(ObsMonth = n()) %>%
    ungroup() %>%
    group_by(cycle) %>%
    mutate(ObsCycle = n()) %>%
    ungroup()

  save(SAR_data_2017, file = "output/SAR_data_2017.Rdata")
  
  SAR_data_2018 = SAR_data_final %>%
    filter(Year == "2018") %>%
    #Number of osbervations per month
    group_by(Month) %>%
    mutate(ObsMonth = n()) %>%
    ungroup() %>%
    group_by(cycle) %>%
    mutate(ObsCycle = n()) %>%
    ungroup()
  
  save(SAR_data_2018, file = "output/SAR_data_2018.Rdata")
  
  SAR_data_2019 = SAR_data_final %>%
    filter(Year == "2019") %>%
    #Number of osbervations per month
    group_by(Month) %>%
    mutate(ObsMonth = n()) %>%
    ungroup() %>%
    group_by(cycle) %>%
    mutate(ObsCycle = n()) %>%
    ungroup()
  
  save(SAR_data_2019, file = "output/SAR_data_2019.Rdata")
  
  SAR_data_2020 = SAR_data_final %>%
    filter(Year == "2020") %>%
    #Number of osbervations per month
    group_by(Month) %>%
    mutate(ObsMonth = n()) %>%
    ungroup() %>%
    group_by(cycle) %>%
    mutate(ObsCycle = n()) %>%
    ungroup()
  
  save(SAR_data_2020, file = "output/SAR_data_2020.Rdata")
  
  

}




