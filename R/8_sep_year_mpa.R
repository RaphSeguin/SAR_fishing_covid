#' One data set per year for MPA data
#'
#' 
#' @param data  Output from the 5_filter_shipping
#' 
#' @return One data frame for each year for mpa data
#'
#' @export
#' 


sep_year_mpa = function(SAR_mpa_final){
  
  #DATA PER YEAR
  SAR_data_mpa_2017 = SAR_mpa_final %>%
    filter(Year == "2017") %>%
    #Number of osbervations per month
    group_by(Month) %>%
    mutate(ObsMonth = n()) %>%
    ungroup() %>%
    group_by(cycle) %>%
    mutate(ObsCycle = n()) %>%
    ungroup()
  
  save(SAR_data_mpa_2017, file = "output/SAR_data_mpa_2017.Rdata")
  
  SAR_data_mpa_2018 = SAR_mpa_final %>%
    filter(Year == "2018") %>%
    #Number of osbervations per month
    group_by(Month) %>%
    mutate(ObsMonth = n()) %>%
    ungroup() %>%
    group_by(cycle) %>%
    mutate(ObsCycle = n()) %>%
    ungroup()
  
  save(SAR_data_mpa_2018, file = "output/SAR_data_mpa_2018.Rdata")
  
  SAR_data_mpa_2019 = SAR_mpa_final %>%
    filter(Year == "2019") %>%
    #Number of osbervations per month
    group_by(Month) %>%
    mutate(ObsMonth = n()) %>%
    ungroup() %>%
    group_by(cycle) %>%
    mutate(ObsCycle = n()) %>%
    ungroup()
  
  save(SAR_data_mpa_2019, file = "output/SAR_data_mpa_2019.Rdata")
  
  SAR_data_mpa_2020 = SAR_mpa_final %>%
    filter(Year == "2020") %>%
    #Number of osbervations per month
    group_by(Month) %>%
    mutate(ObsMonth = n()) %>%
    ungroup() %>%
    group_by(cycle) %>%
    mutate(ObsCycle = n()) %>%
    ungroup()
  
  save(SAR_data_mpa_2020, file = "output/SAR_data_mpa_2020.Rdata")
  
  
  
}




