#' PGet observations inside MPAs and MPA buffers
#'
#' 
#' @param data  Output from the filter_shipping function
#' 
#' @return A dataframe with observartions inside mpas and inside mpa buffers
#'
#' @export
#' 



SAR_inside_mpa = function(SAR_data_final){
  
#Convert observations to sf object with same CRS as ZEE data
SAR_data_final_mpa = SAR_data_final %>%
  mutate(uniqueid = paste0(target_number,detecttime,Day,significance))%>%
  distinct(uniqueid, .keep_all = T)

#Keeping only IUCN MPA
med_mpa_filter = med_mpa %>%
  filter(IUCN_CA != "Not Applicable" & IUCN_CA != "Not Assigned" & IUCN_CA != "Not Reported")

#Creating mpa buffers
med_mpa_filter_5 = st_transform(med_mpa_filter, crs = 27700 ) %>% st_buffer(dist = 5000, units ="meters") %>% st_transform(crs = 4326)
med_mpa_filter_10 = st_transform(med_mpa_filter, crs = 27700 ) %>% st_buffer(dist = 10000, units ="meters") %>% st_transform(crs = 4326)

SAR_mpa_sf = st_as_sf(SAR_data_final_mpa, crs = st_crs(med_mpa),coords=c("lon","lat")) 

#Intersection between our data and mpa, mpa buffers
SAR_mpa = st_intersection(SAR_mpa_sf,med_mpa_filter)
SAR_mpa_5 = st_intersection(SAR_mpa_sf,med_mpa_filter_5)
SAR_mpa_10 = st_intersection(SAR_mpa_sf,med_mpa_filter_10)

#Keeping only unique values to avoid duplicates, for instances points that are in overlapping mpas 
SAR_mpa = SAR_mpa %>% distinct(uniqueid, .keep_all = T) %>% mutate(Status = "Inside_mpa")
SAR_mpa_5 = SAR_mpa_5 %>% distinct(uniqueid, .keep_all = T) %>% filter(!uniqueid %in% SAR_mpa$uniqueid) %>% mutate(Status = "5km_mpa")
SAR_mpa_10 = SAR_mpa_10 %>% distinct(uniqueid, .keep_all = T) %>% filter(!uniqueid %in% SAR_mpa$uniqueid) %>% filter(!uniqueid %in% SAR_mpa_5$uniqueid) %>%  mutate(Status = "10km_mpa")

SAR_mpa_final = rbind(SAR_mpa,SAR_mpa_5,SAR_mpa_10) %>% mutate(Status = as.factor(Status))
save(SAR_mpa_final, file = "output/SAR_mpa_final.Rdata")

}