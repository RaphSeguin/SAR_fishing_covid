#' Plot every image we have to check for RFI
#'
#' 
#' @param data  Output from the 4_filter_RFI
#' 
#' @return Dataframe with SAR observations but without shipping routes
#'
#' @export
#' 



filter_shipping = function(SAR_data_noRFI){
  
  #DElete images where number of boats detected is higher than 99% of other images and delete shipping routes
  raster_sf = raster::extract(shipping_routes, SAR_data_noRFI, df = T, weights = F)
  
  
  SAR_data_final = cbind(SAR_data_noRFI, raster_sf) %>% filter(shipping < 10)
  
  
  SAR_data_final = SAR_data_final %>%
    filter(ObsImg < quantile(SAR_data_noRFI$ObsImg,0.99))

  
  return(SAR_data_final)
  
  
}