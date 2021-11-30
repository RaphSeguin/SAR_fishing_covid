#' Function to get the intersection between our observations and ZEE
#'
#' 
#' @param data Full observations from SAR images extracted from SUMO
#' 
#' @return DAta frame with observations only present in French Med ZEE
#'
#' @export
#' 



ZEE_intersection = function(SAR_data,ZEE_data){
  
  #Convert observations to sf object with same CRS as ZEE data
  SAR_data_sf = st_as_sf(SAR_data, crs = st_crs(ZEE_data),coords=c("lon","lat")) 
  
  #Intersection with our data and ZEE data
  SAR_data_ZEE = st_intersection(ZEE_data,SAR_data_sf)
  
  #save object
  save(SAR_data_ZEE,file="output/SAR_data_ZEE.Rdata")
  
  return(SAR_data_ZEE)
  
}