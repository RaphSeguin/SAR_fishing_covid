#' Figure of study area
#'
#' 
#' @param SAR_data_noRFI output from filter_RFI function
#' 
#' @return In the figures/RFI folder, plot showing how much observations we lose according to ships thresholds, and also map iwth/without shipping routes
#'
#' @export
#' 



shipping_plot = function(SAR_data_noRFI){
  
  #Plot to show the threshold needed for ships
  raster_sf = raster::extract(shipping_routes, SAR_data_noRFI, df = T, weights = F)
  
  output = data.frame()
  
  for(i in 1:50){
    
    temp_forloop = cbind(SAR_data_noRFI, raster_sf) %>% filter(shipping < i)
    
    num_ships= data.frame(num_ships = i, obs = nrow(temp_forloop))
    
    output = rbind(num_ships,output )
    
  }
  
  ggplot(output, aes(num_ships,obs)) +
    geom_point()+
    theme_minimal()+
    labs(x = "Density of ships per pixel chosen",
         y = "Number of observations left after filtering out shipping routes")+
    geom_vline(xintercept = 10)
  
  ggsave("figures/data_lost_ships.png",width = 297, height = 210, units = "mm")
  ggsave("figures/data_lost_ships.pdf",width = 297, height = 210, units = "mm")
  
  
  #Plot to show what the data looks like WITH shipping routes
  
  ggplot(map_background) +
    geom_sf(fill="white" ) +
    # geom_sf(data = med_mpa,fill = "lightblue", alpha = 0.7) + 
    geom_sf(data=SAR_data_noRFI, size = 1.5, alpha = 0.7, shape = ".", color = "darkred")+
    theme(panel.background = element_rect(fill = "black"),panel.grid = element_line(color = "black", size = 0),
          panel.border = element_rect(colour = "#FEEFDB", fill=NA, size=1)) +
    xlim(2,11) +
    ylim(41,44)
  
  ggsave("figures/data_with_shipping.png",width = 297, height = 210, units = "mm")
  ggsave("figures/data_with_shipping.pdf",width = 297, height = 210, units = "mm")
  
  
  
  #Plot to show what the data looks like WITHOUT shipping routes
  temp = filter_shipping(SAR_data_noRFI)
  
  ggplot(map_background) +
    geom_sf(fill="white" ) +
    # geom_sf(data = med_mpa,fill = "lightblue", alpha = 0.7) + 
    geom_sf(data=temp, size = 1.5, alpha = 0.7, shape = ".", color = "darkred")+
    theme(panel.background = element_rect(fill = "black"),panel.grid = element_line(color = "black", size = 0),
          panel.border = element_rect(colour = "#FEEFDB", fill=NA, size=1)) +
    xlim(2,11) +
    ylim(41,44)
  
  
  ggsave("figures/data_without_shipping.png",width = 297, height = 210, units = "mm")
  ggsave("figures/data_without_shipping.pdf",width = 297, height = 210, units = "mm")
  
  
}