#' Plot every image we have to check for RFI
#'
#' 
#' @param data  Output from the 2_data_clean function
#' 
#' @return In the figures/RFI folder, plot from every image
#'
#' @export
#' 



plot_all_images = function(SAR_data_clean){
  
  
  #Data in data frame format
  SAR_df = st_coordinates(SAR_data_clean)
  SAR_df = cbind(SAR_data_clean,SAR_df)
  
  #Plot every image we have
  mclapply(unique(SAR_df$image_name),function(i){
    
    ggplot() +
      theme_minimal() +
      # geom_sf(data = med_mpa,fill = "lightblue", alpha = 0.7) + 
      geom_point(data = filter(SAR_df,image_name == i),aes(X,Y))+
      xlim(2,11) +
      ylim(41,44) +
      labs(title = i)
    
    ggsave(file = paste0("figures/RFI/plot","_",i,".pdf"), width = 20, height = 15)
    
  }, mc.cores = 3)
  
  
}