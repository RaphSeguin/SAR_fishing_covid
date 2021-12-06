#' Figure of study area
#'
#' 
#' @param SAR_data_noRFI output from filter_RFI function
#' 
#' @return In the figures/RFI folder, plot showing how much observations we lose according to ships thresholds, and also map iwth/without shipping routes
#'
#' @export
#' 



shipping_plot = function(SAR_data_clean){
  
  SAR_plot = SAR_data_clean %>%
    filter(image_name == "S1A_IW_GRDH_1SDV_20201112T054445_20201112T054510_035211_041C76_1163.SAFE")
  
  ggplot(map_background) +
    geom_sf(fill="white" ) +
    # geom_sf(data = med_mpa,fill = "lightblue", alpha = 0.7) + 
    geom_sf(data=SAR_plot, size = 2, alpha = 0.7, color = "darkred")+
    theme(panel.background = element_rect(fill = "black"),panel.grid = element_line(color = "black", size = 0),
          panel.border = element_rect(colour = "#FEEFDB", fill=NA, size=1)) +
    xlim(2,11) +
    ylim(41,44)
  
  ggsave("figures/rfi_image_example.png",width = 297, height = 210, units = "mm")
  ggsave("figures/rfi_image_example.pdf",width = 297, height = 210, units = "mm")
  
  
}