#' Figure of study area
#'
#' 
#' @param map_background Background map of Med
#' @param ZEE dataset of french ZEE
#' 
#' @return In the figures/RFI folder, plot from every image
#'
#' @export
#' 



study_area = function(map_background,ZEE){
  
  #PLOT OF STUDY AREA
  ggplot(map_background) +
    geom_sf(fill="white" ) +
    geom_sf(data = ZEE,fill = "lightblue", alpha = 0.7) + 
    theme(panel.background = element_rect(fill = "black"),panel.grid = element_line(color = "black", size = 0),
          panel.border = element_rect(colour = "#FEEFDB", fill=NA, size=1)) +
    xlim(2,11) +
    ylim(41,44)
  
  ggsave("figures/study_area.png",width = 297, height = 210, units = "mm")
  ggsave("figures/study_area.pdf",width = 297, height = 210, units = "mm")
  
}