#' Figure of study area
#'
#' 
#' @param map_background Background map of Med
#' @param ZEE dataset of french ZEE
#' 
#' @return In the figures/RFI folder, plot from the study
#'
#' @export
#' 



study_area = function(map_background,ZEE){

  France = st_read("data/NUTS_RG_20M_2021_4326.shp") %>%
    filter(LEVL_CODE == 0) %>%
    st_crop(xmin = -10, xmax = 20,
            ymin = 35, ymax = 48)
  
  area_plot = ggplot(France) +
    geom_sf(fill = "black")+
    theme(panel.background = element_rect(fill = "white"),panel.grid = element_line(color = "black", size = 0),
          panel.border = element_rect(colour = "#FEEFDB", fill=NA, size=1)) +
    # theme_map() +
    ggrepel::geom_label_repel(
      data = France,
      aes(label = NAME_LATN, geometry = geometry),
      stat = "sf_coordinates",
      min.segment.length = 0,
      colour = "black",
      segment.colour = "white"
    )+
    theme_map()
  
  ggsave(area_plot, file = "figures/area_plot.pdf", width = 297, height = 210, units = "mm")
  
  
  
  
  library(maptools)
  data(wrld_simpl)
  eur=wrld_simpl[wrld_simpl$AREA == 150, ]

  plot(France)
  
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