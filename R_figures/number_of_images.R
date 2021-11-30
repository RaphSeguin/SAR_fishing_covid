#' Figure of the number of images per year and per month
#'
#' 
#' @param The output from the 5_filter_shippinh function, SAR_data_final
#' 
#' @return In the figures folder, plot of the number of images per year and month
#'
#' @export
#' 



number_of_images = function(SAR_data_final,SAR_data_2017,SAR_data_2018,SAR_data_2019,SAR_data_2020){

  #Number of images per year
  images_year = ggplot(SAR_data_final,aes(Year,ImgYear)) +
    geom_point(size = 4,aes(color=Year)) +
    theme_minimal()+
    scale_color_viridis_d() +
    labs(x = "Year",
         y = "Number of images taken",
         color = "Year")+
    ylim(0,600)
  
  #Number of images per month
  (images_month = ggplot(SAR_data_final,aes(Month,ImgMonth)) +
    geom_point(size = 4,aes(color=Year)) +
    scale_color_viridis_d() +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5)) +
    labs(x = "Month",
         y = "Number of images taken",
         color = "Month") + 
    facet_wrap(~Year, ncol = 4) +
    theme(strip.text.x = element_blank())+
    ylim(0,80))
  
  figure = ggarrange(images_year + rremove("ylab") + rremove("xlab"), images_month + rremove("ylab") + rremove("xlab"), nrow = 2, common.legend = T)
  figure = annotate_figure(figure, left = textGrob("Number of images", rot = 90, vjust = 1, gp = gpar(cex = 1.3)))
  
  ggsave(figure, file="figures/number_of_images.pdf",width = 297, height = 210,units="mm")
  ggsave(figure, file="figures/number_of_images.png",width = 297, height = 210,units="mm")
  
}