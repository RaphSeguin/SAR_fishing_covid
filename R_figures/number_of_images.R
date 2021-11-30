#' Figure of the number of images per year and per month
#'
#' 
#' @param The output from the 5_filter_shippinh function, SAR_data_final
#' 
#' @return In the figures folder, plot of the number of images per year and month
#'
#' @export
#' 



number_of_images = function(SAR_data_final){

  #Number of images per year
  ggplot(SAR_data_final,aes(Year,ImgYear)) +
    geom_point(size = 4,aes(color=Year)) +
    theme_minimal()+
    scale_color_viridis_d() +
    labs(x = "Year",
         y = "Number of images taken")+
    ylim(0,600)
  
  ggsave(file="figures/ImagesPerYear.png",width = 297, height = 210,units="mm")
  ggsave(file="figures/ImagesPerYear.pdf",width = 297, height = 210,units="mm")
  
  #Number of images per month
  ggplot(data_final,aes(Month,ImgMonth)) +
    geom_point(size = 4,aes(color=Year)) +
    theme_minimal()+
    scale_color_viridis_d() +
    labs(x = "Month",
         y = "Number of images taken") + 
    facet_wrap(~Year) +
    ylim(0,80)
  
  ggsave(file="figures/ImagesPerMonth.pdf",width = 297, height = 210,units="mm")
  ggsave(file="figures/ImagesPerMonth.png",width = 297, height = 210,units="mm")
  
  
}