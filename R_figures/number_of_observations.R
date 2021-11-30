#' Figure of the number of observations per year/month/day
#' 
#' @param data outputs from the sep_year function and from the 5_filter_shipping
#' 
#' @return In the figures folder, plot of the number of observations per year and month
#'
#' @export
#' 

number_of_images = function(SAR_data_final, SAR_data_2017, SAR_data_2018, SAR_data_2019, SAR_data_2020){
  
  #Number of observations per year
  pYear = ggplot(SAR_data_final, aes(Year, fill = Year)) + 
    geom_bar() +
    scale_fill_viridis_d() +
    theme_minimal() + 
    labs(x = "Year",
        y = "Number of observations") +
    guides(fill = "none")
  
  #Number of observations per month
  (pMonth_2017 = ggplot(SAR_data_2017, aes(Month,fill=Month)) +
     geom_bar()+
     scale_fill_viridis_d() +
     theme_minimal()+
     labs(x = "Month",
          y = "Number of observations",
          title = "2017") +
     ylim(0,7000)+
     guides(fill = "none"))
  
  (pMonth_2018 = ggplot(SAR_data_2018, aes(Month,fill=Month)) +
      geom_bar()+
      scale_fill_viridis_d() +
      theme_minimal()+
      labs(x = "Month",
           y = "Number of observations",
           title = "2018") +
      ylim(0,7000)+guides(fill = "none"))

  (pMonth_2019 = ggplot(SAR_data_2019, aes(Month,fill=Month)) +
      geom_bar()+
      scale_fill_viridis_d() +
      theme_minimal()+
      labs(x = "Month",
           y = "Number of observations",
           title = "2019") +
      ylim(0,7000)+guides(fill = "none"))
  
  (pMonth_2020 = ggplot(SAR_data_2020, aes(Month,fill=Month)) +
      geom_bar()+
      scale_fill_viridis_d() +
      theme_minimal()+
      labs(x = "Month",
           y = "Number of observations",
           title = "2020") +
      ylim(0,7000)+guides(fill = "none"))
  
  #Number of observations per day 
  (pDay_2017 = ggplot(SAR_data_2017, aes(Month, ObsDay, fill = Month)) +
    geom_boxplot()+
    geom_jitter(aes(color = Month))+
    theme_minimal()+
    labs(x = "Month",
         y = "Number of observations per day",
         title = "2017") +
    guides(fill = "none"))
  
  pMonth = ggarrange(pMonth_2017,pMonth_2018,pMonth_2019,pMonth_2020,common.legend = T,nrow=2,ncol=2)
  
library(beepr)
beep(sound = 4)
  
  
  
ggsave(pMonth,file='figures/frequentationmonth.pdf',width = 20, height = 15) 
ggsave(pMonth,file='figures/frequentationmonth.png',width = 20, height = 15) 
  
  
}