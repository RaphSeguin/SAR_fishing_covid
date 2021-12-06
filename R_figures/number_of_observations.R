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
  (pYear = ggplot(SAR_data_final, aes(Year, fill = Year)) + 
    geom_bar() +
    scale_fill_viridis_d() +
    theme_minimal() + 
    labs(x = " ",
        y = "Number of observations"))
  
  #Color palette for Year viridis
  group.colors = c("2017" = "#440154", "2018" = "#31688e","2019" = "#35b779", "2020" = "#fde725")
  
  #Number of observations per month
  (pMonth_2017 = ggplot(SAR_data_2017, aes(Month,fill=Year)) +
     geom_bar()+
     scale_fill_manual(values = group.colors) +
    theme_minimal()+
    theme(axis.text.x = element_blank()) +
     labs(x = " ",
          y = "Number of observations")+
     guides(fill = "none") +
      ylim(0,7000))
  
  (pMonth_2018 = ggplot(SAR_data_2018, aes(Month,fill=Year)) +
      geom_bar()+
      scale_fill_manual(values = group.colors) +
      theme_minimal()+
      theme(axis.text.x = element_blank()) +
      labs(x = " ",
           y = "Number of observations") +
      ylim(0,7000)+guides(fill = "none"))

  (pMonth_2019 = ggplot(SAR_data_2019, aes(Month,fill=Year)) +
      geom_bar()+
      scale_fill_manual(values = group.colors) +
      theme_minimal()+
      theme(axis.text.x = element_blank()) +
      labs(x = " ",
           y = "Number of observations") +
      ylim(0,7000)+guides(fill = "none"))
  
  (pMonth_2020 = ggplot(SAR_data_2020, aes(Month,fill=Year)) +
      geom_bar()+
      scale_fill_manual(values = group.colors) +
      theme_minimal()+
      theme(axis.text.x = element_blank()) +
      labs(x = " ",
           y = "Number of observations")+
      ylim(0,7000)+guides(fill = "none"))
  
  
  # #Number of observations per day 
  # (pDay_2017 = ggplot(SAR_data_2017, aes(Month, ObsDay, fill = Year)) +
  #   geom_boxplot(alpha = 0.5)+
  #   geom_jitter(aes(color = Month), alpha = 0.4, size = 2, shape = ".")+
  #   scale_fill_manual(values = group.colors) + 
  #   scale_color_manual(values = group.colors) + 
  #   theme_minimal()+
  #   theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5)) +
  #   labs(x = " ",
  #        y = "Number of observations per day") +
  #   guides(fill = "none",color="none") +
  #   ylim(0, 800))
  # 
  # (pDay_2018 = ggplot(SAR_data_2018, aes(Month, ObsDay, fill = Year)) +
  #     geom_boxplot(alpha = 0.5)+
  #     geom_jitter(aes(color = Month), alpha = 0.4, size = 2, shape = ".")+
  #     scale_fill_manual(values = group.colors) + 
  #     scale_color_manual(values = group.colors) + 
  #     theme_minimal()+
  #     theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5)) +
  #     labs(x = " ",
  #          y = "Number of observations per day") +
  #     guides(fill = "none",color="none") +
  #     ylim(0, 800))
  # 
  # (pDay_2019 = ggplot(SAR_data_2019, aes(Month, ObsDay, fill = Year)) +
  #     geom_boxplot(alpha = 0.5)+
  #     geom_jitter(aes(color = Month), alpha = 0.4, size = 2, shape = ".")+
  #     scale_fill_manual(values = group.colors) + 
  #     scale_color_manual(values = group.colors) + 
  #     theme_minimal()+
  #     theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5)) +
  #     labs(x = " ",
  #          y = "Number of observations per day") +
  #     guides(fill = "none",color="none") +
  #     ylim(0, 800))
  # 
  # (pDay_2020 = ggplot(SAR_data_2020, aes(Month, ObsDay, fill = Year)) +
  #     geom_boxplot(alpha = 0.5)+
  #     geom_jitter(aes(color = Month), alpha = 0.4, size = 2, shape = ".")+
  #     scale_fill_manual(values = group.colors) + 
  #     scale_color_manual(values = group.colors) + 
  #     theme_minimal()+
  #     theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5)) +
  #     labs(x = " ",
  #          y = "Number of observations per day") +
  #     guides(fill = "none",color="none") +
  #     ylim(0, 800))
  # 
  #Number of observations per cycle
  (pDay_2017 = SAR_data_2017 %>%
      distinct(ObsCycle, .keep_all = T) %>%
      ggplot(aes(Month, ObsCycle, fill = Year)) +
      geom_boxplot(alpha = 0.5)+
      scale_fill_manual(values = group.colors) + 
      scale_color_manual(values = group.colors) + 
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5)) +
      labs(x = " ",
           y = "Number of observations per day") +
      guides(fill = "none",color="none")+
     ylim(0, 2600))

  
  (pDay_2018 = SAR_data_2018 %>% 
      distinct(ObsCycle, .keep_all = T) %>%
      ggplot(aes(Month, ObsCycle, fill = Year)) +
      geom_boxplot(alpha = 0.5)+
      scale_fill_manual(values = group.colors) +
      scale_color_manual(values = group.colors) +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5)) +
      labs(x = " ",
           y = "Number of observations per day") +
      guides(fill = "none",color="none") +
      ylim(0, 2600))

  (pDay_2019 = SAR_data_2019 %>% 
      distinct(ObsCycle, .keep_all = T) %>%
      ggplot(aes(Month, ObsCycle, fill = Year)) +
      geom_boxplot(alpha = 0.5)+
      scale_fill_manual(values = group.colors) +
      scale_color_manual(values = group.colors) +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5)) +
      labs(x = " ",
           y = "Number of observations per day") +
      guides(fill = "none",color="none") +
      ylim(0, 2600))
  

  (pDay_2020 = SAR_data_2020 %>%
      distinct(ObsCycle, .keep_all = T) %>%
      ggplot(aes(Month, ObsCycle, fill = Year)) +
      geom_boxplot(alpha = 0.5)+
      scale_fill_manual(values = group.colors) +
      scale_color_manual(values = group.colors) +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5)) +
      labs(x = " ",
           y = "Number of observations per day") +
      guides(fill = "none",color="none") +
      ylim(0, 2600))

  
  month = ggarrange(pMonth_2017 + rremove("ylab"), pMonth_2018 + rremove("ylab"), pMonth_2019 + rremove("ylab"), pMonth_2020 + rremove("ylab"), nrow = 1)
  day = ggarrange(pDay_2017 + rremove("ylab"), pDay_2018 + rremove("ylab"), pDay_2019 + rremove("ylab"), pDay_2020 + rremove("ylab"), nrow = 1)
    
  figure = ggarrange(pYear + rremove("ylab"), month + rremove("xlab"), day, nrow = 3, heights = c(1,1,2), common.legend = T)
  (figure = annotate_figure(figure, left = textGrob("Number of observations", rot = 90, vjust = 0.7, gp = gpar(cex = 1.3))))
  
  
  ggsave(figure,file='figures/number_of_observations.pdf',width = 297, height = 210, units = "mm") 
  ggsave(figure,file='figures/number_of_observations.png',width = 297, height = 210, units = "mm") 
  
  
}