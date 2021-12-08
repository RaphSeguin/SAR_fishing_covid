#' Figure of the number of observations per year/month/day
#' 
#' @param data outputs from the sep_year function and from the 5_filter_shipping
#' 
#' @return In the figures folder, plot of the number of observations per year and month
#'
#' @export
#' 

number_of_observations_mpa = function(SAR_mpa_final, SAR_data_mpa_2017, SAR_data_mpa_2018, SAR_data_mpa_2019, SAR_data_mpa_2020){
  
  
  SAR_mpa_final$Status = factor(SAR_mpa_final$Status, levels=c("Inside_mpa","5km_mpa","10km_mpa"))
  SAR_data_mpa_2017$Status = factor(SAR_data_mpa_2017$Status, levels=c("Inside_mpa","5km_mpa","10km_mpa"))
  SAR_data_mpa_2018$Status = factor(SAR_data_mpa_2018$Status, levels=c("Inside_mpa","5km_mpa","10km_mpa"))
  SAR_data_mpa_2019$Status = factor(SAR_data_mpa_2019$Status, levels=c("Inside_mpa","5km_mpa","10km_mpa"))
  SAR_data_mpa_2020$Status = factor(SAR_data_mpa_2020$Status, levels=c("Inside_mpa","5km_mpa","10km_mpa"))
  
  group.colors = c("Inside_mpa" = "#5B1A18", "5km_mpa" = "#F66467","10km_mpa" = "#F1BB7B")
  
  #Number of observations per year
  (pYear = ggplot(SAR_mpa_final, aes(Year, fill = Status)) + 
     geom_bar(position = position_dodge()) +
     scale_fill_manual(values = group.colors, labels = c("Inside MPA","5km around MPA", "10km around MPA")) +
     theme_minimal() + 
     labs(x = " ",
          y = "Number of observations",
          fill = "Status"))
  
  #Number of observations per month
  (pMonth_2017 = ggplot(SAR_data_mpa_2017, aes(Month,fill=Status)) +
      geom_bar(position = position_dodge())+
      scale_fill_manual(values = group.colors, labels = c("Inside MPA","5km around MPA", "10km around MPA")) +
      theme_minimal()+
      theme(axis.text.x = element_blank()) +
      labs(x = " ",
           y = "Number of observations",
           title = "2017")+
      ylim(0,2000))
  
  (pMonth_2018 = ggplot(SAR_data_mpa_2018, aes(Month,fill=Status)) +
      geom_bar(position = position_dodge())+
      scale_fill_manual(values = group.colors, labels = c("Inside MPA","5km around MPA", "10km around MPA")) +
      theme_minimal()+
      theme(axis.text.x = element_blank()) +
      labs(x = " ",
           y = "Number of observations",
           title = "2018")++
      ylim(0,2000))
  
  
  (pMonth_2019 = ggplot(SAR_data_mpa_2019, aes(Month,fill=Status)) +
      geom_bar(position = position_dodge())+
      scale_fill_manual(values = group.colors, labels = c("Inside MPA","5km around MPA", "10km around MPA")) +
      theme_minimal()+
      theme(axis.text.x = element_blank()) +
      labs(x = " ",
           y = "Number of observations",
           title = "2019")+
      ylim(0,2000))
  
  
  (pMonth_2020 = ggplot(SAR_data_mpa_2020, aes(Month,fill=Status)) +
      geom_bar(position = position_dodge())+
      scale_fill_manual(values = group.colors, labels = c("Inside MPA","5km around MPA", "10km around MPA")) +
      theme_minimal()+
      theme(axis.text.x = element_blank()) +
      labs(x = " ",
           y = "Number of observations",
           title = "2020")+
      ylim(0,2000))
  
  
  # #Number of observations per days
  # (pDay_2017 = SAR_data_mpa_2017 %>%
  #     group_by(Status) %>%
  #     distinct(ObsDay, .keep_all = T) %>%
  #     ggplot(aes(Month, ObsDay, fill = Status), position = position_dodge(width = 3)) +
  #     geom_boxplot(alpha = 0.8) + 
  #     scale_fill_manual(values = group.colors, labels = c("Inside MPA","5km around MPA", "10km around MPA")) +
  #     theme_minimal()+
  #     theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5)) +
  #     labs(x = " ",
  #          y = "Number of observations per day",
  #          title = '2017') +
  #     ylim(0, 800))
  # 
  # (pDay_2018 = SAR_data_mpa_2018 %>%
  #     group_by(Status) %>%
  #     distinct(ObsDay, .keep_all = T) %>%
  #     ggplot(aes(Month, ObsDay, fill = Status), position = position_dodge(width = 3)) +
  #     geom_boxplot(alpha = 0.8) + 
  #     scale_fill_manual(values = group.colors, labels = c("Inside MPA","5km around MPA", "10km around MPA")) +
  #     theme_minimal()+
  #     theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5)) +
  #     labs(x = " ",
  #          y = "Number of observations per day",
  #          title = '2018') +
  #     ylim(0, 800))
  # 
  # (pDay_2019 =
  #     SAR_data_mpa_2019 %>%
  #     group_by(Status) %>%
  #     distinct(ObsDay, .keep_all = T) %>%
  #     ggplot(aes(Month, ObsDay, fill = Status), position = position_dodge(width = 3)) +
  #     geom_boxplot(alpha = 0.8) + 
  #     scale_fill_manual(values = group.colors, labels = c("Inside MPA","5km around MPA", "10km around MPA")) +
  #     theme_minimal()+
  #     theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5)) +
  #     labs(x = " ",
  #          y = "Number of observations per day",
  #          title = '2019') +
  #     ylim(0, 800))
  # 
  # (pDay_2020 = SAR_data_mpa_2020 %>%
  #     group_by(Status) %>%
  #     distinct(ObsDay, .keep_all = T) %>%
  #     ggplot(aes(Month, ObsDay, fill = Status), position = position_dodge(width = 3)) +
  #     geom_boxplot(alpha = 0.8) + 
  #     scale_fill_manual(values = group.colors, labels = c("Inside MPA","5km around MPA", "10km around MPA")) +
  #     theme_minimal()+
  #     theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5)) +
  #     labs(x = " ",
  #          y = "Number of observations per day",
  #          title = '2020') +
  #     ylim(0, 800))
  # 
  #Number of observations per cycle
  # (pDay_2017 = SAR_data_2017 %>%
  #     distinct(ObsCycle, .keep_all = T) %>%
  #     ggplot(aes(Month, ObsCycle, fill = Year)) +
  #     geom_boxplot(alpha = 0.5)+
  #     scale_fill_manual(values = group.colors) + 
  #     scale_color_manual(values = group.colors) + 
  #     theme_minimal()+
  #     theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5)) +
  #     labs(x = " ",
  #          y = "Number of observations per day") +
  #     guides(fill = "none",color="none")+
  #    ylim(0, 2600))
  # 
  # 
  # (pDay_2018 = SAR_data_2018 %>% 
  #     distinct(ObsCycle, .keep_all = T) %>%
  #     ggplot(aes(Month, ObsCycle, fill = Year)) +
  #     geom_boxplot(alpha = 0.5)+
  #     scale_fill_manual(values = group.colors) +
  #     scale_color_manual(values = group.colors) +
  #     theme_minimal()+
  #     theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5)) +
  #     labs(x = " ",
  #          y = "Number of observations per day") +
  #     guides(fill = "none",color="none") +
  #     ylim(0, 2600))
  # 
  # (pDay_2019 = SAR_data_2019 %>% 
  #     distinct(ObsCycle, .keep_all = T) %>%
  #     ggplot(aes(Month, ObsCycle, fill = Year)) +
  #     geom_boxplot(alpha = 0.5)+
  #     scale_fill_manual(values = group.colors) +
  #     scale_color_manual(values = group.colors) +
  #     theme_minimal()+
  #     theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5)) +
  #     labs(x = " ",
  #          y = "Number of observations per day") +
  #     guides(fill = "none",color="none") +
  #     ylim(0, 2600))
  # 
  # 
  # (pDay_2020 = SAR_data_2020 %>%
  #     distinct(ObsCycle, .keep_all = T) %>%
  #     ggplot(aes(Month, ObsCycle, fill = Year)) +
  #     geom_boxplot(alpha = 0.5)+
  #     scale_fill_manual(values = group.colors) +
  #     scale_color_manual(values = group.colors) +
  #     theme_minimal()+
  #     theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5)) +
  #     labs(x = " ",
  #          y = "Number of observations per day") +
  #     guides(fill = "none",color="none") +
  #     ylim(0, 2600))
  
  
  (month = ggarrange(pMonth_2017 + rremove("ylab"), pMonth_2018 + rremove("ylab"), pMonth_2019 + rremove("ylab"), pMonth_2020 + rremove("ylab"), nrow = 1, common.legend = T))
  # (day = ggarrange(pDay_2017 + rremove("ylab"), pDay_2018 + rremove("ylab"), pDay_2019 + rremove("ylab"), pDay_2020 + rremove("ylab"), nrow = 1, common.legend = T))
  
  figure = ggarrange(pYear + rremove("ylab"), month + rremove("xlab"), nrow = 2, heights = c(1,2), common.legend = T)
  (figure = annotate_figure(figure, left = textGrob("Number of observations", rot = 90, vjust = 0.7, gp = gpar(cex = 1.3))))
  
  
  ggsave(figure,file='figures/number_of_observations.pdf',width = 297, height = 210, units = "mm") 
  ggsave(figure,file='figures/number_of_observations.png',width = 297, height = 210, units = "mm") 
  
  
}