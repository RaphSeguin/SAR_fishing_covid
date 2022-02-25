#' Function to plot the difference between 2020 and the other years per month
#'
#' 
#' @param data SAR_data_2017 to SAR_data_2020
#' 
#' @return Plot of the delta for all years per month
#'
#' @export
#' 
#' 

plot_delta_month = function(SAR_data_2017,SAR_data_2018,SAR_data_2019,SAR_data_2020,
                            SAR_morning_2017,SAR_morning_2018,SAR_morning_2019,SAR_morning_2020,
                            SAR_evening_2017,SAR_evening_2018,SAR_evening_2019,SAR_evening_2020){

SAR_mean = rbind(SAR_data_2017,SAR_data_2018,SAR_data_2019) %>% group_by(Month) %>% mutate(monthmean = mean(ObsMonth)) %>% dplyr::select(Month, monthmean) %>% distinct(monthmean, .keep_all = T) %>%
  st_drop_geometry() %>%
  left_join(SAR_data_2020, by = "Month") %>%
  mutate(diff = ObsMonth - monthmean) %>% distinct(diff, .keep_all = T)

all = ggplot(SAR_mean, aes(Month,diff))+
  geom_bar(stat="identity", aes (fill = Month)) +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(x = "",
       y="",
       size = "Delta")+
  ylim(-1700, 800)

SAR_mean_morning = rbind(SAR_morning_2017,SAR_morning_2018,SAR_morning_2019) %>% group_by(Month) %>% mutate(monthmean = mean(ObsMonth)) %>% dplyr::select(Month, monthmean) %>% distinct(monthmean, .keep_all = T) %>% 
  st_drop_geometry() %>%
  left_join(SAR_morning_2020, by = "Month") %>%
  mutate(diff = ObsMonth - monthmean) %>% distinct(diff, .keep_all = T)

morning = ggplot(SAR_mean_morning, aes(Month,diff))+
  geom_bar(stat="identity", aes (fill = Month)) +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(x = "",
       y="",
       size = "Delta",
       title = "Morning") +
  guides(fill = "none") +
  ylim(-1700, 800)

SAR_mean_evening = rbind(SAR_evening_2017,SAR_evening_2018,SAR_evening_2019) %>% group_by(Month) %>% mutate(monthmean = mean(ObsMonth)) %>% dplyr::select(Month, monthmean) %>% distinct(monthmean, .keep_all = T) %>% 
  st_drop_geometry() %>%
  left_join(SAR_evening_2020, by = "Month") %>%
  mutate(diff = ObsMonth - monthmean) %>% distinct(diff, .keep_all = T)

evening = ggplot(SAR_mean_evening, aes(Month,diff))+
  geom_bar(stat="identity", aes (fill = Month)) +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(x = "",
       y="",
       size = "Delta",
       title = 'Evening') +
  guides(fill = "none")+
  ylim(-1700, 800)


morningevening = ggarrange(morning, evening, nrow = 1, common.legend = T)   
figure = ggarrange(all, morningevening, nrow = 2, common.legend=T)
(figure = annotate_figure(figure, left = textGrob("Difference between monthly observations in 2020 and mean monthly observations in 2017-2019", rot = 90, vjust = 0.9, gp = gpar(cex = 1))))
ggsave(figure, file = "figures/delta_difference.pdf", width = 297, height = 210, units = "mm")

}
