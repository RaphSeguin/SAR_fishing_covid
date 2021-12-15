#' Function to plot the difference between 2020 and the other years
#'
#' 
#' @param data SAR_data_2017 to SAR_data_2020
#' 
#' @return Plot of the delta for all years
#'
#' @export
#' 
#' 


plot_delta_year = function(SAR_data_2017,SAR_data_2018,SAR_data_2019,SAR_data_2020){

SAR_mean_year = rbind(SAR_data_2017,SAR_data_2018,SAR_data_2019) %>% group_by(Year) %>% summarize(total = n()) %>% ungroup() %>% st_drop_geometry()

SAR_mean_year_2020 = SAR_data_2020 %>% group_by(Year) %>% summarize(sum = n()) %>%ungroup() %>% st_drop_geometry()

SAR_mean_year_plot = data.frame(Year = "2017-2019",sum = mean(SAR_mean_year$total))
SAR_year_plot = rbind(SAR_mean_year_plot, SAR_mean_year_2020)

delta_year = ggplot(SAR_year_plot,aes(Year,sum, fill = Year))+
  geom_bar(stat='identity') +
  scale_fill_viridis_d(option = "E") +
  theme_minimal() +
  labs (x = "",
        y = "Total observations") +
  ylim(0, 30000)

ggsave(delta_year, file = "figures/delta_year.pdf", width = 297, height = 210, units = "mm")

}