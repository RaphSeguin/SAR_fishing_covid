#' Density maps for morning/evening comparison 2017-2019 in summer
#'
#' 
#' @param data  Output from the 5_filter_shipping
#' 
#' @return One data frame for each year
#'
#' @export
#' 
#' 

density_maps_summer = function(SAR_data_2017, SAR_data_2018, SAR_data_2019, SAR_data_2020, 
                               SAR_morning_2017, SAR_morning_2018, SAR_morning_2019, SAR_morning_2020,
                               SAR_evening_2017, SAR_evening_2018, SAR_evening_2019, SAR_evening_2020){  

#THIS IS FOR DATA FROM 2017 to 2019

  #Data in data frame format
SAR_density = rbind(SAR_data_2017,SAR_data_2018,SAR_data_2019) %>% filter(Month %in% c("July","August","September")) %>% st_coordinates()
SAR_plot = rbind(SAR_data_2017,SAR_data_2018,SAR_data_2019)%>% filter(Month %in% c("July","August","September")) %>% cbind(SAR_density) %>% st_drop_geometry()
  
Density_20172019_summer = ggplot() +
    geom_density2d(data = SAR_plot, aes(x = X, y = Y), bins = 20, size = 0.1, col = "black") +
    stat_density2d(data = SAR_plot, aes(x = X, y = Y, fill = ..level.., alpha = ..level..),
                   size = 0.01, bins = 20, geom = 'polygon') +
  geom_sf(data = land_map, fill = "darkgrey", color = "black", size = 0.05) +
    # geom_point(data = SAR_morning_plot, aes(x=  X, y = Y), color = "darkred", alpha = 0.7, size = 1, shape = ".")+
  scale_fill_viridis(option = "magma", direction=-1, breaks = c(0,0.2,0.4,0.6,0.8,1), limits = c(0,1),
                     name = "Detection density", 
                     guide = guide_colorbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       # some shifting around
                       title.hjust = 0.5,
                       label.hjust = 0.5
                     )) +
  scale_alpha(range = c(0.1, 0.9), guide = "none") +
  theme_map() + 
  theme(
    text = element_text(size = 12),   
    legend.position = c(0.43, -0.35),
    legend.text.align = 0,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
    plot.title = element_text(hjust = 0.5, color = "#4e4d47", size = 14),
    plot.margin = unit(c(.1,.1,.1,.1), "cm"),
    panel.spacing = unit(c(0.01,0.01,.01,0.01), "cm"),
    panel.border = element_blank())+
    xlim(2,11) +
    ylim(41,44)+
    labs(title = "Detections for 2017-2019 from July to September")
  
  # ggsave(Density_20172019_summer, file = "figures/Density_20172019_summer.pdf", width = 297, height = 210, units = "mm")

#ALL 2020 DATA  
  
SAR_2020_density = SAR_data_2020 %>% filter(Month %in% c("July","August","September")) %>% st_coordinates()
SAR_2020_plot = SAR_data_2020 %>% filter(Month %in% c("July","August","September")) %>% cbind(SAR_2020_density) %>% st_drop_geometry()
  
  
  #THIS IS FOR 2020
Density_2020_summer = ggplot() +
  geom_density2d(data = SAR_2020_plot, aes(x = X, y = Y), bins = 20, size = 0.1, col = "black") +
    stat_density2d(data = SAR_2020_plot, aes(x = X, y = Y, fill = ..level.., alpha = ..level..),
                   size = 0.01, bins = 20, geom = 'polygon') +
  geom_sf(data = land_map, fill = "darkgrey", color = "black", size = 0.05) +
    # geom_point(data = SAR_morning_plot, aes(x=  X, y = Y), color = "darkred", alpha = 0.7, size = 1, shape = ".")+
  scale_fill_viridis(option = "magma", direction=-1, breaks = c(0,0.2,0.4,0.6,0.8,1), limits = c(0,1),
                     name = "Detection density", 
                     guide = "none") +
  scale_alpha(range = c(0.1, 0.9), guide = "none") +
  theme_map() + 
  theme(
    text = element_text(size = 12),   
    legend.text.align = 0,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
    plot.title = element_text(hjust = 0.5, color = "#4e4d47", size = 14),
    plot.margin = unit(c(.1,.1,.1,.1), "cm"),
    panel.spacing = unit(c(0.01,0.01,.01,0.01), "cm"),
    panel.border = element_blank())+
    xlim(2,11) +
    ylim(41,44)+
    labs(title = "Detections for 2020 from July to September")
  
# ggsave(Density_2020_summer, file = "figures/Density_2020_summer.pdf", width = 297, height = 210, units = "mm")


summer = ggarrange(Density_20172019_summer, Density_2020_summer, nrow = 2, heights = c(1, 1))
ggsave(summer, file = "figures/summer.pdf", width = 210, height = 297, units = "mm")
  
#Data in data frame format
SAR_morning_density = rbind(SAR_morning_2017,SAR_morning_2018,SAR_morning_2019) %>% filter(Month %in% c("July","August","September")) %>% st_coordinates()
SAR_morning_plot = rbind(SAR_morning_2017,SAR_morning_2018,SAR_morning_2019)%>% filter(Month %in% c("July","August","September")) %>% cbind(SAR_morning_density) %>% st_drop_geometry()

Morning_density_summer = ggplot() +
  geom_density2d(data = SAR_morning_plot, aes(x = X, y = Y), bins = 20, size = 0.1, col = "black") +
  stat_density2d(data = SAR_morning_plot, aes(x = X, y = Y, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 20, geom = 'polygon') +
  geom_sf(data = land_map, fill = "darkgrey", color = "black", size = 0.05) +
  # geom_point(data = SAR_morning_plot, aes(x=  X, y = Y), color = "darkred", alpha = 0.7, size = 1, shape = ".")+
  scale_fill_viridis(option = "magma", direction=-1, breaks = c(0,0.2,0.4,0.6,0.8,1), limits = c(0,1),
                     name = "Detection density", 
                     guide = guide_colorbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       # some shifting around
                       title.hjust = 0.5,
                       label.hjust = 0.5
                     )) +
  scale_alpha(range = c(0.1, 0.9), guide = "none") +
  theme_map() + 
  theme(
    text = element_text(size = 12),   
    legend.position = c(0.43, -0.35),
    legend.text.align = 0,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
    plot.title = element_text(hjust = 0.5, color = "#4e4d47", size = 14),
    plot.margin = unit(c(.1,.1,.1,.1), "cm"),
    panel.spacing = unit(c(0.01,0.01,.01,0.01), "cm"),
    panel.border = element_blank())+
  xlim(2,11) +
  ylim(41,44)+
  labs(title = "Morning detections for 2017-2019 from July to September")

# ggsave(Morning_density_summer, file = "figures/Morning_density_summer.pdf", width = 297, height = 210, units = "mm")

#Same 2017 2019 but for evening
SAR_evening_density = rbind(SAR_evening_2017,SAR_evening_2018,SAR_evening_2019) %>% filter(Month %in% c("July","August","September")) %>% st_coordinates()
SAR_evening_plot = rbind(SAR_evening_2017,SAR_evening_2018,SAR_evening_2019) %>% filter(Month %in% c("July","August","September")) %>% cbind(SAR_evening_density) %>% st_drop_geometry()

Evening_density_summer = ggplot() +
  geom_density2d(data = SAR_evening_plot, aes(x = X, y = Y), bins = 20, size = 0.1, col = "black") +
  stat_density2d(data = SAR_evening_plot, aes(x = X, y = Y, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 20, geom = 'polygon') +
  geom_sf(data = land_map, fill = "darkgrey", color = "black", size = 0.05) +
  # geom_point(data = SAR_morning_plot, aes(x=  X, y = Y), color = "darkred", alpha = 0.7, size = 1, shape = ".")+
  scale_fill_viridis(option = "magma", direction=-1, breaks = c(0,0.2,0.4,0.6,0.8,1), limits = c(0,1),
                     name = "Detection density", 
                     guide = guide_colorbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       # some shifting around
                       title.hjust = 0.5,
                       label.hjust = 0.5
                     )) +
  scale_alpha(range = c(0.1, 0.9), guide = "none") +
  theme_map() + 
  theme(
    text = element_text(size = 12),   
    legend.position = c(0.43, -0.35),
    legend.text.align = 0,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
    plot.title = element_text(hjust = 0.5, color = "#4e4d47", size = 14),
    plot.margin = unit(c(.1,.1,.1,.1), "cm"),
    panel.spacing = unit(c(0.01,0.01,.01,0.01), "cm"),
    panel.border = element_blank())+
  xlim(2,11) +
  ylim(41,44)+
  labs(title = "Evening detections for 2017-2019 from July to September")

# ggsave(Evening_density_summer, file = "figures/Evening_density_summer.pdf", width = 297, height = 210, units = "mm")

#Data in data frame format
SAR_morning_2020_density = SAR_morning_2020 %>% filter(Month %in% c("July","August","September")) %>% st_coordinates()
SAR_morning_2020_plot = SAR_morning_2020 %>% filter(Month %in% c("July","August","September")) %>% cbind(SAR_morning_2020_density) %>% st_drop_geometry()


#THIS IS FOR 2020
Morning_2020_density_summer = ggplot() +
  geom_density2d(data = SAR_morning_2020_plot, aes(x = X, y = Y), bins = 20, size = 0.1, col = "black") +
  stat_density2d(data = SAR_morning_2020_plot, aes(x = X, y = Y, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 20, geom = 'polygon') +
  geom_sf(data = land_map, fill = "darkgrey", color = "black", size = 0.05) +
  # geom_point(data = SAR_morning_plot, aes(x=  X, y = Y), color = "darkred", alpha = 0.7, size = 1, shape = ".")+
  scale_fill_viridis(option = "magma", direction=-1, breaks = c(0,0.2,0.4,0.6,0.8,1), limits = c(0,1),
                     name = "Detection density", 
                     guide = "none") +
  scale_alpha(range = c(0.1, 0.9), guide = "none") +
  theme_map() + 
  theme(
    text = element_text(size = 12),   
    legend.text.align = 0,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
    plot.title = element_text(hjust = 0.5, color = "#4e4d47", size = 14),
    plot.margin = unit(c(.1,.1,.1,.1), "cm"),
    panel.spacing = unit(c(0.01,0.01,.01,0.01), "cm"),
    panel.border = element_blank())+
  xlim(2,11) +
  ylim(41,44)+
  labs(title = "Morning detections for 2020 from July to September")

# ggsave(Morning_2020_density_summer, file = "figures/Morning_2020_density_summer.pdf", width = 297, height = 210, units = "mm")

SAR_evening_2020_density = SAR_evening_2020 %>% filter(Month %in% c("July","August","September")) %>% st_coordinates()
SAR_evening_2020_plot = SAR_evening_2020 %>% filter(Month %in% c("July","August","September")) %>% cbind(SAR_evening_2020_density) %>% st_drop_geometry()

Evening_2020_density_summer = ggplot() +
  geom_density2d(data = SAR_evening_2020_plot, aes(x = X, y = Y), bins = 20, size = 0.1, col = "black") +
  stat_density2d(data = SAR_evening_2020_plot, aes(x = X, y = Y, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 20, geom = 'polygon') +
  geom_sf(data = land_map, fill = "darkgrey", color = "black", size = 0.05) +
  # geom_point(data = SAR_morning_plot, aes(x=  X, y = Y), color = "darkred", alpha = 0.7, size = 1, shape = ".")+
  scale_fill_viridis(option = "magma", direction=-1, breaks = c(0,0.2,0.4,0.6,0.8,1), limits = c(0,1),
                     name = "Detection density", 
                     guide = "none") +
  scale_alpha(range = c(0.1, 0.9), guide = "none") +
  theme_map() + 
  theme(
    text = element_text(size = 12),   
    legend.text.align = 0,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
    plot.title = element_text(hjust = 0.5, color = "#4e4d47", size = 14),
    plot.margin = unit(c(.1,.1,.1,.1), "cm"),
    panel.spacing = unit(c(0.01,0.01,.01,0.01), "cm"),
    panel.border = element_blank())+
  xlim(2,11) +
  ylim(41,44)+
  labs(title = "Evening detections for 2020 from July to September")

# ggsave(Evening_2020_density_summer, file = "figures/Evening_2020_density_summer.pdf", width = 297, height = 210, units = "mm")

summer_morning = ggarrange(Morning_density_summer, Morning_2020_density_summer, nrow = 2, heights = c(1, 1))
ggsave(summer_morning, file = "figures/summer_morning.pdf", width = 210, height = 297, units = "mm")

summer_evening = ggarrange(Evening_density_summer, Evening_2020_density_summer, nrow = 2, heights = c(1, 1))
ggsave(summer_evening, file = "figures/summer_evening.pdf", width = 210, height = 297, units = "mm")

}