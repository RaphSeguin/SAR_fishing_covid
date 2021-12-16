#' Density maps for morning/evening delta during lockdown
#'
#' 
#' @param data  all datasets year separated
#' 
#' @return plots for delta difference in map format
#'
#' @export
#' 
#' 

density_delta = function(SAR_data_2017, SAR_data_2018, SAR_data_2019, SAR_data_2020, 
                                 SAR_morning_2017, SAR_morning_2018, SAR_morning_2019, SAR_morning_2020,
                                 SAR_evening_2017, SAR_evening_2018, SAR_evening_2019, SAR_evening_2020){  
  
 
  #THIS IS FOR DATA FROM 2017 to 2019
  #Data in data frame format
  SAR_density = rbind(SAR_data_2017,SAR_data_2018,SAR_data_2019) %>% filter(Month %in% c("April","May","June")) %>% st_coordinates()
  SAR_plot = rbind(SAR_data_2017,SAR_data_2018,SAR_data_2019)%>% filter(Month %in% c("April","May","June")) %>% cbind(SAR_density) %>% st_drop_geometry()
  
  
  #ALL 2020 DATA  
  SAR_2020_density = SAR_data_2020 %>%  filter(Month %in% c("April","May","June")) %>% st_coordinates()
  SAR_2020_plot = SAR_data_2020 %>%  filter(Month %in% c("April","May","June")) %>% cbind(SAR_2020_density) %>% st_drop_geometry()
  
  #Calculating density for 2017/2019 data
  SAR_plot_density = kde2d(SAR_plot$X,SAR_plot$Y,n=300)
  raster1 = raster(SAR_plot_density)
  
  #Calculating density for 2020 data
  SAR_2020_plot_density = kde2d(SAR_2020_plot$X,SAR_2020_plot$Y,n=c(ncol(raster1),nrow(raster1)),
                                lims=c(range(coordinates(raster1)[,1]),
                                       range(coordinates(raster1)[,2])))
  raster2 = raster(SAR_2020_plot_density)
  
  #Difference between the two rasteres
  raster_overlay<- overlay(raster1,
                           raster2,
                           fun=function(r1, r2){return(r2-r1)})
  
  crs(raster_overlay)
# 
#   Convert to usable data
  lockdown_2020 <- as(raster_overlay, "SpatialPixelsDataFrame")
  lockdown_2020 <- as.data.frame(lockdown_2020)
  colnames(lockdown_2020) <- c("value", "x", "y")
  
  xy <- lockdown_2020[,c(2,3)]
  
  spdf = SpatialPointsDataFrame(coords = xy, data = lockdown_2020, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  krig = autoKrige(value~1, data = spdf, new_data=fit_points)
  
  
  #plotting density difference
  lockdown_delta = ggplot() + 
    geom_tile(data=lockdown_2020, aes(x=x, y=y, fill=value)) +
    geom_sf(data = land_map, fill = "darkgrey", color = "black", size = 0.05) +
    scale_fill_gradient2(low = "#1B3B5B", mid = "white", high = "#AC0A27", limits = c(-0.7,0.3),
                         breaks = c(-0.7,-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2,0.3),
                         name = "Density difference 2020 - 2017/2019",
                         guide = guide_colorbar(
                           direction = "horizontal",
                           barheight = unit(2, units = "mm"),
                           barwidth = unit(50, units = "mm"),
                           draw.ulim = F,
                           title.position = 'top',
                           title.vjust = 0.4
                         )) +
    theme_map() +
    theme(
      # panel.background = element_rect(fill = "darkgrey"),
      text = element_text(size = 10),
      legend.position = c(0.4, -0.05),
      legend.box.just = "center",
      # legend.text.align = 0,
      legend.background = element_rect(fill = alpha('white', 0.0)),
      legend.text = element_text(size = 7, color = "#4e4d47"),
      # plot.title = element_text(hjust = 0.5, color = "#4e4d47", size = 14),
      plot.margin = unit(c(.1,.1,.1,.1), "cm"),
      # panel.spacing = unit(c(0.01,0.01,.01,0.01), "cm"),
      panel.border = element_blank())+
    labs(title  = "Density difference between 2020 and 2017-2019 from April to June") +
    xlim(3.036763,10.209629) +
    ylim(41.26345,43.78089)
  
  ggsave(lockdown_delta, file = "figures/lockdown_delta.pdf", width = 297, height = 210, units = "mm")

  #2017 2019 for morning
  #Data in data frame format
  SAR_morning_density = rbind(SAR_morning_2017,SAR_morning_2018,SAR_morning_2019) %>%  filter(Month %in% c("April","May","June")) %>% st_coordinates()
  SAR_morning_plot = rbind(SAR_morning_2017,SAR_morning_2018,SAR_morning_2019)%>%  filter(Month %in% c("April","May","June")) %>% cbind(SAR_morning_density) %>% st_drop_geometry()
  
  #2020  for morning
  #Data in data frame format
  SAR_morning_2020_density = SAR_morning_2020 %>%  filter(Month %in% c("April","May","June")) %>% st_coordinates()
  SAR_morning_2020_plot = SAR_morning_2020 %>%  filter(Month %in% c("April","May","June")) %>% cbind(SAR_morning_2020_density) %>% st_drop_geometry()
  
  #Calculating density for 2017/2019 data
  SAR_morning_density = kde2d(SAR_morning_plot$X,SAR_morning_plot$Y,n=100)
  raster1_morning = raster(SAR_morning_density)
  
  #Calculating density for 2020 data
  SAR_2020_morning_density = kde2d(SAR_morning_2020_plot$X,SAR_morning_2020_plot$Y,n=c(ncol(raster1_morning),nrow(raster1_morning)),
                                lims=c(range(coordinates(raster1_morning)[,1]),
                                       range(coordinates(raster1_morning)[,2])))
  raster2_morning = raster(SAR_2020_morning_density)
  
  #Difference between the two rasteres
  raster_overlay_morning <- overlay(raster1_morning,
                           raster2_morning,
                           fun=function(r1, r2){return(r2-r1)})
  
  #Convert to usable data
  lockdown_2020_morning <- as(raster_overlay_morning, "SpatialPixelsDataFrame")
  lockdown_2020_morning <- as.data.frame(lockdown_2020_morning)
  colnames(lockdown_2020_morning) <- c("value", "x", "y")
  
  #plotting density difference
  lockdown_delta_morning = ggplot() + 
    geom_tile(data=lockdown_2020_morning, aes(x=x, y=y, fill=value)) + 
    geom_sf(data = land_map, fill = "darkgrey", color = "black", size = 0.05) +
    scale_fill_gradient2(low = "#343D98", mid = "white", high = "#AC0A27", limits = c(-0.7,0.3),
                         breaks = c(-0.7,-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2,0.3),
                         name = "Density difference 2020 - 2017/2019",
                         guide = guide_colorbar(
                           direction = "horizontal",
                           barheight = unit(2, units = "mm"),
                           barwidth = unit(50, units = "mm"),
                           draw.ulim = F,
                           title.position = 'top',
                           title.vjust = 0.4
                         )) +
    theme_map() +
    theme(
      # panel.background = element_rect(fill = "darkgrey"),
      text = element_text(size = 10),
      legend.position = c(0.4, -0.05),
      legend.box.just = "center",
      # legend.text.align = 0,
      legend.background = element_rect(fill = alpha('white', 0.0)),
      legend.text = element_text(size = 7, color = "#4e4d47"),
      # plot.title = element_text(hjust = 0.5, color = "#4e4d47", size = 14),
      plot.margin = unit(c(.1,.1,.1,.1), "cm"),
      # panel.spacing = unit(c(0.01,0.01,.01,0.01), "cm"),
      panel.border = element_blank())+
    labs(title  = "Density difference between 2020 and 2017-2019 from April to June in the morning") +
    xlim(3.036763,10.209629) +
    ylim(41.26345,43.78089)
  
  ggsave(lockdown_delta_morning, file = "figures/lockdown_delta_morning.pdf", width = 297, height = 210, units = "mm")
  
  #Same 2017 2019 but for evening
  SAR_evening_density = rbind(SAR_evening_2017,SAR_evening_2018,SAR_evening_2019) %>%  filter(Month %in% c("April","May","June")) %>% st_coordinates()
  SAR_evening_plot = rbind(SAR_evening_2017,SAR_evening_2018,SAR_evening_2019) %>%  filter(Month %in% c("April","May","June")) %>% cbind(SAR_evening_density) %>% st_drop_geometry()
  
  #Same for 2020 evening
  SAR_evening_2020_density = SAR_evening_2020 %>% filter(Month %in% c("April","May","June")) %>% st_coordinates()
  SAR_evening_2020_plot = SAR_evening_2020 %>% filter(Month %in% c("April","May","June")) %>% cbind(SAR_evening_2020_density) %>% st_drop_geometry()
  
  #Calculating density for 2017/2019 data
  SAR_evening_density = kde2d(SAR_evening_plot$X,SAR_evening_plot$Y,n=100)
  raster1_evening = raster(SAR_evening_density)
  
  #Calculating density for 2020 data
  SAR_2020_evening_density = kde2d(SAR_evening_2020_plot$X,SAR_evening_2020_plot$Y,n=c(ncol(raster1_evening),nrow(raster1_evening)),
                                   lims=c(range(coordinates(raster1_evening)[,1]),
                                          range(coordinates(raster1_evening)[,2])))
  raster2_evening = raster(SAR_2020_evening_density)
  
  #Difference between the two rasteres
  raster_overlay_evening <- overlay(raster1_evening,
                                    raster2_evening,
                                    fun=function(r1, r2){return(r2-r1)})
  
  #Convert to usable data
  lockdown_2020_evening <- as(raster_overlay_evening, "SpatialPixelsDataFrame")
  lockdown_2020_evening <- as.data.frame(lockdown_2020_evening)
  colnames(lockdown_2020_evening) <- c("value", "x", "y")
  
  range(lockdown_2020_evening$y)
  
  #plotting density difference
  lockdown_delta_evening = ggplot() + 
    geom_tile(data=lockdown_2020_evening, aes(x=x, y=y, fill=value)) + 
    geom_sf(data = land_map, fill = "darkgrey", color = "black", size = 0.05) +
    scale_fill_gradient2(low = "#343D98", mid = "white", high = "#AC0A27", limits = c(-0.7,0.3),
                         breaks = c(-0.7,-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2,0.3),
                         name = "Density difference 2020 - 2017/2019",
                         guide = guide_colorbar(
                           direction = "horizontal",
                           barheight = unit(2, units = "mm"),
                           barwidth = unit(50, units = "mm"),
                           draw.ulim = F,
                           title.position = 'top',
                           title.vjust = 0.4
                         )) +
    theme_map() +
    theme(
      # panel.background = element_rect(fill = "darkgrey"),
      text = element_text(size = 10),
      legend.position = c(0.4, -0.05),
      legend.box.just = "center",
      # legend.text.align = 0,
      legend.background = element_rect(fill = alpha('white', 0.0)),
      legend.text = element_text(size = 7, color = "#4e4d47"),
      # plot.title = element_text(hjust = 0.5, color = "#4e4d47", size = 14),
      plot.margin = unit(c(.1,.1,.1,.1), "cm"),
      # panel.spacing = unit(c(0.01,0.01,.01,0.01), "cm"),
      panel.border = element_blank())+
    labs(title  = "Density difference between 2020 and 2017-2019 from April to June in the evening") +
    xlim(3.036763,10.209629) +
    ylim(41.28429,43.77876)
  
  ggsave(lockdown_delta_evening, file = "figures/lockdown_delta_evening.pdf", width = 297, height = 210, units = "mm")
  
  
}