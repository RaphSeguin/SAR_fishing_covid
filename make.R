#' Run the Entire Project
#'
#' This script reproduces all analyses and figures of the ___________ article.
#'
#' @author Raphaël SEGUIN, \email{raphael.seguin46@@gmail.com},
#'         Nicolas LOISEAU, \email{nicolas.loiseau1@@gmail.com},
#'
#' @date 2021/02/17
#' 

#-----------------Loading packages-------------------

pkgs <- c("tidyverse","here","lme4","broom","tidymodels","parallel","cowplot","sf","RColorBrewer","ggridges","plotly","heatmaply","parsedate","birk","ggthemes","MASS","automap",
          "harrypotter","wesanderson","ranger","ggpubr","data.table","xml2","XML","rnaturalearth","ggExtra","raster","exactextractr","gstat","magrittr","scales","grid","gridExtra")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

#-----------------Loading all data---------------------

#Load Rdata
path = (here::here("data"))
setwd(path)
files <- list.files(pattern="Rdata")
data_list = lapply(files, load, .GlobalEnv)

 #-----------------Loading all functions---------------------

path = (here::here("R"))
setwd(path)
files.source = list.files(here::here("R"))
sapply(files.source, source)

#------------ANALYSES-----------

setwd(here())

#Load SHP and stuff for maps
map_background = st_read("maps/ShpMed_fromGADM.shp")
land_map = st_read("maps/land_med.shp")
med_mpa = st_read("maps/mpaMed.shp")

#PLEASE DOWNLOAD THIS DATA SET FROM : 
#https://www.marineregions.org/downloads.php
#Maritime boundaries V11, Shapefile
#BEFORE RUNNING THE PROJECT
ZEE <- sf::st_read("maps/eez_v11.shp") %>% filter(SOVEREIGN1 == "France")

#PLEASE DOWNLOAD THIS DATA SET FROM : 
#https://knb.ecoinformatics.org/view/doi:10.5063/F1S180FS
#raw_2008_shipping_mol.zip
#BEFORE RUNNING THE PROJECT
shipping_routes = raster("maps/shipping.tif")

#Intersection between our data and SAR data
#This takes a while
SAR_data_ZEE = ZEE_intersection(data_SAR,ZEE)

#Cleaning up data
SAR_data_clean = data_clean(SAR_data_ZEE)

# #Plot every image we have to check for images with radio frequency interfences
#plot_all_images(SAR_data_clean)

#Filter out select RFI images
SAR_data_noRFI = filter_RFI(SAR_data_clean)

#Delete Shipping routes
SAR_data_final = filter_shipping(SAR_data_noRFI)
save(SAR_data_final, file = "output/SAR_data_final.Rdata")

#Save one data set per year
sep_year(SAR_data_final)

#Sar Inside MPA
SAR_mpa_final = SAR_inside_mpa(SAR_data_final)

length(unique(SAR_data_final$image_name))
#Save one data set per year but for MPAs
sep_year_mpa(SAR_mpa_final)

#Separate morning and evening
sep_morning_evening(SAR_data_final, SAR_data_2017, SAR_data_2018,SAR_data_2019,SAR_data_2020)

mean(SAR_data_2020$ObsDay)
sd(SAR_data_2020$ObsDay)
max(SAR_data_2020$ObsDay)
min(SAR_data_2020$ObsDay)

#------Figures----------

path = (here::here("R_figures"))
setwd(path)
files.source = list.files(here::here("R_figures"))
sapply(files.source, source)

path = (here::here("output"))
setwd(path)
files <- list.files(here::here("output"),pattern = ".Rdata")
data_list = lapply(files, load, .GlobalEnv)

setwd(here())

ggplot(map_background) +
  geom_sf(fill="white" ) +
  geom_sf(data = test, color = "red", size = 1) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"), labels = c('10Km around MPA', '30Km around MPA', 'Inside MPA')) +
  theme(panel.background = element_rect(fill = "black"),panel.grid = element_line(color = "black", size = 0),
        panel.border = element_rect(colour = "black", fill=NA, size=1),legend.key=element_blank()) +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  labs (color = "Legend") +
  xlim(2,11) +
  ylim(41,44)



library(geosphere)


distance = mclapply(1:nrow(SAR_data_final),function(i){
  
  temp = as_Spatial(SAR_data_final[i,])
  g = geosphere::dist2Line(temp,map_background_sp)
  
  distances = list(g)
  return(distances)
  
}, mc.cores = 5, mc.preschedule = F)

setwd(here())
save(distance, file ="output/distance.Rdata")


ship_distances = do.call(rbind,do.call(rbind,distance))

SAR_data_final_dist = cbind(SAR_data_final, ship_distances) %>%
  group_by(Year, Month) %>%
  mutate(mean_distance = mean(distance))


dist2017 = SAR_data_final_dist %>%
  filter(Year == 2017) %>%
  ggplot(aes(Month, mean_distance))+
  geom_point() +
  ylim(0,30000) +
  labs(title = 2017)

dist2018 = SAR_data_final_dist %>%
  filter(Year == 2018) %>%
  ggplot(aes(Month, mean_distance))+
  geom_point()+
  ylim(0,30000)+
  labs(title = 2018)

dist2019 = SAR_data_final_dist %>%
  filter(Year == 2019) %>%
  ggplot(aes(Month, mean_distance))+
  geom_point()+
  ylim(0,30000)+
  labs(title = 2019)

dist2020 = SAR_data_final_dist %>%
  filter(Year == 2020) %>%
  ggplot(aes(Month, mean_distance))+
  geom_point()+
  ylim(0,30000)+
  labs(title = 2020)

ggarrange(dist2017,dist2018,dist2019,dist2020)


#Comparison maps


#Let's map this shit
ggplot(map_background) +
  geom_sf(fill="white" ) +
  geom_sf(data = med_mpa_filter,fill = "#E7E3AF", alpha = 0.7, lwd = 0.3) + 
  # geom_sf(data = med_mpa_filter_10,fill = "#2380B3", alpha = 0.2, lwd = 0) +
  # geom_sf(data = med_mpa_filter_30,fill = "#41A6D9", alpha = 0.05, lwd = 0) + 
  geom_sf(data=SAR_mpa_final, aes(color = Status), size = 0.05, alpha = 0.5)+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"), labels = c('10Km around MPA', '30Km around MPA', 'Inside MPA')) +
  theme(panel.background = element_rect(fill = "black"),panel.grid = element_line(color = "black", size = 0),
        panel.border = element_rect(colour = "black", fill=NA, size=1),legend.key=element_blank()) +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  labs (color = "Legend") +
  xlim(2,11) +
  ylim(41,44)

ggsave("figures/obs_MPA.png",width = 297, height = 210, units = "mm")
ggsave("figures/obs_MPA.pdf",width = 297, height = 210, units = "mm")

#Figure of study area
study_area(map_background, ZEE)

#Figure of the number of images per year and per month 
number_of_images(SAR_data_final)

#Number of observations per year, per month and per day boxplot
number_of_observations(SAR_data_final,SAR_data_2017,SAR_data_2018,SAR_data_2019,SAR_data_2020)

#Number of observations per year, per month and per day boxplot for MPAs
number_of_observations_mpa(SAR_data_final,SAR_data_2017,SAR_data_2018,SAR_data_2019,SAR_data_2020)

#Number of images we lose due to shippinh
shipping_plot(SAR_data_noRFI)

#Example of RFI images
RFI_plot(SAR_data_clean)

#Delta year
plot_delta_year(SAR_data_2017, SAR_data_2018, SAR_data_2019, SAR_data_2020)

#Delta month
plot_delta_month(SAR_data_2017, SAR_data_2018, SAR_data_2019, SAR_data_2020, 
                 SAR_morning_2017, SAR_morning_2018,SAR_morning_2019,SAR_morning_2020,
                 SAR_evening_2017, SAR_evening_2018, SAR_evening_2019, SAR_evening_2020)

#Plot
density_maps_summer(SAR_data_2017, SAR_data_2018, SAR_data_2019, SAR_data_2020,  
                    SAR_morning_2017, SAR_morning_2018,SAR_morning_2019,SAR_morning_2020,
                     SAR_evening_2017, SAR_evening_2018, SAR_evening_2019, SAR_evening_2020)

density_maps_lockdown(SAR_data_2017, SAR_data_2018, SAR_data_2019, SAR_data_2020,  
                    SAR_morning_2017, SAR_morning_2018,SAR_morning_2019,SAR_morning_2020,
                    SAR_evening_2017, SAR_evening_2018, SAR_evening_2019, SAR_evening_2020)



#MEAN PER YEAR
plot_year = SAR_dat %>%
  group_by(Year) %>%
  mutate(ObsYear = n())

meanperyear = ggplot(plot_year)+
  geom_point(aes(x=Year,y=ObsYear,color=Year),size=4)+
  scale_color_viridis_d()+
  ylim(0,30000)+
  theme_minimal()+
  labs(y = "Number of observations per year")

ggsave(meanperyear, file='figures/meanperyear.pdf',width = 20, height = 15) 
ggsave(meanperyear, file='figures/meanperyear.png',width = 20, height = 15) 

#Mean per day for each Month for each Year
plot_month = data_final %>%
  group_by(Year,Month)%>%
  mutate(MeanDay = mean(ObsDay))

(meanperday = ggplot(plot_month)+
  geom_point(aes(x=Month,y=MeanDay,color=Year),size=4)+
  theme_minimal()+
  scale_color_viridis_d()+
  ylim(0,500)+
  labs(y = "Mean observations per day"))

ggsave(meanperday, file='figures/meanperday.pdf',width = 20, height = 15) 
ggsave(meanperday, file='figures/meanperday.png',width = 20, height = 15) 

#Mean per month for each Year
plot_month = full %>%
  group_by(Year)%>%
  mutate(MeanMonth = mean(ObsMonth))

(meanpermonth = ggplot(plot_month)+
  geom_point(aes(x=Year,y=MeanMonth,color=Year),size=4)+
  theme_minimal()+
  scale_color_viridis_d()+
  ylim(0,5000)+
  labs(y = "Mean observations per Month"))

ggsave(meanpermonth, file='figures/meanpermonth.pdf',width = 20, height = 15) 
ggsave(meanpermonth, file='figures/meanpermonth.png',width = 20, height = 15) 


SAR_2017_lockdown = SAR_data_2017 %>% filter(Month %in% c("March","April","May"))
SAR_2018_lockdown = SAR_data_2018 %>% filter(Month %in% c("March","April","May"))
SAR_2019_lockdown = SAR_data_2019 %>% filter(Month %in% c("March","April","May"))
SAR_2020_lockdown = SAR_data_2020 %>% filter(Month %in% c("March","April","May"))

mean(SAR_2020_lockdown$ObsDay)

5626/mean(6723, 5966, 5944)

mean(SAR_2020_lockdown$ObsDay)
sd(SAR_2020_lockdown$ObsDay)
max(SAR_2020_lockdown$ObsDay)
min(SAR_2020_lockdown$ObsDay)

SAR_2017_june = SAR_data_2017 %>% filter(Month %in% c("June"))
SAR_2018_june = SAR_data_2018 %>% filter(Month %in% c("June"))
SAR_2019_june = SAR_data_2019 %>% filter(Month %in% c("June"))
SAR_2020_june = SAR_data_2020 %>% filter(Month %in% c("June"))


SAR_2017_summer = SAR_data_2017 %>% filter(Month %in% c("July","August","September"))
SAR_2018_summer  = SAR_data_2018 %>% filter(Month %in% c("July","August","September"))
SAR_2019_summer  = SAR_data_2019 %>% filter(Month %in% c("July","August","September"))
SAR_2020_summer  = SAR_data_2020 %>% filter(Month %in% c("July","August","September"))

t.test(SAR_2020_summer$ObsDay, SAR_2017_summer$ObsDay)

