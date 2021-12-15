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

pkgs <- c("tidyverse","here","lme4","broom","tidymodels","parallel","cowplot","sf","RColorBrewer","ggridges","plotly","heatmaply","parsedate","birk","ggthemes",
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

#Plot every image we have to check for images with radio frequency interfences
plot_all_images(SAR_data_clean)

#Filter out select RFI images
SAR_data_noRFI = filter_RFI(SAR_data_clean)

#Delete Shipping routes
SAR_data_final = filter_shipping(SAR_data_noRFI)
save(SAR_data_final, file = "output/SAR_data_final.Rdata")

#Save one data set per year
sep_year(SAR_data_final)

#Sar Inside MPA
SAR_mpa_final = SAR_inside_mpa(SAR_data_final)

#Save one data set per year but for MPAs
sep_year_mpa(SAR_mpa_final)

#Separate morning and evening
sep_morning_evening(SAR_data_final, SAR_data_2017, SAR_data_2018,SAR_data_2019,SAR_data_2020)

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


#MAPPING THE SHIT

#Same CRS ? 
st_crs(map_background) == st_crs(data_2017)


all_maps = ggarrange(Morning_density, Evening_density, Morning_2020_density, Evening_2020_density, nrow = 2 , ncol = 2, common.legend = T, legend = 'bottom')
ggsave(all_maps, file = "figures/all_maps.pdf", width = 297, height = 210, units = "mm")


mclapply(unique(data_2017$Month),function(i){

  ggplot(map_background) +
    geom_sf(fill="white" ) +
    geom_sf(data = med_mpa,fill = "lightblue", alpha = 0.7) + 
    geom_sf(data=filter(data_2017,Month==i), size = 1.5, alpha = 0.7, color = "darkred")+
    theme(panel.background = element_rect(fill = "black"),panel.grid = element_line(color = "black", size = 0),
          panel.border = element_rect(colour = "#FEEFDB", fill=NA, size=1)) +
    xlim(2,11) +
    ylim(41,44)
  
  
  ggsave(file = paste0("figures/maps/2017/plot",i,"2017.pdf"), width = 20, height = 15)
  
}, mc.cores = 2)


mclapply(unique(data_2018$Month),function(i){
  
  
  ggplot(map_background) +
    geom_sf(fill="white" ) +
    geom_sf(data = med_mpa,fill = "lightblue", alpha = 0.7) + 
    geom_sf(data=filter(data_2018,Month==i), size = 1.5, alpha = 0.7, color = "darkred")+
    theme(panel.background = element_rect(fill = "black"),panel.grid = element_line(color = "black", size = 0),
          panel.border = element_rect(colour = "#FEEFDB", fill=NA, size=1)) +
    xlim(2,11) +
    ylim(41,44)
  
  ggsave(file = paste0("figures/maps/2018/plot",i,"2018.pdf"), width = 20, height = 15)
  
}, mc.cores = 2)

mclapply(unique(data_2019$Month),function(i){
  
  ggplot(map_background) +
    geom_sf(fill="white" ) +
    # geom_sf(data = med_mpa,fill = "lightblue", alpha = 0.7) + 
    geom_sf(data=filter(data_2019,Month==i), size = 1.5, alpha = 0.7, color = "darkred")+
    theme(panel.background = element_rect(fill = "black"),panel.grid = element_line(color = "black", size = 0),
          panel.border = element_rect(colour = "#FEEFDB", fill=NA, size=1)) +
    xlim(2,11) +
    ylim(41,44)
  
  
  ggsave(file = paste0("figures/maps/2019/plot",i,"2019.pdf"), width = 20, height = 15)
  
}, mc.cores = 2)

mclapply(unique(data_2020$Month),function(i){
  
  ggplot(map_background) +
    geom_sf(fill="white" ) +
    # geom_sf(data = med_mpa,fill = "lightblue", alpha = 0.7) + 
    geom_sf(data=filter(data_2020,Month==i), size = 1.5, alpha = 0.7, color = "darkred")+
    theme(panel.background = element_rect(fill = "black"),panel.grid = element_line(color = "black", size = 0),
          panel.border = element_rect(colour = "#FEEFDB", fill=NA, size=1)) +
    xlim(2,11) +
    ylim(41,44)
  
  
  ggsave(file = paste0("figures/maps/2020/plot",i,"2020.pdf"), width = 20, height = 15)
  
}, mc.cores = 2)

# library(gganimate)
# map_2017 + transition_time(MonthNum)
# 
# 
# anim_save("filenamehere.gif")



#maps


mean(data_2017$ObsDay)
sd(data_2017$ObsDay)
max(data_2017$ObsDay)
min(data_2017$ObsDay)


mean(data_2018$ObsDay)
sd(data_2018$ObsDay)
max(data_2018$ObsDay)
min(data_2018$ObsDay)

mean(data_2019$ObsDay)
sd(data_2019$ObsDay)
max(data_2019$ObsDay)
min(data_2019$ObsDay)


mean(data_2020$ObsDay)
sd(data_2020$ObsDay)
max(data_2020$ObsDay)
min(data_2020$ObsDay)

data_2017_LD = data_2017 %>%
  filter(Month %in% c("March","April","May"))

nrow(data_2017_LD)

data_2018_LD = data_2018 %>%
  filter(Month %in% c("March","April","May"))

nrow(data_2018_LD)

data_2019_LD = data_2019 %>%
  filter(Month %in% c("March","April","May"))

nrow(data_2019_LD)

data_2020_LD = data_2020 %>%
  filter(Month %in% c("March","April","May"))

nrow(data_2020_LD)

5626/6723

#############
mean(data_2017_LD$ObsDay)
sd(data_2017_LD$ObsDay)
max(data_2017_LD$ObsDay)
min(data_2017_LD$ObsDay)
#############
mean(data_2018_LD$ObsDay)
sd(data_2018_LD$ObsDay)
max(data_2018_LD$ObsDay)
min(data_2018_LD$ObsDay)
#########
mean(data_2019_LD$ObsDay)
sd(data_2019_LD$ObsDay)
max(data_2019_LD$ObsDay)
min(data_2019_LD$ObsDay)
#########
mean(data_2020_LD$ObsDay)
sd(data_2020_LD$ObsDay)
max(data_2020_LD$ObsDay)
min(data_2020_LD$ObsDay)

data_2017_PD = data_2017 %>%
  filter(Month %in% c("June"))
data_2018_PD = data_2018 %>%
  filter(Month %in% c("June"))
data_2019_PD = data_2019 %>%
  filter(Month %in% c("June"))
data_2020_PD = data_2020 %>%
  filter(Month %in% c("June"))

nrow(data_2017_PD)
nrow(data_2018_PD)
nrow(data_2019_PD)
nrow(data_2020_PD)

mean(data_2017_PD$ObsDay)
sd(data_2017_PD$ObsDay)
max(data_2017_PD$ObsDay)
min(data_2017_PD$ObsDay)
#############
mean(data_2018_PD$ObsDay)
sd(data_2018_PD$ObsDay)
max(data_2018_PD$ObsDay)
min(data_2018_PD$ObsDay)
#########
mean(data_2019_PD$ObsDay)
sd(data_2019_PD$ObsDay)
max(data_2019_PD$ObsDay)
min(data_2019_PD$ObsDay)
#########
mean(data_2020_PD$ObsDay)
sd(data_2020_PD$ObsDay)
max(data_2020_PD$ObsDay)
min(data_2020_PD$ObsDay)

t.test(data_2019_PD$ObsDay,data_2020_PD$ObsDay)

######

data_2017_S = data_2017 %>%
  filter(Month %in% c("July","August","September"))
data_2018_S = data_2018 %>%
  filter(Month %in% c("July","August","September"))
data_2019_S = data_2019 %>%
  filter(Month %in% c("July","August","September"))
data_2020_S = data_2020 %>%
  filter(Month %in% c("July","August","September"))

nrow(data_2017_S)
nrow(data_2018_S)
nrow(data_2019_S)
nrow(data_2020_S)

mean(data_2017_S$ObsDay)
sd(data_2017_S$ObsDay)
max(data_2017_S$ObsDay)
min(data_2017_S$ObsDay)
#############
mean(data_2018_S$ObsDay)
sd(data_2018_S$ObsDay)
max(data_2018_S$ObsDay)
min(data_2018_S$ObsDay)
#########
mean(data_2019_S$ObsDay)
sd(data_2019_S$ObsDay)
max(data_2019_S$ObsDay)
min(data_2019_S$ObsDay)
#########
mean(data_2020_S$ObsDay)
sd(data_2020_S$ObsDay)
max(data_2020_S$ObsDay)
min(data_2020_S$ObsDay)

t.test(data_2018_S$ObsDay,data_2020_S$ObsDay)

#MPAs
levels(SAR_mpa_final$Status)

SAR_data_mpa_2017_inside_LD = SAR_data_mpa_2017 %>% filter(Status == "Inside_mpa") %>% filter(Month %in% c("March","April","May"))
SAR_data_mpa_2017_inside_PD = SAR_data_mpa_2017 %>% filter(Status == "Inside_mpa") %>% filter(Month %in% c("June"))
SAR_data_mpa_2017_inside_S = SAR_data_mpa_2017 %>% filter(Status == "Inside_mpa") %>% filter(Month %in% c("July","August","September"))
SAR_data_mpa_2017_5_LD = SAR_data_mpa_2017 %>% filter(Status == "5km_mpa") %>% filter(Month %in% c("March","April","May"))
SAR_data_mpa_2017_5_PD = SAR_data_mpa_2017 %>% filter(Status == "5km_mpa") %>% filter(Month %in% c("June"))
SAR_data_mpa_2017_5_S = SAR_data_mpa_2017 %>% filter(Status == "5km_mpa") %>% filter(Month %in% c("July","August","September"))
SAR_data_mpa_2017_10_LD  = SAR_data_mpa_2017 %>% filter(Status == "10km_mpa") %>% filter(Month %in% c("March","April","May"))
SAR_data_mpa_2017_10_PD  = SAR_data_mpa_2017 %>% filter(Status == "10km_mpa") %>% filter(Month %in% c("June"))
SAR_data_mpa_2017_10_S  = SAR_data_mpa_2017 %>% filter(Status == "10km_mpa") %>% filter(Month %in% c("July","August","September"))

SAR_data_mpa_2018_inside_LD = SAR_data_mpa_2018 %>% filter(Status == "Inside_mpa")  %>% filter(Month %in% c("March","April","May"))
SAR_data_mpa_2018_inside_PD = SAR_data_mpa_2018 %>% filter(Status == "Inside_mpa") %>% filter(Month %in% c("June"))
SAR_data_mpa_2018_inside_S = SAR_data_mpa_2018 %>% filter(Status == "Inside_mpa") %>% filter(Month %in% c("July","August","September"))
SAR_data_mpa_2018_5_LD = SAR_data_mpa_2018 %>% filter(Status == "5km_mpa") %>% filter(Month %in% c("March","April","May"))
SAR_data_mpa_2018_5_PD = SAR_data_mpa_2018 %>% filter(Status == "5km_mpa") %>% filter(Month %in% c("June"))
SAR_data_mpa_2018_5_S = SAR_data_mpa_2018 %>% filter(Status == "5km_mpa") %>% filter(Month %in% c("July","August","September"))
SAR_data_mpa_2018_10_LD  = SAR_data_mpa_2018 %>% filter(Status == "10km_mpa") %>% filter(Month %in% c("March","April","May"))
SAR_data_mpa_2018_10_PD  = SAR_data_mpa_2018 %>% filter(Status == "10km_mpa") %>% filter(Month %in% c("June"))
SAR_data_mpa_2018_10_S  = SAR_data_mpa_2018 %>% filter(Status == "10km_mpa") %>% filter(Month %in% c("July","August","September"))

SAR_data_mpa_2019_inside_LD = SAR_data_mpa_2019 %>% filter(Status == "Inside_mpa") %>% filter(Month %in% c("March","April","May"))
SAR_data_mpa_2019_inside_PD = SAR_data_mpa_2019 %>% filter(Status == "Inside_mpa") %>% filter(Month %in% c("June"))
SAR_data_mpa_2019_inside_S = SAR_data_mpa_2019 %>% filter(Status == "Inside_mpa")%>% filter(Month %in% c("July","August","September"))
SAR_data_mpa_2019_5_LD = SAR_data_mpa_2019 %>% filter(Status == "5km_mpa") %>% filter(Month %in% c("March","April","May"))
SAR_data_mpa_2019_5_PD = SAR_data_mpa_2019 %>% filter(Status == "5km_mpa") %>% filter(Month %in% c("June"))
SAR_data_mpa_2019_5_S = SAR_data_mpa_2019 %>% filter(Status == "5km_mpa")%>% filter(Month %in% c("July","August","September"))
SAR_data_mpa_2019_10_LD  = SAR_data_mpa_2019 %>% filter(Status == "10km_mpa") %>% filter(Month %in% c("March","April","May"))
SAR_data_mpa_2019_10_PD  = SAR_data_mpa_2019 %>% filter(Status == "10km_mpa") %>% filter(Month %in% c("June"))
SAR_data_mpa_2019_10_S  = SAR_data_mpa_2019 %>% filter(Status == "10km_mpa") %>% filter(Month %in% c("July","August","September"))

SAR_data_mpa_2020_inside_LD = SAR_data_mpa_2020 %>% filter(Status == "Inside_mpa") %>% filter(Month %in% c("March","April","May"))
SAR_data_mpa_2020_inside_PD = SAR_data_mpa_2020 %>% filter(Status == "Inside_mpa") %>% filter(Month %in% c("June"))
SAR_data_mpa_2020_inside_S = SAR_data_mpa_2020 %>% filter(Status == "Inside_mpa") %>% filter(Month %in% c("July","August","September"))
SAR_data_mpa_2020_5_LD = SAR_data_mpa_2020 %>% filter(Status == "5km_mpa") %>% filter(Month %in% c("March","April","May"))
SAR_data_mpa_2020_5_PD = SAR_data_mpa_2020 %>% filter(Status == "5km_mpa") %>% filter(Month %in% c("June"))
SAR_data_mpa_2020_5_S = SAR_data_mpa_2020 %>% filter(Status == "5km_mpa") %>% filter(Month %in% c("July","August","September"))
SAR_data_mpa_2020_10_LD  = SAR_data_mpa_2020 %>% filter(Status == "10km_mpa") %>% filter(Month %in% c("March","April","May"))
SAR_data_mpa_2020_10_PD  = SAR_data_mpa_2020 %>% filter(Status == "10km_mpa") %>% filter(Month %in% c("June"))
SAR_data_mpa_2020_10_S  = SAR_data_mpa_2020 %>% filter(Status == "10km_mpa")%>% filter(Month %in% c("July","August","September"))

nrow(SAR_data_mpa_2020_10_S)


# 
# boats_filtered$detecttime = as.factor(boats_filtered$detecttime)
# 
# (test2016 = boats_filtered %>%
#   filter(str_detect(detecttime,"2016"))%>%
#   ggplot(aes(Date,n,fill=detecttime))+
#   geom_histogram(stat="identity",position="dodge")+
#   theme(legend.position="none"))
# 
# (test2017 = boats_filtered %>%
#   filter(str_detect(detecttime,"2017"))%>%
#   ggplot(aes(Date,n,fill=detecttime))+
#   geom_histogram(stat="identity",position="dodge")+
#   theme(legend.position="none"))
# 
# (test2018 = boats_filtered %>%
#   filter(str_detect(detecttime,"2018"))%>%
#   ggplot(aes(Date,n,fill=detecttime))+
#   geom_histogram(stat="identity",position="dodge")+
#   theme(legend.position="none"))
# 
# (test2019 = boats_filtered %>%
#   filter(str_detect(detecttime,"2019"))%>%
#   ggplot(aes(Date,n,fill=detecttime))+
#   geom_histogram(stat="identity",position="dodge")+
#   theme(legend.position="none"))
# 
# (test2020 = boats_filtered %>%
#   filter(str_detect(detecttime,"2020"))%>%
#   ggplot(aes(Date,n,fill=detecttime))+
#   geom_histogram(stat="identity",position="dodge")+
#   theme(legend.position="none"))
# 
#   
#   mutate(Month = as.factor(ifelse(str_detect(Date,"03"),"March",ifelse(str_detect(Date,"04"),"April",ifelse(str_detect(Date,"05"),"May",ifelse(str_detect(Date,"06"),"June",ifelse(str_detect(Date,"07"),"July",
#                ifelse(str_detect(Date,"08"),"August",ifelse(str_detect(Date,"09"),"September","October"))))))))) %>%
#   mutate(Year = as.factor(ifelse(str_detect(Date,"2016"),"2016",ifelse(str_detect(Date,"2017"),"2017",ifelse(str_detect(Date,"2018"),"2018",ifelse(str_detect(Date,"2019"),"2019",ifelse(str_detect(Date,"2020"),"2020","2021")))))))%>%
#   filter(reliability != 3)
# 
#   
# 
# 
# ggplot(Banyuls_merge,aes(inc_ang, n))+
#   geom_bar(stat="identity",width = 0.009,aes(fill= Year),alpha = 0.7)+
#   scale_fill_viridis_d()+
#   scale_x_continuous(breaks=seq(30, 44, 0.5))+
#   theme_bw()
# 
# ggsave("plot.pdf",width=297,height=210,units="mm")
# 
# ggplot(Banyuls_data,aes(Date,Year,fill=Year))+
#   geom_density_ridges()+
#   theme_ridges()+
# 
# 
# test = Banyuls_merge %>%
#   filter(inc_ang >= 32 & inc_ang <= 33)
# 
# pic = Banyuls_merge %>% filter(n > 1000)
# 
# (plot2018 = Banyuls_merge %>%
#     filter(str_detect(Date, '2016')) %>%
#     ggplot(aes(Date,n)) +
#     geom_bar(stat="identity",position = "dodge",fill="#46ACC8") +
#     theme_minimal()+
#     theme(axis.text.x = element_text(angle = 65, hjust = 1)) +
#     labs(x= "Time",
#          y = "Number of boats counted on SAR image") +
#     theme(legend.position="none"))
# 
# (plot2018 = Banyuls_merge %>%
#   filter(str_detect(Date, '2017')) %>%
# ggplot(aes(Date,n)) +
#   geom_bar(stat="identity",position = "dodge",fill="#46ACC8") +
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 65, hjust = 1)) +
#     labs(x= "Time",
#          y = "Number of boats counted on SAR image") +
#   theme(legend.position="none"))
# 
# (plot2018 = Banyuls_merge %>%
#     filter(str_detect(Date, '2018')) %>%
#     ggplot(aes(Date,n)) +
#     geom_bar(stat="identity",position = "dodge",fill="#46ACC8") +
#     theme_minimal()+
#     theme(axis.text.x = element_text(angle = 65, hjust = 1)) +
#     labs(x= "Time",
#          y = "Number of boats counted on SAR image") +
#     theme(legend.position="none")+
#     ylim(0,500))
# 
# (plot2019 = Banyuls_merge %>%
#     filter(str_detect(Date, '2019')) %>%
#     ggplot(aes(Date,n)) +
#     geom_bar(stat="identity",position = "dodge",fill="#DD8D29") +
#     theme_minimal()+
#     theme(axis.text.x = element_text(angle = 65, hjust = 1)) +
#     scale_fill_viridis_d()+
#     theme(legend.position="none")+
#   ylim(0,500))
# 
# (plot2020 = Banyuls_merge %>%
#     filter(str_detect(Date, '2020')) %>%
#     ggplot(aes(Date,n)) +
#     geom_bar(stat="identity",position = "dodge",fill="#B41820") +
#     theme_minimal()+
#     theme(axis.text.x = element_text(angle = 65, hjust = 1)) +
#     scale_fill_viridis_d()+
#     theme(legend.position="none")+
#     ylim(0,500))
# 
# ggarrange(plot2018,plot2019,plot2020,ncol=1)
# 
# # lelz %>% 
# #   filter(!str_detect(Date, '2016|2017'))%>%
# # ggplot(aes(y=year,x=n,fill=stat(x)))+
# #   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01)+
# #   scale_fill_viridis_c(name = "Boat number", option = "C")+
# #   theme_minimal()
# 
# 
# formap2020 = Banyuls_data %>%
#   filter(str_detect(Date,"2020")) 
# 
# 
# 
# (map2020 = ggplot(map_background) +
+ 
#   stat_density2d(data=formap2020,aes(fill=target_number,x=lon,y=lat), alpha=0.5, geom="polygon")+
#   geom_sf(data=sites,size=0.5,alpha=0.2,colour="#B41820"))
# 
# formap2018 = Banuls_2016_2020 %>%
#   filter(str_detect(Date,"2018"))
# 
# sites <- st_as_sf(formap2018, coords = c("lon", "lat"), 
#                   crs = 4326, agr = "constant")
# 
# map2018 = ggplot(map_background) +
#   geom_sf(fill="#F0F8FF") +
#   theme(panel.background = element_rect(fill = "#FEEFDB"),panel.grid = element_line(color = "white", size = 0),
#         panel.border = element_rect(colour = "#FEEFDB", fill=NA, size=1)) +
#   xlim(1,6) +
#   ylim(40,44) + 
#   stat_density2d(data=formap2018,aes(fill=target_number,x=lon,y=lat), alpha=0.5, geom="polygon")+
#   geom_sf(data=sites,size=0.5,alpha=0.2,colour="#46ACC8")
# 
# formap2019 = Banuls_2016_2020 %>%
#   filter(str_detect(Date,"2019"))
# 
# sites <- st_as_sf(formap2019, coords = c("lon", "lat"), 
#                   crs = 4326, agr = "constant")
# 
# map2019 = ggplot(map_background) +
#     geom_sf(fill="#F0F8FF") +
#     theme(panel.background = element_rect(fill = "#FEEFDB"),panel.grid = element_line(color = "white", size = 0),
#           panel.border = element_rect(colour = "#FEEFDB", fill=NA, size=1)) +
#   xlim(1,6) +
#   ylim(40,44) + 
#   stat_density2d(data=formap2019,aes(fill=target_number,x=lon,y=lat), alpha=0.5, geom="polygon")+
#   geom_sf(data=sites,size=0.5,alpha=0.2,colour="#DD8D29")
# 
# 
# graphe2018 = ggarrange(plot2018,map2018,ncol = 2,widths = c(1,1))
# ggsave(graphe2018,file="output/graphe2018.pdf",width=297,height=210,units="mm")
# 
# graphe2019 = ggarrange(plot2019,map2019,ncol = 2, heights = c(1,1))
# ggsave(graphe2019,file="output/graphe2019.pdf",width=297,height=210,units="mm")
# 
# graphe2020 = ggarrange(plot2020,map2020,ncol = 2, heights = c(1,1))
# ggsave(graphe2020,file="output/graphe2020.pdf",width=297,height=210,units="mm")
# 
# # 
# # ggsave("figures/timeplot.pdf",width=297,height=210,units="mm")
# # 
# # ggplot(test, aes())
