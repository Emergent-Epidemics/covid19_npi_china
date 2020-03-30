#Map of cases
#Sam 
#nCoV 
#Feb. 14 2020

###########
#libraries#
###########
library(maptools)
library(raster)
library(plyr)
library(ggplot2)
library(rgdal)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(leaflet.mapboxgl)
library(leaflet.extras)
library(ggmap)
library(rgeos)
library(wesanderson)
library(proj4)

#########
#Globals#
#########

######
#Data#
######
source("../../Emergent_Epidemics_Lab_nCoV2019/Emergent_Epidemics_Lab_nCoV2019/scripts/update_data.R")
full_data <- update_data(savefile = FALSE)
china <- readOGR("China/Provincial.shp")
china_outline <- readOGR("China/Boundary.shp")
china_bound <- readOGR("China/UndefinedBoundary.shp")

#########
#Mapping#
#########
travel_cases <- grep("wuhan", full_data$travel_history_location, ignore.case = TRUE)

#here is onset before travel
#china_cases <- which(full_data$country == "China" & full_data$date_onset_symptoms < strptime("01-23-2020", format = "%m-%d-%Y") & full_data$date_onset_symptoms < full_data$travel_history_dates)

china_cases <- which(full_data$country == "China" & full_data$date_onset_symptoms < strptime("01-23-2020", format = "%m-%d-%Y"))

use_travel <- travel_cases[which(travel_cases %in% china_cases)]
travel_case_data <- full_data[china_cases,]
travel_case_data$days_before_quarantine <- as.numeric(strptime("01-23-2020", format = "%m-%d-%Y") - travel_case_data$date_onset_symptoms, unit = "days")

#china <- getData("GADM", country="CN", level=1)
#china_simple <- gSimplify(china, tol=0.01, topologyPreserve=TRUE)
china_tf <- spTransform(china, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
china_prov_df <- fortify(china_tf)

china_bound_tf <- spTransform(china_bound, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
china_bound_df <- fortify(china_bound_tf)

##########
#Plotting#
##########
theme_opts<-list(theme(panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.background = element_blank(),
                       plot.background = element_blank(),
                       axis.line = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks = element_blank(),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       plot.title = element_blank(),
                       legend.background = element_rect(fill = "white", colour = "white"),
                       legend.key = element_rect(fill = "white")))

travel_case_data$days_before_quarantine_bin <- cut(x = travel_case_data$days_before_quarantine, c(0, 2, 3, 4, 5, 6, 7, 14, 48))

quartz()
ggplot() + 
  geom_polygon(data=china_prov_df, aes(long,lat,group=group), fill="white", color = "#000000") +
  geom_point(data = travel_case_data, aes(x = longitude, y = latitude, color = days_before_quarantine_bin))+
  geom_segment(aes(x = rep(114.1717, nrow(travel_case_data)), y = rep(30.3515, nrow(travel_case_data)), xend = travel_case_data$longitude, yend = travel_case_data$latitude, color = travel_case_data$days_before_quarantine_bin)) + geom_polygon(data = china_bound_df, aes(x = long, y = lat), color = "white", fill = "white") + geom_polygon(data = china_bound_df, aes(x = long, y = lat), color = "#000000", lty = 3, fill = "white") + 
  theme(aspect.ratio=1)+
  scale_color_brewer(palette="Dark2", name = "Travelers with symptoms before Wuhan quarantine", labels = c("0-1 days", "2 days", "3 days", "4 days", "5 days", " 6 days", "1 week", "2 weeks", "3+ weeks"))+ theme(legend.position = "none")+
  theme_opts

