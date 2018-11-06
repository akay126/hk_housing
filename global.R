library('rgdal')
library('spdplyr')
# library('geojsonio')
# library('rmapshaper')
library('leaflet')
# library('raster')
library('htmltools')
library('DT')

library('rgdal')
library('dplyr')
library('tidyr')
library('ggplot2')
library("htmltools")

library('shiny')
library('shinydashboard')
library('leaflet')
library('RColorBrewer')
library('DT')

shp_district = readOGR(dsn = "./GIS/Hong_Kong_18_Districts",verbose = FALSE)
shp_district1 = readOGR(dsn = "./GIS/Hong_Kong_18_Districts",verbose = FALSE)
shp_district2 = readOGR(dsn = "./GIS/Hong_Kong_18_Districts",verbose = FALSE)
shp_subdistrict = readOGR(dsn = "./GIS/Hong_Kong_431_Districts",verbose = FALSE)
Centa_fl_18dist = read.csv(file = "./data/Centa_fl_18dist.csv",header = T)
Centa_fl_18distYR = read.csv(file = "./data/Centa_fl_18distYR.csv",header = T)
Centa_flr = read.csv("./data/Centa_floor.csv",header = T)


Centa_reduced = read.csv(file = "./data/Centa_reduced1.csv",header = T)
# Centa_reduced = Centa_reduced[,-c(3:4,6,7,9:11,17)]
# Centa_reduced$price_per_saleable_area = Centa_reduced$sold.price/Centa_reduced$saleable.area
Centa_reduced =Centa_reduced %>% 
  tidyr::drop_na(.,price_per_saleable_area)

shp_district = readOGR(dsn = "./GIS/Hong_Kong_18_Districts_join",verbose = FALSE)
shp_district@data = left_join(shp_district@data,Centa_fl_18dist)
# write.csv(Centa_reduced,"./data/Centa_reduced1.csv",row.names = F)
# Centa_floor = read.csv("./data/Centa_floor_2017.csv", header =  T)
