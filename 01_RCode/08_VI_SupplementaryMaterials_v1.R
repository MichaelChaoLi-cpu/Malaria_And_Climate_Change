# Author: M.L.

# input: 01_dataset_used.RData
# 01_dataset_used.RData: "PfPR": Plasmodium falciparum parasite rate, range 0 ~ 1
# 01_dataset_used.RData: "NDVIMean": NDVI value -100% ~ 100% from M*D13C2
# 01_dataset_used.RData: "TempMean": Annually average temperature C
# 01_dataset_used.RData: "AirPressureMean" kPa
# 01_dataset_used.RData: "HumidityMean" unit is g/kg
# 01_dataset_used.RData: "PrecipitationMean" g / (m2 * h)
# 01_dataset_used.RData: "WindSpeedMean" m/s
# 01_dataset_used.RData: "PopulationDensity" cap/km2
# 01_dataset_used.RData: "GDPperCap" USD/Cap
# 01_dataset_used.RData: "TempSd": Annually standard deviation temperature C
# 01_dataset_used.RData: "TempSquare": Annually average temperature square C2

# end

library(tidyverse)
library(dplyr)
library(sp)
library(tmap)
library(raster)
library("rnaturalearth")

# mean PfPR figure S1
world <- ne_countries(scale = "medium", returnclass = "sp")

# make the raster -180 -60 180 60
nx = 480                                       # number of cells in the x direction
ny = 1440                                     # number of cells in the y direction
xmin = -179.875                                     # x coordinate of lower, left cell center 
ymin = -59.875                                     # y coordinate of lower, left cell center 
xsize = 0.25                                   # extent of cells in x direction
ysize = 0.25                                   # extent of cells in y direction
proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

addcoord <- function(nx,xmin,xsize,ny,ymin,ysize,proj) { # Michael Pyrcz, March, 2018                      
  # makes a 2D dataframe with coordinates based on GSLIB specification
  coords = matrix(nrow = nx*ny,ncol=2)
  ixy = 1
  for(iy in 1:nx) {
    for(ix in 1:ny) {
      coords[ixy,1] = xmin + (ix-1)*xsize  
      coords[ixy,2] = ymin + (iy-1)*ysize 
      ixy = ixy + 1
    }
  }
  coords.df = data.frame(coords)
  colnames(coords.df) <- c("X","Y")
  coords.df$id = 1:nrow(coords.df)
  xy <- coords.df[,c(1,2)]
  coords.df <- SpatialPointsDataFrame(coords = xy, data = coords.df %>% dplyr::select("id"),
                                      proj4string = CRS(proj))
  return (coords.df)
  
}

coords <- addcoord(nx,xmin,xsize,ny,ymin,ysize,proj)

load("04_Data/01_dataset_used.RData")

dataset_used.mean <- aggregate(dataset_used$PfPR, by = list(dataset_used$id), 
                               FUN = mean, na.rm = T)
colnames(dataset_used.mean) <- c("id", "Mean.PfPR")

location <- coordinates(coords) %>% as.data.frame()
location$id <- coords@data$id
location <- left_join(dataset_used.mean, location, by ='id')
xy <- location[,c(3,4)]
coords.df <- SpatialPointsDataFrame(coords = xy, data = location %>% dplyr::select("id"),
                                    proj4string = CRS(proj))
rm(coords)
rm(location)

coords.df@data <- left_join(coords.df@data, dataset_used.mean, by = "id")
coords.df <- as(coords.df, 'SpatialPixelsDataFrame')
coords.df <- as(coords.df, "SpatialGridDataFrame")
coords.df@data <- coords.df@data %>% dplyr::select(Mean.PfPR)
coords.df <- raster(coords.df)

pal <- colorRampPalette(c("blue","green","yellow","red"))
brks <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
labels_brks <- c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")
title_size = .0001
legend_title_size = 1
margin = 0
mean.PfPR.tmap <- tm_shape(coords.df) +
  tm_raster("Mean.PfPR", palette = pal(10), breaks = brks, 
            style = 'cont', legend.is.portrait = F, title = "The Mean of PfPR in 2-10 year olds",
            labels = labels_brks) +
  #tm_shape(world) +
  #tm_borders(col = 'black', lwd = 0.5, alpha = 0.8) +
  tm_grid(alpha = .25) + 
  tm_scale_bar(position = c("right", "bottom")) + 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("right", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) 
mean.PfPR.tmap %>%
  tmap_save(filename = "06_Figure/S1.mean.PfPR.tmap.jpg", width = 300, height = 140, units = 'mm', dpi = 1000)
# mean PfPR figure S1


