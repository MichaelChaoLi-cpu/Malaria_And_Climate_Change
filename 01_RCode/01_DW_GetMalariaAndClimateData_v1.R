# Author: M.L.

# end

library(tidyverse)
library(dplyr)
library(sp) 
library(raster)
library(tmap)

# make the raster -180 -60 180 90
nx = 600                                       # number of cells in the x direction
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
coords <- as(coords, 'SpatialPixelsDataFrame')
coords <- as(coords, "SpatialGridDataFrame")
coords <- as(coords, "SpatialPolygonsDataFrame")

malaria.raster.folder <- "D:/13_Article/PfPR/Raster Data/PfPR_rmean/"
filelist <- list.files(malaria.raster.folder)

filename <- filelist[1]
test_tiff <- raster::raster(paste0(malaria.raster.folder, filename))
data_ext <- raster::extract(test_tiff, coords, fun = mean, na.rm = TRUE)
