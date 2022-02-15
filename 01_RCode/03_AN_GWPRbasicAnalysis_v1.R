# Author: M.L.

# note: From this project, the data base is built in outside disk F:

# end

library(tidyverse)
library(dplyr)
library(stringr)
library(plm)
library(sp)
library(GWPR.light)
library(doParallel)
library(foreach)

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

cor(dataset_used %>% dplyr::select(-id, -year) %>% na.omit())

formula <- PfPR ~ TempMean + TempSd + AirPressureMean + AirPressureSd +  
  NDVIMean + NDVISd + 
  HumidityMean + HumiditySd + PrecipitationMean + PrecipitationSd + 
  WindSpeedMean + WindSpeedSd + Population
pdata <- pdata.frame(dataset_used, index = c("id", "year"))
ols <- plm(formula, pdata, model = "pooling")
summary(ols)
fem <- plm(formula, pdata, model = "within")
summary(fem)
rem <- plm(formula, pdata, model = "random")
summary(rem)
pFtest(fem, ols)
phtest(fem, rem)
plmtest(ols, type = c("bp"))
rm(fem, ols, rem, pdata)

dataset_used <- dataset_used %>% na.omit()
dataset.id <- dataset_used %>% dplyr::select(id) %>% distinct()

location <- coordinates(coords) %>% as.data.frame()
location$id <- coords@data$id
location <- left_join(dataset.id, location, by ='id')
xy <- location[,c(2,3)]
coords.df <- SpatialPointsDataFrame(coords = xy, data = location %>% dplyr::select("id"),
                                    proj4string = CRS(proj))
rm(coords)

source("01_RCode/07_AF_GWPRBandwidthStepSelection_v1.R")
# we exiamine from the GWPR based on fem 
formula
GWPR.FEM.bandwidth <- # this is about fixed bandwidth
  bw.GWPR.step.selection(formula = formula, data = dataset_used, index = c("id", "year"),
                         SDF = coords.df, adaptive = F, p = 2, bigdata = F,
                         upperratio = 0.10, effect = "individual", model = "within", approach = "CV",
                         kernel = "bisquare",doParallel = T, cluster.number = 6, gradientIncrecement = T,
                         GI.step = 0.25, GI.upper = 20, GI.lower = 0.5)
GWPR.FEM.bandwidth.step.list <- GWPR.FEM.bandwidth
plot(GWPR.FEM.bandwidth.step.list[,1], GWPR.FEM.bandwidth.step.list[,2])
save(GWPR.FEM.bandwidth.step.list, file = "03_Results/GWPR_BW_setp_list.Tokyo.001025.0005.Rdata")