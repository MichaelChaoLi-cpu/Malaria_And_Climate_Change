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

# output: GWPR_BW_setp_list_0.5_20_0.25.Rdata
# GWPR_BW_setp_list_0.5_20_0.25.Rdata: "CVScore" the mean of square residuals
# GWPR_BW_setp_list_0.5_20_0.25.Rdata: "Bandwidth" distance

# output: GWPR_FEM_CV_F_result_425.Rdata
# GWPR_FEM_CV_F_result_425.Rdata: this is a result of GWPR based on FEM with 4.25

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

run <- F
if(run){
  cor.matrix <- cor(dataset_used %>% dplyr::select(-id, -year) %>% na.omit())
  cor.matrix %>% write.csv("05_Results/CorrelationMatrix.csv")
  
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
}

## several variables
formula <- PfPR ~ TempMean + TempSquare + TempSd + NDVIMean + Population + GDPperCap
cor(dataset_used %>% dplyr::select(all.vars(formula)) %>% na.omit())
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

dataset_used <- dataset_used %>% dplyr::select(id, year, all.vars(formula)) %>% na.omit()
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
                         GI.step = 0.25, GI.upper = 25, GI.lower = 20)
GWPR.FEM.bandwidth.step.list <- GWPR.FEM.bandwidth
plot(GWPR.FEM.bandwidth.step.list[,1], GWPR.FEM.bandwidth.step.list[,2])
save(GWPR.FEM.bandwidth.step.list, file = "05_Results/GWPR_BW_setp_list_0.5_20_0.25.Rdata")

################################ this is GWPR based on FEM
GWPR.FEM.bandwidth = 4.25
GWPR.FEM.CV.F.result <- GWPR(formula = formula, dataset_used, index = c("id", "year"),
                             SDF = coords.df, bw = GWPR.FEM.bandwidth, adaptive = F,
                             p = 2, effect = "individual", kernel = "bisquare", longlat = F, 
                             model = "within")
GWPR.FEM.CV.F.result$SDF@data %>% View()
GWPR.FEM.CV.F.result$SDF@data %>% summary()
save(GWPR.FEM.CV.F.result, file = "05_Results/GWPR_FEM_CV_F_result_425.Rdata")
