# Author: M.L.

# input: GWPR_FEM_CV_F_result_425.Rdata
# GWPR_FEM_CV_F_result_425.Rdata: this is a result of GWPR based on FEM with 4.25

# input: TempRasterDataset.ssp***
# note: these data are from BCC-CSM2-MR model based on ssp123 245 370 585
#       https://www.worldclim.org/data/cmip6/cmip6_clim5m.html

# output: GWPR_FEM_CV_F_result.predict.Rdata
# GWPR_FEM_CV_F_result.predict.Rdata: "predictPfPR126_245" marginal effects from ssp126 to ssp245
# GWPR_FEM_CV_F_result.predict.Rdata: "predictPfPR126_370" marginal effects from ssp126 to ssp370
# GWPR_FEM_CV_F_result.predict.Rdata: "predictPfPR126_585" marginal effects from ssp126 to ssp585

# note: From this project, the data base is built in outside disk F:

# end

library(tidyverse)
library(dplyr)
library(sp) 
library(raster)
library(tmap)
library(stringr)
library("rnaturalearth")
library(matrixStats)

extractPointDataFromRasterMultiband <- function(RasterFolder, filelist, cityLocationSpatialPoint,
                                       year.input = 2000, flip_reverse = T,
                                       aimed_column_name = "raw", nband = 1
){
  RasterDataset <- 
    data.frame(Doubles=double(),
               Ints=integer(),
               Factors=factor(),
               Logicals=logical(),
               Characters=character(),
               stringsAsFactors=FALSE)
  for (filename in filelist){
    here_band = 1
    while (here_band - 1 < nband){
      test_tiff <- raster::raster(paste0(RasterFolder, filename), band = here_band)
      if(flip_reverse){
        test_tiff <- flip(test_tiff, direction = 'y')
      }
      crs(test_tiff) <- proj
      Year <- year.input
      Month <- here_band
      
      data_ext <- raster::extract(test_tiff, cityLocationSpatialPoint)
      cityLocationSpatialPoint@data$raw <- data_ext
      monthly_data <- cityLocationSpatialPoint@data %>%
        dplyr::select(id, raw)
      monthly_data <- monthly_data %>%
        mutate(year = Year,
               month = Month)
      RasterDataset <- rbind(RasterDataset, monthly_data)
      here_band = here_band + 1
    }
  }
  colnames(RasterDataset) <- c("id", aimed_column_name, "year", "month")
  return(RasterDataset)
}

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

TempRasterFolder <- "F:/13_Article/09_TempPrediction/Res025/"
filelist <- list.files(TempRasterFolder)

load("05_Results/GWPR_FEM_CV_F_result_425.Rdata")
coef.SDF <- GWPR.FEM.CV.F.result$SDF

#sub
num <- 1
future.temperature <-
  data.frame(Date=as.Date(character()),
                 File=character(), 
                 User=character(), 
                 stringsAsFactors=FALSE) 
scenario.name <- c("ssp126", "ssp245", "ssp460", "ssp585")

while(num < 13){
  filelist.sub <- filelist[c(num, num + 12)]
  year_num <- num%%3
  scenario.num <- floor((num-1)/3) + 1
  TempRasterDataset <- 
    extractPointDataFromRasterMultiband(TempRasterFolder, filelist.sub, coords,
                                        num, F, "Temp", 12)
  TempRasterDataset <- TempRasterDataset %>% na.omit()
  TempRasterDataset <- aggregate(TempRasterDataset$Temp,
                                 by = list(TempRasterDataset$id, 
                                           TempRasterDataset$year, 
                                           TempRasterDataset$month), 
                                 FUN = "mean", na.rm = T
  )
  colnames(TempRasterDataset) <- c("id", "year", "month", "Temp")
  TempRasterDataset <- TempRasterDataset %>% dplyr::select(id, Temp)
  TempRasterDataset.mean <- aggregate(TempRasterDataset$Temp, by = list(TempRasterDataset$id), 
                                      FUN = "mean", na.rm = T)
  colnames(TempRasterDataset.mean) <- c("id", "TempMean")
  TempRasterDataset.sd <- aggregate(TempRasterDataset$Temp, by = list(TempRasterDataset$id), 
                                      FUN = sd, na.rm = T)
  colnames(TempRasterDataset.sd) <- c("id", "TempSD")
  TempRasterDataset <- left_join(TempRasterDataset.mean, TempRasterDataset.sd)
  TempRasterDataset$year <- year_num
  TempRasterDataset$scenario <- scenario.name[scenario.num]
  future.temperature <- rbind(future.temperature, TempRasterDataset)
  num <- num + 1
}

year2060.mean <- future.temperature %>%
  filter(year == 0) %>% dplyr::select(id, TempMean, scenario) %>%
  pivot_wider(names_from = scenario, values_from = TempMean)
year2060.square <- year2060.mean
year2060.square[2:5] <- year2060.square[2:5]^2 
year2060.sd <- future.temperature %>%
  filter(year == 0) %>% dplyr::select(id, TempSD, scenario) %>%
  pivot_wider(names_from = scenario, values_from = TempSD)
year2060.mean <- year2060.mean %>%
  mutate(mean.dif.245.126 = ssp245 - ssp126,
         mean.dif.460.126 = ssp460 - ssp126,
         mean.dif.585.126 = ssp585 - ssp126) %>%
  dplyr::select(id, mean.dif.245.126, mean.dif.460.126, mean.dif.585.126)
year2060.square <- year2060.square %>%
  mutate(square.dif.245.126 = ssp245 - ssp126,
         square.dif.460.126 = ssp460 - ssp126,
         square.dif.585.126 = ssp585 - ssp126) %>%
  dplyr::select(id, square.dif.245.126, square.dif.460.126, square.dif.585.126)
year2060.sd <- year2060.sd %>%
  mutate(sd.dif.245.126 = ssp245 - ssp126,
         sd.dif.460.126 = ssp460 - ssp126,
         sd.dif.585.126 = ssp585 - ssp126) %>%
  dplyr::select(id, sd.dif.245.126, sd.dif.460.126, sd.dif.585.126)
year2060.dataset <- left_join(year2060.mean, year2060.square)
year2060.dataset <- left_join(year2060.dataset, year2060.sd)
rm(year2060.mean, year2060.square, year2060.sd)

prediction.2060 <- coef.SDF
prediction.2060@data <- left_join(prediction.2060@data, year2060.dataset)
prediction.2060@data <- prediction.2060@data %>%
  mutate(
    predictPfPR.245.126 = TempMean * mean.dif.245.126 + TempSquare * square.dif.245.126 +
      TempSd * sd.dif.245.126,
    predictPfPR.460.126 = TempMean * mean.dif.460.126 + TempSquare * square.dif.460.126 +
      TempSd * sd.dif.460.126,
    predictPfPR.585.126 = TempMean * mean.dif.585.126 + TempSquare * square.dif.585.126 +
      TempSd * sd.dif.585.126 )

save(prediction.2060, file = "05_Results/prediction.2060.Rdata")

year2080.mean <- future.temperature %>%
  filter(year == 1) %>% dplyr::select(id, TempMean, scenario) %>%
  pivot_wider(names_from = scenario, values_from = TempMean)
year2080.square <- year2080.mean
year2080.square[2:5] <- year2080.square[2:5]^2 
year2080.sd <- future.temperature %>%
  filter(year == 1) %>% dplyr::select(id, TempSD, scenario) %>%
  pivot_wider(names_from = scenario, values_from = TempSD)
year2080.mean <- year2080.mean %>%
  mutate(mean.dif.245.126 = ssp245 - ssp126,
         mean.dif.460.126 = ssp460 - ssp126,
         mean.dif.585.126 = ssp585 - ssp126) %>%
  dplyr::select(id, mean.dif.245.126, mean.dif.460.126, mean.dif.585.126)
year2080.square <- year2080.square %>%
  mutate(square.dif.245.126 = ssp245 - ssp126,
         square.dif.460.126 = ssp460 - ssp126,
         square.dif.585.126 = ssp585 - ssp126) %>%
  dplyr::select(id, square.dif.245.126, square.dif.460.126, square.dif.585.126)
year2080.sd <- year2080.sd %>%
  mutate(sd.dif.245.126 = ssp245 - ssp126,
         sd.dif.460.126 = ssp460 - ssp126,
         sd.dif.585.126 = ssp585 - ssp126) %>%
  dplyr::select(id, sd.dif.245.126, sd.dif.460.126, sd.dif.585.126)
year2080.dataset <- left_join(year2080.mean, year2080.square)
year2080.dataset <- left_join(year2080.dataset, year2080.sd)
rm(year2080.mean, year2080.square, year2080.sd)

prediction.2080 <- coef.SDF
prediction.2080@data <- left_join(prediction.2080@data, year2080.dataset)
prediction.2080@data <- prediction.2080@data %>%
  mutate(
    predictPfPR.245.126 = TempMean * mean.dif.245.126 + TempSquare * square.dif.245.126 +
      TempSd * sd.dif.245.126,
    predictPfPR.460.126 = TempMean * mean.dif.460.126 + TempSquare * square.dif.460.126 +
      TempSd * sd.dif.460.126,
    predictPfPR.585.126 = TempMean * mean.dif.585.126 + TempSquare * square.dif.585.126 +
      TempSd * sd.dif.585.126 )

save(prediction.2080, file = "05_Results/prediction.2080.Rdata")

year2100.mean <- future.temperature %>%
  filter(year == 2) %>% dplyr::select(id, TempMean, scenario) %>%
  pivot_wider(names_from = scenario, values_from = TempMean)
year2100.square <- year2100.mean
year2100.square[2:5] <- year2100.square[2:5]^2 
year2100.sd <- future.temperature %>%
  filter(year == 2) %>% dplyr::select(id, TempSD, scenario) %>%
  pivot_wider(names_from = scenario, values_from = TempSD)
year2100.mean <- year2100.mean %>%
  mutate(mean.dif.245.126 = ssp245 - ssp126,
         mean.dif.460.126 = ssp460 - ssp126,
         mean.dif.585.126 = ssp585 - ssp126) %>%
  dplyr::select(id, mean.dif.245.126, mean.dif.460.126, mean.dif.585.126)
year2100.square <- year2100.square %>%
  mutate(square.dif.245.126 = ssp245 - ssp126,
         square.dif.460.126 = ssp460 - ssp126,
         square.dif.585.126 = ssp585 - ssp126) %>%
  dplyr::select(id, square.dif.245.126, square.dif.460.126, square.dif.585.126)
year2100.sd <- year2100.sd %>%
  mutate(sd.dif.245.126 = ssp245 - ssp126,
         sd.dif.460.126 = ssp460 - ssp126,
         sd.dif.585.126 = ssp585 - ssp126) %>%
  dplyr::select(id, sd.dif.245.126, sd.dif.460.126, sd.dif.585.126)
year2100.dataset <- left_join(year2100.mean, year2100.square)
year2100.dataset <- left_join(year2100.dataset, year2100.sd)
rm(year2100.mean, year2100.square, year2100.sd)

prediction.2100 <- coef.SDF
prediction.2100@data <- left_join(prediction.2100@data, year2100.dataset)
prediction.2100@data <- prediction.2100@data %>%
  mutate(
    predictPfPR.245.126 = TempMean * mean.dif.245.126 + TempSquare * square.dif.245.126 +
      TempSd * sd.dif.245.126,
    predictPfPR.460.126 = TempMean * mean.dif.460.126 + TempSquare * square.dif.460.126 +
      TempSd * sd.dif.460.126,
    predictPfPR.585.126 = TempMean * mean.dif.585.126 + TempSquare * square.dif.585.126 +
      TempSd * sd.dif.585.126 )

save(prediction.2100, file = "05_Results/prediction.2100.Rdata")