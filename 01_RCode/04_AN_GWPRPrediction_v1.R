# Author: M.L.

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

#ssp126
filelist.ssp126 <- filelist[c(1,5)]
TempRasterDataset.ssp126 <- 
  extractPointDataFromRasterMultiband(TempRasterFolder, filelist.ssp126, coords,
                                      2041, F, "TempSSP126", 12)
TempRasterDataset.ssp126 <- aggregate(TempRasterDataset.ssp126$TempSSP126,
                               by = list(TempRasterDataset.ssp126$id, 
                                         TempRasterDataset.ssp126$year, 
                                         TempRasterDataset.ssp126$month), 
                               FUN = "mean", na.rm = T
)
colnames(TempRasterDataset.ssp126) <- c("id", "year", "month", "TempSSP126")

TempRasterDataset.ssp126 <- TempRasterDataset.ssp126 %>%
  pivot_wider(names_from = "month", values_from = "TempSSP126")
TempRasterDataset.ssp126$TempMean <- rowMeans(TempRasterDataset.ssp126[3:14], na.rm = T)
TempRasterDataset.ssp126$TempSd <- rowSds(as.matrix(TempRasterDataset.ssp126[3:14]), na.rm = T)
TempRasterDataset.ssp126 <- TempRasterDataset.ssp126 %>%
  filter(!is.na(TempMean))
TempRasterDataset.ssp126 <- TempRasterDataset.ssp126 %>% dplyr::select(id, year, TempMean, TempSd)
colnames(TempRasterDataset.ssp126) <- c("id", "year.2041", "TempMean.2041.ssp126", "TempSd.2041.ssp126")

#ssp245
filelist.ssp245 <- filelist[c(2,6)]
TempRasterDataset.ssp245 <- 
  extractPointDataFromRasterMultiband(TempRasterFolder, filelist.ssp245, coords,
                                      2041, F, "TempSSP245", 12)
TempRasterDataset.ssp245 <- aggregate(TempRasterDataset.ssp245$TempSSP245,
                                      by = list(TempRasterDataset.ssp245$id, 
                                                TempRasterDataset.ssp245$year, 
                                                TempRasterDataset.ssp245$month), 
                                      FUN = "mean", na.rm = T
)
colnames(TempRasterDataset.ssp245) <- c("id", "year", "month", "TempSSP245")

TempRasterDataset.ssp245 <- TempRasterDataset.ssp245 %>%
  pivot_wider(names_from = "month", values_from = "TempSSP245")
TempRasterDataset.ssp245$TempMean <- rowMeans(TempRasterDataset.ssp245[3:14], na.rm = T)
TempRasterDataset.ssp245$TempSd <- rowSds(as.matrix(TempRasterDataset.ssp245[3:14]), na.rm = T)
TempRasterDataset.ssp245 <- TempRasterDataset.ssp245 %>%
  filter(!is.na(TempMean))
TempRasterDataset.ssp245 <- TempRasterDataset.ssp245 %>% dplyr::select(id, year, TempMean, TempSd)
colnames(TempRasterDataset.ssp245) <- c("id", "year.2041", "TempMean.2041.ssp245", "TempSd.2041.ssp245")

#ssp370
filelist.ssp370 <- filelist[c(3,7)]
TempRasterDataset.ssp370 <- 
  extractPointDataFromRasterMultiband(TempRasterFolder, filelist.ssp370, coords,
                                      2041, F, "TempSSP370", 12)
TempRasterDataset.ssp370 <- aggregate(TempRasterDataset.ssp370$TempSSP370,
                                      by = list(TempRasterDataset.ssp370$id, 
                                                TempRasterDataset.ssp370$year, 
                                                TempRasterDataset.ssp370$month), 
                                      FUN = "mean", na.rm = T
)
colnames(TempRasterDataset.ssp370) <- c("id", "year", "month", "TempSSP370")

TempRasterDataset.ssp370 <- TempRasterDataset.ssp370 %>%
  pivot_wider(names_from = "month", values_from = "TempSSP370")
TempRasterDataset.ssp370$TempMean <- rowMeans(TempRasterDataset.ssp370[3:14], na.rm = T)
TempRasterDataset.ssp370$TempSd <- rowSds(as.matrix(TempRasterDataset.ssp370[3:14]), na.rm = T)
TempRasterDataset.ssp370 <- TempRasterDataset.ssp370 %>%
  filter(!is.na(TempMean))
TempRasterDataset.ssp370 <- TempRasterDataset.ssp370 %>% dplyr::select(id, year, TempMean, TempSd)
colnames(TempRasterDataset.ssp370) <- c("id", "year.2041", "TempMean.2041.ssp370", "TempSd.2041.ssp370")

#ssp585
filelist.ssp585 <- filelist[c(4,8)]
TempRasterDataset.ssp585 <- 
  extractPointDataFromRasterMultiband(TempRasterFolder, filelist.ssp585, coords,
                                      2041, F, "TempSSP585", 12)
TempRasterDataset.ssp585 <- aggregate(TempRasterDataset.ssp585$TempSSP585,
                                      by = list(TempRasterDataset.ssp585$id, 
                                                TempRasterDataset.ssp585$year, 
                                                TempRasterDataset.ssp585$month), 
                                      FUN = "mean", na.rm = T
)
colnames(TempRasterDataset.ssp585) <- c("id", "year", "month", "TempSSP585")

TempRasterDataset.ssp585 <- TempRasterDataset.ssp585 %>%
  pivot_wider(names_from = "month", values_from = "TempSSP585")
TempRasterDataset.ssp585$TempMean <- rowMeans(TempRasterDataset.ssp585[3:14], na.rm = T)
TempRasterDataset.ssp585$TempSd <- rowSds(as.matrix(TempRasterDataset.ssp585[3:14]), na.rm = T)
TempRasterDataset.ssp585 <- TempRasterDataset.ssp585 %>%
  filter(!is.na(TempMean))
TempRasterDataset.ssp585 <- TempRasterDataset.ssp585 %>% dplyr::select(id, year, TempMean, TempSd)
colnames(TempRasterDataset.ssp585) <- c("id", "year.2041", "TempMean.2041.ssp585", "TempSd.2041.ssp585")

future.temp.dataframe <- left_join(TempRasterDataset.ssp126, TempRasterDataset.ssp245)
future.temp.dataframe <- left_join(future.temp.dataframe, TempRasterDataset.ssp370)
future.temp.dataframe <- left_join(future.temp.dataframe, TempRasterDataset.ssp585)

load("05_Results/GWPR_FEM_CV_F_result_425.Rdata")
#GWPR.FEM.CV.F.result$SDF@data <- GWPR.FEM.CV.F.result$SDF@data %>%
#  mutate(TempMean = ifelse(abs(TempMean_TVa) < 1.645, 0, TempMean),
#         TempSquare = ifelse(abs(TempSquare_TVa) < 1.645, 0, TempSquare),
#         TempSd = ifelse(abs(TempSd_TVa) < 1.645, 0, TempSd)
#  )

GWPR.FEM.CV.F.result$SDF@data <- left_join(GWPR.FEM.CV.F.result$SDF@data, future.temp.dataframe)
GWPR.FEM.CV.F.result$SDF@data$TempMeanDelta <- GWPR.FEM.CV.F.result$SDF@data$TempMean.2041.ssp245 - 
  GWPR.FEM.CV.F.result$SDF@data$TempMean.2041.ssp126
GWPR.FEM.CV.F.result$SDF@data$TempSquareDelta <- 
  2*GWPR.FEM.CV.F.result$SDF@data$TempMean.2041.ssp126*GWPR.FEM.CV.F.result$SDF@data$TempMeanDelta +
  GWPR.FEM.CV.F.result$SDF@data$TempMeanDelta^2
GWPR.FEM.CV.F.result$SDF@data$TempSdDelta <- GWPR.FEM.CV.F.result$SDF@data$TempSd.2041.ssp245 - 
  GWPR.FEM.CV.F.result$SDF@data$TempSd.2041.ssp126

GWPR.FEM.CV.F.result$SDF@data$predictPfPR126_245 <- 
  GWPR.FEM.CV.F.result$SDF@data$TempMean * GWPR.FEM.CV.F.result$SDF@data$TempMeanDelta +
  GWPR.FEM.CV.F.result$SDF@data$TempSquare * GWPR.FEM.CV.F.result$SDF@data$TempSquareDelta +
  GWPR.FEM.CV.F.result$SDF@data$TempSd + GWPR.FEM.CV.F.result$SDF@data$TempSdDelta 

GWPR.FEM.CV.F.result$SDF@data$TempMeanDelta <- GWPR.FEM.CV.F.result$SDF@data$TempMean.2041.ssp370 - 
  GWPR.FEM.CV.F.result$SDF@data$TempMean.2041.ssp126
GWPR.FEM.CV.F.result$SDF@data$TempSquareDelta <- 
  2*GWPR.FEM.CV.F.result$SDF@data$TempMean.2041.ssp126*GWPR.FEM.CV.F.result$SDF@data$TempMeanDelta +
  GWPR.FEM.CV.F.result$SDF@data$TempMeanDelta^2
GWPR.FEM.CV.F.result$SDF@data$TempSdDelta <- GWPR.FEM.CV.F.result$SDF@data$TempSd.2041.ssp370 - 
  GWPR.FEM.CV.F.result$SDF@data$TempSd.2041.ssp126

GWPR.FEM.CV.F.result$SDF@data$predictPfPR126_370 <- 
  GWPR.FEM.CV.F.result$SDF@data$TempMean * GWPR.FEM.CV.F.result$SDF@data$TempMeanDelta +
  GWPR.FEM.CV.F.result$SDF@data$TempSquare * GWPR.FEM.CV.F.result$SDF@data$TempSquareDelta +
  GWPR.FEM.CV.F.result$SDF@data$TempSd + GWPR.FEM.CV.F.result$SDF@data$TempSdDelta 

GWPR.FEM.CV.F.result$SDF@data$TempMeanDelta <- GWPR.FEM.CV.F.result$SDF@data$TempMean.2041.ssp585 - 
  GWPR.FEM.CV.F.result$SDF@data$TempMean.2041.ssp126
GWPR.FEM.CV.F.result$SDF@data$TempSquareDelta <- 
  2*GWPR.FEM.CV.F.result$SDF@data$TempMean.2041.ssp126*GWPR.FEM.CV.F.result$SDF@data$TempMeanDelta +
  GWPR.FEM.CV.F.result$SDF@data$TempMeanDelta^2
GWPR.FEM.CV.F.result$SDF@data$TempSdDelta <- GWPR.FEM.CV.F.result$SDF@data$TempSd.2041.ssp585 - 
  GWPR.FEM.CV.F.result$SDF@data$TempSd.2041.ssp126

GWPR.FEM.CV.F.result$SDF@data$predictPfPR126_585 <- 
  GWPR.FEM.CV.F.result$SDF@data$TempMean * GWPR.FEM.CV.F.result$SDF@data$TempMeanDelta +
  GWPR.FEM.CV.F.result$SDF@data$TempSquare * GWPR.FEM.CV.F.result$SDF@data$TempSquareDelta +
  GWPR.FEM.CV.F.result$SDF@data$TempSd + GWPR.FEM.CV.F.result$SDF@data$TempSdDelta 

GWPR.FEM.CV.F.result.predict <- GWPR.FEM.CV.F.result
save(GWPR.FEM.CV.F.result.predict, file = "05_Results/GWPR_FEM_CV_F_result.predict.Rdata")
