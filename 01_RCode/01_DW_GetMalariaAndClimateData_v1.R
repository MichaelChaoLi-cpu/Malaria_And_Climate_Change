# Author: M.L.

# From this project, the data base is built in outside disk F:

# end

library(tidyverse)
library(dplyr)
library(sp) 
library(raster)
library(tmap)
library(stringr)

extractPointDataFromRaster <- function(RasterFolder, filelist, cityLocationSpatialPoint,
                                       year_start_location, month_start_location, flip_reverse = T,
                                       aimed_column_name = "raw", year_end_location = year_start_location + 3,
                                       month_end_location = month_start_location + 1
){
  RasterDataset <- 
    data.frame(Doubles=double(),
               Ints=integer(),
               Factors=factor(),
               Logicals=logical(),
               Characters=character(),
               stringsAsFactors=FALSE)
  for (filename in filelist){
    test_tiff <- raster::raster(paste0(RasterFolder, filename))
    if(flip_reverse){
      test_tiff <- flip(test_tiff, direction = 'y')
    }
    crs(test_tiff) <- proj
    Year <- str_sub(filename, year_start_location, year_end_location) %>% as.numeric()
    Month <- str_sub(filename, month_start_location, month_end_location) %>% as.numeric()
    
    data_ext <- raster::extract(test_tiff, cityLocationSpatialPoint)
    cityLocationSpatialPoint@data$raw <- data_ext
    monthly_data <- cityLocationSpatialPoint@data %>%
      dplyr::select(id, raw)
    monthly_data <- monthly_data %>%
      mutate(year = Year,
             month = Month)
    RasterDataset <- rbind(RasterDataset, monthly_data)
  }
  colnames(RasterDataset) <- c("id", aimed_column_name, "year", "month")
  return(RasterDataset)
}

# make the raster -180 -60 180 90
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

### Malaria
malaria.raster.folder <- "F:/13_Article/PfPR/Raster Data/PfPR_rmean_025/"
filelist <- list.files(malaria.raster.folder)

year.count <- 1
while (year.count < length(filelist) + 1) {
  filename = filelist[year.count]
  test_tiff <- raster::raster(paste0(malaria.raster.folder, filename))
  data_ext <- raster::extract(test_tiff, coords)
  coords@data <- cbind(coords@data, data_ext)
  colnames(coords@data)[ncol(coords@data)] <- paste0("Year_", as.character(year.count + 1999))
  year.count <- year.count + 1
}

malaria.dataframe <- coords@data
save(malaria.dataframe, file = "03_RawData/01_malariaDataframe.RData")

### #get ndvi
coords <- addcoord(nx,xmin,xsize,ny,ymin,ysize,proj)
NDVIRasterFolder <- "F:/13_Article/02_NDVI/VI_Monthly_005dg_v6/NDVI/"
filelist <- list.files(NDVIRasterFolder)
NDVIRasterDataset <- 
  extractPointDataFromRaster(NDVIRasterFolder, filelist, coords,
                             14, 19, F, "NDVI", 17, 21)
colnames(NDVIRasterDataset) <- c("id", "NDVI", "year", "month")
NDVIRasterDataset$date <- 
  as.Date((NDVIRasterDataset$month - 1),
          origin = paste0(NDVIRasterDataset$year,"-01-01")) %>% as.character()
NDVIRasterDataset$month <- str_sub(NDVIRasterDataset$date, 6, 7) %>% as.numeric()
NDVIRasterDataset <- NDVIRasterDataset %>% dplyr::select(-date)
NDVIRasterDataset <- aggregate(NDVIRasterDataset$NDVI,
                               by = list(NDVIRasterDataset$id, 
                                         NDVIRasterDataset$year, 
                                         NDVIRasterDataset$month), 
                               FUN = "mean", na.rm = T
)
colnames(NDVIRasterDataset) <- c("id", "year", "month", "NDVI")
NDVIRasterDataset$NDVI <- NDVIRasterDataset$NDVI / 10000  #convert into from 1 to -1 
NDVIRasterDataset$NDVI <- NDVIRasterDataset$NDVI * 100 #convert into from 100% to -100% 
NDVIRasterDataset <- NDVIRasterDataset %>% filter(!is.na(NDVI))
save(NDVIRasterDataset, file = "03_RawData/02_NDVIRasterDataset.RData")
rm(NDVIRasterDataset)
gc()

### get temperature
TempRasterFolder <- "F:/13_Article/01_Temperature/"
filelist <- list.files(TempRasterFolder)
TempRasterDataset <- 
  extractPointDataFromRaster(TempRasterFolder, filelist, coords,
                             6, 10, T, "Temp")
TempRasterDataset$Temp <- TempRasterDataset$Temp - 273.16
TempRasterDataset <- TempRasterDataset %>% filter(!is.na(Temp))
save(TempRasterDataset,  file = "03_RawData/03_TempRasterDataset.RData")

### get air pressure
AirPressureRasterFolder <- "F:/13_Article/03_AirPressure/"
filelist <- list.files(AirPressureRasterFolder)
AirPressureRasterDataset <- 
  extractPointDataFromRaster(AirPressureRasterFolder, filelist, coords,
                             13, 17, T, "AirPressure")
AirPressureRasterDataset <- AirPressureRasterDataset %>% filter(!is.na(AirPressure))
save(AirPressureRasterDataset,  file = "03_RawData/04_AirPressureRasterDataset.RData")

### get humidity
HumidityRasterFolder <- "F:/13_Article/04_Humidity/"
filelist <- list.files(HumidityRasterFolder)
HumidityRasterDataset <- 
  extractPointDataFromRaster(HumidityRasterFolder, filelist, coords,
                             10, 14, T, "Humidity")
HumidityRasterDataset <- HumidityRasterDataset %>% filter(!is.na(Humidity))
save(HumidityRasterDataset, file = "03_RawData/05_HumidityRasterDataset.RData")

### get precipitation
PrecipitationRasterFolder <- "F:/13_Article/05_Precipitation/"
filelist <- list.files(PrecipitationRasterFolder)
PrecipitationRasterDataset <- 
  extractPointDataFromRaster(PrecipitationRasterFolder, filelist, coords,
                             10, 14, T, "Precipitation")
PrecipitationRasterDataset <- PrecipitationRasterDataset %>% filter(!is.na(Precipitation))
save(PrecipitationRasterDataset, file = "03_RawData/06_PrecipitationRasterDataset.RData")
