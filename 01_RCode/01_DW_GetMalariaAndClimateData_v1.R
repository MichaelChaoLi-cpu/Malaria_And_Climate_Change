# Author: M.L.

# output: 01_malariaDataframe.RData
# 01_malariaDataframe.RData: "PfPR": Plasmodium falciparum parasite rate, range 0 ~ 1

# output: 02_NDVIRasterDataset.RData
# 02_NDVIRasterDataset.RData: "NDVI": NDVI value -100% ~ 100% from M*D13C2

# output: 03_TempRasterDataset.RData
# 03_TempRasterDataset.RData: "Temp": Monthly average temperature Kelvin

# output: 04_AirPressureRasterDataset.RData
# 04_AirPressureRasterDataset.RData: "AirPressure" Pa

# output: 05_HumidityRasterDataset.RData
# 05_HumidityRasterDataset.RData: "Humidity" unit is g/g

# output: 06_PrecipitationRasterDataset.RData
# 06_PrecipitationRasterDataset.RData: "Precipitation" g / (cm2 * s)

# output: 07_WindSpeedRasterDataset.RData
# 07_WindSpeedRasterDataset.RData: "WindSpeed" m/s

# output: 08_PopulationDataset.RData
# 08_PopulationDataset.RData: "PopulationDensity" cap/km2

# output: 09_incomeDataset.RData
# 09_incomeDataset.RData: "GDPperCap" USD/Cap

# note: From this project, the data base is built in outside disk F:

# end

library(tidyverse)
library(dplyr)
library(sp) 
library(raster)
library(tmap)
library(stringr)
library("rnaturalearth")

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
NDVIRasterDataset <- NDVIRasterDataset %>% filter(!is.na(NDVI))
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
                             15, 19, T, "Precipitation")
PrecipitationRasterDataset <- PrecipitationRasterDataset %>% filter(!is.na(Precipitation))
save(PrecipitationRasterDataset, file = "03_RawData/06_PrecipitationRasterDataset.RData")

### get windSpeed
WindSpeedRasterFolder <- "F:/13_Article/06_WindSpeed/"
filelist <- list.files(WindSpeedRasterFolder)
WindSpeedRasterDataset <- 
  extractPointDataFromRaster(WindSpeedRasterFolder, filelist, coords,
                             11, 15, T, "WindSpeed")
WindSpeedRasterDataset <- WindSpeedRasterDataset %>% filter(!is.na(WindSpeed))
save(WindSpeedRasterDataset, file = "03_RawData/07_WindSpeedRasterDataset.RData")

### get Population
PopulationFolder <- "F:/13_Article/07_Population/WorldPop025Deg/"
filelist <- list.files(PopulationFolder)
year.count <- 1
while (year.count < length(filelist) + 1) {
  filename = filelist[year.count]
  test_tiff <- raster::raster(paste0(PopulationFolder, filename))
  data_ext <- raster::extract(test_tiff, coords)
  coords@data <- cbind(coords@data, data_ext)
  colnames(coords@data)[ncol(coords@data)] <- paste0("Year_", as.character(year.count + 1999))
  year.count <- year.count + 1
}

PopulationRasterDataset <- coords@data
save(PopulationRasterDataset, file = "03_RawData/08_PopulationDataset.RData")
coords <- addcoord(nx,xmin,xsize,ny,ymin,ysize,proj)

# GDP per capita USD currency
incomeCountry <- read.csv("F:/13_Article/08_Income/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_3603754.csv",
                          skip = 4) %>% dplyr::select(Country.Code, X2000:X2019)
#incomeCountry <- incomeCountry %>% pivot_longer(!Country.Code, names_to = "year", values_to = "GDPperCap")
world <- ne_countries(scale = "medium", returnclass = "sp")
world@data <- world@data %>% dplyr::select(iso_a3)
incomeCountry <- incomeCountry %>% rename("iso_a3" = "Country.Code")
world@data <- left_join(world@data, incomeCountry)

income <- over(coords, world) 
income <- income %>% as.data.frame()
income$id <- coords$id
income <- income %>% dplyr::select(-iso_a3)
income <- income %>%  pivot_longer(!id, names_to = "year", values_to = "GDPperCap")
income$year <- income$year %>% str_sub(2,5) %>% as.numeric()
income <- income %>% filter(!is.na(GDPperCap))
save(income, file = "03_RawData/09_incomeDataset.RData")
