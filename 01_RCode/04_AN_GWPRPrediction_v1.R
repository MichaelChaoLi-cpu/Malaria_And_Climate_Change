# Author: M.L.

# input: GWPR_FEM_CV_F_result_425.Rdata
# GWPR_FEM_CV_F_result_425.Rdata: this is a result of GWPR based on FEM with 4.25

# input: TempRasterDataset.ssp***
# note: these data are from BCC-CSM2-MR model based on ssp123 245 370 585, resolution is 1 degree
#       https://interactive-atlas.ipcc.ch/

# output: prediction.2040.Rdata
# prediction.2040.Rdata: "predictPfPR.245.126" marginal effects from ssp126 to ssp245
# prediction.2040.Rdata: "predictPfPR.460.126" marginal effects from ssp126 to ssp370 
# note: here "460" is a typo, it should 370. the period is from 2021 to 2040
# prediction.2040.Rdata: "predictPfPR.585.126" marginal effects from ssp126 to ssp585 
# prediction.2040.Rdata: "PfPR_incr_2040_245.126" the increase malaria infection 
#                        due to temperature change from ssp 1-2.6 to 2-4.6 (capita)  
# prediction.2040.Rdata: "PfPR_incr_2040_460.126" the increase malaria infection 
#                        due to temperature change from ssp 1-2.6 to 3-7.0 (capita)  
# prediction.2040.Rdata: "PfPR_incr_2040_585.126" the increase malaria infection 
#                        due to temperature change from ssp 1-2.6 to 5-8.5 (capita)  

# output: prediction.2060.Rdata
# prediction.2060.Rdata: "predictPfPR.245.126" marginal effects from ssp126 to ssp245
# prediction.2060.Rdata: "predictPfPR.460.126" marginal effects from ssp126 to ssp370 
# note: here "460" is a typo, it should 370. the period is from 2041 to 2060
# prediction.2060.Rdata: "predictPfPR.585.126" marginal effects from ssp126 to ssp585 
# prediction.2060.Rdata: "PfPR_incr_2060_245.126" the increase malaria infection 
#                        due to temperature change from ssp 1-2.6 to 2-4.6 (capita)  
# prediction.2060.Rdata: "PfPR_incr_2060_460.126" the increase malaria infection 
#                        due to temperature change from ssp 1-2.6 to 3-7.0 (capita)  
# prediction.2060.Rdata: "PfPR_incr_2060_585.126" the increase malaria infection 
#                        due to temperature change from ssp 1-2.6 to 5-8.5 (capita)

# output: prediction.2100.Rdata
# prediction.2100.Rdata: "predictPfPR.245.126" marginal effects from ssp126 to ssp245
# prediction.2100.Rdata: "predictPfPR.460.126" marginal effects from ssp126 to ssp370 
# note: here "460" is a typo, it should 370. the period is from 2081 to 2100
# prediction.2100.Rdata: "predictPfPR.585.126" marginal effects from ssp126 to ssp585
# prediction.2100.Rdata: "PfPR_incr_2100_245.126" the increase malaria infection 
#                        due to temperature change from ssp 1-2.6 to 2-4.6 (capita)  
# prediction.2100.Rdata: "PfPR_incr_2100_460.126" the increase malaria infection 
#                        due to temperature change from ssp 1-2.6 to 3-7.0 (capita)  
# prediction.2100.Rdata: "PfPR_incr_2100_585.126" the increase malaria infection 
#                        due to temperature change from ssp 1-2.6 to 5-8.5 (capita)

# output: symmetry_distribution.Rdata
# symmetry_distribution.Rdata: "distribution_category" discribe the shape of symmetry

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

TempRasterFolder <- "F:/13_Article/09_TempPrediction/IPCC1degree/"
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
  filelist.sub <- filelist[(num*12-11):(num*12)]
  year_num <- floor((num-1)/4)
  scenario.num <- (num-1)%%4 + 1
  TempRasterDataset <- 
    extractPointDataFromRasterMultiband(TempRasterFolder, filelist.sub, coords,
                                        year_num, F, "Temp", 1)
  TempRasterDataset <- TempRasterDataset %>% na.omit()
  
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


year2040.mean <- future.temperature %>%
  filter(year == 2) %>% dplyr::select(id, TempMean, scenario) %>%
  pivot_wider(names_from = scenario, values_from = TempMean)
year2040.square <- year2040.mean
year2040.square[2:5] <- year2040.square[2:5]^2 
year2040.sd <- future.temperature %>%
  filter(year == 2) %>% dplyr::select(id, TempSD, scenario) %>%
  pivot_wider(names_from = scenario, values_from = TempSD)
year2040.mean <- year2040.mean %>%
  mutate(mean.dif.245.126 = ssp245 - ssp126,
         mean.dif.460.126 = ssp460 - ssp126,
         mean.dif.585.126 = ssp585 - ssp126) %>%
  dplyr::select(id, mean.dif.245.126, mean.dif.460.126, mean.dif.585.126)
year2040.square <- year2040.square %>%
  mutate(square.dif.245.126 = ssp245 - ssp126,
         square.dif.460.126 = ssp460 - ssp126,
         square.dif.585.126 = ssp585 - ssp126) %>%
  dplyr::select(id, square.dif.245.126, square.dif.460.126, square.dif.585.126)
year2040.sd <- year2040.sd %>%
  mutate(sd.dif.245.126 = ssp245 - ssp126,
         sd.dif.460.126 = ssp460 - ssp126,
         sd.dif.585.126 = ssp585 - ssp126) %>%
  dplyr::select(id, sd.dif.245.126, sd.dif.460.126, sd.dif.585.126)
year2040.dataset <- left_join(year2040.mean, year2040.square)
year2040.dataset <- left_join(year2040.dataset, year2040.sd)
rm(year2040.mean, year2040.square, year2040.sd)

prediction.2040 <- coef.SDF
prediction.2040@data <- left_join(prediction.2040@data, year2040.dataset)
prediction.2040@data <- prediction.2040@data %>%
  mutate(
    predictPfPR.245.126 = TempMean * mean.dif.245.126 + TempSquare * square.dif.245.126 +
      TempSd * sd.dif.245.126,
    predictPfPR.460.126 = TempMean * mean.dif.460.126 + TempSquare * square.dif.460.126 +
      TempSd * sd.dif.460.126,
    predictPfPR.585.126 = TempMean * mean.dif.585.126 + TempSquare * square.dif.585.126 +
      TempSd * sd.dif.585.126 )

save(prediction.2040, file = "05_Results/prediction.2040.Rdata")

year2060.mean <- future.temperature %>%
  filter(year == 1) %>% dplyr::select(id, TempMean, scenario) %>%
  pivot_wider(names_from = scenario, values_from = TempMean)
year2060.square <- year2060.mean
year2060.square[2:5] <- year2060.square[2:5]^2 
year2060.sd <- future.temperature %>%
  filter(year == 1) %>% dplyr::select(id, TempSD, scenario) %>%
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

year2100.mean <- future.temperature %>%
  filter(year == 0) %>% dplyr::select(id, TempMean, scenario) %>%
  pivot_wider(names_from = scenario, values_from = TempMean)
year2100.square <- year2100.mean
year2100.square[2:5] <- year2100.square[2:5]^2 
year2100.sd <- future.temperature %>%
  filter(year == 0) %>% dplyr::select(id, TempSD, scenario) %>%
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

#symmetry distribution
symmetry_distribution <- coef.SDF
symmetry_distribution$symmetry <- - (symmetry_distribution$TempMean/2/symmetry_distribution$TempSquare) 
symmetry_distribution$u_shape <- ifelse(symmetry_distribution$TempSquare > 0, 1, 0)
symmetry_distribution$symmetry_over_50 <- ifelse(symmetry_distribution$symmetry > 50, 10, 0)
symmetry_distribution$symmetry_below_0 <- ifelse(symmetry_distribution$symmetry < 10, 20, 0)
symmetry_distribution$distribution_category <- symmetry_distribution$u_shape + 
  symmetry_distribution$symmetry_over_50 + symmetry_distribution$symmetry_below_0
symmetry_distribution$distribution_category <- symmetry_distribution$distribution_category %>% as.factor()
save(symmetry_distribution, file = "05_Results/symmetry_distribution.Rdata")
#symmetry distribution

#future temperature grids
id.dataset <- GWPR.FEM.CV.F.result$SDF@data %>% dplyr::select(id)
id.dataset$flag <- 1
future.temperature.grid <- left_join(future.temperature, id.dataset)
future.temperature.grid <- future.temperature.grid %>% filter(flag == 1)

future.temperature.summary <- future.temperature.grid %>%
  group_by(year, scenario) %>% 
  summarise(across(c(TempMean, TempSD), list(mean = mean, min = min,
                                             median = median, max = max)))

#### increased population prediction
RasterFolder <- "F:/13_Article/10_PopulationPrediction/"
#### 2040
load("05_Results/prediction.2040.Rdata")

filename = "CMIP6 - Population density  persons_km__2 - Near Term (2021-2040) SSP2  - Annual .tiff"
population_2040_ssp2_tiff <- raster::raster(paste0(RasterFolder, filename))
data_ext <- raster::extract(population_2040_ssp2_tiff, prediction.2040)
#data_ext <- data_ext * 770
prediction.2040@data$pop_2040_ssp2 <- data_ext * 770
prediction.2040@data$pop_den_2040_ssp2 <- data_ext

filename = "CMIP6 - Population density  persons_km__2 - Near Term (2021-2040) SSP3  - Annual .tiff"
population_2040_ssp3_tiff <- raster::raster(paste0(RasterFolder, filename))
data_ext <- raster::extract(population_2040_ssp3_tiff, prediction.2040)
#data_ext <- data_ext * 770
prediction.2040@data$pop_2040_ssp3 <- data_ext * 770
prediction.2040@data$pop_den_2040_ssp3 <- data_ext

filename = "CMIP6 - Population density  persons_km__2 - Near Term (2021-2040) SSP5  - Annual .tiff"
population_2040_ssp5_tiff <- raster::raster(paste0(RasterFolder, filename))
data_ext <- raster::extract(population_2040_ssp5_tiff, prediction.2040)
#data_ext <- data_ext * 770
prediction.2040@data$pop_2040_ssp5 <- data_ext * 770
prediction.2040@data$pop_den_2040_ssp5 <- data_ext

prediction.2040@data$PfPR_incr_2040_245.126 <- prediction.2040@data$pop_2040_ssp2 *
  prediction.2040@data$predictPfPR.245.126
prediction.2040@data$PfPR_incr_2040_460.126 <- prediction.2040@data$pop_2040_ssp3 *
  prediction.2040@data$predictPfPR.460.126
prediction.2040@data$PfPR_incr_2040_585.126 <- prediction.2040@data$pop_2040_ssp5 *
  prediction.2040@data$predictPfPR.585.126

prediction.2040@data$PfPR_incr_2040_245.126_den <- prediction.2040@data$pop_den_2040_ssp2 *
  prediction.2040@data$predictPfPR.245.126
prediction.2040@data$PfPR_incr_2040_460.126_den <- prediction.2040@data$pop_den_2040_ssp3 *
  prediction.2040@data$predictPfPR.460.126
prediction.2040@data$PfPR_incr_2040_585.126_den <- prediction.2040@data$pop_den_2040_ssp5 *
  prediction.2040@data$predictPfPR.585.126

save(prediction.2040, file = "05_Results/prediction.2040.Rdata")

#### 2060
load("05_Results/prediction.2060.Rdata")

filename = "CMIP6 - Population density  persons_km__2 - Medium Term (2041-2060) SSP2  - Annual .tiff"
population_2060_ssp2_tiff <- raster::raster(paste0(RasterFolder, filename))
data_ext <- raster::extract(population_2060_ssp2_tiff, prediction.2060)
#data_ext <- data_ext * 770
prediction.2060@data$pop_2060_ssp2 <- data_ext * 770
prediction.2060@data$pop_den_2060_ssp2 <- data_ext

filename = "CMIP6 - Population density  persons_km__2 - Medium Term (2041-2060) SSP3  - Annual .tiff"
population_2060_ssp3_tiff <- raster::raster(paste0(RasterFolder, filename))
data_ext <- raster::extract(population_2060_ssp3_tiff, prediction.2060)
#data_ext <- data_ext * 770
prediction.2060@data$pop_2060_ssp3 <- data_ext * 770
prediction.2060@data$pop_den_2060_ssp3 <- data_ext

filename = "CMIP6 - Population density  persons_km__2 - Medium Term (2041-2060) SSP5  - Annual .tiff"
population_2060_ssp5_tiff <- raster::raster(paste0(RasterFolder, filename))
data_ext <- raster::extract(population_2060_ssp5_tiff, prediction.2060)
#data_ext <- data_ext * 770
prediction.2060@data$pop_2060_ssp5 <- data_ext * 770
prediction.2060@data$pop_den_2060_ssp5 <- data_ext

prediction.2060@data$PfPR_incr_2060_245.126 <- prediction.2060@data$pop_2060_ssp2 *
  prediction.2060@data$predictPfPR.245.126
prediction.2060@data$PfPR_incr_2060_460.126 <- prediction.2060@data$pop_2060_ssp3 *
  prediction.2060@data$predictPfPR.460.126
prediction.2060@data$PfPR_incr_2060_585.126 <- prediction.2060@data$pop_2060_ssp5 *
  prediction.2060@data$predictPfPR.585.126

prediction.2060@data$PfPR_incr_2060_245.126_den <- prediction.2060@data$pop_den_2060_ssp2 *
  prediction.2060@data$predictPfPR.245.126
prediction.2060@data$PfPR_incr_2060_460.126_den <- prediction.2060@data$pop_den_2060_ssp3 *
  prediction.2060@data$predictPfPR.460.126
prediction.2060@data$PfPR_incr_2060_585.126_den <- prediction.2060@data$pop_den_2060_ssp5 *
  prediction.2060@data$predictPfPR.585.126

save(prediction.2060, file = "05_Results/prediction.2060.Rdata")

#### 2100
load("05_Results/prediction.2100.Rdata")

filename = "CMIP6 - Population density  persons_km__2 - Long Term (2081-2100) SSP2  - Annual .tiff"
population_2100_ssp2_tiff <- raster::raster(paste0(RasterFolder, filename))
data_ext <- raster::extract(population_2100_ssp2_tiff, prediction.2100)
#data_ext <- data_ext * 770
prediction.2100@data$pop_2100_ssp2 <- data_ext * 770
prediction.2100@data$pop_den_2100_ssp2 <- data_ext

filename = "CMIP6 - Population density  persons_km__2 - Long Term (2081-2100) SSP3  - Annual .tiff"
population_2100_ssp3_tiff <- raster::raster(paste0(RasterFolder, filename))
data_ext <- raster::extract(population_2100_ssp3_tiff, prediction.2100)
#data_ext <- data_ext * 770
prediction.2100@data$pop_2100_ssp3 <- data_ext * 770
prediction.2100@data$pop_den_2100_ssp3 <- data_ext

filename = "CMIP6 - Population density  persons_km__2 - Long Term (2081-2100) SSP5  - Annual .tiff"
population_2100_ssp5_tiff <- raster::raster(paste0(RasterFolder, filename))
data_ext <- raster::extract(population_2100_ssp5_tiff, prediction.2100)
#data_ext <- data_ext * 770
prediction.2100@data$pop_2100_ssp5 <- data_ext * 770
prediction.2100@data$pop_den_2100_ssp5 <- data_ext 

prediction.2100@data$PfPR_incr_2100_245.126 <- prediction.2100@data$pop_2100_ssp2 *
  prediction.2100@data$predictPfPR.245.126
prediction.2100@data$PfPR_incr_2100_460.126 <- prediction.2100@data$pop_2100_ssp3 *
  prediction.2100@data$predictPfPR.460.126
prediction.2100@data$PfPR_incr_2100_585.126 <- prediction.2100@data$pop_2100_ssp5 *
  prediction.2100@data$predictPfPR.585.126

prediction.2100@data$PfPR_incr_2100_245.126_den <- prediction.2100@data$pop_den_2100_ssp2 *
  prediction.2100@data$predictPfPR.245.126
prediction.2100@data$PfPR_incr_2100_460.126_den <- prediction.2100@data$pop_den_2100_ssp3 *
  prediction.2100@data$predictPfPR.460.126
prediction.2100@data$PfPR_incr_2100_585.126_den <- prediction.2100@data$pop_den_2100_ssp5 *
  prediction.2100@data$predictPfPR.585.126

save(prediction.2100, file = "05_Results/prediction.2100.Rdata")
