# Author: M.L.

# end

library(tidyverse)
library(dplyr)
library(plm)
library(GWPR.light)
library(tmap)
library(sp)

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

## several variables
formula <- PfPR ~ TempMean + TempSquare + TempSd + NDVIMean + Population + GDPperCap
dataset_used <- dataset_used %>% dplyr::select(id, year, all.vars(formula)) %>% na.omit()
dataset.id <- dataset_used %>% dplyr::select(id) %>% distinct()

#get location of point
location <- coordinates(coords) %>% as.data.frame()
location$id <- coords@data$id
location <- left_join(dataset.id, location, by ='id')

# build the dataset
rawCrossValidationDataset <- dataset_used
meanValueOfVariables <- stats::aggregate(rawCrossValidationDataset[,all.vars(formula)],
                                         by = list(rawCrossValidationDataset$id), mean)
colnames(meanValueOfVariables)[1] <- "id"
meanValueOfVariablesCity <- meanValueOfVariables
meanValueOfVariables <- dplyr::left_join(dplyr::select(rawCrossValidationDataset, "id", "year"),
                                         meanValueOfVariables, by = "id")
meanValueOfVariables <- meanValueOfVariables %>% arrange("id", "year")
rawCrossValidationDataset <- rawCrossValidationDataset %>% arrange("id", "year")

#### get FEM Transformation Dataset
femTransformationDataset <- (dplyr::select(rawCrossValidationDataset, -"id", -"year")) - 
  (dplyr::select(meanValueOfVariables, -"id", -"year"))
femTransformationDataset$id <- rawCrossValidationDataset$id
femTransformationDataset$year <- rawCrossValidationDataset$year

#### bw FEM CV
source("01_RCode/05_AF_GWPRRevisedForCrossValidation_v1.R")
bw.GWPR.FEM = 1.15

set.seed(42)

rows <- sample(nrow(femTransformationDataset))
femTransformationDataset <- femTransformationDataset[rows,]

singleFoldNumber <- floor(nrow(femTransformationDataset)/10)
foldNumberth <- 1

CV.F.result.table <- data.frame(Doubles=double(),
                                Ints=integer(),
                                Factors=factor(),
                                Logicals=logical(),
                                Characters=character(),
                                stringsAsFactors=FALSE)

while (foldNumberth < 11){
  meanValueOfVariables.use <- meanValueOfVariables
  if (foldNumberth == 10){
    rows.test <- rows[((foldNumberth-1)*singleFoldNumber+1):nrow(femTransformationDataset)]
  } else {
    rows.test <- rows[((foldNumberth-1)*singleFoldNumber+1):(foldNumberth*singleFoldNumber)]
  }
  
  test <- femTransformationDataset[rows.test,]
  train <- femTransformationDataset[-rows.test,]
  
  trainCode <- train %>%
    dplyr::select(id) %>% distinct()
  
  trainCityLocation <- left_join(trainCode, location, by = "id")
  xy <- trainCityLocation %>% dplyr::select(X, Y)
  trainCityLocationSpatialPoint <- SpatialPointsDataFrame(coords = xy, data = location[,c(1, 2, 3)],
                                                          proj4string = CRS(proj))
  rm(xy)
  # get the train city points 
  
  GWPR.FEM.CV.F.result.CV1 <- GWPR.user(formula = formula, data = train, index = c("id", "year"),
                                        SDF = trainCityLocationSpatialPoint, bw = bw.GWPR.FEM, adaptive = F,
                                        p = 2, effect = "individual", kernel = "bisquare", longlat = F, 
                                        model = "pooling")
  #CVtrain.R2 <- GWPR.FEM.CV.F.result.CV1$R2
  coef.CV1 <- GWPR.FEM.CV.F.result.CV1$SDF@data
  coef.CV1 <- coef.CV1[,1:17]
  colnames(coef.CV1) <- paste0(colnames(coef.CV1), "_Coef")
  colnames(coef.CV1)[1] <- "CityCode"
  colnames(meanValueOfVariables.use) <- paste0(colnames(meanValueOfVariables.use), "_mean")
  colnames(meanValueOfVariables.use)[1] <- "CityCode"
  meanValueOfVariables.use <- meanValueOfVariables.use %>% dplyr::select(-"period_mean") %>% distinct()
  
  train.predict <- left_join(train, coef.CV1, by = "CityCode")
  train.predict <- left_join(train.predict, meanValueOfVariables.use, by = "CityCode")
  train.predict <- train.predict %>%
    mutate(predictNo2 = ug_m2_troposphere_no2_Coef * (ug_m2_troposphere_no2) + 
             ter_pressure_Coef * (ter_pressure) + 
             temp_Coef * temp +
             ndvi_Coef * (ndvi) +
             precipitation_Coef * (precipitation) +
             PBLH_Coef * (PBLH) +
             Y2016_Coef * (Y2016) + Y2017_Coef * (Y2017) +
             Y2018_Coef * (Y2018) + Y2019_Coef * (Y2019) +
             Y2020_Coef * (Y2020) + Y2021_Coef * (Y2021) + no2_measured_ug.m3_mean
    )
  train.predict$no2_measured_ug.m3.ori <- train.predict$no2_measured_ug.m3 + train.predict$no2_measured_ug.m3_mean
  #ss.tot <- sum((train.predict$no2_measured_ug.m3.ori - mean(train.predict$no2_measured_ug.m3.ori))^2)
  ss.tot <- sum((train.predict$no2_measured_ug.m3.ori - mean(train.predict$no2_measured_ug.m3))^2)
  ss.res <- sum((train.predict$no2_measured_ug.m3.ori - train.predict$predictNo2)^2)
  CVtrain.R2 <- 1 - ss.res/ss.tot
  reg <- lm(predictNo2 ~ no2_measured_ug.m3.ori, data = train.predict)
  coeff.train = coefficients(reg)
  N.train = length(train.predict$predictNo2)
  corre.train <- cor(train.predict$predictNo2, train.predict$no2_measured_ug.m3.ori)
  rmse.train <- sqrt(ss.res/nrow(train.predict)) 
  mae.train <- mean(abs(train.predict$no2_measured_ug.m3.ori - train.predict$predictNo2))
  
  test.predict <- left_join(test, coef.CV1, by = "CityCode")
  test.predict <- left_join(test.predict, meanValueOfVariables.use, by = "CityCode")
  test.predict <- test.predict %>%
    mutate(predictNo2 = ug_m2_troposphere_no2_Coef * (ug_m2_troposphere_no2) + 
             ter_pressure_Coef * (ter_pressure) + 
             temp_Coef * temp +
             ndvi_Coef * (ndvi) +
             precipitation_Coef * (precipitation) +
             PBLH_Coef * (PBLH) +
             Y2016_Coef * (Y2016) + Y2017_Coef * (Y2017) +
             Y2018_Coef * (Y2018) + Y2019_Coef * (Y2019) +
             Y2020_Coef * (Y2020) + Y2021_Coef * (Y2021) + no2_measured_ug.m3_mean
    )
  test.predict$no2_measured_ug.m3.ori <- test.predict$no2_measured_ug.m3 + test.predict$no2_measured_ug.m3_mean
  #ss.tot <- sum((test.predict$no2_measured_ug.m3.ori - mean(test.predict$no2_measured_ug.m3.ori))^2)
  ss.tot <- sum((test.predict$no2_measured_ug.m3.ori - mean(test.predict$no2_measured_ug.m3))^2)
  ss.res <- sum((test.predict$no2_measured_ug.m3.ori - test.predict$predictNo2)^2)
  CVtest.R2 <- 1 - ss.res/ss.tot
  reg <- lm(predictNo2 ~ no2_measured_ug.m3.ori, data = test.predict)
  coeff.test = coefficients(reg)
  N.test = length(test.predict$predictNo2)
  corre.test <- cor(test.predict$predictNo2, test.predict$no2_measured_ug.m3.ori)
  rmse.test <- sqrt(ss.res/nrow(test.predict))
  mae.test <- mean(abs(test.predict$no2_measured_ug.m3.ori - test.predict$predictNo2))
  
  result <- c(foldNumberth, CVtrain.R2, coeff.train, N.train, corre.train, rmse.train, mae.train,
              CVtest.R2, coeff.test, N.test, corre.test, rmse.test, mae.test)
  print(result)
  CV.A.result.table <- rbind(CV.A.result.table, result)
  foldNumberth <- foldNumberth + 1
}
colnames(CV.A.result.table) <- c("foldNumber", "CVtrain.R2", "train.inter", "train.slope", "N.train", "corre.train",
                                 "rmse.train", "mae.train",
                                 "CVtest.R2", "test.inter", "test.slope", "N.test", "corre.test", "rmse.test", "mae.test")
save(CV.A.result.table, file = "04_Results/AdaptivefemCrossValidation.Rdata")
write.csv(CV.A.result.table, file = "08_Tables/AdaptivefemCrossValidation.csv")