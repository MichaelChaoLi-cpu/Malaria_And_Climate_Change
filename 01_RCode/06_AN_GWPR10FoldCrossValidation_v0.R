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

# output: femCrossValidation.Rdata
# femCrossValidation.Rdata: "foldNumber" the order of the fold
# femCrossValidation.Rdata: "CVtrain.R2" R2 of train data set
# femCrossValidation.Rdata: "train.inter" the intercept of train data
# femCrossValidation.Rdata: "train.slope" the slope of train data
# femCrossValidation.Rdata: "N.train" data size N
# femCrossValidation.Rdata: "corre.train" the correlation coefficient in train data
# femCrossValidation.Rdata: "rmse.train" rmse 
# femCrossValidation.Rdata: "mae.train" mae 
# femCrossValidation.Rdata: "CVtest.R2"
# femCrossValidation.Rdata: "test.inter"
# femCrossValidation.Rdata: "test.slope"
# femCrossValidation.Rdata: "N.test"
# femCrossValidation.Rdata: "corre.test"
# femCrossValidation.Rdata: "rmse.test"
# femCrossValidation.Rdata: "mae.test"

# note: the bandwidth is set to 4.25

# note: when we perform FEM Cross Validation, we should care about the formula in the function (+ 0)

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
formula.CV.FEM <- PfPR ~ TempMean + TempSquare + TempSd + NDVIMean + Population + GDPperCap + 0
###### it must be careful, if cross-validation in FEM the formula must be revised + 0 ^^^
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
bw.GWPR.FEM = 4.25

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
  meanValueOfVariables.use <- meanValueOfVariables %>% dplyr::select(-year) %>% distinct()
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
  
  GWPR.FEM.CV.F.result.CV1 <- GWPR.user(formula = formula.CV.FEM, data = train, index = c("id", "year"),
                                        SDF = trainCityLocationSpatialPoint, bw = bw.GWPR.FEM, adaptive = F,
                                        p = 2, effect = "individual", kernel = "bisquare", longlat = F, 
                                        model = "pooling")
  residual.result <- GWPR.FEM.CV.F.result.CV1$GWPR.residuals
  colnames(meanValueOfVariables.use) <- paste0(colnames(meanValueOfVariables.use), "_mean")
  colnames(meanValueOfVariables.use)[1] <- "id"
  meanValueOfVariables.use <- meanValueOfVariables.use %>% dplyr::select(id, PfPR_mean)
  residual.result <- left_join(residual.result, meanValueOfVariables.use, by = "id")
  residual.result$yhat.fem <- residual.result$yhat + residual.result$PfPR_mean
  residual.result$y.ori <- residual.result$y + residual.result$PfPR_mean
  ss.tot <- sum((residual.result$y.ori - mean(residual.result$y.ori))^2)
  ss.res <- sum((residual.result$y.ori - residual.result$yhat.fem)^2)
  CVtrain.R2 <- 1 - ss.res/ss.tot
  
  reg <- lm(yhat.fem ~ y.ori, data = residual.result)
  coeff.train = coefficients(reg)
  N.train = length(residual.result$y.ori)
  corre.train <- cor(residual.result$y.ori, residual.result$yhat.fem)
  rmse.train <- sqrt(ss.res/nrow(residual.result)) 
  mae.train <- mean(abs(residual.result$y.ori - residual.result$yhat.fem))
  
  coef.CV1 <- GWPR.FEM.CV.F.result.CV1$SDF@data
  coef.CV1 <- coef.CV1[,1:7]
  colnames(coef.CV1) <- paste0(colnames(coef.CV1), "_Coef")
  colnames(coef.CV1)[1] <- "id"
  test.predict <- left_join(test, coef.CV1, by = "id")
  test.predict <- left_join(test.predict, meanValueOfVariables.use, by = "id")
  test.predict <- test.predict %>%
    mutate(predictPfPR = TempMean_Coef * (TempMean) + 
             TempSquare_Coef * (TempSquare) +
             TempSd_Coef * (TempSd) + NDVIMean_Coef * NDVIMean +
             Population_Coef * (Population) + GDPperCap_Coef * (GDPperCap) +
             PfPR_mean
    )
  test.predict$PfPR.ori <- test.predict$PfPR + test.predict$PfPR_mean
  ss.tot <- sum((test.predict$PfPR.ori - mean(test.predict$PfPR.ori))^2)
  ss.res <- sum((test.predict$PfPR.ori - test.predict$predictPfPR)^2)
  CVtest.R2 <- 1 - ss.res/ss.tot
  reg <- lm(predictPfPR ~ PfPR.ori, data = test.predict)
  coeff.test = coefficients(reg)
  N.test = length(test.predict$predictPfPR)
  corre.test <- cor(test.predict$predictPfPR, test.predict$PfPR.ori)
  rmse.test <- sqrt(ss.res/nrow(test.predict))
  mae.test <- mean(abs(test.predict$PfPR.ori - test.predict$predictPfPR))
  
  result <- c(foldNumberth, CVtrain.R2, coeff.train, N.train, corre.train, rmse.train, mae.train,
              CVtest.R2, coeff.test, N.test, corre.test, rmse.test, mae.test)
  print(result)
  CV.F.result.table <- rbind(CV.F.result.table, result)
  foldNumberth <- foldNumberth + 1
}
colnames(CV.F.result.table) <- c("foldNumber", "CVtrain.R2", "train.inter", "train.slope", "N.train", "corre.train",
                                 "rmse.train", "mae.train",
                                 "CVtest.R2", "test.inter", "test.slope", "N.test", "corre.test", "rmse.test", "mae.test")
save(CV.F.result.table, file = "05_Results/femCrossValidation.Rdata")
write.csv(CV.F.result.table, file = "05_Results/femCrossValidation.csv")
