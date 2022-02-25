# Author: M.L.

# input: 01_malariaDataframe.RData
# 01_malariaDataframe.RData: "PfPR": Plasmodium falciparum parasite rate, range 0 ~ 1

# input: 02_NDVIRasterDataset.RData
# 02_NDVIRasterDataset.RData: "NDVI": NDVI value -100% ~ 100% from M*D13C2

# input: 03_TempRasterDataset.RData
# 03_TempRasterDataset.RData: "Temp": Monthly average temperature Kelvin

# input: 04_AirPressureRasterDataset.RData
# 04_AirPressureRasterDataset.RData: "AirPressure" Pa

# input: 05_HumidityRasterDataset.RData
# 05_HumidityRasterDataset.RData: "Humidity" unit is g/g

# input: 06_PrecipitationRasterDataset.RData
# 06_PrecipitationRasterDataset.RData: "Precipitation" g / (cm2 * s)

# input: 07_WindSpeedRasterDataset.RData
# 07_WindSpeedRasterDataset.RData: "WindSpeed" m/s

# input: 08_PopulationDataset.RData
# 08_PopulationDataset.RData: "PopulationDensity" cap/km2

# input: 09_incomeDataset.RData
# 09_incomeDataset.RData: "GDPperCap" USD/Cap

# note: climate data are from
#       https://hydro1.gesdisc.eosdis.nasa.gov/data/GLDAS/GLDAS_NOAH025_M.2.1/doc/README_GLDAS2.pdf

# output: 01_dataset_used.RData
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

# note: From this project, the data base is built in outside disk F:

# end

library(tidyverse)
library(dplyr)
library(stringr)
library(matrixStats)
library(plm)


# malaria
load("03_RawData/01_malariaDataframe.RData")
malaria.dataframe <- malaria.dataframe %>% na.omit()
malaria.dataframe$rowsum <- rowSums(malaria.dataframe[2:21])
malaria.dataframe <- malaria.dataframe %>% filter(rowsum > 0) %>%
  dplyr::select(-rowsum)

malaria.id <- malaria.dataframe %>% dplyr::select(id) %>% distinct()
malaria.id$flag <- 1

malaria.dataframe <- malaria.dataframe %>%
  pivot_longer(!id, names_to = "year", values_to = "PfPR")
malaria.dataframe$year <- malaria.dataframe$year %>% str_sub(6,9) %>% as.numeric()

# temperature
load("03_RawData/03_TempRasterDataset.RData")
Temp29RasterDataset <- TempRasterDataset
TempRasterDataset <- left_join(TempRasterDataset, malaria.id, by = "id")
TempRasterDataset <- TempRasterDataset %>% filter(!is.na(flag)) %>% dplyr::select(-flag)
TempRasterDataset$Temp <- TempRasterDataset$Temp - 273.16
TempRasterDataset <- TempRasterDataset %>% pivot_wider(names_from = "month", values_from = "Temp")
TempRasterDataset$TempMean <- rowMeans(TempRasterDataset[3:14], na.rm = T)
TempRasterDataset$TempSd <- rowSds(as.matrix(TempRasterDataset[3:14]), na.rm = T)
TempRasterDataset <- TempRasterDataset %>% dplyr::select(id, year, TempMean, TempSd)

# NDVI %
load("03_RawData/02_NDVIRasterDataset.RData")
NDVIRasterDataset <- left_join(NDVIRasterDataset, malaria.id, by = "id")
NDVIRasterDataset <- NDVIRasterDataset %>% filter(!is.na(flag)) %>% dplyr::select(-flag)
NDVIRasterDataset$NDVI <- NDVIRasterDataset$NDVI #%
NDVIRasterDataset <- NDVIRasterDataset %>% pivot_wider(names_from = "month", values_from = "NDVI")
NDVIRasterDataset$NDVIMean <- rowMeans(NDVIRasterDataset[3:14], na.rm = T)
NDVIRasterDataset$NDVISd <- rowSds(as.matrix(NDVIRasterDataset[3:14]), na.rm = T)
NDVIRasterDataset <- NDVIRasterDataset %>% dplyr::select(id, year, NDVIMean, NDVISd)


# AirPressure
load("03_RawData/04_AirPressureRasterDataset.RData")
AirPressureRasterDataset <- left_join(AirPressureRasterDataset, malaria.id, by = "id")
AirPressureRasterDataset <- AirPressureRasterDataset %>% filter(!is.na(flag)) %>% dplyr::select(-flag)
AirPressureRasterDataset$AirPressure <- AirPressureRasterDataset$AirPressure / 1000 #kPa
AirPressureRasterDataset <- AirPressureRasterDataset %>% pivot_wider(names_from = "month", values_from = "AirPressure")
AirPressureRasterDataset$AirPressureMean <- rowMeans(AirPressureRasterDataset[3:14], na.rm = T)
AirPressureRasterDataset$AirPressureSd <- rowSds(as.matrix(AirPressureRasterDataset[3:14]), na.rm = T)
AirPressureRasterDataset <- AirPressureRasterDataset %>% dplyr::select(id, year, AirPressureMean, AirPressureSd)

# Humidity unit:g/kg water/air 
load("03_RawData/05_HumidityRasterDataset.RData")
HumidityRasterDataset <- left_join(HumidityRasterDataset, malaria.id, by = "id")
HumidityRasterDataset <- HumidityRasterDataset %>% filter(!is.na(flag)) %>% dplyr::select(-flag)
HumidityRasterDataset$Humidity <- HumidityRasterDataset$Humidity * 1000 #convert to  g/kg
HumidityRasterDataset <- HumidityRasterDataset %>% pivot_wider(names_from = "month", values_from = "Humidity")
HumidityRasterDataset$HumidityMean <- rowMeans(HumidityRasterDataset[3:14], na.rm = T)
HumidityRasterDataset$HumiditySd <- rowSds(as.matrix(HumidityRasterDataset[3:14]), na.rm = T)
HumidityRasterDataset <- HumidityRasterDataset %>% dplyr::select(id, year, HumidityMean, HumiditySd)

# Precipitation g / (m2 * s) 
load("03_RawData/06_PrecipitationRasterDataset.RData")
PrecipitationRasterDataset <- left_join(PrecipitationRasterDataset, malaria.id, by = "id")
PrecipitationRasterDataset <- PrecipitationRasterDataset %>% filter(!is.na(flag)) %>% dplyr::select(-flag)
PrecipitationRasterDataset$Precipitation <- PrecipitationRasterDataset$Precipitation * 1000 * 3600 #convert to  g / (m2 * h) 
PrecipitationRasterDataset <- PrecipitationRasterDataset %>% pivot_wider(names_from = "month", values_from = "Precipitation")
PrecipitationRasterDataset$PrecipitationMean <- rowMeans(PrecipitationRasterDataset[3:14], na.rm = T)
PrecipitationRasterDataset$PrecipitationSd <- rowSds(as.matrix(PrecipitationRasterDataset[3:14]), na.rm = T)
PrecipitationRasterDataset <- PrecipitationRasterDataset %>% dplyr::select(id, year, PrecipitationMean, PrecipitationSd)

# WindSpeed m/s
load("03_RawData/07_WindSpeedRasterDataset.RData")
WindSpeedRasterDataset <- left_join(WindSpeedRasterDataset, malaria.id, by = "id")
WindSpeedRasterDataset <- WindSpeedRasterDataset %>% filter(!is.na(flag)) %>% dplyr::select(-flag)
WindSpeedRasterDataset <- WindSpeedRasterDataset %>% pivot_wider(names_from = "month", values_from = "WindSpeed")
WindSpeedRasterDataset$WindSpeedMean <- rowMeans(WindSpeedRasterDataset[3:14], na.rm = T)
WindSpeedRasterDataset$WindSpeedSd <- rowSds(as.matrix(WindSpeedRasterDataset[3:14]), na.rm = T)
WindSpeedRasterDataset <- WindSpeedRasterDataset %>% dplyr::select(id, year, WindSpeedMean, WindSpeedSd)

# Population m/s
load("03_RawData/08_PopulationDataset.RData")
PopulationRasterDataset <- left_join(PopulationRasterDataset, malaria.id, by = "id")
PopulationRasterDataset <- PopulationRasterDataset %>% filter(!is.na(flag)) %>% dplyr::select(-flag)
PopulationRasterDataset <- PopulationRasterDataset %>%
  pivot_longer(!id, names_to = "year", values_to = "Population")
PopulationRasterDataset$year <- PopulationRasterDataset$year %>% str_sub(6,9) %>% as.numeric()

# GDP per capita USD currency
load("03_RawData/09_incomeDataset.RData")

# temperature to 29 degrees
#load("03_RawData/03_TempRasterDataset.RData")
Temp29RasterDataset <- left_join(Temp29RasterDataset, malaria.id, by = "id")
Temp29RasterDataset <- Temp29RasterDataset %>% filter(!is.na(flag)) %>% dplyr::select(-flag)
Temp29RasterDataset$Temp <- Temp29RasterDataset$Temp - 273.16
Temp29RasterDataset$Temp <- (29 - Temp29RasterDataset$Temp)^2
Temp29RasterDataset <- Temp29RasterDataset %>% pivot_wider(names_from = "month", values_from = "Temp")
Temp29RasterDataset$Temp29Mean <- rowMeans(Temp29RasterDataset[3:14], na.rm = T)
Temp29RasterDataset <- Temp29RasterDataset %>% dplyr::select(id, year, Temp29Mean)

dataset_used <- left_join(malaria.dataframe, TempRasterDataset, by = c("id","year"))  
cor.test(dataset_used$PfPR, dataset_used$TempMean)
cor.test(dataset_used$PfPR, dataset_used$TempSd)
dataset_used <- left_join(dataset_used, Temp29RasterDataset, by = c("id","year"))  
cor.test(dataset_used$PfPR, dataset_used$Temp29Mean)
dataset_used <- left_join(dataset_used, NDVIRasterDataset, by = c("id","year"))  
cor.test(dataset_used$PfPR, dataset_used$NDVIMean)
cor.test(dataset_used$PfPR, dataset_used$NDVISd)
dataset_used <- left_join(dataset_used, AirPressureRasterDataset, by = c("id","year"))  
cor.test(dataset_used$PfPR, dataset_used$AirPressureMean)
cor.test(dataset_used$PfPR, dataset_used$AirPressureSd)
dataset_used <- left_join(dataset_used, HumidityRasterDataset, by = c("id","year"))  
cor.test(dataset_used$PfPR, dataset_used$HumidityMean)
cor.test(dataset_used$PfPR, dataset_used$HumiditySd)
dataset_used <- left_join(dataset_used, PrecipitationRasterDataset, by = c("id","year"))  
cor.test(dataset_used$PfPR, dataset_used$PrecipitationMean)
cor.test(dataset_used$PfPR, dataset_used$PrecipitationSd)
dataset_used <- left_join(dataset_used, WindSpeedRasterDataset, by = c("id","year"))  
cor.test(dataset_used$PfPR, dataset_used$WindSpeedMean)
cor.test(dataset_used$PfPR, dataset_used$WindSpeedSd)
dataset_used <- left_join(dataset_used, PopulationRasterDataset, by = c("id","year"))  
cor.test(dataset_used$PfPR, dataset_used$Population)
dataset_used <- left_join(dataset_used, income, by = c("id","year"))  
cor.test(dataset_used$PfPR, dataset_used$GDPperCap)

dataset_used$TempSquare <- dataset_used$TempMean ^2
save(dataset_used, file = "04_Data/01_dataset_used.RData")
