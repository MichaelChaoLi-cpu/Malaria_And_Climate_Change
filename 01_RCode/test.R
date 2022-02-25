library(tmap)

pal <- colorRampPalette(c("blue","green","yellow","red"))
brks <- c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6)

TempMeanMap <- tm_shape(GWPR.FEM.CV.F.result$SDF) + 
  tm_dots(col = "TempMean", palette = "RdBu", size = 0.001)
TempSquareMap <- tm_shape(GWPR.FEM.CV.F.result$SDF) + 
  tm_dots(col = "TempSquare", palette = "RdBu", size = 0.001)
TempSdMap <- tm_shape(GWPR.FEM.CV.F.result$SDF) + 
  tm_dots(col = "TempSd", palette = "RdBu", size = 0.001)

TempMeanMap %>%
  tmap_save(filename = "06_Figure/01_TempMeanMap.jpg" ,width = 210, height = 120, units = 'mm', dpi = 1000)
TempSquareMap %>%
  tmap_save(filename = "06_Figure/02_TempSquareMap.jpg" ,width = 210, height = 120, units = 'mm', dpi = 1000)
TempSdMap %>%
  tmap_save(filename = "06_Figure/03_TempSdMap.jpg" ,width = 210, height = 120, units = 'mm', dpi = 1000)


PfPRMap <- tm_shape(GWPR.FEM.CV.F.result$SDF) + 
  tm_dots(col = "PfPR", palette = pal(10), size = 0.0001)
PfPRMap%>%
  tmap_save(filename = "06_Figure/00_PfPRMap.ssp126.jpg" ,width = 210, height = 120, units = 'mm', dpi = 1000)

PredictionMap.spp245 <- tm_shape(GWPR.FEM.CV.F.result$SDF) + 
  tm_dots(col = "predictPfPR126_245", breaks = brks, palette = pal(6), size = 0.0001)
PredictionMap.spp245 %>%
  tmap_save(filename = "06_Figure/05_PredictMap.spp245.jpg", width = 210, height = 120, units = 'mm', dpi = 1000)

PredictionMap.spp370 <- tm_shape(GWPR.FEM.CV.F.result$SDF) + 
  tm_dots(col = "predictPfPR126_370", breaks = brks, palette = pal(6), size = 0.0001)
PredictionMap.spp370 %>%
  tmap_save(filename = "06_Figure/06_PredictMap.spp370.jpg", width = 210, height = 120, units = 'mm', dpi = 1000)

PredictionMap.spp585 <- tm_shape(GWPR.FEM.CV.F.result$SDF) + 
  tm_dots(col = "predictPfPR126_585", breaks = brks, palette = pal(6), size = 0.0001)
PredictionMap.spp585 %>%
  tmap_save(filename = "06_Figure/07_PredictMap.spp585.jpg", width = 210, height = 120, units = 'mm', dpi = 1000)

brks = c(-0.05, -0.04, -0.03, -0.02, -0.01, 0, 0.01, 0.02)
NDVI.map <- tm_shape(GWPR.FEM.CV.F.result$SDF) + 
  tm_dots(col = "NDVIMean", breaks = brks ,
          palette = pal(7), size = 0.0001)
NDVI.map %>%
  tmap_save(filename = "06_Figure/08_NDVIMap.jpg", width = 210, height = 120, units = 'mm', dpi = 1000)
