# Author: M.L.

# end

library(tidyverse)
library(dplyr)
library(sp)
library(tmap)
library(raster)
library("rnaturalearth")
library(ggplot2)

world <- ne_countries(scale = "medium", returnclass = "sp")

title_size = .0001
legend_title_size = 1
margin = 0

# sub panel 1 
load("05_Results/prediction.2040.Rdata")
cols <- c( "blue","green", "gray88","yellow","red")
pal.n.p <- colorRampPalette(cols)
prediction.2040 <- as(prediction.2040, 'SpatialPixelsDataFrame')
prediction.2040 <- as(prediction.2040, "SpatialGridDataFrame")
prediction.2040.245.126 <- prediction.2040
prediction.2040.245.126@data <- prediction.2040.245.126@data %>% dplyr::select(predictPfPR.245.126)
prediction.2040.245.126 <- raster::raster(prediction.2040.245.126)
brks <- c( -0.2, -0.175, -0.15, -0.125, -0.1, -0.075, -0.05, -0.025, 0,
           0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175, 0.2)
labels_brks <- c("-20%", "", "-15%", "", "-10%", "", "-5", "", "0%",
                 "", "5%", "", "10%", "", "15%", "", "20%")
PredictionMap.245.126.2040 <- tm_shape(prediction.2040.245.126) +
  tm_raster("predictPfPR.245.126", palette = pal.n.p(16), breaks = brks, 
            style = 'cont', legend.is.portrait = F, title = "The Difference of PfPRs between SSP1-2.6 and SSP2-4.5 (2021 - 2040)",
            labels = labels_brks, midpoint = 0) +
  tm_shape(world) +
  tm_borders(col = 'black', lwd = 0.5, alpha = 0.8) +
  tm_text("iso_a2", size = legend_title_size * 0.2, remove.overlap = T) + 
  tm_grid(alpha = .25, labels.show = c(F, T)) + 
  tm_ylab("Near Term 2021 - 2040") +
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = NULL,legend.show = F,
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75, 
    main.title = "From SSP1-2.6 to SSP 2-4.5", 
    main.title.position = "center",
    main.title.size = 0.5
  ) 
# sub panel 1 

# sub panel 2
prediction.2040.370.126 <- prediction.2040
prediction.2040.370.126@data <- prediction.2040.370.126@data %>% dplyr::select(predictPfPR.460.126)
prediction.2040.370.126 <- raster::raster(prediction.2040.370.126)
PredictionMap.370.126.2040 <- tm_shape(prediction.2040.370.126) +
  tm_raster("predictPfPR.460.126", palette = pal.n.p(16), breaks = brks, 
            style = 'cont', legend.is.portrait = F, title = "The Difference of PfPRs between SSP1-2.6 and SSP3-7.0 (2021 - 2040)",
            labels = labels_brks, midpoint = 0) +
  tm_shape(world) +
  tm_borders(col = 'black', lwd = 0.5, alpha = 0.8) +
  tm_text("iso_a2", size = legend_title_size * 0.2, remove.overlap = T) + 
  tm_grid(alpha = .25, labels.show = F) + 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = NULL,legend.show = F,
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75,
    main.title = "From SSP1-2.6 to SSP 3-7.0", 
    main.title.position = "center",
    main.title.size = 0.5
  )  
# sub panel 2

# sub panel 3
prediction.2040.585.126 <- prediction.2040
prediction.2040.585.126@data <- prediction.2040.585.126@data %>% dplyr::select(predictPfPR.585.126)
prediction.2040.585.126 <- raster::raster(prediction.2040.585.126)
PredictionMap.585.126.2040 <- tm_shape(prediction.2040.585.126) +
  tm_raster("predictPfPR.585.126", palette = pal.n.p(16), breaks = brks, 
            style = 'cont', legend.is.portrait = F, title = "The Difference of PfPRs between SSP1-2.6 and SSP5-8.5 (2021 - 2040)",
            labels = labels_brks, midpoint = 0) +
  tm_shape(world) +
  tm_borders(col = 'black', lwd = 0.5, alpha = 0.8) +
  tm_text("iso_a2", size = legend_title_size * 0.2, remove.overlap = T) + 
  tm_grid(alpha = .25, labels.show = F) + 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = NULL,legend.show = F,
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75,
    main.title = "From SSP1-2.6 to SSP 3-7.0", 
    main.title.position = "center",
    main.title.size = 0.5
  ) 
# sub panel 3

# sub panel 4 
load("05_Results/prediction.2060.Rdata")
cols <- c( "blue","green", "gray88","yellow","red")
pal.n.p <- colorRampPalette(cols)
prediction.2060 <- as(prediction.2060, 'SpatialPixelsDataFrame')
prediction.2060 <- as(prediction.2060, "SpatialGridDataFrame")
prediction.2060.245.126 <- prediction.2060
prediction.2060.245.126@data <- prediction.2060.245.126@data %>% dplyr::select(predictPfPR.245.126)
prediction.2060.245.126 <- raster::raster(prediction.2060.245.126)
brks <- c( -0.2, -0.175, -0.15, -0.125, -0.1, -0.075, -0.05, -0.025, 0,
           0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175, 0.2)
labels_brks <- c("-20%", "", "-15%", "", "-10%", "", "-5", "", "0%",
                 "", "5%", "", "10%", "", "15%", "", "20%")
PredictionMap.245.126.2060 <- tm_shape(prediction.2060.245.126) +
  tm_raster("predictPfPR.245.126", palette = pal.n.p(16), breaks = brks, 
            style = 'cont', legend.is.portrait = F, title = "The Difference of PfPRs between SSP1-2.6 and SSP2-4.5 (2041 - 2060)",
            labels = labels_brks, midpoint = 0) +
  tm_shape(world) +
  tm_borders(col = 'black', lwd = 0.5, alpha = 0.8) +
  tm_text("iso_a2", size = legend_title_size * 0.2, remove.overlap = T) + 
  tm_grid(alpha = .25, labels.show = c(F, T)) + 
  tm_ylab("Medium Term 2041 - 2060") +
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = NULL,legend.show = F,
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) 
# sub panel 4

# sub panel 5
prediction.2060.370.126 <- prediction.2060
prediction.2060.370.126@data <- prediction.2060.370.126@data %>% dplyr::select(predictPfPR.460.126)
prediction.2060.370.126 <- raster::raster(prediction.2060.370.126)
PredictionMap.370.126.2060 <- tm_shape(prediction.2060.370.126) +
  tm_raster("predictPfPR.460.126", palette = pal.n.p(16), breaks = brks, 
            style = 'cont', legend.is.portrait = F, title = "The Difference of PfPRs between SSP1-2.6 and SSP3-7.0 (2041 - 2060)",
            labels = labels_brks, midpoint = 0) +
  tm_shape(world) +
  tm_borders(col = 'black', lwd = 0.5, alpha = 0.8) +
  tm_text("iso_a2", size = legend_title_size * 0.2, remove.overlap = T) + 
  tm_grid(alpha = .25, labels.show = c(F, F)) + 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = NULL,legend.show = F,
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) 
# sub panel 5

# sub panel 6
prediction.2060.585.126 <- prediction.2060
prediction.2060.585.126@data <- prediction.2060.585.126@data %>% dplyr::select(predictPfPR.585.126)
prediction.2060.585.126 <- raster::raster(prediction.2060.585.126)
PredictionMap.585.126.2060 <- tm_shape(prediction.2060.585.126) +
  tm_raster("predictPfPR.585.126", palette = pal.n.p(16), breaks = brks, 
            style = 'cont', legend.is.portrait = F, title = "The Difference of PfPRs between SSP1-2.6 and SSP5-8.5 (2041 - 2060)",
            labels = labels_brks, midpoint = 0) +
  tm_shape(world) +
  tm_borders(col = 'black', lwd = 0.5, alpha = 0.8) +
  tm_text("iso_a2", size = legend_title_size * 0.2, remove.overlap = T) + 
  tm_grid(alpha = .25, labels.show = c(F, F)) + 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = NULL,legend.show = F,
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) 
# sub panel 6

# sub panel 7
load("05_Results/prediction.2100.Rdata")
cols <- c( "blue","green", "gray88","yellow","red")
pal.n.p <- colorRampPalette(cols)
prediction.2100 <- as(prediction.2100, 'SpatialPixelsDataFrame')
prediction.2100 <- as(prediction.2100, "SpatialGridDataFrame")
prediction.2100.245.126 <- prediction.2100
prediction.2100.245.126@data <- prediction.2100.245.126@data %>% dplyr::select(predictPfPR.245.126)
prediction.2100.245.126 <- raster::raster(prediction.2100.245.126)
brks <- c( -0.2, -0.175, -0.15, -0.125, -0.1, -0.075, -0.05, -0.025, 0,
           0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175, 0.2)
labels_brks <- c("-20%", "", "-15%", "", "-10%", "", "-5", "", "0%",
                 "", "5%", "", "10%", "", "15%", "", "20%")
PredictionMap.245.126.2100 <- tm_shape(prediction.2100.245.126) +
  tm_raster("predictPfPR.245.126", palette = pal.n.p(16), breaks = brks, 
            style = 'cont', legend.is.portrait = F, title = "The Difference of PfPRs between SSP1-2.6 and SSP2-4.5 (2081 - 2100)",
            labels = labels_brks, midpoint = 0) +
  tm_shape(world) +
  tm_borders(col = 'black', lwd = 0.5, alpha = 0.8) +
  tm_text("iso_a2", size = legend_title_size * 0.2, remove.overlap = T) + 
  tm_grid(alpha = .25) + 
  tm_ylab("Long Term 2081 - 2100") +
  tm_scale_bar(position = c("right", "bottom")) + 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = NULL,legend.show = F,
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) 
# sub panel 7

# sub panel 8
prediction.2100.370.126 <- prediction.2100
prediction.2100.370.126@data <- prediction.2100.370.126@data %>% dplyr::select(predictPfPR.460.126)
prediction.2100.370.126 <- raster::raster(prediction.2100.370.126)
PredictionMap.370.126.2100 <- tm_shape(prediction.2100.370.126) +
  tm_raster("predictPfPR.460.126", palette = pal.n.p(16), breaks = brks, 
            style = 'cont', legend.is.portrait = F, title = "The Difference of PfPRs between SSP1-2.6 and SSP3-7.0 (2081 - 2100)",
            labels = labels_brks, midpoint = 0) +
  tm_shape(world) +
  tm_borders(col = 'black', lwd = 0.5, alpha = 0.8) +
  tm_text("iso_a2", size = legend_title_size * 0.2, remove.overlap = T) + 
  tm_grid(alpha = .25, labels.show = c(T, F)) + 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = NULL,legend.show = F,
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) 
# sub panel 8

# sub panel 9
prediction.2100.585.126 <- prediction.2100
prediction.2100.585.126@data <- prediction.2100.585.126@data %>% dplyr::select(predictPfPR.585.126)
prediction.2100.585.126 <- raster::raster(prediction.2100.585.126)
PredictionMap.585.126.2100 <- tm_shape(prediction.2100.585.126) +
  tm_raster("predictPfPR.585.126", palette = pal.n.p(16), breaks = brks, 
            style = 'cont', legend.is.portrait = F, title = "The Difference of PfPRs between Scenarios",
            labels = labels_brks, midpoint = 0) +
  tm_shape(world) +
  tm_borders(col = 'black', lwd = 0.5, alpha = 0.8) +
  tm_text("iso_a2", size = legend_title_size * 0.2, remove.overlap = T) + 
  tm_grid(alpha = .25, labels.show = c(T, F)) + 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size * 2, 
    legend.position = c("right", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) 
# sub panel 9

multi.panel <-
  tmap_arrange(PredictionMap.245.126.2040, PredictionMap.370.126.2040, PredictionMap.585.126.2040,
               PredictionMap.245.126.2060, PredictionMap.370.126.2060, PredictionMap.585.126.2060,
               PredictionMap.245.126.2100, PredictionMap.370.126.2100, PredictionMap.585.126.2100,
               ncol = 3, nrow = 3)
multi.panel %>%
  tmap_save(filename = "06_Figure/S22_multiPanel.jpg", width = 300, height = 140, units = 'mm', dpi = 1000)
