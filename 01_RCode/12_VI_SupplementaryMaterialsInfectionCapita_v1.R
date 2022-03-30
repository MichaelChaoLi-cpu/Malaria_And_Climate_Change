# Author: M.L.

# output: Figure Sxx
# Note: These figures are about the malaria infection increase.

# end

library(tidyverse)
library(dplyr)
library(sp)
library(tmap)
library(raster)
library("rnaturalearth")
library(ggplot2)

# mean PfPR figure S1
world <- ne_countries(scale = "medium", returnclass = "sp")

title_size = .0001
legend_title_size = 1
margin = 0

# figure S23
load("05_Results/prediction.2040.Rdata")
cols <- c( "blue","green", "gray88","yellow","red")
pal.n.p <- colorRampPalette(cols)
prediction.2040 <- as(prediction.2040, 'SpatialPixelsDataFrame')
prediction.2040 <- as(prediction.2040, "SpatialGridDataFrame")
prediction.2040.245.126 <- prediction.2040
prediction.2040.245.126@data <- prediction.2040.245.126@data %>% dplyr::select(PfPR_incr_2040_245.126_den)
prediction.2040.245.126 <- raster::raster(prediction.2040.245.126)
hist(prediction.2040.245.126)
brks <- c( -20, -17.5, -15, -12.5, -10, -7.5, -5, -2.5, 0,
           2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20)
labels_brks <- c("-20", "", "-15", "", "-10", "", "-5", "", "0",
                 "", "5", "", "10", "", "15", "", "20")
PredictionMap.245.126.2040 <- tm_shape(prediction.2040.245.126) +
  tm_raster("PfPR_incr_2040_245.126_den", palette = pal.n.p(16), breaks = brks, 
            style = 'cont', legend.is.portrait = F, 
            title = "The Difference in Infection Cases between SSP1-2.6 and SSP2-4.5\n(2021 - 2040, Unit: Cases/km2)",
            labels = labels_brks, midpoint = 0) +
  tm_shape(world) +
  tm_borders(col = 'black', lwd = 0.5, alpha = 0.8) +
  tm_text("iso_a2", size = legend_title_size * 0.5, remove.overlap = T) + 
  tm_grid(alpha = .25) + 
  tm_scale_bar(position = c("right", "bottom")) + 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("right", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) 
PredictionMap.245.126.2040 %>%
  tmap_save(filename = "06_Figure/S23_PredictMap.Den.245.126.2040.jpg", width = 300, height = 140, units = 'mm', dpi = 1000)
# figure S23 

# figure S24
prediction.2040.370.126 <- prediction.2040
prediction.2040.370.126@data <- prediction.2040.370.126@data %>% dplyr::select(PfPR_incr_2040_460.126_den)
prediction.2040.370.126 <- raster::raster(prediction.2040.370.126)
hist(prediction.2040.370.126)
brks <- c( -20, -17.5, -15, -12.5, -10, -7.5, -5, -2.5, 0,
           2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20)
labels_brks <- c("-20", "", "-15", "", "-10", "", "-5", "", "0",
                 "", "5", "", "10", "", "15", "", "20")
PredictionMap.370.126.2040 <- tm_shape(prediction.2040.370.126) +
  tm_raster("PfPR_incr_2040_460.126_den", palette = pal.n.p(16), breaks = brks, 
            style = 'cont', legend.is.portrait = F, 
            title = "The Difference in Infection Cases between SSP1-2.6 and SSP3-7.0\n(2021 - 2040, Unit: Cases/km2)",
            labels = labels_brks, midpoint = 0) +
  tm_shape(world) +
  tm_borders(col = 'black', lwd = 0.5, alpha = 0.8) +
  tm_text("iso_a2", size = legend_title_size * 0.5, remove.overlap = T) + 
  tm_grid(alpha = .25) + 
  tm_scale_bar(position = c("right", "bottom")) + 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("right", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) 
PredictionMap.370.126.2040 %>%
  tmap_save(filename = "06_Figure/S24_PredictMap.Den.370.126.2040.jpg", width = 300, height = 140, units = 'mm', dpi = 1000)
# figure S24

# figure S25
prediction.2040.585.126 <- prediction.2040
prediction.2040.585.126@data <- prediction.2040.585.126@data %>% dplyr::select(PfPR_incr_2040_585.126_den)
prediction.2040.585.126 <- raster::raster(prediction.2040.585.126)
hist(prediction.2040.585.126)
brks <- c( -20, -17.5, -15, -12.5, -10, -7.5, -5, -2.5, 0,
           2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20)
labels_brks <- c("-20", "", "-15", "", "-10", "", "-5", "", "0",
                 "", "5", "", "10", "", "15", "", "20")
PredictionMap.585.126.2040 <- tm_shape(prediction.2040.585.126) +
  tm_raster("PfPR_incr_2040_585.126_den", palette = pal.n.p(16), breaks = brks, 
            style = 'cont', legend.is.portrait = F, 
            title = "The Difference in Infection Cases between SSP1-2.6 and SSP5-8.5\n(2021 - 2040, Unit: Cases/km2)",
            labels = labels_brks, midpoint = 0) +
  tm_shape(world) +
  tm_borders(col = 'black', lwd = 0.5, alpha = 0.8) +
  tm_text("iso_a2", size = legend_title_size * 0.5, remove.overlap = T) + 
  tm_grid(alpha = .25) + 
  tm_scale_bar(position = c("right", "bottom")) + 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("right", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) 
PredictionMap.585.126.2040 %>%
  tmap_save(filename = "06_Figure/S25_PredictMap.Den.585.126.2040.jpg", width = 300, height = 140, units = 'mm', dpi = 1000)
# figure S25

# figure S26
load("05_Results/prediction.2060.Rdata")
cols <- c( "blue","green", "gray88","yellow","red")
pal.n.p <- colorRampPalette(cols)
prediction.2060 <- as(prediction.2060, 'SpatialPixelsDataFrame')
prediction.2060 <- as(prediction.2060, "SpatialGridDataFrame")
prediction.2060.245.126 <- prediction.2060
prediction.2060.245.126@data <- prediction.2060.245.126@data %>% dplyr::select(PfPR_incr_2060_245.126_den)
prediction.2060.245.126 <- raster::raster(prediction.2060.245.126)
hist(prediction.2060.245.126)
brks <- c( -20, -17.5, -15, -12.5, -10, -7.5, -5, -2.5, 0,
           2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20)
labels_brks <- c("-20", "", "-15", "", "-10", "", "-5", "", "0",
                 "", "5", "", "10", "", "15", "", "20")
PredictionMap.245.126.2060 <- tm_shape(prediction.2060.245.126) +
  tm_raster("PfPR_incr_2060_245.126_den", palette = pal.n.p(16), breaks = brks, 
            style = 'cont', legend.is.portrait = F, 
            title = "The Difference in Infection Cases between SSP1-2.6 and SSP2-4.5\n(2041 - 2060, Unit: Cases/km2)",
            labels = labels_brks, midpoint = 0) +
  tm_shape(world) +
  tm_borders(col = 'black', lwd = 0.5, alpha = 0.8) +
  tm_text("iso_a2", size = legend_title_size * 0.5, remove.overlap = T) + 
  tm_grid(alpha = .25) + 
  tm_scale_bar(position = c("right", "bottom")) + 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("right", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) 
PredictionMap.245.126.2060 %>%
  tmap_save(filename = "06_Figure/S26_PredictMap.Den.245.126.2060.jpg", width = 300, height = 140, units = 'mm', dpi = 1000)
# figure S26 

# figure S27
prediction.2060.370.126 <- prediction.2060
prediction.2060.370.126@data <- prediction.2060.370.126@data %>% dplyr::select(PfPR_incr_2060_460.126_den)
prediction.2060.370.126 <- raster::raster(prediction.2060.370.126)
hist(prediction.2060.370.126)
brks <- c( -20, -17.5, -15, -12.5, -10, -7.5, -5, -2.5, 0,
           2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20)
labels_brks <- c("-20", "", "-15", "", "-10", "", "-5", "", "0",
                 "", "5", "", "10", "", "15", "", "20")
PredictionMap.370.126.2060 <- tm_shape(prediction.2060.370.126) +
  tm_raster("PfPR_incr_2060_460.126_den", palette = pal.n.p(16), breaks = brks, 
            style = 'cont', legend.is.portrait = F, 
            title = "The Difference in Infection Cases between SSP1-2.6 and SSP3-7.0\n(2041 - 2060, Unit: Cases/km2)",
            labels = labels_brks, midpoint = 0) +
  tm_shape(world) +
  tm_borders(col = 'black', lwd = 0.5, alpha = 0.8) +
  tm_text("iso_a2", size = legend_title_size * 0.5, remove.overlap = T) + 
  tm_grid(alpha = .25) + 
  tm_scale_bar(position = c("right", "bottom")) + 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("right", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) 
PredictionMap.370.126.2060 %>%
  tmap_save(filename = "06_Figure/S27_PredictMap.Den.370.126.2060.jpg", width = 300, height = 140, units = 'mm', dpi = 1000)
# figure S27

# figure S28
prediction.2060.585.126 <- prediction.2060
prediction.2060.585.126@data <- prediction.2060.585.126@data %>% dplyr::select(PfPR_incr_2060_585.126_den)
prediction.2060.585.126 <- raster::raster(prediction.2060.585.126)
hist(prediction.2060.585.126)
brks <- c( -20, -17.5, -15, -12.5, -10, -7.5, -5, -2.5, 0,
           2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20)
labels_brks <- c("-20", "", "-15", "", "-10", "", "-5", "", "0",
                 "", "5", "", "10", "", "15", "", "20")
PredictionMap.585.126.2060 <- tm_shape(prediction.2060.585.126) +
  tm_raster("PfPR_incr_2060_585.126_den", palette = pal.n.p(16), breaks = brks, 
            style = 'cont', legend.is.portrait = F, 
            title = "The Difference in Infection Cases between SSP1-2.6 and SSP5-8.5\n(2041 - 2060, Unit: Cases/km2)",
            labels = labels_brks, midpoint = 0) +
  tm_shape(world) +
  tm_borders(col = 'black', lwd = 0.5, alpha = 0.8) +
  tm_text("iso_a2", size = legend_title_size * 0.5, remove.overlap = T) + 
  tm_grid(alpha = .25) + 
  tm_scale_bar(position = c("right", "bottom")) + 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("right", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) 
PredictionMap.585.126.2060 %>%
  tmap_save(filename = "06_Figure/S28_PredictMap.Den.585.126.2060.jpg", width = 300, height = 140, units = 'mm', dpi = 1000)
# figure S28

# figure S29
load("05_Results/prediction.2100.Rdata")
cols <- c( "blue","green", "gray88","yellow","red")
pal.n.p <- colorRampPalette(cols)
prediction.2100 <- as(prediction.2100, 'SpatialPixelsDataFrame')
prediction.2100 <- as(prediction.2100, "SpatialGridDataFrame")
prediction.2100.245.126 <- prediction.2100
prediction.2100.245.126@data <- prediction.2100.245.126@data %>% dplyr::select(PfPR_incr_2100_245.126_den)
prediction.2100.245.126 <- raster::raster(prediction.2100.245.126)
hist(prediction.2100.245.126)
brks <- c( -20, -17.5, -15, -12.5, -10, -7.5, -5, -2.5, 0,
           2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20)
labels_brks <- c("-20", "", "-15", "", "-10", "", "-5", "", "0",
                 "", "5", "", "10", "", "15", "", "20")
PredictionMap.245.126.2100 <- tm_shape(prediction.2100.245.126) +
  tm_raster("PfPR_incr_2100_245.126_den", palette = pal.n.p(16), breaks = brks, 
            style = 'cont', legend.is.portrait = F, 
            title = "The Difference in Infection Cases between SSP1-2.6 and SSP2-4.5\n(2081 - 2100, Unit: Cases/km2)",
            labels = labels_brks, midpoint = 0) +
  tm_shape(world) +
  tm_borders(col = 'black', lwd = 0.5, alpha = 0.8) +
  tm_text("iso_a2", size = legend_title_size * 0.5, remove.overlap = T) + 
  tm_grid(alpha = .25) + 
  tm_scale_bar(position = c("right", "bottom")) + 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("right", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) 
PredictionMap.245.126.2100 %>%
  tmap_save(filename = "06_Figure/S29_PredictMap.Den.245.126.2100.jpg", width = 300, height = 140, units = 'mm', dpi = 1000)
# figure S29 

# figure S30
prediction.2100.370.126 <- prediction.2100
prediction.2100.370.126@data <- prediction.2100.370.126@data %>% dplyr::select(PfPR_incr_2100_460.126_den)
prediction.2100.370.126 <- raster::raster(prediction.2100.370.126)
hist(prediction.2100.370.126)
brks <- c( -20, -17.5, -15, -12.5, -10, -7.5, -5, -2.5, 0,
           2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20)
labels_brks <- c("-20", "", "-15", "", "-10", "", "-5", "", "0",
                 "", "5", "", "10", "", "15", "", "20")
PredictionMap.370.126.2100 <- tm_shape(prediction.2100.370.126) +
  tm_raster("PfPR_incr_2100_460.126_den", palette = pal.n.p(16), breaks = brks, 
            style = 'cont', legend.is.portrait = F, 
            title = "The Difference in Infection Cases between SSP1-2.6 and SSP3-7.0\n(2081 - 2100, Unit: Cases/km2)",
            labels = labels_brks, midpoint = 0) +
  tm_shape(world) +
  tm_borders(col = 'black', lwd = 0.5, alpha = 0.8) +
  tm_text("iso_a2", size = legend_title_size * 0.5, remove.overlap = T) + 
  tm_grid(alpha = .25) + 
  tm_scale_bar(position = c("right", "bottom")) + 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("right", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) 
PredictionMap.370.126.2100 %>%
  tmap_save(filename = "06_Figure/S30_PredictMap.Den.370.126.2100.jpg", width = 300, height = 140, units = 'mm', dpi = 1000)
# figure S30

# figure S31
prediction.2100.585.126 <- prediction.2100
prediction.2100.585.126@data <- prediction.2100.585.126@data %>% dplyr::select(PfPR_incr_2100_585.126_den)
prediction.2100.585.126 <- raster::raster(prediction.2100.585.126)
hist(prediction.2100.585.126)
brks <- c( -20, -17.5, -15, -12.5, -10, -7.5, -5, -2.5, 0,
           2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20)
labels_brks <- c("-20", "", "-15", "", "-10", "", "-5", "", "0",
                 "", "5", "", "10", "", "15", "", "20")
PredictionMap.585.126.2100 <- tm_shape(prediction.2100.585.126) +
  tm_raster("PfPR_incr_2100_585.126_den", palette = pal.n.p(16), breaks = brks, 
            style = 'cont', legend.is.portrait = F, 
            title = "The Difference in Infection Cases between SSP1-2.6 and SSP5-8.5\n(2081 - 2100, Unit: Cases/km2)",
            labels = labels_brks, midpoint = 0) +
  tm_shape(world) +
  tm_borders(col = 'black', lwd = 0.5, alpha = 0.8) +
  tm_text("iso_a2", size = legend_title_size * 0.5, remove.overlap = T) + 
  tm_grid(alpha = .25) + 
  tm_scale_bar(position = c("right", "bottom")) + 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("right", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) 
PredictionMap.585.126.2100 %>%
  tmap_save(filename = "06_Figure/S31_PredictMap.Den.585.126.2100.jpg", width = 300, height = 140, units = 'mm', dpi = 1000)
# figure S31

