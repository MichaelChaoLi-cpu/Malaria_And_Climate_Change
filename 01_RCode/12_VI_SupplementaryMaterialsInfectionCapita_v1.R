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
prediction.2040.245.126@data <- prediction.2040.245.126@data %>% dplyr::select(PfPR_incr_2040_245.126)
prediction.2040.245.126 <- raster::raster(prediction.2040.245.126)
brks <- c( -8000, -7000, -6000, -5000, -4000, -3000, -2000, -1000, 0,
           1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000)
labels_brks <- c("-8k", "", "-6k", "", "-4k", "", "-2k", "", "0k",
                 "", "2k", "", "4k", "", "6k", "", "8k")
PredictionMap.245.126.2040 <- tm_shape(prediction.2040.245.126) +
  tm_raster("PfPR_incr_2040_245.126", palette = pal.n.p(16), breaks = brks, 
            style = 'cont', legend.is.portrait = F, title = "The Difference of PfPRs between SSP1-2.6 and SSP2-4.5 (2021 - 2040)",
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
  tmap_save(filename = "06_Figure/S23_PredictMap.245.126.2040.jpg", width = 300, height = 140, units = 'mm', dpi = 1000)
# figure 3 