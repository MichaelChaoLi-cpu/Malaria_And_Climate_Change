# Author: M.L.

# output: Figure xx

# end

library(tidyverse)
library(dplyr)
library(sp)
library(tmap)
library(raster)
library("rnaturalearth")
library(ggplot2)
library("plotrix")

# mean PfPR figure S1
world <- ne_countries(scale = "medium", returnclass = "sp")
world@data <- world@data %>% 
  mutate(name_sort = ifelse(name_sort == "Côte d'Ivoire", "Cote d'Ivoire", name_sort))

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

# country iso a3 to id
world_country <- world
world_country@data$name_sort <- paste0(world_country@data$name_sort, ",", world_country@data$iso_a2)
world_continent <- world_country@data %>% dplyr::select(name_sort, continent)
world_country@data <- world_country@data %>% dplyr::select(name_sort)
coords_country <- over(coords, world_country)
coords_country <- coords_country %>% as.data.frame()
coords_country$id <- coords$id
coords_country <- coords_country %>% arrange(desc(name_sort)) %>% filter(id != 415165)
#coords_country$name_sort <- coords_country$name_sort %>% as.factor()
#world_continent$continent <- world_continent$continent %>% as.factor()
#world_continent$name_sort <- world_continent$name_sort %>% as.factor()
# country iso a3 to id

# figure 1
load("05_Results/prediction.2040.Rdata")
prediction.2040.df <- prediction.2040@data
prediction.2040.df <- left_join(prediction.2040.df, coords_country)
prediction.2040.df <- prediction.2040.df %>%
  dplyr::select("predictPfPR.245.126","predictPfPR.460.126",
                "predictPfPR.585.126", "name_sort")
prediction.2040.df.mean <- prediction.2040.df %>%
  group_by(name_sort) %>%
  summarise(across(c("predictPfPR.245.126","predictPfPR.460.126","predictPfPR.585.126"), 
                   list(sce = mean))) 
prediction.2040.df.mean <- prediction.2040.df.mean %>%
  pivot_longer(!name_sort, names_to = "scenarios.change", values_to = "PfPR.change")
prediction.2040.df.se <- prediction.2040.df %>%
  group_by(name_sort) %>%
  summarise(across(c("predictPfPR.245.126","predictPfPR.460.126","predictPfPR.585.126"), 
                   list(sce = std.error))) 
prediction.2040.df.se <- prediction.2040.df.se %>%
  pivot_longer(!name_sort, names_to = "scenarios.change", values_to = "SE")
prediction.2040.df <- left_join(prediction.2040.df.mean, prediction.2040.df.se, 
                                by = c("name_sort", "scenarios.change")) %>% na.omit()
prediction.2040.df <- left_join(prediction.2040.df, world_continent)
prediction.2040.df$name_sort <- prediction.2040.df$name_sort %>% as.factor()
prediction.2040.df$continent <- prediction.2040.df$continent %>% as.factor()
#levels(prediction.2040.df$name_sort) <- rev(levels(prediction.2040.df$name_sort))

country.2040 <- 
  ggplot(prediction.2040.df, aes(x = PfPR.change*100, y = name_sort, color = scenarios.change)) +
  geom_point(aes(shape = continent)) +
  geom_errorbar(aes(xmin = PfPR.change*100 - 1.96*SE*100, xmax = PfPR.change*100 + 1.96*SE*100))+
  scale_color_manual(values = c("darkgreen", "orange", "orchid3"), name = "Scenarios",
                     labels = c("SSP1-2.6 -> SSP2-4.5", "SSP1-2.6 -> SSP3-7.0", "SSP1-2.6 -> SSP5-8.5")) +
  scale_shape_manual(values = c(15, 16, 17, 18, 22, 25), name = "Continent") +
  scale_y_discrete(limits = rev(levels(prediction.2040.df$name_sort)) ) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.35),
        axis.text.y = element_text(size=5), legend.background = element_blank()) +
  labs(x = "Difference of PfPRs (%)", 
       y = NULL, title = "Near Term (2021 - 2040)")
ggsave(file = "06_Figure/figure_1_country.2040.jpg", device = "jpg", width = 6,
       height = 8)

# figure 2
load("05_Results/prediction.2060.Rdata")
prediction.2060.df <- prediction.2060@data
prediction.2060.df <- left_join(prediction.2060.df, coords_country)
prediction.2060.df <- prediction.2060.df %>%
  dplyr::select("predictPfPR.245.126","predictPfPR.460.126",
                "predictPfPR.585.126", "name_sort")
prediction.2060.df.mean <- prediction.2060.df %>%
  group_by(name_sort) %>%
  summarise(across(c("predictPfPR.245.126","predictPfPR.460.126","predictPfPR.585.126"), 
                   list(sce = mean))) 
prediction.2060.df.mean <- prediction.2060.df.mean %>%
  pivot_longer(!name_sort, names_to = "scenarios.change", values_to = "PfPR.change")
prediction.2060.df.se <- prediction.2060.df %>%
  group_by(name_sort) %>%
  summarise(across(c("predictPfPR.245.126","predictPfPR.460.126","predictPfPR.585.126"), 
                   list(sce = std.error))) 
prediction.2060.df.se <- prediction.2060.df.se %>%
  pivot_longer(!name_sort, names_to = "scenarios.change", values_to = "SE")
prediction.2060.df <- left_join(prediction.2060.df.mean, prediction.2060.df.se, 
                                by = c("name_sort", "scenarios.change")) %>% na.omit()
prediction.2060.df <- left_join(prediction.2060.df, world_continent)
prediction.2060.df$name_sort <- prediction.2060.df$name_sort %>% as.factor()
prediction.2060.df$continent <- prediction.2060.df$continent %>% as.factor()
#levels(prediction.2060.df$name_sort) <- rev(levels(prediction.2060.df$name_sort))

country.2060 <- 
  ggplot(prediction.2060.df, aes(x = PfPR.change*100, y = name_sort, color = scenarios.change)) +
  geom_point(aes(shape = continent)) +
  geom_errorbar(aes(xmin = PfPR.change*100 - 1.96*SE*100, xmax = PfPR.change*100 + 1.96*SE*100)) +
  scale_color_manual(values = c("darkgreen", "orange", "orchid3"), name = "Scenarios",
                     labels = c("SSP1-2.6 -> SSP2-4.5", "SSP1-2.6 -> SSP3-7.0", "SSP1-2.6 -> SSP5-8.5")) +
  scale_shape_manual(values = c(15, 16, 17, 18, 22, 25), name = "Continent") +
  scale_y_discrete(limits = rev(levels(prediction.2040.df$name_sort)) ) +
  theme_bw() +
  theme(legend.position = c(0.2, 0.70),
        axis.text.y = element_text(size=5), legend.background = element_blank()) +
  labs(x = "Difference of PfPRs (%)", 
       y = NULL,
       title = "Medium Term (2041 - 2060)")
ggsave(file = "06_Figure/figure_2_country.2060.jpg", device = "jpg", width = 6,
       height = 8)

# figure 3
load("05_Results/prediction.2100.Rdata")
prediction.2100.df <- prediction.2100@data
prediction.2100.df <- left_join(prediction.2100.df, coords_country)
prediction.2100.df <- prediction.2100.df %>%
  dplyr::select("predictPfPR.245.126","predictPfPR.460.126",
                "predictPfPR.585.126", "name_sort")
prediction.2100.df.mean <- prediction.2100.df %>%
  group_by(name_sort) %>%
  summarise(across(c("predictPfPR.245.126","predictPfPR.460.126","predictPfPR.585.126"), 
                   list(sce = mean))) 
prediction.2100.df.mean <- prediction.2100.df.mean %>%
  pivot_longer(!name_sort, names_to = "scenarios.change", values_to = "PfPR.change")
prediction.2100.df.se <- prediction.2100.df %>%
  group_by(name_sort) %>%
  summarise(across(c("predictPfPR.245.126","predictPfPR.460.126","predictPfPR.585.126"), 
                   list(sce = std.error))) 
prediction.2100.df.se <- prediction.2100.df.se %>%
  pivot_longer(!name_sort, names_to = "scenarios.change", values_to = "SE")
prediction.2100.df <- left_join(prediction.2100.df.mean, prediction.2100.df.se, 
                                by = c("name_sort", "scenarios.change")) %>% na.omit()
prediction.2100.df <- left_join(prediction.2100.df, world_continent)
prediction.2100.df$name_sort <- prediction.2100.df$name_sort %>% as.factor()
prediction.2100.df$continent <- prediction.2100.df$continent %>% as.factor()
#levels(prediction.2100.df$name_sort) <- rev(levels(prediction.2100.df$name_sort))

country.2100 <- 
  ggplot(prediction.2100.df, aes(x = PfPR.change*100, y = name_sort, color = scenarios.change)) +
  geom_point(aes(shape = continent)) +
  geom_errorbar(aes(xmin = PfPR.change*100 - 1.96*SE*100, xmax = PfPR.change*100 + 1.96*SE*100)) +
  scale_color_manual(values = c("darkgreen", "orange", "orchid3"), name = "Scenarios",
                     labels = c("SSP1-2.6 -> SSP2-4.5", "SSP1-2.6 -> SSP3-7.0", "SSP1-2.6 -> SSP5-8.5")) +
  scale_shape_manual(values = c(15, 16, 17, 18, 22, 25), name = "Continent") +
  scale_y_discrete(limits = rev(levels(prediction.2040.df$name_sort)) ) +
  theme_bw() +
  theme(legend.position = c(0.2, 0.70),
        axis.text.y = element_text(size=5), legend.background = element_blank()) +
  labs(x = "Difference of PfPRs (%)", 
       y = NULL,
       title = "Long Term (2081 - 2100)")
ggsave(file = "06_Figure/figure_3_country.2100.jpg", device = "jpg", width = 6,
       height = 8)

# figure 4
load("05_Results/prediction.2040.Rdata")
prediction.2040.df <- prediction.2040@data
prediction.2040.df <- left_join(prediction.2040.df, coords_country)
prediction.2040.df <- prediction.2040.df %>%
  dplyr::select("PfPR_incr_2040_245.126_den", "PfPR_incr_2040_460.126_den",
                "PfPR_incr_2040_585.126_den", "name_sort")
prediction.2040.df.mean = prediction.2040.df %>%
  group_by(name_sort) %>%
  summarise(across(everything(), list(sce = base::mean))) 
prediction.2040.df.mean <- prediction.2040.df.mean %>%
  pivot_longer(!name_sort, names_to = "scenarios.change", values_to = "PfPR.change")
prediction.2040.df.se <- prediction.2040.df %>%
  group_by(name_sort) %>%
  summarise(across(c("PfPR_incr_2040_245.126_den", "PfPR_incr_2040_460.126_den",
                     "PfPR_incr_2040_585.126_den"), 
                   list(sce = std.error))) 
prediction.2040.df.se <- prediction.2040.df.se %>%
  pivot_longer(!name_sort, names_to = "scenarios.change", values_to = "SE")
prediction.2040.df <- left_join(prediction.2040.df.mean, prediction.2040.df.se, 
                                by = c("name_sort", "scenarios.change")) %>% na.omit()
prediction.2040.df <- left_join(prediction.2040.df, world_continent)
prediction.2040.df$name_sort <- prediction.2040.df$name_sort %>% as.factor()
prediction.2040.df$continent <- prediction.2040.df$continent %>% as.factor()
#levels(prediction.2040.df$name_sort) <- rev(levels(prediction.2040.df$name_sort))

country.2040 <- 
  ggplot(prediction.2040.df, aes(x = PfPR.change*100, y = name_sort, color = scenarios.change)) +
  geom_point(aes(shape = continent)) +
  geom_errorbar(aes(xmin = PfPR.change*100 - 1.96*SE*100, xmax = PfPR.change*100 + 1.96*SE*100))+
  scale_color_manual(values = c("darkgreen", "orange", "orchid3"), name = "Scenarios",
                     labels = c("SSP1-2.6 -> SSP2-4.5", "SSP1-2.6 -> SSP3-7.0", "SSP1-2.6 -> SSP5-8.5")) +
  scale_shape_manual(values = c(15, 16, 17, 18, 22, 25), name = "Continent") +
  scale_y_discrete(limits = rev(levels(prediction.2040.df$name_sort)) ) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.35),
        axis.text.y = element_text(size=5), legend.background = element_blank()) +
  labs(x = "Infection Case Changes (case/km2)", 
       y = NULL, title = "Near Term (2021 - 2040)")
ggsave(file = "06_Figure/figure_4_country.2040.jpg", device = "jpg", width = 6,
       height = 8)

# figure 5
load("05_Results/prediction.2060.Rdata")
prediction.2060.df <- prediction.2060@data
prediction.2060.df <- left_join(prediction.2060.df, coords_country)
prediction.2060.df <- prediction.2060.df %>%
  dplyr::select("PfPR_incr_2060_245.126_den", "PfPR_incr_2060_460.126_den",
                "PfPR_incr_2060_585.126_den", "name_sort")
prediction.2060.df.mean <- prediction.2060.df %>%
  group_by(name_sort) %>%
  summarise(across(c("PfPR_incr_2060_245.126_den", "PfPR_incr_2060_460.126_den",
                     "PfPR_incr_2060_585.126_den"), 
                   list(sce = base::mean))) 
prediction.2060.df.mean <- prediction.2060.df.mean %>%
  pivot_longer(!name_sort, names_to = "scenarios.change", values_to = "PfPR.change")
prediction.2060.df.se <- prediction.2060.df %>%
  group_by(name_sort) %>%
  summarise(across(c("PfPR_incr_2060_245.126_den", "PfPR_incr_2060_460.126_den",
                     "PfPR_incr_2060_585.126_den"), 
                   list(sce = std.error))) 
prediction.2060.df.se <- prediction.2060.df.se %>%
  pivot_longer(!name_sort, names_to = "scenarios.change", values_to = "SE")
prediction.2060.df <- left_join(prediction.2060.df.mean, prediction.2060.df.se, 
                                by = c("name_sort", "scenarios.change")) %>% na.omit()
prediction.2060.df <- left_join(prediction.2060.df, world_continent)
prediction.2060.df$name_sort <- prediction.2060.df$name_sort %>% as.factor()
prediction.2060.df$continent <- prediction.2060.df$continent %>% as.factor()
#levels(prediction.2060.df$name_sort) <- rev(levels(prediction.2060.df$name_sort))

country.2060 <- 
  ggplot(prediction.2060.df, aes(x = PfPR.change*100, y = name_sort, color = scenarios.change)) +
  geom_point(aes(shape = continent)) +
  geom_errorbar(aes(xmin = PfPR.change*100 - 1.96*SE*100, xmax = PfPR.change*100 + 1.96*SE*100)) +
  scale_color_manual(values = c("darkgreen", "orange", "orchid3"), name = "Scenarios",
                     labels = c("SSP1-2.6 -> SSP2-4.5", "SSP1-2.6 -> SSP3-7.0", "SSP1-2.6 -> SSP5-8.5")) +
  scale_shape_manual(values = c(15, 16, 17, 18, 22, 25), name = "Continent") +
  scale_y_discrete(limits = rev(levels(prediction.2040.df$name_sort)) ) +
  theme_bw() +
  theme(legend.position = c(0.2, 0.70),
        axis.text.y = element_text(size=5), legend.background = element_blank()) +
  labs(x = "Infection Case Changes (case/km2)", 
       y = NULL,
       title = "Medium Term (2041 - 2060)")
ggsave(file = "06_Figure/figure_5_country.2060.jpg", device = "jpg", width = 6,
       height = 8)

# figure 6
load("05_Results/prediction.2100.Rdata")
prediction.2100.df <- prediction.2100@data
prediction.2100.df <- left_join(prediction.2100.df, coords_country)
prediction.2100.df <- prediction.2100.df %>%
  dplyr::select("PfPR_incr_2100_245.126_den", "PfPR_incr_2100_460.126_den",
                "PfPR_incr_2100_585.126_den", "name_sort")
prediction.2100.df.mean <- prediction.2100.df %>%
  group_by(name_sort) %>%
  summarise(across(c("PfPR_incr_2100_245.126_den", "PfPR_incr_2100_460.126_den",
                     "PfPR_incr_2100_585.126_den"), 
                   list(sce = base::mean))) 
prediction.2100.df.mean <- prediction.2100.df.mean %>%
  pivot_longer(!name_sort, names_to = "scenarios.change", values_to = "PfPR.change")
prediction.2100.df.se <- prediction.2100.df %>%
  group_by(name_sort) %>%
  summarise(across(c("PfPR_incr_2100_245.126_den", "PfPR_incr_2100_460.126_den",
                     "PfPR_incr_2100_585.126_den"), 
                   list(sce = std.error))) 
prediction.2100.df.se <- prediction.2100.df.se %>%
  pivot_longer(!name_sort, names_to = "scenarios.change", values_to = "SE")
prediction.2100.df <- left_join(prediction.2100.df.mean, prediction.2100.df.se, 
                                by = c("name_sort", "scenarios.change")) %>% na.omit()
prediction.2100.df <- left_join(prediction.2100.df, world_continent)
prediction.2100.df$name_sort <- prediction.2100.df$name_sort %>% as.factor()
prediction.2100.df$continent <- prediction.2100.df$continent %>% as.factor()
#levels(prediction.2100.df$name_sort) <- rev(levels(prediction.2100.df$name_sort))

country.2100 <- 
  ggplot(prediction.2100.df, aes(x = PfPR.change*100, y = name_sort, color = scenarios.change)) +
  geom_point(aes(shape = continent)) +
  geom_errorbar(aes(xmin = PfPR.change*100 - 1.96*SE*100, xmax = PfPR.change*100 + 1.96*SE*100)) +
  scale_color_manual(values = c("darkgreen", "orange", "orchid3"), name = "Scenarios",
                     labels = c("SSP1-2.6 -> SSP2-4.5", "SSP1-2.6 -> SSP3-7.0", "SSP1-2.6 -> SSP5-8.5")) +
  scale_shape_manual(values = c(15, 16, 17, 18, 22, 25), name = "Continent") +
  scale_y_discrete(limits = rev(levels(prediction.2040.df$name_sort)) ) +
  theme_bw() +
  theme(legend.position = c(0.2, 0.70),
        axis.text.y = element_text(size=5), legend.background = element_blank()) +
  labs(x = "Infection Case Changes (case/km2)", 
       y = NULL,
       title = "Long Term (2081 - 2100)")
ggsave(file = "06_Figure/figure_6_country.2100.jpg", device = "jpg", width = 6,
       height = 8)
