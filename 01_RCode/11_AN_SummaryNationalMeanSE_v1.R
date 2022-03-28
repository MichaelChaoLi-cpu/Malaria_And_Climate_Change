# Author: M.L.

# input: prediction.2040.Rdata
# prediction.2040.Rdata: "predictPfPR.245.126" marginal effects from ssp126 to ssp245
# prediction.2040.Rdata: "predictPfPR.460.126" marginal effects from ssp126 to ssp370 
# note: here "460" is a typo, it should 370. the period is from 2021 to 2040
# prediction.2040.Rdata: "predictPfPR.585.126" marginal effects from ssp126 to ssp585 

# input: prediction.2060.Rdata
# prediction.2060.Rdata: "predictPfPR.245.126" marginal effects from ssp126 to ssp245
# prediction.2060.Rdata: "predictPfPR.460.126" marginal effects from ssp126 to ssp370 
# note: here "460" is a typo, it should 370. the period is from 2041 to 2060
# prediction.2060.Rdata: "predictPfPR.585.126" marginal effects from ssp126 to ssp585 

# input: prediction.2100.Rdata
# prediction.2100.Rdata: "predictPfPR.245.126" marginal effects from ssp126 to ssp245
# prediction.2100.Rdata: "predictPfPR.460.126" marginal effects from ssp126 to ssp370 
# note: here "460" is a typo, it should 370. the period is from 2081 to 2100
# prediction.2100.Rdata: "predictPfPR.585.126" marginal effects from ssp126 to ssp585

# output: table.se.csv
# table.se.csv: this is Table 1

# end

library(tidyverse)
library(dplyr)
library("plotrix")

make.table.with.95CI <- function(table){
  table[,8] <- table[,2] + 1.96 * table[,3]
  table[,9] <- table[,2] - 1.96 * table[,3]
  table[,10] <- table[,4] + 1.96 * table[,5]
  table[,11] <- table[,4] - 1.96 * table[,5]
  table[,12] <- table[,6] + 1.96 * table[,7]
  table[,13] <- table[,6] - 1.96 * table[,7]
  table <- table[,c(1,2,4,6,8:13)]
  return(table)
}
  

load("04_Data/02_coords_continent.RData")

load("05_Results/prediction.2040.Rdata")

mean <- (prediction.2040@data$predictPfPR.245.126*100) %>% mean()
se <- (prediction.2040@data$predictPfPR.245.126*100) %>% std.error()
mean - 1.96 * se
mean + 1.96 * se

mean <- (prediction.2040@data$predictPfPR.460.126*100) %>% mean()
se <- (prediction.2040@data$predictPfPR.460.126*100) %>% std.error()
mean
mean - 1.96 * se
mean + 1.96 * se

mean <- (prediction.2040@data$predictPfPR.585.126*100) %>% mean()
se <- (prediction.2040@data$predictPfPR.585.126*100) %>% std.error()
mean
mean - 1.96 * se
mean + 1.96 * se

prediction.2040@data <- left_join(prediction.2040@data, coords_continent, by = 'id')
prediction.2040.continent <- prediction.2040@data %>%
  dplyr::select("predictPfPR.245.126", "predictPfPR.460.126",
                "predictPfPR.585.126", "continent")
prediction.2040.continent <- prediction.2040.continent %>%
  group_by(prediction.2040.continent$continent) %>%
  summarise(
    across(c("predictPfPR.245.126","predictPfPR.460.126","predictPfPR.585.126"), 
                   list(mean = base::mean, se = plotrix::std.error))
    ) 

load("05_Results/prediction.2060.Rdata")

mean <- (prediction.2060@data$predictPfPR.245.126*100) %>% mean()
se <- (prediction.2060@data$predictPfPR.245.126*100) %>% std.error()
mean
mean - 1.96 * se
mean + 1.96 * se

mean <- (prediction.2060@data$predictPfPR.460.126*100) %>% mean()
se <- (prediction.2060@data$predictPfPR.460.126*100) %>% std.error()
mean
mean - 1.96 * se
mean + 1.96 * se

mean <- (prediction.2060@data$predictPfPR.585.126*100) %>% mean()
se <- (prediction.2060@data$predictPfPR.585.126*100) %>% std.error()
mean
mean - 1.96 * se
mean + 1.96 * se

prediction.2060@data <- left_join(prediction.2060@data, coords_continent, by = 'id')
prediction.2060.continent <- prediction.2060@data %>%
  dplyr::select("predictPfPR.245.126", "predictPfPR.460.126",
                "predictPfPR.585.126", "continent")
prediction.2060.continent <- prediction.2060.continent %>%
  group_by(prediction.2060.continent$continent) %>%
  summarise(
    across(c("predictPfPR.245.126","predictPfPR.460.126","predictPfPR.585.126"), 
           list(mean = base::mean, se = plotrix::std.error))
  ) 

load("05_Results/prediction.2100.Rdata")

mean <- (prediction.2100@data$predictPfPR.245.126*100) %>% mean()
se <- (prediction.2100@data$predictPfPR.245.126*100) %>% std.error()
mean
mean - 1.96 * se
mean + 1.96 * se

mean <- (prediction.2100@data$predictPfPR.460.126*100) %>% mean()
se <- (prediction.2100@data$predictPfPR.460.126*100) %>% std.error()
mean
mean - 1.96 * se
mean + 1.96 * se

mean <- (prediction.2100@data$predictPfPR.585.126*100) %>% mean()
se <- (prediction.2100@data$predictPfPR.585.126*100) %>% std.error()
mean
mean - 1.96 * se
mean + 1.96 * se

prediction.2100@data <- left_join(prediction.2100@data, coords_continent, by = 'id')
prediction.2100.continent <- prediction.2100@data %>%
  dplyr::select("predictPfPR.245.126", "predictPfPR.460.126",
                "predictPfPR.585.126", "continent")
prediction.2100.continent <- prediction.2100.continent %>%
  group_by(prediction.2100.continent$continent) %>%
  summarise(
    across(c("predictPfPR.245.126","predictPfPR.460.126","predictPfPR.585.126"), 
           list(mean = base::mean, se = plotrix::std.error))
  ) 

table.2040 <- make.table.with.95CI(prediction.2040.continent)
table.2060 <- make.table.with.95CI(prediction.2060.continent)
table.2100 <- make.table.with.95CI(prediction.2100.continent)
colnames(table.2040)[1] <- "continent"
colnames(table.2060)[1] <- "continent"
colnames(table.2100)[1] <- "continent"

table.se <- rbind(table.2040, table.2060, table.2100)

table.se %>% write.csv("05_Results/table.se.csv")


#### prediction of infection case change
load("04_Data/02_coords_continent.RData")

load("05_Results/prediction.2040.Rdata")

mean <- (prediction.2040@data$PfPR_incr_2040_245.126_den) %>% mean()
se <- (prediction.2040@data$PfPR_incr_2040_245.126_den) %>% std.error()
mean
mean - 1.96 * se
mean + 1.96 * se

mean <- (prediction.2040@data$PfPR_incr_2040_460.126_den) %>% mean()
se <- (prediction.2040@data$PfPR_incr_2040_460.126_den) %>% std.error()
mean
mean - 1.96 * se
mean + 1.96 * se

mean <- (prediction.2040@data$PfPR_incr_2040_585.126_den) %>% mean()
se <- (prediction.2040@data$PfPR_incr_2040_585.126_den) %>% std.error()
mean
mean - 1.96 * se
mean + 1.96 * se

prediction.2040@data <- left_join(prediction.2040@data, coords_continent, by = 'id')
prediction.2040.continent <- prediction.2040@data %>%
  dplyr::select("PfPR_incr_2040_245.126_den", "PfPR_incr_2040_460.126_den",
                "PfPR_incr_2040_585.126_den", "continent")
prediction.2040.continent <- prediction.2040.continent %>%
  group_by(prediction.2040.continent$continent) %>%
  summarise(
    across(c("PfPR_incr_2040_245.126_den", "PfPR_incr_2040_460.126_den",
             "PfPR_incr_2040_585.126_den"), 
           list(mean = base::mean, se = plotrix::std.error))
  ) 

load("05_Results/prediction.2060.Rdata")

mean <- (prediction.2060@data$PfPR_incr_2060_245.126_den) %>% mean()
se <- (prediction.2060@data$PfPR_incr_2060_245.126_den) %>% std.error()
mean
mean - 1.96 * se
mean + 1.96 * se

mean <- (prediction.2060@data$PfPR_incr_2060_460.126_den) %>% mean()
se <- (prediction.2060@data$PfPR_incr_2060_460.126_den) %>% std.error()
mean
mean - 1.96 * se
mean + 1.96 * se

mean <- (prediction.2060@data$PfPR_incr_2060_585.126_den) %>% mean()
se <- (prediction.2060@data$PfPR_incr_2060_585.126_den) %>% std.error()
mean
mean - 1.96 * se
mean + 1.96 * se

prediction.2060@data <- left_join(prediction.2060@data, coords_continent, by = 'id')
prediction.2060.continent <- prediction.2060@data %>%
  dplyr::select("PfPR_incr_2060_245.126_den", "PfPR_incr_2060_460.126_den",
                "PfPR_incr_2060_585.126_den", "continent")
prediction.2060.continent <- prediction.2060.continent %>%
  group_by(prediction.2060.continent$continent) %>%
  summarise(
    across(c("PfPR_incr_2060_245.126_den", "PfPR_incr_2060_460.126_den",
             "PfPR_incr_2060_585.126_den"), 
           list(mean = base::mean, se = plotrix::std.error))
  ) 

load("05_Results/prediction.2100.Rdata")

mean <- (prediction.2100@data$PfPR_incr_2100_245.126_den) %>% mean()
se <- (prediction.2100@data$PfPR_incr_2100_245.126_den) %>% std.error()
mean
mean - 1.96 * se
mean + 1.96 * se

mean <- (prediction.2100@data$PfPR_incr_2100_460.126_den) %>% mean()
se <- (prediction.2100@data$PfPR_incr_2100_460.126_den) %>% std.error()
mean
mean - 1.96 * se
mean + 1.96 * se

mean <- (prediction.2100@data$PfPR_incr_2100_585.126_den) %>% mean()
se <- (prediction.2100@data$PfPR_incr_2100_585.126_den) %>% std.error()
mean
mean - 1.96 * se
mean + 1.96 * se

prediction.2100@data <- left_join(prediction.2100@data, coords_continent, by = 'id')
prediction.2100.continent <- prediction.2100@data %>%
  dplyr::select("PfPR_incr_2100_245.126_den", "PfPR_incr_2100_460.126_den",
                "PfPR_incr_2100_585.126_den", "continent")
prediction.2100.continent <- prediction.2100.continent %>%
  group_by(prediction.2100.continent$continent) %>%
  summarise(
    across(c("PfPR_incr_2100_245.126_den", "PfPR_incr_2100_460.126_den",
             "PfPR_incr_2100_585.126_den"), 
           list(mean = base::mean, se = plotrix::std.error))
  ) 

table.2040 <- make.table.with.95CI(prediction.2040.continent)
table.2060 <- make.table.with.95CI(prediction.2060.continent)
table.2100 <- make.table.with.95CI(prediction.2100.continent)
colnames(table.2040) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
colnames(table.2060) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
colnames(table.2100) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
colnames(table.2040)[1] <- "continent"
colnames(table.2060)[1] <- "continent"
colnames(table.2100)[1] <- "continent"

table.se <- rbind(table.2040, table.2060, table.2100)

table.se %>% write.csv("05_Results/InfectCasetable.se.csv")

