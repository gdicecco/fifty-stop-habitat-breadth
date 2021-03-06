## Calculate morisita's index of aggregation for BBS data

library(dplyr)

setwd("/Volumes/hurlbertlab/Databases/BBS/2017/") # Mac
#setwd("//BioArk//HurlbertLab//Databases//BBS//2017//") # PC
data.long <- read.csv("bbs_counts_20170712.csv")
weather <- read.csv("bbs_weather_20170712.csv")

# read in original values
setwd("/Volumes/hurlbertlab/DiCecco/")
#setwd("C:/Users/gdicecco/Desktop/")  
rocorr <- read.csv("Master_RO_Correlates_20110610.csv")

species <- unique(rocorr$AOU)

weather$stateroute <-weather$statenum*1000 + weather$route
data.long$stateroute <- data.long$statenum*1000 + data.long$route
RT1 <- subset(weather, runtype == 1, select = c("stateroute", "year"))
RT1.routes <- left_join(RT1, data.long, by = c("stateroute", "year"))

data <- RT1.routes %>%
  filter(year <= 2005 & year >= 2003, aou %in% species) %>%
  select(-record_id, -countrynum, -rpid, - speciestotal)

data$stateroute <- data$statenum*1000 + data$route
data$Ntot <- data$stoptotal
data$mean_stopN <- apply(data[,6:10], 1, mean)
data$var_stopN <- apply(data[,6:10], 1, function(x) sd(x)^2)
part1 <- data$Ntot/(data$Ntot-1)
part2 <- data$mean_stopN^-1
part3 <- (data$var_stopN/data$mean_stopN) + data$mean_stopN - 1
Im <- part1*part2*part3
data$Im <- Im

## dplyr to get mean Ims
newdata <- data %>%
  select(stateroute, year, aou, Im) %>%
  group_by(stateroute, year) %>%
  mutate(Im_rank = rank(Im), valid_Ims = sum(Im != Inf), 
         Im_index = ifelse(Im != Inf, Im_rank/valid_Ims, NA))

spp_mean <- newdata %>%
  group_by(aou) %>%
  summarize(mean_Im_index = mean(Im_index, na.rm = T))

# compare with dplyr
ims <- left_join(rocorr, spp_mean, by = c("AOU" = "aou"))
imssorted <- ims %>% arrange(mean_Im_index) %>% select(AOU, CommonName, mean_Im_index, new_Im)

plot(ims$new_Im, ims$mean_Im_index, ylim = c(0,1), xlim = c(0,1))
abline(0,1)

# Missing species from 2017 data subset: 3040, 4700, 5240
species[! species %in% unique(data$aou)]

# Route level ranks

# Filter species: AOU in rocorr

aous <- unique(rocorr$AOU)

routeranks <- data %>%
  filter(aou %in% aous) %>%
  select(stateroute, year, aou, Im) %>%
  group_by(aou, year) %>%
  mutate(Im_rank = rank(Im), valid_Ims = sum(Im != Inf), 
         Im_index = ifelse(Im != Inf, Im_rank/valid_Ims, NA))

route_mean <- routeranks %>%
  group_by(stateroute) %>%
  summarize(mean_Im = mean(Im_index, na.rm = T))

# Get land cover data

## http://esapubs.org/archive/ecol/E093/215/metadata.php
## Land cover - also has aggregation index for each route

setwd("/Users/gracedicecco/Desktop/git/fifty-stop/")
#setwd("C:/Users/gdicecco/Desktop/git/fifty-stop-habitat-breadth/")
nlcd <- read.csv("bbs_route_nlcd.csv")
config <- read.csv("BBS_route_landcover_configuration_metrics.csv")

# Plots comparing route species aggregation to landscape fragmentation indices

route_data <- na.omit(left_join(config, route_mean, by = c("RT..NO." = "stateroute"))) #merge routes in common
length(unique(route_data$RT..NO.))
#2893 routes in common

library(ggplot2)
theme_set(theme_bw())

ggplot(data, aes(x = Im)) + geom_histogram() + geom_vline(xintercept = 1, color = "blue") + ylab("Frequency")
# Most species are more aggregated than expected from random distribution

# Summary plots about data
ggplot(route_data, aes(x = mean_Im)) + geom_histogram()

# Compare Im with landscape configuration metrics
pdens_mod <- lm(route_data$mean_Im ~ route_data$Patch.Density)
ggplot(route_data, aes(x = Patch.Density, y = mean_Im)) + geom_point(pch = 1) + xlab("Patch Density") +
  ylab("Route Im") + geom_smooth(method = "lm", se = F)

psize_mod <- lm(route_data$mean_Im ~ route_data$Largest.Patch.Index)
ggplot(route_data, aes(x = Largest.Patch.Index, y = mean_Im)) + geom_point(pch = 1) + xlab("Largest Patch Index") +
  ylab("Route Im") + geom_smooth(method = "lm", se = F)

agg_mod <- lm(route_data$mean_Im ~ route_data$Aggregation.Index)
ggplot(route_data, aes(x = Aggregation.Index, y = mean_Im)) + geom_point(pch = 1) + xlab("Aggregation Index") +
  ylab("Route Im") + geom_smooth(method = "lm", se = F)

cor <- cor(route_data[, c("mean_Im", "Patch.Density", "Largest.Patch.Index", "Aggregation.Index")])
round(cor, 3)
