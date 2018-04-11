## Convert matlab script to R - calculate morisita's index of aggregation for BBS data

## http://esapubs.org/archive/ecol/E093/215/metadata.php
## Land cover - also has aggregation index for each route
  
library(dplyr)

setwd("/Volumes/hurlbertlab/Databases/BBS/FiftyStopData/")
#setwd("//BioArk//HurlbertLab//Databases//BBS//FiftyStopData//")
setwd("/Users/gracedicecco/Desktop/git/fifty-stop/")
#data.long <- read.csv("2010_fiftystopdata.csv")
data.long <- read.csv("tenstopdata04.csv", header = F)
#data <- data.long %>%
#  filter(year <= 2005 & year <= 2003) %>%
 # select(-RouteDataID, -countrynum, -RPID)
colnames(data.long) <- c("statenum", "Route", "year", "AOU", "stop1", "stop2", "stop3", "stop4", "stop5")
#data <- data.long %>%
 # select(-RouteDataID, -countrynum, -RPID)
data <- data.long

data$stateroute <- data$statenum*1000 + data$Route
data$Ntot <- rowSums(data[,5:9])
data$mean_stopN <- apply(data[,5:9], 1, mean)
data$var_stopN <- apply(data[,5:9], 1, function(x) sd(x)^2)
part1 <- data$Ntot/(data$Ntot-1)
part2 <- data$mean_stopN^-1
part3 <- (data$var_stopN/data$mean_stopN) + data$mean_stopN - 1
Im <- part1*part2*part3
Im <- 10000*Im
Im <- round(Im)
Im <- Im/10000
data$Im <- Im

## dplyr to get mean Ims
newdata <- data %>%
  select(stateroute, year, AOU, Im) %>%
  group_by(stateroute, year) %>%
  mutate(Im_rank = rank(Im), valid_Ims = length(Im != Inf), 
         Im_index = ifelse(Im != Inf, Im_rank/valid_Ims, NA))

spp_mean <- newdata %>%
  group_by(AOU) %>%
  summarize(mean_Im_index = mean(Im_index, na.rm = T))

## for loop to get mean Ims
newdata2 <- select(data, stateroute, year, AOU, Im)

unique_routes <- unique(newdata2$stateroute)
num_routes <- length(unique_routes)
unique_spp <- unique(newdata2$AOU)
num_spp <- length(unique_spp)

rte_results <- data.frame(stateroute = NA, year = NA, AOU = NA, Im = NA, Im_rank = NA, Im_index = NA)
for(i in 1:num_routes) {
  for(k in 2004) {
    rte_rows <- filter(newdata2, stateroute == unique_routes[i] & year == k)
    if(nrow(rte_rows) > 0) {
      rte_rows$Im_rank <- rank(rte_rows$Im)
      valid_Ims <- length(rte_rows$Im != Inf)
      rte_rows$Im_index <- rte_rows$Im_rank/valid_Ims
      rte_results <- rbind(rte_results, rte_rows)
    }
  }
}
rte_results <- rte_results[-1, ]

# flag Inf Im_index as NA
rte_results$Im_index[which(rte_results$Im == Inf)] <- NA

spp_mean <- rte_results %>%
  group_by(AOU) %>%
  summarize(mean_Im_index = mean(Im_index, na.rm = T))

#read in original values
setwd("/Volumes/hurlbertlab/DiCecco/")
#setwd("C:/Users/gdicecco/Desktop/")  
rocorr <- read.csv("Master_RO_Correlates_20110610.csv")

# compare with dplyr
ims <- left_join(rocorr, spp_mean, by = "AOU")
imssorted <- ims %>% arrange(mean_Im_index) %>% select(AOU, CommonName, mean_Im_index, Mean_Rel_Im)

plot(ims$new_Im, ims$mean_Im_index, ylim = c(0,1), xlim = c(0,1))
abline(0,1)

# Route level ranks

routeranks <- data %>%
  select(stateroute, year, AOU, Im) %>%
  group_by(AOU, year) %>%
  mutate(Im_rank = rank(Im), valid_Ims = length(Im != Inf), 
         Im_index = ifelse(Im != Inf, Im_rank/valid_Ims, NA))

route_mean <- routeranks %>%
  group_by(stateroute) %>%
  summarize(mean_Im = mean(Im_index, na.rm = T))

# Get land cover data
setwd("C:/Users/gdicecco/Desktop/git/fifty-stop-habitat-breadth/")
nlcd <- read.csv("bbs_route_nlcd.csv")
config <- read.csv("BBS_route_landcover_configuration_metrics.csv")
