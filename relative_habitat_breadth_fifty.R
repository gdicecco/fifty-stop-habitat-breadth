## Convert matlab script to R - calculate morisita's index of aggregation for BBS data

## http://esapubs.org/archive/ecol/E093/215/metadata.php
## Land cover - also has aggregation index for each route
  
library(dplyr)

setwd("//BioArk//HurlbertLab//Databases//BBS//FiftyStopData//")
data.long <- read.csv("2010_fiftystopdata.csv")
data <- data.long %>%
  filter(year >= 2005 & year <= 2007) %>%
  select(-RouteDataID, -countrynum, -RPID)

data$stateroute <- data$statenum*1000 + data$Route
data$Ntot <- rowSums(data[,5:54])
data$mean_stopN <- apply(data[,5:54], 1, mean)
data$var_stopN <- apply(data[,5:54], 1, function(x) sd(x)^2)
part1 <- data$Ntot/(data$Ntot-1)
part2 <- data$mean_stopN^-1
part3 <- (data$var_stopN/data$mean_stopN) + data$mean_stopN - 1
Im <- part1*part2*part3
Im <- 10000*Im
Im <- round(Im)
Im <- Im/10000
data$Im <- Im
#Some Im values are Inf

newdata2 <- data %>%
  select(stateroute, year, AOU, Im) %>%
  group_by(stateroute, year) %>%
  mutate(Im_rank = rank(Im), valid_Ims = length(Im != Inf), 
         Im_index = ifelse(Im != Inf, Im_rank/valid_Ims, NA))

spp_mean <- newdata %>%
  group_by(AOU) %>%
  summarize(mean_Im_index = mean(Im_index, na.rm = T))
  