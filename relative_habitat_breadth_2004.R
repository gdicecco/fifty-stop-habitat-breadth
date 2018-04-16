library(dplyr)

#setwd("/Users/gracedicecco/Desktop/git/fifty-stop/")
setwd("C:/Users/gdicecco/Desktop/git/fifty-stop-habitat-breadth")
data.long <- read.csv("tenstopdata04.csv", header = F)
colnames(data.long) <- c("statenum", "Route", "year", "AOU", "stop10", "stop20", "stop30", "stop40", "stop50")
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
  mutate(Im_rank = rank(Im), valid_Ims = sum(Im != Inf), 
         Im_index = ifelse(Im != Inf, Im_rank/valid_Ims, NA))

spp_mean <- newdata %>%
  group_by(AOU) %>%
  summarize(mean_Im_index = mean(Im_index, na.rm = T))

#read in original values
#setwd("/Volumes/hurlbertlab/DiCecco/")
setwd("C:/Users/gdicecco/Desktop/")  
rocorr <- read.csv("Master_RO_Correlates_20110610.csv")

# compare with dplyr
ims <- left_join(rocorr, spp_mean, by = "AOU")
imssorted <- ims %>% arrange(mean_Im_index) %>% select(AOU, CommonName, mean_Im_index, Mean_Rel_Im)

plot(ims$Mean_Rel_Im, ims$mean_Im_index, ylim = c(0,1), xlim = c(0,1))
abline(0,1)


