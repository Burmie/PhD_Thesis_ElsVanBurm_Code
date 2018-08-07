### Create different datasets (gradually using more data) to fit the model 
### Start with data from 2001 and gradually add more years using a proportion of 25%, 50%, 75% and 100% of the total data (geographically stratified)

## Load all data 
CIdata <- read.table("CIdata_all_years_with_Predictors_and_Travel_and_Survey_cost_RAINFALL_GEOLOGY.txt", header=TRUE, sep="\t")
## PLOT REAL AND RESCALED COST 
par(mfrow=c(1,2))
hist(CIdata$COST_Travel_plus_Survey, xlab= "Survey cost REAL (min)")
hist(CIdata$COST_Travel_plus_Survey-15, xlab= "Survey cost RESCALED (min)")


# Rescale cost 
CIdata$COST_Travel_plus_Survey_rescaled <- CIdata$COST_Travel_plus_Survey-15
# Collapse Geology levels containing limestone 
for (i in 1:length(CIdata$geology)){
  if(CIdata$geology[i] == 1 | CIdata$geology[i] == 2 | CIdata$geology[i] == 3 | CIdata$geology[i] == 4 | CIdata$geology[i] == 7) {CIdata$geology_collapsed[i] <- 1}
    if (CIdata$geology[i] == 5 | CIdata$geology[i] == 6 | CIdata$geology[i] == 8) {CIdata$geology_collapsed[i] <- 5}
      if (CIdata$geology[i] == 9) {CIdata$geology_collapsed[i] <- 9}
}  
write.table(CIdata,file="CIdata_all_years_Predictors_Cost_collapsed_geology.txt",sep="\t",col.names=T,row.names=F)
# Re-arrange dataset so that observations of repeat visits appear next to each other 
unique(CIdata$Year)
CIdata2001 <- subset(CIdata,CIdata$Year == 2001)
CIdata2003 <- subset(CIdata,CIdata$Year == 2003)
CIdata2005 <- subset(CIdata,CIdata$Year == 2005)
CIdata2007 <- subset(CIdata,CIdata$Year == 2007)
CIdata2009 <- subset(CIdata,CIdata$Year == 2009)
CIdata2011 <- subset(CIdata,CIdata$Year == 2011)
CIdata2013 <- subset(CIdata,CIdata$Year == 2013)
CIdata2015 <- subset(CIdata,CIdata$Year == 2015)
Data_2009 <- read.csv("repeat_visits_2009.csv",header=TRUE)
Data_2011 <- read.csv("repeat_visits_2011.csv",header=TRUE)
# add 3 columns to each dataframe -> obs_1, obs_2 and obs_3  
CIdata2001$obs1 <- CIdata2001$p_a_SC
CIdata2001$obs2 <- 100 # arbitatrary number instead of NA in order to make the if-statement run (see further)
CIdata2001$obs3 <- 100
CIdata2003$obs1 <- CIdata2003$p_a_SC
CIdata2003$obs2 <- 100
CIdata2003$obs3 <- 100 
CIdata2005$obs1 <- CIdata2005$p_a_SC
CIdata2005$obs2 <- 100
CIdata2005$obs3 <- 100 
CIdata2007$obs1 <- CIdata2007$p_a_SC
CIdata2007$obs2 <- 100
CIdata2007$obs3 <- 100 
CIdata2009$obs1 <- CIdata2009$p_a_SC
dup <- which(duplicated(CIdata2009$WPT)) # returns indices of duplicated waypoints 
CIdata2009 <- CIdata2009[-dup, ] # remove duplicated waypoints
which(duplicated(CIdata2009$WPT))
CIdata2009$obs2 <- Data_2009[ ,4]
CIdata2009$obs3 <- Data_2009[ ,5]
CIdata2011$obs1 <- CIdata2011$p_a_SC
dup <- which(duplicated(CIdata2011$WPT)) # returns indices of duplicated waypoints 
CIdata2011 <- CIdata2011[-dup, ] # remove duplicated waypoints
which(duplicated(CIdata2011$WPT))
CIdata2011$obs2 <- Data_2011[ ,4]
CIdata2011$obs3 <- Data_2011[ ,5]
CIdata2013$obs1 <- CIdata2013$p_a_SC
CIdata2013$obs2 <- 100
CIdata2013$obs3 <- 100 
CIdata2015$obs1 <- CIdata2015$p_a_SC
CIdata2015$obs2 <- 100
CIdata2015$obs3 <- 100 
# combine all dataframes again - observations are now in columnn obs1, obs2, obs3
CIdata_multi_visits <- rbind(CIdata2001, CIdata2003, CIdata2005, CIdata2007, CIdata2009, CIdata2011, CIdata2013, CIdata2015)
# change p_a_SC according to the repeat visits - as soon as one presence is observed, p_a_SC should be 1 
for (i in 1:length(CIdata_multi_visits)){
  if(CIdata_multi_visits$obs1[i] == 1 | CIdata_multi_visits$obs2[i] == 1 | CIdata_multi_visits$obs3[i] == 1){CIdata_multi_visits$p_a_SC[i] <- 1}
}
write.csv(CIdata_multi_visits, file="CIdata_multi_visits.csv") # csv file to read into unmarked 

## when starting from here
CIdata_multi_visits <- read.csv("CIdata_multi_visits.csv", header=TRUE)

## Split CIdata_multi_visits dataframe into different datasets, one for each year, now with updated p_a_SC 
CIdata2001 <- subset(CIdata_multi_visits,CIdata_multi_visits$Year == 2001)
CIdata2003 <- subset(CIdata_multi_visits,CIdata_multi_visits$Year == 2003)
CIdata2005 <- subset(CIdata_multi_visits,CIdata_multi_visits$Year == 2005)
CIdata2007 <- subset(CIdata_multi_visits,CIdata_multi_visits$Year == 2007)
CIdata2009 <- subset(CIdata_multi_visits,CIdata_multi_visits$Year == 2009)
CIdata2011 <- subset(CIdata_multi_visits,CIdata_multi_visits$Year == 2011)
CIdata2013 <- subset(CIdata_multi_visits,CIdata_multi_visits$Year == 2013)
CIdata2015 <- subset(CIdata_multi_visits,CIdata_multi_visits$Year == 2015)

## Only keep the survey points that were surveyed every year between 2001 and 2015
WPTs_to_keep <- Reduce(intersect, list(CIdata2001$WPT, CIdata2003$WPT,  CIdata2005$WPT, CIdata2007$WPT, 
                              CIdata2009$WPT, CIdata2011$WPT, CIdata2013$WPT, CIdata2015$WPT))

CIdata2001 <- CIdata2001[match(WPTs_to_keep, CIdata2001$WPT), ]
CIdata2003 <- CIdata2003[match(WPTs_to_keep, CIdata2003$WPT), ]
CIdata2005 <- CIdata2005[match(WPTs_to_keep, CIdata2005$WPT), ]
CIdata2007 <- CIdata2007[match(WPTs_to_keep, CIdata2007$WPT), ]
CIdata2009 <- CIdata2009[match(WPTs_to_keep, CIdata2009$WPT), ]
CIdata2011 <- CIdata2011[match(WPTs_to_keep, CIdata2011$WPT), ]
CIdata2013 <- CIdata2013[match(WPTs_to_keep, CIdata2013$WPT), ]
CIdata2015 <- CIdata2015[match(WPTs_to_keep, CIdata2015$WPT), ]

## percentages of sites being surveyed 
aa <- vector()
bb <- vector()
percentages <- list() 

## Subset each dataset by systematically taking out sites geographically
# fill list 
for (i in 0:99){
  aa <- seq(i, length(WPTs_to_keep), 100)
  bb <- c(bb, aa)
  percentages[[i+1]] <- bb
}
# sort the integers of each vector in list 
for (i in 1:100){
  percentages[[i]] <- sort(percentages[[i]])
}

# subset full datasets by taking out specific sites according to percentage that is surveyed
CIdata2001_list <- list() 
CIdata2003_list <- list()  
CIdata2005_list <- list()  
CIdata2007_list <- list()
CIdata2009_list <- list()
CIdata2011_list <- list()
CIdata2013_list <- list()
CIdata2015_list <- list()

for (i in 1:100){
  CIdata2001_list[[i]] <- CIdata2001[percentages[[i]], ]# this list contains dataframes with data for 10 to 1000 sites, incrementally
  # increasing with 10 sites (from 1 % to 100 % of total island) for 2001
  CIdata2003_list[[i]] <- CIdata2003[percentages[[i]], ] # same for 2003
  CIdata2005_list[[i]] <- CIdata2005[percentages[[i]], ] # etc
  CIdata2007_list[[i]] <- CIdata2007[percentages[[i]], ]
  CIdata2009_list[[i]] <- CIdata2009[percentages[[i]], ]
  CIdata2011_list[[i]] <- CIdata2011[percentages[[i]], ]
  CIdata2013_list[[i]] <- CIdata2013[percentages[[i]], ]
  CIdata2015_list[[i]] <- CIdata2015[percentages[[i]], ]
}

# cumulatively merge datasets years 
CIdata2001_2003_list <- list()
CIdata2001_2003_2005_list <- list()
CIdata2001_2003_2005_2007_list <- list()
CIdata2001_2003_2005_2007_2009_list <- list()
CIdata2001_2003_2005_2007_2009_2011_list <- list()
CIdata2001_2003_2005_2007_2009_2011_2013_list <- list()
CIdata2001_2003_2005_2007_2009_2011_2013_2015_list <- list()

for (i in 1:100){
  CIdata2001_2003_list[[i]] <- rbind(CIdata2001_list[[i]], CIdata2003_list[[i]]) # bind previous dataframes - with data for 10 to 1000 sites
  # incrementally increasing with 10 sites for 2001 and 2003 
  CIdata2001_2003_2005_list[[i]] <- rbind(CIdata2001_list[[i]], CIdata2003_list[[i]], CIdata2005_list[[i]]) # and 2005 and so on 
  CIdata2001_2003_2005_2007_list[[i]] <- rbind(CIdata2001_list[[i]], CIdata2003_list[[i]], CIdata2005_list[[i]], CIdata2007_list[[i]])
  CIdata2001_2003_2005_2007_2009_list[[i]] <- rbind(CIdata2001_list[[i]], CIdata2003_list[[i]], CIdata2005_list[[i]], CIdata2007_list[[i]], CIdata2009_list[[i]])
  CIdata2001_2003_2005_2007_2009_2011_list[[i]] <- rbind(CIdata2001_list[[i]], CIdata2003_list[[i]], CIdata2005_list[[i]], CIdata2007_list[[i]], CIdata2009_list[[i]], CIdata2011_list[[i]])
  CIdata2001_2003_2005_2007_2009_2011_2013_list[[i]] <- rbind(CIdata2001_list[[i]], CIdata2003_list[[i]], CIdata2005_list[[i]], CIdata2007_list[[i]], CIdata2009_list[[i]], CIdata2011_list[[i]], CIdata2013_list[[i]])
  CIdata2001_2003_2005_2007_2009_2011_2013_2015_list[[i]] <- rbind(CIdata2001_list[[i]], CIdata2003_list[[i]], CIdata2005_list[[i]], CIdata2007_list[[i]], CIdata2009_list[[i]], CIdata2011_list[[i]], CIdata2013_list[[i]], CIdata2015_list[[i]])
}

# calculate prevalence of presences for the different datasets 
pres2001_2003 <- vector()
pres2001_2003_2005 <- vector()
pres2001_2003_2005_2007 <- vector()
pres2001_2003_2005_2007_2009 <- vector()
pres2001_2003_2005_2007_2009_2011 <- vector()
pres2001_2003_2005_2007_2009_2011_2013 <- vector()
datapoints2001_2003 <- vector()
datapoints2001_2003_2005 <- vector()
datapoints2001_2003_2005_2007 <- vector()
datapoints2001_2003_2005_2007_2009 <- vector()
datapoints2001_2003_2005_2007_2009_2011 <- vector()
datapoints2001_2003_2005_2007_2009_2011_2013 <- vector()
prevalence_pres2001_2003 <- vector()
prevalence_pres2001_2003_2005 <- vector()
prevalence_pres2001_2003_2005_2007 <- vector()
prevalence_pres2001_2003_2005_2007_2009 <- vector()
prevalence_pres2001_2003_2005_2007_2009_2011 <- vector()
prevalence_pres2001_2003_2005_2007_2009_2011_2013 <- vector()

for (i in 1:100){
  pres2001_2003[i] <- sum(CIdata2001_2003_list[[i]]$p_a_SC)
  datapoints2001_2003[i] <- nrow(CIdata2001_2003_list[[i]])
  prevalence_pres2001_2003[i] <- pres2001_2003[i]/datapoints2001_2003[i]
}
for (i in 1:100){
  pres2001_2003_2005[i] <- sum(CIdata2001_2003_2005_list[[i]]$p_a_SC)
  datapoints2001_2003_2005[i] <- nrow(CIdata2001_2003_2005_list[[i]])
  prevalence_pres2001_2003_2005[i] <- pres2001_2003_2005[i]/datapoints2001_2003_2005[i]
}
for (i in 1:100){
  pres2001_2003_2005_2007[i] <- sum(CIdata2001_2003_2005_2007_list[[i]]$p_a_SC)
  datapoints2001_2003_2005_2007[i] <- nrow(CIdata2001_2003_2005_2007_list[[i]])
  prevalence_pres2001_2003_2005_2007[i] <- pres2001_2003_2005_2007[i]/datapoints2001_2003_2005_2007[i]
}
for (i in 1:100){
  pres2001_2003_2005_2007_2009[i] <- sum(CIdata2001_2003_2005_2007_2009_list[[i]]$p_a_SC)
  datapoints2001_2003_2005_2007_2009[i] <- nrow(CIdata2001_2003_2005_2007_2009_list[[i]])
  prevalence_pres2001_2003_2005_2007_2009[i] <- pres2001_2003_2005_2007_2009[i]/datapoints2001_2003_2005_2007_2009[i]
}
for (i in 1:100){
  pres2001_2003_2005_2007_2009_2011[i] <- sum(CIdata2001_2003_2005_2007_2009_2011_list[[i]]$p_a_SC)
  datapoints2001_2003_2005_2007_2009_2011[i] <- nrow(CIdata2001_2003_2005_2007_2009_2011_list[[i]])
  prevalence_pres2001_2003_2005_2007_2009_2011[i] <- pres2001_2003_2005_2007_2009_2011[i]/datapoints2001_2003_2005_2007_2009_2011[i]
}
for (i in 1:100){
  pres2001_2003_2005_2007_2009_2011_2013[i] <- sum(CIdata2001_2003_2005_2007_2009_2011_2013_list[[i]]$p_a_SC)
  datapoints2001_2003_2005_2007_2009_2011_2013[i] <- nrow(CIdata2001_2003_2005_2007_2009_2011_2013_list[[i]])
  prevalence_pres2001_2003_2005_2007_2009_2011_2013[i] <- pres2001_2003_2005_2007_2009_2011_2013[i]/datapoints2001_2003_2005_2007_2009_2011_2013[i]
}
prevalence_pres2001_2003
prevalence_pres2001_2003_2005 
prevalence_pres2001_2003_2005_2007 
prevalence_pres2001_2003_2005_2007_2009
prevalence_pres2001_2003_2005_2007_2009_2011 
prevalence_pres2001_2003_2005_2007_2009_2011_2013 

# scale elevation and distance from road 
for (i in 1:100) {
  CIdata2001_2003_list[[i]]$sd_elev <- as.numeric(scale(CIdata2001_2003_list[[i]]$Elevation))
  CIdata2001_2003_list[[i]]$sd_DFR <- as.numeric(scale(CIdata2001_2003_list[[i]]$DistanceFromRoad))
  CIdata2001_2003_list[[i]]$sd_rainfall_meters <- as.numeric(scale(CIdata2001_2003_list[[i]]$rainfall_meters))
}

for (i in 1:100) {
  CIdata2001_2003_2005_list[[i]]$sd_elev <- as.numeric(scale(CIdata2001_2003_2005_list[[i]]$Elevation))
  CIdata2001_2003_2005_list[[i]]$sd_DFR <- as.numeric(scale(CIdata2001_2003_2005_list[[i]]$DistanceFromRoad))
  CIdata2001_2003_2005_list[[i]]$sd_rainfall_meters <- as.numeric(scale(CIdata2001_2003_2005_list[[i]]$rainfall_meters))
}  

for (i in 1:100) {
  CIdata2001_2003_2005_2007_list[[i]]$sd_elev <- as.numeric(scale(CIdata2001_2003_2005_2007_list[[i]]$Elevation))
  CIdata2001_2003_2005_2007_list[[i]]$sd_DFR <- as.numeric(scale(CIdata2001_2003_2005_2007_list[[i]]$DistanceFromRoad))
  CIdata2001_2003_2005_2007_list[[i]]$sd_rainfall_meters <- as.numeric(scale(CIdata2001_2003_2005_2007_list[[i]]$rainfall_meters))
}

for (i in 1:100) {
  CIdata2001_2003_2005_2007_2009_list[[i]]$sd_elev <- as.numeric(scale(CIdata2001_2003_2005_2007_2009_list[[i]]$Elevation))
  CIdata2001_2003_2005_2007_2009_list[[i]]$sd_DFR <- as.numeric(scale(CIdata2001_2003_2005_2007_2009_list[[i]]$DistanceFromRoad))
  CIdata2001_2003_2005_2007_2009_list[[i]]$sd_rainfall_meters <- as.numeric(scale(CIdata2001_2003_2005_2007_2009_list[[i]]$rainfall_meters))
}

for (i in 1:100) {
  CIdata2001_2003_2005_2007_2009_2011_list[[i]]$sd_elev <- as.numeric(scale(CIdata2001_2003_2005_2007_2009_2011_list[[i]]$Elevation))
  CIdata2001_2003_2005_2007_2009_2011_list[[i]]$sd_DFR <- as.numeric(scale(CIdata2001_2003_2005_2007_2009_2011_list[[i]]$DistanceFromRoad))
  CIdata2001_2003_2005_2007_2009_2011_list[[i]]$sd_rainfall_meters <- as.numeric(scale(CIdata2001_2003_2005_2007_2009_2011_list[[i]]$rainfall_meters))
}

for (i in 1:100) {
  CIdata2001_2003_2005_2007_2009_2011_2013_list[[i]]$sd_elev <- as.numeric(scale(CIdata2001_2003_2005_2007_2009_2011_2013_list[[i]]$Elevation))
  CIdata2001_2003_2005_2007_2009_2011_2013_list[[i]]$sd_DFR <- as.numeric(scale(CIdata2001_2003_2005_2007_2009_2011_2013_list[[i]]$DistanceFromRoad))
  CIdata2001_2003_2005_2007_2009_2011_2013_list[[i]]$sd_rainfall_meters <- as.numeric(scale(CIdata2001_2003_2005_2007_2009_2011_2013_list[[i]]$rainfall_meters))
}

for (i in 1:100) {
  CIdata2001_2003_2005_2007_2009_2011_2013_2015_list[[i]]$sd_elev <- as.numeric(scale(CIdata2001_2003_2005_2007_2009_2011_2013_2015_list[[i]]$Elevation))
  CIdata2001_2003_2005_2007_2009_2011_2013_2015_list[[i]]$sd_DFR <- as.numeric(scale(CIdata2001_2003_2005_2007_2009_2011_2013_2015_list[[i]]$DistanceFromRoad))
  CIdata2001_2003_2005_2007_2009_2011_2013_2015_list[[i]]$sd_rainfall_meters <- as.numeric(scale(CIdata2001_2003_2005_2007_2009_2011_2013_2015_list[[i]]$rainfall_meters))
}

# scale elevation and DFR for AUC calculation 
for (i in 1:100) {
  CIdata2001_list[[i]]$sd_elev <- as.numeric(scale(CIdata2001_list[[i]]$Elevation))
  CIdata2001_list[[i]]$sd_DFR <- as.numeric(scale(CIdata2001_list[[i]]$DistanceFromRoad))
  CIdata2001_list[[i]]$sd_rainfall_meters <- as.numeric(scale(CIdata2001_list[[i]]$rainfall_meters))
}
for (i in 1:100) {
  CIdata2003_list[[i]]$sd_elev <- as.numeric(scale(CIdata2003_list[[i]]$Elevation))
  CIdata2003_list[[i]]$sd_DFR <- as.numeric(scale(CIdata2003_list[[i]]$DistanceFromRoad))
  CIdata2003_list[[i]]$sd_rainfall_meters <- as.numeric(scale(CIdata2003_list[[i]]$rainfall_meters))
}
for (i in 1:100) {
  CIdata2005_list[[i]]$sd_elev <- as.numeric(scale(CIdata2005_list[[i]]$Elevation))
  CIdata2005_list[[i]]$sd_DFR <- as.numeric(scale(CIdata2005_list[[i]]$DistanceFromRoad))
  CIdata2005_list[[i]]$sd_rainfall_meters <- as.numeric(scale(CIdata2005_list[[i]]$rainfall_meters))
}
for (i in 1:100) {
  CIdata2007_list[[i]]$sd_elev <- as.numeric(scale(CIdata2007_list[[i]]$Elevation))
  CIdata2007_list[[i]]$sd_DFR <- as.numeric(scale(CIdata2007_list[[i]]$DistanceFromRoad))
  CIdata2007_list[[i]]$sd_rainfall_meters <- as.numeric(scale(CIdata2007_list[[i]]$rainfall_meters))
}
for (i in 1:100) {
  CIdata2009_list[[i]]$sd_elev <- as.numeric(scale(CIdata2009_list[[i]]$Elevation))
  CIdata2009_list[[i]]$sd_DFR <- as.numeric(scale(CIdata2009_list[[i]]$DistanceFromRoad))
  CIdata2009_list[[i]]$sd_rainfall_meters <- as.numeric(scale(CIdata2009_list[[i]]$rainfall_meters))
}
for (i in 1:100) {
  CIdata2011_list[[i]]$sd_elev <- as.numeric(scale(CIdata2011_list[[i]]$Elevation))
  CIdata2011_list[[i]]$sd_DFR <- as.numeric(scale(CIdata2011_list[[i]]$DistanceFromRoad))
  CIdata2011_list[[i]]$sd_rainfall_meters <- as.numeric(scale(CIdata2011_list[[i]]$rainfall_meters))
}
for (i in 1:100) {
  CIdata2013_list[[i]]$sd_elev <- as.numeric(scale(CIdata2013_list[[i]]$Elevation))
  CIdata2013_list[[i]]$sd_DFR <- as.numeric(scale(CIdata2013_list[[i]]$DistanceFromRoad))
  CIdata2013_list[[i]]$sd_rainfall_meters <- as.numeric(scale(CIdata2013_list[[i]]$rainfall_meters))
}
for (i in 1:100) {
  CIdata2015_list[[i]]$sd_elev <- as.numeric(scale(CIdata2015_list[[i]]$Elevation))
  CIdata2015_list[[i]]$sd_DFR <- as.numeric(scale(CIdata2015_list[[i]]$DistanceFromRoad))
  CIdata2015_list[[i]]$sd_rainfall_meters <- as.numeric(scale(CIdata2015_list[[i]]$rainfall_meters))
}



## Plot surveyed sites 
# load required packages
library(raster)
library(rgdal)

# load coast layer 
setwd("../Data/Files_created_by_EVB/Rcode_Darren_separateyears/XmasIsland/6_GIS layers")
coast <- readOGR(dsn=getwd(), layer="coast_poly")
plot(coast)

# plot the sites that are surveyed 
plot(CIdata2001_list[[1]]$avg_X_MARK, CIdata2001_list[[1]]$avg_Y_MARK, xlim=c(556000, 580000), ylim=c(8830000, 8850000), pch=21, cex=0.7, col="darkgreen", bg="darkgreen")
plot(coast, add=TRUE)
plot(CIdata2001_list[[10]]$avg_X_MARK, CIdata2001_list[[10]]$avg_Y_MARK, xlim=c(556000, 580000), ylim=c(8830000, 8850000), pch=21, cex=0.7, col="darkgreen", bg="darkgreen")
plot(coast, add=TRUE)
plot(CIdata2001_list[[20]]$avg_X_MARK, CIdata2001_list[[20]]$avg_Y_MARK, xlim=c(556000, 580000), ylim=c(8830000, 8850000), pch=21, cex=0.7, col="darkgreen", bg="darkgreen")
plot(coast, add=TRUE)
plot(CIdata2001_list[[30]]$avg_X_MARK, CIdata2001_list[[30]]$avg_Y_MARK, xlim=c(556000, 580000), ylim=c(8830000, 8850000), pch=21, cex=0.7, col="darkgreen", bg="darkgreen")
plot(coast, add=TRUE)
plot(CIdata2001_list[[40]]$avg_X_MARK, CIdata2001_list[[40]]$avg_Y_MARK, xlim=c(556000, 580000), ylim=c(8830000, 8850000), pch=21, cex=0.7, col="darkgreen", bg="darkgreen")
plot(coast, add=TRUE)
plot(CIdata2001_list[[50]]$avg_X_MARK, CIdata2001_list[[50]]$avg_Y_MARK, xlim=c(556000, 580000), ylim=c(8830000, 8850000), pch=21, cex=0.7, col="darkgreen", bg="darkgreen")
plot(coast, add=TRUE)
plot(CIdata2001_list[[60]]$avg_X_MARK, CIdata2001_list[[60]]$avg_Y_MARK, xlim=c(556000, 580000), ylim=c(8830000, 8850000), pch=21, cex=0.7, col="darkgreen", bg="darkgreen")
plot(coast, add=TRUE)
plot(CIdata2001_list[[70]]$avg_X_MARK, CIdata2001_list[[70]]$avg_Y_MARK, xlim=c(556000, 580000), ylim=c(8830000, 8850000), pch=21, cex=0.7, col="darkgreen", bg="darkgreen")
plot(coast, add=TRUE)
plot(CIdata2001_list[[80]]$avg_X_MARK, CIdata2001_list[[80]]$avg_Y_MARK, xlim=c(556000, 580000), ylim=c(8830000, 8850000), pch=21, cex=0.7, col="darkgreen", bg="darkgreen")
plot(coast, add=TRUE)
plot(CIdata2001_list[[90]]$avg_X_MARK, CIdata2001_list[[90]]$avg_Y_MARK, xlim=c(556000, 580000), ylim=c(8830000, 8850000), pch=21, cex=0.7, col="darkgreen", bg="darkgreen")
plot(coast, add=TRUE)
plot(CIdata2001_list[[100]]$avg_X_MARK, CIdata2001_list[[100]]$avg_Y_MARK, xlim=c(556000, 580000), ylim=c(8830000, 8850000), pch=21, cex=0.7, col="darkgreen", bg="darkgreen")
plot(coast, add=TRUE)






################################################## Archive with all_data_files (10-20-30% sites monitored) ###################################################


Percent_1 <- seq(1, length(WPTs_to_keep), 100)
Percent_5 <- seq(5, length(WPTs_to_keep), 100)
Percent_10 <- seq(0, length(WPTs_to_keep), 10)
Percent_20 <- seq(1, length(WPTs_to_keep), 10)
Percent_30 <- seq(2, length(WPTs_to_keep), 10)
Percent_40 <- seq(3, length(WPTs_to_keep), 10)
Percent_50 <- seq(4, length(WPTs_to_keep), 10)
Percent_60 <- seq(5, length(WPTs_to_keep), 10)
Percent_70 <- seq(6, length(WPTs_to_keep), 10)
Percent_80 <- seq(7, length(WPTs_to_keep), 10)
Percent_90 <- seq(8, length(WPTs_to_keep), 10)

# 1% of sites
CIdata2001_1_percent <- CIdata2001[Percent_1, ] 
CIdata2003_1_percent <- CIdata2003[Percent_1, ]
CIdata2005_1_percent <- CIdata2005[Percent_1, ]
CIdata2007_1_percent <- CIdata2007[Percent_1, ]
CIdata2009_1_percent <- CIdata2009[Percent_1, ]
CIdata2011_1_percent <- CIdata2011[Percent_1, ]
CIdata2013_1_percent <- CIdata2013[Percent_1, ]
CIdata2015_1_percent <- CIdata2015[Percent_1, ]
# 5% of sites
CIdata2001_5_percent <- CIdata2001[Percent_5, ] 
CIdata2003_5_percent <- CIdata2003[Percent_5, ]
CIdata2005_5_percent <- CIdata2005[Percent_5, ]
CIdata2007_5_percent <- CIdata2007[Percent_5, ]
CIdata2009_5_percent <- CIdata2009[Percent_5, ]
CIdata2011_5_percent <- CIdata2011[Percent_5, ]
CIdata2013_5_percent <- CIdata2013[Percent_5, ]
CIdata2015_5_percent <- CIdata2015[Percent_5, ]
# 10% of sites 
CIdata2001_10_percent <- CIdata2001[Percent_10, ] 
CIdata2003_10_percent <- CIdata2003[Percent_10, ]
CIdata2005_10_percent <- CIdata2005[Percent_10, ]
CIdata2007_10_percent <- CIdata2007[Percent_10, ]
CIdata2009_10_percent <- CIdata2009[Percent_10, ]
CIdata2011_10_percent <- CIdata2011[Percent_10, ]
CIdata2013_10_percent <- CIdata2013[Percent_10, ]
CIdata2015_10_percent <- CIdata2015[Percent_10, ]

# 20% of sites
CIdata2001_20_percent <- CIdata2001[sort(c(Percent_10,Percent_20)), ] 
CIdata2003_20_percent <- CIdata2003[sort(c(Percent_10,Percent_20)), ]
CIdata2005_20_percent <- CIdata2005[sort(c(Percent_10,Percent_20)), ]
CIdata2007_20_percent <- CIdata2007[sort(c(Percent_10,Percent_20)), ]
CIdata2009_20_percent <- CIdata2009[sort(c(Percent_10,Percent_20)), ]
CIdata2011_20_percent <- CIdata2011[sort(c(Percent_10,Percent_20)), ]
CIdata2013_20_percent <- CIdata2013[sort(c(Percent_10,Percent_20)), ]
CIdata2015_20_percent <- CIdata2015[sort(c(Percent_10,Percent_20)), ]

# 30% of sites
CIdata2001_30_percent <- CIdata2001[sort(c(Percent_10,Percent_20,Percent_30)), ] 
CIdata2003_30_percent <- CIdata2003[sort(c(Percent_10,Percent_20,Percent_30)), ]
CIdata2005_30_percent <- CIdata2005[sort(c(Percent_10,Percent_20,Percent_30)), ]
CIdata2007_30_percent <- CIdata2007[sort(c(Percent_10,Percent_20,Percent_30)), ]
CIdata2009_30_percent <- CIdata2009[sort(c(Percent_10,Percent_20,Percent_30)), ]
CIdata2011_30_percent <- CIdata2011[sort(c(Percent_10,Percent_20,Percent_30)), ]
CIdata2013_30_percent <- CIdata2013[sort(c(Percent_10,Percent_20,Percent_30)), ]
CIdata2015_30_percent <- CIdata2015[sort(c(Percent_10,Percent_20,Percent_30)), ]

# 40% of sites
CIdata2001_40_percent <- CIdata2001[sort(c(Percent_10,Percent_20,Percent_30,Percent_40)), ] 
CIdata2003_40_percent <- CIdata2003[sort(c(Percent_10,Percent_20,Percent_30,Percent_40)), ]
CIdata2005_40_percent <- CIdata2005[sort(c(Percent_10,Percent_20,Percent_30,Percent_40)), ]
CIdata2007_40_percent <- CIdata2007[sort(c(Percent_10,Percent_20,Percent_30,Percent_40)), ]
CIdata2009_40_percent <- CIdata2009[sort(c(Percent_10,Percent_20,Percent_30,Percent_40)), ]
CIdata2011_40_percent <- CIdata2011[sort(c(Percent_10,Percent_20,Percent_30,Percent_40)), ]
CIdata2013_40_percent <- CIdata2013[sort(c(Percent_10,Percent_20,Percent_30,Percent_40)), ]
CIdata2015_40_percent <- CIdata2015[sort(c(Percent_10,Percent_20,Percent_30,Percent_40)), ]

# 50% of sites
CIdata2001_50_percent <- CIdata2001[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50)), ] 
CIdata2003_50_percent <- CIdata2003[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50)), ]
CIdata2005_50_percent <- CIdata2005[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50)), ]
CIdata2007_50_percent <- CIdata2007[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50)), ]
CIdata2009_50_percent <- CIdata2009[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50)), ]
CIdata2011_50_percent <- CIdata2011[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50)), ]
CIdata2013_50_percent <- CIdata2013[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50)), ]
CIdata2015_50_percent <- CIdata2015[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50)), ]

# 60% of sites
CIdata2001_60_percent <- CIdata2001[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60)), ] 
CIdata2003_60_percent <- CIdata2003[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60)), ]
CIdata2005_60_percent <- CIdata2005[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60)), ]
CIdata2007_60_percent <- CIdata2007[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60)), ]
CIdata2009_60_percent <- CIdata2009[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60)), ]
CIdata2011_60_percent <- CIdata2011[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60)), ]
CIdata2013_60_percent <- CIdata2013[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60)), ]
CIdata2015_60_percent <- CIdata2015[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60)), ]

# 70% of sites
CIdata2001_70_percent <- CIdata2001[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70)), ] 
CIdata2003_70_percent <- CIdata2003[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70)), ]
CIdata2005_70_percent <- CIdata2005[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70)), ]
CIdata2007_70_percent <- CIdata2007[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70)), ]
CIdata2009_70_percent <- CIdata2009[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70)), ]
CIdata2011_70_percent <- CIdata2011[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70)), ]
CIdata2013_70_percent <- CIdata2013[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70)), ]
CIdata2015_70_percent <- CIdata2015[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70)), ]

# 80% of sites
CIdata2001_80_percent <- CIdata2001[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80)), ] 
CIdata2003_80_percent <- CIdata2003[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80)), ]
CIdata2005_80_percent <- CIdata2005[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80)), ]
CIdata2007_80_percent <- CIdata2007[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80)), ]
CIdata2009_80_percent <- CIdata2009[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80)), ]
CIdata2011_80_percent <- CIdata2011[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80)), ]
CIdata2013_80_percent <- CIdata2013[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80)), ]
CIdata2015_80_percent <- CIdata2015[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80)), ]

# 90% of sites
CIdata2001_90_percent <- CIdata2001[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80,Percent_90)), ] 
CIdata2003_90_percent <- CIdata2003[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80,Percent_90)), ]
CIdata2005_90_percent <- CIdata2005[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80,Percent_90)), ]
CIdata2007_90_percent <- CIdata2007[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80,Percent_90)), ]
CIdata2009_90_percent <- CIdata2009[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80,Percent_90)), ]
CIdata2011_90_percent <- CIdata2011[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80,Percent_90)), ]
CIdata2013_90_percent <- CIdata2013[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80,Percent_90)), ]
CIdata2015_90_percent <- CIdata2015[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80,Percent_90)), ]

# 100% of sites
CIdata2001_100_percent <- CIdata2001
CIdata2003_100_percent <- CIdata2003
CIdata2005_100_percent <- CIdata2005
CIdata2007_100_percent <- CIdata2007
CIdata2009_100_percent <- CIdata2009
CIdata2011_100_percent <- CIdata2011
CIdata2013_100_percent <- CIdata2013
CIdata2015_100_percent <- CIdata2015

## Merge datasets by proportion of sites kept
# 1% of sites 
CIdata2001_2003_1_percent <- rbind(CIdata2001_1_percent, CIdata2003_1_percent)
CIdata2001_2003_2005_1_percent <- rbind(CIdata2001_2003_1_percent, CIdata2005_1_percent) 
CIdata2001_2003_2005_2007_1_percent <- rbind(CIdata2001_2003_2005_1_percent, CIdata2007_1_percent)
CIdata2001_2003_2005_2007_2009_1_percent <- rbind(CIdata2001_2003_2005_2007_1_percent, CIdata2009_1_percent)
CIdata2001_2003_2005_2007_2009_2011_1_percent <- rbind(CIdata2001_2003_2005_2007_2009_1_percent, CIdata2011_1_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_1_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_1_percent, CIdata2013_1_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_2015_1_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_2013_1_percent, CIdata2015_1_percent)

# 5% of sites
CIdata2001_2003_5_percent <- rbind(CIdata2001_5_percent, CIdata2003_5_percent)
CIdata2001_2003_2005_5_percent <- rbind(CIdata2001_2003_5_percent, CIdata2005_5_percent) 
CIdata2001_2003_2005_2007_5_percent <- rbind(CIdata2001_2003_2005_5_percent, CIdata2007_5_percent)
CIdata2001_2003_2005_2007_2009_5_percent <- rbind(CIdata2001_2003_2005_2007_5_percent, CIdata2009_5_percent)
CIdata2001_2003_2005_2007_2009_2011_5_percent <- rbind(CIdata2001_2003_2005_2007_2009_5_percent, CIdata2011_5_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_5_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_5_percent, CIdata2013_5_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_2015_5_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_2013_5_percent, CIdata2015_5_percent)

# 10% of sites 
CIdata2001_2003_10_percent <- rbind(CIdata2001_10_percent, CIdata2003_10_percent)
CIdata2001_2003_2005_10_percent <- rbind(CIdata2001_2003_10_percent, CIdata2005_10_percent) 
CIdata2001_2003_2005_2007_10_percent <- rbind(CIdata2001_2003_2005_10_percent, CIdata2007_10_percent)
CIdata2001_2003_2005_2007_2009_10_percent <- rbind(CIdata2001_2003_2005_2007_10_percent, CIdata2009_10_percent)
CIdata2001_2003_2005_2007_2009_2011_10_percent <- rbind(CIdata2001_2003_2005_2007_2009_10_percent, CIdata2011_10_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_10_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_10_percent, CIdata2013_10_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_2015_10_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_2013_10_percent, CIdata2015_10_percent)

# 20% of sites 
CIdata2001_2003_20_percent <- rbind(CIdata2001_20_percent, CIdata2003_20_percent)
CIdata2001_2003_2005_20_percent <- rbind(CIdata2001_2003_20_percent, CIdata2005_20_percent) 
CIdata2001_2003_2005_2007_20_percent <- rbind(CIdata2001_2003_2005_20_percent, CIdata2007_20_percent)
CIdata2001_2003_2005_2007_2009_20_percent <- rbind(CIdata2001_2003_2005_2007_20_percent, CIdata2009_20_percent)
CIdata2001_2003_2005_2007_2009_2011_20_percent <- rbind(CIdata2001_2003_2005_2007_2009_10_percent, CIdata2011_10_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_20_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_20_percent, CIdata2013_20_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_2015_20_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_2013_20_percent, CIdata2015_20_percent)

# 30% of sites 
CIdata2001_2003_30_percent <- rbind(CIdata2001_30_percent, CIdata2003_30_percent)
CIdata2001_2003_2005_30_percent <- rbind(CIdata2001_2003_30_percent, CIdata2005_30_percent) 
CIdata2001_2003_2005_2007_30_percent <- rbind(CIdata2001_2003_2005_30_percent, CIdata2007_30_percent)
CIdata2001_2003_2005_2007_2009_30_percent <- rbind(CIdata2001_2003_2005_2007_30_percent, CIdata2009_30_percent)
CIdata2001_2003_2005_2007_2009_2011_30_percent <- rbind(CIdata2001_2003_2005_2007_2009_30_percent, CIdata2011_30_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_30_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_30_percent, CIdata2013_30_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_2015_30_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_2013_30_percent, CIdata2015_30_percent)

# 40% of sites 
CIdata2001_2003_40_percent <- rbind(CIdata2001_40_percent, CIdata2003_40_percent)
CIdata2001_2003_2005_40_percent <- rbind(CIdata2001_2003_40_percent, CIdata2005_40_percent) 
CIdata2001_2003_2005_2007_40_percent <- rbind(CIdata2001_2003_2005_40_percent, CIdata2007_40_percent)
CIdata2001_2003_2005_2007_2009_40_percent <- rbind(CIdata2001_2003_2005_2007_40_percent, CIdata2009_40_percent)
CIdata2001_2003_2005_2007_2009_2011_40_percent <- rbind(CIdata2001_2003_2005_2007_2009_40_percent, CIdata2011_40_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_40_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_40_percent, CIdata2013_40_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_2015_40_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_2013_40_percent, CIdata2015_40_percent)

# 50% of sites 
CIdata2001_2003_50_percent <- rbind(CIdata2001_50_percent, CIdata2003_50_percent)
CIdata2001_2003_2005_50_percent <- rbind(CIdata2001_2003_50_percent, CIdata2005_50_percent) 
CIdata2001_2003_2005_2007_50_percent <- rbind(CIdata2001_2003_2005_50_percent, CIdata2007_50_percent)
CIdata2001_2003_2005_2007_2009_50_percent <- rbind(CIdata2001_2003_2005_2007_50_percent, CIdata2009_50_percent)
CIdata2001_2003_2005_2007_2009_2011_50_percent <- rbind(CIdata2001_2003_2005_2007_2009_50_percent, CIdata2011_50_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_50_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_50_percent, CIdata2013_50_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_2015_50_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_2013_50_percent, CIdata2015_50_percent)

# 60% of sites 
CIdata2001_2003_60_percent <- rbind(CIdata2001_60_percent, CIdata2003_60_percent)
CIdata2001_2003_2005_60_percent <- rbind(CIdata2001_2003_60_percent, CIdata2005_60_percent) 
CIdata2001_2003_2005_2007_60_percent <- rbind(CIdata2001_2003_2005_60_percent, CIdata2007_60_percent)
CIdata2001_2003_2005_2007_2009_60_percent <- rbind(CIdata2001_2003_2005_2007_60_percent, CIdata2009_60_percent)
CIdata2001_2003_2005_2007_2009_2011_60_percent <- rbind(CIdata2001_2003_2005_2007_2009_60_percent, CIdata2011_60_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_60_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_60_percent, CIdata2013_60_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_2015_60_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_2013_60_percent, CIdata2015_60_percent)

# 70% of sites 
CIdata2001_2003_70_percent <- rbind(CIdata2001_70_percent, CIdata2003_70_percent)
CIdata2001_2003_2005_70_percent <- rbind(CIdata2001_2003_70_percent, CIdata2005_70_percent) 
CIdata2001_2003_2005_2007_70_percent <- rbind(CIdata2001_2003_2005_70_percent, CIdata2007_70_percent)
CIdata2001_2003_2005_2007_2009_70_percent <- rbind(CIdata2001_2003_2005_2007_70_percent, CIdata2009_70_percent)
CIdata2001_2003_2005_2007_2009_2011_70_percent <- rbind(CIdata2001_2003_2005_2007_2009_70_percent, CIdata2011_70_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_70_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_70_percent, CIdata2013_70_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_2015_70_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_2013_70_percent, CIdata2015_70_percent)

# 80% of sites 
CIdata2001_2003_80_percent <- rbind(CIdata2001_80_percent, CIdata2003_80_percent)
CIdata2001_2003_2005_80_percent <- rbind(CIdata2001_2003_80_percent, CIdata2005_80_percent) 
CIdata2001_2003_2005_2007_80_percent <- rbind(CIdata2001_2003_2005_80_percent, CIdata2007_80_percent)
CIdata2001_2003_2005_2007_2009_80_percent <- rbind(CIdata2001_2003_2005_2007_80_percent, CIdata2009_80_percent)
CIdata2001_2003_2005_2007_2009_2011_80_percent <- rbind(CIdata2001_2003_2005_2007_2009_80_percent, CIdata2011_80_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_80_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_80_percent, CIdata2013_80_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_2015_80_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_2013_80_percent, CIdata2015_80_percent)

# 90% of sites 
CIdata2001_2003_90_percent <- rbind(CIdata2001_90_percent, CIdata2003_90_percent)
CIdata2001_2003_2005_90_percent <- rbind(CIdata2001_2003_90_percent, CIdata2005_90_percent) 
CIdata2001_2003_2005_2007_90_percent <- rbind(CIdata2001_2003_2005_90_percent, CIdata2007_90_percent)
CIdata2001_2003_2005_2007_2009_90_percent <- rbind(CIdata2001_2003_2005_2007_90_percent, CIdata2009_90_percent)
CIdata2001_2003_2005_2007_2009_2011_90_percent <- rbind(CIdata2001_2003_2005_2007_2009_90_percent, CIdata2011_90_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_90_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_90_percent, CIdata2013_90_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_2015_90_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_2013_90_percent, CIdata2015_90_percent)

# 100% of sites 
CIdata2001_2003_100_percent <- rbind(CIdata2001_100_percent, CIdata2003_100_percent)
CIdata2001_2003_2005_100_percent <- rbind(CIdata2001_2003_100_percent, CIdata2005_100_percent) 
CIdata2001_2003_2005_2007_100_percent <- rbind(CIdata2001_2003_2005_100_percent, CIdata2007_100_percent)
CIdata2001_2003_2005_2007_2009_100_percent <- rbind(CIdata2001_2003_2005_2007_100_percent, CIdata2009_100_percent)
CIdata2001_2003_2005_2007_2009_2011_100_percent <- rbind(CIdata2001_2003_2005_2007_2009_100_percent, CIdata2011_100_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_100_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_100_percent, CIdata2013_100_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_2015_100_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_2013_100_percent, CIdata2015_100_percent)

# Add all dataframes to a list 
all_data_frames <- list()
all_data_files <- vector()


percentage_of_total_survey_budget <- c(1, 5, seq(10, 100, 10)) # fractions of survey budget
cumulative_surveyed_years <- c("2001_2003", "2001_2003_2005", "2001_2003_2005_2007",
                               "2001_2003_2005_2007_2009","2001_2003_2005_2007_2009_2011", 
                               "2001_2003_2005_2007_2009_2011_2013")
# add all dataframes together 
for (i in cumulative_surveyed_years) {
  for (j in percentage_of_total_survey_budget){
    add <- paste("CIdata",i,"_",j,"_percent", sep="")
    all_data_files <- c(all_data_files, add)
  }
}
all_data_files # check 

for (i in 1:length(all_data_files)) {
  all_data_frames[[i]] <- get(all_data_files[i])
}

# scale elevation and distance from road 
for (i in 1:length(all_data_files)) {
  all_data_frames[[i]] <- get(all_data_files[i])
  all_data_frames[[i]]$sd_elev <- as.numeric(scale(all_data_frames[[i]]$Elevation))
  all_data_frames[[i]]$sd_DFR <- as.numeric(scale(all_data_frames[[i]]$DistanceFromRoad))
  all_data_frames[[i]]$sd_rainfall_meters <- as.numeric(scale(all_data_frames[[i]]$rainfall_meters))
}





















































######################################################## Archive #####################################################333
## Subset each dataset by systematically taking out sites (geographically)
Percent_10 <- seq(0, nrow(CIdata2001), 10)
Percent_20 <- seq(1, nrow(CIdata2001), 10)
Percent_30 <- seq(2, nrow(CIdata2001), 10)
Percent_40 <- seq(3, nrow(CIdata2001), 10)
Percent_50 <- seq(4, nrow(CIdata2001), 10)
Percent_60 <- seq(5, nrow(CIdata2001), 10)
Percent_70 <- seq(6, nrow(CIdata2001), 10)
Percent_80 <- seq(7, nrow(CIdata2001), 10)
Percent_90 <- seq(8, nrow(CIdata2001), 10)

# 10% of sites 
CIdata2001_10_percent <- CIdata2001[Percent_10, ] 
CIdata2003_10_percent <- CIdata2003[Percent_10, ]
CIdata2005_10_percent <- CIdata2005[Percent_10, ]
CIdata2007_10_percent <- CIdata2007[Percent_10, ]
CIdata2009_10_percent <- CIdata2009[Percent_10, ]
CIdata2011_10_percent <- CIdata2011[Percent_10, ]
CIdata2013_10_percent <- CIdata2013[Percent_10, ]
CIdata2015_10_percent <- CIdata2015[Percent_10, ]

# 20% of sites
CIdata2001_20_percent <- CIdata2001[sort(c(Percent_10,Percent_20)), ] 
CIdata2003_20_percent <- CIdata2003[sort(c(Percent_10,Percent_20)), ]
CIdata2005_20_percent <- CIdata2005[sort(c(Percent_10,Percent_20)), ]
CIdata2007_20_percent <- CIdata2007[sort(c(Percent_10,Percent_20)), ]
CIdata2009_20_percent <- CIdata2009[sort(c(Percent_10,Percent_20)), ]
CIdata2011_20_percent <- CIdata2011[sort(c(Percent_10,Percent_20)), ]
CIdata2013_20_percent <- CIdata2013[sort(c(Percent_10,Percent_20)), ]
CIdata2015_20_percent <- CIdata2015[sort(c(Percent_10,Percent_20)), ]

# 30% of sites
CIdata2001_30_percent <- CIdata2001[sort(c(Percent_10,Percent_20,Percent_30)), ] 
CIdata2003_30_percent <- CIdata2003[sort(c(Percent_10,Percent_20,Percent_30)), ]
CIdata2005_30_percent <- CIdata2005[sort(c(Percent_10,Percent_20,Percent_30)), ]
CIdata2007_30_percent <- CIdata2007[sort(c(Percent_10,Percent_20,Percent_30)), ]
CIdata2009_30_percent <- CIdata2009[sort(c(Percent_10,Percent_20,Percent_30)), ]
CIdata2011_30_percent <- CIdata2011[sort(c(Percent_10,Percent_20,Percent_30)), ]
CIdata2013_30_percent <- CIdata2013[sort(c(Percent_10,Percent_20,Percent_30)), ]
CIdata2015_30_percent <- CIdata2015[sort(c(Percent_10,Percent_20,Percent_30)), ]

# 40% of sites
CIdata2001_40_percent <- CIdata2001[sort(c(Percent_10,Percent_20,Percent_30,Percent_40)), ] 
CIdata2003_40_percent <- CIdata2003[sort(c(Percent_10,Percent_20,Percent_30,Percent_40)), ]
CIdata2005_40_percent <- CIdata2005[sort(c(Percent_10,Percent_20,Percent_30,Percent_40)), ]
CIdata2007_40_percent <- CIdata2007[sort(c(Percent_10,Percent_20,Percent_30,Percent_40)), ]
CIdata2009_40_percent <- CIdata2009[sort(c(Percent_10,Percent_20,Percent_30,Percent_40)), ]
CIdata2011_40_percent <- CIdata2011[sort(c(Percent_10,Percent_20,Percent_30,Percent_40)), ]
CIdata2013_40_percent <- CIdata2013[sort(c(Percent_10,Percent_20,Percent_30,Percent_40)), ]
CIdata2015_40_percent <- CIdata2015[sort(c(Percent_10,Percent_20,Percent_30,Percent_40)), ]

# 50% of sites
CIdata2001_50_percent <- CIdata2001[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50)), ] 
CIdata2003_50_percent <- CIdata2003[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50)), ]
CIdata2005_50_percent <- CIdata2005[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50)), ]
CIdata2007_50_percent <- CIdata2007[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50)), ]
CIdata2009_50_percent <- CIdata2009[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50)), ]
CIdata2011_50_percent <- CIdata2011[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50)), ]
CIdata2013_50_percent <- CIdata2013[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50)), ]
CIdata2015_50_percent <- CIdata2015[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50)), ]

# 60% of sites
CIdata2001_60_percent <- CIdata2001[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60)), ] 
CIdata2003_60_percent <- CIdata2003[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60)), ]
CIdata2005_60_percent <- CIdata2005[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60)), ]
CIdata2007_60_percent <- CIdata2007[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60)), ]
CIdata2009_60_percent <- CIdata2009[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60)), ]
CIdata2011_60_percent <- CIdata2011[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60)), ]
CIdata2013_60_percent <- CIdata2013[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60)), ]
CIdata2015_60_percent <- CIdata2015[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60)), ]

# 70% of sites
CIdata2001_70_percent <- CIdata2001[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70)), ] 
CIdata2003_70_percent <- CIdata2003[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70)), ]
CIdata2005_70_percent <- CIdata2005[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70)), ]
CIdata2007_70_percent <- CIdata2007[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70)), ]
CIdata2009_70_percent <- CIdata2009[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70)), ]
CIdata2011_70_percent <- CIdata2011[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70)), ]
CIdata2013_70_percent <- CIdata2013[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70)), ]
CIdata2015_70_percent <- CIdata2015[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70)), ]

# 80% of sites
CIdata2001_80_percent <- CIdata2001[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80)), ] 
CIdata2003_80_percent <- CIdata2003[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80)), ]
CIdata2005_80_percent <- CIdata2005[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80)), ]
CIdata2007_80_percent <- CIdata2007[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80)), ]
CIdata2009_80_percent <- CIdata2009[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80)), ]
CIdata2011_80_percent <- CIdata2011[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80)), ]
CIdata2013_80_percent <- CIdata2013[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80)), ]
CIdata2015_80_percent <- CIdata2015[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80)), ]

# 90% of sites
CIdata2001_90_percent <- CIdata2001[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80,Percent_90)), ] 
CIdata2003_90_percent <- CIdata2003[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80,Percent_90)), ]
CIdata2005_90_percent <- CIdata2005[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80,Percent_90)), ]
CIdata2007_90_percent <- CIdata2007[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80,Percent_90)), ]
CIdata2009_90_percent <- CIdata2009[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80,Percent_90)), ]
CIdata2011_90_percent <- CIdata2011[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80,Percent_90)), ]
CIdata2013_90_percent <- CIdata2013[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80,Percent_90)), ]
CIdata2015_90_percent <- CIdata2015[sort(c(Percent_10,Percent_20,Percent_30,Percent_40,Percent_50,Percent_60,Percent_70,Percent_80,Percent_90)), ]

# 100% of sites
CIdata2001_100_percent <- CIdata2001
CIdata2003_100_percent <- CIdata2003
CIdata2005_100_percent <- CIdata2005
CIdata2007_100_percent <- CIdata2007
CIdata2009_100_percent <- CIdata2009
CIdata2011_100_percent <- CIdata2011
CIdata2013_100_percent <- CIdata2013
CIdata2015_100_percent <- CIdata2015

## Merge datasets by proportion of sites kept
# 10% of sites 
CIdata2001_2003_10_percent <- rbind(CIdata2001_10_percent, CIdata2003_10_percent)
CIdata2001_2003_2005_10_percent <- rbind(CIdata2001_2003_10_percent, CIdata2005_10_percent) 
CIdata2001_2003_2005_2007_10_percent <- rbind(CIdata2001_2003_2005_10_percent, CIdata2007_10_percent)
CIdata2001_2003_2005_2007_2009_10_percent <- rbind(CIdata2001_2003_2005_2007_10_percent, CIdata2009_10_percent)
CIdata2001_2003_2005_2007_2009_2011_10_percent <- rbind(CIdata2001_2003_2005_2007_2009_10_percent, CIdata2011_10_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_10_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_10_percent, CIdata2013_10_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_2015_10_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_2013_10_percent, CIdata2015_10_percent)

# 20% of sites 
CIdata2001_2003_20_percent <- rbind(CIdata2001_20_percent, CIdata2003_20_percent)
CIdata2001_2003_2005_20_percent <- rbind(CIdata2001_2003_20_percent, CIdata2005_20_percent) 
CIdata2001_2003_2005_2007_20_percent <- rbind(CIdata2001_2003_2005_20_percent, CIdata2007_20_percent)
CIdata2001_2003_2005_2007_2009_20_percent <- rbind(CIdata2001_2003_2005_2007_20_percent, CIdata2009_20_percent)
CIdata2001_2003_2005_2007_2009_2011_20_percent <- rbind(CIdata2001_2003_2005_2007_2009_10_percent, CIdata2011_10_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_20_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_20_percent, CIdata2013_20_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_2015_20_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_2013_20_percent, CIdata2015_20_percent)

# 30% of sites 
CIdata2001_2003_30_percent <- rbind(CIdata2001_30_percent, CIdata2003_30_percent)
CIdata2001_2003_2005_30_percent <- rbind(CIdata2001_2003_30_percent, CIdata2005_30_percent) 
CIdata2001_2003_2005_2007_30_percent <- rbind(CIdata2001_2003_2005_30_percent, CIdata2007_30_percent)
CIdata2001_2003_2005_2007_2009_30_percent <- rbind(CIdata2001_2003_2005_2007_30_percent, CIdata2009_30_percent)
CIdata2001_2003_2005_2007_2009_2011_30_percent <- rbind(CIdata2001_2003_2005_2007_2009_30_percent, CIdata2011_30_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_30_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_30_percent, CIdata2013_30_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_2015_30_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_2013_30_percent, CIdata2015_30_percent)

# 40% of sites 
CIdata2001_2003_40_percent <- rbind(CIdata2001_40_percent, CIdata2003_40_percent)
CIdata2001_2003_2005_40_percent <- rbind(CIdata2001_2003_40_percent, CIdata2005_40_percent) 
CIdata2001_2003_2005_2007_40_percent <- rbind(CIdata2001_2003_2005_40_percent, CIdata2007_40_percent)
CIdata2001_2003_2005_2007_2009_40_percent <- rbind(CIdata2001_2003_2005_2007_40_percent, CIdata2009_40_percent)
CIdata2001_2003_2005_2007_2009_2011_40_percent <- rbind(CIdata2001_2003_2005_2007_2009_40_percent, CIdata2011_40_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_40_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_40_percent, CIdata2013_40_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_2015_40_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_2013_40_percent, CIdata2015_40_percent)

# 50% of sites 
CIdata2001_2003_50_percent <- rbind(CIdata2001_50_percent, CIdata2003_50_percent)
CIdata2001_2003_2005_50_percent <- rbind(CIdata2001_2003_50_percent, CIdata2005_50_percent) 
CIdata2001_2003_2005_2007_50_percent <- rbind(CIdata2001_2003_2005_50_percent, CIdata2007_50_percent)
CIdata2001_2003_2005_2007_2009_50_percent <- rbind(CIdata2001_2003_2005_2007_50_percent, CIdata2009_50_percent)
CIdata2001_2003_2005_2007_2009_2011_50_percent <- rbind(CIdata2001_2003_2005_2007_2009_50_percent, CIdata2011_50_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_50_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_50_percent, CIdata2013_50_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_2015_50_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_2013_50_percent, CIdata2015_50_percent)

# 60% of sites 
CIdata2001_2003_60_percent <- rbind(CIdata2001_60_percent, CIdata2003_60_percent)
CIdata2001_2003_2005_60_percent <- rbind(CIdata2001_2003_60_percent, CIdata2005_60_percent) 
CIdata2001_2003_2005_2007_60_percent <- rbind(CIdata2001_2003_2005_60_percent, CIdata2007_60_percent)
CIdata2001_2003_2005_2007_2009_60_percent <- rbind(CIdata2001_2003_2005_2007_60_percent, CIdata2009_60_percent)
CIdata2001_2003_2005_2007_2009_2011_60_percent <- rbind(CIdata2001_2003_2005_2007_2009_60_percent, CIdata2011_60_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_60_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_60_percent, CIdata2013_60_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_2015_60_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_2013_60_percent, CIdata2015_60_percent)

# 70% of sites 
CIdata2001_2003_70_percent <- rbind(CIdata2001_70_percent, CIdata2003_70_percent)
CIdata2001_2003_2005_70_percent <- rbind(CIdata2001_2003_70_percent, CIdata2005_70_percent) 
CIdata2001_2003_2005_2007_70_percent <- rbind(CIdata2001_2003_2005_70_percent, CIdata2007_70_percent)
CIdata2001_2003_2005_2007_2009_70_percent <- rbind(CIdata2001_2003_2005_2007_70_percent, CIdata2009_70_percent)
CIdata2001_2003_2005_2007_2009_2011_70_percent <- rbind(CIdata2001_2003_2005_2007_2009_70_percent, CIdata2011_70_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_70_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_70_percent, CIdata2013_70_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_2015_70_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_2013_70_percent, CIdata2015_70_percent)

# 80% of sites 
CIdata2001_2003_80_percent <- rbind(CIdata2001_80_percent, CIdata2003_80_percent)
CIdata2001_2003_2005_80_percent <- rbind(CIdata2001_2003_80_percent, CIdata2005_80_percent) 
CIdata2001_2003_2005_2007_80_percent <- rbind(CIdata2001_2003_2005_80_percent, CIdata2007_80_percent)
CIdata2001_2003_2005_2007_2009_80_percent <- rbind(CIdata2001_2003_2005_2007_80_percent, CIdata2009_80_percent)
CIdata2001_2003_2005_2007_2009_2011_80_percent <- rbind(CIdata2001_2003_2005_2007_2009_80_percent, CIdata2011_80_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_80_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_80_percent, CIdata2013_80_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_2015_80_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_2013_80_percent, CIdata2015_80_percent)

# 90% of sites 
CIdata2001_2003_90_percent <- rbind(CIdata2001_90_percent, CIdata2003_90_percent)
CIdata2001_2003_2005_90_percent <- rbind(CIdata2001_2003_90_percent, CIdata2005_90_percent) 
CIdata2001_2003_2005_2007_90_percent <- rbind(CIdata2001_2003_2005_90_percent, CIdata2007_90_percent)
CIdata2001_2003_2005_2007_2009_90_percent <- rbind(CIdata2001_2003_2005_2007_90_percent, CIdata2009_90_percent)
CIdata2001_2003_2005_2007_2009_2011_90_percent <- rbind(CIdata2001_2003_2005_2007_2009_90_percent, CIdata2011_90_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_90_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_90_percent, CIdata2013_90_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_2015_90_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_2013_90_percent, CIdata2015_90_percent)

# 100% of sites 
CIdata2001_2003_100_percent <- rbind(CIdata2001_100_percent, CIdata2003_100_percent)
CIdata2001_2003_2005_100_percent <- rbind(CIdata2001_2003_100_percent, CIdata2005_100_percent) 
CIdata2001_2003_2005_2007_100_percent <- rbind(CIdata2001_2003_2005_100_percent, CIdata2007_100_percent)
CIdata2001_2003_2005_2007_2009_100_percent <- rbind(CIdata2001_2003_2005_2007_100_percent, CIdata2009_100_percent)
CIdata2001_2003_2005_2007_2009_2011_100_percent <- rbind(CIdata2001_2003_2005_2007_2009_100_percent, CIdata2011_100_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_100_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_100_percent, CIdata2013_100_percent)
CIdata2001_2003_2005_2007_2009_2011_2013_2015_100_percent <- rbind(CIdata2001_2003_2005_2007_2009_2011_2013_100_percent, CIdata2015_100_percent)

# Add all dataframes to a list 
all_data_frames <- list()
all_data_files <- vector()

percentage_of_total_survey_budget <- c(seq(10, 100, 10)) # fractions of survey budget
cumulative_surveyed_years <- c("2001_2003", "2001_2003_2005", "2001_2003_2005_2007",
                                       "2001_2003_2005_2007_2009","2001_2003_2005_2007_2009_2011", 
                                       "2001_2003_2005_2007_2009_2011_2013")
# add all dataframes together 
for (i in cumulative_surveyed_years) {
  for (j in percentage_of_total_survey_budget){
    add <- paste("CIdata",i,"_",j,"_percent", sep="")
    all_data_files <- c(all_data_files, add)
  }
}
all_data_files # check 

for (i in 1:length(all_data_files)) {
  all_data_frames[[i]] <- get(all_data_files[i])
}



### PLOT COST 

