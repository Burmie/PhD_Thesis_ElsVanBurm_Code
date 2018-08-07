## Read in CIdata, remove rows without sampling date
CIdata <- read.table("../Data/CINP_Data/CIdata_edited.csv", header=TRUE, sep=",")

## Load required packages
library(raster)  
library(maptools)  
library(rgdal)
library(sp) 
library(rgeos)
library(spatstat)
library(foreign)

## Create modelraster
modelraster <- raster(nrow=180,ncol=210)
extent(modelraster) <- c(558000,579000,8831000,8849000)
modelraster[] <- NA
summary(modelraster)

## ADD SUPERCOLONY PRESENCE/ABSENCE DATA TO CIDATA 
# 2013 SC DATA
# read in supercolony polygons, rasterize, extract SC information from raster for each survey point   
setwd("../Data/CINP_Data")
SC_2013 <- readOGR(dsn=getwd(), layer="YCA_ESTIMATE_2013")  
plot(SC_2013) # check 
SC_2013_raster <- rasterize(SC_2013, modelraster, field=1, background=0) # rasterize polygon with pixels containing SC get a value of 1, pixels without SC get a 0
plot(SC_2013_raster) # check
SC_2013_points <- NULL
for (i in 1:nrow(CIdata)) {
  cat(round(100*i/nrow(CIdata),digits=2),"%\n",sep="")
  x = CIdata[i,"X_MARK"]
  y = CIdata[i,"Y_MARK"]
  SC_2013_points = c(SC_2013_points,unname(extract(SC_2013_raster,matrix(data = c(x,y),ncol=2))))
}
SC_2013_points
CIdata$SC_2013 <- SC_2013_points # this adds a column of 2013 supercolonies to the data.frame but we actually want to add the 2013 supercolony data into the data.frame
# Subset the CIdata dataframe by unique waypoint values (survey points) to add 2013 supercolony data 
CIdata_unique_WPT_2013 <- subset(CIdata, !duplicated(WPT)) # create a dataframe that keeps the rows for unique WPT values 
CIdata_unique_WPT_2013[ , "Year"] <- 2013
CIdata_unique_WPT_2013[ , "Date"] <- NA
CIdata_unique_WPT_2013[ , "YCA_Total"] <- NA
CIdata_unique_WPT_2013[ , "YCA_P_A"] <- NA
CIdata_unique_WPT_2013[ , "RC_Burrows"] <- NA
CIdata_unique_WPT_2013[ , "Burr_width"] <- NA
CIdata_unique_WPT_2013[ , "Anted_burr"] <- NA
head(CIdata_unique_WPT_2013) # check

# ADD 2015 SC DATA TO CIDATA
SC_2015 <- readOGR(dsn=getwd(), layer="YCA_SC_2015")
plot(SC_2015, col=2, border=NA)
plot(coast, add=TRUE)
SC_2015_raster <- rasterize(SC_2015, modelraster, field=1, background=0)
plot(SC_2015_raster)
SC_2015_points <- NULL
for (i in 1:nrow(CIdata)) {
  cat(round(100*i/nrow(CIdata),digits=2),"%\n",sep="")
  x = CIdata[i,"X_MARK"]
  y = CIdata[i,"Y_MARK"]
  SC_2015_points = c(SC_2015_points,unname(extract(SC_2015_raster,matrix(data = c(x,y),ncol=2))))
}
SC_2015_points
CIdata$SC_2015 <- SC_2015_points 
# Subset the CIdata dataframe by unique waypoint values to add 2015 supercolony data 
CIdata_unique_WPT_2015 <- subset(CIdata, !duplicated(WPT)) # create a dataframe that keeps the rows for unique WPT values 
CIdata_unique_WPT_2015$SC_2013 <- NULL # remove the SC 2013 column 
CIdata_unique_WPT_2015[ , "Year"] <- 2015
CIdata_unique_WPT_2015[ , "Date"] <- NA
CIdata_unique_WPT_2015[ , "YCA_Total"] <- NA
CIdata_unique_WPT_2015[ , "YCA_P_A"] <- NA
CIdata_unique_WPT_2015[ , "RC_Burrows"] <- NA
CIdata_unique_WPT_2015[ , "Burr_width"] <- NA
CIdata_unique_WPT_2015[ , "Anted_burr"] <- NA
head(CIdata_unique_WPT_2015) # check

# Create a new data.frame to add the 2013 and 2015 SC data with NAs for SC data of other years  
CIdata_2013_2015 <- matrix(NA, nrow=(nrow(CIdata)+2*(nrow(CIdata_unique_WPT_2013))), ncol=ncol(CIdata))
CIdata_2013_2015 <- as.data.frame(CIdata_2013_2015)
CIdata_2013_2015[1:nrow(CIdata), 1:ncol(CIdata)] <- CIdata # fill the new data.frame with original CIdata
colnames(CIdata_2013_2015) <- colnames(CIdata)
head(CIdata_2013_2015) # check 
CIdata_2013_2015$SC_2013 <- NULL
CIdata_2013_2015$SC_2015 <- NULL
CIdata_2013_2015$p_a_SC <- NA 
CIdata_2013_2015[(nrow(CIdata)+1):(nrow(CIdata)+(nrow(CIdata_unique_WPT_2013))), ] <- CIdata_unique_WPT_2013 # add 2013 data to dataframe 
CIdata_2013_2015[(nrow(CIdata)+(nrow(CIdata_unique_WPT_2013))+1):(nrow(CIdata_2013_2015)), ] <- CIdata_unique_WPT_2015 # add 2015 data to dataframe
head(CIdata_2013_2015) # check
tail(CIdata_2013_2015)

# Add presence/absence of supercolonies for all other years than 2013 and 2015
presabs_sc <- NULL # assign vector to store p/a SC 
head(CIdata_2013_2015) # check if there is a column for p/a SC 
for (i in 1: nrow(CIdata)) { # only loop through nrows of CIdata because we already got p/a SC values for 2013 and 2015
  if (CIdata_2013_2015[i,"YCA_Total"] >= 37) {
    presabs_sc = c(presabs_sc,1)
  } else {
    presabs_sc = c(presabs_sc,0)
  }
}
CIdata_2013_2015$p_a_SC[1:nrow(CIdata)] <- presabs_sc
head(CIdata_2013_2015) # check if p/a of supercolonies is added

## READ IN RELEVANT ENVIRONMENTAL VARIABLES (vegetation and elevation) to model probability of occurrence of supercolonies.
# Values of vegetation and elevation will be extracted for each survey point.
# Vegetation is a categorical variable, elevation is inserted as a digital elevation model.
# (Time since baiting is no longer included - probability of occurrence was negatively correlated with TSB because 
# baiting mainly happened at sites that already had high probability of occurrence - for script: ../Data/Files_created_by_EVB)

# VARIABLE 1: VEGETATION CATEGORY
setwd("../CIGIS_2012/environment")
vegetation <- readOGR(dsn=getwd(), "vegetation") # PolygonLayer
plot(vegetation) # have a look at the layer
vegetation_raster <- rasterize(vegetation,modelraster,field='MAP_SYMB',fun='max') # create a RasterLayer object (many RasterLayers together create a RasterStack)
vegetation_raster
plot(vegetation_raster)

# VARIABLE 2: ELEVATION (Digital Elevation Model)
setwd("../height_data/2011_LiDAR_survey/digital_elevation_model")
DEM <- raster("DEM_CI_2011.tif") # read in RasterLayer
DEM
plot(DEM)
resampled_DEM <- resample(DEM, vegetation_raster, method="bilinear") # resample DEM so it has same resolution as vegetation_raster
resampled_DEM

# write rasters
setwd("../../../../../Ch03_Cost_effective_survey_methods_for_invasive_species")
writeRaster(vegetation_raster,"vegetation_raster") 
writeRaster(vegetation_raster,"vegetation_raster.asc",format="ascii")
writeRaster(resampled_DEM,"DEM")
writeRaster(resampled_DEM,"DEM.asc",format="ascii")

# extract predictor values at survey points and add to CIdata_2013_2015 (dataframe with all data)
demdata <- NULL
vegdata <- NULL
for (i in 1:nrow(CIdata_2013_2015)) {
  cat(round(100*i/nrow(CIdata_2013_2015),digits=2),"%\n",sep="")
  x = CIdata_2013_2015[i,"X_MARK"]
  y = CIdata_2013_2015[i,"Y_MARK"]
  demdata = c(demdata,unname(extract(DEM,matrix(data = c(x,y),ncol=2))))
  vegdata = c(vegdata,unname(extract(vegetation_raster,matrix(data = c(x,y),ncol=2))))
}
CIdata_2013_2015$Elevation <- demdata
CIdata_2013_2015$Vegetation <- vegdata
head(CIdata_2013_2015) # check if added 
tail(CIdata_2013_2015)

## READ IN VARIABLES TO DETERMINE SURVEY COST (distance to roads, pinnaclefields and slope).
# Values for each of these variables will also be extracted for each survey point.

# DISTANCE TO ROADS
# read in road vector file (shapefile)
setwd("../Data/Files_created_by_EVB/Rcode_Darren_separateyears/XmasIsland/6_GIS layers")
coast <- readOGR(dsn=getwd(), layer="coast_poly")
plot(coast)
setwd("../../../../CIGIS_2012/infra_topo/topo30k")
roads <- readOGR(dsn=getwd(),layer="roads_30k")
plot(roads, add=TRUE)
lines(roads)
# rasterize roads vector
roads_raster <- rasterize(roads, modelraster, field=1) # all pixels crossed by a road should have "1"
summary(roads_raster) # check whether pixels crossed by a road have "1"
plot(roads_raster, add=TRUE)
# calculate distance to roads
roaddist.r <- distance(roads_raster)
class(roaddist.r)
# check:
plot(roaddist.r)
# plot with Christmas Island shape - make sure I read in the correct vegetation type raster later than 
values(vegetation_raster)[values(vegetation_raster) == 0 ] <- 1 
roaddist_to_plot <- vegetation_raster * roaddist.r
plot(roaddist_to_plot)

# save the rasterfile (geo-tiff file or an ESRI ascii file for loading into QGIS)
setwd("../../../../Ch03_Cost_effective_survey_methods_for_invasive_species")
writeRaster(roaddist.r, "DistFromRoad.tiff", "GTiff")
# extract the distance-from-road information from roaddist.r for each survey point
dist_from_road <- NULL
for (i in 1:nrow(CIdata_2013_2015)) {
  cat(round(100*i/nrow(CIdata_2013_2015),digits=2),"%\n",sep="")
  x = CIdata_2013_2015[i,"X_MARK"]
  y = CIdata_2013_2015[i,"Y_MARK"]
  dist_from_road = c(dist_from_road,unname(extract(roaddist.r,matrix(data = c(x,y),ncol=2))))
}
dist_from_road
CIdata_2013_2015$DistanceFromRoad <- dist_from_road
head(CIdata_2013_2015) # check if added 
tail(CIdata_2013_2015)

## PINNACLEFIELDS
# rasterize and extract pinnacle polygons 
setwd("../Data/CIGIS_2012/environment")
pinnacles <- readOGR(dsn=getwd(), layer="pinnaclefields")
plot(pinnacles)
pinnacles2010 <- readOGR(dsn=getwd(), layer="pinnacles_2010")
plot(pinnacles2010, add=TRUE)
# combine all pinnacle polygons 
pinnacles_total <- union(pinnacles,pinnacles2010)
plot(pinnacles_total, col=17, border=NA)
plot(coast, add=TRUE)
# rasterize and extract survey points from pinnacle raster
pinnacle.r <- rasterize(pinnacles_total, modelraster, field=1, background=0) # pixels with pinnacles should have 1, other pixels 0
summary(pinnacle.r)
plot(pinnacle.r)
setwd("../../../Ch03_Cost_effective_survey_methods_for_invasive_species")
writeRaster(pinnacle.r, "Pinnacle.tiff", "GTiff") # save rasterfile
# extract value of pinnacle from pinnaclefields.r for each survey point
pinnacles_points <- NULL
for (i in 1:nrow(CIdata_2013_2015)) {
  cat(round(100*i/nrow(CIdata_2013_2015),digits=2),"%\n",sep="")
  x = CIdata_2013_2015[i,"X_MARK"]
  y = CIdata_2013_2015[i,"Y_MARK"]
  pinnacles_points = c(pinnacles_points,unname(extract(pinnacle.r,matrix(data = c(x,y),ncol=2))))
}
pinnacles_points
CIdata_2013_2015$Pinnacles <- pinnacles_points
head(CIdata_2013_2015) # check if added 
tail(CIdata_2013_2015)

## SLOPE
# add slope data to CIdata_2013_2015
elevation.r <- raster(x="DEM.grd") # read in elevation raster
slope.r <- terrain(elevation.r, opt="slope", unit="radians", neighbours="8") # calculate slope and create slope raster
writeRaster(slope.r, "Slope.tiff", "GTiff")
slope_extract <- NULL
for (i in 1:nrow(CIdata_2013_2015)) {
  cat(round(100*i/nrow(CIdata_2013_2015),digits=2),"%\n",sep="")
  x = CIdata_2013_2015[i,"X_MARK"]
  y = CIdata_2013_2015[i,"Y_MARK"]
  slope_extract = c(slope_extract,unname(extract(slope.r,matrix(data = c(x,y),ncol=2))))
}
slope_extract
CIdata_2013_2015$Slope <- slope_extract
head(CIdata_2013_2015) # check
tail(CIdata_2013_2015)

## CALCULATE SURVEY COST PER SITE (WHICH IS TRAVEL COST PLUS SURVEY COST)
# Time it takes to survey different terrains I obtained from Dion Maple from CINP (see email)
# Regression model with survey time at a site is a function of veg, slope, pinnaclefield
# Travel to a survey site is a function of distancetoroad
# Think of 20 datapoints per predictor (rough estimate for regression model - as suggested by GGA)
# Vegetation categories
# 1: Tall closed forest, deep soil phase
# 2: Closed forest, shallow soil phase
# 3: Closed forest, screen/pinnacle phase
# 4: Open forest and vine woodland
# 5: Closed forest, freshwater seepage 
# 6: Heath, shrubland and low closed woodland
# 7: Natural and planted revegetation

pin_cat <- CIdata_2013_2015$Pinnacles
slope_cat <- vector()
veg_cat <- vector()
travel_cost <- vector()
survey_cost <- vector()
total_cost <- vector()

which(is.na(CIdata_2013_2015$Slope)) # check where NAs (rows_to_remove)
CIdata_2013_2015 <- CIdata_2013_2015[-which(is.na(CIdata_2013_2015$Slope)),] # remove rows with NA in slope column
which(is.na(CIdata_2013_2015$Slope)) # check if NAs dissappeared
for (i in 1:nrow(CIdata_2013_2015)) {
  if (CIdata_2013_2015[i, "Slope"] < 0.1) {slope_category = "low"}
    if (CIdata_2013_2015[i, "Slope"] > 0.1 && CIdata_2013_2015[i, "Slope"] < 0.2) {slope_category = "med"}
      if (CIdata_2013_2015[i, "Slope"] > 0.2) {slope_category = "high"}
        slope_cat <- c(slope_cat, slope_category)
}
CIdata_2013_2015$Slope_cat <- slope_cat
head(CIdata_2013_2015) # check

which(is.na(CIdata_2013_2015$Vegetation)) # check where NAs 
CIdata_2013_2015 <-CIdata_2013_2015[-which(is.na(CIdata_2013_2015$Vegetation)),] # remove rows with NA in vegetation column
which(is.na(CIdata_2013_2015$Vegetation)) # check if NAs dissappeared

for (i in 1:nrow(CIdata_2013_2015)){
  if (CIdata_2013_2015[i, "Vegetation"] == 1) {veg_category = "closed"}
  if (CIdata_2013_2015[i, "Vegetation"] == 2) {veg_category = "closed"}
  if (CIdata_2013_2015[i, "Vegetation"] == 3) {veg_category = "closed"}
  if (CIdata_2013_2015[i, "Vegetation"] == 4) {veg_category = "open"}
  if (CIdata_2013_2015[i, "Vegetation"] == 5) {veg_category = "closed"}
  if (CIdata_2013_2015[i, "Vegetation"] == 6) {veg_category = "closed"}
  if (CIdata_2013_2015[i, "Vegetation"] == 7) {veg_category = "closed"}
  veg_cat <- c(veg_cat, veg_category)
}    
CIdata_2013_2015$Veg_cat <- veg_cat
head(CIdata_2013_2015)

for (i in 1:nrow(CIdata_2013_2015)) {
  if (CIdata_2013_2015[i, "Slope_cat"] == "high" && CIdata_2013_2015[i, "Veg_cat"] == "closed" && CIdata_2013_2015[i, "Pinnacles"] == 1) {survey_c = 40}
  if (CIdata_2013_2015[i, "Slope_cat"] == "med" && CIdata_2013_2015[i, "Veg_cat"] == "closed" && CIdata_2013_2015[i, "Pinnacles"] == 1) {survey_c = 35}
  if (CIdata_2013_2015[i, "Slope_cat"] == "low" && CIdata_2013_2015[i, "Veg_cat"] == "closed" && CIdata_2013_2015[i, "Pinnacles"] == 1) {survey_c = 30}
  if (CIdata_2013_2015[i, "Slope_cat"] == "high" && CIdata_2013_2015[i, "Veg_cat"] == "open" && CIdata_2013_2015[i, "Pinnacles"] == 1) {survey_c = 30}
  if (CIdata_2013_2015[i, "Slope_cat"] == "med" && CIdata_2013_2015[i, "Veg_cat"] == "open" && CIdata_2013_2015[i, "Pinnacles"] == 1) {survey_c = 28}
  if (CIdata_2013_2015[i, "Slope_cat"] == "low" && CIdata_2013_2015[i, "Veg_cat"] == "open" && CIdata_2013_2015[i, "Pinnacles"] == 1) {survey_c = 25}
  if (CIdata_2013_2015[i, "Slope_cat"] == "high" && CIdata_2013_2015[i, "Veg_cat"] == "closed" && CIdata_2013_2015[i, "Pinnacles"] == 0) {survey_c = 20}
  if (CIdata_2013_2015[i, "Slope_cat"] == "med" && CIdata_2013_2015[i, "Veg_cat"] == "closed" && CIdata_2013_2015[i, "Pinnacles"] == 0) {survey_c = 19}
  if (CIdata_2013_2015[i, "Slope_cat"] == "low" && CIdata_2013_2015[i, "Veg_cat"] == "closed" && CIdata_2013_2015[i, "Pinnacles"] == 0) {survey_c = 18}
  if (CIdata_2013_2015[i, "Slope_cat"] == "high" && CIdata_2013_2015[i, "Veg_cat"] == "open" && CIdata_2013_2015[i, "Pinnacles"] == 0) {survey_c = 18}
  if (CIdata_2013_2015[i, "Slope_cat"] == "med" && CIdata_2013_2015[i, "Veg_cat"] == "open" && CIdata_2013_2015[i, "Pinnacles"] == 0) {survey_c = 17}
  if (CIdata_2013_2015[i, "Slope_cat"] == "low" && CIdata_2013_2015[i, "Veg_cat"] == "open" && CIdata_2013_2015[i, "Pinnacles"] == 0) {survey_c = 15}
  survey_cost <- c(survey_cost, survey_c)
}
CIdata_2013_2015$survey_cost <- survey_cost 
head(CIdata_2013_2015)

# to plot open and closed vegetation 
values(vegetation_raster)[values(vegetation_raster) == 1] <- 0
values(vegetation_raster)[values(vegetation_raster) == 2] <- 0
values(vegetation_raster)[values(vegetation_raster) == 3] <- 0
values(vegetation_raster)[values(vegetation_raster) == 4] <- 1
values(vegetation_raster)[values(vegetation_raster) == 5] <- 0
values(vegetation_raster)[values(vegetation_raster) == 6] <- 0
values(vegetation_raster)[values(vegetation_raster) == 7] <- 0
plot(vegetation_raster)
plot(coast, add=TRUE)




for (i in (1:nrow(CIdata_2013_2015))) {
  travel_c <- (CIdata_2013_2015[i, "DistanceFromRoad"])/100 # travel cost is calculated in minutes, distance from road is in metres -> travel time is set to 100m/min 
  travel_cost <- c(travel_cost, travel_c) 
}
CIdata_2013_2015$travel_cost <- travel_cost
head(CIdata_2013_2015)

for (i in (1:nrow(CIdata_2013_2015))) {
  total_c <- survey_cost[i] + travel_cost[i]
  total_cost <- c(total_cost, total_c) 
}

CIdata_2013_2015$COST_Travel_plus_Survey <- total_cost
head(CIdata_2013_2015)
write.table(CIdata_2013_2015,file="CIdata_all_years_with_Predictors_and_Travel_and_Survey_cost.txt",sep="\t",col.names=T,row.names=F)

# Total annual rainfall (in mm) for the sampled period (obtained from the BOM website) 
annual_rainfall_2000 <- 2443
annual_rainfall_2001 <- 2525
annual_rainfall_2002 <- 1609
annual_rainfall_2003 <- 1526
annual_rainfall_2004 <- 2011
annual_rainfall_2005 <- 1712
annual_rainfall_2006 <- 1246
annual_rainfall_2007 <- 1624
annual_rainfall_2008 <- 1906
annual_rainfall_2009 <- 1872
annual_rainfall_2010 <- 3655
annual_rainfall_2011 <- 2387
annual_rainfall_2012 <- 2242
annual_rainfall_2013 <- 3165
annual_rainfall_2014 <- 1922
annual_rainfall_2015 <- 1722
# Add rainfall columns (rainfall in same year and rainfall in previous year)
CIdata_2013_2015$Rainfall <- NULL
CIdata_2013_2015$Rainfall_previous_year <- NULL
# Add rainfall for each sampled year
rows_2001 <- as.vector(which(CIdata_2013_2015$Year == 2001))
for (i in rows_2001){
  CIdata_2013_2015$Rainfall[i] <- annual_rainfall_2001
  print(i)
}
rows_2003 <- as.vector(which(CIdata_2013_2015$Year == 2003))
for (i in rows_2003){
  CIdata_2013_2015$Rainfall[i] <- annual_rainfall_2003
  print(i)
}
rows_2005 <- as.vector(which(CIdata_2013_2015$Year == 2005))
for (i in rows_2005){
  CIdata_2013_2015$Rainfall[i] <- annual_rainfall_2005
  print(i)
}
rows_2007 <- as.vector(which(CIdata_2013_2015$Year == 2007))
for (i in rows_2007){
  CIdata_2013_2015$Rainfall[i] <- annual_rainfall_2007
  print(i)
}
rows_2009 <- as.vector(which(CIdata_2013_2015$Year == 2009))
for (i in rows_2009){
  CIdata_2013_2015$Rainfall[i] <- annual_rainfall_2009
  print(i)
}
rows_2011 <- as.vector(which(CIdata_2013_2015$Year == 2011))
for (i in rows_2011){
  CIdata_2013_2015$Rainfall[i] <- annual_rainfall_2011
  print(i)
}
rows_2013 <- as.vector(which(CIdata_2013_2015$Year == 2013))
for (i in rows_2013){
  CIdata_2013_2015$Rainfall[i] <- annual_rainfall_2013
  print(i)
}
rows_2015 <- as.vector(which(CIdata_2013_2015$Year == 2015))
for (i in rows_2015){
  CIdata_2013_2015$Rainfall[i] <- annual_rainfall_2015
  print(i)
}
# Add rainfall previous year 
for (i in rows_2001){
  CIdata_2013_2015$Rainfall_previous_year[i] <- annual_rainfall_2000
  print(i)
}
for (i in rows_2003){
  CIdata_2013_2015$Rainfall_previous_year[i] <- annual_rainfall_2002
  print(i)
}
for (i in rows_2005){
  CIdata_2013_2015$Rainfall_previous_year[i] <- annual_rainfall_2004
  print(i)
}
for (i in rows_2007){
  CIdata_2013_2015$Rainfall_previous_year[i] <- annual_rainfall_2006
  print(i)
}
for (i in rows_2009){
  CIdata_2013_2015$Rainfall_previous_year[i] <- annual_rainfall_2008
  print(i)
}
for (i in rows_2011){
  CIdata_2013_2015$Rainfall_previous_year[i] <- annual_rainfall_2010
  print(i)
}
for (i in rows_2013){
  CIdata_2013_2015$Rainfall_previous_year[i] <- annual_rainfall_2012
  print(i)
}
for (i in rows_2015){
  CIdata_2013_2015$Rainfall_previous_year[i] <- annual_rainfall_2014
  print(i)
}

# Add rainfall column in meters and a standardize
CIdata_2013_2015$rainfall_meters <- CIdata_2013_2015$Rainfall/1000
CIdata_2013_2015$rainfall_prev_year_metres <- CIdata_2013_2015$Rainfall_previous_year/1000
CIdata_2013_2015$sd_rainfall_meters <- scale(CIdata_2013_2015$rainfall_meters)
CIdata_2013_2015$sd_rainfall_prev_year_meters <- scale(CIdata_2013_2015$rainfall_prev_year_metres)

write.table(CIdata_2013_2015, file="CIdata_all_years_with_Predictors_and_Travel_and_Survey_cost_RAINFALL.txt",sep="\t",col.names=T,row.names=F)

# Add geology values 
# Read in CIdata to add geology
CIdata <- read.table("CIdata_all_years_with_Predictors_and_Travel_and_Survey_cost_RAINFALL.txt", header=TRUE)
# Navigate to geology folder 
setwd("../Data/CIGIS_2012/environment")
geology <- readOGR(dsn=getwd(), layer="geology")
plot(geology)
levels(geology@data$LITH_DESC) # check different levels
#[1] "Limestone"                                                                    
#[2] "Limestone - undifferentiated"                                                 
#[3] "Limestone/Talus"                                                              
#[4] "Pellet limestone, contains pellets and pebbles of phosphate rock"             
#[5] "Phosphate rock - massive, boulder, and pebble deposits"                       
#[6] "Phosphatised \"volcanics\""                                                   
#[7] "Pinnacles of limestone with variable amounts of unconsolidated material (Czp)"
#[8] "Unconsolidated material - mostly phosphatic soil containing >20% P2O5"        
#[9] "Volcanics - mostly basalt, some tuff, scoria - undifferentiated"     
# rasterize and extract survey points from pinnacle raster
geology.r <- rasterize(geology, modelraster, field='MAP_SYMB',fun='max') 
summary(geology.r)
plot(geology.r)
setwd("../../../Ch03_Cost_effective_survey_methods_for_invasive_species")
writeRaster(geology.r, "Geology.tiff", "GTiff") # save rasterfile
# extract value of pinnacle from pinnaclefields.r for each survey point
geology_points <- NULL
for (i in 1:nrow(CIdata)) {
  cat(round(100*i/nrow(CIdata),digits=2),"%\n",sep="")
  x = CIdata[i,"X_MARK"]
  y = CIdata[i,"Y_MARK"]
  geology_points = c(geology_points,unname(extract(geology.r,matrix(data = c(x,y),ncol=2))))
}
geology_points
CIdata$geology <- geology_points
head(CIdata) # check if added 
tail(CIdata)

write.table(CIdata, file="CIdata_all_years_with_Predictors_and_Travel_and_Survey_cost_RAINFALL_GEOLOGY.txt",sep="\t",col.names=T,row.names=F)

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




####### Archive: Extra - check additional vegetation map - from GeoSCience Australia ###########
setwd("D:\\Users\\evanburm\\Dropbox\\Christmas Island\\Data\\GeoScience_Australia_Veg_and_Clearing_map\\CI_vegetation_and_clearing_map\\CI_vegetation_and_clearing_map")
veg_polygon <- readOGR(dsn=getwd(), layer="CI_vegetation_and_clearing_map")
unique(veg_polygon$Level1) # check whether these levels are useful to distinguish between open/closed vegetation  -> not really
unique(veg_polygon$Level2)
