## PREDICT TO EACH SURVEY SITE ON THE ISLAND 
# Load required packages 
library(dismo)
library(SDMTools)
library(raster)
library(sp)
library(lme4)
library(arm)
library(rgdal)
library(foreign)
library(maptools)


#################################################### Predict to new data (without raster) #####################################
years <- c("2001","2003","2005","2007","2009","2011","2013","2015") # create vector containing years for which we have data
multi_year_datasets_excluding_one_year <- vector() # declare vector to store datasets with the test dataset left out (training datasets = all years without one)
# Standardize continuous variables for prediction - we are predicting to the same area 
CIdata2001$sd_elev <- scale(CIdata2001$Elevation)
CIdata2003$sd_elev <- scale(CIdata2003$Elevation)
CIdata2005$sd_elev <- scale(CIdata2005$Elevation)
CIdata2007$sd_elev <- scale(CIdata2007$Elevation)
CIdata2009$sd_elev <- scale(CIdata2009$Elevation)
CIdata2011$sd_elev <- scale(CIdata2011$Elevation)
CIdata2013$sd_elev <- scale(CIdata2013$Elevation)
CIdata2015$sd_elev <- scale(CIdata2015$Elevation)

CIdata2001$sd_DFR <- scale(CIdata2001$DistanceFromRoad)
CIdata2003$sd_DFR <- scale(CIdata2003$DistanceFromRoad)
CIdata2005$sd_DFR <- scale(CIdata2005$DistanceFromRoad)
CIdata2007$sd_DFR <- scale(CIdata2007$DistanceFromRoad)
CIdata2009$sd_DFR <- scale(CIdata2009$DistanceFromRoad)
CIdata2011$sd_DFR <- scale(CIdata2011$DistanceFromRoad)
CIdata2013$sd_DFR <- scale(CIdata2013$DistanceFromRoad)
CIdata2015$sd_DFR <- scale(CIdata2015$DistanceFromRoad)

# Create new datasets for prediction
# Remove NAs and factorize 
# to predict to 2001
tmpID <- which(is.na(CIdata2001$Vegetation)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2001 <- CIdata2001[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2001$Vegetation)) # check if NAs dissappeared 
CIdata2001$Vegetation <- factor(CIdata2001$Vegetation) # factorize categorical variable to use in the model and overwrite data with factorized vegetation
print(class(CIdata2001$Vegetation)) # check if factorization worked

tmpID <- which(is.na(CIdata2001$geology_collapsed)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2001 <- CIdata2001[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2001$geology_collapsed)) # check if NAs dissappeared 
CIdata2001$geology_collapsed <- factor(CIdata2001$geology_collapsed)
print(class(CIdata2001$geology_collapsed))

tmpID <- which(is.na(CIdata2001$sd_elev)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2001 <- CIdata2001[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2001$sd_elev)) # check if NAs dissappeared 

tmpID <- which(is.na(CIdata2001$sd_DFR)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2001 <- CIdata2001[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2001$sd_DFR)) # check if NAs dissappeared 

CIdata2001$Year <- factor(CIdata2001$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
print(class(CIdata2001$Year)) # check if factorization worked

# to predict to 2003
tmpID <- which(is.na(CIdata2003$Vegetation)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2003 <- CIdata2003[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2003$Vegetation)) # check if NAs dissappeared 
CIdata2003$Vegetation <- factor(CIdata2003$Vegetation) # factorize categorical variable to use in the model and overwrite data with factorized vegetation
print(class(CIdata2003$Vegetation)) # check if factorization worked

tmpID <- which(is.na(CIdata2003$geology_collapsed)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2003 <- CIdata2003[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2003$geology_collapsed)) # check if NAs dissappeared 
CIdata2003$geology_collapsed <- factor(CIdata2003$geology_collapsed)
print(class(CIdata2003$geology_collapsed))

tmpID <- which(is.na(CIdata2003$sd_elev)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2003 <- CIdata2003[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2003$sd_elev)) # check if NAs dissappeared 

tmpID <- which(is.na(CIdata2003$sd_DFR)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2003 <- CIdata2003[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2003$sd_DFR)) # check if NAs dissappeared 

CIdata2003$Year <- factor(CIdata2003$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
print(class(CIdata2003$Year)) # check if factorization worked

# to predict to 2005
tmpID <- which(is.na(CIdata2005$Vegetation)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2005 <- CIdata2005[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2005$Vegetation)) # check if NAs dissappeared 
CIdata2005$Vegetation <- factor(CIdata2005$Vegetation) # factorize categorical variable to use in the model and overwrite data with factorized vegetation
print(class(CIdata2005$Vegetation)) # check if factorization worked

tmpID <- which(is.na(CIdata2005$geology_collapsed)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2005 <- CIdata2005[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2005$geology_collapsed)) # check if NAs dissappeared 
CIdata2005$geology_collapsed <- factor(CIdata2005$geology_collapsed)
print(class(CIdata2005$geology_collapsed))

tmpID <- which(is.na(CIdata2005$sd_elev)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2005 <- CIdata2005[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2005$sd_elev)) # check if NAs dissappeared 

tmpID <- which(is.na(CIdata2005$sd_DFR)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2005 <- CIdata2005[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2005$sd_DFR)) # check if NAs dissappeared 

CIdata2005$Year <- factor(CIdata2005$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
print(class(CIdata2005$Year)) # check if factorization worked

# to predict to 2007
tmpID <- which(is.na(CIdata2007$Vegetation)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2007 <- CIdata2007[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2007$Vegetation)) # check if NAs dissappeared 
CIdata2007$Vegetation <- factor(CIdata2007$Vegetation) # factorize categorical variable to use in the model and overwrite data with factorized vegetation
print(class(CIdata2007$Vegetation)) # check if factorization worked

tmpID <- which(is.na(CIdata2007$geology_collapsed)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2007 <- CIdata2007[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2007$geology_collapsed)) # check if NAs dissappeared 
CIdata2007$geology_collapsed <- factor(CIdata2007$geology_collapsed)
print(class(CIdata2007$geology_collapsed))

tmpID <- which(is.na(CIdata2007$sd_elev)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2007 <- CIdata2007[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2007$sd_elev)) # check if NAs dissappeared 

tmpID <- which(is.na(CIdata2007$sd_DFR)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2007 <- CIdata2007[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2007$sd_DFR)) # check if NAs dissappeared 

CIdata2007$Year <- factor(CIdata2007$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
print(class(CIdata2007$Year)) # check if factorization worked

# to predict to 2009
tmpID <- which(is.na(CIdata2009$Vegetation)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2009 <- CIdata2009[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2009$Vegetation)) # check if NAs dissappeared 
CIdata2009$Vegetation <- factor(CIdata2009$Vegetation) # factorize categorical variable to use in the model and overwrite data with factorized vegetation
print(class(CIdata2009$Vegetation)) # check if factorization worked

tmpID <- which(is.na(CIdata2009$geology_collapsed)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2009 <- CIdata2009[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2009$geology_collapsed)) # check if NAs dissappeared 
CIdata2009$geology_collapsed <- factor(CIdata2009$geology_collapsed)
print(class(CIdata2009$geology_collapsed))

tmpID <- which(is.na(CIdata2009$sd_elev)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2009 <- CIdata2009[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2009$sd_elev)) # check if NAs dissappeared 

tmpID <- which(is.na(CIdata2009$sd_DFR)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2009 <- CIdata2009[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2009$sd_DFR)) # check if NAs dissappeared 

CIdata2009$Year <- factor(CIdata2009$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
print(class(CIdata2009$Year)) # check if factorization worked

# to predict to 2011
tmpID <- which(is.na(CIdata2011$Vegetation)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2011 <- CIdata2011[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2011$Vegetation)) # check if NAs dissappeared 
CIdata2011$Vegetation <- factor(CIdata2011$Vegetation) # factorize categorical variable to use in the model and overwrite data with factorized vegetation
print(class(CIdata2011$Vegetation)) # check if factorization worked

tmpID <- which(is.na(CIdata2011$geology_collapsed)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2011 <- CIdata2011[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2011$geology_collapsed)) # check if NAs dissappeared 
CIdata2011$geology_collapsed <- factor(CIdata2011$geology_collapsed)
print(class(CIdata2011$geology_collapsed))

tmpID <- which(is.na(CIdata2011$sd_elev)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2011 <- CIdata2011[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2011$sd_elev)) # check if NAs dissappeared 

tmpID <- which(is.na(CIdata2011$sd_DFR)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2011 <- CIdata2011[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2011$sd_DFR)) # check if NAs dissappeared 

CIdata2011$Year <- factor(CIdata2011$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
print(class(CIdata2011$Year)) # check if factorization worked

# to predict to 2013
tmpID <- which(is.na(CIdata2013$Vegetation)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2013 <- CIdata2013[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2013$Vegetation)) # check if NAs dissappeared 
CIdata2013$Vegetation <- factor(CIdata2013$Vegetation) # factorize categorical variable to use in the model and overwrite data with factorized vegetation
print(class(CIdata2013$Vegetation)) # check if factorization worked

tmpID <- which(is.na(CIdata2013$geology_collapsed)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2013 <- CIdata2013[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2013$geology_collapsed)) # check if NAs dissappeared 
CIdata2013$geology_collapsed <- factor(CIdata2013$geology_collapsed)
print(class(CIdata2013$geology_collapsed))

tmpID <- which(is.na(CIdata2013$sd_elev)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2013 <- CIdata2013[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2013$sd_elev)) # check if NAs dissappeared 

tmpID <- which(is.na(CIdata2013$sd_DFR)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2013 <- CIdata2013[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2013$sd_DFR)) # check if NAs dissappeared 

CIdata2013$Year <- factor(CIdata2013$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
print(class(CIdata2013$Year)) # check if factorization worked

# to predict to 2015
tmpID <- which(is.na(CIdata2015$Vegetation)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2015 <- CIdata2015[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2015$Vegetation)) # check if NAs dissappeared 
CIdata2015$Vegetation <- factor(CIdata2015$Vegetation) # factorize categorical variable to use in the model and overwrite data with factorized vegetation
print(class(CIdata2015$Vegetation)) # check if factorization worked

tmpID <- which(is.na(CIdata2015$geology_collapsed)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2015 <- CIdata2015[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2015$geology_collapsed)) # check if NAs dissappeared 
CIdata2015$geology_collapsed <- factor(CIdata2015$geology_collapsed)
print(class(CIdata2015$geology_collapsed))

tmpID <- which(is.na(CIdata2015$sd_elev)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2015 <- CIdata2015[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2015$sd_elev)) # check if NAs dissappeared 

tmpID <- which(is.na(CIdata2015$sd_DFR)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2015 <- CIdata2015[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2015$sd_DFR)) # check if NAs dissappeared 

CIdata2015$Year <- factor(CIdata2015$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
print(class(CIdata2015$Year)) # check if factorization worked


# PREDICTION TO 2001
new_data_2001 <- as.data.frame(matrix(NA, nrow=nrow(CIdata2001), ncol=5))
# Fill matrix
new_data_2001[ ,1] <- CIdata2001$Vegetation
new_data_2001[ ,2] <- CIdata2001$geology_collapsed
new_data_2001[ ,3] <- CIdata2001$sd_elev
new_data_2001[ ,4] <- CIdata2001$sd_DFR
new_data_2001[ ,5] <- rep(as.factor(2003), nrow(new_data_2001))
colnames(new_data_2001) <- c("Vegetation", "geology_collapsed", "sd_elev","sd_DFR","Year")

# PREDICTION TO 2003
new_data_2003 <- as.data.frame(matrix(NA, nrow=nrow(CIdata2003), ncol=5))
# Fill matrix
new_data_2003[ ,1] <- CIdata2003$Vegetation
new_data_2003[ ,2] <- CIdata2003$geology_collapsed
new_data_2003[ ,3] <- CIdata2003$sd_elev
new_data_2003[ ,4] <- CIdata2003$sd_DFR
new_data_2003[ ,5] <- rep(as.factor(2001), nrow(new_data_2003))
colnames(new_data_2003) <- c("Vegetation", "geology_collapsed", "sd_elev","sd_DFR","Year")

# PREDICTION TO 2005
new_data_2005 <- as.data.frame(matrix(NA, nrow=nrow(CIdata2005), ncol=5))
# Fill matrix
new_data_2005[ ,1] <- CIdata2005$Vegetation
new_data_2005[ ,2] <- CIdata2005$geology_collapsed
new_data_2005[ ,3] <- CIdata2005$sd_elev
new_data_2005[ ,4] <- CIdata2005$sd_DFR
new_data_2005[ ,5] <- rep(as.factor(2003), nrow(new_data_2005))
colnames(new_data_2005) <- c("Vegetation", "geology_collapsed", "sd_elev","sd_DFR","Year")

# PREDICTION TO 2007
new_data_2007 <- as.data.frame(matrix(NA, nrow=nrow(CIdata2007), ncol=5))
# Fill matrix
new_data_2007[ ,1] <- CIdata2007$Vegetation
new_data_2007[ ,2] <- CIdata2007$geology_collapsed
new_data_2007[ ,3] <- CIdata2007$sd_elev
new_data_2007[ ,4] <- CIdata2007$sd_DFR
new_data_2007[ ,5] <- rep(as.factor(2005), nrow(new_data_2007))
colnames(new_data_2007) <- c("Vegetation", "geology_collapsed", "sd_elev","sd_DFR","Year")

# PREDICTION TO 2009
new_data_2009 <- as.data.frame(matrix(NA, nrow=nrow(CIdata2009), ncol=5))
# Fill matrix
new_data_2009[ ,1] <- CIdata2009$Vegetation
new_data_2009[ ,2] <- CIdata2009$geology_collapsed
new_data_2009[ ,3] <- CIdata2009$sd_elev
new_data_2009[ ,4] <- CIdata2009$sd_DFR
new_data_2009[ ,5] <- rep(as.factor(2007), nrow(new_data_2009))
colnames(new_data_2009) <- c("Vegetation", "geology_collapsed", "sd_elev","sd_DFR","Year")

# PREDICTION TO 2011
new_data_2011 <- as.data.frame(matrix(NA, nrow=nrow(CIdata2011), ncol=5))
# Fill matrix
new_data_2011[ ,1] <- CIdata2011$Vegetation
new_data_2011[ ,2] <- CIdata2011$geology_collapsed
new_data_2011[ ,3] <- CIdata2011$sd_elev
new_data_2011[ ,4] <- CIdata2011$sd_DFR
new_data_2011[ ,5] <- rep(as.factor(2009), nrow(new_data_2011))
colnames(new_data_2011) <- c("Vegetation", "geology_collapsed", "sd_elev","sd_DFR","Year")

# PREDICTION TO 2013
new_data_2013 <- as.data.frame(matrix(NA, nrow=nrow(CIdata2013), ncol=5))
# Fill matrix
new_data_2013[ ,1] <- CIdata2013$Vegetation
new_data_2013[ ,2] <- CIdata2013$geology_collapsed
new_data_2013[ ,3] <- CIdata2013$sd_elev
new_data_2013[ ,4] <- CIdata2013$sd_DFR
new_data_2013[ ,5] <- rep(as.factor(2011), nrow(new_data_2013))
colnames(new_data_2013) <- c("Vegetation", "geology_collapsed", "sd_elev","sd_DFR","Year")

# PREDICTION TO 2015
new_data_2015 <- as.data.frame(matrix(NA, nrow=nrow(CIdata2015), ncol=5))
# Fill matrix
new_data_2015[ ,1] <- CIdata2015$Vegetation
new_data_2015[ ,2] <- CIdata2015$geology_collapsed
new_data_2015[ ,3] <- CIdata2015$sd_elev
new_data_2015[ ,4] <- CIdata2015$sd_DFR
new_data_2015[ ,5] <- rep(as.factor(2013), nrow(new_data_2015))
colnames(new_data_2015) <- c("Vegetation", "geology_collapsed", "sd_elev","sd_DFR","Year")


# HERE IS WHERE THE PREDICTION ACTION STARTS 
new_datasets <- vector()
multi_year_datasets_excluding_one_year <- vector()
for (y in years){
  add_year <- paste("CIdata_without_",y,sep="")
  multi_year_datasets_excluding_one_year <- c(multi_year_datasets_excluding_one_year, add_year)
  add_new_data <- paste("new_data_",y,sep="")
  new_datasets <- c(new_datasets, add_new_data)
}

for(k in 1:length(multi_year_datasets_excluding_one_year)){
  model <- paste("model_YEAR_",multi_year_datasets_excluding_one_year[k],".Rda", sep="")
  load(model)
  new_data <- get(new_datasets[k])
  prediction <- predict(best.model, new_data, type="response", na.rm=TRUE)
  save(prediction, file=paste("prediction_to_",years[k],".Rda", sep=""))
}





## RasterStack does not seem to work with Year Raster - GRRRR
# Create RasterStack of the environmental variables -> RasterLayer objects all need to have the same spatial extent and resolution
DFR <- raster("DistFromRoad.tif")
sd_DFR <- scale(DFR) # standardize this layer 
names(sd_DFR) <- "sd_DFR"
DEM <- raster("DEM.grd") 
sd_DEM <- scale(DEM)
names(sd_DEM) <- "sd_elev"
GEO <- raster("Geology.tif")
names(GEO) <- "geology"
VEG <- raster("vegetation_raster.grd")
names(VEG) <- "Vegetation"
YEAR_2015 <- raster(nrow=180, ncol=210)
extent(YEAR_2015) <- c(558000,579000,8831000,8849000)
crs(YEAR_2015) <- "+proj=utm +zone=48 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
YEAR_2015[] <- 2013
summary(YEAR_2015) 
names(YEAR_2015) <- "Year"
# Specify factors in predict function?? Should not be necessary with glm... 
# Do categorical variables need to be factorized to predict?? 
Vegetation_levels <- as.factor(CIdata_without_2015$Vegetation) 
Geology_levels <- as.factor(CIdata_without_2015$geology)
Year_levels <- as.factor(CIdata_without_2015$Year)
v <- list(levels(Vegetation_levels))
g <- list(levels(Geology_levels))
y <- list(levels(Year_levels))
names(v) <- "Vegetation"
names(g) <- "geology"
names(y) <- "Year"  

RasterStack <- stack(sd_DFR, sd_DEM, GEO, VEG, YEAR_2015) # create RasterStack of environmental variables
names(RasterStack) <- c("sd_DFR","sd_elev","geology","Vegetation", "Year") # names in the Raster object should exactly match those expected by the model
load("glm_output_VEG_sd_ELEV_geology_sd_DISTFROMROAD_YEARCIdata_without_2015.Rda")
predicted_map <- predict(RasterStack, model_fit, factors=list(v,g,y), type="response", na.rm=TRUE) # predict to points across the island based on the RasterStack 
plot(predicted_map)

### Manually predict using all data but 2015 to create P(occ) map 
# Load rasters to predict
DFR <- raster("DistFromRoad.tif")
sd_DFR <- scale(DFR) # standardize this layer 
DEM <- raster("DEM.grd") 
sd_DEM <- scale(DEM)
GEO <- raster("Geology.tif")
VEG <- raster("vegetation_raster.grd")
YEAR_2015 <- raster(nrow=180, ncol=210)
extent(YEAR_2015) <- c(558000,579000,8831000,8849000)
crs(YEAR_2015) <- "+proj=utm +zone=48 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
YEAR_2015[] <- 1
# define different levels of categorical variables
veg2 <- (VEG==2)
veg3 <- (VEG==3)
veg4 <- (VEG==4)
veg5 <- (VEG==5)
veg6 <- (VEG==6)
veg7 <- (VEG==7)
geo2 <- (GEO==2)
geo3 <- (GEO==3)
geo4 <- (GEO==4)
geo5 <- (GEO==5)
geo6 <- (GEO==6)
geo7 <- (GEO==7)
geo8 <- (GEO==8)
geo9 <- (GEO==9)
# load regression coefficients obtained from model using all data but 2015 
load("glm_output_VEG_sd_ELEV_geology_sd_DISTFROMROAD_YEARCIdata_without_2015.Rda")
summary(model_fit) 
alpha <- -1.27856
beta_sd_dist <- 0.22433 
beta_sd_elev <- -0.47129
beta_veg2 <- -0.28717
beta_veg3 <- -0.27534
beta_veg4 <- -0.27731
beta_veg5 <- -0.56220
beta_veg6 <- -0.76798
beta_veg7 <- -1.46304
beta_geo2 <- -0.21960
beta_geo3 <- -0.09600
#beta_geo4 <-
#beta_geo5 <- 
beta_geo6 <- 0.09787
beta_geo7 <- -11.51097
beta_geo8 <- 0.35374
beta_geo9 <- 0.55218
beta_yr <- -1.39241

# Calculate Pr(occ)
Pr_occ_2015 <-  1/(1+exp(-alpha-beta_sd_dist*sd_DFR-beta_sd_elev*sd_DEM
                         -beta_veg2*veg2-beta_veg3*veg3-beta_veg4*veg4-beta_veg5*veg5
                         -beta_veg6*veg6-beta_veg7*veg7-beta_geo2*geo2-beta_geo3*geo3
                         -beta_geo6*geo6-beta_geo7*geo7
                         -beta_geo8*geo8-beta_geo9*geo9-beta_yr*YEAR_2015))




### Manually predict using all data but 2015 to create P(occ) map - for Kat, regression coefficients obtained from paper Ch03
# Load rasters to predict
DFR <- raster("DistFromRoad.tif")
sd_DFR <- scale(DFR) # standardize this layer 
DEM <- raster("DEM.grd") 
sd_DEM <- scale(DEM)
GEO <- raster("Geology.tif")
VEG <- raster("vegetation_raster.grd")
YEAR_2015 <- raster(nrow=180, ncol=210)
extent(YEAR_2015) <- c(558000,579000,8831000,8849000)
crs(YEAR_2015) <- "+proj=utm +zone=48 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
YEAR_2015[] <- 1
# define different levels of categorical variables
veg2 <- (VEG==2)
veg3 <- (VEG==3)
veg4 <- (VEG==4)
veg5 <- (VEG==5)
veg6 <- (VEG==6)
veg7 <- (VEG==7)
geo5 <- (GEO==5)
geo9 <- (GEO==9)
# load regression coefficients obtained from model using all data but 2015 
load("model_YEAR_CIdata_without_2015.Rda")
summary(best.model) 
alpha <- -1.27856
beta_sd_dist <- 0.22433 
beta_sd_elev <- -0.47129
beta_veg2 <- -0.28717
beta_veg3 <- -0.27534
beta_veg4 <- -0.27731
beta_veg5 <- -0.56220
beta_veg6 <- -0.76798
beta_veg7 <- -1.46304
beta_geo_col_5 <- -0.21960
beta_geo_col_9 <- 0.55218
beta_yr <- -1.39241

# Calculate Pr(occ)
Pr_occ_2015 <-  1/(1+exp(-alpha-beta_sd_dist*sd_DFR-beta_sd_elev*sd_DEM
                         -beta_veg2*veg2-beta_veg3*veg3-beta_veg4*veg4-beta_veg5*veg5
                         -beta_veg6*veg6-beta_veg7*veg7-beta_geo5*geo5-beta_geo9*geo9
                         -beta_yr*YEAR_2015))

























##########################################################################################################
################################### Archive ##############################################################
###########################################################################################################
year2001 <- 1 # set all years equal to 1 and use regression coefficient obtained from glm 
year2003 <- 1
year2005 <- 1
year2007 <- 1
year2009 <- 1
year2011 <- 1
year2013 <- 1
year2015 <- 1

### Using regresion coefficients from glm with standardized variables 
## Predicting to 2001 (glm without 2001)
# Intercept 
beta0 <- 
  # sd_elevation 
  beta1 <- 
  # sd_Distance To Roads
  beta2 <-   
  # Year
  beta3 <-  # regression coefficient associated with closest related YEAR 
  # Geology
  beta4 <-
  # Vegetation 
  beta5 <-   
  # predict 
  prob_occ <- 1/(1+exp(-beta0-beta1*sd_dem-beta2*sd_DistFromRoad-beta3*year2001-beta4*geo-beta5*veg)) # problem! categorical 
# variables have many levels -> too much to predict manually 
write.asc(prob_occ, "prob_occ_SC_predicted_to_2001.asc", gz = FALSE)
# plot predictions 
prob_occ_2001 <- read.asc("prob_occ_SC_predicted_to_2001.asc")
raster_prob_occ_2001 <- raster(prob_occ_2001)
plot(raster_prob_occ_2001)


## Predicting to 2003
# Intercept 
beta0 <- -0.9901244
# Elevation 
beta1 <- -0.0045671
# Distance To Roads
beta2 <- 0.0010753
# Year
beta3 <-  -2.1213496 # regression coefficient obtained from pattern in categorical variables
# manually predict 
prob_occ <- 1/(1+exp(-beta0-beta1*dem-beta2*DistFromRoad-beta3*year2003))
write.asc(prob_occ, "prob_occ_SC_predicted_to_2003.asc", gz = FALSE)
# plot predictions 
prob_occ_2003 <- read.asc("prob_occ_SC_predicted_to_2003.asc")
raster_prob_occ_2003 <- raster(prob_occ_2003)
plot(raster_prob_occ_2003)


## Predicting to 2005
# Intercept 
beta0 <- -0.9674660
# Elevation 
beta1 <- -0.0045480
# Distance To Roads
beta2 <- 0.0010033
# Year
beta3 <-  -1.6349669 # regression coefficient obtained from pattern in categorical variables
# manually predict 
prob_occ <- 1/(1+exp(-beta0-beta1*dem-beta2*DistFromRoad-beta3*year2005))
write.asc(prob_occ, "prob_occ_SC_predicted_to_2005.asc", gz = FALSE)
# plot predictions 
prob_occ_2005 <- read.asc("prob_occ_SC_predicted_to_2005.asc")
raster_prob_occ_2005 <- raster(prob_occ_2005)
plot(raster_prob_occ_2005)


## Predicting to 2007
# Intercept 
beta0 <- -0.9830374
# Elevation 
beta1 <- -0.0044571
# Distance To Roads
beta2 <- 0.0010057
# Year
beta3 <- -1.1596734 # regression coefficient obtained from pattern in categorical variables
# manually predict 
prob_occ <- 1/(1+exp(-beta0-beta1*dem-beta2*DistFromRoad-beta3*year2007))
write.asc(prob_occ, "prob_occ_SC_predicted_to_2007.asc", gz = FALSE)
# plot predictions 
prob_occ_2007 <- read.asc("prob_occ_SC_predicted_to_2007.asc")
raster_prob_occ_2007 <- raster(prob_occ_2007)
plot(raster_prob_occ_2007)


## Predicting to 2009
# Intercept 
beta0 <- -0.9756018
# Elevation 
beta1 <- -0.0045945
# Distance To Roads
beta2 <- 0.0010471
# Year
beta3 <- -0.9842458 # regression coefficient obtained from pattern in categorical variables
# manually predict 
prob_occ <- 1/(1+exp(-beta0-beta1*dem-beta2*DistFromRoad-beta3*year2009))
write.asc(prob_occ, "prob_occ_SC_predicted_to_2009.asc", gz = FALSE)
# plot predictions 
prob_occ_2009 <- read.asc("prob_occ_SC_predicted_to_2009.asc")
raster_prob_occ_2009 <- raster(prob_occ_2009)
plot(raster_prob_occ_2009)


## Predicting to 2011
# Intercept 
beta0 <- -1.0067035
# Elevation 
beta1 <- -0.0045396
# Distance To Roads
beta2 <- 0.0011091
# Year
beta3 <-  -1.2091341 # regression coefficient obtained from pattern in categorical variables
# manually predict 
prob_occ <- 1/(1+exp(-beta0-beta1*dem-beta2*DistFromRoad-beta3*year2011))
write.asc(prob_occ, "prob_occ_SC_predicted_to_2011.asc", gz = FALSE)
# plot predictions 
prob_occ_2011 <- read.asc("prob_occ_SC_predicted_to_2011.asc")
raster_prob_occ_2011 <- raster(prob_occ_2011)
plot(raster_prob_occ_2011)


## Predicting to 2013
# Intercept 
beta0 <- -0.9581173
# Elevation 
beta1 <- -0.0045861
# Distance To Roads
beta2 <- 0.0009943
# Year
beta3 <-  -1.3412628 # regression coefficient obtained from pattern in categorical variables
# manually predict 
prob_occ <- 1/(1+exp(-beta0-beta1*dem-beta2*DistFromRoad-beta3*year2013))
write.asc(prob_occ, "prob_occ_SC_predicted_to_2013.asc", gz = FALSE)
# plot predictions 
prob_occ_2013 <- read.asc("prob_occ_SC_predicted_to_2013.asc")
raster_prob_occ_2013 <- raster(prob_occ_2013)
plot(raster_prob_occ_2013)

## Predicting to 2015
# Intercept 
beta0 <- -0.8752035
# Elevation 
beta1 <- -0.0051447
# Distance To Roads
beta2 <- 0.0010134
# Year
beta3 <-  -0.4089913 # regression coefficient obtained from pattern in categorical variables
# manually predict 
prob_occ <- 1/(1+exp(-beta0-beta1*dem-beta2*DistFromRoad-beta3*year2015))
write.asc(prob_occ, "prob_occ_SC_predicted_to_2015.asc", gz = FALSE)
# plot predictions 
prob_occ_2015 <- read.asc("prob_occ_SC_predicted_to_2015.asc")
raster_prob_occ_2015 <- raster(prob_occ_2015)
plot(raster_prob_occ_2015)
## Load the intercepts and regression coefficients for vegetation, elevation and year manually 
## Predicting to 2001
# Elevation parameters
beta0 <- -2.6577625   #intercept 
beta1 <- -0.0043532   #regression coefficient
# Vegetation parameters 
beta2_2 <- 0.2199670
beta2_3 <- 0.0342811
beta2_4 <- 0.3668517
beta2_5 <- -0.1288004
beta2_6 <- -0.4928231
beta2_7 <- -1.1353012
# Year parameter
beta3 <- -2.3 # based on the inferred regression coefficients 
  
## imagine these are your rasters
dem <- read.asc("DEM.asc")
veg <- read.asc("vegetation_raster.asc")
year2001 <- 1 # arbitrary -> discuss with Mick and Guru 

## create the rasters needed with the categories
veg2 <- (veg==2)
veg3 <- (veg==3)
veg4 <- (veg==4)
veg5 <- (veg==5)
veg6 <- (veg==6)
veg7 <- (veg==7)

prob_occ<- 1/(1+exp(-beta0-beta1*dem-beta2_2*veg2-beta2_3*veg3-beta2_4*veg4-beta2_5*veg5-beta2_6*veg6-beta2_7*veg7-beta3*year2001))
write.asc(prob_occ, "prob_occ_SC_predicted_to_2001.asc", gz = FALSE)

## mapping the probability of occupancy based on DEM and vegetation (plot predictions)
prob_occ <- read.asc("prob_occ_SC_predicted_to_2001.asc")
raster_prob_occ <- raster(prob_occ)
plot(raster_prob_occ)

## Predicting to 2003
# Elevation parameters
beta0 <- -0.7149000   #intercept 
beta1 <- -0.0043171   #regression coefficient
# Vegetation parameters 
beta2_2 <- 0.4300173
beta2_3 <- 0.3566578
beta2_4 <- 0.4078760
beta2_5 <- 0.6521652
beta2_6 <- -0.3776541
beta2_7 <- -1.2404251
# Year parameter
beta3 <- -2.1 # based on the inferred regression coefficients 

## imagine these are your rasters
dem <- read.asc("DEM.asc")
veg <- read.asc("vegetation_raster.asc")
year2003 <- 1 # arbitrary -> discuss with Mick and Guru 

## create the rasters needed with the categories
veg2 <- (veg==2)
veg3 <- (veg==3)
veg4 <- (veg==4)
veg5 <- (veg==5)
veg6 <- (veg==6)
veg7 <- (veg==7)

prob_occ<- 1/(1+exp(-beta0-beta1*dem-beta2_2*veg2-beta2_3*veg3-beta2_4*veg4-beta2_5*veg5-beta2_6*veg6-beta2_7*veg7-beta3*year2003))
write.asc(prob_occ, "prob_occ_SC_predicted_to_2003.asc", gz = FALSE)

## mapping the probability of occupancy based on DEM and vegetation (plot predictions)
prob_occ <- read.asc("prob_occ_SC_predicted_to_2003.asc")
raster_prob_occ <- raster(prob_occ)
plot(raster_prob_occ)

## Predicting to 2005
# Elevation parameters
beta0 <- -0.6978414   #intercept 
beta1 <- -0.0042898   #regression coefficient
# Vegetation parameters 
beta2_2 <- 0.3907767
beta2_3 <- 0.3096781
beta2_4 <- 0.4139062
beta2_5 <- 0.6642915
beta2_6 <- -0.3830807
beta2_7 <- -1.1693013
# Year parameter
beta3 <- -1.6 # arbitrary -> discuss with Mick and Guru 

## imagine these are your rasters
dem <- read.asc("DEM.asc")
veg <- read.asc("vegetation_raster.asc")
year2005 <- 1 # arbitrary -> discuss with Mick and Guru 

## create the rasters needed with the categories
veg2 <- (veg==2)
veg3 <- (veg==3)
veg4 <- (veg==4)
veg5 <- (veg==5)
veg6 <- (veg==6)
veg7 <- (veg==7)

prob_occ<- 1/(1+exp(-beta0-beta1*dem-beta2_2*veg2-beta2_3*veg3-beta2_4*veg4-beta2_5*veg5-beta2_6*veg6-beta2_7*veg7-beta3*year2005))
write.asc(prob_occ, "prob_occ_SC_predicted_to_2005.asc", gz = FALSE)

## mapping the probability of occupancy based on DEM and vegetation (plot predictions)
prob_occ <- read.asc("prob_occ_SC_predicted_to_2005.asc")
raster_prob_occ <- raster(prob_occ)
plot(raster_prob_occ)

## Predicting to 2007
# Elevation parameters
beta0 <- -0.7448868   #intercept 
beta1 <- -0.0041649   #regression coefficient
# Vegetation parameters 
beta2_2 <- 0.4181234
beta2_3 <- 0.3689226
beta2_4 <- 0.3873658
beta2_5 <- 0.7525600
beta2_6 <- -0.9050425
beta2_7 <- -1.1217371
# Year parameter
beta3 <- -1.2 # based on the inferred regression coefficients

## imagine these are your rasters
dem <- read.asc("DEM.asc")
veg <- read.asc("vegetation_raster.asc")
year2007 <- 1 # arbitrary -> discuss with Mick and Guru 

## create the rasters needed with the categories
veg2 <- (veg==2)
veg3 <- (veg==3)
veg4 <- (veg==4)
veg5 <- (veg==5)
veg6 <- (veg==6)
veg7 <- (veg==7)

prob_occ<- 1/(1+exp(-beta0-beta1*dem-beta2_2*veg2-beta2_3*veg3-beta2_4*veg4-beta2_5*veg5-beta2_6*veg6-beta2_7*veg7-beta3*year2007))
write.asc(prob_occ, "prob_occ_SC_predicted_to_2007.asc", gz = FALSE)

## mapping the probability of occupancy based on DEM and vegetation (plot predictions)
prob_occ <- read.asc("prob_occ_SC_predicted_to_2007.asc")
raster_prob_occ <- raster(prob_occ)
plot(raster_prob_occ)

## Predicting to 2009
# Elevation parameters
beta0 <- -0.7535806   #intercept 
beta1 <- -0.0043446  #regression coefficient
# Vegetation parameters 
beta2_2 <- 0.4939353
beta2_3 <- 0.3715954
beta2_4 <- 0.0845661
beta2_5 <- 0.9897583
beta2_6 <- -0.2451065
beta2_7 <- -1.0887359
# Year parameter
beta3 <- -1.1 # based on the inferred regression coefficients

## imagine these are your rasters
dem <- read.asc("DEM.asc")
veg <- read.asc("vegetation_raster.asc")
year2009 <- 1 # arbitrary -> discuss with Mick and Guru 

## create the rasters needed with the categories
veg2 <- (veg==2)
veg3 <- (veg==3)
veg4 <- (veg==4)
veg5 <- (veg==5)
veg6 <- (veg==6)
veg7 <- (veg==7)

prob_occ<- 1/(1+exp(-beta0-beta1*dem-beta2_2*veg2-beta2_3*veg3-beta2_4*veg4-beta2_5*veg5-beta2_6*veg6-beta2_7*veg7-beta3*year2009))
write.asc(prob_occ, "prob_occ_SC_predicted_to_2009.asc", gz = FALSE)

## mapping the probability of occupancy based on DEM and vegetation (plot predictions)
prob_occ <- read.asc("prob_occ_SC_predicted_to_2009.asc")
raster_prob_occ <- raster(prob_occ)
plot(raster_prob_occ)

## Predicting to 2011
# Elevation parameters
beta0 <- -0.7489744   #intercept 
beta1 <- -0.0042947  #regression coefficient
# Vegetation parameters 
beta2_2 <- 0.4799925
beta2_3 <- 0.3885802
beta2_4 <- 0.2095540
beta2_5 <- 0.7634515
beta2_6 <- -0.5134941
beta2_7 <- -1.1881860
# Year parameter
beta3 <- -1.2 # based on the inferred regression coefficients 

## imagine these are your rasters
dem <- read.asc("DEM.asc")
veg <- read.asc("vegetation_raster.asc")
year2011 <- 1 # arbitrary -> discuss with Mick and Guru 

## create the rasters needed with the categories
veg2 <- (veg==2)
veg3 <- (veg==3)
veg4 <- (veg==4)
veg5 <- (veg==5)
veg6 <- (veg==6)
veg7 <- (veg==7)

prob_occ<- 1/(1+exp(-beta0-beta1*dem-beta2_2*veg2-beta2_3*veg3-beta2_4*veg4-beta2_5*veg5-beta2_6*veg6-beta2_7*veg7-beta3*year2011))
write.asc(prob_occ, "prob_occ_SC_predicted_to_2011.asc", gz = FALSE)

## mapping the probability of occupancy based on DEM and vegetation (plot predictions)
prob_occ <- read.asc("prob_occ_SC_predicted_to_2011.asc")
raster_prob_occ <- raster(prob_occ)
plot(raster_prob_occ)

## Predicting to 2013
# Elevation parameters
beta0 <- -0.7414601   #intercept 
beta1 <- -0.0042519  #regression coefficient
# Vegetation parameters 
beta2_2 <- 0.4234554
beta2_3 <- 0.4100898
beta2_4 <- 0.2968740
beta2_5 <- 0.7390532
beta2_6 <- -0.5395559
beta2_7 <- -1.1507020
# Year parameter
beta3 <- -1.3 # based on the inferred regression coefficients 

## imagine these are your rasters
dem <- read.asc("DEM.asc")
veg <- read.asc("vegetation_raster.asc")
year2013 <- 1 # arbitrary -> discuss with Mick and Guru 

## create the rasters needed with the categories
veg2 <- (veg==2)
veg3 <- (veg==3)
veg4 <- (veg==4)
veg5 <- (veg==5)
veg6 <- (veg==6)
veg7 <- (veg==7)

prob_occ<- 1/(1+exp(-beta0-beta1*dem-beta2_2*veg2-beta2_3*veg3-beta2_4*veg4-beta2_5*veg5-beta2_6*veg6-beta2_7*veg7-beta3*year2013))
write.asc(prob_occ, "prob_occ_SC_predicted_to_2013.asc", gz = FALSE)

## mapping the probability of occupancy based on DEM and vegetation (plot predictions)
prob_occ <- read.asc("prob_occ_SC_predicted_to_2013.asc")
raster_prob_occ <- raster(prob_occ)
plot(raster_prob_occ)

## Predicting to 2015
# Elevation parameters
beta0 <- -0.6481682   #intercept 
beta1 <- -0.0048363  #regression coefficient
# Vegetation parameters 
beta2_2 <- 0.4758992
beta2_3 <- 0.3644500
beta2_4 <- 0.3996018
beta2_5 <- -0.0878341
beta2_6 <- -0.1672475
beta2_7 <- -1.3337224
# Year parameter
beta3 <- -0.4 # based on the inferred regression coefficients 

## imagine these are your rasters
dem <- read.asc("DEM.asc")
veg <- read.asc("vegetation_raster.asc")
year2015 <- 1 # arbitrary -> discuss with Mick and Guru 

## create the rasters needed with the categories
veg2 <- (veg==2)
veg3 <- (veg==3)
veg4 <- (veg==4)
veg5 <- (veg==5)
veg6 <- (veg==6)
veg7 <- (veg==7)

prob_occ<- 1/(1+exp(-beta0-beta1*dem-beta2_2*veg2-beta2_3*veg3-beta2_4*veg4-beta2_5*veg5-beta2_6*veg6-beta2_7*veg7-beta3*year2015))
write.asc(prob_occ, "prob_occ_SC_predicted_to_2015.asc", gz = FALSE)

## mapping the probability of occupancy based on DEM and vegetation (plot predictions)
prob_occ <- read.asc("prob_occ_SC_predicted_to_2015.asc")
raster_prob_occ <- raster(prob_occ)
plot(raster_prob_occ)