### Calculation of probability of supercolony occurrence for each survey site (new data)
### No need to create maps because I am predicting 
### The probability of supercolony occurrence (reward) and the cost to travel to and survey at a site (cost) 
### will be used to prioritize sites to manage(i.e. using the Knapsack formulation)
### Sites will be prioritized based on the Knapsack formulation (benefit over cost or P(occ)/cost) and management budget
### Sites will be ordered by decreasing Knapsack value and all sites that lay within the management budget will be visited (top sites)

### Prediciting to next year happens with coefficients derived from previous year
### e.g. new_data_2005 actually means that the probability of supercolony occurrence is 
### calculated using data of 2001 and 2003 (spatial data and regression coefficient associated with 
### 2003). It is called new_data_2005 because we will use the SC distribution of 2005 to evaluate 
### performance of the model in finding the supercolonies (based on the Knapsack formulation)

### All previous scripts need to run first to load CIdata into R memory 

## Create matrices containing the new data which we will predict to (we are predicting to points 
# = survey sites, which is what we want in this analysis)

## Create matrix with new data to predict to 2005
new_data_2005 <- as.data.frame(matrix(NA, nrow=nrow(CIdata2003), ncol=5)) # number of columns = number of predictors used in the model  
# Remove NAs and factorize categorical variables 
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
  
tmpID <- which(is.na(CIdata2003$sd_elevation)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2003 <- CIdata2003[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2003$sd_elevation)) # check if NAs dissappeared 
  
CIdata2003$Year <- factor(CIdata2003$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
print(class(CIdata2003$Year)) # check if factorization worked

# Fill matrix 
new_data_2005[ ,1] <- CIdata2003$Vegetation
new_data_2005[ ,2] <- CIdata2003$geology_collapsed
new_data_2005[ ,3] <- CIdata2003$sd_elevation
new_data_2005[ ,4] <- CIdata2003$sd_distancefromroad
new_data_2005[ ,5] <- rep(as.factor(2003), nrow(new_data_2005))
colnames(new_data_2005) <- c("Vegetation", "geology_collapsed", "sd_elev","sd_DFR","Year")

## Create matrix with new data to predict to 2007
new_data_2007 <- as.data.frame(matrix(NA, nrow=nrow(CIdata2005), ncol=5)) # number of columns = number of predictors used in the model  
# Remove NAs and factorize categorical variables 
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

tmpID <- which(is.na(CIdata2005$sd_elevation)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2005 <- CIdata2005[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2005$sd_elevation)) # check if NAs dissappeared 

CIdata2005$Year <- factor(CIdata2005$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
print(class(CIdata2005$Year)) # check if factorization worked

# Fill matrix 
new_data_2007[ ,1] <- CIdata2005$Vegetation
new_data_2007[ ,2] <- CIdata2005$geology_collapsed
new_data_2007[ ,3] <- CIdata2005$sd_elevation
new_data_2007[ ,4] <- CIdata2005$sd_distancefromroad
new_data_2007[ ,5] <- rep(as.factor(2005), nrow(new_data_2007))
colnames(new_data_2007) <- c("Vegetation", "geology_collapsed", "sd_elev","sd_DFR","Year")

## Create matrix with new data to predict to 2009
new_data_2009 <- as.data.frame(matrix(NA, nrow=nrow(CIdata2007), ncol=5)) # number of columns = number of predictors used in the model  
# Remove NAs and factorize categorical variables 
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

tmpID <- which(is.na(CIdata2007$sd_elevation)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2007 <- CIdata2007[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2007$sd_elevation)) # check if NAs dissappeared 

CIdata2007$Year <- factor(CIdata2007$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
print(class(CIdata2007$Year)) # check if factorization worked

# Fill matrix 
new_data_2009[ ,1] <- CIdata2007$Vegetation
new_data_2009[ ,2] <- CIdata2007$geology_collapsed
new_data_2009[ ,3] <- CIdata2007$sd_elevation
new_data_2009[ ,4] <- CIdata2007$sd_distancefromroad
new_data_2009[ ,5] <- rep(as.factor(2007), nrow(new_data_2009))
colnames(new_data_2009) <- c("Vegetation", "geology_collapsed", "sd_elev","sd_DFR","Year")

## Create matrix with new data to predict to 2009
new_data_2011 <- as.data.frame(matrix(NA, nrow=nrow(CIdata2009), ncol=5)) # number of columns = number of predictors used in the model  
# Remove NAs and factorize categorical variables 
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

tmpID <- which(is.na(CIdata2009$sd_elevation)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2009 <- CIdata2009[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2009$sd_elevation)) # check if NAs dissappeared 

CIdata2009$Year <- factor(CIdata2009$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
print(class(CIdata2009$Year)) # check if factorization worked

# Fill matrix 
new_data_2011[ ,1] <- CIdata2009$Vegetation
new_data_2011[ ,2] <- CIdata2009$geology_collapsed
new_data_2011[ ,3] <- CIdata2009$sd_elevation
new_data_2011[ ,4] <- CIdata2009$sd_distancefromroad
new_data_2011[ ,5] <- rep(as.factor(2009), nrow(new_data_2011))
colnames(new_data_2011) <- c("Vegetation", "geology_collapsed", "sd_elev","sd_DFR","Year")

## Create matrix with new data to predict to 2009
new_data_2013 <- as.data.frame(matrix(NA, nrow=nrow(CIdata2011), ncol=5)) # number of columns = number of predictors used in the model  
# Remove NAs and factorize categorical variables 
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

tmpID <- which(is.na(CIdata2011$sd_elevation)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2011 <- CIdata2011[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2011$sd_elevation)) # check if NAs dissappeared 

CIdata2011$Year <- factor(CIdata2011$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
print(class(CIdata2011$Year)) # check if factorization worked

# Fill matrix 
new_data_2013[ ,1] <- CIdata2011$Vegetation
new_data_2013[ ,2] <- CIdata2011$geology_collapsed
new_data_2013[ ,3] <- CIdata2011$sd_elevation
new_data_2013[ ,4] <- CIdata2011$sd_distancefromroad
new_data_2013[ ,5] <- rep(as.factor(2011), nrow(new_data_2013))
colnames(new_data_2013) <- c("Vegetation", "geology_collapsed", "sd_elev","sd_DFR","Year")

## Create matrix with new data to predict to 2009
new_data_2015 <- as.data.frame(matrix(NA, nrow=nrow(CIdata2013), ncol=5)) # number of columns = number of predictors used in the model  
# Remove NAs and factorize categorical variables 
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

tmpID <- which(is.na(CIdata2013$sd_elevation)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata2013 <- CIdata2013[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata2013$sd_elevation)) # check if NAs dissappeared 

CIdata2013$Year <- factor(CIdata2013$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
print(class(CIdata2013$Year)) # check if factorization worked

# Fill matrix 
new_data_2015[ ,1] <- CIdata2013$Vegetation
new_data_2015[ ,2] <- CIdata2013$geology_collapsed
new_data_2015[ ,3] <- CIdata2013$sd_elevation
new_data_2015[ ,4] <- CIdata2013$sd_distancefromroad
new_data_2015[ ,5] <- rep(as.factor(2013), nrow(new_data_2015))
colnames(new_data_2015) <- c("Vegetation", "geology_collapsed", "sd_elev","sd_DFR","Year")

## Old and new data 
# create list containing all new data matrices 
new_data_matrices <- list()
new_data_matrices[[1]] <- new_data_2005
new_data_matrices[[2]] <- new_data_2007
new_data_matrices[[3]] <- new_data_2009
new_data_matrices[[4]] <- new_data_2011
new_data_matrices[[5]] <- new_data_2013
new_data_matrices[[6]] <- new_data_2015

# create list containing all model_fit files 
all_data_files <- vector()
model_fits <- list()

percentage_of_total_survey_budget <- 1:100 # fractions of survey budget
cumulative_surveyed_years <- c("2001_2003", "2001_2003_2005", "2001_2003_2005_2007",
                               "2001_2003_2005_2007_2009","2001_2003_2005_2007_2009_2011", 
                               "2001_2003_2005_2007_2009_2011_2013")

for (i in cumulative_surveyed_years) {
  for (j in percentage_of_total_survey_budget){
    add <- paste("CIdata_",i,"_",j,"_percent", sep="")
    all_data_files <- c(all_data_files, add)
  }
}

for (s in 1:length(all_data_files)){
  b <- paste("model_", all_data_files[s], ".Rda", sep="")
  model_fits[[s]] <- b
}

## There is a problem when new data contains levels of categorical variables that are not
# present in the model fit. This is possible when we only survey a proportion of the sites 
# R automatically stops the prediction in that case. Therefore we want to set those levels 
# in the new data that are absent in the old data to NA, so we can still do the prediciton.

# Function to set levels that are present in new data but absent in model data equal to NA
missingLevelsToNA <- function(object,data){
  
  #Obtain factor predictors in the model and their levels ------------------
  
  factors <- (gsub("[-^0-9]|as.factor|\\(|\\)", "",names(unlist(object$xlevels))))
  factorLevels <- unname(unlist(object$xlevels))
  modelFactors <- as.data.frame(cbind(factors,factorLevels))
  
  
  #Select column names in your data that are factor predictors in your model -----
  
  predictors<-names(data[names(data) %in% factors])
  
  
  #For each factor predictor in your data if the level is not in the model set the value to NA --------------
  
  for (i in 1:length(predictors)){
    found<-data[,predictors[i]] %in% modelFactors[modelFactors$factors==predictors[i],]$factorLevels
    if (any(!found)) data[!found,predictors[i]] <- NA
  }
  data
}



# HERE'S WHERE THE ACTION STARTS  
# We have 100 model fits (obtained from surveying 1 - 100% sites) per new data matrix 
# We loop over sites from x - 100 -> starting from the number of sites that results in 
# covariables being identified as predictors  
# These loops are meant to set the levels of categorical variables that appear in new data 
# but NOT in the model, to NA (we need to do this because otherwise R will stop executing predict function)

new_data_MissingLevelsToNa <- list() # declare list to store new_data_matrices with NAs for new levels 

# for new_data_2005 
for (m in 4:100){
  n <- new_data_matrices[[1]]
  load(model_fits[[m]])
  y <- missingLevelsToNA(object=best.model, data=n)
  new_data_MissingLevelsToNa[[m]] <- y
}
# for new_data_2007
for (m in 102:200){
  n <- new_data_matrices[[2]]
  load(model_fits[[m]])
  y <- missingLevelsToNA(object=best.model, data=n)
  new_data_MissingLevelsToNa[[m]] <- y
}
# for new_data_2009
for (m in 205:300){
  n <- new_data_matrices[[3]]
  load(model_fits[[m]])
  y <- missingLevelsToNA(object=best.model, data=n)
  new_data_MissingLevelsToNa[[m]] <- y
}
for (m in c(203,204)){ # model 203 and 204 only contain elevation -> previous loop does not work for these models because no categorical variables
  n <- new_data_matrices[[3]] 
  new_data_MissingLevelsToNa[[m]] <- n # new data for these models can stay the same because no levels 
  #need to be set to NA (predict function will ignore the variables that appear in new data but not in model)
}
# for new_data_2011
for (m in 301:400){
  n <- new_data_matrices[[4]]
  load(model_fits[[m]])
  y <- missingLevelsToNA(object=best.model, data=n)
  new_data_MissingLevelsToNa[[m]] <- y
}
# for new_data_2013
for (m in 401:500){
  n <- new_data_matrices[[5]]
  load(model_fits[[m]])
  y <- missingLevelsToNA(object=best.model, data=n)
  new_data_MissingLevelsToNa[[m]] <- y
}
# for new_data_2015
for (m in 501:600){
  n <- new_data_matrices[[6]]
  load(model_fits[[m]])
  y <- missingLevelsToNA(object=best.model, data=n)
  new_data_MissingLevelsToNa[[m]] <- y
}


## Prediction
# Predictions are done based on the best.model and new data (with NAs for new levels)
# when the best.model only contains an intercept, predictions are only made for those sites
# for which it has data and a constant probability of occurrence is assigned to those sites 
# calculated based on the intercept (this equals the prevalence - or proportion of presences)
# This is the case for the following components in the list: predictions[[1,2,3,101,201,202]]
# Those components equal the components in the model_fits list that were left out in the previous loop 
# Make sure you don't use these predictions to guide the survey. In those cases a random search (or management)
# needs to be done (see further script)

predictions <- list()

for (k in 1:length(all_data_files)){
  print(k)
  a <- paste("model_", all_data_files[k], ".Rda", sep="")
  load(a)
  prediction_to_add_to_list <- predict(best.model, new_data_MissingLevelsToNa[[k]], type="response", na.rm=TRUE)
  predictions[[k]] <- prediction_to_add_to_list 
}

save(predictions, file="predictions.Rda")











################################ TRY OUT - TO PLOT P(OCC) MAP #########################################################

### plot P(occ) map ###
results <- DEM # to create a modelraster
results[results>0] <- predictions[[1]] # give each cell of the raster the predicted value ! PROBLEM: only 837 sites that were predicted to 


### Manually predict using all years but 2015 to create P(occ) map 
# Load rasters to predict
DFR <- raster("DistFromRoad.tif")
sd_DFR <- scale(DFR) # standardize this layer 
DEM <- raster("DEM.grd") 
sd_DEM <- scale(DEM)
GEO <- raster("Geology.tif")
GEO[GEO==4] <- NA
GEO[GEO==5] <- NA
GEO[GEO==7] <- NA
VEG <- raster("vegetation_raster.grd")
YEAR_2015 <- DEM
YEAR_2015[YEAR_2015>-1] <- 2013

RasterStack <- stack(sd_DFR, sd_DEM, GEO, VEG, YEAR_2015)
names(RasterStack) <- c("sd_DFR","sd_elev","geology_collapsed","Vegetation", "Year")
load("glm_output_VEG_sd_ELEV_geology_sd_DISTFROMROAD_YEARCIdata2001_2003_2005_2007_2009_2011_2013_1_percent.Rda")
predicted_map <- predict(RasterStack, model_fit, type="response", na.rm=FALSE) # predict to points across the island based on the RasterStack 
plot(predicted_map)







# 1% of sites 
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
load("glm_output_VEG_sd_ELEV_geology_sd_DISTFROMROAD_YEARCIdata2001_2003_2005_2007_2009_2011_2013_1_percent.Rda")
summary(model_fit) 
alpha <- -24.0359
beta_sd_dist <- 2.1689
beta_sd_elev <- 2.1933 
beta_veg2 <- 20.6339
beta_veg3 <- 38.7112
#beta_veg4 <- 
#beta_veg5 <- 
#beta_veg6 <- 
beta_veg7 <- 21.1594
#beta_geo2 <- 
beta_geo3 <- -0.09600
#beta_geo4 <-
#beta_geo5 <- 
#beta_geo6 <- 
#beta_geo7 <- 
beta_geo8 <- -15.8649
#beta_geo9 <- 
beta_yr <- -18.4715

# Calculate Pr(occ)
Pr_occ_2015 <-  1/(1+exp(-alpha-beta_sd_dist*sd_DFR-beta_sd_elev*sd_DEM
                         -beta_veg2*veg2-beta_veg3*veg3-beta_veg4*veg4-beta_veg5*veg5
                         -beta_veg6*veg6-beta_veg7*veg7-beta_geo2*geo2-beta_geo3*geo3
                         -beta_geo6*geo6-beta_geo7*geo7
                         -beta_geo8*geo8-beta_geo9*geo9-beta_yr*YEAR_2015))


# 10% of sites 
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
load("glm_output_VEG_sd_ELEV_geology_sd_DISTFROMROAD_YEARCIdata2001_2003_2005_2007_2009_2011_2013_10_percent.Rda")
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

# 50% of sites 
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
load("glm_output_VEG_sd_ELEV_geology_sd_DISTFROMROAD_YEARCIdata2001_2003_2005_2007_2009_2011_2013_50_percent.Rda")
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

# 100% of sites 
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
load("glm_output_VEG_sd_ELEV_geology_sd_DISTFROMROAD_YEARCIdata2001_2003_2005_2007_2009_2011_2013_100_percent.Rda")
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






################################################################################################################
###################################### Archive: Rasters - not needed in this case ##############################
################################################################################################################
# Load required packages 
library(SDMTools)
library(rgdal)
library(raster)

# Create modelraster
modelraster <- raster(nrow=180,ncol=210)
extent(modelraster) <- c(558000,579000,8831000,8849000)
crs(modelraster) <- "+proj=utm +zone=48 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
modelraster[] <- NA
summary(modelraster)

# Create Year Raster (to to able to predict to each year)
year_raster <- raster(nrow=180,ncol=210)
extent(year_raster) <- c(558000,579000,8831000,8849000)
crs(year_raster) <- "+proj=utm +zone=48 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
year_raster[] <- 1
summary(year_raster) 

# Create RasterStack of the environmental variables -> RasterLayer objects all need to have the same spatial extent and resolution
DEM <- raster("DEM.grd") # read in DEM RasterLayer
names(DEM) <- "elevation"
vegetation_raster <- raster("vegetation_raster.grd") # read in Vegetation RasterLayer
names(vegetation_raster) <- "Vegetation"
year_raster <- year_raster
names(year_raster) <- "Year" # can only be used if Year is a continuous variable 
RasterStack_Elev_Year <- stack(DEM,year_raster) # create RasterStack 
names(RasterStack_Elev_Year) <- c("Elevation", "Year") 

# Create RasterLayers for different years -> each grid cell gets value of 1 - prediction will happen with the appropriate coefficient for that year 
# We can only start predicting to 2005 when we include Year as a covariable -> because
# the model needs at least two years (i.e. 2001 and 2003) to be able to estimate regression 
# coefficient for Year reliably



######################################################################################################################
######################### Manually predicting to each dataset between 2005 and 2015 with glm with lowest AIC, which is
### glm vegetation, sd_elevation, geology, sd_distancefromroad, Year as fixed effects ################################
######################################################################################################################
## Read in environmental variables 
DistFromRoad <- raster("DistFromRoad.tif")
DistFromRoad_scaled <- scale(DistFromRoad)
names(DistFromRoad_scaled) <- "sd_distancefromroad"
Elevation <- raster("DEM.grd")
Elevation_scaled <- scale(Elevation)
names(Elevation_scaled) <- "sd_elevation"
Vegetation <- raster("vegetation_raster.grd")
names(Vegetation) <- "Vegetation"
Geology <- raster("Geology.tif")
names(Geology) <- "geology_collapsed"
# Create Year Raster (to to able to predict to each year)
year_raster <- raster(nrow=180,ncol=210)
extent(year_raster) <- c(558000,579000,8831000,8849000)
crs(year_raster) <- "+proj=utm +zone=48 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
year_raster[] <- 1
summary(year_raster) 
names(year_raster) <- "Year"
## Create RasterStack - does not work!!! 
#RasterStack_2003 <- stack(DistFromRoad_scaled, Elevation_scaled, Vegetation, Geology, year_raster)
#names(RasterStack_2003) <- c("sd_distancefromroad", "sd_elevation", "Vegetation", "geology_collapsed", "Year")
#predict(RasterStack_2003, model_fit, type="response", na.rm=TRUE) # Returns NAs!!!! WHy?????













########################################################################################################################
#################################### Archive: manual prediction ########################################################
########################################################################################################################

# create rasters with different levels of categorical variables
veg2 <- (Vegetation==2)
veg3 <- (Vegetation==3)
veg4 <- (Vegetation==4)
veg5 <- (Vegetation==5)
veg6 <- (Vegetation==6)
veg7 <- (Vegetation==7)
geo2 <- (Geology==2)
geo3 <- (Geology==3)
geo4 <- (Geology==4)
geo5 <- (Geology==5)
geo6 <- (Geology==6)
geo7 <- (Geology==7)
geo8 <- (Geology==8)
geo9 <- (Geology==9)


## Predicting using 2005 as independent test dataset 
# 2005_10%
alpha <- 0.08714
beta_dist <- 2
beta_elev <- 3
beta_veg2 <- 1.05469
beta_veg3 <- 0.67270
beta_veg4 <- -16.19701
beta_veg5 <- 0
beta_veg6 <- -16.45838
beta_veg7 <- -1.00389
beta_geo2 <- 5
beta_geo3 <- 
beta_geo4 <-
beta_geo5 <-
beta_geo6 <-
beta_geo7 <-
beta_geo8 <-
beta_geo9 <-
beta_yr <- 6

predicted_pr_occ_with_CIdata2001_2003_10_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                             -beta_veg2*veg2-beta_veg3*veg3-beta_veg4*veg4-beta_veg5*veg5
                                                             -beta_veg6*veg6-beta_veg7*veg7-beta_geo2*geo2-beta_geo3*geo3
                                                             -beta_geo4*geo4-beta_geo5*geo5-beta_geo6*geo6-beta_geo7*geo7
                                                             -beta_geo8*geo8-beta_geo9*geo9-beta_yr*year_raster))
write.asc(predicted_pr_occ_with_CIdata2001_2003_10_percent, "prob_occ_SC_predicted_with_CIdata2001_2003_10_percent.asc", gz = FALSE)
plot(predicted_pr_occ_with_CIdata2001_2003_10_percent, main="Predicted Pr(occ) using survey data of 2001_2003 and 10 of sites")

# 2005_20%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_20_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                             -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2005_30%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_30_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                             -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2005_40%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_40_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                             -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2005_50%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_50_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                             -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2005_60%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_60_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                             -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2005_70%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_70_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                             -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2005_80%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_80_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                             -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2005_90%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_90_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                             -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2005_100%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_100_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                             -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

## Predicting to 2007
# 2007_10%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_10_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                             -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))
# 2007_20%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_20_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                             -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2007_30%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_30_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                             -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2007_40%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_40_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                             -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2007_50%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_50_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                             -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2007_60%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_60_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                             -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2007_70%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_70_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                             -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2007_80%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_80_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                             -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2007_90%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_90_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                             -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2007_100%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_100_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                              -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

## Predicting to 2009
# 2009_10%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_10_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                  -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))
# 2009_20%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_20_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                  -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2009_30%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_30_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                  -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2009_40%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_40_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                  -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2009_50%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_50_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                  -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2009_60%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_60_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                  -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2009_70%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_70_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                  -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2009_80%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_80_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                  -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2009_90%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_90_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                  -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2009_100%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_100_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                   -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

## Predicting to 2011
# 2011_10%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_10_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                       -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))
# 2011_20%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_20_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                       -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2011_30%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_30_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                       -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2011_40%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_40_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                       -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2011_50%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_50_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                       -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2011_60%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_60_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                       -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2011_70%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_70_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                       -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2011_80%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_80_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                       -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2011_90%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_90_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                       -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2011_100%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_100_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                        -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

## Predicting to 2013
# 2013_10%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_2011_10_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                            -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))
# 2013_20%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_2011_20_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                            -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2013_30%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_2011_30_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                            -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2013_40%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_2011_40_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                            -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2013_50%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_2011_50_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                            -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2013_60%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_2011_60_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                            -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2013_70%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_2011_70_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                            -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2013_80%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_2011_80_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                            -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2013_90%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_2011_90_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                            -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2013_100%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_2011_100_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                             -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

## Predicting to 2015
# 2015_10%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_2011_2013_10_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                                 -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))
# 2015_20%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_2011_2013_20_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                                 -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2015_30%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_2011_2013_30_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                                 -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2015_40%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_2011_2013_40_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                                 -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2015_50%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_2011_2013_50_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                                 -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2015_60%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_2011_2013_60_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                                 -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2015_70%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_2011_2013_70_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                                 -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2015_80%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_2011_2013_80_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                                 -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2015_90%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_2011_2013_90_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                                 -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

# 2015_100%
alpha <- 1
beta_dist <- 2
beta_elev <- 3
beta_veg <- 4
beta_geo <- 5
beta_yr <- 6
predicted_pr_occ_with_CIdata2001_2003_2005_2007_2009_2011_2013_100_percent <- 1/(1+exp(-alpha-beta_dist*DistFromRoad_scaled-beta_elev*Elevation_scaled
                                                                                  -beta_veg*Vegetation-beta_geo*Geology-beta_yr*year_raster))

##################### Manually predicting to 2005 using elevation and Year models fitted to 10 - 100% of data #########
dem <- read.asc("DEM.asc")
year_raster <- raster(nrow=180,ncol=210)
extent(year_raster) <- c(558000,579000,8831000,8849000)
crs(year_raster) <- "+proj=utm +zone=48 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
year_raster[] <- 1
summary(year_raster)
writeRaster(year_raster,"Year_Raster.asc",format="ascii")
Year_Raster <- read.asc("Year_Raster.asc")
  # these values are different for each datascenario - coefficient for Year 2005 is based
  # on coefficient previous Year, i.e. 2003
  # 2005_10%
  beta0 <- 0.112074
  beta_elev <- -0.007509
  beta_2005_10 <- -2.735682
  # manually predict 
  prob_occ_predicted_with_CIdata2001_2003_10_percent <- 1/(1+exp(-beta0-beta_elev*dem-beta_2005_10*Year_Raster))
  write.asc(prob_occ_predicted_with_CIdata2001_2003_10_percent, "prob_occ_SC_predicted_with_CIdata2001_2003_10_percent.asc", gz = FALSE)
  # plot predictions 
  prob_occ_predicted_with_CIdata2001_2003_10_percent <- read.asc("prob_occ_SC_predicted_with_CIdata2001_2003_10_percent.asc")
  raster_prob_occ_predicted_with_CIdata2001_2003_10_percent <- raster(prob_occ_predicted_with_CIdata2001_2003_10_percent)
  plot(raster_prob_occ_predicted_with_CIdata2001_2003_10_percent)
  
  # 2005_20%
  beta0 <- 0.038933
  beta_elev <- -0.008854
  beta_2005_20 <- -1.909521
  # manually predict 
  prob_occ_predicted_with_CIdata2001_2003_20_percent <- 1/(1+exp(-beta0-beta_elev*dem-beta_2005_20*Year_Raster))
  write.asc(prob_occ_predicted_with_CIdata2001_2003_20_percent, "prob_occ_SC_predicted_with_CIdata2001_2003_20_percent.asc", gz = FALSE)
  # plot predictions 
  prob_occ_predicted_with_CIdata2001_2003_20_percent <- read.asc("prob_occ_SC_predicted_with_CIdata2001_2003_20_percent.asc")
  raster_prob_occ_predicted_with_CIdata2001_2003_20_percent <- raster(prob_occ_predicted_with_CIdata2001_2003_20_percent)
  plot(raster_prob_occ_predicted_with_CIdata2001_2003_20_percent)
  
  # 2005_30%
  beta0 <- -0.047756
  beta_elev <- -0.008367
  beta_2005_30 <- -1.841521
  # manually predict 
  prob_occ_predicted_with_CIdata2001_2003_30_percent <- 1/(1+exp(-beta0-beta_elev*dem-beta_2005_30*Year_Raster))
  write.asc(prob_occ_predicted_with_CIdata2001_2003_30_percent, "prob_occ_SC_predicted_with_CIdata2001_2003_30_percent.asc", gz = FALSE)
  # plot predictions 
  prob_occ_predicted_with_CIdata2001_2003_30_percent <- read.asc("prob_occ_SC_predicted_with_CIdata2001_2003_30_percent.asc")
  raster_prob_occ_predicted_with_CIdata2001_2003_30_percent <- raster(prob_occ_predicted_with_CIdata2001_2003_30_percent)
  plot(raster_prob_occ_predicted_with_CIdata2001_2003_30_percent)

  # 2005_40%
  beta0 <- -0.061528
  beta_elev <- -0.008170
  beta_2005_40 <- -2.160112 
  # manually predict 
  prob_occ_predicted_with_CIdata2001_2003_40_percent <- 1/(1+exp(-beta0-beta_elev*dem-beta_2005_40*Year_Raster))
  write.asc(prob_occ_predicted_with_CIdata2001_2003_40_percent, "prob_occ_SC_predicted_with_CIdata2001_2003_40_percent.asc", gz = FALSE)
  # plot predictions 
  prob_occ_predicted_with_CIdata2001_2003_40_percent <- read.asc("prob_occ_SC_predicted_with_CIdata2001_2003_40_percent.asc")
  raster_prob_occ_predicted_with_CIdata2001_2003_40_percent<- raster(prob_occ_predicted_with_CIdata2001_2003_40_percent)
  plot(raster_prob_occ_predicted_with_CIdata2001_2003_40_percent)
  
  # 2005_50%
  beta0 <- -0.217974
  beta_elev <- -0.007402
  beta_2005_50 <- -1.977940
  # manually predict 
  prob_occ_predicted_with_CIdata2001_2003_50_percent <- 1/(1+exp(-beta0-beta_elev*dem-beta_2005_50*Year_Raster))
  write.asc(prob_occ_predicted_with_CIdata2001_2003_50_percent, "prob_occ_SC_predicted_with_CIdata2001_2003_50_percent.asc", gz = FALSE)
  # plot predictions 
  prob_occ_predicted_with_CIdata2001_2003_50_percent <- read.asc("prob_occ_SC_predicted_with_CIdata2001_2003_50_percent.asc")
  raster_prob_occ_predicted_with_CIdata2001_2003_50_percent <- raster(prob_occ_predicted_with_CIdata2001_2003_50_percent)
  plot(raster_prob_occ_predicted_with_CIdata2001_2003_50_percent)
  
  # 2005_60%
  beta0 <- -0.319046
  beta_elev <- -0.006977
  beta_2005_60 <- -2.009454
  # manually predict 
  prob_occ_predicted_with_CIdata2001_2003_60_percent <- 1/(1+exp(-beta0-beta_elev*dem-beta_2005_60*Year_Raster))
  write.asc(prob_occ_predicted_with_CIdata2001_2003_60_percent, "prob_occ_SC_predicted_with_CIdata2001_2003_60_percent.asc", gz = FALSE)
  # plot predictions 
  prob_occ_predicted_with_CIdata2001_2003_60_percent <- read.asc("prob_occ_SC_predicted_with_CIdata2001_2003_60_percent.asc")
  raster_prob_occ_predicted_with_CIdata2001_2003_60_percent <- raster(prob_occ_predicted_with_CIdata2001_2003_60_percent)
  plot(raster_prob_occ_predicted_with_CIdata2001_2003_60_percent)
  
  # 2005_70%
  beta0 <- -0.290793
  beta_elev <- -0.006944
  beta_2005_70 <- -2.153408
  # manually predict 
  prob_occ_predicted_with_CIdata2001_2003_70_percent <- 1/(1+exp(-beta0-beta_elev*dem-beta_2005_70*Year_Raster))
  write.asc(prob_occ_predicted_with_CIdata2001_2003_70_percent, "prob_occ_SC_predicted_with_CIdata2001_2003_70_percent.asc", gz = FALSE)
  # plot predictions 
  prob_occ_predicted_with_CIdata2001_2003_70_percent <- read.asc("prob_occ_SC_predicted_with_CIdata2001_2003_70_percent.asc")
  raster_prob_occ_predicted_with_CIdata2001_2003_70_percent <- raster(prob_occ_predicted_with_CIdata2001_2003_70_percent)
  plot(raster_prob_occ_predicted_with_CIdata2001_2003_70_percent)
  
  # 2005_80%
  beta0 <- -0.210844
  beta_elev <- -0.007493
  beta_2005_80 <- -2.196235
  # manually predict 
  prob_occ_predicted_with_CIdata2001_2003_80_percent <- 1/(1+exp(-beta0-beta_elev*dem-beta_2005_80*Year_Raster))
  write.asc(prob_occ_predicted_with_CIdata2001_2003_80_percent, "prob_occ_SC_predicted_with_CIdata2001_2003_80_percent.asc", gz = FALSE)
  # plot predictions 
  prob_occ_predicted_with_CIdata2001_2003_80_percent <- read.asc("prob_occ_SC_predicted_with_CIdata2001_2003_80_percent.asc")
  raster_prob_occ_predicted_with_CIdata2001_2003_80_percent <- raster(prob_occ_predicted_with_CIdata2001_2003_80_percent)
  plot(raster_prob_occ_predicted_with_CIdata2001_2003_80_percent)
  
  # 2005_90%
  beta0 <- -0.1780598
  beta_elev <- -0.0075263
  beta_2005_90 <- -2.2416352
  # manually predict 
  prob_occ_predicted_with_CIdata2001_2003_90_percent <- 1/(1+exp(-beta0-beta_elev*dem-beta_2005_90*Year_Raster))
  write.asc(prob_occ_predicted_with_CIdata2001_2003_90_percent, "prob_occ_SC_predicted_with_CIdata2001_2003_90_percent.asc", gz = FALSE)
  # plot predictions 
  prob_occ_predicted_with_CIdata2001_2003_90_percent <- read.asc("prob_occ_SC_predicted_with_CIdata2001_2003_90_percent.asc")
  raster_prob_occ_predicted_with_CIdata2001_2003_90_percent <- raster(prob_occ_predicted_with_CIdata2001_2003_90_percent)
  plot(raster_prob_occ_predicted_with_CIdata2001_2003_90_percent)
  
  # 2005_100%
  beta0 <- -0.1917566
  beta_elev <- -0.0072963
  beta_2005_100 <- -2.1137720
  # manually predict 
  prob_occ_predicted_with_CIdata2001_2003_100_percent <- 1/(1+exp(-beta0-beta_elev*dem-beta_2005_100*Year_Raster))
  write.asc(prob_occ_predicted_with_CIdata2001_2003_100_percent, "prob_occ_SC_predicted_with_CIdata2001_2003_100_percent.asc", gz = FALSE)
  # plot predictions 
  prob_occ_predicted_with_CIdata2001_2003_100_percent <- read.asc("prob_occ_SC_predicted_with_CIdata2001_2003_100_percent.asc")
  raster_prob_occ_predicted_with_CIdata2001_2003_100_percent <- raster(prob_occ_predicted_with_CIdata2001_2003_100_percent)
  plot(raster_prob_occ_predicted_with_CIdata2001_2003_100_percent)
  
  
  
  
  
  
  
  
##################################### Predict when Year is included as continuous variable ##################################################
percentage_of_total_survey_budget <- c(seq(10, 100, 10))
cumulative_surveyed_years <- c("2001_2003", "2001_2003_2005", "2001_2003_2005", "2001_2003_2005_2007",
                                     "2001_2003_2005_2007_2009","2001_2003_2005_2007_2009_2011", 
                                     "2001_2003_2005_2007_2009_2011_2013")
      
load(".Rda")
prediction_Elev_Year <- predict(RasterStack_Elev_Year, model_fit, type="response", na.rm=TRUE)
plot(prediction_Elev_Year, main=paste("")) 
write.asc(prediction_Elev_Year, "")





#################################### Predict using models fitted to 25% of the data ######################################################
## MODEL 1: predict based on vegetation only
prediction_veg_2001_25 <- predict(vegetation_raster, glm_veg_2001_25, type="response", na.rm=TRUE)


## MODEL 2: predict based on elevation only 
prediction_elev_2001_25 <- predict(DEM, model_fit, type="response", na.rm=TRUE)


## MODEL 3: predict based on elevation and vegetation
prediction_veg_elev_2003_25 <- predict(RasterStack, glm_veg_elev_2001_25, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2003_25, main="veg_elev_2003_25")
prediction_veg_elev_2005_25 <- predict(RasterStack, glm_veg_elev_2001_2003_25, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2005_25, main="veg_elev_2005_25")
prediction_veg_elev_2007_25 <- predict(RasterStack, glm_veg_elev_2001_2003_2005_25, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2007_25, main="veg_elev_2007_25")
prediction_veg_elev_2009_25 <- predict(RasterStack, glm_veg_elev_2001_2003_2005_2007_25, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2009_25, main="veg_elev_2009_25")
prediction_veg_elev_2011_25 <- predict(RasterStack, glm_veg_elev_2001_2003_2005_2007_2009_25, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2011_25, main="veg_elev_2011_25")
prediction_veg_elev_2013_25 <- predict(RasterStack, glm_veg_elev_2001_2003_2005_2007_2009_2011_25, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2013_25, main="veg_elev_2013_25")
prediction_veg_elev_2015_25 <- predict(RasterStack, glm_veg_elev_2001_2003_2005_2007_2009_2011_2013_25, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2015_25, main="veg_elev_2015_25")


## MODEL 4: predict based on vegetation, elevation and year 
# A different RasterStack has to be made for each year if year is inluded in the model as a categorised variable 
# If it is a continuous variable, one value for year can be used across all predictions (for each year)
prediction_elev_veg_year_2005_25 <- predict(RasterStack_year_2005, glm_DEM_veg_year_2001_2003_25, type="response", na.rm=TRUE)
plot(prediction_elev_veg_year_2005_25)


#################################### Predict using models fitted to 50% of the data ######################################################

## MODEL 3: predict based on elevation and vegetation
prediction_veg_elev_2003_50 <- predict(RasterStack, glm_veg_elev_2001_50, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2003_50, main="veg_elev_2003_50")
prediction_veg_elev_2005_50 <- predict(RasterStack, glm_veg_elev_2001_2003_50, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2005_50, main="veg_elev_2005_50")
prediction_veg_elev_2007_50 <- predict(RasterStack, glm_veg_elev_2001_2003_2005_50, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2007_50, main="veg_elev_2007_50")
prediction_veg_elev_2009_50 <- predict(RasterStack, glm_veg_elev_2001_2003_2005_2007_50, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2009_50, main="veg_elev_2009_50")
prediction_veg_elev_2011_50 <- predict(RasterStack, glm_veg_elev_2001_2003_2005_2007_2009_50, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2011_50, main="veg_elev_2011_50")
prediction_veg_elev_2013_50 <- predict(RasterStack, glm_veg_elev_2001_2003_2005_2007_2009_2011_50, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2013_50, main="veg_elev_2013_50")
prediction_veg_elev_2015_50 <- predict(RasterStack, glm_veg_elev_2001_2003_2005_2007_2009_2011_2013_50, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2015_50, main="veg_elev_2015_50")


#################################### Predict using models fitted to 75% of the data ######################################################

## MODEL 3: predict based on elevation and vegetation
prediction_veg_elev_2003_75 <- predict(RasterStack, glm_veg_elev_2001_75, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2003_75, main="veg_elev_2003_75")
prediction_veg_elev_2005_75 <- predict(RasterStack, glm_veg_elev_2001_2003_75, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2005_75, main="veg_elev_2005_75")
prediction_veg_elev_2007_75 <- predict(RasterStack, glm_veg_elev_2001_2003_2005_75, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2007_75, main="veg_elev_2007_75")
prediction_veg_elev_2009_75 <- predict(RasterStack, glm_veg_elev_2001_2003_2005_2007_75, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2009_75, main="veg_elev_2009_75")
prediction_veg_elev_2011_75 <- predict(RasterStack, glm_veg_elev_2001_2003_2005_2007_2009_75, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2011_75, main="veg_elev_2011_75")
prediction_veg_elev_2013_75 <- predict(RasterStack, glm_veg_elev_2001_2003_2005_2007_2009_2011_75, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2013_75, main="veg_elev_2013_75")
prediction_veg_elev_2015_75 <- predict(RasterStack, glm_veg_elev_2001_2003_2005_2007_2009_2011_2013_75, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2015_75, main="veg_elev_2015_75")


#################################### Predict using models fitted to 100% of the data ######################################################

## MODEL 3: predict based on elevation and vegetation
prediction_veg_elev_2003_100 <- predict(RasterStack, glm_veg_elev_2001_100, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2003_100, main="veg_elev_2003_100")
prediction_veg_elev_2005_100 <- predict(RasterStack, glm_veg_elev_2001_2003_100, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2005_100, main="veg_elev_2005_100")
prediction_veg_elev_2007_100 <- predict(RasterStack, glm_veg_elev_2001_2003_2005_100, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2007_100, main="veg_elev_2007_100")
prediction_veg_elev_2009_100 <- predict(RasterStack, glm_veg_elev_2001_2003_2005_2007_100, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2009_100, main="veg_elev_2009_100")
prediction_veg_elev_2011_100 <- predict(RasterStack, glm_veg_elev_2001_2003_2005_2007_2009_100, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2011_100, main="veg_elev_2011_100")
prediction_veg_elev_2013_100 <- predict(RasterStack, glm_veg_elev_2001_2003_2005_2007_2009_2011_100, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2013_100, main="veg_elev_2013_100")
prediction_veg_elev_2015_100 <- predict(RasterStack, glm_veg_elev_2001_2003_2005_2007_2009_2011_2013_100, type="response", na.rm=TRUE)
plot(prediction_veg_elev_2015_100, main="veg_elev_2015_100")