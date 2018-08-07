### LOADING PACKAGES ########################################################################################################
# Load some needed packages 
library(lme4)
library(arm)
library(raster)
library(rgdal)
library(foreign)
library(sp)
library(maptools)
library(caret) # for model evaluation and diagnostics
library(lmtest) 
library(survey)
library(MuMIn)
library(blockCV)
library(dismo)

## LOAD DATA 
CIdata_multi_visits <- read.csv("CIdata_multi_visits.csv", header=TRUE)

## REMOVE NAs IN FULL DATASET 
tmpID <- which(is.na(CIdata_multi_visits$Vegetation)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata_multi_visits <- CIdata_multi_visits[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata_multi_visits$Vegetation)) # check if NAs dissappeared 

tmpID <- which(is.na(CIdata_multi_visits$Elevation)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata_multi_visits <- CIdata_multi_visits[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata_multi_visits$Elevation)) # check if NAs dissappeared 

tmpID <- which(is.na(CIdata_multi_visits$geology_collapsed)) # check where NAs (rows_to_remove)
if (length(tmpID) > 0) CIdata_multi_visits <- CIdata_multi_visits[-tmpID, ] # remove rows with NA in vegetation column
which(is.na(CIdata_multi_visits$geology_collapsed)) # check if NAs dissappeared 

tmpID <- which(is.na(CIdata_multi_visits$DistanceFromRoad))
if (length(tmpID) > 0) CIdata_multi_visits <- CIdata_multi_visits[-tmpID, ]
which(is.na(CIdata_multi_visits$DistanceFromRoad))

## Standardise full dataset 
CIdata_multi_visits$sd_elev <- scale(CIdata_multi_visits$Elevation)
CIdata_multi_visits$sd_DFR <- scale(CIdata_multi_visits$DistanceFromRoad)
CIdata_multi_visits$Year <- factor(CIdata_multi_visits$Year)
CIdata_multi_visits$Vegetation <- factor(CIdata_multi_visits$Vegetation)
CIdata_multi_visits$geology_collapsed <- factor(CIdata_multi_visits$geology_collapsed)

## Split CIdata_multi_visits dataframe into different datasets, one for each year, now with updated p_a_SC 
CIdata2001 <- subset(CIdata_multi_visits,CIdata_multi_visits$Year == 2001)
CIdata2003 <- subset(CIdata_multi_visits,CIdata_multi_visits$Year == 2003)
CIdata2005 <- subset(CIdata_multi_visits,CIdata_multi_visits$Year == 2005)
CIdata2007 <- subset(CIdata_multi_visits,CIdata_multi_visits$Year == 2007)
CIdata2009 <- subset(CIdata_multi_visits,CIdata_multi_visits$Year == 2009)
CIdata2011 <- subset(CIdata_multi_visits,CIdata_multi_visits$Year == 2011)
CIdata2013 <- subset(CIdata_multi_visits,CIdata_multi_visits$Year == 2013)
CIdata2015 <- subset(CIdata_multi_visits,CIdata_multi_visits$Year == 2015)

# Create datasets to train the model, excluding one year
CIdata_without_2001 <- subset(CIdata_multi_visits, !(Year == 2001))
CIdata_without_2003 <- subset(CIdata_multi_visits, !(Year == 2003))
CIdata_without_2005 <- subset(CIdata_multi_visits, !(Year == 2005))
CIdata_without_2007 <- subset(CIdata_multi_visits, !(Year == 2007))
CIdata_without_2009 <- subset(CIdata_multi_visits, !(Year == 2009))
CIdata_without_2011 <- subset(CIdata_multi_visits, !(Year == 2011))
CIdata_without_2013 <- subset(CIdata_multi_visits, !(Year == 2013))
CIdata_without_2015 <- subset(CIdata_multi_visits, !(Year == 2015))

write.table(CIdata_without_2001, file="CIdata_without_2001.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2003, file="CIdata_without_2003.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2005, file="CIdata_without_2005.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2007, file="CIdata_without_2007.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2009, file="CIdata_without_2009.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2011, file="CIdata_without_2011.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2013, file="CIdata_without_2013.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2015, file="CIdata_without_2015.txt", sep="\t",col.names=T,row.names=F)



######################################################## Final model - glm #######################################################
years <- c("2001","2003","2005","2007","2009","2011","2013","2015") # create vector containing years for which we have data
multi_year_datasets_excluding_one_year <- vector() # declare vector to store datasets with the test dataset left out (training datasets = all years without one)
test_datasets <- vector()
AUC_list <- list() # to store AUC values 
AUC_vector <- vector()

for (y in years){
  add_year <- paste("CIdata_without_",y,sep="")
  multi_year_datasets_excluding_one_year <- c(multi_year_datasets_excluding_one_year, add_year)
  add_year_CIdata <- paste("CIdata",y,sep="")
  test_datasets <- c(test_datasets, add_year_CIdata)
}

# set up for dredge function -> this makes sure that datasets with NA are removed, so all models are fit to 
# same datasets. This is needed because it is impossible to compare AICs obtained from models that are trained
# with different datasets (e.g. when a site would be removed because it contains NA for a specific variable)
options(na.action=na.fail)

# loop through all training datasets
for (k in 1:length(multi_year_datasets_excluding_one_year)) {
  dataset <- get(multi_year_datasets_excluding_one_year[k])
  model <- glm(p_a_SC ~ Vegetation + sd_elev + geology_collapsed + sd_DFR, data=dataset, family=binomial)
  x <- dredge(model, beta="none") # model selection
  #save(x, file=paste("AIC_ranked_YEAR_",multi_year_datasets_excluding_one_year[k],".Rda",sep=""))
  best.model <- (get.models(x, 1)[[1]]) # only keep best model
  #save(best.model, file=paste("model_YEAR_",multi_year_datasets_excluding_one_year[k],".Rda", sep=""))
  if (k == 1) {
    test_ds <- get(test_datasets[k])
    test_ds$Year <- as.factor(2003)
  }
    else {
      test_ds <- get(test_datasets[k])
      test_year <- get(test_datasets[k-1])
      test_ds$Year <- as.factor(test_year$Year[1])
    }
      pres <- test_ds[test_ds[ ,"p_a_SC"]==1, ]
      abs <- test_ds[test_ds[ ,"p_a_SC"]==0, ]
      e <- evaluate(pres,abs,best.model)
      AUC_list[[k]] <- e
      AUC_vector[k] <- e@auc
      #pdf(paste("AUC_model_YEAR_",multi_year_datasets_excluding_one_year[k],".pdf", sep=""))
      par(mfrow=c(2,2))
      plot(e, "ROC")
      #boxplot(e)
      #density(e)
}

# Calculate AUC range 
mean_AUC <- mean(AUC_vector)
se_AUC <- sd(AUC_vector)/sqrt(length(AUC_vector))
mean_AUC
se_AUC





# CROSS VALIDATION wITH SPATIAL STRATIFICATION 
# The way I see it (but discuss with David W): 
# run models (8) with all data except one year (loop through multi_year_datasets_without_year)
# and calculate AUC for these models with spatial stratification of that multi_year_dataset.
# I am not sure however, how to do the spatial stratification, as the SPDF is made of
# unique coords, whereas the multiyear dataset consists of multiple data with same coords 

YCA_LL <- cbind(CIdata2001$avg_X_MARK, CIdata2001$avg_Y_MARK)

YCA_SP <- SpatialPoints(coords = YCA_LL, proj4string = CRS("+proj=longlat +datum=WGS84"))

YCA_blockCV <- spatialBlock(speciesData = YCA_SP,
                            rows = 5,
                            cols = 10,
                            k = 5)

YCA_LL <- unique(cbind(CIdata_multi_visits$avg_X_MARK, CIdata_multi_visits$avg_Y_MARK)) 


for(i in 1:5){
  
  fold_id <- paste0("Fold",i, collapse = "")          # set fold id
  
  train_id <- YCA_blockCV$folds[[i]][[1]]            # Training site ids
  test_id <- YCA_blockCV$folds[[i]][[2]]             # Testing site ids
  
  tmp_y_test <- CIdata2001$avg_Y_MARK[test_id, ]                     # y test data
  tmp_y_train <- CIdata2001$avg_Y_MARK[train_id, ]                   # y train data
  
  tmp_X_test <- CIdata2001$avg_X_MARK[test_id, ]                     # X test data
  tmp_X_train <- CIdata2001$avg_X_MARK[train_id, ]                   # X train data
  
  tmp_LL_test <- YCA_LL[test_id, ]                   # Lat/long test data
  tmp_LL_train <- YCA_LL[train_id, ]                 # Lat/long train data
  
  y_test_filename <- sprintf('y_YCA_%s_test_spatial.csv', fold_id)     # test filename
  y_train_filename <- sprintf('y_YCA_%s_train_spatial.csv', fold_id)   # train filename
  
  X_test_filename <- sprintf('X_YCA_%s_test_spatial.csv', fold_id)     # test filename
  X_train_filename <- sprintf('X_YCA_%s_train_spatial.csv', fold_id)   # train filename
  
  LL_test_filename <- sprintf('LL_YCA_%s_test_spatial.csv', fold_id)   # test filename
  LL_train_filename <- sprintf('LL_YCA_%s_train_spatial.csv', fold_id) # train filename
  
  write.csv(tmp_y_test, y_test_filename)              # write y fold i test csv
  write.csv(tmp_y_train, y_train_filename)            # write y fold i train csv
  
  write.csv(tmp_X_test, X_test_filename)              # write X fold i test csv
  write.csv(tmp_X_train, X_train_filename)            # write X fold i train csv
  
  write.csv(tmp_LL_test, LL_test_filename)            # write LL fold i test csv
  write.csv(tmp_LL_train, LL_train_filename)          # write LL fold i train csv
  
}


## David's code 
for(i in 1:5){
  
  fold_id <- paste0("Fold",i, collapse = "")          # set fold id
  
  train_id <- Bird_blockCV$folds[[i]][[1]]            # Training site ids
  test_id <- Bird_blockCV$folds[[i]][[2]]             # Testing site ids
  
  tmp_y_test <- Bird_y[test_id, ]                     # y test data
  tmp_y_train <- Bird_y[train_id, ]                   # y train data
  
  tmp_X_test <- Bird_X[test_id, ]                     # X test data
  tmp_X_train <- Bird_X[train_id, ]                   # X train data
  
  tmp_LL_test <- Bird_LL[test_id, ]                   # Lat/long test data
  tmp_LL_train <- Bird_LL[train_id, ]                 # Lat/long train data
  
  y_test_filename <- sprintf('Data/Birds/y_Bird_%s_test_spatial.csv', fold_id)     # test filename
  y_train_filename <- sprintf('Data/Birds/y_Bird_%s_train_spatial.csv', fold_id)   # train filename
  
  X_test_filename <- sprintf('Data/Birds/X_Bird_%s_test_spatial.csv', fold_id)     # test filename
  X_train_filename <- sprintf('Data/Birds/X_Bird_%s_train_spatial.csv', fold_id)   # train filename
  
  LL_test_filename <- sprintf('Data/Birds/LL_Bird_%s_test_spatial.csv', fold_id)   # test filename
  LL_train_filename <- sprintf('Data/Birds/LL_Bird_%s_train_spatial.csv', fold_id) # train filename
  
  write.csv(tmp_y_test, y_test_filename)              # write y fold i test csv
  write.csv(tmp_y_train, y_train_filename)            # write y fold i train csv
  
  write.csv(tmp_X_test, X_test_filename)              # write X fold i test csv
  write.csv(tmp_X_train, X_train_filename)            # write X fold i train csv
  
  write.csv(tmp_LL_test, LL_test_filename)            # write LL fold i test csv
  write.csv(tmp_LL_train, LL_train_filename)          # write LL fold i train csv
  
}


















############# Archive ########################################################################################################################
############################################################# GAM TO EXPLORE DATA ############################################################
# Elevation
gam_elev <- gam(p_a_SC ~ s(Elevation), data=CIdata, family=binomial)
plot.gam(gam_elev, se=TRUE, shade=TRUE, col="blue")
# Distance From Road 
gam_distancefromroad <- gam(p_a_SC ~ s(DistanceFromRoad), data=CIdata, family=binomial)
plot.gam(gam_distancefromroad, se=TRUE, col="red")


################################################################# GLM - ALL DATA ##############################################################
# VEGETATION
glm_veg <- glm(p_a_SC ~ Vegetation, data=CIdata, family=binomial) # run glm
glm_veg # show model
save(glm_veg,file="glm_veg.Rda")
summary(glm_veg) # summary of the model
plot(CIdata$Vegetation, CIdata$p_a_SC, xlab = "Vegetation", ylab = "Probability of occurrence of supercolony")
points(CIdata$Vegetation, fitted(glm_veg), col= "red")

# ELEVATION
glm_elev <- glm(p_a_SC ~ sd_elevation, data=CIdata, family=binomial) # run glm
glm_elev # show model
save(glm_elev,file="glm_sd_elev.Rda")
summary(glm_elev) # summary of the model
plot(CIdata$sd_elevation, CIdata$p_a_SC, xlab="SD_Elevation", ylab="Probability of occurrence of supercolony") # plot data
points(CIdata$sd_elevation, fitted(glm_elev),col = 'red') # plot response curve
plot(CIdata$sd_elevation, fitted(glm_elev),col = 'red') # plot only response curve

# Model with combined predictors vegetation and elevation 
glm_DEM_veg <- glm(p_a_SC ~ Vegetation + sd_elevation, data = CIdata, family = binomial)
glm_DEM_veg
save(glm_DEM_veg, file="glm_sd_DEM_veg.Rda")
summary(glm_DEM_veg)

# Model with year included as a fixed effect
glm_DEM_veg_year <- glm(p_a_SC ~ Vegetation + sd_elevation + Year, data=CIdata, family = binomial)
glm_DEM_veg_year
save(glm_DEM_veg_year, file="glm_sd_DEM_veg_year.Rda")
summary(glm_DEM_veg_year)

# Model with distance from road, elevation and Year as predictor, without vegetation 
glm_elev_dist_year <- glm(p_a_SC ~ sd_elevation + sd_distancefromroad + Year, data=CIdata, family = binomial)
summary(glm_elev_dist_year)
save(glm_elev_dist_year, file="glm_elev_dist_year.Rda")

glm_elev_dist_year_unstandardized <- glm(p_a_SC ~ Elevation + DistanceFromRoad + Year, data=CIdata, family = binomial)
summary(glm_elev_dist_year_unstandardized)
save(glm_elev_dist_year_unstandardized, file="glm_elev_dist_year_unstandardized.Rda")
# add "-1" to the equation to compare with glmm
glm_elev_dist_year_glmm <- glm(p_a_SC ~ sd_elevation + sd_distancefromroad + Year - 1, data=CIdata, family = binomial)
summary(glm_elev_dist_year_glmm)
save(glm_elev_dist_year_glmm, file="glm_elev_dist_year_glmm.Rda")

# Model with distance from road, elevation, rainfall and Year as predictors (fixed effects)
glm_elev_dist_rainfall_year <- glm(p_a_SC ~ Rainfall + Year, data=CIdata, family = binomial)
glm_elev_dist_PreviousYearRainfall_year <- glm(p_a_SC ~ Elevation + DistanceFromRoad + Rainfall_previous_year + Year, 
                                               data = CIdata, family = binomial)
summary(glm_elev_dist_rainfall_year)
summary(glm_elev_dist_PreviousYearRainfall_year)

save(glm_elev_dist_rainfall_year, file="glm_elev_dist_rainfall_year.Rda")
save(glm_elev_dist_PreviousYearRainfall_year, file="glm_elev_dist_RainfallPreviousYear_year.Rda")

glm_elev_dist_rainfall <- glm(p_a_SC ~ Elevation + DistanceFromRoad + Rainfall, data=CIdata, family = binomial)
summary(glm_elev_dist_rainfall)
save(glm_elev_dist_rainfall, file="glm_elev_dist_rainfall.Rda")

# Model without 2015 including geology 
glm_test <- glm(p_a_SC ~ Elevation + DistanceFromRoad + Year, data=CIdata_without_2015, family = binomial) 
glm_Year_as_cont <- glm(p_a_SC ~ sd_elevation + Vegetation + sd_rainfall_meters, data=CIdata, family=binomial)

glm_veg_geo_elev_YEAR <- glm(p_a_SC ~ sd_elevation + Vegetation + geology + Year, data=CIdata, family=binomial)
summary(glm_veg_geo_elev_YEAR)
save(glm_veg_geo_elev_YEAR, file="glm_veg_geo_elev_YEAR.Rda")

###################################################### GLMM ##########################################################################
# Year is included as random effect 
# This is basically the same as running a glm with Year included as fixed effect 
# since that will result in a variable intercept 
glmm_elev_dist_year <- glmer(p_a_SC ~ sd_elevation + sd_distancefromroad + (1|Year), data=CIdata, family=binomial)
summary(glmm_elev_dist_year)
save(glmm_elev_dist_year, file="glmm_elev_dist_year.Rda")

###################################################### TEST #############################################################################
glm_1 <- glm(p_a_SC ~ sd_elevation + Vegetation + sd_rainfall_meters + Year, data=CIdata, family=binomial)
glm_2 <- glm(p_a_SC ~ sd_elevation + sd_rainfall_meters + Year, data=CIdata, family=binomial)

glmm_1 <- glmer(p_a_SC ~ sd_elevation + sd_rainfall_meters + (1|Year), data=CIdata, family=binomial)
summary(glmm_1)
save(glmm_1, file ="glmm_elev_RF.Rda")

## likelihood ratio test 
anova(glm_1, glmm_1, test="LRT")
## The likelihood ratio test can also be used for predictors in 1 model
anova(glm_1, test="LRT")





############################# standardising variables in separe datasets #####################
# Standardize continuous variables using correct dataset 
CIdata_without_2001$sd_elev <- scale(CIdata_without_2001$Elevation)
CIdata_without_2003$sd_elev <- scale(CIdata_without_2003$Elevation)
CIdata_without_2005$sd_elev <- scale(CIdata_without_2005$Elevation)
CIdata_without_2007$sd_elev <- scale(CIdata_without_2007$Elevation)
CIdata_without_2009$sd_elev <- scale(CIdata_without_2009$Elevation)
CIdata_without_2011$sd_elev <- scale(CIdata_without_2011$Elevation)
CIdata_without_2013$sd_elev <- scale(CIdata_without_2013$Elevation)
CIdata_without_2015$sd_elev <- scale(CIdata_without_2015$Elevation)

CIdata_without_2001$sd_DFR <- scale(CIdata_without_2001$DistanceFromRoad)
CIdata_without_2003$sd_DFR <- scale(CIdata_without_2003$DistanceFromRoad)
CIdata_without_2005$sd_DFR <- scale(CIdata_without_2005$DistanceFromRoad)
CIdata_without_2007$sd_DFR <- scale(CIdata_without_2007$DistanceFromRoad)
CIdata_without_2009$sd_DFR <- scale(CIdata_without_2009$DistanceFromRoad)
CIdata_without_2011$sd_DFR <- scale(CIdata_without_2011$DistanceFromRoad)
CIdata_without_2013$sd_DFR <- scale(CIdata_without_2013$DistanceFromRoad)
CIdata_without_2015$sd_DFR <- scale(CIdata_without_2015$DistanceFromRoad)

# Factorize categorical variables 
CIdata_without_2001$Year <- factor(CIdata_without_2001$Year)
CIdata_without_2003$Year <- factor(CIdata_without_2003$Year)
CIdata_without_2005$Year <- factor(CIdata_without_2005$Year)
CIdata_without_2007$Year <- factor(CIdata_without_2007$Year)
CIdata_without_2009$Year <- factor(CIdata_without_2009$Year)
CIdata_without_2011$Year <- factor(CIdata_without_2011$Year)
CIdata_without_2013$Year <- factor(CIdata_without_2013$Year)
CIdata_without_2015$Year <- factor(CIdata_without_2015$Year)

CIdata_without_2001$Vegetation <- factor(CIdata_without_2001$Vegetation)
CIdata_without_2003$Vegetation <- factor(CIdata_without_2003$Vegetation)
CIdata_without_2005$Vegetation <- factor(CIdata_without_2005$Vegetation)
CIdata_without_2007$Vegetation <- factor(CIdata_without_2007$Vegetation)
CIdata_without_2009$Vegetation <- factor(CIdata_without_2009$Vegetation)
CIdata_without_2011$Vegetation <- factor(CIdata_without_2011$Vegetation)
CIdata_without_2013$Vegetation <- factor(CIdata_without_2013$Vegetation)
CIdata_without_2015$Vegetation <- factor(CIdata_without_2015$Vegetation)

CIdata_without_2001$geology_collapsed <- factor(CIdata_without_2001$geology_collapsed)
CIdata_without_2003$geology_collapsed <- factor(CIdata_without_2003$geology_collapsed)
CIdata_without_2005$geology_collapsed <- factor(CIdata_without_2005$geology_collapsed)
CIdata_without_2007$geology_collapsed <- factor(CIdata_without_2007$geology_collapsed)
CIdata_without_2009$geology_collapsed <- factor(CIdata_without_2009$geology_collapsed)
CIdata_without_2011$geology_collapsed <- factor(CIdata_without_2011$geology_collapsed)
CIdata_without_2013$geology_collapsed <- factor(CIdata_without_2013$geology_collapsed)
CIdata_without_2015$geology_collapsed <- factor(CIdata_without_2015$geology_collapsed)









