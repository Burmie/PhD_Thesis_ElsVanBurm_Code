### Fit a habitat suitability model (glm) to each dataset to use further for management
## Load required packages 
library(ROCR)
library(MuMIn)
library(dismo)
library(ResourceSelection) # to run a Hosmer-Lemeshow goodness of fit test 

## Declare AUC container 
AUC <- matrix(NA, nrow=100, ncol=8)
rownames(AUC) <- 1:100
colnames(AUC) <- c("2005","2007", "2009", "2011", "2013", "2015","mean", "SE")

## Load the list with different datasets - run data preparation script before this so the datasets are loaded in R memory
### Data 2001_2003
## Data preparation - remove NAs and factorize categorical variables 
for (k in 1:100) {
  tmpID <- which(is.na(CIdata2001_2003_list[[k]]$Vegetation)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2001_2003_list[[k]] <- CIdata2001_2003_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2001_2003_list[[k]]$Vegetation)) # check if NAs dissappeared
  CIdata2001_2003_list[[k]]$Vegetation <- factor(CIdata2001_2003_list[[k]]$Vegetation) # factorize categorical variable to use in the model and overwrite data with factorized vegetation
  print(class(CIdata2001_2003_list[[k]]$Vegetation)) # check if factorization worked
  
  tmpID <- which(is.na(CIdata2001_2003_list[[k]]$sd_elev)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2001_2003_list[[k]] <- CIdata2001_2003_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2001_2003_list[[k]]$sd_elev)) # check if NAs dissappeared
  
  tmpID <- which(is.na(CIdata2001_2003_list[[k]]$sd_DFR)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2001_2003_list[[k]] <- CIdata2001_2003_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2001_2003_list[[k]]$sd_DFR)) # check if NAs dissappeared
  
  CIdata2001_2003_list[[k]]$Year <- factor(CIdata2001_2003_list[[k]]$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
  print(class(CIdata2001_2003_list[[k]]$Year)) # check if factorization worked
  
  tmpID <- which(is.na(CIdata2001_2003_list[[k]]$geology_collapsed)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2001_2003_list[[k]] <- CIdata2001_2003_list[[k]][-tmpID, ] # remove rows with NA in geology_collapsed column
  which(is.na(CIdata2001_2003_list[[k]]$geology_collapsed)) # check if NAs dissappeared
  CIdata2001_2003_list[[k]]$geology_collapsed <- factor(CIdata2001_2003_list[[k]]$geology_collapsed) # factorize geology_collapsed levels 

  tmpID <- which(is.na(CIdata2005_list[[k]]$Vegetation)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2005_list[[k]] <- CIdata2005_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2005_list[[k]]$Vegetation)) # check if NAs dissappeared
  CIdata2005_list[[k]]$Vegetation <- factor(CIdata2005_list[[k]]$Vegetation) # factorize categorical variable to use in the model and overwrite data with factorized vegetation
  print(class(CIdata2005_list[[k]]$Vegetation)) # check if factorization worked
  
  tmpID <- which(is.na(CIdata2005_list[[k]]$sd_elev)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2005_list[[k]] <- CIdata2005_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2005_list[[k]]$sd_elev)) # check if NAs dissappeared
  
  tmpID <- which(is.na(CIdata2005_list[[k]]$sd_DFR)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2005_list[[k]] <- CIdata2005_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2005_list[[k]]$sd_DFR)) # check if NAs dissappeared
  
  CIdata2005_list[[k]]$Year <- factor(CIdata2005_list[[k]]$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
  print(class(CIdata2005_list[[k]]$Year)) # check if factorization worked
  
  tmpID <- which(is.na(CIdata2005_list[[k]]$geology_collapsed)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2005_list[[k]] <- CIdata2005_list[[k]][-tmpID, ] # remove rows with NA in geology_collapsed column
  which(is.na(CIdata2005_list[[k]]$geology_collapsed)) # check if NAs dissappeared
  CIdata2005_list[[k]]$geology_collapsed <- factor(CIdata2005_list[[k]]$geology_collapsed) # factorize geology_collapsed levels 
}

## Run model 
options(na.action=na.fail) # set up for dredge function 
# VEGETATION, SD_ELEVATION, GEOLOGY_COLLAPSED, SD_DISTANCE_FROM_ROAD AND YEAR
for (k in 1:100) { # for 1 - 100% of sites 
  model_fit <- glm(p_a_SC ~ Vegetation + sd_elev + geology_collapsed + sd_DFR + Year, data=CIdata2001_2003_list[[k]], family=binomial) # run glm 
  #save(model_fit, file=paste("glm_output_VEG_sd_ELEV_geology_collapsed_sd_DISTFROMROAD_YEAR_CIdata2001_2003_",k,"_percent.Rda", sep="")) # save model summary
  x <- dredge(model_fit, beta="none") # model selection 
  #save(x, file=paste("AIC_ranked_CIdata2001_2003_",k,"percent.Rda",sep="")) # save ranked AICs 
  best.model <- (get.models(x, 1)[[1]]) # only keep best model 
  #save(best.model, file=paste("model_CIdata_2001_2003_",k,"_percent.Rda", sep="")) # save best model 
  CIdata_test <- CIdata2005_list[[100]] # make sure categorical variables are factorized 
  
  if (k > 3){ # because model fit to data from less than 4% of sites, results in null model being best model 
    
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
    
    CIdata_test_data_with_newlevels_NA <- missingLevelsToNA(object=best.model, data=CIdata_test)
    
    CIdata_test_data_with_newlevels_NA$Year <- as.factor(2003)
    
    pres <- CIdata_test_data_with_newlevels_NA[CIdata_test_data_with_newlevels_NA[ ,"p_a_SC"]==1, ]
    abs <- CIdata_test_data_with_newlevels_NA[CIdata_test_data_with_newlevels_NA[ ,"p_a_SC"]==0, ]
    e <- evaluate(pres,abs,best.model)
    #pdf(paste("AUC_CIdata2001_2003_",i,"_percent.pdf"))
    par(mfrow=c(2,2))
    plot(e,"ROC")
    #boxplot(e)
    #density(e)
    #dev.off()
    AUC[k, 1] <- e@auc
  }
}

### Data 2001_2003_2005
## Data preparation - remove NAs and factorize categorical variables 
for (k in 1:100) {
  tmpID <- which(is.na(CIdata2001_2003_2005_list[[k]]$Vegetation)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2001_2003_2005_list[[k]] <- CIdata2001_2003_2005_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2001_2003_2005_list[[k]]$Vegetation)) # check if NAs dissappeared 
  CIdata2001_2003_2005_list[[k]]$Vegetation <- factor(CIdata2001_2003_2005_list[[k]]$Vegetation) # factorize categorical variable to use in the model and overwrite data with factorized vegetation
  print(class(CIdata2001_2003_2005_list[[k]]$Vegetation)) # check if factorization worked
  
  tmpID <- which(is.na(CIdata2001_2003_2005_list[[k]]$sd_elev)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2001_2003_2005_list[[k]] <- CIdata2001_2003_2005_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2001_2003_2005_list[[k]]$sd_elev)) # check if NAs dissappeared 
  
  tmpID <- which(is.na(CIdata2001_2003_2005_list[[k]]$sd_DFR)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2001_2003_2005_list[[k]] <- CIdata2001_2003_2005_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2001_2003_2005_list[[k]]$sd_DFR)) # check if NAs dissappeared 
  
  CIdata2001_2003_2005_list[[k]]$Year <- factor(CIdata2001_2003_2005_list[[k]]$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
  print(class(CIdata2001_2003_2005_list[[k]]$Year)) # check if factorization worked

  tmpID <- which(is.na(CIdata2001_2003_2005_list[[k]]$geology_collapsed)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2001_2003_2005_list[[k]] <- CIdata2001_2003_2005_list[[k]][-tmpID, ] # remove rows with NA in geology_collapsed column
  which(is.na(CIdata2001_2003_2005_list[[k]]$geology_collapsed)) # check if NAs dissappeared
  CIdata2001_2003_2005_list[[k]]$geology_collapsed <- factor(CIdata2001_2003_2005_list[[k]]$geology_collapsed) # factorize geology_collapsed levels 

  tmpID <- which(is.na(CIdata2007_list[[k]]$Vegetation)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2007_list[[k]] <- CIdata2007_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2007_list[[k]]$Vegetation)) # check if NAs dissappeared
  CIdata2007_list[[k]]$Vegetation <- factor(CIdata2007_list[[k]]$Vegetation) # factorize categorical variable to use in the model and overwrite data with factorized vegetation
  print(class(CIdata2007_list[[k]]$Vegetation)) # check if factorization worked
  
  tmpID <- which(is.na(CIdata2007_list[[k]]$sd_elev)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2007_list[[k]] <- CIdata2007_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2007_list[[k]]$sd_elev)) # check if NAs dissappeared
  
  tmpID <- which(is.na(CIdata2007_list[[k]]$sd_DFR)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2007_list[[k]] <- CIdata2007_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2007_list[[k]]$sd_DFR)) # check if NAs dissappeared
  
  CIdata2007_list[[k]]$Year <- factor(CIdata2007_list[[k]]$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
  print(class(CIdata2007_list[[k]]$Year)) # check if factorization worked
  
  tmpID <- which(is.na(CIdata2007_list[[k]]$geology_collapsed)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2007_list[[k]] <- CIdata2007_list[[k]][-tmpID, ] # remove rows with NA in geology_collapsed column
  which(is.na(CIdata2007_list[[k]]$geology_collapsed)) # check if NAs dissappeared
  CIdata2007_list[[k]]$geology_collapsed <- factor(CIdata2007_list[[k]]$geology_collapsed) # factorize geology_collapsed levels 
}

## Run model 
options(na.action=na.fail) # set up for dredge function 
# VEGETATION, SD_ELEVATION, GEOLOGY_COLLAPSED, SD_DISTANCE_FROM_ROAD AND YEAR
for (k in 1:100) { # for 1 - 100% of sites 
  model_fit <- glm(p_a_SC ~ Vegetation + sd_elev + geology_collapsed + sd_DFR + Year, data=CIdata2001_2003_2005_list[[k]], family=binomial) # run glm 
  #save(model_fit, file=paste("glm_output_VEG_sd_ELEV_geology_collapsed_sd_DISTFROMROAD_YEAR_CIdata2001_2003_",k,"_percent.Rda", sep="")) # save model summary
  x <- dredge(model_fit, beta="none") # model selection 
  #save(x, file=paste("AIC_ranked_CIdata2001_2003_",k,"percent.Rda",sep="")) # save ranked AICs 
  best.model <- (get.models(x, 1)[[1]]) # only keep best model 
  #save(best.model, file=paste("model_CIdata_2001_2003_",k,"_percent.Rda", sep="")) # save best model 
  CIdata_test <- CIdata2007_list[[100]] # make sure categorical variables are factorized 
  
  if (k > 1){ # because model fit to data from less than 2% of sites, results in null model being best model 
    
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
    
    CIdata_test_data_with_newlevels_NA <- missingLevelsToNA(object=best.model, data=CIdata_test)
    
    CIdata_test_data_with_newlevels_NA$Year <- as.factor(2005)
    
    pres <- CIdata_test_data_with_newlevels_NA[CIdata_test_data_with_newlevels_NA[ ,"p_a_SC"]==1, ]
    abs <- CIdata_test_data_with_newlevels_NA[CIdata_test_data_with_newlevels_NA[ ,"p_a_SC"]==0, ]
    e <- evaluate(pres,abs,best.model)
    #pdf(paste("AUC_CIdata2001_2003_",i,"_percent.pdf"))
    par(mfrow=c(2,2))
    plot(e,"ROC")
    #boxplot(e)
    #density(e)
    #dev.off()
    AUC[k, 2] <- e@auc
  }
}




### Data 2001_2003_2005_2007
## Data preparation - remove NAs and factorize categorical variables 
for (k in 1:100) {
  tmpID <- which(is.na(CIdata2001_2003_2005_2007_list[[k]]$Vegetation)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2001_2003_2005_2007_list[[k]] <- CIdata2001_2003_2005_2007_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2001_2003_2005_2007_list[[k]]$Vegetation)) # check if NAs dissappeared 
  CIdata2001_2003_2005_2007_list[[k]]$Vegetation <- factor(CIdata2001_2003_2005_2007_list[[k]]$Vegetation) # factorize categorical variable to use in the model and overwrite data with factorized vegetation
  print(class(CIdata2001_2003_2005_2007_list[[k]]$Vegetation)) # check if factorization worked

  tmpID <- which(is.na(CIdata2001_2003_2005_2007_list[[k]]$sd_elev)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2001_2003_2005_2007_list[[k]] <- CIdata2001_2003_2005_2007_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2001_2003_2005_2007_list[[k]]$sd_elev)) # check if NAs dissappeared 
  
  tmpID <- which(is.na(CIdata2001_2003_2005_2007_list[[k]]$sd_DFR)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2001_2003_2005_2007_list[[k]] <- CIdata2001_2003_2005_2007_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2001_2003_2005_2007_list[[k]]$sd_DFR)) # check if NAs dissappeared 
  
  CIdata2001_2003_2005_2007_list[[k]]$Year <- factor(CIdata2001_2003_2005_2007_list[[k]]$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
  print(class(CIdata2001_2003_2005_2007_list[[k]]$Year)) # check if factorization worked

  tmpID <- which(is.na(CIdata2001_2003_2005_2007_list[[k]]$geology_collapsed)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2001_2003_2005_2007_list[[k]] <- CIdata2001_2003_2005_2007_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2001_2003_2005_2007_list[[k]]$geology_collapsed)) # check if NAs dissappeared 
  CIdata2001_2003_2005_2007_list[[k]]$geology_collapsed <- factor(CIdata2001_2003_2005_2007_list[[k]]$geology_collapsed)
  print(class(CIdata2001_2003_2005_2007_list[[k]]$geology_collapsed))

  tmpID <- which(is.na(CIdata2009_list[[k]]$Vegetation)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2009_list[[k]] <- CIdata2009_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2009_list[[k]]$Vegetation)) # check if NAs dissappeared
  CIdata2009_list[[k]]$Vegetation <- factor(CIdata2009_list[[k]]$Vegetation) # factorize categorical variable to use in the model and overwrite data with factorized vegetation
  print(class(CIdata2009_list[[k]]$Vegetation)) # check if factorization worked
  
  tmpID <- which(is.na(CIdata2009_list[[k]]$sd_elev)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2009_list[[k]] <- CIdata2009_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2009_list[[k]]$sd_elev)) # check if NAs dissappeared
  
  tmpID <- which(is.na(CIdata2009_list[[k]]$sd_DFR)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2009_list[[k]] <- CIdata2009_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2009_list[[k]]$sd_DFR)) # check if NAs dissappeared
  
  CIdata2009_list[[k]]$Year <- factor(CIdata2009_list[[k]]$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
  print(class(CIdata2009_list[[k]]$Year)) # check if factorization worked
  
  tmpID <- which(is.na(CIdata2009_list[[k]]$geology_collapsed)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2009_list[[k]] <- CIdata2009_list[[k]][-tmpID, ] # remove rows with NA in geology_collapsed column
  which(is.na(CIdata2009_list[[k]]$geology_collapsed)) # check if NAs dissappeared
  CIdata2009_list[[k]]$geology_collapsed <- factor(CIdata2009_list[[k]]$geology_collapsed) # factorize geology_collapsed levels 
}

## Run model 
options(na.action=na.fail) # set up for dredge function 
# VEGETATION, SD_ELEVATION, GEOLOGY_COLLAPSED, SD_DISTANCE_FROM_ROAD AND YEAR
for (k in 1:100) { # for 1 - 100% of sites 
  model_fit <- glm(p_a_SC ~ Vegetation + sd_elev + geology_collapsed + sd_DFR + Year, data=CIdata2001_2003_2005_2007_list[[k]], family=binomial) # run glm 
  #save(model_fit, file=paste("glm_output_VEG_sd_ELEV_geology_collapsed_sd_DISTFROMROAD_YEAR_CIdata2001_2003_",k,"_percent.Rda", sep="")) # save model summary
  x <- dredge(model_fit, beta="none") # model selection 
  #save(x, file=paste("AIC_ranked_CIdata2001_2003_",k,"percent.Rda",sep="")) # save ranked AICs 
  best.model <- (get.models(x, 1)[[1]]) # only keep best model 
  #save(best.model, file=paste("model_CIdata_2001_2003_",k,"_percent.Rda", sep="")) # save best model 
  CIdata_test <- CIdata2009_list[[100]] # make sure categorical variables are factorized 
  
  if (k > 4){ # because model fit to data from less than 5% of sites, results in null model being best model 
    
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
    
    CIdata_test_data_with_newlevels_NA <- missingLevelsToNA(object=best.model, data=CIdata_test)
    
    CIdata_test_data_with_newlevels_NA$Year <- as.factor(2007)
    
    pres <- CIdata_test_data_with_newlevels_NA[CIdata_test_data_with_newlevels_NA[ ,"p_a_SC"]==1, ]
    abs <- CIdata_test_data_with_newlevels_NA[CIdata_test_data_with_newlevels_NA[ ,"p_a_SC"]==0, ]
    e <- evaluate(pres,abs,best.model)
    #pdf(paste("AUC_CIdata2001_2003_",i,"_percent.pdf"))
    par(mfrow=c(2,2))
    plot(e,"ROC")
    #boxplot(e)
    #density(e)
    #dev.off()
    AUC[k, 3] <- e@auc
  }
}




### Data 2001_2003_2005_2007_2009
## Data preparation - remove NAs and factorize categorical variables 
for (k in 1:100) {
  tmpID <- which(is.na(CIdata2001_2003_2005_2007_2009_list[[k]]$Vegetation)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2001_2003_2005_2007_2009_list[[k]] <- CIdata2001_2003_2005_2007_2009_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2001_2003_2005_2007_2009_list[[k]]$Vegetation)) # check if NAs dissappeared 
  CIdata2001_2003_2005_2007_2009_list[[k]]$Vegetation <- factor(CIdata2001_2003_2005_2007_2009_list[[k]]$Vegetation) # factorize categorical variable to use in the model and overwrite data with factorized vegetation
  print(class(CIdata2001_2003_2005_2007_2009_list[[k]]$Vegetation)) # check if factorization worked
  
  tmpID <- which(is.na(CIdata2001_2003_2005_2007_2009_list[[k]]$sd_elev)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2001_2003_2005_2007_2009_list[[k]] <- CIdata2001_2003_2005_2007_2009_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2001_2003_2005_2007_2009_list[[k]]$sd_elev)) # check if NAs dissappeared 
  
  tmpID <- which(is.na(CIdata2001_2003_2005_2007_2009_list[[k]]$sd_DFR)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2001_2003_2005_2007_2009_list[[k]] <- CIdata2001_2003_2005_2007_2009_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2001_2003_2005_2007_2009_list[[k]]$sd_DFR)) # check if NAs dissappeared 
  
  CIdata2001_2003_2005_2007_2009_list[[k]]$Year <- factor(CIdata2001_2003_2005_2007_2009_list[[k]]$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
  print(class(CIdata2001_2003_2005_2007_2009_list[[k]]$Year)) # check if factorization worked
  
  tmpID <- which(is.na(CIdata2001_2003_2005_2007_2009_list[[k]]$geology_collapsed)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2001_2003_2005_2007_2009_list[[k]] <- CIdata2001_2003_2005_2007_2009_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2001_2003_2005_2007_2009_list[[k]]$geology_collapsed)) # check if NAs dissappeared 
  
  CIdata2001_2003_2005_2007_2009_list[[k]]$geology_collapsed <- factor(CIdata2001_2003_2005_2007_2009_list[[k]]$geology_collapsed)
  print(class(CIdata2001_2003_2005_2007_2009_list[[k]]$geology_collapsed))
  
  tmpID <- which(is.na(CIdata2011_list[[k]]$Vegetation)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2011_list[[k]] <- CIdata2011_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2011_list[[k]]$Vegetation)) # check if NAs dissappeared
  CIdata2011_list[[k]]$Vegetation <- factor(CIdata2011_list[[k]]$Vegetation) # factorize categorical variable to use in the model and overwrite data with factorized vegetation
  print(class(CIdata2011_list[[k]]$Vegetation)) # check if factorization worked
  
  tmpID <- which(is.na(CIdata2011_list[[k]]$sd_elev)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2011_list[[k]] <- CIdata2011_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2011_list[[k]]$sd_elev)) # check if NAs dissappeared
  
  tmpID <- which(is.na(CIdata2011_list[[k]]$sd_DFR)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2011_list[[k]] <- CIdata2011_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2011_list[[k]]$sd_DFR)) # check if NAs dissappeared
  
  CIdata2011_list[[k]]$Year <- factor(CIdata2011_list[[k]]$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
  print(class(CIdata2011_list[[k]]$Year)) # check if factorization worked
  
  tmpID <- which(is.na(CIdata2011_list[[k]]$geology_collapsed)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2011_list[[k]] <- CIdata2011_list[[k]][-tmpID, ] # remove rows with NA in geology_collapsed column
  which(is.na(CIdata2011_list[[k]]$geology_collapsed)) # check if NAs dissappeared
  CIdata2011_list[[k]]$geology_collapsed <- factor(CIdata2011_list[[k]]$geology_collapsed) # factorize geology_collapsed levels 
}

## Run model 
options(na.action=na.fail) # set up for dredge function 
# VEGETATION, SD_ELEVATION, GEOLOGY_COLLAPSED, SD_DISTANCE_FROM_ROAD AND YEAR
for (k in 1:100) { # for 1 - 100% of sites 
  model_fit <- glm(p_a_SC ~ Vegetation + sd_elev + geology_collapsed + sd_DFR + Year, data=CIdata2001_2003_2005_2007_2009_list[[k]], family=binomial) # run glm 
  #save(model_fit, file=paste("glm_output_VEG_sd_ELEV_geology_collapsed_sd_DISTFROMROAD_YEAR_CIdata2001_2003_",k,"_percent.Rda", sep="")) # save model summary
  x <- dredge(model_fit, beta="none") # model selection 
  #save(x, file=paste("AIC_ranked_CIdata2001_2003_",k,"percent.Rda",sep="")) # save ranked AICs 
  best.model <- (get.models(x, 1)[[1]]) # only keep best model 
  #save(best.model, file=paste("model_CIdata_2001_2003_",k,"_percent.Rda", sep="")) # save best model 
  CIdata_test <- CIdata2011_list[[100]] # make sure categorical variables are factorized 
  
  if (k > 4){ # because model fit to data collected at fewer than 5% of sites, results in null model being best model 
    
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
    
    CIdata_test_data_with_newlevels_NA <- missingLevelsToNA(object=best.model, data=CIdata_test)
    
    CIdata_test_data_with_newlevels_NA$Year <- as.factor(2009)
    
    pres <- CIdata_test_data_with_newlevels_NA[CIdata_test_data_with_newlevels_NA[ ,"p_a_SC"]==1, ]
    abs <- CIdata_test_data_with_newlevels_NA[CIdata_test_data_with_newlevels_NA[ ,"p_a_SC"]==0, ]
    e <- evaluate(pres,abs,best.model)
    #pdf(paste("AUC_CIdata2001_2003_",i,"_percent.pdf"))
    par(mfrow=c(2,2))
    plot(e,"ROC")
    #boxplot(e)
    #density(e)
    #dev.off()
    AUC[k, 4] <- e@auc
  }
}


### Data 2001_2003_2005_2007_2009_2011
## Data preparation - remove NAs and factorize categorical variables 
for (k in 1:100) {
  tmpID <- which(is.na(CIdata2001_2003_2005_2007_2009_2011_list[[k]]$Vegetation)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2001_2003_2005_2007_2009_2011_list[[k]] <- CIdata2001_2003_2005_2007_2009_2011_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2001_2003_2005_2007_2009_2011_list[[k]]$Vegetation)) # check if NAs dissappeared 
  CIdata2001_2003_2005_2007_2009_2011_list[[k]]$Vegetation <- factor(CIdata2001_2003_2005_2007_2009_2011_list[[k]]$Vegetation) # factorize categorical variable to use in the model and overwrite data with factorized vegetation
  print(class(CIdata2001_2003_2005_2007_2009_2011_list[[k]]$Vegetation)) # check if factorization worked

  tmpID <- which(is.na(CIdata2001_2003_2005_2007_2009_2011_list[[k]]$sd_elev)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2001_2003_2005_2007_2009_2011_list[[k]] <- CIdata2001_2003_2005_2007_2009_2011_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2001_2003_2005_2007_2009_2011_list[[k]]$sd_elev)) # check if NAs dissappeared 
  
  tmpID <- which(is.na(CIdata2001_2003_2005_2007_2009_2011_list[[k]]$sd_DFR)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2001_2003_2005_2007_2009_2011_list[[k]] <- CIdata2001_2003_2005_2007_2009_2011_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2001_2003_2005_2007_2009_2011_list[[k]]$sd_DFR)) # check if NAs dissappeared 
  
  CIdata2001_2003_2005_2007_2009_2011_list[[k]]$Year <- factor(CIdata2001_2003_2005_2007_2009_2011_list[[k]]$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
  print(class(CIdata2001_2003_2005_2007_2009_2011_list[[k]]$Year)) # check if factorization worked
  
  tmpID <- which(is.na(CIdata2001_2003_2005_2007_2009_2011_list[[k]]$geology_collapsed)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2001_2003_2005_2007_2009_2011_list[[k]] <- CIdata2001_2003_2005_2007_2009_2011_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2001_2003_2005_2007_2009_2011_list[[k]]$geology_collapsed)) # check if NAs dissappeared 
  
  CIdata2001_2003_2005_2007_2009_2011_list[[k]]$geology_collapsed <- factor(CIdata2001_2003_2005_2007_2009_2011_list[[k]]$geology_collapsed)
  print(class(CIdata2001_2003_2005_2007_2009_2011_list[[k]]$geology_collapsed))

  tmpID <- which(is.na(CIdata2013_list[[k]]$Vegetation)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2013_list[[k]] <- CIdata2013_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2013_list[[k]]$Vegetation)) # check if NAs dissappeared
  CIdata2013_list[[k]]$Vegetation <- factor(CIdata2013_list[[k]]$Vegetation) # factorize categorical variable to use in the model and overwrite data with factorized vegetation
  print(class(CIdata2013_list[[k]]$Vegetation)) # check if factorization worked
  
  tmpID <- which(is.na(CIdata2013_list[[k]]$sd_elev)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2013_list[[k]] <- CIdata2013_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2013_list[[k]]$sd_elev)) # check if NAs dissappeared
  
  tmpID <- which(is.na(CIdata2013_list[[k]]$sd_DFR)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2013_list[[k]] <- CIdata2013_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2013_list[[k]]$sd_DFR)) # check if NAs dissappeared
  
  CIdata2013_list[[k]]$Year <- factor(CIdata2013_list[[k]]$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
  print(class(CIdata2013_list[[k]]$Year)) # check if factorization worked
  
  tmpID <- which(is.na(CIdata2013_list[[k]]$geology_collapsed)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2013_list[[k]] <- CIdata2013_list[[k]][-tmpID, ] # remove rows with NA in geology_collapsed column
  which(is.na(CIdata2013_list[[k]]$geology_collapsed)) # check if NAs dissappeared
  CIdata2013_list[[k]]$geology_collapsed <- factor(CIdata2013_list[[k]]$geology_collapsed) # factorize geology_collapsed levels 
}

## Run model 
options(na.action=na.fail) # set up for dredge function 
# VEGETATION, SD_ELEVATION, GEOLOGY_COLLAPSED, SD_DISTANCE_FROM_ROAD AND YEAR
for (k in 1:100) { # for 1 - 100% of sites 
  model_fit <- glm(p_a_SC ~ Vegetation + sd_elev + geology_collapsed + sd_DFR + Year, data=CIdata2001_2003_2005_2007_2009_2011_list[[k]], family=binomial) # run glm 
  #save(model_fit, file=paste("glm_output_VEG_sd_ELEV_geology_collapsed_sd_DISTFROMROAD_YEAR_CIdata2001_2003_",k,"_percent.Rda", sep="")) # save model summary
  x <- dredge(model_fit, beta="none") # model selection 
  #save(x, file=paste("AIC_ranked_CIdata2001_2003_",k,"percent.Rda",sep="")) # save ranked AICs 
  best.model <- (get.models(x, 1)[[1]]) # only keep best model 
  #save(best.model, file=paste("model_CIdata_2001_2003_",k,"_percent.Rda", sep="")) # save best model 
  CIdata_test <- CIdata2013_list[[100]] # make sure categorical variables are factorized 
  
  if (k > 0){ # because model fit to data from less than 5% of sites, results in null model being best model 
    
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
    
    CIdata_test_data_with_newlevels_NA <- missingLevelsToNA(object=best.model, data=CIdata_test)
    
    CIdata_test_data_with_newlevels_NA$Year <- as.factor(2011)
    
    pres <- CIdata_test_data_with_newlevels_NA[CIdata_test_data_with_newlevels_NA[ ,"p_a_SC"]==1, ]
    abs <- CIdata_test_data_with_newlevels_NA[CIdata_test_data_with_newlevels_NA[ ,"p_a_SC"]==0, ]
    e <- evaluate(pres,abs,best.model)
    #pdf(paste("AUC_CIdata2001_2003_",i,"_percent.pdf"))
    par(mfrow=c(2,2))
    plot(e,"ROC")
    #boxplot(e)
    #density(e)
    #dev.off()
    AUC[k, 5] <- e@auc
  }
}


### Data 2001_2003_2005_2007_2009_2011_2013
## Data preparation - remove NAs and factorize categorical variables 
for (k in 1:100) {
  tmpID <- which(is.na(CIdata2001_2003_2005_2007_2009_2011_2013_list[[k]]$Vegetation)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2001_2003_2005_2007_2009_2011_2013_list[[k]] <- CIdata2001_2003_2005_2007_2009_2011_2013_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2001_2003_2005_2007_2009_2011_2013_list[[k]]$Vegetation)) # check if NAs dissappeared 
  CIdata2001_2003_2005_2007_2009_2011_2013_list[[k]]$Vegetation <- factor(CIdata2001_2003_2005_2007_2009_2011_2013_list[[k]]$Vegetation) # factorize categorical variable to use in the model and overwrite data with factorized vegetation
  print(class(CIdata2001_2003_2005_2007_2009_2011_2013_list[[k]]$Vegetation)) # check if factorization worked

  tmpID <- which(is.na(CIdata2001_2003_2005_2007_2009_2011_2013_list[[k]]$sd_elev)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2001_2003_2005_2007_2009_2011_2013_list[[k]] <- CIdata2001_2003_2005_2007_2009_2011_2013_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2001_2003_2005_2007_2009_2011_2013_list[[k]]$sd_elev)) # check if NAs dissappeared 
  
  tmpID <- which(is.na(CIdata2001_2003_2005_2007_2009_2011_2013_list[[k]]$sd_DFR)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2001_2003_2005_2007_2009_2011_2013_list[[k]] <- CIdata2001_2003_2005_2007_2009_2011_2013_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2001_2003_2005_2007_2009_2011_2013_list[[k]]$sd_DFR)) # check if NAs dissappeared 
  
  CIdata2001_2003_2005_2007_2009_2011_2013_list[[k]]$Year <- factor(CIdata2001_2003_2005_2007_2009_2011_2013_list[[k]]$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
  print(class(CIdata2001_2003_2005_2007_2009_2011_2013_list[[k]]$Year)) # check if factorization worked
  
  tmpID <- which(is.na(CIdata2001_2003_2005_2007_2009_2011_2013_list[[k]]$geology_collapsed)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2001_2003_2005_2007_2009_2011_2013_list[[k]] <- CIdata2001_2003_2005_2007_2009_2011_2013_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2001_2003_2005_2007_2009_2011_2013_list[[k]]$geology_collapsed)) # check if NAs dissappeared 
  
  CIdata2001_2003_2005_2007_2009_2011_2013_list[[k]]$geology_collapsed <- factor(CIdata2001_2003_2005_2007_2009_2011_2013_list[[k]]$geology_collapsed)
  print(class(CIdata2001_2003_2005_2007_2009_2011_2013_list[[k]]$geology_collapsed))
  
  tmpID <- which(is.na(CIdata2015_list[[k]]$Vegetation)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2015_list[[k]] <- CIdata2015_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2015_list[[k]]$Vegetation)) # check if NAs dissappeared
  CIdata2015_list[[k]]$Vegetation <- factor(CIdata2015_list[[k]]$Vegetation) # factorize categorical variable to use in the model and overwrite data with factorized vegetation
  print(class(CIdata2015_list[[k]]$Vegetation)) # check if factorization worked
  
  tmpID <- which(is.na(CIdata2015_list[[k]]$sd_elev)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2015_list[[k]] <- CIdata2015_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2015_list[[k]]$sd_elev)) # check if NAs dissappeared
  
  tmpID <- which(is.na(CIdata2015_list[[k]]$sd_DFR)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2015_list[[k]] <- CIdata2015_list[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(CIdata2015_list[[k]]$sd_DFR)) # check if NAs dissappeared
  
  CIdata2015_list[[k]]$Year <- factor(CIdata2015_list[[k]]$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
  print(class(CIdata2015_list[[k]]$Year)) # check if factorization worked
  
  tmpID <- which(is.na(CIdata2015_list[[k]]$geology_collapsed)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) CIdata2015_list[[k]] <- CIdata2015_list[[k]][-tmpID, ] # remove rows with NA in geology_collapsed column
  which(is.na(CIdata2015_list[[k]]$geology_collapsed)) # check if NAs dissappeared
  CIdata2015_list[[k]]$geology_collapsed <- factor(CIdata2015_list[[k]]$geology_collapsed) # factorize geology_collapsed levels 
  
}

## Run model 
options(na.action=na.fail) # set up for dredge function 
# VEGETATION, SD_ELEVATION, GEOLOGY_COLLAPSED, SD_DISTANCE_FROM_ROAD AND YEAR
for (k in 1:100) { # for 1 - 100% of sites 
  model_fit <- glm(p_a_SC ~ Vegetation + sd_elev + geology_collapsed + sd_DFR + Year, data=CIdata2001_2003_2005_2007_2009_2011_2013_list[[k]], family=binomial) # run glm 
  #save(model_fit, file=paste("glm_output_VEG_sd_ELEV_geology_collapsed_sd_DISTFROMROAD_YEAR_CIdata2001_2003_",k,"_percent.Rda", sep="")) # save model summary
  x <- dredge(model_fit, beta="none") # model selection 
  #save(x, file=paste("AIC_ranked_CIdata2001_2003_",k,"percent.Rda",sep="")) # save ranked AICs 
  best.model <- (get.models(x, 1)[[1]]) # only keep best model 
  #save(best.model, file=paste("model_CIdata_2001_2003_",k,"_percent.Rda", sep="")) # save best model 
  CIdata_test <- CIdata2015_list[[100]] # make sure categorical variables are factorized 
  
  if (k > 0){ # because model fit to data from less than 5% of sites, results in null model being best model 
    
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
    
    CIdata_test_data_with_newlevels_NA <- missingLevelsToNA(object=best.model, data=CIdata_test)
    
    CIdata_test_data_with_newlevels_NA$Year <- as.factor(2013)
    
    pres <- CIdata_test_data_with_newlevels_NA[CIdata_test_data_with_newlevels_NA[ ,"p_a_SC"]==1, ]
    abs <- CIdata_test_data_with_newlevels_NA[CIdata_test_data_with_newlevels_NA[ ,"p_a_SC"]==0, ]
    e <- evaluate(pres,abs,best.model)
    #pdf(paste("AUC_CIdata2001_2003_",i,"_percent.pdf"))
    par(mfrow=c(2,2))
    plot(e,"ROC")
    #boxplot(e)
    #density(e)
    #dev.off()
    AUC[k, 6] <- e@auc
  }
  AUC[k, 7] <- mean(AUC[k,1:6 ], na.rm=TRUE)
  AUC[k, 8] <- sd((AUC[k,1:6])/sqrt(length(AUC[k,1:6])),na.rm=TRUE)
}

write.table(AUC, file="AUC_values.txt", row.names = TRUE, col.names = TRUE)
plot(1:100, AUC[ , 7], ylim=c(0,1), type="l")
lines(1:100, AUC[ , 1], col="grey")
lines(1:100, AUC[ , 2], col="grey")
lines(1:100, AUC[ , 3], col="grey")
lines(1:100, AUC[ , 4], col="grey")
lines(1:100, AUC[ , 5], col="grey")
lines(1:100, AUC[ , 6], col="grey")
#upperlim_AUC <- AUC[ , 7] + AUC[ , 8]
#lowerlim_AUC <- AUC[ , 7] - AUC[ , 8]
#lines(1:100, upperlim_AUC, col="grey")
#lines(1:100, lowerlim_AUC, col="grey")





#################### write the entire thing as a function, but without model selection #############################################

model_fit_function <- function(data_list, percentage){
  model_fit <- glm(p_a_SC ~ Vegetation + sd_elev + geology_collapsed + sd_DFR + Year, data=get(data_list), family=binomial)
  save(model_fit, file=paste("glm_VEG_sd_ELEV_geo_coll_sd_DFR_YEAR_",data_list,""))
}


cumul_datasets <- c("CIdata2001_2003_list","CIdata2001_2003_2005_list", "CIdata2001_2003_2005_2007_list", "CIdata2001_2003_2005_2007_2009_list",
                    "CIdata2001_2003_2005_2007_2009_2011_list", "CIdata2001_2003_2005_2007_2009_2011_2013_list")

for(l in cumul_datasets){
  for (k in 1:100){
    model_fit_function(data_list=l , percentage=k)
  }
}




















############################# Archive with all_data_files (only 10-20-30% of sites monitored) ######################


################### Modeling process #########################################
## Data preparation - remove NAs and factorize categorical variables 
for (k in 1:length(all_data_frames)) {
  tmpID <- which(is.na(all_data_frames[[k]]$Vegetation)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) all_data_frames[[k]] <- all_data_frames[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(all_data_frames[[k]]$Vegetation)) # check if NAs dissappeared 
  all_data_frames[[k]]$Vegetation <- factor(all_data_frames[[k]]$Vegetation) # factorize categorical variable to use in the model and overwrite data with factorized vegetation
  print(class(all_data_frames[[k]]$Vegetation)) # check if factorization worked
  
  tmpID <- which(is.na(all_data_frames[[k]]$Elevation)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) all_data_frames[[k]] <- all_data_frames[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(all_data_frames[[k]]$Elevation)) # check if NAs dissappeared 
  
  tmpID <- which(is.na(all_data_frames[[k]]$sd_elev)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) all_data_frames[[k]] <- all_data_frames[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(all_data_frames[[k]]$sd_elev)) # check if NAs dissappeared 
  
  tmpID <- which(is.na(all_data_frames[[k]]$sd_DFR)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) all_data_frames[[k]] <- all_data_frames[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(all_data_frames[[k]]$sd_DFR)) # check if NAs dissappeared 
  
  all_data_frames[[k]]$Year <- factor(all_data_frames[[k]]$Year) # factorize categorical variable to use in the model and overwrite data with factorized years
  print(class(all_data_frames[[k]]$Year)) # check if factorization worked
  
  tmpID <- which(is.na(all_data_frames[[k]]$geology)) # check where NAs (rows_to_remove)
  if (length(tmpID) > 0) all_data_frames[[k]] <- all_data_frames[[k]][-tmpID, ] # remove rows with NA in vegetation column
  which(is.na(all_data_frames[[k]]$geology)) # check if NAs dissappeared 
  
  all_data_frames[[k]]$geology <- factor(all_data_frames[[k]]$geology)
  print(class(all_data_frames[[k]]$geology))
}

### Generalized linear models 
# 1 PREDICTOR 
# VEGETATION 
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ Vegetation, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_VEG",all_data_files[k],".Rda", sep=""))
  
  plot(all_data_frames[[k]]$Vegetation, all_data_frames[[k]]$p_a_SC, xlab = "Vegetation", ylab = "Probability of occurrence of supercolony", main=all_data_files[k])
  points(all_data_frames[[k]]$Vegetation, fitted(model_fit), col= "red")
}

# ELEVATION  
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ Elevation, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_ELEV",all_data_files[k],".Rda", sep=""))
  
  plot(all_data_frames[[k]]$Elevation, all_data_frames[[k]]$p_a_SC, xlab = "Elevation", ylab = "Probability of occurrence of supercolony", main=all_data_files[k])
  points(all_data_frames[[k]]$Elevation, fitted(model_fit), col= "red")
}

# GEOLOGY 
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ geology, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_GEO",all_data_files[k],".Rda", sep=""))
  
  plot(all_data_frames[[k]]$geology, all_data_frames[[k]]$p_a_SC, xlab = "Geology", ylab = "Probability of occurrence of supercolony", main=all_data_files[k])
  points(all_data_frames[[k]]$geology, fitted(model_fit), col= "red")
}

# DISTANCE_FROM_ROAD 
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ distancefromroad, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_DISTFROMROAD",all_data_files[k],".Rda", sep=""))
  
  plot(all_data_frames[[k]]$distancefromroad, all_data_frames[[k]]$p_a_SC, xlab = "Distance from road", ylab = "Probability of occurrence of supercolony", main=all_data_files[k])
  points(all_data_frames[[k]]$distancefromroad, fitted(model_fit), col= "red")
}

# RAINFALL
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ rainfall_meters, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_RAINFALL_METERS",all_data_files[k],".Rda", sep=""))
  
  plot(all_data_frames[[k]]$rainfall_meters, all_data_frames[[k]]$p_a_SC, xlab = "rainfall_meters", ylab = "Probability of occurrence of supercolony", main=all_data_files[k])
  points(all_data_frames[[k]]$rainfall_meters, fitted(model_fit), col= "red")
}


## STANDARDIZED CONTINUOUS COVARIABLES 
# SD_ELEVATION 
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ sd_elev, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_sd_ELEV",all_data_files[k],".Rda", sep=""))
}

# SD_DISTANCE_FROM_ROAD 
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ sd_DFR, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_sd_DISTFROMROAD",all_data_files[k],".Rda", sep=""))
}

# SD_RAINFALL
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ sd_rainfall_meters, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_sd_rainfall_meters",all_data_files[k],".Rda", sep=""))
}


## 2 PREDICTORS 
# VEGETATION AND YEAR 
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ Vegetation + Year, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_VEG_YEAR",all_data_files[k],".Rda", sep=""))
}
# SD_ELEVATION AND YEAR
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ sd_elev + Year, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_sd_ELEV_YEAR",all_data_files[k],".Rda", sep=""))
}
# GEOLOGY AND YEAR
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ geology + Year, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_geology_YEAR",all_data_files[k],".Rda", sep=""))
}
# SD_DISTANCE_FROM_ROAD AND YEAR
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ sd_DFR + Year, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_sd_DISTFROMROAD_YEAR",all_data_files[k],".Rda", sep=""))
}
# RAINFALL AND YEAR 
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ sd_rainfall_meters + Year, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_sd_rainfall_meters_YEAR",all_data_files[k],".Rda", sep=""))
}

## 3 PREDICTORS 
# VEGETATION, SD_ELEVATION AND YEAR
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ Vegetation + sd_elev + Year, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_VEG_sd_ELEV_YEAR",all_data_files[k],".Rda", sep=""))
}
# VEGETATION, GEOLOGY AND YEAR
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ Vegetation + geology + Year, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_VEG_geology_YEAR",all_data_files[k],".Rda", sep=""))
}
# VEGETATION, SD_DISTANCE_FROM_ROAD AND YEAR
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ Vegetation + sd_DFR + Year, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_VEG_DISTFROMROAD_YEAR",all_data_files[k],".Rda", sep=""))
}
# VEGETATION, RAINFALL AND YEAR 
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ Vegetation + sd_rainfall_meters + Year, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_VEG_sd_rainfall_meters_YEAR",all_data_files[k],".Rda", sep=""))
}
# SD_ELEVATION, GEOLOGY AND YEAR
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ sd_elev + geology + Year, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_sd_ELEV_geology_YEAR",all_data_files[k],".Rda", sep=""))
}
# SD_ELEVATION, SD_DISTANCE_FROM_ROAD AND YEAR
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ sd_elev + sd_DFR + Year, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_sd_ELEV_sd_DISTFROMROAD_YEAR",all_data_files[k],".Rda", sep=""))
}
# SD_ELEVATION, RAINFALL AND YEAR 
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ sd_elev + sd_rainfall_meters + Year, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_sd_ELEV_sd_rainfall_meters_YEAR",all_data_files[k],".Rda", sep=""))
}
# GEOLOGY, SD_DISTANCE_FROM_ROAD AND YEAR
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ geology + sd_DFR + Year, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_geology_sd_DISTFROMROAD_YEAR",all_data_files[k],".Rda", sep=""))
}
# GEOLOGY, RAINFALL AND YEAR 
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ geology + sd_rainfall_meters + Year, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_geology_sd_rainfall_meters_YEAR",all_data_files[k],".Rda", sep=""))
}
# SD_DISTANCE_FROM_ROAD, RAINFALL AND YEAR
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ sd_DFR + sd_rainfall_meters + Year, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_sd_DISTFROMROAD_sd_rainfall_meters_YEAR",all_data_files[k],".Rda", sep=""))
}

## 4 PREDICTORS 
# VEGETATION, SD_ELEVATION, GEOLOGY AND YEAR
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ Vegetation + sd_elev + geology + Year, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_VEG_sd_ELEV_geology_YEAR",all_data_files[k],".Rda", sep=""))
}
# VEGETATION, SD_ELEVATION, SD_DISTANCE_FROM_ROAD AND YEAR
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ Vegetation + sd_elev + sd_DFR + Year, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_VEG_sd_ELEV_sd_DISTFROMROAD_YEAR",all_data_files[k],".Rda", sep=""))
}
# VEGETATION, SD_ELEVATION, RAINFALL AND YEAR
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ Vegetation + sd_elev + sd_rainfall_meters + Year, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_VEG_sd_ELEV_sd_rainfall_meters_YEAR",all_data_files[k],".Rda", sep=""))
}
# SD_ELEVATION, GEOLOGY, SD_DISTANCE_FROM_ROAD AND YEAR
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ sd_elev + geology + sd_DFR + Year, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_sd_ELEV_geology_sd_DISTFROMROAD_YEAR",all_data_files[k],".Rda", sep=""))
}
# SD_ELEVATION, GEOLOGY, RAINFALL AND YEAR
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ sd_elev + geology + sd_rainfall_meters + Year, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_sd_ELEV_geology_sd_rainfall_meters_YEAR",all_data_files[k],".Rda", sep=""))
}
# GEOLOGY, SD_DISTANCE_FROM_ROAD, RAINFALL AND YEAR 
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ geology + sd_DFR + sd_rainfall_meters + Year, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_geology_sd_DISTFROMROAD_sd_rainfall_meters_YEAR",all_data_files[k],".Rda", sep=""))
}
## 5 PREDICTORS 
# VEGETATION, SD_ELEVATION, GEOLOGY, SD_DISTANCE_FROM_ROAD AND YEAR
for (k in 1:length(all_data_frames)) {
  print(all_data_files[k])
  model_fit <- glm(p_a_SC ~ Vegetation + sd_elev + geology + sd_DFR + Year, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_VEG_sd_ELEV_geology_sd_DISTFROMROAD_YEAR",all_data_files[k],".Rda", sep=""))
}
# VEGETATION, SD_ELEVATION, GEOLOGY, RAINFALL AND YEAR
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ Vegetation + sd_elev + geology + sd_rainfall_meters + Year, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_VEG_sd_ELEV_geology_sd_rainfall_meters_YEAR",all_data_files[k],".Rda", sep=""))
}
# SD_ELEVATION, GEOLOGY, SD_DISTANCE_FROM_ROAD, RAINFALL AND YEAR 
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ sd_elev + geology + sd_DFR + sd_rainfall_meters + Year, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_sd_ELEV_geology_sd_DISTFROMROAD_sd_rainfall_meters_YEAR",all_data_files[k],".Rda", sep=""))
}
## ALL PREDICTORS 
# VEGETATION, SD_ELEVATION, GEOLOGY, SD_DISTANCE_FROM_ROAD, RAINFALL AND YEAR
for (k in 1:length(all_data_frames)) {
  model_fit <- glm(p_a_SC ~ Vegetation + sd_elev + geology + sd_DFR + sd_rainfall_meters + Year, data=all_data_frames[[k]], family=binomial)
  save(model_fit, file=paste("glm_output_VEG_sd_ELEV_geology_sd_DISTFROMROAD_sd_rainfall_meters_YEAR",all_data_files[k],".Rda", sep=""))
}


### Examining fitted model - adjust for correct dataset and model_fit 
for (i in 1:72){ 
  CIdata_test <- all_data_frames[[i]] # make sure categorical variables are factorized 
  model_name <- paste("glm_output_VEG_sd_ELEV_geology_sd_DISTFROMROAD_YEAR", all_data_files[i], ".Rda", sep="")
  load(model_name)
  pres <- CIdata_test[CIdata_test[ ,"p_a_SC"]==1, ]
  abs <- CIdata_test[CIdata_test[ ,"p_a_SC"]==0, ]
  e <- evaluate(pres,abs,model_fit)
  pdf(paste("AUC_",all_data_files[i],".pdf"))
  par(mfrow=c(2,2))
  plot(e,"ROC")
  boxplot(e)
  density(e)
  dev.off()
}


## Other possibilities
# K-fold cross validation (divides dataset in training and test data) -> predictive performance 
# R squared
# mean deviance 
# lack of fit test: residualPlots(model_fit, id.n=2) -> table with Lack of Fit test = t-test for predictor added 
# -> this is a scatterplot with Pearson residuals vs each of the predictors and fitted values 























