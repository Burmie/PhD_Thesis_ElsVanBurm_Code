### Here we provide each site with a "knapsack value", which is calculated as the ratio managegment_benefit:management_cost
### Management_benfefit is the predicted P(occ) at each site
### Management_cost is the cost to manage that specific site 
### Sites are ranked in decreasing order of "knapsack value"  (or ratio management_benefit:management_cost)
### Sites that fall within manegement budget are selected for management 

### Each site ends up with 600 "knapsack values" obtained from 600 predictive maps created (from building a HSM with 600 subsets)
### and a management cost 

### !!!! Monitoring cost is the same across years (since the number of sites monitored each year is equal across years, i.e. 837)
### !!!! Management budget is equal across years, management benefit (or probability of occupancy) differs among years
#################################################################################################################################

## Load predictions
load("predictions.Rda")
all_data_files <- vector()
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

## Load required packages
library(data.table)

predictions
all_data_files
profit <- vector()
full_datasets <- c("CIdata2003", "CIdata2005","CIdata2007","CIdata2009", "CIdata2011", "CIdata2013","CIdata2015")

# for prediction based on data from 2001_2003 - add profit and Knapsack column to 2003 dataset (but it actually doens't matter which year because all datasets have same nsites)
for (j in 4:100) { # we start using predictions from 4% of sites (as soon as predictors were identified - fewer sites: random management)
  profit <- predictions[[j]] # get profit (probability of occurrence) values for each survey point
  df <- get(full_datasets[1])
  df <- cbind(df, profit) # add profit values to dataframe 
  tmpID <- which(is.na(df$profit)) # check if any NAs in P(occ)
  if (length(tmpID) > 0) df <- df[-tmpID, ] # remove rows with NA for P(occ) from dataset
  if (length(tmpID) > 0) profit <- profit[-tmpID]
  which(is.na(df$profit)) 
  cost <- df$COST_Travel_plus_Survey_rescaled # read in cost file
  knapsack <- profit/cost # Knapsack formulation for each survey point (benefit/cost) 
  df$Knapsack_profit_per_cost <- knapsack
  dt <- setorder(as.data.table(df), -Knapsack_profit_per_cost) # Order sites in decreasing Knapsack value
  print(paste(all_data_files[j],"_SURVEYED",sep="")) 
  #kk <- dt[which(dt$Year == max(dt$Year)), ]
  #print(kk)
  dt <- dt[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2003_MANAGEMENT_BUDGET[j], ] # go to sites for which COST lies within management budget (caluclated in previous script)
  # Write table for model validation (SC evaluation)
  write.table(dt, file=(paste(all_data_files[j],"_SURVEYED_sitesTOmanage.txt",sep="")), sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)
}              

# for prediction based on data from 2001_2003_2005 - add profit and Knapsack column to 2005 dataset (but it actually doens't matter)
for (j in 102:200) { # same as above
  profit <- predictions[[j]] # get profit (probability of occurrence) values for each survey point
  df <- get(full_datasets[2])
  df <- cbind(df, profit) # add profit values to dataframe 
  tmpID <- which(is.na(df$profit)) # check if any NAs in P(occ)
  if (length(tmpID) > 0) df <- df[-tmpID, ] # remove rows with NA for P(occ) from dataset
  if (length(tmpID) > 0) profit <- profit[-tmpID]
  which(is.na(df$profit)) 
  cost <- df$COST_Travel_plus_Survey_rescaled # read in cost file
  knapsack <- profit/cost # Knapsack formulation for each survey point (benefit/cost) 
  df$Knapsack_profit_per_cost <- knapsack
  dt <- setorder(as.data.table(df), -Knapsack_profit_per_cost) # Order sites in decreasing Knapsack value
  print(paste(all_data_files[j],"_SURVEYED",sep="")) 
  #kk <- dt[which(dt$Year == max(dt$Year)), ]
  #print(kk)
  dt <- dt[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2005_MANAGEMENT_BUDGET[j-100], ] # go to sites for which COST lies within management budget (caluclated in previous script)
  # Write table for model validation (SC evaluation)
  write.table(dt, file=(paste(all_data_files[j],"_SURVEYED_sitesTOmanage.txt",sep="")), sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)
}              

# for prediction based on data from 2001_2003_2005_2007 - add profit and Knapsack column to 2007 dataset (but it actually doens't matter)
for (j in 203:300) { # same as above
  profit <- predictions[[j]] # get profit (probability of occurrence) values for each survey point
  df <- get(full_datasets[3])
  df <- cbind(df, profit) # add profit values to dataframe 
  tmpID <- which(is.na(df$profit)) # check if any NAs in P(occ)
  if (length(tmpID) > 0) df <- df[-tmpID, ] # remove rows with NA for P(occ) from dataset
  if (length(tmpID) > 0) profit <- profit[-tmpID]
  which(is.na(df$profit)) 
  cost <- df$COST_Travel_plus_Survey_rescaled # read in cost file
  knapsack <- profit/cost # Knapsack formulation for each survey point (benefit/cost) 
  df$Knapsack_profit_per_cost <- knapsack
  dt <- setorder(as.data.table(df), -Knapsack_profit_per_cost) # Order sites in decreasing Knapsack value
  print(paste(all_data_files[j],"_SURVEYED",sep="")) 
  #kk <- dt[which(dt$Year == max(dt$Year)), ]
  #print(kk)
  dt <- dt[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2007_MANAGEMENT_BUDGET[j-200], ] # go to sites for which COST lies within management budget (caluclated in previous script)
  # Write table for model validation (SC evaluation)
  write.table(dt, file=(paste(all_data_files[j],"_SURVEYED_sitesTOmanage.txt",sep="")), sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)
}       

# for prediction based on data from 2001_2003_2005_2007_2009 - add profit and Knapsack column to 2009 dataset (but it actually doens't matter)
for (j in 301:400) {
  profit <- predictions[[j]] # get profit (probability of occurrence) values for each survey point
  df <- get(full_datasets[4])
  df <- cbind(df, profit) # add profit values to dataframe 
  tmpID <- which(is.na(df$profit)) # check if any NAs in P(occ)
  if (length(tmpID) > 0) df <- df[-tmpID, ] # remove rows with NA for P(occ) from dataset
  if (length(tmpID) > 0) profit <- profit[-tmpID]
  which(is.na(df$profit)) 
  cost <- df$COST_Travel_plus_Survey_rescaled # read in cost file
  knapsack <- profit/cost # Knapsack formulation for each survey point (benefit/cost) 
  df$Knapsack_profit_per_cost <- knapsack
  dt <- setorder(as.data.table(df), -Knapsack_profit_per_cost) # Order sites in decreasing Knapsack value
  print(paste(all_data_files[j],"_SURVEYED",sep="")) 
  #kk <- dt[which(dt$Year == max(dt$Year)), ]
  #print(kk)
  dt <- dt[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2009_MANAGEMENT_BUDGET[j-300], ] # go to sites for which COST lies within management budget (caluclated in previous script)
  # Write table for model validation (SC evaluation)
  write.table(dt, file=(paste(all_data_files[j],"_SURVEYED_sitesTOmanage.txt",sep="")), sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)
}       

# for prediction based on data from 2001_2003_2005_2007_2009_2011 - add profit and Knapsack column to 2011 dataset (but it actually doens't matter)
for (j in 401:500) {
  profit <- predictions[[j]] # get profit (probability of occurrence) values for each survey point
  df <- get(full_datasets[5])
  df <- cbind(df, profit) # add profit values to dataframe 
  tmpID <- which(is.na(df$profit)) # check if any NAs in P(occ)
  if (length(tmpID) > 0) df <- df[-tmpID, ] # remove rows with NA for P(occ) from dataset
  if (length(tmpID) > 0) profit <- profit[-tmpID]
  which(is.na(df$profit)) 
  cost <- df$COST_Travel_plus_Survey_rescaled # read in cost file
  knapsack <- profit/cost # Knapsack formulation for each survey point (benefit/cost) 
  df$Knapsack_profit_per_cost <- knapsack
  dt <- setorder(as.data.table(df), -Knapsack_profit_per_cost) # Order sites in decreasing Knapsack value
  print(paste(all_data_files[j],"_SURVEYED",sep="")) 
  #kk <- dt[which(dt$Year == max(dt$Year)), ]
  #print(kk)
  dt <- dt[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2011_MANAGEMENT_BUDGET[j-400], ] # go to sites for which COST lies within management budget (caluclated in previous script)
  # Write table for model validation (SC evaluation)
  write.table(dt, file=(paste(all_data_files[j],"_SURVEYED_sitesTOmanage.txt",sep="")), sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)
}  

# for prediction based on data from 2001_2003_2005_2007_2009_2011_2013 - add profit and Knapsack column to 2013 dataset (but it actually doens't matter)
for (j in 501:600) {
  profit <- predictions[[j]] # get profit (probability of occurrence) values for each survey point
  df <- get(full_datasets[6])
  df <- cbind(df, profit) # add profit values to dataframe 
  tmpID <- which(is.na(df$profit)) # check if any NAs in P(occ)
  if (length(tmpID) > 0) df <- df[-tmpID, ] # remove rows with NA for P(occ) from dataset
  if (length(tmpID) > 0) profit <- profit[-tmpID]
  which(is.na(df$profit)) 
  cost <- df$COST_Travel_plus_Survey_rescaled # read in cost file
  knapsack <- profit/cost # Knapsack formulation for each survey point (benefit/cost) 
  df$Knapsack_profit_per_cost <- knapsack
  dt <- setorder(as.data.table(df), -Knapsack_profit_per_cost) # Order sites in decreasing Knapsack value
  print(paste(all_data_files[j],"_SURVEYED",sep="")) 
  #kk <- dt[which(dt$Year == max(dt$Year)), ]
  #print(kk)
  dt <- dt[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2013_MANAGEMENT_BUDGET[j-500], ] # go to sites for which COST lies within management budget (caluclated in previous script)
  # Write table for model validation (SC evaluation)
  write.table(dt, file=(paste(all_data_files[j],"_SURVEYED_sitesTOmanage.txt",sep="")), sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)
}  

# Here we assign sites that need to be managed randomly (instead of using predictions)
# This is the case for the following "models": all_data_files[1,2,3,101,201,202]
for (j in c(1,2,3)){
  df <- get(full_datasets[6]) # this loads CIdata2013, which is OK
  dt <- as.data.table(df)
  dk <- sample(nrow(dt))
  ds <- dt[dk, ]
  print(paste(all_data_files[j],"_SURVEYED", sep=""))
  dp <- ds[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2013_MANAGEMENT_BUDGET[j], ] # go to sites for which COST lies within management budget (caluclated in previous script)
  write.table(dp, file=(paste(all_data_files[j],"_SURVEYED_sitesTOmanage.txt",sep="")), sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)
}
for (j in c(101)){
  df <- get(full_datasets[6]) # this loads CIdata2013, which is OK
  dt <- as.data.table(df)
  dk <- sample(nrow(dt))
  ds <- dt[dk, ]
  print(paste(all_data_files[j],"_SURVEYED", sep=""))
  dp <- ds[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2013_MANAGEMENT_BUDGET[j-100], ] # go to sites for which COST lies within management budget (caluclated in previous script)
  write.table(dp, file=(paste(all_data_files[j],"_SURVEYED_sitesTOmanage.txt",sep="")), sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)
}
for (j in c(201,202)){
  df <- get(full_datasets[6]) # this loads CIdata2013, which is OK
  dt <- as.data.table(df)
  dk <- sample(nrow(dt))
  ds <- dt[dk, ]
  print(paste(all_data_files[j],"_SURVEYED", sep=""))
  dp <- ds[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2013_MANAGEMENT_BUDGET[j-200], ] # go to sites for which COST lies within management budget (caluclated in previous script)
  write.table(dp, file=(paste(all_data_files[j],"_SURVEYED_sitesTOmanage.txt",sep="")), sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)
}



# TEST
kk <- read.table("CIdata_2001_2003_1_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="")
nrow(kk)

# PLOT sites to manage
# load required packages
library(raster)
library(rgdal)

# load coast layer 
setwd("../Data/Files_created_by_EVB/Rcode_Darren_separateyears/XmasIsland/6_GIS layers")
coast <- readOGR(dsn=getwd(), layer="coast_poly")
plot(coast)
setwd("../../../../../Ch04_Optimal_resource_allocation_between_survey_and_management_of_invasive_species/Total_budget_3000/00_Zero_monitoring_cost")

# plot the sites that are surveyed 
knapsack_sites_1_percent_surveyed <- read.table("CIdata_2001_2003_1_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="")
plot(knapsack_sites_1_percent_surveyed$avg_X_MARK,knapsack_sites_1_percent_surveyed$avg_Y_MARK, xlim=c(556000, 580000), ylim=c(8830000, 8850000), pch=21, cex=0.7, col="purple", bg="purple")
plot(coast, add=TRUE)
knapsack_sites_10_percent_surveyed <- read.table("CIdata_2001_2003_10_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="")
plot(knapsack_sites_10_percent_surveyed$avg_X_MARK,knapsack_sites_10_percent_surveyed$avg_Y_MARK, xlim=c(556000, 580000), ylim=c(8830000, 8850000), pch=21, cex=0.7, col="purple", bg="purple")
plot(coast, add=TRUE)
knapsack_sites_20_percent_surveyed <- read.table("CIdata_2001_2003_20_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="")
plot(knapsack_sites_20_percent_surveyed$avg_X_MARK,knapsack_sites_20_percent_surveyed$avg_Y_MARK, xlim=c(556000, 580000), ylim=c(8830000, 8850000), pch=21, cex=0.7, col="purple", bg="purple")
plot(coast, add=TRUE)
knapsack_sites_30_percent_surveyed <- read.table("CIdata_2001_2003_30_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="")
plot(knapsack_sites_30_percent_surveyed$avg_X_MARK,knapsack_sites_30_percent_surveyed$avg_Y_MARK, xlim=c(556000, 580000), ylim=c(8830000, 8850000), pch=21, cex=0.7, col="purple", bg="purple")
plot(coast, add=TRUE)
knapsack_sites_40_percent_surveyed <- read.table("CIdata_2001_2003_40_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="")
plot(knapsack_sites_40_percent_surveyed$avg_X_MARK,knapsack_sites_40_percent_surveyed$avg_Y_MARK, xlim=c(556000, 580000), ylim=c(8830000, 8850000), pch=21, cex=0.7, col="purple", bg="purple")
plot(coast, add=TRUE)
knapsack_sites_50_percent_surveyed <- read.table("CIdata_2001_2003_50_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="")
plot(knapsack_sites_50_percent_surveyed$avg_X_MARK,knapsack_sites_50_percent_surveyed$avg_Y_MARK, xlim=c(556000, 580000), ylim=c(8830000, 8850000), pch=21, cex=0.7, col="purple", bg="purple")
plot(coast, add=TRUE)
knapsack_sites_60_percent_surveyed <- read.table("CIdata_2001_2003_60_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="")
plot(knapsack_sites_60_percent_surveyed$avg_X_MARK,knapsack_sites_60_percent_surveyed$avg_Y_MARK, xlim=c(556000, 580000), ylim=c(8830000, 8850000), pch=21, cex=0.7, col="purple", bg="purple")
plot(coast, add=TRUE)
knapsack_sites_70_percent_surveyed <- read.table("CIdata_2001_2003_70_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="")
plot(knapsack_sites_70_percent_surveyed$avg_X_MARK,knapsack_sites_70_percent_surveyed$avg_Y_MARK, xlim=c(556000, 580000), ylim=c(8830000, 8850000), pch=21, cex=0.7, col="purple", bg="purple")
plot(coast, add=TRUE)
knapsack_sites_80_percent_surveyed <- read.table("CIdata_2001_2003_80_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="")
plot(knapsack_sites_80_percent_surveyed$avg_X_MARK,knapsack_sites_80_percent_surveyed$avg_Y_MARK, xlim=c(556000, 580000), ylim=c(8830000, 8850000), pch=21, cex=0.7, col="purple", bg="purple")
plot(coast, add=TRUE)
knapsack_sites_90_percent_surveyed <- read.table("CIdata_2001_2003_90_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="")
plot(knapsack_sites_90_percent_surveyed$avg_X_MARK,knapsack_sites_90_percent_surveyed$avg_Y_MARK, xlim=c(556000, 580000), ylim=c(8830000, 8850000), pch=21, cex=0.7, col="purple", bg="purple")
plot(coast, add=TRUE)
knapsack_sites_100_percent_surveyed <- read.table("CIdata_2001_2003_100_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="")
plot(knapsack_sites_100_percent_surveyed$avg_X_MARK,knapsack_sites_100_percent_surveyed$avg_Y_MARK, xlim=c(556000, 580000), ylim=c(8830000, 8850000), pch=21, cex=0.7, col="purple", bg="purple")
plot(coast, add=TRUE)


###############################################################################################################
######################################## Archive: when RASTERS were still used ################################
###############################################################################################################
for (j in all_data_files) {
  #get(paste("raster_prob_occ_predicted_with_",j,sep="")) 
  #class(get(paste("raster_prob_occ_predicted_with_",j,sep=""))) # check 
  profit <- vector()
  # extract profit values for each survey point
  for (i in 1:nrow(get(j))) {
    cat(round(100*i/nrow(get(j)),digits=2),"%\n",sep="")
    x = get(j)[i,"avg_X_MARK"]
    y = get(j)[i,"avg_Y_MARK"]
    profit <- c(profit, unname(extract(get(paste("raster_prob_occ_predicted_with_",j,sep="")),matrix(data = c(x,y),ncol=2))))
  }
  df <- get(j)
  df <- cbind(df, profit) # add profit values to dataframe 
  tmpID <- which(is.na(df$profit)) # check if any NAs in P(occ)
  if (length(tmpID) > 0) df <- df[-tmpID, ] # remove rows with NA for P(occ) from dataset
  if (length(tmpID) > 0) profit <- profit[-tmpID]
  which(is.na(df$profit)) 
  # read in cost file 
  cost <- df$COST_Travel_plus_Survey_rescaled 
  # Knapsack formulation for each cell (benefit/cost) 
  knapsack <- profit/cost
  df$Knapsack_profit_per_cost <- knapsack
  # Order sites in decreasing Knapsack value
  dt <- setorder(as.data.table(df), -Knapsack_profit_per_cost) 
  #print(head(dt)) # check if ordered by Knapsack (decreasing order)
  print(paste(j,"_SURVEYED",sep="")) 
  kk <- dt[which(dt$Year == max(dt$Year)), ]
  print(kk)
  kk <- kk[cumsum(COST_Travel_plus_Survey_rescaled) <= get(paste(j,"_MANAGEMENT_BUDGET",sep="")), ] # go to sites for which COST lies within management budget (caluclated in previous script)
  
  # Write table for model validation (SC evaluation)
  write.table(kk, file=(paste(j,"_SURVEYED_sitesTOmanage.txt",sep="")), sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)
}              












################################################ GLM fit to 25% of data ##################################################################### 

##### 2001_25_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2003_25 
class(prediction_veg_elev_2003_25) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2001_25_percent)) {
  cat(round(100*i/nrow(CIdata2001_25_percent),digits=2),"%\n",sep="")
  x = CIdata2001_25_percent[i,"avg_X_MARK"]
  y = CIdata2001_25_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2003_25,matrix(data = c(x,y),ncol=2))))
}
CIdata2001_25_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2001_25_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2001_25_percent <- CIdata2001_25_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2001_25_percent$Profit)) 
# read in cost file 
cost <- CIdata2001_25_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2001_25_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2001_25_percent <- as.data.table(CIdata2001_25_percent)
setorder(datatable_CIdata2001_25_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2001_25_percent) # check if ordered by Knapsack (decreasing order)
CIdata2001_25_percent_SURVEYED <- datatable_CIdata2001_25_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2001_25_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2001_25_percent_SURVEYED, file="CIdata2001_25_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)

##### 2001_2003_25_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2005_25 
class(prediction_veg_elev_2005_25) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2003_25_percent)) {
  cat(round(100*i/nrow(CIdata2003_25_percent),digits=2),"%\n",sep="")
  x = CIdata2003_25_percent[i,"avg_X_MARK"]
  y = CIdata2003_25_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2003_25,matrix(data = c(x,y),ncol=2))))
}
CIdata2003_25_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2003_25_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2003_25_percent <- CIdata2003_25_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2003_25_percent$Profit)) 
# read in cost file 
cost <- CIdata2003_25_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2003_25_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2003_25_percent <- as.data.table(CIdata2003_25_percent)
setorder(datatable_CIdata2003_25_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2003_25_percent) # check if ordered by Knapsack (decreasing order)
CIdata2003_25_percent_SURVEYED <- datatable_CIdata2003_25_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2003_25_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2003_25_percent_SURVEYED, file="CIdata2003_25_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)

##### 2001_2003_2005_25_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2007_25 
class(prediction_veg_elev_2007_25) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2005_25_percent)) {
  cat(round(100*i/nrow(CIdata2005_25_percent),digits=2),"%\n",sep="")
  x = CIdata2005_25_percent[i,"avg_X_MARK"]
  y = CIdata2005_25_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2005_25,matrix(data = c(x,y),ncol=2))))
}
CIdata2005_25_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2005_25_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2005_25_percent <- CIdata2005_25_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2005_25_percent$Profit)) 
# read in cost file 
cost <- CIdata2005_25_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2005_25_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2005_25_percent <- as.data.table(CIdata2005_25_percent)
setorder(datatable_CIdata2005_25_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2005_25_percent) # check if ordered by Knapsack (decreasing order)
CIdata2005_25_percent_SURVEYED <- datatable_CIdata2005_25_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2005_25_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2005_25_percent_SURVEYED, file="CIdata2005_25_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)

##### 2001_2003_2005_2007_25_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2009_25 
class(prediction_veg_elev_2009_25) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2007_25_percent)) {
  cat(round(100*i/nrow(CIdata2007_25_percent),digits=2),"%\n",sep="")
  x = CIdata2007_25_percent[i,"avg_X_MARK"]
  y = CIdata2007_25_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2009_25,matrix(data = c(x,y),ncol=2))))
}
CIdata2007_25_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2007_25_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2007_25_percent <- CIdata2007_25_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2007_25_percent$Profit)) 
# read in cost file 
cost <- CIdata2007_25_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2007_25_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2007_25_percent <- as.data.table(CIdata2007_25_percent)
setorder(datatable_CIdata2007_25_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2007_25_percent) # check if ordered by Knapsack (decreasing order)
CIdata2007_25_percent_SURVEYED <- datatable_CIdata2007_25_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2007_25_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2007_25_percent_SURVEYED, file="CIdata2007_25_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)

##### 2001_2003_2005_2007_2009_25_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2011_25 
class(prediction_veg_elev_2011_25) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2009_25_percent)) {
  cat(round(100*i/nrow(CIdata2009_25_percent),digits=2),"%\n",sep="")
  x = CIdata2009_25_percent[i,"avg_X_MARK"]
  y = CIdata2009_25_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2011_25,matrix(data = c(x,y),ncol=2))))
}
CIdata2009_25_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2009_25_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2009_25_percent <- CIdata2009_25_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2009_25_percent$Profit)) 
# read in cost file 
cost <- CIdata2009_25_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2009_25_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2009_25_percent <- as.data.table(CIdata2009_25_percent)
setorder(datatable_CIdata2009_25_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2009_25_percent) # check if ordered by Knapsack (decreasing order)
CIdata2009_25_percent_SURVEYED <- datatable_CIdata2009_25_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2009_25_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2009_25_percent_SURVEYED, file="CIdata2009_25_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)

##### 2001_2003_2005_2007_2009_2011_25_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2013_25 
class(prediction_veg_elev_2013_25) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2011_25_percent)) {
  cat(round(100*i/nrow(CIdata2011_25_percent),digits=2),"%\n",sep="")
  x = CIdata2011_25_percent[i,"avg_X_MARK"]
  y = CIdata2011_25_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2013_25,matrix(data = c(x,y),ncol=2))))
}
CIdata2011_25_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2011_25_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2011_25_percent <- CIdata2011_25_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2011_25_percent$Profit))
# read in cost file 
cost <- CIdata2011_25_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2011_25_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2011_25_percent <- as.data.table(CIdata2011_25_percent)
setorder(datatable_CIdata2011_25_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2011_25_percent) # check if ordered by Knapsack (decreasing order)
CIdata2011_25_percent_SURVEYED <- datatable_CIdata2011_25_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2011_25_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2011_25_percent_SURVEYED, file="CIdata2011_25_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)

##### 2001_2003_2005_2007_2009_2011_2013_25_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2015_25 
class(prediction_veg_elev_2015_25) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2013_25_percent)) {
  cat(round(100*i/nrow(CIdata2013_25_percent),digits=2),"%\n",sep="")
  x = CIdata2013_25_percent[i,"avg_X_MARK"]
  y = CIdata2013_25_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2015_25,matrix(data = c(x,y),ncol=2))))
}
CIdata2013_25_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2013_25_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2013_25_percent <- CIdata2013_25_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2013_25_percent$Profit))
# read in cost file 
cost <- CIdata2013_25_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2013_25_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2013_25_percent <- as.data.table(CIdata2013_25_percent)
setorder(datatable_CIdata2013_25_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2013_25_percent) # check if ordered by Knapsack (decreasing order)
CIdata2013_25_percent_SURVEYED <- datatable_CIdata2013_25_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2013_25_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2013_25_percent_SURVEYED, file="CIdata2013_25_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)


################################################ GLM fit to 50% of data ##################################################################### 

##### 2001_50_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2003_50 
class(prediction_veg_elev_2003_50) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2001_50_percent)) {
  cat(round(100*i/nrow(CIdata2001_50_percent),digits=2),"%\n",sep="")
  x = CIdata2001_50_percent[i,"avg_X_MARK"]
  y = CIdata2001_50_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2003_50,matrix(data = c(x,y),ncol=2))))
}
CIdata2001_50_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2001_50_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2001_50_percent <- CIdata2001_50_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2001_50_percent$Profit))
# read in cost file 
cost <- CIdata2001_50_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2001_50_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2001_50_percent <- as.data.table(CIdata2001_50_percent)
setorder(datatable_CIdata2001_50_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2001_50_percent) # check if ordered by Knapsack (decreasing order)
CIdata2001_50_percent_SURVEYED <- datatable_CIdata2001_50_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2001_50_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2001_50_percent_SURVEYED, file="CIdata2001_50_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)

##### 2001_2003_50_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2005_50 
class(prediction_veg_elev_2005_50) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2003_50_percent)) {
  cat(round(100*i/nrow(CIdata2003_50_percent),digits=2),"%\n",sep="")
  x = CIdata2003_50_percent[i,"avg_X_MARK"]
  y = CIdata2003_50_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2005_50,matrix(data = c(x,y),ncol=2))))
}
CIdata2003_50_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2003_50_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2003_50_percent <- CIdata2003_50_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2003_50_percent$Profit))
# read in cost file 
cost <- CIdata2003_50_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2003_50_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2003_50_percent <- as.data.table(CIdata2003_50_percent)
setorder(datatable_CIdata2003_50_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2003_50_percent) # check if ordered by Knapsack (decreasing order)
CIdata2003_50_percent_SURVEYED <- datatable_CIdata2003_50_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2003_50_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2003_50_percent_SURVEYED, file="CIdata2003_50_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)

##### 2001_2003_2005_50_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2007_50 
class(prediction_veg_elev_2007_50) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2005_50_percent)) {
  cat(round(100*i/nrow(CIdata2005_50_percent),digits=2),"%\n",sep="")
  x = CIdata2005_50_percent[i,"avg_X_MARK"]
  y = CIdata2005_50_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2007_50,matrix(data = c(x,y),ncol=2))))
}
CIdata2005_50_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2005_50_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2005_50_percent <- CIdata2005_50_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2005_50_percent$Profit))
# read in cost file 
cost <- CIdata2005_50_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2005_50_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2005_50_percent <- as.data.table(CIdata2005_50_percent)
setorder(datatable_CIdata2005_50_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2005_50_percent) # check if ordered by Knapsack (decreasing order)
CIdata2005_50_percent_SURVEYED <- datatable_CIdata2005_50_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2005_50_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2005_50_percent_SURVEYED, file="CIdata2005_50_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)

##### 2001_2003_2005_2007_50_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2009_50 
class(prediction_veg_elev_2009_50) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2007_50_percent)) {
  cat(round(100*i/nrow(CIdata2007_50_percent),digits=2),"%\n",sep="")
  x = CIdata2007_50_percent[i,"avg_X_MARK"]
  y = CIdata2007_50_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2009_50,matrix(data = c(x,y),ncol=2))))
}
CIdata2007_50_percent$Profit <- profit # add profit values to dataframe
tmpID <- which(is.na(CIdata2007_50_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2007_50_percent <- CIdata2007_50_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2007_50_percent$Profit))
# read in cost file 
cost <- CIdata2007_50_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2007_50_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2007_50_percent <- as.data.table(CIdata2007_50_percent)
setorder(datatable_CIdata2007_50_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2007_50_percent) # check if ordered by Knapsack (decreasing order)
CIdata2007_50_percent_SURVEYED <- datatable_CIdata2007_50_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2007_50_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2007_50_percent_SURVEYED, file="CIdata2007_50_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)

##### 2001_2003_2005_2007_2009_50_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2011_50 
class(prediction_veg_elev_2011_50) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2009_50_percent)) {
  cat(round(100*i/nrow(CIdata2009_50_percent),digits=2),"%\n",sep="")
  x = CIdata2009_50_percent[i,"avg_X_MARK"]
  y = CIdata2009_50_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2011_50,matrix(data = c(x,y),ncol=2))))
}
CIdata2009_50_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2009_50_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2009_50_percent <- CIdata2009_50_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2009_50_percent$Profit))
# read in cost file 
cost <- CIdata2009_50_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2009_50_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2009_50_percent <- as.data.table(CIdata2009_50_percent)
setorder(datatable_CIdata2009_50_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2009_50_percent) # check if ordered by Knapsack (decreasing order)
CIdata2009_50_percent_SURVEYED <- datatable_CIdata2009_50_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2009_50_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2009_50_percent_SURVEYED, file="CIdata2009_50_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)

##### 2001_2003_2005_2007_2009_2011_50_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2013_50 
class(prediction_veg_elev_2013_50) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2011_50_percent)) {
  cat(round(100*i/nrow(CIdata2011_50_percent),digits=2),"%\n",sep="")
  x = CIdata2011_50_percent[i,"avg_X_MARK"]
  y = CIdata2011_50_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2013_50,matrix(data = c(x,y),ncol=2))))
}
CIdata2011_50_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2011_50_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2011_50_percent <- CIdata2011_50_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2011_50_percent$Profit))
# read in cost file 
cost <- CIdata2011_50_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2011_50_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2011_50_percent <- as.data.table(CIdata2011_50_percent)
setorder(datatable_CIdata2011_50_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2011_50_percent) # check if ordered by Knapsack (decreasing order)
CIdata2011_50_percent_SURVEYED <- datatable_CIdata2011_50_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2011_50_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2011_50_percent_SURVEYED, file="CIdata2011_50_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)

##### 2001_2003_2005_2007_2009_2011_2013_50_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2015_50 
class(prediction_veg_elev_2015_50) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2013_50_percent)) {
  cat(round(100*i/nrow(CIdata2013_50_percent),digits=2),"%\n",sep="")
  x = CIdata2013_50_percent[i,"avg_X_MARK"]
  y = CIdata2013_50_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2015_50,matrix(data = c(x,y),ncol=2))))
}
CIdata2013_50_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2013_50_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2013_50_percent <- CIdata2013_50_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2013_50_percent$Profit))
# read in cost file 
cost <- CIdata2013_50_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2013_50_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2013_50_percent <- as.data.table(CIdata2013_50_percent)
setorder(datatable_CIdata2013_50_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2013_50_percent) # check if ordered by Knapsack (decreasing order)
CIdata2013_50_percent_SURVEYED <- datatable_CIdata2013_50_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2013_50_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2013_50_percent_SURVEYED, file="CIdata2013_50_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)


################################################ GLM fit to 75% of data ##################################################################### 

##### 2001_75_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2003_75 
class(prediction_veg_elev_2003_75) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2001_75_percent)) {
  cat(round(100*i/nrow(CIdata2001_75_percent),digits=2),"%\n",sep="")
  x = CIdata2001_75_percent[i,"avg_X_MARK"]
  y = CIdata2001_75_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2003_75,matrix(data = c(x,y),ncol=2))))
}
CIdata2001_75_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2001_75_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2001_75_percent <- CIdata2001_75_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2001_75_percent$Profit))
# read in cost file 
cost <- CIdata2001_75_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2001_75_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2001_75_percent <- as.data.table(CIdata2001_75_percent)
setorder(datatable_CIdata2001_75_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2001_75_percent) # check if ordered by Knapsack (decreasing order)
CIdata2001_75_percent_SURVEYED <- datatable_CIdata2001_75_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2001_75_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2001_75_percent_SURVEYED, file="CIdata2001_75_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)

##### 2001_2003_75_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2005_75 
class(prediction_veg_elev_2005_75) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2003_75_percent)) {
  cat(round(100*i/nrow(CIdata2003_75_percent),digits=2),"%\n",sep="")
  x = CIdata2003_75_percent[i,"avg_X_MARK"]
  y = CIdata2003_75_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2005_75,matrix(data = c(x,y),ncol=2))))
}
CIdata2003_75_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2003_75_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2003_75_percent <- CIdata2003_75_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2003_75_percent$Profit))
# read in cost file 
cost <- CIdata2003_75_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2003_75_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2003_75_percent <- as.data.table(CIdata2003_75_percent)
setorder(datatable_CIdata2003_75_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2003_75_percent) # check if ordered by Knapsack (decreasing order)
CIdata2003_75_percent_SURVEYED <- datatable_CIdata2003_75_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2003_75_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2003_75_percent_SURVEYED, file="CIdata2003_75_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)

##### 2001_2003_2005_75_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2007_75 
class(prediction_veg_elev_2007_75) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2005_75_percent)) {
  cat(round(100*i/nrow(CIdata2005_75_percent),digits=2),"%\n",sep="")
  x = CIdata2005_75_percent[i,"avg_X_MARK"]
  y = CIdata2005_75_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2007_75,matrix(data = c(x,y),ncol=2))))
}
CIdata2005_75_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2005_75_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2005_75_percent <- CIdata2005_75_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2005_75_percent$Profit))
# read in cost file 
cost <- CIdata2005_75_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2005_75_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2005_75_percent <- as.data.table(CIdata2005_75_percent)
setorder(datatable_CIdata2005_75_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2005_75_percent) # check if ordered by Knapsack (decreasing order)
CIdata2005_75_percent_SURVEYED <- datatable_CIdata2005_75_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2005_75_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2005_75_percent_SURVEYED, file="CIdata2005_75_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)

##### 2001_2003_2005_2007_75_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2009_75 
class(prediction_veg_elev_2009_75) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2007_75_percent)) {
  cat(round(100*i/nrow(CIdata2007_75_percent),digits=2),"%\n",sep="")
  x = CIdata2007_75_percent[i,"avg_X_MARK"]
  y = CIdata2007_75_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2009_75,matrix(data = c(x,y),ncol=2))))
}
CIdata2007_75_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2007_75_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2007_75_percent <- CIdata2007_75_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2007_75_percent$Profit))
# read in cost file 
cost <- CIdata2007_75_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2007_75_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2007_75_percent <- as.data.table(CIdata2007_75_percent)
setorder(datatable_CIdata2007_75_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2007_75_percent) # check if ordered by Knapsack (decreasing order)
CIdata2007_75_percent_SURVEYED <- datatable_CIdata2007_75_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2007_75_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2007_75_percent_SURVEYED, file="CIdata2007_75_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)

##### 2001_2003_2005_2007_2009_75_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2011_75 
class(prediction_veg_elev_2011_75) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2009_75_percent)) {
  cat(round(100*i/nrow(CIdata2009_75_percent),digits=2),"%\n",sep="")
  x = CIdata2009_75_percent[i,"avg_X_MARK"]
  y = CIdata2009_75_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2011_75,matrix(data = c(x,y),ncol=2))))
}
CIdata2009_75_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2009_75_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2009_75_percent <- CIdata2009_75_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2009_75_percent$Profit))
# read in cost file 
cost <- CIdata2009_75_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2009_75_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2009_75_percent <- as.data.table(CIdata2009_75_percent)
setorder(datatable_CIdata2009_75_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2009_75_percent) # check if ordered by Knapsack (decreasing order)
CIdata2009_75_percent_SURVEYED <- datatable_CIdata2009_75_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2009_75_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2009_75_percent_SURVEYED, file="CIdata2009_75_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)

##### 2001_2003_2005_2007_2009_2011_75_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2013_75 
class(prediction_veg_elev_2013_75) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2011_75_percent)) {
  cat(round(100*i/nrow(CIdata2011_75_percent),digits=2),"%\n",sep="")
  x = CIdata2011_75_percent[i,"avg_X_MARK"]
  y = CIdata2011_75_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2013_75,matrix(data = c(x,y),ncol=2))))
}
CIdata2011_75_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2011_75_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2011_75_percent <- CIdata2011_75_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2011_75_percent$Profit))
# read in cost file 
cost <- CIdata2011_75_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2011_75_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2011_75_percent <- as.data.table(CIdata2011_75_percent)
setorder(datatable_CIdata2011_75_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2011_75_percent) # check if ordered by Knapsack (decreasing order)
CIdata2011_75_percent_SURVEYED <- datatable_CIdata2011_75_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2011_75_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2011_75_percent_SURVEYED, file="CIdata2011_75_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)

##### 2001_2003_2005_2007_2009_2011_2013_75_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2015_75 
class(prediction_veg_elev_2015_75) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2013_75_percent)) {
  cat(round(100*i/nrow(CIdata2013_75_percent),digits=2),"%\n",sep="")
  x = CIdata2013_75_percent[i,"avg_X_MARK"]
  y = CIdata2013_75_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2015_75,matrix(data = c(x,y),ncol=2))))
}
CIdata2013_75_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2013_75_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2013_75_percent <- CIdata2013_75_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2013_75_percent$Profit))
# read in cost file 
cost <- CIdata2013_75_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2013_75_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2013_75_percent <- as.data.table(CIdata2013_75_percent)
setorder(datatable_CIdata2013_75_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2013_75_percent) # check if ordered by Knapsack (decreasing order)
CIdata2013_75_percent_SURVEYED <- datatable_CIdata2013_75_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2013_75_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2013_75_percent_SURVEYED, file="CIdata2013_75_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)


################################################ GLM fit to 100% of data ##################################################################### 

##### 2001_100_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2003_100 
class(prediction_veg_elev_2003_100) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2001_100_percent)) {
  cat(round(100*i/nrow(CIdata2001_100_percent),digits=2),"%\n",sep="")
  x = CIdata2001_100_percent[i,"avg_X_MARK"]
  y = CIdata2001_100_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2003_100,matrix(data = c(x,y),ncol=2))))
}
CIdata2001_100_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2001_100_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2001_100_percent <- CIdata2001_100_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2001_100_percent$Profit))
# read in cost file 
cost <- CIdata2001_100_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2001_100_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2001_100_percent <- as.data.table(CIdata2001_100_percent)
setorder(datatable_CIdata2001_100_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2001_100_percent) # check if ordered by Knapsack (decreasing order)
CIdata2001_100_percent_SURVEYED <- datatable_CIdata2001_100_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2001_100_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2001_100_percent_SURVEYED, file="CIdata2001_100_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)

##### 2001_2003_100_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2005_100 
class(prediction_veg_elev_2005_100) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2003_100_percent)) {
  cat(round(100*i/nrow(CIdata2003_100_percent),digits=2),"%\n",sep="")
  x = CIdata2003_100_percent[i,"avg_X_MARK"]
  y = CIdata2003_100_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2005_100,matrix(data = c(x,y),ncol=2))))
}
CIdata2003_100_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2003_100_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2003_100_percent <- CIdata2003_100_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2003_100_percent$Profit))
# read in cost file 
cost <- CIdata2003_100_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2003_100_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2003_100_percent <- as.data.table(CIdata2003_100_percent)
setorder(datatable_CIdata2003_100_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2003_100_percent) # check if ordered by Knapsack (decreasing order)
CIdata2003_100_percent_SURVEYED <- datatable_CIdata2003_100_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2003_100_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2003_100_percent_SURVEYED, file="CIdata2003_100_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)

##### 2001_2003_2005_100_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2007_100 
class(prediction_veg_elev_2007_100) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2005_100_percent)) {
  cat(round(100*i/nrow(CIdata2005_100_percent),digits=2),"%\n",sep="")
  x = CIdata2005_100_percent[i,"avg_X_MARK"]
  y = CIdata2005_100_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2007_100,matrix(data = c(x,y),ncol=2))))
}
CIdata2005_100_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2005_100_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2005_100_percent <- CIdata2005_100_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2005_100_percent$Profit))
# read in cost file 
cost <- CIdata2005_100_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2005_100_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2005_100_percent <- as.data.table(CIdata2005_100_percent)
setorder(datatable_CIdata2005_100_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2005_100_percent) # check if ordered by Knapsack (decreasing order)
CIdata2005_100_percent_SURVEYED <- datatable_CIdata2005_100_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2005_100_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2005_100_percent_SURVEYED, file="CIdata2005_100_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)

##### 2001_2003_2005_2007_100_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2009_100 
class(prediction_veg_elev_2009_100) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2007_100_percent)) {
  cat(round(100*i/nrow(CIdata2007_100_percent),digits=2),"%\n",sep="")
  x = CIdata2007_100_percent[i,"avg_X_MARK"]
  y = CIdata2007_100_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2009_100,matrix(data = c(x,y),ncol=2))))
}
CIdata2007_100_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2007_100_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2007_100_percent <- CIdata2007_100_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2007_100_percent$Profit))
# read in cost file 
cost <- CIdata2007_100_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2007_100_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2007_100_percent <- as.data.table(CIdata2007_100_percent)
setorder(datatable_CIdata2007_100_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2007_100_percent) # check if ordered by Knapsack (decreasing order)
CIdata2007_100_percent_SURVEYED <- datatable_CIdata2007_100_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2007_100_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2007_100_percent_SURVEYED, file="CIdata2007_100_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)

##### 2001_2003_2005_2007_2009_100_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2011_100 
class(prediction_veg_elev_2011_100) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2009_100_percent)) {
  cat(round(100*i/nrow(CIdata2009_100_percent),digits=2),"%\n",sep="")
  x = CIdata2009_100_percent[i,"avg_X_MARK"]
  y = CIdata2009_100_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2011_100,matrix(data = c(x,y),ncol=2))))
}
CIdata2009_100_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2009_100_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2009_100_percent <- CIdata2009_100_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2009_100_percent$Profit))
# read in cost file 
cost <- CIdata2009_100_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2009_100_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2009_100_percent <- as.data.table(CIdata2009_100_percent)
setorder(datatable_CIdata2009_100_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2009_100_percent) # check if ordered by Knapsack (decreasing order)
CIdata2009_100_percent_SURVEYED <- datatable_CIdata2009_100_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2009_100_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2009_100_percent_SURVEYED, file="CIdata2009_100_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)

##### 2001_2003_2005_2007_2009_2011_100_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2013_100 
class(prediction_veg_elev_2013_100) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2011_100_percent)) {
  cat(round(100*i/nrow(CIdata2011_100_percent),digits=2),"%\n",sep="")
  x = CIdata2011_100_percent[i,"avg_X_MARK"]
  y = CIdata2011_100_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2013_100,matrix(data = c(x,y),ncol=2))))
}
CIdata2011_100_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2011_100_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2011_100_percent <- CIdata2011_100_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2011_100_percent$Profit))
# read in cost file 
cost <- CIdata2011_100_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2011_100_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2011_100_percent <- as.data.table(CIdata2011_100_percent)
setorder(datatable_CIdata2011_100_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2011_100_percent) # check if ordered by Knapsack (decreasing order)
CIdata2011_100_percent_SURVEYED <- datatable_CIdata2011_100_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2011_100_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2011_100_percent_SURVEYED, file="CIdata2011_100_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)

##### 2001_2003_2005_2007_2009_2011_2013_100_percent
# Define profit and cost per survey grid cell
prediction_veg_elev_2015_100 
class(prediction_veg_elev_2015_100) # check 
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata2013_100_percent)) {
  cat(round(100*i/nrow(CIdata2013_100_percent),digits=2),"%\n",sep="")
  x = CIdata2013_100_percent[i,"avg_X_MARK"]
  y = CIdata2013_100_percent[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(prediction_veg_elev_2015_100,matrix(data = c(x,y),ncol=2))))
}
CIdata2013_100_percent$Profit <- profit # add profit values to dataframe 
tmpID <- which(is.na(CIdata2013_100_percent$Profit)) # check if any NAs in P(occ)
if (length(tmpID) > 0) CIdata2013_100_percent <- CIdata2013_100_percent[-tmpID, ] # remove rows with NA for P(occ) from dataset
if (length(tmpID) > 0) profit <- profit[-tmpID]
which(is.na(CIdata2013_100_percent$Profit))
# read in cost file 
cost <- CIdata2013_100_percent$COST_Travel_plus_Survey_rescaled 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata2013_100_percent$Knapsack_profit_per_cost <- knapsack
# Order sites in decreasing Knapsack value 
datatable_CIdata2013_100_percent <- as.data.table(CIdata2013_100_percent)
setorder(datatable_CIdata2013_100_percent, -Knapsack_profit_per_cost) 
head(datatable_CIdata2013_100_percent) # check if ordered by Knapsack (decreasing order)
CIdata2013_100_percent_SURVEYED <- datatable_CIdata2013_100_percent[cumsum(COST_Travel_plus_Survey_rescaled) <= CIdata2013_100_percent_MANAGEMENT_BUDGET, ] # go to sites for which COST lies within management budget (caluclated in previous script)
# Write table for model validation (SC evaluation)
write.table(CIdata2013_100_percent_SURVEYED, file="CIdata2013_100_percent_SURVEYED_sitesTOmanage.txt", sep="\t",col.names=T,row.names=F) # this saves a table with the sites that can be managed (those sites lie within the management budget)
