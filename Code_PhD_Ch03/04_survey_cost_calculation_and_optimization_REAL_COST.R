## Load required packages
library(data.table)
library(adagio)

## Declare some vectors and lists 
profit <- vector()
predictions <- c("prediction_to_2001.Rda","prediction_to_2003.Rda", "prediction_to_2005.Rda", "prediction_to_2007.Rda", 
                 "prediction_to_2009.Rda", "prediction_to_2011.Rda","prediction_to_2013.Rda", "prediction_to_2015.Rda")
full_datasets <- c("CIdata2001","CIdata2003", "CIdata2005","CIdata2007","CIdata2009", "CIdata2011", "CIdata2013","CIdata2015")
dataframes_with_R_vs_C_value_REAL_COST <- list()

## For prediction to multiple CIdataYEAR - add profit column to CIdataYEAR (we only need same # WPTs to calculate P(occ) and cost)
for (j in 1:length(full_datasets)) {
  load(predictions[j])
  profit <- prediction # get profit (probability of occurrence) values for each survey point
  df <- get(full_datasets[j])
  df <- cbind(df, profit) # add profit values to dataframe 
  tmpID <- which(is.na(df$profit)) # check if any NAs in P(occ)
  if (length(tmpID) > 0) df <- df[-tmpID, ] # remove rows with NA for P(occ) from dataset
  if (length(tmpID) > 0) profit <- profit[-tmpID]
  which(is.na(df$profit)) 
  cost <- round(10*(df$COST_Travel_plus_Survey)) # read in cost file - rounded and multiplied by 100 to make it consistent with knapsack calculations 
  R_vs_C <- profit/cost # reward vs cost for each survey point
  df$profit_per_cost <- R_vs_C # add column 
  dataframes_with_R_vs_C_value_REAL_COST[[j]] <- df # add to list
  save(dataframes_with_R_vs_C_value_REAL_COST, file="dataframes_with_R_vs_C_values_REAL_COST.Rda") # dataframes_with_R_vs_C_values
  # is a list with 8 elements, each element containing values for p(occ) predicted to a specific year,
  # corresponding with that year that has been excluded from the dataset to train the model  
} 

## Define total budget and fractions of total budget to run the optimisation 
df <- dataframes_with_R_vs_C_value_REAL_COST[[8]] # choose CIdata2015 for total budget calculation 
total_cost <- round(10*(sum(df$COST_Travel_plus_Survey, na.rm=TRUE))) # define total budget as the cost to survey the entire island in 2015
total_budget_25_percent <- 0.25 * total_cost # 25% of budget
total_budget_50_percent <- 0.5 * total_cost # 50 % of budget
total_budget_75_percent <- 0.75 * total_cost # 75 % of budget

## Sites are ranked based on different algorithms 
## Knapsack algorithm 
for (k in 1:length(full_datasets)){
  df <- dataframes_with_R_vs_C_value_REAL_COST[[k]] # load dataframe
  w <- round(10*(as.vector(df$COST_Travel_plus_Survey))) # vector with weights (i.e. survey cost) for each site
  b <- as.vector(df$profit) # define profit (i.e. probability of supercolony occurrence) associated with each site
  # 25% of total budget available 
  cap <- total_budget_25_percent # define total budget 
  (is <- knapsack(w, b, cap)) # run knapsack algorithm to identify most cost-effective sites 
  kk <- is$indices # indices of most cost-effective sites 
  WPTs_25_percent_KNAPSACK <- df$WPT[kk] # select WPTs choosen by knapsack algorithm 
  save(WPTs_25_percent_KNAPSACK, file=paste("WPTs_for_KNAPSACK_",full_datasets[k],"_25_percent_budget.Rda", sep="")) # save WPTs choosen by knapsack algorithm 
  pr <- 1 - prod(1 - b[kk]) # this is the probability of finding a supercolony at a single site (makes more sense than summing rewards)
  save(pr, file=paste("Probability_finding_SC_at_any_site_", full_datasets[k], "_25_percent.Rda", sep=""))
  # 50% of total budget available 
  cap <- total_budget_50_percent # define total budget
  (is <- knapsack(w, b, cap)) # run knapsack algorithm to identify most cost-effective sites
  kk <- is$indices # indices of most cost-effective sites
  WPTs_50_percent_KNAPSACK <- df$WPT[kk] # select WPTs choosen by knapsack algorithm 
  save(WPTs_50_percent_KNAPSACK, file=paste("WPTs_for_KNAPSACK_",full_datasets[k],"_50_percent_budget.Rda", sep=""))
  pr <- 1 - prod(1 - b[kk]) # this is the probability of finding a supercolony at a single site (makes more sense than summing rewards)
  save(pr, file=paste("Probability_finding_SC_at_any_site_", full_datasets[k], "_50_percent.Rda", sep=""))
  # 75% of total budget available 
  cap <- total_budget_75_percent # select WPTs choosen by knapsack algorithm
  (is <- knapsack(w, b, cap)) # run knapsack algorithm to identify most cost-effective sites
  kk <- is$indices# indices of most cost-effective sites
  WPTs_75_percent_KNAPSACK <- df$WPT[kk] # select WPTs choosen by knapsack algorithm 
  save(WPTs_75_percent_KNAPSACK, file=paste("WPTs_for_KNAPSACK_",full_datasets[k],"_75_percent_budget.Rda", sep=""))
  pr <- 1 - prod(1 - b[kk]) # this is the probability of finding a supercolony at a single site (makes more sense than summing rewards)
  save(pr, file=paste("Probability_finding_SC_at_any_site_", full_datasets[k], "_75_percent.Rda", sep=""))
}

## sites selected by efficiency (i.e. probability of occurrence of supercolony versus survey cost) 
for (k in 1:length(full_datasets)){
  df <- dataframes_with_R_vs_C_value_REAL_COST[[k]]
  df$COST_Travel_plus_Survey <- round(10*(df$COST_Travel_plus_Survey)) # rescale cost column to be consistent with Knapsack calculation
  df <- setorder(as.data.table(df), -profit_per_cost) # Order sites in decreasing Knapsack value
  df_R_vs_C_25_percent <- df[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ]
  df_R_vs_C_50_percent <- df[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ]
  df_R_vs_C_75_percent <- df[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
  WPTs_R_vs_C_25_percent <- df_R_vs_C_25_percent$WPT
  save(WPTs_R_vs_C_25_percent, file=paste("R_vs_C_WPTs_25_percent_budget_",full_datasets[k],".Rda",sep=""))
  WPTs_R_vs_C_50_percent <- df_R_vs_C_50_percent$WPT
  save(WPTs_R_vs_C_50_percent, file=paste("R_vs_C_WPTs_50_percent_budget_",full_datasets[k],".Rda",sep=""))
  WPTs_R_vs_C_75_percent <- df_R_vs_C_75_percent$WPT
  save(WPTs_R_vs_C_75_percent, file=paste("R_vs_C_WPTs_75_percent_budget_",full_datasets[k],".Rda",sep=""))
  WPTs_R_vs_C_100_percent <- df$WPT
  save(WPTs_R_vs_C_100_percent, file=paste("R_vs_C_WPTs_100_percent_budget_",full_datasets[k],".Rda",sep=""))
}

## Sites selected by probability of occurrence of supercolony 
for (k in 1:length(full_datasets)){
  df <- dataframes_with_R_vs_C_value_REAL_COST[[k]]
  df$COST_Travel_plus_Survey <- round(10*(df$COST_Travel_plus_Survey)) # rescale cost column to be consistent with Knapsack calculation
  df <- setorder(as.data.table(df), -profit) # Order sites in decreasing P(occ) value
  df_Pocc_25_percent <- df[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ]
  df_Pocc_50_percent <- df[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ]
  df_Pocc_75_percent <- df[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
  WPTs_Pocc_25_percent <- df_Pocc_25_percent$WPT
  save(WPTs_Pocc_25_percent, file=paste("Pocc_WPTs_25_percent_budget_",full_datasets[k],".Rda",sep=""))
  WPTs_Pocc_50_percent <- df_Pocc_50_percent$WPT
  save(WPTs_Pocc_50_percent, file=paste("Pocc_WPTs_50_percent_budget_",full_datasets[k],".Rda",sep=""))
  WPTs_Pocc_75_percent <- df_Pocc_75_percent$WPT
  save(WPTs_Pocc_75_percent, file=paste("Pocc_WPTs_75_percent_budget_",full_datasets[k],".Rda",sep=""))
  WPTs_Pocc_100_percent <- df$WPT
  save(WPTs_Pocc_100_percent, file=paste("Pocc_WPTs_100_percent_budget_",full_datasets[k],".Rda",sep=""))
}

## Sites selected based on expert opinion 
# Dion: only survey sites around the edges (low elevation) and survey a small number of sites on the plateau
# The df_remaining can be outcommented since the first pick of sites happens on highest elevation, whereas the second pick on lowest elevation
for (k in 1:length(full_datasets)){
  df <- dataframes_with_R_vs_C_value_REAL_COST[[k]]
  df$COST_Travel_plus_Survey <- round(10*(df$COST_Travel_plus_Survey))
  df <- setorder(as.data.table(df), Elevation) # Order sites based on increasing Elevation (from low to high elevation)
  df_Elevation_random_pick_above_250m_25_percent <- df[sample(which(Elevation>250), size=25), ] # randomly picks 25 sites on the plateau (above 250m)
  df_Elevation_random_pick_above_250m_25_percent # those represent the sites that will be surveyed on the plateau
  #df_remaining <- df[-df_Elevation_random_pick_above_250m_25_percent$WPT, ]
  #df_remaining # those represent remaining sites after 25 sites were removed on the plateau 
  cost_survey_25_plateau_sites <- sum(df_Elevation_random_pick_above_250m_25_percent$COST_Travel_plus_Survey)  
  cost_survey_25_plateau_sites # cost to survey 25 plateau sites
  remaining_budget_25_percent <- total_budget_25_percent - cost_survey_25_plateau_sites # this is the remaining budget that can be used to survey sites at lower elevation
  remaining_budget_25_percent
  df_Elevation_edges_25_percent_of_budget <- df[cumsum(COST_Travel_plus_Survey)<=remaining_budget_25_percent, ] # extract those sites around the edges (lowest elevation) for which cost to survey lies within the remaining budget
  df_Elevation_edges_25_percent_of_budget
  WPTs_ExpOp_25_percent <- c(df_Elevation_random_pick_above_250m_25_percent$WPT, df_Elevation_edges_25_percent_of_budget$WPT) 
  save(WPTs_ExpOp_25_percent, file=paste("ExpOp_WPTs_25_percent_budget_",full_datasets[k],".Rda",sep=""))
  df_Elevation_random_pick_above_250m_50_percent <- df[sample(which(Elevation>250), size=50), ] # randomly picks 50 sites on the plateau (above 250m)
  df_Elevation_random_pick_above_250m_50_percent # those represent the sites that will be surveyed on the plateau
  #df_remaining <- df[-df_Elevation_random_pick_above_250m_50_percent$WPT, ]
  #df_remaining # those represent remaining sites after 25 sites were removed on the plateau 
  cost_survey_50_plateau_sites <- sum(df_Elevation_random_pick_above_250m_50_percent$COST_Travel_plus_Survey)  
  cost_survey_50_plateau_sites # cost to survey 25 plateau sites
  remaining_budget_50_percent <- total_budget_50_percent - cost_survey_50_plateau_sites # this is the remaining budget that can be used to survey sites at lower elevation
  remaining_budget_50_percent
  df_Elevation_edges_50_percent_of_budget <- df[cumsum(COST_Travel_plus_Survey)<=remaining_budget_50_percent, ] # extract those sites around the edges (lowest elevation) for which cost to survey lies within the remaining budget
  df_Elevation_edges_50_percent_of_budget
  WPTs_ExpOp_50_percent <- c(df_Elevation_random_pick_above_250m_50_percent$WPT, df_Elevation_edges_50_percent_of_budget$WPT) 
  save(WPTs_ExpOp_50_percent, file=paste("ExpOp_WPTs_50_percent_budget_",full_datasets[k],".Rda",sep=""))
  df_Elevation_random_pick_above_250m_75_percent <- df[sample(which(Elevation>250), size=75), ] # randomly picks 75 sites on the plateau (above 250m)
  df_Elevation_random_pick_above_250m_75_percent # those represent the sites that will be surveyed on the plateau
  #df_remaining <- df[-df_Elevation_random_pick_above_250m_75_percent$WPT, ]
  #df_remaining # those represent remaining sites after 25 sites were removed on the plateau 
  cost_survey_75_plateau_sites <- sum(df_Elevation_random_pick_above_250m_75_percent$COST_Travel_plus_Survey)  
  cost_survey_75_plateau_sites # cost to survey 25 plateau sites
  remaining_budget_75_percent <- total_budget_75_percent - cost_survey_75_plateau_sites # this is the remaining budget that can be used to survey sites at lower elevation
  remaining_budget_75_percent
  df_Elevation_edges_75_percent_of_budget <- df[cumsum(COST_Travel_plus_Survey)<=remaining_budget_75_percent, ] # extract those sites around the edges (lowest elevation) for which cost to survey lies within the remaining budget
  df_Elevation_edges_75_percent_of_budget
  WPTs_ExpOp_75_percent <- c(df_Elevation_random_pick_above_250m_75_percent$WPT, df_Elevation_edges_75_percent_of_budget$WPT) 
  save(WPTs_ExpOp_75_percent, file=paste("ExpOp_WPTs_75_percent_budget_",full_datasets[k],".Rda",sep=""))
  WPTs_ExpOp_100_percent <- df$WPT
  save(WPTs_ExpOp_100_percent, file=paste("ExpOp_WPTs_100_percent_budget_",full_datasets[k],".Rda",sep=""))
}   


## Evenly distributed sites 
for (k in 1:length(full_datasets)){
  Sequence_1 <- seq(0, nrow(dataframes_with_R_vs_C_value_REAL_COST[[k]]), 4)
  Sequence_2 <- seq(1, nrow(dataframes_with_R_vs_C_value_REAL_COST[[k]]), 4)
  Sequence_3 <- seq(2, nrow(dataframes_with_R_vs_C_value_REAL_COST[[k]]), 4)
  Reordered_sites <- c(Sequence_1, Sequence_2, Sequence_3)
  df <- dataframes_with_R_vs_C_value_REAL_COST[[k]]
  df$COST_Travel_plus_Survey <- round(10*(df$COST_Travel_plus_Survey))
  df <- setorder(as.data.table(df), WPT) # Order sites by WPT
  df_1 <- df[Reordered_sites, ]
  df_Syst_25_percent <- df_1[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ]
  WPTs_Syst_25_percent <- df_Syst_25_percent$WPT
  save(WPTs_Syst_25_percent, file=paste("Syst_WPTs_25_percent_budget_",full_datasets[k],".Rda",sep=""))
  df_Syst_50_percent <- df_1[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ]
  WPTs_Syst_50_percent <- df_Syst_50_percent$WPT
  save(WPTs_Syst_50_percent, file=paste("Syst_WPTs_50_percent_budget_",full_datasets[k],".Rda",sep=""))
  df_Syst_75_percent <- df_1[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
  WPTs_Syst_75_percent <- df_Syst_75_percent$WPT
  save(WPTs_Syst_75_percent, file=paste("Syst_WPTs_75_percent_budget_",full_datasets[k],".Rda",sep=""))
  WPTs_Syst_100_percent <- df$WPT
  save(WPTs_Syst_100_percent, file=paste("Syst_WPTs_100_percent_budget_",full_datasets[k],".Rda",sep=""))
}


# for (k in 1:length(full_datasets)){
#   Sequence_1 <- seq(0, nrow(dataframes_with_R_vs_C_value_REAL_COST[[k]]), 3)
#   Sequence_2 <- seq(1, nrow(dataframes_with_R_vs_C_value_REAL_COST[[k]]), 3)
#   Sequence_3 <- seq(2, nrow(dataframes_with_R_vs_C_value_REAL_COST[[k]]), 3)
#   df <- dataframes_with_R_vs_C_value_REAL_COST[[k]]
#   df$COST_Travel_plus_Survey <- round(10*(df$COST_Travel_plus_Survey))
#   df <- setorder(as.data.table(df), WPT) # Order sites by WPT
#   df_1 <- df[Sequence_1, ] 
#   df_Syst_25_percent <- df_1[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ]
#   WPTs_Syst_25_percent <- df_Syst_25_percent$WPT
#   save(WPTs_Syst_25_percent, file=paste("Syst_WPTs_25_percent_budget_",full_datasets[k],".Rda",sep=""))
#   df_2 <- df[Sequence_2, ]
#   df_2 <- rbind(df_1, df_2)
#   df_Syst_50_percent <- df_2[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ]
#   WPTs_Syst_50_percent <- df_Syst_50_percent$WPT
#   save(WPTs_Syst_50_percent, file=paste("Syst_WPTs_50_percent_budget_",full_datasets[k],".Rda",sep=""))
#   df_3 <- df[Sequence_3, ]
#   df_3 <- rbind(df_2, df_3)
#   df_Syst_75_percent <- df_3[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
#   WPTs_Syst_75_percent <- df_Syst_75_percent$WPT
#   save(WPTs_Syst_75_percent, file=paste("Syst_WPTs_75_percent_budget_",full_datasets[k],".Rda",sep=""))
#   WPTs_Syst_100_percent <- df$WPT
#   save(WPTs_Syst_100_percent, file=paste("Syst_WPTs_100_percent_budget_",full_datasets[k],".Rda",sep=""))
# }


## Randomly distributed sites  
for (k in 1:length(full_datasets)){
  df <- as.data.table(dataframes_with_R_vs_C_value_REAL_COST[[k]])
  df$COST_Travel_plus_Survey <- round(10*(df$COST_Travel_plus_Survey))
  kk <- sample(nrow(dataframes_with_R_vs_C_value_REAL_COST[[k]]))
  dk <- df[kk, ] # Randomly shuffle sites (different sites for each loop)
  df_Random_25_percent <- dk[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ]
  WPTs_Random_25_percent <- df_Random_25_percent$WPT
  save(WPTs_Random_25_percent, file=paste("Random_WPTs_25_percent_budget_",full_datasets[k],".Rda",sep=""))
  df_Random_50_percent <- dk[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ]
  WPTs_Random_50_percent <- df_Random_50_percent$WPT
  save(WPTs_Random_50_percent, file=paste("Random_WPTs_50_percent_budget_",full_datasets[k],".Rda",sep=""))
  df_Random_75_percent <- dk[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
  WPTs_Random_75_percent <- df_Random_75_percent$WPT
  save(WPTs_Random_75_percent, file=paste("Random_WPTs_75_percent_budget_",full_datasets[k],".Rda",sep=""))
  WPTs_Random_100_percent <- df$WPT
  save(WPTs_Random_100_percent, file=paste("Random_WPTs_100_percent_budget_",full_datasets[k],".Rda",sep=""))
}


## Random survey 
#kk <- sample(850) # this number was originally 850, derived from the fact that all years have at least 850 waypoints
#for (k in 1:length(full_datasets)){
#  df <- as.data.table(dataframes_with_R_vs_C_value[[k]])
#  dk <- df[kk, ] # Randomly shuffle sites (same sites for each loop)
#  df_Random_25_percent <- dk[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ]
#  WPTs_Random_25_percent <- df_Random_25_percent$WPT
#  save(WPTs_Random_25_percent, file=paste("Random_WPTs_25_percent_budget_",full_datasets[k],".Rda",sep=""))
#  df_Random_50_percent <- dk[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ]
#  WPTs_Random_50_percent <- df_Random_50_percent$WPT
#  save(WPTs_Random_50_percent, file=paste("Random_WPTs_50_percent_budget_",full_datasets[k],".Rda",sep=""))
#  df_Random_75_percent <- dk[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
#  WPTs_Random_75_percent <- df_Random_75_percent$WPT
#  save(WPTs_Random_75_percent, file=paste("Random_WPTs_75_percent_budget_",full_datasets[k],".Rda",sep=""))
#  WPTs_Random_100_percent <- df$WPT
#  save(WPTs_Random_100_percent, file=paste("Random_WPTs_100_percent_budget_",full_datasets[k],".Rda",sep=""))
#}




################################# Archive - predictions with rasters and complicated files ############################

####################################### Calculate cost without 2001 ###################################################
# Define profit and cost per survey grid cell
profit_raster <- read.asc("prob_occ_SC_predicted_to_2001.asc") # this prediction is built without 2001 data 
raster_prob_occ <- raster(profit_raster) # change asci file into a raster file 
class(raster_prob_occ)
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata_without_2001)) {
  cat(round(10*i/nrow(CIdata_without_2001),digits=2),"%\n",sep="")
  x = CIdata_without_2001[i,"avg_X_MARK"]
  y = CIdata_without_2001[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(raster_prob_occ,matrix(data = c(x,y),ncol=2))))
}
CIdata_without_2001$Profit <- profit # add profit values to dataframe 
# read in cost file 
cost <- CIdata_without_2001$COST_Travel_plus_Survey 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata_without_2001$Knapsack_profit_per_cost <- knapsack

## Organize sites to survey for 25% - 50% - 75% and 100% of budget
CIdata_without_2001 <- CIdata_without_2001[order(CIdata_without_2001$WPT), ] # Order CIdata by WPT 
CIdata_without_2001 <- CIdata_without_2001[!duplicated(CIdata_without_2001$WPT), ] # Keep one row per waypoint
head(CIdata_without_2001)
write.table(CIdata_without_2001, file="CIdata_without_2001_with_Profit_Knapsack_per_WPT.txt", sep="\t",col.names=T,row.names=F)

## Total cost of surveying all 1018 sites
total_cost <- sum(CIdata_without_2001$COST_Travel_plus_Survey, na.rm=TRUE) # total cost to survey all sites once (= total budget)
total_cost
total_budget_25_percent <- 0.25*total_cost # 25% of budget
total_budget_50_percent <- 0.5*total_cost # 50 % of budget
total_budget_75_percent <- 0.75*total_cost # 75 % of budget
total_budget_100_percent <- total_cost # full budget

## Order sites based on probability of occurrence -> to do the survey based on decreasing Pr(occ), while including the cost
library(data.table) # use package data.table which wraps a data.table around a data.frame
datatable_CIdata_without_2001 <- as.data.table(CIdata_without_2001)
setorder(datatable_CIdata_without_2001, -Profit) # order CIdata.table by the Profit column in descending order (highest Pr(occ) first)
datatable_CIdata_without_2001 # check if ordered by Profit
# Recalculate total cost to survey 1017 sites (because NAs are removed)
total_cost <- sum(datatable_CIdata_without_2001$COST_Travel_plus_Survey)
total_cost
total_budget_25_percent <- 0.25*total_cost # 25% of budget
total_budget_50_percent <- 0.5*total_cost # 50 % of budget
total_budget_75_percent <- 0.75*total_cost # 75 % of budget
total_budget_100_percent <- total_cost # full budget
# Group sites for different budgets based on decreasing probability of occurrence 
CIdata_without_2001_Pr_Occ_25_percent_of_budget <- datatable_CIdata_without_2001[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ] # take the first x columns until 25% of budget is reached (sites ordered by Pr(occ))
CIdata_without_2001_Pr_Occ_50_percent_of_budget <- datatable_CIdata_without_2001[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ] 
CIdata_without_2001_Pr_Occ_75_percent_of_budget <- datatable_CIdata_without_2001[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
CIdata_without_2001_Pr_Occ_100_percent_of_budget <- datatable_CIdata_without_2001
write.table(CIdata_without_2001_Pr_Occ_25_percent_of_budget, file="CIdata_without_2001_Pr_Occ_25_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2001_Pr_Occ_50_percent_of_budget, file="CIdata_without_2001_Pr_Occ_50_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2001_Pr_Occ_75_percent_of_budget, file="CIdata_without_2001_Pr_Occ_75_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2001_Pr_Occ_100_percent_of_budget, file="CIdata_without_2001_Pr_Occ_100_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)

## Order sites based on Knapsack calculation (benefit/cost)
setorder(datatable_CIdata_without_2001, -Knapsack_profit_per_cost) # order CIdata.table by column Knapsack_profit_per_cost (highest profit/cost first)
datatable_CIdata_without_2001 # check if ordered by Knapsack
CIdata_without_2001_Knapsack_25_percent_of_budget <- datatable_CIdata_without_2001[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ] # subset CIdata.table with total_cost_25_percent
CIdata_without_2001_Knapsack_50_percent_of_budget <- datatable_CIdata_without_2001[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ]
CIdata_without_2001_Knapsack_75_percent_of_budget <- datatable_CIdata_without_2001[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
CIdata_without_2001_Knapsack_100_percent_of_budget <- datatable_CIdata_without_2001
write.table(CIdata_without_2001_Knapsack_25_percent_of_budget, file="CIdata_without_2001_Knapsack_25_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2001_Knapsack_50_percent_of_budget, file="CIdata_without_2001_Knapsack_50_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2001_Knapsack_75_percent_of_budget, file="CIdata_without_2001_Knapsack_75_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2001_Knapsack_100_percent_of_budget, file="CIdata_without_2001_Knapsack_100_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)

## Order sites based on expert opinion (managers' best guess):
# Dion: only survey sites around the edges (low elevation) and survey a small number of sites on the plateau
# Pete & Pinky: make the gridcells larger 
setorder(datatable_CIdata_without_2001, Elevation) # order CIdata.table by column Elevation in increasing order (from low elevation to high)
# 25% budget
CIdata_without_2001_Elevation_25_random_pick_above_250m <- datatable_CIdata_without_2001[sample(which(Elevation>250), size=25), ] # randomly pick 25 sites on the plateau (above 250m)
cost_survey_25_plateau_sites <- sum(CIdata_without_2001_Elevation_25_random_pick_above_250m$COST_Travel_plus_Survey)
cost_survey_25_plateau_sites
remaining_budget_25_percent <- total_budget_25_percent - cost_survey_25_plateau_sites # this is the remaining budget that can be used to survey sites at lower elevation
remaining_budget_25_percent
total_budget_25_percent # check
CIdata_without_2001_Elevation_25_percent_of_budget <- datatable_CIdata_without_2001[cumsum(COST_Travel_plus_Survey)<=remaining_budget_25_percent, ] # extract those rows with sites at lowest elevation for which cost to survey lies within the remaining budget
CIdata_without_2001_Elevation_25_percent_of_budget
write.table(CIdata_without_2001_Elevation_25_percent_of_budget, file="CIdata_without_2001_managers_guess_25_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
WPT_25_plateau <- CIdata_without_2001_Elevation_25_random_pick_above_250m$WPT # those sites will be surveyed on the plateau
WPT_25_plateau
WPT_25_low_elevation <- CIdata_without_2001_Elevation_25_percent_of_budget$WPT # those lower elevation sites will be surveyed
WPT_25_low_elevation
# 50% budget
CIdata_without_2001_Elevation_50_random_pick_above_250m <- datatable_CIdata_without_2001[sample(which(Elevation>250), size=50), ] # randomly pick 50 sites on the plateau (above 250m)
cost_survey_50_plateau_sites <- sum(CIdata_without_2001_Elevation_50_random_pick_above_250m$COST_Travel_plus_Survey)
cost_survey_50_plateau_sites
remaining_budget_50_percent <- total_budget_50_percent - cost_survey_50_plateau_sites # this is the remaining budget that can be used to survey sites at lower elevation
remaining_budget_50_percent
total_budget_50_percent # check
CIdata_without_2001_Elevation_50_percent_of_budget <- datatable_CIdata_without_2001[cumsum(COST_Travel_plus_Survey) <= remaining_budget_50_percent, ] # extract those rows with sites at lowest elevation for which cost to survey lies within the remaining budget
CIdata_without_2001_Elevation_50_percent_of_budget
write.table(CIdata_without_2001_Elevation_50_percent_of_budget, file="CIdata_without_2001_managers_guess_50_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
WPT_50_plateau <- CIdata_without_2001_Elevation_50_random_pick_above_250m$WPT # these sites will be surveyed on the plateau
WPT_50_plateau
WPT_50_low_elevation <- CIdata_without_2001_Elevation_50_percent_of_budget$WPT # these lower elevation sites will be surveyed
WPT_50_low_elevation
# 75% budget
CIdata_without_2001_Elevation_75_random_pick_above_250m <- datatable_CIdata_without_2001[sample(which(Elevation>250), size=75), ] # randomly pick 75 sites on the plateau (above 250m)
cost_survey_75_plateau_sites <- sum(CIdata_without_2001_Elevation_75_random_pick_above_250m$COST_Travel_plus_Survey)
cost_survey_75_plateau_sites
remaining_budget_75_percent <- total_budget_75_percent - cost_survey_75_plateau_sites
remaining_budget_75_percent
total_budget_75_percent # check
CIdata_without_2001_Elevation_75_percent_of_budget <- datatable_CIdata_without_2001[cumsum(COST_Travel_plus_Survey) <= remaining_budget_75_percent, ] # extract those rows with sites at lowest elevation for which cost to survey lies within the remaining budget
CIdata_without_2001_Elevation_75_percent_of_budget
write.table(CIdata_without_2001_Elevation_75_percent_of_budget, file="CIdata_without_2001_managers_guess_75_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
WPT_75_plateau <- CIdata_without_2001_Elevation_75_random_pick_above_250m$WPT # these sites will be surveyed on the plateau
WPT_75_plateau
WPT_75_low_elevation <- CIdata_without_2001_Elevation_75_percent_of_budget$WPT # these lower elevation sites will be surveyed
WPT_75_low_elevation
# 100% budget
CIdata_without_2001_Elevation_100_percent_of_budget <- datatable_CIdata_without_2001
write.table(CIdata_without_2001_Elevation_100_percent_of_budget, file="CIdata_without_2001_managers_guess_100_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)

## Order sites randomly
random_shuffled_sites <- datatable_CIdata_without_2001[sample(nrow(datatable_CIdata_without_2001)), ]
class(random_shuffled_sites)

random_sites_25_percent <- random_shuffled_sites[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ]
random_sites_25_percent 
total_budget_25_percent
sum(random_sites_25_percent$COST_Travel_plus_Survey, na.rm = TRUE) # check if total cost is 25% of total budget
write.table(random_sites_25_percent, file="CIdata_without_2001_random_sites_25_percent.txt", sep="\t",col.names=T,row.names=F)

random_sites_50_percent <- random_shuffled_sites[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ]
random_sites_50_percent
total_budget_50_percent
sum(random_sites_50_percent$COST_Travel_plus_Survey, na.rm = TRUE) # check if total cost is 50% of total budget
write.table(random_sites_50_percent, file="CIdata_without_2001_random_sites_50_percent.txt", sep="\t",col.names=T,row.names=F)

random_sites_75_percent <- random_shuffled_sites[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
random_sites_75_percent
total_budget_75_percent
sum(random_sites_75_percent$COST_Travel_plus, na.rm = TRUE) # check if total cost is 75% of total budget
write.table(random_sites_75_percent, file="CIdata_without_2001_random_sites_75_percent.txt", sep="\t",col.names=T,row.names=F)

random_sites_100_percent <- random_shuffled_sites
random_sites_100_percent
total_budget_100_percent
sum(random_sites_100_percent$COST_Travel_plus, na.rm = TRUE) # check if total cost is 100% of total budget
write.table(random_sites_100_percent, file="CIdata_without_2001_random_sites_100_percent.txt", sep="\t",col.names=T,row.names=F)


## Order sites systematically -> algorithm that selects sites systematically for 25% - 50% - 75% - 100% of budget 
head(datatable_CIdata_without_2001) # check if CIdata has one row per waypoint
CIdata_without_2001_systematic <- datatable_CIdata_without_2001[-(1001:1017), ] # remove last 18 sites from dataframe in order to make it possible to split 

# split dataframe in 100 small dataframes based on area (dataframe 1: site 1-10, dataframe 2: site 11-20, etc)
splitted_dataframe_without_2001 <- split(CIdata_without_2001_systematic, rep(1:100, each=10))
class(splitted_dataframe_without_2001) # check class; should be a list with 100 elements, containing 10 rows each  
# define list to store shuffled dataframes for further calculation 
systematic_sites_without_2001_25_percent <- list()
systematic_sites_without_2001_50_percent <- list()
systematic_sites_without_2001_75_percent <- list()
# create lists with systematically organised sites for different budgets 
for (i in 1:100){
  kk <- splitted_dataframe_without_2001[[i]] # take the first dataframe out of the list (containing first 10 sites)
  kk <- as.data.table(kk)
  shuffle_sites <- kk[sample(nrow(kk))] # randomize sites
  shuffle_sites_25_percent <- shuffle_sites[cumsum(COST_Travel_plus_Survey) <= 0.01 * total_budget_25_percent, ] # take a number of sites with survey budget = 0.25% or
  # total budget (and do that 100 times to have a total budget of 25% of total island) -> it is done this way
  # to ensure sites are sytematically chosen across the island 
  shuffle_sites_50_percent <- shuffle_sites[cumsum(COST_Travel_plus_Survey) <= 0.01 * total_budget_50_percent, ]
  shuffle_sites_75_percent <- shuffle_sites[cumsum(COST_Travel_plus_Survey) <= 0.01 * total_budget_75_percent, ]
  systematic_sites_without_2001_25_percent[[i]] <- shuffle_sites_25_percent
  systematic_sites_without_2001_50_percent[[i]] <- shuffle_sites_50_percent
  systematic_sites_without_2001_75_percent[[i]] <- shuffle_sites_75_percent
}
systematic_sites_without_2001_25_percent #check if list is filled with correct dataframes 
CIdata_without_2001_systematic_survey_25_percent_budget <- as.data.frame(do.call("rbind", systematic_sites_without_2001_25_percent)) # convert list of dataframes into one dataframe
write.table(CIdata_without_2001_systematic_survey_25_percent_budget, file="CIdata_without_2001_systematic_survey_25_percent_budget.txt", sep="\t",col.names=T,row.names=F)
systematic_sites_without_2001_50_percent
CIdata_without_2001_systematic_survey_50_percent_budget <- as.data.frame(do.call("rbind", systematic_sites_without_2001_50_percent))
write.table(CIdata_without_2001_systematic_survey_50_percent_budget, file="CIdata_without_2001_systematic_survey_50_percent_budget.txt", sep="\t",col.names=T,row.names=F)
systematic_sites_without_2001_75_percent
CIdata_without_2001_systematic_survey_75_percent_budget <- as.data.frame(do.call("rbind", systematic_sites_without_2001_75_percent))
write.table(CIdata_without_2001_systematic_survey_75_percent_budget, file="CIdata_without_2001_systematic_survey_75_percent_budget.txt", sep="\t",col.names=T,row.names=F)
CIdata_without_2001_systematic_survey_100_percent_budget <- datatable_CIdata_without_2001
write.table(CIdata_without_2001_systematic_survey_100_percent_budget, file="CIdata_without_2001_systematic_survey_100_percent_budget.txt", sep="\t",col.names=T,row.names=F)


####################################### Calculate cost without 2003 ###################################################
# Define profit and cost per survey grid cell
profit_raster <- read.asc("prob_occ_SC_predicted_to_2003.asc") # this prediction is built without 2001 data 
raster_prob_occ <- raster(profit_raster) # change asci file into a raster file 
class(raster_prob_occ)
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata_without_2003)) {
  cat(round(10*i/nrow(CIdata_without_2003),digits=2),"%\n",sep="")
  x = CIdata_without_2003[i,"avg_X_MARK"]
  y = CIdata_without_2003[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(raster_prob_occ,matrix(data = c(x,y),ncol=2))))
}
CIdata_without_2003$Profit <- profit # add profit values to dataframe 
# read in cost file 
cost <- CIdata_without_2003$COST_Travel_plus_Survey 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata_without_2003$Knapsack_profit_per_cost <- knapsack

## Organize sites to survey for 25% - 50% - 75% and 100% of budget
CIdata_without_2003 <- CIdata_without_2003[order(CIdata_without_2003$WPT), ] # Order CIdata by WPT 
CIdata_without_2003 <- CIdata_without_2003[!duplicated(CIdata_without_2003$WPT), ] # Keep one row per waypoint
head(CIdata_without_2003)
write.table(CIdata_without_2003, file="CIdata_without_2003_with_Profit_Knapsack_per_WPT.txt", sep="\t",col.names=T,row.names=F)

## Total cost of surveying all 1018 sites
total_cost <- sum(CIdata_without_2003$COST_Travel_plus_Survey, na.rm=TRUE) # total cost to survey all sites once (= total budget)
total_cost
total_budget_25_percent <- 0.25*total_cost # 25% of budget
total_budget_50_percent <- 0.5*total_cost # 50 % of budget
total_budget_75_percent <- 0.75*total_cost # 75 % of budget
total_budget_100_percent <- total_cost # full budget

## Order sites based on probability of occurrence -> to do the survey based on decreasing Pr(occ), while including the cost
library(data.table) # use package data.table which wraps a data.table around a data.frame
datatable_CIdata_without_2003 <- as.data.table(CIdata_without_2003)
setorder(datatable_CIdata_without_2003, -Profit) # order CIdata.table by the Profit column in descending order (highest Pr(occ) first)
datatable_CIdata_without_2003 # check if ordered by Profit
datatable_CIdata_without_2003 <- datatable_CIdata_without_2003[-1, ] # remove the first row because profit (= probability of occurrence is NA)
datatable_CIdata_without_2003 # check if NA in Profit and Knapsack row has disappeared
# Recalculate total cost to survey 1017 sites (because NAs are removed)
total_cost <- sum(datatable_CIdata_without_2003$COST_Travel_plus_Survey)
total_cost
total_budget_25_percent <- 0.25*total_cost # 25% of budget
total_budget_50_percent <- 0.5*total_cost # 50 % of budget
total_budget_75_percent <- 0.75*total_cost # 75 % of budget
total_budget_100_percent <- total_cost # full budget
# Group sites for different budgets based on decreasing probability of occurrence 
CIdata_without_2003_Pr_Occ_25_percent_of_budget <- datatable_CIdata_without_2003[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ] # take the first x columns until 25% of budget is reached (sites ordered by Pr(occ))
CIdata_without_2003_Pr_Occ_50_percent_of_budget <- datatable_CIdata_without_2003[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ] 
CIdata_without_2003_Pr_Occ_75_percent_of_budget <- datatable_CIdata_without_2003[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
CIdata_without_2003_Pr_Occ_100_percent_of_budget <- datatable_CIdata_without_2003
write.table(CIdata_without_2003_Pr_Occ_25_percent_of_budget, file="CIdata_without_2003_Pr_Occ_25_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2003_Pr_Occ_50_percent_of_budget, file="CIdata_without_2003_Pr_Occ_50_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2003_Pr_Occ_75_percent_of_budget, file="CIdata_without_2003_Pr_Occ_75_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2003_Pr_Occ_100_percent_of_budget, file="CIdata_without_2003_Pr_Occ_100_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)

## Order sites based on Knapsack calculation (benefit/cost)
setorder(datatable_CIdata_without_2003, -Knapsack_profit_per_cost) # order CIdata.table by column Knapsack_profit_per_cost (highest profit/cost first)
datatable_CIdata_without_2003 # check if ordered by Knapsack
CIdata_without_2003_Knapsack_25_percent_of_budget <- datatable_CIdata_without_2003[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ] # subset CIdata.table with total_cost_25_percent
CIdata_without_2003_Knapsack_50_percent_of_budget <- datatable_CIdata_without_2003[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ]
CIdata_without_2003_Knapsack_75_percent_of_budget <- datatable_CIdata_without_2003[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
CIdata_without_2003_Knapsack_100_percent_of_budget <- datatable_CIdata_without_2003
write.table(CIdata_without_2003_Knapsack_25_percent_of_budget, file="CIdata_without_2003_Knapsack_25_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2003_Knapsack_50_percent_of_budget, file="CIdata_without_2003_Knapsack_50_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2003_Knapsack_75_percent_of_budget, file="CIdata_without_2003_Knapsack_75_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2003_Knapsack_100_percent_of_budget, file="CIdata_without_2003_Knapsack_100_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)

## Order sites based on expert opinion (managers' best guess):
# Dion: only survey sites around the edges (low elevation) and survey a small number of sites on the plateau
# Pete & Pinky: make the gridcells larger 
setorder(datatable_CIdata_without_2003, Elevation) # order CIdata.table by column Elevation in increasing order (from low elevation to high)
# 25% budget
CIdata_without_2003_Elevation_25_random_pick_above_250m <- datatable_CIdata_without_2003[sample(which(Elevation>250), size=25), ] # randomly pick 25 sites on the plateau (above 250m)
cost_survey_25_plateau_sites <- sum(CIdata_without_2003_Elevation_25_random_pick_above_250m$COST_Travel_plus_Survey)
cost_survey_25_plateau_sites
remaining_budget_25_percent <- total_budget_25_percent - cost_survey_25_plateau_sites # this is the remaining budget that can be used to survey sites at lower elevation
remaining_budget_25_percent
total_budget_25_percent # check
CIdata_without_2003_Elevation_25_percent_of_budget <- datatable_CIdata_without_2003[cumsum(COST_Travel_plus_Survey)<=remaining_budget_25_percent, ] # extract those rows with sites at lowest elevation for which cost to survey lies within the remaining budget
CIdata_without_2003_Elevation_25_percent_of_budget
write.table(CIdata_without_2003_Elevation_25_percent_of_budget, file="CIdata_without_2003_managers_guess_25_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
WPT_25_plateau <- CIdata_without_2003_Elevation_25_random_pick_above_250m$WPT # those sites will be surveyed on the plateau
WPT_25_plateau
WPT_25_low_elevation <- CIdata_without_2003_Elevation_25_percent_of_budget$WPT # those lower elevation sites will be surveyed
WPT_25_low_elevation
# 50% budget
CIdata_without_2003_Elevation_50_random_pick_above_250m <- datatable_CIdata_without_2003[sample(which(Elevation>250), size=50), ] # randomly pick 50 sites on the plateau (above 250m)
cost_survey_50_plateau_sites <- sum(CIdata_without_2003_Elevation_50_random_pick_above_250m$COST_Travel_plus_Survey)
cost_survey_50_plateau_sites
remaining_budget_50_percent <- total_budget_50_percent - cost_survey_50_plateau_sites # this is the remaining budget that can be used to survey sites at lower elevation
remaining_budget_50_percent
total_budget_50_percent # check
CIdata_without_2003_Elevation_50_percent_of_budget <- datatable_CIdata_without_2003[cumsum(COST_Travel_plus_Survey) <= remaining_budget_50_percent, ] # extract those rows with sites at lowest elevation for which cost to survey lies within the remaining budget
CIdata_without_2003_Elevation_50_percent_of_budget
write.table(CIdata_without_2003_Elevation_50_percent_of_budget, file="CIdata_without_2003_managers_guess_50_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
WPT_50_plateau <- CIdata_without_2003_Elevation_50_random_pick_above_250m$WPT # these sites will be surveyed on the plateau
WPT_50_plateau
WPT_50_low_elevation <- CIdata_without_2003_Elevation_50_percent_of_budget$WPT # these lower elevation sites will be surveyed
WPT_50_low_elevation
# 75% budget
CIdata_without_2003_Elevation_75_random_pick_above_250m <- datatable_CIdata_without_2003[sample(which(Elevation>250), size=75), ] # randomly pick 75 sites on the plateau (above 250m)
cost_survey_75_plateau_sites <- sum(CIdata_without_2003_Elevation_75_random_pick_above_250m$COST_Travel_plus_Survey)
cost_survey_75_plateau_sites
remaining_budget_75_percent <- total_budget_75_percent - cost_survey_75_plateau_sites
remaining_budget_75_percent
total_budget_75_percent # check
CIdata_without_2003_Elevation_75_percent_of_budget <- datatable_CIdata_without_2003[cumsum(COST_Travel_plus_Survey) <= remaining_budget_75_percent, ] # extract those rows with sites at lowest elevation for which cost to survey lies within the remaining budget
CIdata_without_2003_Elevation_75_percent_of_budget
write.table(CIdata_without_2003_Elevation_75_percent_of_budget, file="CIdata_without_2003_managers_guess_75_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
WPT_75_plateau <- CIdata_without_2003_Elevation_75_random_pick_above_250m$WPT # these sites will be surveyed on the plateau
WPT_75_plateau
WPT_75_low_elevation <- CIdata_without_2003_Elevation_75_percent_of_budget$WPT # these lower elevation sites will be surveyed
WPT_75_low_elevation
# 100% budget
CIdata_without_2003_Elevation_100_percent_of_budget <- datatable_CIdata_without_2003
write.table(CIdata_without_2003_Elevation_100_percent_of_budget, file="CIdata_without_2003_managers_guess_100_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)

## Order sites randomly
random_shuffled_sites <- datatable_CIdata_without_2003[sample(nrow(datatable_CIdata_without_2003)), ]
class(random_shuffled_sites)

random_sites_25_percent <- random_shuffled_sites[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ]
random_sites_25_percent 
total_budget_25_percent
sum(random_sites_25_percent$COST_Travel_plus_Survey, na.rm = TRUE) # check if total cost is 25% of total budget
write.table(random_sites_25_percent, file="CIdata_without_2003_random_sites_25_percent.txt", sep="\t",col.names=T,row.names=F)

random_sites_50_percent <- random_shuffled_sites[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ]
random_sites_50_percent
total_budget_50_percent
sum(random_sites_50_percent$COST_Travel_plus_Survey, na.rm = TRUE) # check if total cost is 50% of total budget
write.table(random_sites_50_percent, file="CIdata_without_2003_random_sites_50_percent.txt", sep="\t",col.names=T,row.names=F)

random_sites_75_percent <- random_shuffled_sites[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
random_sites_75_percent
total_budget_75_percent
sum(random_sites_75_percent$COST_Travel_plus, na.rm = TRUE) # check if total cost is 75% of total budget
write.table(random_sites_75_percent, file="CIdata_without_2003_random_sites_75_percent.txt", sep="\t",col.names=T,row.names=F)

random_sites_100_percent <- random_shuffled_sites
random_sites_100_percent
total_budget_100_percent
sum(random_sites_100_percent$COST_Travel_plus, na.rm = TRUE) # check if total cost is 100% of total budget
write.table(random_sites_100_percent, file="CIdata_without_2003_random_sites_100_percent.txt", sep="\t",col.names=T,row.names=F)


## Order sites systematically -> algorithm that selects sites systematically for 25% - 50% - 75% - 100% of budget 
head(datatable_CIdata_without_2003) # check if CIdata has one row per waypoint
CIdata_without_2003_systematic <- datatable_CIdata_without_2003[-(1001:1017), ] # remove last 18 sites from dataframe in order to make it possible to split 

# split dataframe in 100 small dataframes based on area (dataframe 1: site 1-10, dataframe 2: site 11-20, etc)
splitted_dataframe_without_2003 <- split(CIdata_without_2003_systematic, rep(1:100, each=10))
class(splitted_dataframe_without_2003) # check class; should be a list with 100 elements, containing 10 rows each  
# define list to store shuffled dataframes for further calculation 
systematic_sites_without_2003_25_percent <- list()
systematic_sites_without_2003_50_percent <- list()
systematic_sites_without_2003_75_percent <- list()
# create lists with systematically organised sites for different budgets 
for (i in 1:100){
  kk <- splitted_dataframe_without_2003[[i]] # take the first dataframe out of the list (containing first 10 sites)
  kk <- as.data.table(kk)
  shuffle_sites <- kk[sample(nrow(kk))] # randomize sites
  shuffle_sites_25_percent <- shuffle_sites[cumsum(COST_Travel_plus_Survey) <= 0.01 * total_budget_25_percent, ] # take a number of sites with survey budget = 0.25% or
  # total budget (and do that 100 times to have a total budget of 25% of total island) -> it is done this way
  # to ensure sites are sytematically chosen across the island 
  shuffle_sites_50_percent <- shuffle_sites[cumsum(COST_Travel_plus_Survey) <= 0.01 * total_budget_50_percent, ]
  shuffle_sites_75_percent <- shuffle_sites[cumsum(COST_Travel_plus_Survey) <= 0.01 * total_budget_75_percent, ]
  systematic_sites_without_2003_25_percent[[i]] <- shuffle_sites_25_percent
  systematic_sites_without_2003_50_percent[[i]] <- shuffle_sites_50_percent
  systematic_sites_without_2003_75_percent[[i]] <- shuffle_sites_75_percent
}
systematic_sites_without_2003_25_percent #check if list is filled with correct dataframes 
CIdata_without_2003_systematic_survey_25_percent_budget <- as.data.frame(do.call("rbind", systematic_sites_without_2003_25_percent)) # convert list of dataframes into one dataframe
write.table(CIdata_without_2003_systematic_survey_25_percent_budget, file="CIdata_without_2003_systematic_survey_25_percent_budget.txt", sep="\t",col.names=T,row.names=F)
systematic_sites_without_2003_50_percent
CIdata_without_2003_systematic_survey_50_percent_budget <- as.data.frame(do.call("rbind", systematic_sites_without_2003_50_percent))
write.table(CIdata_without_2003_systematic_survey_50_percent_budget, file="CIdata_without_2003_systematic_survey_50_percent_budget.txt", sep="\t",col.names=T,row.names=F)
systematic_sites_without_2003_75_percent
CIdata_without_2003_systematic_survey_75_percent_budget <- as.data.frame(do.call("rbind", systematic_sites_without_2003_75_percent))
write.table(CIdata_without_2003_systematic_survey_75_percent_budget, file="CIdata_without_2003_systematic_survey_75_percent_budget.txt", sep="\t",col.names=T,row.names=F)
CIdata_without_2003_systematic_survey_100_percent_budget <- datatable_CIdata_without_2003
write.table(CIdata_without_2003_systematic_survey_100_percent_budget, file="CIdata_without_2003_systematic_survey_100_percent_budget.txt", sep="\t",col.names=T,row.names=F)


####################################### Calculate cost without 2005 ###################################################
# Define profit and cost per survey grid cell
profit_raster <- read.asc("prob_occ_SC_predicted_to_2005.asc") # this prediction is built without 2001 data 
raster_prob_occ <- raster(profit_raster) # change asci file into a raster file 
class(raster_prob_occ)
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata_without_2005)) {
  cat(round(10*i/nrow(CIdata_without_2005),digits=2),"%\n",sep="")
  x = CIdata_without_2005[i,"avg_X_MARK"]
  y = CIdata_without_2005[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(raster_prob_occ,matrix(data = c(x,y),ncol=2))))
}
CIdata_without_2005$Profit <- profit # add profit values to dataframe 
# read in cost file 
cost <- CIdata_without_2005$COST_Travel_plus_Survey 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata_without_2005$Knapsack_profit_per_cost <- knapsack

## Organize sites to survey for 25% - 50% - 75% and 100% of budget
CIdata_without_2005 <- CIdata_without_2005[order(CIdata_without_2005$WPT), ] # Order CIdata by WPT 
CIdata_without_2005 <- CIdata_without_2005[!duplicated(CIdata_without_2005$WPT), ] # Keep one row per waypoint
head(CIdata_without_2005)
write.table(CIdata_without_2005, file="CIdata_without_2005_with_Profit_Knapsack_per_WPT.txt", sep="\t",col.names=T,row.names=F)

## Total cost of surveying all 1018 sites
total_cost <- sum(CIdata_without_2005$COST_Travel_plus_Survey, na.rm=TRUE) # total cost to survey all sites once (= total budget)
total_cost
total_budget_25_percent <- 0.25*total_cost # 25% of budget
total_budget_50_percent <- 0.5*total_cost # 50 % of budget
total_budget_75_percent <- 0.75*total_cost # 75 % of budget
total_budget_100_percent <- total_cost # full budget

## Order sites based on probability of occurrence -> to do the survey based on decreasing Pr(occ), while including the cost
library(data.table) # use package data.table which wraps a data.table around a data.frame
datatable_CIdata_without_2005 <- as.data.table(CIdata_without_2005)
setorder(datatable_CIdata_without_2005, -Profit) # order CIdata.table by the Profit column in descending order (highest Pr(occ) first)
datatable_CIdata_without_2005 # check if ordered by Profit
datatable_CIdata_without_2005 <- datatable_CIdata_without_2005[-1, ] # remove the first row because profit (= probability of occurrence is NA)
datatable_CIdata_without_2005 # check if NA in Profit and Knapsack row has disappeared
# Recalculate total cost to survey 1017 sites (because NAs are removed)
total_cost <- sum(datatable_CIdata_without_2005$COST_Travel_plus_Survey)
total_cost
total_budget_25_percent <- 0.25*total_cost # 25% of budget
total_budget_50_percent <- 0.5*total_cost # 50 % of budget
total_budget_75_percent <- 0.75*total_cost # 75 % of budget
total_budget_100_percent <- total_cost # full budget
# Group sites for different budgets based on decreasing probability of occurrence 
CIdata_without_2005_Pr_Occ_25_percent_of_budget <- datatable_CIdata_without_2005[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ] # take the first x columns until 25% of budget is reached (sites ordered by Pr(occ))
CIdata_without_2005_Pr_Occ_50_percent_of_budget <- datatable_CIdata_without_2005[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ] 
CIdata_without_2005_Pr_Occ_75_percent_of_budget <- datatable_CIdata_without_2005[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
CIdata_without_2005_Pr_Occ_100_percent_of_budget <- datatable_CIdata_without_2005
write.table(CIdata_without_2005_Pr_Occ_25_percent_of_budget, file="CIdata_without_2005_Pr_Occ_25_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2005_Pr_Occ_50_percent_of_budget, file="CIdata_without_2005_Pr_Occ_50_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2005_Pr_Occ_75_percent_of_budget, file="CIdata_without_2005_Pr_Occ_75_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2005_Pr_Occ_100_percent_of_budget, file="CIdata_without_2005_Pr_Occ_100_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)

## Order sites based on Knapsack calculation (benefit/cost)
setorder(datatable_CIdata_without_2005, -Knapsack_profit_per_cost) # order CIdata.table by column Knapsack_profit_per_cost (highest profit/cost first)
datatable_CIdata_without_2005 # check if ordered by Knapsack
CIdata_without_2005_Knapsack_25_percent_of_budget <- datatable_CIdata_without_2005[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ] # subset CIdata.table with total_cost_25_percent
CIdata_without_2005_Knapsack_50_percent_of_budget <- datatable_CIdata_without_2005[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ]
CIdata_without_2005_Knapsack_75_percent_of_budget <- datatable_CIdata_without_2005[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
CIdata_without_2005_Knapsack_100_percent_of_budget <- datatable_CIdata_without_2005
write.table(CIdata_without_2005_Knapsack_25_percent_of_budget, file="CIdata_without_2005_Knapsack_25_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2005_Knapsack_50_percent_of_budget, file="CIdata_without_2005_Knapsack_50_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2005_Knapsack_75_percent_of_budget, file="CIdata_without_2005_Knapsack_75_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2005_Knapsack_100_percent_of_budget, file="CIdata_without_2005_Knapsack_100_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)

## Order sites based on expert opinion (managers' best guess):
# Dion: only survey sites around the edges (low elevation) and survey a small number of sites on the plateau
# Pete & Pinky: make the gridcells larger 
setorder(datatable_CIdata_without_2005, Elevation) # order CIdata.table by column Elevation in increasing order (from low elevation to high)
# 25% budget
CIdata_without_2005_Elevation_25_random_pick_above_250m <- datatable_CIdata_without_2005[sample(which(Elevation>250), size=25), ] # randomly pick 25 sites on the plateau (above 250m)
cost_survey_25_plateau_sites <- sum(CIdata_without_2005_Elevation_25_random_pick_above_250m$COST_Travel_plus_Survey)
cost_survey_25_plateau_sites
remaining_budget_25_percent <- total_budget_25_percent - cost_survey_25_plateau_sites # this is the remaining budget that can be used to survey sites at lower elevation
remaining_budget_25_percent
total_budget_25_percent # check
CIdata_without_2005_Elevation_25_percent_of_budget <- datatable_CIdata_without_2005[cumsum(COST_Travel_plus_Survey)<=remaining_budget_25_percent, ] # extract those rows with sites at lowest elevation for which cost to survey lies within the remaining budget
CIdata_without_2005_Elevation_25_percent_of_budget
write.table(CIdata_without_2005_Elevation_25_percent_of_budget, file="CIdata_without_2005_managers_guess_25_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
WPT_25_plateau <- CIdata_without_2005_Elevation_25_random_pick_above_250m$WPT # those sites will be surveyed on the plateau
WPT_25_plateau
WPT_25_low_elevation <- CIdata_without_2005_Elevation_25_percent_of_budget$WPT # those lower elevation sites will be surveyed
WPT_25_low_elevation
# 50% budget
CIdata_without_2005_Elevation_50_random_pick_above_250m <- datatable_CIdata_without_2005[sample(which(Elevation>250), size=50), ] # randomly pick 50 sites on the plateau (above 250m)
cost_survey_50_plateau_sites <- sum(CIdata_without_2005_Elevation_50_random_pick_above_250m$COST_Travel_plus_Survey)
cost_survey_50_plateau_sites
remaining_budget_50_percent <- total_budget_50_percent - cost_survey_50_plateau_sites # this is the remaining budget that can be used to survey sites at lower elevation
remaining_budget_50_percent
total_budget_50_percent # check
CIdata_without_2005_Elevation_50_percent_of_budget <- datatable_CIdata_without_2005[cumsum(COST_Travel_plus_Survey) <= remaining_budget_50_percent, ] # extract those rows with sites at lowest elevation for which cost to survey lies within the remaining budget
CIdata_without_2005_Elevation_50_percent_of_budget
write.table(CIdata_without_2005_Elevation_50_percent_of_budget, file="CIdata_without_2005_managers_guess_50_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
WPT_50_plateau <- CIdata_without_2005_Elevation_50_random_pick_above_250m$WPT # these sites will be surveyed on the plateau
WPT_50_plateau
WPT_50_low_elevation <- CIdata_without_2005_Elevation_50_percent_of_budget$WPT # these lower elevation sites will be surveyed
WPT_50_low_elevation
# 75% budget
CIdata_without_2005_Elevation_75_random_pick_above_250m <- datatable_CIdata_without_2005[sample(which(Elevation>250), size=75), ] # randomly pick 75 sites on the plateau (above 250m)
cost_survey_75_plateau_sites <- sum(CIdata_without_2005_Elevation_75_random_pick_above_250m$COST_Travel_plus_Survey)
cost_survey_75_plateau_sites
remaining_budget_75_percent <- total_budget_75_percent - cost_survey_75_plateau_sites
remaining_budget_75_percent
total_budget_75_percent # check
CIdata_without_2005_Elevation_75_percent_of_budget <- datatable_CIdata_without_2005[cumsum(COST_Travel_plus_Survey) <= remaining_budget_75_percent, ] # extract those rows with sites at lowest elevation for which cost to survey lies within the remaining budget
CIdata_without_2005_Elevation_75_percent_of_budget
write.table(CIdata_without_2005_Elevation_75_percent_of_budget, file="CIdata_without_2005_managers_guess_75_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
WPT_75_plateau <- CIdata_without_2005_Elevation_75_random_pick_above_250m$WPT # these sites will be surveyed on the plateau
WPT_75_plateau
WPT_75_low_elevation <- CIdata_without_2005_Elevation_75_percent_of_budget$WPT # these lower elevation sites will be surveyed
WPT_75_low_elevation
# 100% budget
CIdata_without_2005_Elevation_100_percent_of_budget <- datatable_CIdata_without_2005
CIdata_without_2005_Elevation_100_percent_of_budget
write.table(CIdata_without_2005_Elevation_100_percent_of_budget, file="CIdata_without_2005_managers_guess_100_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)

## Order sites randomly
random_shuffled_sites <- datatable_CIdata_without_2005[sample(nrow(datatable_CIdata_without_2005)), ]
class(random_shuffled_sites)

random_sites_25_percent <- random_shuffled_sites[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ]
random_sites_25_percent 
total_budget_25_percent
sum(random_sites_25_percent$COST_Travel_plus_Survey, na.rm = TRUE) # check if total cost is 25% of total budget
write.table(random_sites_25_percent, file="CIdata_without_2005_random_sites_25_percent.txt", sep="\t",col.names=T,row.names=F)

random_sites_50_percent <- random_shuffled_sites[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ]
random_sites_50_percent
total_budget_50_percent
sum(random_sites_50_percent$COST_Travel_plus_Survey, na.rm = TRUE) # check if total cost is 50% of total budget
write.table(random_sites_50_percent, file="CIdata_without_2005_random_sites_50_percent.txt", sep="\t",col.names=T,row.names=F)

random_sites_75_percent <- random_shuffled_sites[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
random_sites_75_percent
total_budget_75_percent
sum(random_sites_75_percent$COST_Travel_plus, na.rm = TRUE) # check if total cost is 75% of total budget
write.table(random_sites_75_percent, file="CIdata_without_2005_random_sites_75_percent.txt", sep="\t",col.names=T,row.names=F)

random_sites_100_percent <- random_shuffled_sites
random_sites_100_percent
total_budget_100_percent
sum(random_sites_100_percent$COST_Travel_plus, na.rm = TRUE) # check if total cost is 100% of total budget
write.table(random_sites_100_percent, file="CIdata_without_2005_random_sites_100_percent.txt", sep="\t",col.names=T,row.names=F)

## Order sites systematically -> algorithm that selects sites systematically for 25% - 50% - 75% - 100% of budget 
head(datatable_CIdata_without_2005) # check if CIdata has one row per waypoint
CIdata_without_2005_systematic <- datatable_CIdata_without_2005[-(1001:1017), ] # remove last 18 sites from dataframe in order to make it possible to split 

# split dataframe in 100 small dataframes based on area (dataframe 1: site 1-10, dataframe 2: site 11-20, etc)
splitted_dataframe_without_2005 <- split(CIdata_without_2005_systematic, rep(1:100, each=10))
class(splitted_dataframe_without_2005) # check class; should be a list with 100 elements, containing 10 rows each  
# define list to store shuffled dataframes for further calculation 
systematic_sites_without_2005_25_percent <- list()
systematic_sites_without_2005_50_percent <- list()
systematic_sites_without_2005_75_percent <- list()
# create lists with systematically organised sites for different budgets 
for (i in 1:100){
  kk <- splitted_dataframe_without_2005[[i]] # take the first dataframe out of the list (containing first 10 sites)
  kk <- as.data.table(kk)
  shuffle_sites <- kk[sample(nrow(kk))] # randomize sites
  shuffle_sites_25_percent <- shuffle_sites[cumsum(COST_Travel_plus_Survey) <= 0.01 * total_budget_25_percent, ] # take a number of sites with survey budget = 0.25% or
  # total budget (and do that 100 times to have a total budget of 25% of total island) -> it is done this way
  # to ensure sites are sytematically chosen across the island 
  shuffle_sites_50_percent <- shuffle_sites[cumsum(COST_Travel_plus_Survey) <= 0.01 * total_budget_50_percent, ]
  shuffle_sites_75_percent <- shuffle_sites[cumsum(COST_Travel_plus_Survey) <= 0.01 * total_budget_75_percent, ]
  systematic_sites_without_2005_25_percent[[i]] <- shuffle_sites_25_percent
  systematic_sites_without_2005_50_percent[[i]] <- shuffle_sites_50_percent
  systematic_sites_without_2005_75_percent[[i]] <- shuffle_sites_75_percent
}
systematic_sites_without_2005_25_percent #check if list is filled with correct dataframes 
CIdata_without_2005_systematic_survey_25_percent_budget <- as.data.frame(do.call("rbind", systematic_sites_without_2005_25_percent)) # convert list of dataframes into one dataframe
write.table(CIdata_without_2005_systematic_survey_25_percent_budget, file="CIdata_without_2005_systematic_survey_25_percent_budget.txt", sep="\t",col.names=T,row.names=F)
systematic_sites_without_2005_50_percent
CIdata_without_2005_systematic_survey_50_percent_budget <- as.data.frame(do.call("rbind", systematic_sites_without_2005_50_percent))
write.table(CIdata_without_2005_systematic_survey_50_percent_budget, file="CIdata_without_2005_systematic_survey_50_percent_budget.txt", sep="\t",col.names=T,row.names=F)
systematic_sites_without_2005_75_percent
CIdata_without_2005_systematic_survey_75_percent_budget <- as.data.frame(do.call("rbind", systematic_sites_without_2005_75_percent))
write.table(CIdata_without_2005_systematic_survey_75_percent_budget, file="CIdata_without_2005_systematic_survey_75_percent_budget.txt", sep="\t",col.names=T,row.names=F)
CIdata_without_2005_systematic_survey_100_percent_budget <- datatable_CIdata_without_2005
write.table(CIdata_without_2005_systematic_survey_100_percent_budget, file="CIdata_without_2005_systematic_survey_100_percent_budget.txt", sep="\t",col.names=T,row.names=F)


####################################### Calculate cost without 2007 ###################################################
# Define profit and cost per survey grid cell
profit_raster <- read.asc("prob_occ_SC_predicted_to_2007.asc") # this prediction is built without 2001 data 
raster_prob_occ <- raster(profit_raster) # change asci file into a raster file 
class(raster_prob_occ)
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata_without_2007)) {
  cat(round(10*i/nrow(CIdata_without_2007),digits=2),"%\n",sep="")
  x = CIdata_without_2007[i,"avg_X_MARK"]
  y = CIdata_without_2007[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(raster_prob_occ,matrix(data = c(x,y),ncol=2))))
}
CIdata_without_2007$Profit <- profit # add profit values to dataframe 
# read in cost file 
cost <- CIdata_without_2007$COST_Travel_plus_Survey 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata_without_2007$Knapsack_profit_per_cost <- knapsack

## Organize sites to survey for 25% - 50% - 75% and 100% of budget
CIdata_without_2007 <- CIdata_without_2007[order(CIdata_without_2007$WPT), ] # Order CIdata by WPT 
CIdata_without_2007 <- CIdata_without_2007[!duplicated(CIdata_without_2007$WPT), ] # Keep one row per waypoint
head(CIdata_without_2007)
write.table(CIdata_without_2007, file="CIdata_without_2007_with_Profit_Knapsack_per_WPT.txt", sep="\t",col.names=T,row.names=F)

## Total cost of surveying all 1018 sites
total_cost <- sum(CIdata_without_2007$COST_Travel_plus_Survey, na.rm=TRUE) # total cost to survey all sites once (= total budget)
total_cost
total_budget_25_percent <- 0.25*total_cost # 25% of budget
total_budget_50_percent <- 0.5*total_cost # 50 % of budget
total_budget_75_percent <- 0.75*total_cost # 75 % of budget
total_budget_100_percent <- total_cost # full budget

## Order sites based on probability of occurrence -> to do the survey based on decreasing Pr(occ), while including the cost
library(data.table) # use package data.table which wraps a data.table around a data.frame
datatable_CIdata_without_2007 <- as.data.table(CIdata_without_2007)
setorder(datatable_CIdata_without_2007, -Profit) # order CIdata.table by the Profit column in descending order (highest Pr(occ) first)
datatable_CIdata_without_2007 # check if ordered by Profit
datatable_CIdata_without_2007 <- datatable_CIdata_without_2007[-1, ] # remove the first row because profit (= probability of occurrence is NA)
datatable_CIdata_without_2007 # check if NA in Profit and Knapsack row has disappeared
# Recalculate total cost to survey 1017 sites (because NAs are removed)
total_cost <- sum(datatable_CIdata_without_2007$COST_Travel_plus_Survey)
total_cost
total_budget_25_percent <- 0.25*total_cost # 25% of budget
total_budget_50_percent <- 0.5*total_cost # 50 % of budget
total_budget_75_percent <- 0.75*total_cost # 75 % of budget
total_budget_100_percent <- total_cost # full budget
# Group sites for different budgets based on decreasing probability of occurrence 
CIdata_without_2007_Pr_Occ_25_percent_of_budget <- datatable_CIdata_without_2007[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ] # take the first x columns until 25% of budget is reached (sites ordered by Pr(occ))
CIdata_without_2007_Pr_Occ_50_percent_of_budget <- datatable_CIdata_without_2007[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ] 
CIdata_without_2007_Pr_Occ_75_percent_of_budget <- datatable_CIdata_without_2007[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
CIdata_without_2007_Pr_Occ_100_percent_of_budget <- datatable_CIdata_without_2007
write.table(CIdata_without_2007_Pr_Occ_25_percent_of_budget, file="CIdata_without_2007_Pr_Occ_25_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2007_Pr_Occ_50_percent_of_budget, file="CIdata_without_2007_Pr_Occ_50_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2007_Pr_Occ_75_percent_of_budget, file="CIdata_without_2007_Pr_Occ_75_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2007_Pr_Occ_100_percent_of_budget, file="CIdata_without_2007_Pr_Occ_100_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)

## Order sites based on Knapsack calculation (benefit/cost)
setorder(datatable_CIdata_without_2007, -Knapsack_profit_per_cost) # order CIdata.table by column Knapsack_profit_per_cost (highest profit/cost first)
datatable_CIdata_without_2007 # check if ordered by Knapsack
CIdata_without_2007_Knapsack_25_percent_of_budget <- datatable_CIdata_without_2007[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ] # subset CIdata.table with total_cost_25_percent
CIdata_without_2007_Knapsack_50_percent_of_budget <- datatable_CIdata_without_2007[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ]
CIdata_without_2007_Knapsack_75_percent_of_budget <- datatable_CIdata_without_2007[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
CIdata_without_2007_Knapsack_100_percent_of_budget <- datatable_CIdata_without_2007
write.table(CIdata_without_2007_Knapsack_25_percent_of_budget, file="CIdata_without_2007_Knapsack_25_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2007_Knapsack_50_percent_of_budget, file="CIdata_without_2007_Knapsack_50_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2007_Knapsack_75_percent_of_budget, file="CIdata_without_2007_Knapsack_75_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2007_Knapsack_100_percent_of_budget, file="CIdata_without_2007_Knapsack_100_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)

## Order sites based on expert opinion (managers' best guess):
# Dion: only survey sites around the edges (low elevation) and survey a small number of sites on the plateau
# Pete & Pinky: make the gridcells larger 
setorder(datatable_CIdata_without_2007, Elevation) # order CIdata.table by column Elevation in increasing order (from low elevation to high)
# 25% budget
CIdata_without_2007_Elevation_25_random_pick_above_250m <- datatable_CIdata_without_2007[sample(which(Elevation>250), size=25), ] # randomly pick 25 sites on the plateau (above 250m)
cost_survey_25_plateau_sites <- sum(CIdata_without_2007_Elevation_25_random_pick_above_250m$COST_Travel_plus_Survey)
cost_survey_25_plateau_sites
remaining_budget_25_percent <- total_budget_25_percent - cost_survey_25_plateau_sites # this is the remaining budget that can be used to survey sites at lower elevation
remaining_budget_25_percent
total_budget_25_percent # check
CIdata_without_2007_Elevation_25_percent_of_budget <- datatable_CIdata_without_2007[cumsum(COST_Travel_plus_Survey)<=remaining_budget_25_percent, ] # extract those rows with sites at lowest elevation for which cost to survey lies within the remaining budget
CIdata_without_2007_Elevation_25_percent_of_budget
write.table(CIdata_without_2007_Elevation_25_percent_of_budget, file="CIdata_without_2007_managers_guess_25_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
WPT_25_plateau <- CIdata_without_2007_Elevation_25_random_pick_above_250m$WPT # those sites will be surveyed on the plateau
WPT_25_plateau
WPT_25_low_elevation <- CIdata_without_2007_Elevation_25_percent_of_budget$WPT # those lower elevation sites will be surveyed
WPT_25_low_elevation
# 50% budget
CIdata_without_2007_Elevation_50_random_pick_above_250m <- datatable_CIdata_without_2007[sample(which(Elevation>250), size=50), ] # randomly pick 50 sites on the plateau (above 250m)
cost_survey_50_plateau_sites <- sum(CIdata_without_2007_Elevation_50_random_pick_above_250m$COST_Travel_plus_Survey)
cost_survey_50_plateau_sites
remaining_budget_50_percent <- total_budget_50_percent - cost_survey_50_plateau_sites # this is the remaining budget that can be used to survey sites at lower elevation
remaining_budget_50_percent
total_budget_50_percent # check
CIdata_without_2007_Elevation_50_percent_of_budget <- datatable_CIdata_without_2007[cumsum(COST_Travel_plus_Survey) <= remaining_budget_50_percent, ] # extract those rows with sites at lowest elevation for which cost to survey lies within the remaining budget
CIdata_without_2007_Elevation_50_percent_of_budget
write.table(CIdata_without_2007_Elevation_50_percent_of_budget, file="CIdata_without_2007_managers_guess_50_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
WPT_50_plateau <- CIdata_without_2007_Elevation_50_random_pick_above_250m$WPT # these sites will be surveyed on the plateau
WPT_50_plateau
WPT_50_low_elevation <- CIdata_without_2007_Elevation_50_percent_of_budget$WPT # these lower elevation sites will be surveyed
WPT_50_low_elevation
# 75% budget
CIdata_without_2007_Elevation_75_random_pick_above_250m <- datatable_CIdata_without_2007[sample(which(Elevation>250), size=75), ] # randomly pick 75 sites on the plateau (above 250m)
cost_survey_75_plateau_sites <- sum(CIdata_without_2007_Elevation_75_random_pick_above_250m$COST_Travel_plus_Survey)
cost_survey_75_plateau_sites
remaining_budget_75_percent <- total_budget_75_percent - cost_survey_75_plateau_sites
remaining_budget_75_percent
total_budget_75_percent # check
CIdata_without_2007_Elevation_75_percent_of_budget <- datatable_CIdata_without_2007[cumsum(COST_Travel_plus_Survey) <= remaining_budget_75_percent, ] # extract those rows with sites at lowest elevation for which cost to survey lies within the remaining budget
CIdata_without_2007_Elevation_75_percent_of_budget
write.table(CIdata_without_2007_Elevation_75_percent_of_budget, file="CIdata_without_2007_managers_guess_75_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
WPT_75_plateau <- CIdata_without_2007_Elevation_75_random_pick_above_250m$WPT # these sites will be surveyed on the plateau
WPT_75_plateau
WPT_75_low_elevation <- CIdata_without_2007_Elevation_75_percent_of_budget$WPT # these lower elevation sites will be surveyed
WPT_75_low_elevation
# 100% budget
CIdata_without_2007_Elevation_100_percent_of_budget <- datatable_CIdata_without_2007
CIdata_without_2007_Elevation_100_percent_of_budget
write.table(CIdata_without_2007_Elevation_100_percent_of_budget, file="CIdata_without_2007_managers_guess_100_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)

## Order sites randomly
random_shuffled_sites <- datatable_CIdata_without_2007[sample(nrow(datatable_CIdata_without_2007)), ]
class(random_shuffled_sites)

random_sites_25_percent <- random_shuffled_sites[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ]
random_sites_25_percent 
total_budget_25_percent
sum(random_sites_25_percent$COST_Travel_plus_Survey, na.rm = TRUE) # check if total cost is 25% of total budget
write.table(random_sites_25_percent, file="CIdata_without_2007_random_sites_25_percent.txt", sep="\t",col.names=T,row.names=F)

random_sites_50_percent <- random_shuffled_sites[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ]
random_sites_50_percent
total_budget_50_percent
sum(random_sites_50_percent$COST_Travel_plus_Survey, na.rm = TRUE) # check if total cost is 50% of total budget
write.table(random_sites_50_percent, file="CIdata_without_2007_random_sites_50_percent.txt", sep="\t",col.names=T,row.names=F)

random_sites_75_percent <- random_shuffled_sites[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
random_sites_75_percent
total_budget_75_percent
sum(random_sites_75_percent$COST_Travel_plus, na.rm = TRUE) # check if total cost is 75% of total budget
write.table(random_sites_75_percent, file="CIdata_without_2007_random_sites_75_percent.txt", sep="\t",col.names=T,row.names=F)

random_sites_100_percent <- random_shuffled_sites
random_sites_100_percent
total_budget_100_percent
sum(random_sites_100_percent$COST_Travel_plus, na.rm = TRUE) # check if total cost is 100% of total budget
write.table(random_sites_100_percent, file="CIdata_without_2007_random_sites_100_percent.txt", sep="\t",col.names=T,row.names=F)


## Order sites systematically -> algorithm that selects sites systematically for 25% - 50% - 75% - 100% of budget 
head(datatable_CIdata_without_2007) # check if CIdata has one row per waypoint
CIdata_without_2007_systematic <- datatable_CIdata_without_2007[-(1001:1017), ] # remove last 18 sites from dataframe in order to make it possible to split 

# split dataframe in 100 small dataframes based on area (dataframe 1: site 1-10, dataframe 2: site 11-20, etc)
splitted_dataframe_without_2007 <- split(CIdata_without_2007_systematic, rep(1:100, each=10))
class(splitted_dataframe_without_2007) # check class; should be a list with 100 elements, containing 10 rows each  
# define list to store shuffled dataframes for further calculation 
systematic_sites_without_2007_25_percent <- list()
systematic_sites_without_2007_50_percent <- list()
systematic_sites_without_2007_75_percent <- list()
# create lists with systematically organised sites for different budgets 
for (i in 1:100){
  kk <- splitted_dataframe_without_2007[[i]] # take the first dataframe out of the list (containing first 10 sites)
  kk <- as.data.table(kk)
  shuffle_sites <- kk[sample(nrow(kk))] # randomize sites
  shuffle_sites_25_percent <- shuffle_sites[cumsum(COST_Travel_plus_Survey) <= 0.01 * total_budget_25_percent, ] # take a number of sites with survey budget = 0.25% or
  # total budget (and do that 100 times to have a total budget of 25% of total island) -> it is done this way
  # to ensure sites are sytematically chosen across the island 
  shuffle_sites_50_percent <- shuffle_sites[cumsum(COST_Travel_plus_Survey) <= 0.01 * total_budget_50_percent, ]
  shuffle_sites_75_percent <- shuffle_sites[cumsum(COST_Travel_plus_Survey) <= 0.01 * total_budget_75_percent, ]
  systematic_sites_without_2005_25_percent[[i]] <- shuffle_sites_25_percent
  systematic_sites_without_2005_50_percent[[i]] <- shuffle_sites_50_percent
  systematic_sites_without_2005_75_percent[[i]] <- shuffle_sites_75_percent
}
systematic_sites_without_2007_25_percent #check if list is filled with correct dataframes 
CIdata_without_2007_systematic_survey_25_percent_budget <- as.data.frame(do.call("rbind", systematic_sites_without_2007_25_percent)) # convert list of dataframes into one dataframe
write.table(CIdata_without_2007_systematic_survey_25_percent_budget, file="CIdata_without_2007_systematic_survey_25_percent_budget.txt", sep="\t",col.names=T,row.names=F)
systematic_sites_without_2007_50_percent
CIdata_without_2007_systematic_survey_50_percent_budget <- as.data.frame(do.call("rbind", systematic_sites_without_2007_50_percent))
write.table(CIdata_without_2007_systematic_survey_50_percent_budget, file="CIdata_without_2007_systematic_survey_50_percent_budget.txt", sep="\t",col.names=T,row.names=F)
systematic_sites_without_2007_75_percent
CIdata_without_2007_systematic_survey_75_percent_budget <- as.data.frame(do.call("rbind", systematic_sites_without_2007_75_percent))
write.table(CIdata_without_2007_systematic_survey_75_percent_budget, file="CIdata_without_2007_systematic_survey_75_percent_budget.txt", sep="\t",col.names=T,row.names=F)
CIdata_without_2007_systematic_survey_100_percent_budget <- datatable_CIdata_without_2007
write.table(CIdata_without_2007_systematic_survey_100_percent_budget, file="CIdata_without_2007_systematic_survey_100_percent_budget.txt", sep="\t",col.names=T,row.names=F)


####################################### Calculate cost without 2009 ###################################################
# Define profit and cost per survey grid cell
profit_raster <- read.asc("prob_occ_SC_predicted_to_2009.asc") # this prediction is built without 2001 data 
raster_prob_occ <- raster(profit_raster) # change asci file into a raster file 
class(raster_prob_occ)
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata_without_2009)) {
  cat(round(10*i/nrow(CIdata_without_2009),digits=2),"%\n",sep="")
  x = CIdata_without_2009[i,"avg_X_MARK"]
  y = CIdata_without_2009[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(raster_prob_occ,matrix(data = c(x,y),ncol=2))))
}
CIdata_without_2009$Profit <- profit # add profit values to dataframe 
# read in cost file 
cost <- CIdata_without_2009$COST_Travel_plus_Survey 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata_without_2009$Knapsack_profit_per_cost <- knapsack

## Organize sites to survey for 25% - 50% - 75% and 100% of budget
CIdata_without_2009 <- CIdata_without_2009[order(CIdata_without_2009$WPT), ] # Order CIdata by WPT 
CIdata_without_2009 <- CIdata_without_2009[!duplicated(CIdata_without_2009$WPT), ] # Keep one row per waypoint
head(CIdata_without_2009)
write.table(CIdata_without_2009, file="CIdata_without_2009_with_Profit_Knapsack_per_WPT.txt", sep="\t",col.names=T,row.names=F)

## Total cost of surveying all 1018 sites
total_cost <- sum(CIdata_without_2009$COST_Travel_plus_Survey, na.rm=TRUE) # total cost to survey all sites once (= total budget)
total_cost
total_budget_25_percent <- 0.25*total_cost # 25% of budget
total_budget_50_percent <- 0.5*total_cost # 50 % of budget
total_budget_75_percent <- 0.75*total_cost # 75 % of budget
total_budget_100_percent <- total_cost # full budget

## Order sites based on probability of occurrence -> to do the survey based on decreasing Pr(occ), while including the cost
library(data.table) # use package data.table which wraps a data.table around a data.frame
datatable_CIdata_without_2009 <- as.data.table(CIdata_without_2009)
setorder(datatable_CIdata_without_2009, -Profit) # order CIdata.table by the Profit column in descending order (highest Pr(occ) first)
datatable_CIdata_without_2009 # check if ordered by Profit
datatable_CIdata_without_2009 <- datatable_CIdata_without_2009[-1, ] # remove the first row because profit (= probability of occurrence is NA)
datatable_CIdata_without_2009 # check if NA in Profit and Knapsack row has disappeared
# Recalculate total cost to survey 1017 sites (because NAs are removed)
total_cost <- sum(datatable_CIdata_without_2009$COST_Travel_plus_Survey)
total_cost
total_budget_25_percent <- 0.25*total_cost # 25% of budget
total_budget_50_percent <- 0.5*total_cost # 50 % of budget
total_budget_75_percent <- 0.75*total_cost # 75 % of budget
total_budget_100_percent <- total_cost # full budget
# Group sites for different budgets based on decreasing probability of occurrence 
CIdata_without_2009_Pr_Occ_25_percent_of_budget <- datatable_CIdata_without_2009[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ] # take the first x columns until 25% of budget is reached (sites ordered by Pr(occ))
CIdata_without_2009_Pr_Occ_50_percent_of_budget <- datatable_CIdata_without_2009[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ] 
CIdata_without_2009_Pr_Occ_75_percent_of_budget <- datatable_CIdata_without_2009[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
CIdata_without_2009_Pr_Occ_100_percent_of_budget <- datatable_CIdata_without_2009
write.table(CIdata_without_2009_Pr_Occ_25_percent_of_budget, file="CIdata_without_2009_Pr_Occ_25_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2009_Pr_Occ_50_percent_of_budget, file="CIdata_without_2009_Pr_Occ_50_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2009_Pr_Occ_75_percent_of_budget, file="CIdata_without_2009_Pr_Occ_75_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2009_Pr_Occ_100_percent_of_budget, file="CIdata_without_2009_Pr_Occ_100_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)

## Order sites based on Knapsack calculation (benefit/cost)
setorder(datatable_CIdata_without_2009, -Knapsack_profit_per_cost) # order CIdata.table by column Knapsack_profit_per_cost (highest profit/cost first)
datatable_CIdata_without_2009 # check if ordered by Knapsack
CIdata_without_2009_Knapsack_25_percent_of_budget <- datatable_CIdata_without_2009[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ] # subset CIdata.table with total_cost_25_percent
CIdata_without_2009_Knapsack_50_percent_of_budget <- datatable_CIdata_without_2009[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ]
CIdata_without_2009_Knapsack_75_percent_of_budget <- datatable_CIdata_without_2009[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
CIdata_without_2009_Knapsack_100_percent_of_budget <- datatable_CIdata_without_2009
write.table(CIdata_without_2009_Knapsack_25_percent_of_budget, file="CIdata_without_2009_Knapsack_25_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2009_Knapsack_50_percent_of_budget, file="CIdata_without_2009_Knapsack_50_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2009_Knapsack_75_percent_of_budget, file="CIdata_without_2009_Knapsack_75_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2009_Knapsack_100_percent_of_budget, file="CIdata_without_2009_Knapsack_100_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)

## Order sites based on expert opinion (managers' best guess):
# Dion: only survey sites around the edges (low elevation) and survey a small number of sites on the plateau
# Pete & Pinky: make the gridcells larger 
setorder(datatable_CIdata_without_2009, Elevation) # order CIdata.table by column Elevation in increasing order (from low elevation to high)
# 25% budget
CIdata_without_2009_Elevation_25_random_pick_above_250m <- datatable_CIdata_without_2009[sample(which(Elevation>250), size=25), ] # randomly pick 25 sites on the plateau (above 250m)
cost_survey_25_plateau_sites <- sum(CIdata_without_2009_Elevation_25_random_pick_above_250m$COST_Travel_plus_Survey)
cost_survey_25_plateau_sites
remaining_budget_25_percent <- total_budget_25_percent - cost_survey_25_plateau_sites # this is the remaining budget that can be used to survey sites at lower elevation
remaining_budget_25_percent
total_budget_25_percent # check
CIdata_without_2009_Elevation_25_percent_of_budget <- datatable_CIdata_without_2009[cumsum(COST_Travel_plus_Survey)<=remaining_budget_25_percent, ] # extract those rows with sites at lowest elevation for which cost to survey lies within the remaining budget
CIdata_without_2009_Elevation_25_percent_of_budget
write.table(CIdata_without_2009_Elevation_25_percent_of_budget, file="CIdata_without_2009_managers_guess_25_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
WPT_25_plateau <- CIdata_without_2009_Elevation_25_random_pick_above_250m$WPT # those sites will be surveyed on the plateau
WPT_25_plateau
WPT_25_low_elevation <- CIdata_without_2009_Elevation_25_percent_of_budget$WPT # those lower elevation sites will be surveyed
WPT_25_low_elevation
# 50% budget
CIdata_without_2009_Elevation_50_random_pick_above_250m <- datatable_CIdata_without_2009[sample(which(Elevation>250), size=50), ] # randomly pick 50 sites on the plateau (above 250m)
cost_survey_50_plateau_sites <- sum(CIdata_without_2009_Elevation_50_random_pick_above_250m$COST_Travel_plus_Survey)
cost_survey_50_plateau_sites
remaining_budget_50_percent <- total_budget_50_percent - cost_survey_50_plateau_sites # this is the remaining budget that can be used to survey sites at lower elevation
remaining_budget_50_percent
total_budget_50_percent # check
CIdata_without_2009_Elevation_50_percent_of_budget <- datatable_CIdata_without_2009[cumsum(COST_Travel_plus_Survey) <= remaining_budget_50_percent, ] # extract those rows with sites at lowest elevation for which cost to survey lies within the remaining budget
CIdata_without_2009_Elevation_50_percent_of_budget
write.table(CIdata_without_2009_Elevation_50_percent_of_budget, file="CIdata_without_2009_managers_guess_50_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
WPT_50_plateau <- CIdata_without_2009_Elevation_50_random_pick_above_250m$WPT # these sites will be surveyed on the plateau
WPT_50_plateau
WPT_50_low_elevation <- CIdata_without_2009_Elevation_50_percent_of_budget$WPT # these lower elevation sites will be surveyed
WPT_50_low_elevation
# 75% budget
CIdata_without_2009_Elevation_75_random_pick_above_250m <- datatable_CIdata_without_2009[sample(which(Elevation>250), size=75), ] # randomly pick 75 sites on the plateau (above 250m)
cost_survey_75_plateau_sites <- sum(CIdata_without_2009_Elevation_75_random_pick_above_250m$COST_Travel_plus_Survey)
cost_survey_75_plateau_sites
remaining_budget_75_percent <- total_budget_75_percent - cost_survey_75_plateau_sites
remaining_budget_75_percent
total_budget_75_percent # check
CIdata_without_2009_Elevation_75_percent_of_budget <- datatable_CIdata_without_2009[cumsum(COST_Travel_plus_Survey) <= remaining_budget_75_percent, ] # extract those rows with sites at lowest elevation for which cost to survey lies within the remaining budget
CIdata_without_2009_Elevation_75_percent_of_budget
write.table(CIdata_without_2009_Elevation_75_percent_of_budget, file="CIdata_without_2009_managers_guess_75_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
WPT_75_plateau <- CIdata_without_2009_Elevation_75_random_pick_above_250m$WPT # these sites will be surveyed on the plateau
WPT_75_plateau
WPT_75_low_elevation <- CIdata_without_2009_Elevation_75_percent_of_budget$WPT # these lower elevation sites will be surveyed
WPT_75_low_elevation
# 100% budget
CIdata_without_2009_Elevation_100_percent_of_budget <- datatable_CIdata_without_2009
CIdata_without_2009_Elevation_100_percent_of_budget
write.table(CIdata_without_2009_Elevation_100_percent_of_budget, file="CIdata_without_2009_managers_guess_100_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)

## Order sites randomly
random_shuffled_sites <- datatable_CIdata_without_2009[sample(nrow(datatable_CIdata_without_2009)), ]
class(random_shuffled_sites)

random_sites_25_percent <- random_shuffled_sites[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ]
random_sites_25_percent 
total_budget_25_percent
sum(random_sites_25_percent$COST_Travel_plus_Survey, na.rm = TRUE) # check if total cost is 25% of total budget
write.table(random_sites_25_percent, file="CIdata_without_2009_random_sites_25_percent.txt", sep="\t",col.names=T,row.names=F)

random_sites_50_percent <- random_shuffled_sites[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ]
random_sites_50_percent
total_budget_50_percent
sum(random_sites_50_percent$COST_Travel_plus_Survey, na.rm = TRUE) # check if total cost is 50% of total budget
write.table(random_sites_50_percent, file="CIdata_without_2009_random_sites_50_percent.txt", sep="\t",col.names=T,row.names=F)

random_sites_75_percent <- random_shuffled_sites[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
random_sites_75_percent
total_budget_75_percent
sum(random_sites_75_percent$COST_Travel_plus, na.rm = TRUE) # check if total cost is 75% of total budget
write.table(random_sites_75_percent, file="CIdata_without_2009_random_sites_75_percent.txt", sep="\t",col.names=T,row.names=F)

random_sites_100_percent <- random_shuffled_sites
random_sites_100_percent
total_budget_100_percent
sum(random_sites_100_percent$COST_Travel_plus, na.rm = TRUE) # check if total cost is 100% of total budget
write.table(random_sites_100_percent, file="CIdata_without_2009_random_sites_100_percent.txt", sep="\t",col.names=T,row.names=F)


## Order sites systematically -> algorithm that selects sites systematically for 25% - 50% - 75% - 100% of budget 
head(datatable_CIdata_without_2009) # check if CIdata has one row per waypoint
CIdata_without_2009_systematic <- datatable_CIdata_without_2009[-(1001:1017), ] # remove last 18 sites from dataframe in order to make it possible to split 

# split dataframe in 100 small dataframes based on area (dataframe 1: site 1-10, dataframe 2: site 11-20, etc)
splitted_dataframe_without_2009 <- split(CIdata_without_2009_systematic, rep(1:100, each=10))
class(splitted_dataframe_without_2009) # check class; should be a list with 100 elements, containing 10 rows each  
# define list to store shuffled dataframes for further calculation 
systematic_sites_without_2009_25_percent <- list()
systematic_sites_without_2009_50_percent <- list()
systematic_sites_without_2009_75_percent <- list()
# create lists with systematically organised sites for different budgets 
for (i in 1:100){
  kk <- splitted_dataframe_without_2009[[i]] # take the first dataframe out of the list (containing first 10 sites)
  kk <- as.data.table(kk)
  shuffle_sites <- kk[sample(nrow(kk))] # randomize sites
  shuffle_sites_25_percent <- shuffle_sites[cumsum(COST_Travel_plus_Survey) <= 0.01 * total_budget_25_percent, ] # take a number of sites with survey budget = 0.25% or
  # total budget (and do that 100 times to have a total budget of 25% of total island) -> it is done this way
  # to ensure sites are sytematically chosen across the island 
  shuffle_sites_50_percent <- shuffle_sites[cumsum(COST_Travel_plus_Survey) <= 0.01 * total_budget_50_percent, ]
  shuffle_sites_75_percent <- shuffle_sites[cumsum(COST_Travel_plus_Survey) <= 0.01 * total_budget_75_percent, ]
  systematic_sites_without_2005_25_percent[[i]] <- shuffle_sites_25_percent
  systematic_sites_without_2005_50_percent[[i]] <- shuffle_sites_50_percent
  systematic_sites_without_2005_75_percent[[i]] <- shuffle_sites_75_percent
}
systematic_sites_without_2009_25_percent #check if list is filled with correct dataframes 
CIdata_without_2009_systematic_survey_25_percent_budget <- as.data.frame(do.call("rbind", systematic_sites_without_2009_25_percent)) # convert list of dataframes into one dataframe
write.table(CIdata_without_2009_systematic_survey_25_percent_budget, file="CIdata_without_2009_systematic_survey_25_percent_budget.txt", sep="\t",col.names=T,row.names=F)
systematic_sites_without_2009_50_percent
CIdata_without_2009_systematic_survey_50_percent_budget <- as.data.frame(do.call("rbind", systematic_sites_without_2009_50_percent))
write.table(CIdata_without_2009_systematic_survey_50_percent_budget, file="CIdata_without_2009_systematic_survey_50_percent_budget.txt", sep="\t",col.names=T,row.names=F)
systematic_sites_without_2009_75_percent
CIdata_without_2009_systematic_survey_75_percent_budget <- as.data.frame(do.call("rbind", systematic_sites_without_2009_75_percent))
write.table(CIdata_without_2009_systematic_survey_75_percent_budget, file="CIdata_without_2009_systematic_survey_75_percent_budget.txt", sep="\t",col.names=T,row.names=F)
CIdata_without_2009_systematic_survey_100_percent_budget <- datatable_CIdata_without_2009
write.table(CIdata_without_2009_systematic_survey_100_percent_budget, file="CIdata_without_2009_systematic_survey_100_percent_budget.txt", sep="\t",col.names=T,row.names=F)



####################################### Calculate cost without 2011 ###################################################
# Define profit and cost per survey grid cell
profit_raster <- read.asc("prob_occ_SC_predicted_to_2011.asc") # this prediction is built without 2001 data 
raster_prob_occ <- raster(profit_raster) # change asci file into a raster file 
class(raster_prob_occ)
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata_without_2011)) {
  cat(round(10*i/nrow(CIdata_without_2011),digits=2),"%\n",sep="")
  x = CIdata_without_2011[i,"avg_X_MARK"]
  y = CIdata_without_2011[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(raster_prob_occ,matrix(data = c(x,y),ncol=2))))
}
CIdata_without_2011$Profit <- profit # add profit values to dataframe 
# read in cost file 
cost <- CIdata_without_2011$COST_Travel_plus_Survey 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata_without_2011$Knapsack_profit_per_cost <- knapsack

## Organize sites to survey for 25% - 50% - 75% and 100% of budget
CIdata_without_2011 <- CIdata_without_2011[order(CIdata_without_2011$WPT), ] # Order CIdata by WPT 
CIdata_without_2011 <- CIdata_without_2011[!duplicated(CIdata_without_2011$WPT), ] # Keep one row per waypoint
head(CIdata_without_2011)
write.table(CIdata_without_2011, file="CIdata_without_2011_with_Profit_Knapsack_per_WPT.txt", sep="\t",col.names=T,row.names=F)

## Total cost of surveying all 1018 sites
total_cost <- sum(CIdata_without_2011$COST_Travel_plus_Survey, na.rm=TRUE) # total cost to survey all sites once (= total budget)
total_cost
total_budget_25_percent <- 0.25*total_cost # 25% of budget
total_budget_50_percent <- 0.5*total_cost # 50 % of budget
total_budget_75_percent <- 0.75*total_cost # 75 % of budget
total_budget_100_percent <- total_cost # full budget

## Order sites based on probability of occurrence -> to do the survey based on decreasing Pr(occ), while including the cost
library(data.table) # use package data.table which wraps a data.table around a data.frame
datatable_CIdata_without_2011 <- as.data.table(CIdata_without_2011)
setorder(datatable_CIdata_without_2011, -Profit) # order CIdata.table by the Profit column in descending order (highest Pr(occ) first)
datatable_CIdata_without_2011 # check if ordered by Profit
datatable_CIdata_without_2011 <- datatable_CIdata_without_2011[-1, ] # remove the first row because profit (= probability of occurrence is NA)
datatable_CIdata_without_2011 # check if NA in Profit and Knapsack row has disappeared
# Recalculate total cost to survey 1017 sites (because NAs are removed)
total_cost <- sum(datatable_CIdata_without_2011$COST_Travel_plus_Survey)
total_cost
total_budget_25_percent <- 0.25*total_cost # 25% of budget
total_budget_50_percent <- 0.5*total_cost # 50 % of budget
total_budget_75_percent <- 0.75*total_cost # 75 % of budget
total_budget_100_percent <- total_cost # full budget
# Group sites for different budgets based on decreasing probability of occurrence 
CIdata_without_2011_Pr_Occ_25_percent_of_budget <- datatable_CIdata_without_2011[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ] # take the first x columns until 25% of budget is reached (sites ordered by Pr(occ))
CIdata_without_2011_Pr_Occ_50_percent_of_budget <- datatable_CIdata_without_2011[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ] 
CIdata_without_2011_Pr_Occ_75_percent_of_budget <- datatable_CIdata_without_2011[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
CIdata_without_2011_Pr_Occ_100_percent_of_budget <- datatable_CIdata_without_2011
write.table(CIdata_without_2011_Pr_Occ_25_percent_of_budget, file="CIdata_without_2011_Pr_Occ_25_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2011_Pr_Occ_50_percent_of_budget, file="CIdata_without_2011_Pr_Occ_50_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2011_Pr_Occ_75_percent_of_budget, file="CIdata_without_2011_Pr_Occ_75_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2011_Pr_Occ_100_percent_of_budget, file="CIdata_without_2011_Pr_Occ_100_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)

## Order sites based on Knapsack calculation (benefit/cost)
setorder(datatable_CIdata_without_2011, -Knapsack_profit_per_cost) # order CIdata.table by column Knapsack_profit_per_cost (highest profit/cost first)
datatable_CIdata_without_2011 # check if ordered by Knapsack
CIdata_without_2011_Knapsack_25_percent_of_budget <- datatable_CIdata_without_2011[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ] # subset CIdata.table with total_cost_25_percent
CIdata_without_2011_Knapsack_50_percent_of_budget <- datatable_CIdata_without_2011[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ]
CIdata_without_2011_Knapsack_75_percent_of_budget <- datatable_CIdata_without_2011[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
CIdata_without_2011_Knapsack_100_percent_of_budget <- datatable_CIdata_without_2011
write.table(CIdata_without_2011_Knapsack_25_percent_of_budget, file="CIdata_without_2011_Knapsack_25_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2011_Knapsack_50_percent_of_budget, file="CIdata_without_2011_Knapsack_50_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2011_Knapsack_75_percent_of_budget, file="CIdata_without_2011_Knapsack_75_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2011_Knapsack_100_percent_of_budget, file="CIdata_without_2011_Knapsack_100_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)

## Order sites based on expert opinion (managers' best guess):
# Dion: only survey sites around the edges (low elevation) and survey a small number of sites on the plateau
# Pete & Pinky: make the gridcells larger 
setorder(datatable_CIdata_without_2011, Elevation) # order CIdata.table by column Elevation in increasing order (from low elevation to high)
# 25% budget
CIdata_without_2011_Elevation_25_random_pick_above_250m <- datatable_CIdata_without_2011[sample(which(Elevation>250), size=25), ] # randomly pick 25 sites on the plateau (above 250m)
cost_survey_25_plateau_sites <- sum(CIdata_without_2011_Elevation_25_random_pick_above_250m$COST_Travel_plus_Survey)
cost_survey_25_plateau_sites
remaining_budget_25_percent <- total_budget_25_percent - cost_survey_25_plateau_sites # this is the remaining budget that can be used to survey sites at lower elevation
remaining_budget_25_percent
total_budget_25_percent # check
CIdata_without_2011_Elevation_25_percent_of_budget <- datatable_CIdata_without_2011[cumsum(COST_Travel_plus_Survey)<=remaining_budget_25_percent, ] # extract those rows with sites at lowest elevation for which cost to survey lies within the remaining budget
CIdata_without_2011_Elevation_25_percent_of_budget
write.table(CIdata_without_2011_Elevation_25_percent_of_budget, file="CIdata_without_2011_managers_guess_25_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
WPT_25_plateau <- CIdata_without_2011_Elevation_25_random_pick_above_250m$WPT # those sites will be surveyed on the plateau
WPT_25_plateau
WPT_25_low_elevation <- CIdata_without_2011_Elevation_25_percent_of_budget$WPT # those lower elevation sites will be surveyed
WPT_25_low_elevation
# 50% budget
CIdata_without_2011_Elevation_50_random_pick_above_250m <- datatable_CIdata_without_2011[sample(which(Elevation>250), size=50), ] # randomly pick 50 sites on the plateau (above 250m)
cost_survey_50_plateau_sites <- sum(CIdata_without_2011_Elevation_50_random_pick_above_250m$COST_Travel_plus_Survey)
cost_survey_50_plateau_sites
remaining_budget_50_percent <- total_budget_50_percent - cost_survey_50_plateau_sites # this is the remaining budget that can be used to survey sites at lower elevation
remaining_budget_50_percent
total_budget_50_percent # check
CIdata_without_2011_Elevation_50_percent_of_budget <- datatable_CIdata_without_2011[cumsum(COST_Travel_plus_Survey) <= remaining_budget_50_percent, ] # extract those rows with sites at lowest elevation for which cost to survey lies within the remaining budget
CIdata_without_2011_Elevation_50_percent_of_budget
write.table(CIdata_without_2011_Elevation_50_percent_of_budget, file="CIdata_without_2011_managers_guess_50_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
WPT_50_plateau <- CIdata_without_2011_Elevation_50_random_pick_above_250m$WPT # these sites will be surveyed on the plateau
WPT_50_plateau
WPT_50_low_elevation <- CIdata_without_2011_Elevation_50_percent_of_budget$WPT # these lower elevation sites will be surveyed
WPT_50_low_elevation
# 75% budget
CIdata_without_2011_Elevation_75_random_pick_above_250m <- datatable_CIdata_without_2011[sample(which(Elevation>250), size=75), ] # randomly pick 75 sites on the plateau (above 250m)
cost_survey_75_plateau_sites <- sum(CIdata_without_2011_Elevation_75_random_pick_above_250m$COST_Travel_plus_Survey)
cost_survey_75_plateau_sites
remaining_budget_75_percent <- total_budget_75_percent - cost_survey_75_plateau_sites
remaining_budget_75_percent
total_budget_75_percent # check
CIdata_without_2011_Elevation_75_percent_of_budget <- datatable_CIdata_without_2011[cumsum(COST_Travel_plus_Survey) <= remaining_budget_75_percent, ] # extract those rows with sites at lowest elevation for which cost to survey lies within the remaining budget
CIdata_without_2011_Elevation_75_percent_of_budget
write.table(CIdata_without_2011_Elevation_75_percent_of_budget, file="CIdata_without_2011_managers_guess_75_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
WPT_75_plateau <- CIdata_without_2011_Elevation_75_random_pick_above_250m$WPT # these sites will be surveyed on the plateau
WPT_75_plateau
WPT_75_low_elevation <- CIdata_without_2011_Elevation_75_percent_of_budget$WPT # these lower elevation sites will be surveyed
WPT_75_low_elevation
# 100% budget
CIdata_without_2011_Elevation_100_percent_of_budget <- datatable_CIdata_without_2011
CIdata_without_2011_Elevation_100_percent_of_budget
write.table(CIdata_without_2011_Elevation_100_percent_of_budget, file="CIdata_without_2011_managers_guess_100_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)

## Order sites randomly
random_shuffled_sites <- datatable_CIdata_without_2011[sample(nrow(datatable_CIdata_without_2011)), ]
class(random_shuffled_sites)

random_sites_25_percent <- random_shuffled_sites[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ]
random_sites_25_percent 
total_budget_25_percent
sum(random_sites_25_percent$COST_Travel_plus_Survey, na.rm = TRUE) # check if total cost is 25% of total budget
write.table(random_sites_25_percent, file="CIdata_without_2011_random_sites_25_percent.txt", sep="\t",col.names=T,row.names=F)

random_sites_50_percent <- random_shuffled_sites[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ]
random_sites_50_percent
total_budget_50_percent
sum(random_sites_50_percent$COST_Travel_plus_Survey, na.rm = TRUE) # check if total cost is 50% of total budget
write.table(random_sites_50_percent, file="CIdata_without_2011_random_sites_50_percent.txt", sep="\t",col.names=T,row.names=F)

random_sites_75_percent <- random_shuffled_sites[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
random_sites_75_percent
total_budget_75_percent
sum(random_sites_75_percent$COST_Travel_plus, na.rm = TRUE) # check if total cost is 75% of total budget
write.table(random_sites_75_percent, file="CIdata_without_2011_random_sites_75_percent.txt", sep="\t",col.names=T,row.names=F)

random_sites_100_percent <- random_shuffled_sites
random_sites_100_percent
total_budget_100_percent
sum(random_sites_100_percent$COST_Travel_plus, na.rm = TRUE) # check if total cost is 100% of total budget
write.table(random_sites_100_percent, file="CIdata_without_2011_random_sites_100_percent.txt", sep="\t",col.names=T,row.names=F)

## Order sites systematically -> algorithm that selects sites systematically for 25% - 50% - 75% - 100% of budget 
head(datatable_CIdata_without_2011) # check if CIdata has one row per waypoint
CIdata_without_2011_systematic <- datatable_CIdata_without_2011[-(1001:1017), ] # remove last 18 sites from dataframe in order to make it possible to split 

# split dataframe in 100 small dataframes based on area (dataframe 1: site 1-10, dataframe 2: site 11-20, etc)
splitted_dataframe_without_2011 <- split(CIdata_without_2011_systematic, rep(1:100, each=10))
class(splitted_dataframe_without_2011) # check class; should be a list with 100 elements, containing 10 rows each  
# define list to store shuffled dataframes for further calculation 
systematic_sites_without_2011_25_percent <- list()
systematic_sites_without_2011_50_percent <- list()
systematic_sites_without_2011_75_percent <- list()
# create lists with systematically organised sites for different budgets 
for (i in 1:100){
  kk <- splitted_dataframe_without_2011[[i]] # take the first dataframe out of the list (containing first 10 sites)
  kk <- as.data.table(kk)
  shuffle_sites <- kk[sample(nrow(kk))] # randomize sites
  shuffle_sites_25_percent <- shuffle_sites[cumsum(COST_Travel_plus_Survey) <= 0.01 * total_budget_25_percent, ] # take a number of sites with survey budget = 0.25% or
  # total budget (and do that 100 times to have a total budget of 25% of total island) -> it is done this way
  # to ensure sites are sytematically chosen across the island 
  shuffle_sites_50_percent <- shuffle_sites[cumsum(COST_Travel_plus_Survey) <= 0.01 * total_budget_50_percent, ]
  shuffle_sites_75_percent <- shuffle_sites[cumsum(COST_Travel_plus_Survey) <= 0.01 * total_budget_75_percent, ]
  systematic_sites_without_2011_25_percent[[i]] <- shuffle_sites_25_percent
  systematic_sites_without_2011_50_percent[[i]] <- shuffle_sites_50_percent
  systematic_sites_without_2011_75_percent[[i]] <- shuffle_sites_75_percent
}
systematic_sites_without_2011_25_percent #check if list is filled with correct dataframes 
CIdata_without_2011_systematic_survey_25_percent_budget <- as.data.frame(do.call("rbind", systematic_sites_without_2011_25_percent)) # convert list of dataframes into one dataframe
write.table(CIdata_without_2011_systematic_survey_25_percent_budget, file="CIdata_without_2011_systematic_survey_25_percent_budget.txt", sep="\t",col.names=T,row.names=F)
systematic_sites_without_2011_50_percent
CIdata_without_2011_systematic_survey_50_percent_budget <- as.data.frame(do.call("rbind", systematic_sites_without_2011_50_percent))
write.table(CIdata_without_2011_systematic_survey_50_percent_budget, file="CIdata_without_2011_systematic_survey_50_percent_budget.txt", sep="\t",col.names=T,row.names=F)
systematic_sites_without_2011_75_percent
CIdata_without_2011_systematic_survey_75_percent_budget <- as.data.frame(do.call("rbind", systematic_sites_without_2011_75_percent))
write.table(CIdata_without_2011_systematic_survey_75_percent_budget, file="CIdata_without_2011_systematic_survey_75_percent_budget.txt", sep="\t",col.names=T,row.names=F)
CIdata_without_2011_systematic_survey_100_percent_budget <- datatable_CIdata_without_2011
write.table(CIdata_without_2011_systematic_survey_100_percent_budget, file="CIdata_without_2011_systematic_survey_100_percent_budget.txt", sep="\t",col.names=T,row.names=F)


####################################### Calculate cost without 2013 ###################################################
# Define profit and cost per survey grid cell
profit_raster <- read.asc("prob_occ_SC_predicted_to_2013.asc") # this prediction is built without 2001 data 
raster_prob_occ <- raster(profit_raster) # change asci file into a raster file 
class(raster_prob_occ)
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata_without_2013)) {
  cat(round(10*i/nrow(CIdata_without_2013),digits=2),"%\n",sep="")
  x = CIdata_without_2013[i,"avg_X_MARK"]
  y = CIdata_without_2013[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(raster_prob_occ,matrix(data = c(x,y),ncol=2))))
}
CIdata_without_2013$Profit <- profit # add profit values to dataframe 
# read in cost file 
cost <- CIdata_without_2013$COST_Travel_plus_Survey 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata_without_2013$Knapsack_profit_per_cost <- knapsack

## Organize sites to survey for 25% - 50% - 75% and 100% of budget
CIdata_without_2013 <- CIdata_without_2013[order(CIdata_without_2013$WPT), ] # Order CIdata by WPT 
CIdata_without_2013 <- CIdata_without_2013[!duplicated(CIdata_without_2013$WPT), ] # Keep one row per waypoint
head(CIdata_without_2013)
write.table(CIdata_without_2013, file="CIdata_without_2013_with_Profit_Knapsack_per_WPT.txt", sep="\t",col.names=T,row.names=F)

## Total cost of surveying all 1018 sites
total_cost <- sum(CIdata_without_2013$COST_Travel_plus_Survey, na.rm=TRUE) # total cost to survey all sites once (= total budget)
total_cost
total_budget_25_percent <- 0.25*total_cost # 25% of budget
total_budget_50_percent <- 0.5*total_cost # 50 % of budget
total_budget_75_percent <- 0.75*total_cost # 75 % of budget
total_budget_100_percent <- total_cost # full budget

## Order sites based on probability of occurrence -> to do the survey based on decreasing Pr(occ), while including the cost
library(data.table) # use package data.table which wraps a data.table around a data.frame
datatable_CIdata_without_2013 <- as.data.table(CIdata_without_2013)
setorder(datatable_CIdata_without_2013, -Profit) # order CIdata.table by the Profit column in descending order (highest Pr(occ) first)
datatable_CIdata_without_2013 # check if ordered by Profit
datatable_CIdata_without_2013 <- datatable_CIdata_without_2013[-1, ] # remove the first row because profit (= probability of occurrence is NA)
datatable_CIdata_without_2013 # check if NA in Profit and Knapsack row has disappeared
# Recalculate total cost to survey 1017 sites (because NAs are removed)
total_cost <- sum(datatable_CIdata_without_2013$COST_Travel_plus_Survey)
total_cost
total_budget_25_percent <- 0.25*total_cost # 25% of budget
total_budget_50_percent <- 0.5*total_cost # 50 % of budget
total_budget_75_percent <- 0.75*total_cost # 75 % of budget
total_budget_100_percent <- total_cost # full budget
# Group sites for different budgets based on decreasing probability of occurrence 
CIdata_without_2013_Pr_Occ_25_percent_of_budget <- datatable_CIdata_without_2013[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ] # take the first x columns until 25% of budget is reached (sites ordered by Pr(occ))
CIdata_without_2013_Pr_Occ_50_percent_of_budget <- datatable_CIdata_without_2013[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ] 
CIdata_without_2013_Pr_Occ_75_percent_of_budget <- datatable_CIdata_without_2013[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
CIdata_without_2013_Pr_Occ_100_percent_of_budget <- datatable_CIdata_without_2013
write.table(CIdata_without_2013_Pr_Occ_25_percent_of_budget, file="CIdata_without_2013_Pr_Occ_25_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2013_Pr_Occ_50_percent_of_budget, file="CIdata_without_2013_Pr_Occ_50_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2013_Pr_Occ_75_percent_of_budget, file="CIdata_without_2013_Pr_Occ_75_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2013_Pr_Occ_100_percent_of_budget, file="CIdata_without_2013_Pr_Occ_100_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)

## Order sites based on Knapsack calculation (benefit/cost)
setorder(datatable_CIdata_without_2013, -Knapsack_profit_per_cost) # order CIdata.table by column Knapsack_profit_per_cost (highest profit/cost first)
datatable_CIdata_without_2013 # check if ordered by Knapsack
CIdata_without_2013_Knapsack_25_percent_of_budget <- datatable_CIdata_without_2013[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ] # subset CIdata.table with total_cost_25_percent
CIdata_without_2013_Knapsack_50_percent_of_budget <- datatable_CIdata_without_2013[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ]
CIdata_without_2013_Knapsack_75_percent_of_budget <- datatable_CIdata_without_2013[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
CIdata_without_2013_Knapsack_100_percent_of_budget <- datatable_CIdata_without_2013
write.table(CIdata_without_2013_Knapsack_25_percent_of_budget, file="CIdata_without_2013_Knapsack_25_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2013_Knapsack_50_percent_of_budget, file="CIdata_without_2013_Knapsack_50_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2013_Knapsack_75_percent_of_budget, file="CIdata_without_2013_Knapsack_75_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2013_Knapsack_100_percent_of_budget, file="CIdata_without_2013_Knapsack_100_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)

## Order sites based on expert opinion (managers' best guess):
# Dion: only survey sites around the edges (low elevation) and survey a small number of sites on the plateau
# Pete & Pinky: make the gridcells larger 
setorder(datatable_CIdata_without_2013, Elevation) # order CIdata.table by column Elevation in increasing order (from low elevation to high)
# 25% budget
CIdata_without_2013_Elevation_25_random_pick_above_250m <- datatable_CIdata_without_2013[sample(which(Elevation>250), size=25), ] # randomly pick 25 sites on the plateau (above 250m)
cost_survey_25_plateau_sites <- sum(CIdata_without_2013_Elevation_25_random_pick_above_250m$COST_Travel_plus_Survey)
cost_survey_25_plateau_sites
remaining_budget_25_percent <- total_budget_25_percent - cost_survey_25_plateau_sites # this is the remaining budget that can be used to survey sites at lower elevation
remaining_budget_25_percent
total_budget_25_percent # check
CIdata_without_2013_Elevation_25_percent_of_budget <- datatable_CIdata_without_2013[cumsum(COST_Travel_plus_Survey)<=remaining_budget_25_percent, ] # extract those rows with sites at lowest elevation for which cost to survey lies within the remaining budget
CIdata_without_2013_Elevation_25_percent_of_budget
write.table(CIdata_without_2013_Elevation_25_percent_of_budget, file="CIdata_without_2013_managers_guess_25_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
WPT_25_plateau <- CIdata_without_2013_Elevation_25_random_pick_above_250m$WPT # those sites will be surveyed on the plateau
WPT_25_plateau
WPT_25_low_elevation <- CIdata_without_2013_Elevation_25_percent_of_budget$WPT # those lower elevation sites will be surveyed
WPT_25_low_elevation
# 50% budget
CIdata_without_2013_Elevation_50_random_pick_above_250m <- datatable_CIdata_without_2013[sample(which(Elevation>250), size=50), ] # randomly pick 50 sites on the plateau (above 250m)
cost_survey_50_plateau_sites <- sum(CIdata_without_2013_Elevation_50_random_pick_above_250m$COST_Travel_plus_Survey)
cost_survey_50_plateau_sites
remaining_budget_50_percent <- total_budget_50_percent - cost_survey_50_plateau_sites # this is the remaining budget that can be used to survey sites at lower elevation
remaining_budget_50_percent
total_budget_50_percent # check
CIdata_without_2013_Elevation_50_percent_of_budget <- datatable_CIdata_without_2013[cumsum(COST_Travel_plus_Survey) <= remaining_budget_50_percent, ] # extract those rows with sites at lowest elevation for which cost to survey lies within the remaining budget
CIdata_without_2013_Elevation_50_percent_of_budget
write.table(CIdata_without_2013_Elevation_50_percent_of_budget, file="CIdata_without_2013_managers_guess_50_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
WPT_50_plateau <- CIdata_without_2013_Elevation_50_random_pick_above_250m$WPT # these sites will be surveyed on the plateau
WPT_50_plateau
WPT_50_low_elevation <- CIdata_without_2013_Elevation_50_percent_of_budget$WPT # these lower elevation sites will be surveyed
WPT_50_low_elevation
# 75% budget
CIdata_without_2013_Elevation_75_random_pick_above_250m <- datatable_CIdata_without_2013[sample(which(Elevation>250), size=75), ] # randomly pick 75 sites on the plateau (above 250m)
cost_survey_75_plateau_sites <- sum(CIdata_without_2013_Elevation_75_random_pick_above_250m$COST_Travel_plus_Survey)
cost_survey_75_plateau_sites
remaining_budget_75_percent <- total_budget_75_percent - cost_survey_75_plateau_sites
remaining_budget_75_percent
total_budget_75_percent # check
CIdata_without_2013_Elevation_75_percent_of_budget <- datatable_CIdata_without_2013[cumsum(COST_Travel_plus_Survey) <= remaining_budget_75_percent, ] # extract those rows with sites at lowest elevation for which cost to survey lies within the remaining budget
CIdata_without_2013_Elevation_75_percent_of_budget
write.table(CIdata_without_2013_Elevation_75_percent_of_budget, file="CIdata_without_2013_managers_guess_75_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
WPT_75_plateau <- CIdata_without_2013_Elevation_75_random_pick_above_250m$WPT # these sites will be surveyed on the plateau
WPT_75_plateau
WPT_75_low_elevation <- CIdata_without_2013_Elevation_75_percent_of_budget$WPT # these lower elevation sites will be surveyed
WPT_75_low_elevation
# 100% budget
CIdata_without_2013_Elevation_100_percent_of_budget <- datatable_CIdata_without_2013
CIdata_without_2013_Elevation_100_percent_of_budget
write.table(CIdata_without_2013_Elevation_100_percent_of_budget, file="CIdata_without_2013_managers_guess_100_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)

## Order sites randomly
random_shuffled_sites <- datatable_CIdata_without_2013[sample(nrow(datatable_CIdata_without_2013)), ]
class(random_shuffled_sites)

random_sites_25_percent <- random_shuffled_sites[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ]
random_sites_25_percent 
total_budget_25_percent
sum(random_sites_25_percent$COST_Travel_plus_Survey, na.rm = TRUE) # check if total cost is 25% of total budget
write.table(random_sites_25_percent, file="CIdata_without_2013_random_sites_25_percent.txt", sep="\t",col.names=T,row.names=F)

random_sites_50_percent <- random_shuffled_sites[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ]
random_sites_50_percent
total_budget_50_percent
sum(random_sites_50_percent$COST_Travel_plus_Survey, na.rm = TRUE) # check if total cost is 50% of total budget
write.table(random_sites_50_percent, file="CIdata_without_2013_random_sites_50_percent.txt", sep="\t",col.names=T,row.names=F)

random_sites_75_percent <- random_shuffled_sites[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
random_sites_75_percent
total_budget_75_percent
sum(random_sites_75_percent$COST_Travel_plus, na.rm = TRUE) # check if total cost is 75% of total budget
write.table(random_sites_75_percent, file="CIdata_without_2013_random_sites_75_percent.txt", sep="\t",col.names=T,row.names=F)

random_sites_100_percent <- random_shuffled_sites
random_sites_100_percent
total_budget_100_percent
sum(random_sites_100_percent$COST_Travel_plus, na.rm = TRUE) # check if total cost is 100% of total budget
write.table(random_sites_100_percent, file="CIdata_without_2013_random_sites_100_percent.txt", sep="\t",col.names=T,row.names=F)


## Order sites systematically -> algorithm that selects sites systematically for 25% - 50% - 75% - 100% of budget 
head(datatable_CIdata_without_2013) # check if CIdata has one row per waypoint
CIdata_without_2013_systematic <- datatable_CIdata_without_2013[-(1001:1017), ] # remove last 18 sites from dataframe in order to make it possible to split 

# split dataframe in 100 small dataframes based on area (dataframe 1: site 1-10, dataframe 2: site 11-20, etc)
splitted_dataframe_without_2013 <- split(CIdata_without_2013_systematic, rep(1:100, each=10))
class(splitted_dataframe_without_2013) # check class; should be a list with 100 elements, containing 10 rows each  
# define list to store shuffled dataframes for further calculation 
systematic_sites_without_2013_25_percent <- list()
systematic_sites_without_2013_50_percent <- list()
systematic_sites_without_2013_75_percent <- list()
# create lists with systematically organised sites for different budgets 
for (i in 1:100){
  kk <- splitted_dataframe_without_2013[[i]] # take the first dataframe out of the list (containing first 10 sites)
  kk <- as.data.table(kk)
  shuffle_sites <- kk[sample(nrow(kk))] # randomize sites
  shuffle_sites_25_percent <- shuffle_sites[cumsum(COST_Travel_plus_Survey) <= 0.01 * total_budget_25_percent, ] # take a number of sites with survey budget = 0.25% or
  # total budget (and do that 100 times to have a total budget of 25% of total island) -> it is done this way
  # to ensure sites are sytematically chosen across the island 
  shuffle_sites_50_percent <- shuffle_sites[cumsum(COST_Travel_plus_Survey) <= 0.01 * total_budget_50_percent, ]
  shuffle_sites_75_percent <- shuffle_sites[cumsum(COST_Travel_plus_Survey) <= 0.01 * total_budget_75_percent, ]
  systematic_sites_without_2013_25_percent[[i]] <- shuffle_sites_25_percent
  systematic_sites_without_2013_50_percent[[i]] <- shuffle_sites_50_percent
  systematic_sites_without_2013_75_percent[[i]] <- shuffle_sites_75_percent
}
systematic_sites_without_2013_25_percent #check if list is filled with correct dataframes 
CIdata_without_2013_systematic_survey_25_percent_budget <- as.data.frame(do.call("rbind", systematic_sites_without_2013_25_percent)) # convert list of dataframes into one dataframe
write.table(CIdata_without_2013_systematic_survey_25_percent_budget, file="CIdata_without_2013_systematic_survey_25_percent_budget.txt", sep="\t",col.names=T,row.names=F)
systematic_sites_without_2013_50_percent
CIdata_without_2013_systematic_survey_50_percent_budget <- as.data.frame(do.call("rbind", systematic_sites_without_2013_50_percent))
write.table(CIdata_without_2013_systematic_survey_50_percent_budget, file="CIdata_without_2013_systematic_survey_50_percent_budget.txt", sep="\t",col.names=T,row.names=F)
systematic_sites_without_2013_75_percent
CIdata_without_2013_systematic_survey_75_percent_budget <- as.data.frame(do.call("rbind", systematic_sites_without_2013_75_percent))
write.table(CIdata_without_2013_systematic_survey_75_percent_budget, file="CIdata_without_2013_systematic_survey_75_percent_budget.txt", sep="\t",col.names=T,row.names=F)
CIdata_without_2013_systematic_survey_100_percent_budget <- datatable_CIdata_without_2013
write.table(CIdata_without_2013_systematic_survey_100_percent_budget, file="CIdata_without_2013_systematic_survey_100_percent_budget.txt", sep="\t",col.names=T,row.names=F)




####################################### Calculate cost without 2015 ###################################################
# Define profit and cost per survey grid cell
profit_raster <- read.asc("prob_occ_SC_predicted_to_2015.asc") # this prediction is built without 2001 data 
raster_prob_occ <- raster(profit_raster) # change asci file into a raster file 
class(raster_prob_occ)
profit <- vector()
# extract profit values for each survey point
for (i in 1:nrow(CIdata_without_2015)) {
  cat(round(10*i/nrow(CIdata_without_2015),digits=2),"%\n",sep="")
  x = CIdata_without_2015[i,"avg_X_MARK"]
  y = CIdata_without_2015[i,"avg_Y_MARK"]
  profit <- c(profit, unname(extract(raster_prob_occ,matrix(data = c(x,y),ncol=2))))
}
CIdata_without_2015$Profit <- profit # add profit values to dataframe 
# read in cost file 
cost <- CIdata_without_2015$COST_Travel_plus_Survey 
# Knapsack formulation for each cell (benefit/cost) 
knapsack <- profit/cost
CIdata_without_2015$Knapsack_profit_per_cost <- knapsack

## Organize sites to survey for 25% - 50% - 75% and 100% of budget
CIdata_without_2015 <- CIdata_without_2015[order(CIdata_without_2015$WPT), ] # Order CIdata by WPT 
CIdata_without_2015 <- CIdata_without_2015[!duplicated(CIdata_without_2015$WPT), ] # Keep one row per waypoint
head(CIdata_without_2015)
write.table(CIdata_without_2015, file="CIdata_without_2015_with_Profit_Knapsack_per_WPT.txt", sep="\t",col.names=T,row.names=F)

## Total cost of surveying all 1018 sites
total_cost <- sum(CIdata_without_2015$COST_Travel_plus_Survey, na.rm=TRUE) # total cost to survey all sites once (= total budget)
total_cost
total_budget_25_percent <- 0.25*total_cost # 25% of budget
total_budget_50_percent <- 0.5*total_cost # 50 % of budget
total_budget_75_percent <- 0.75*total_cost # 75 % of budget
total_budget_100_percent <- total_cost # full budget

## Order sites based on probability of occurrence -> to do the survey based on decreasing Pr(occ), while including the cost
library(data.table) # use package data.table which wraps a data.table around a data.frame
datatable_CIdata_without_2015 <- as.data.table(CIdata_without_2015)
setorder(datatable_CIdata_without_2015, -Profit) # order CIdata.table by the Profit column in descending order (highest Pr(occ) first)
datatable_CIdata_without_2015 # check if ordered by Profit
datatable_CIdata_without_2015 <- datatable_CIdata_without_2015[-1, ] # remove the first row because profit (= probability of occurrence is NA)
datatable_CIdata_without_2015 # check if NA in Profit and Knapsack row has disappeared
# Recalculate total cost to survey 1017 sites (because NAs are removed)
total_cost <- sum(datatable_CIdata_without_2015$COST_Travel_plus_Survey)
total_cost
total_budget_25_percent <- 0.25*total_cost # 25% of budget
total_budget_50_percent <- 0.5*total_cost # 50 % of budget
total_budget_75_percent <- 0.75*total_cost # 75 % of budget
total_budget_100_percent <- total_cost # full budget
# Group sites for different budgets based on decreasing probability of occurrence 
CIdata_without_2015_Pr_Occ_25_percent_of_budget <- datatable_CIdata_without_2015[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ] # take the first x columns until 25% of budget is reached (sites ordered by Pr(occ))
CIdata_without_2015_Pr_Occ_50_percent_of_budget <- datatable_CIdata_without_2015[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ] 
CIdata_without_2015_Pr_Occ_75_percent_of_budget <- datatable_CIdata_without_2015[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
CIdata_without_2015_Pr_Occ_100_percent_of_budget <- datatable_CIdata_without_2015
write.table(CIdata_without_2015_Pr_Occ_25_percent_of_budget, file="CIdata_without_2015_Pr_Occ_25_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2015_Pr_Occ_50_percent_of_budget, file="CIdata_without_2015_Pr_Occ_50_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2015_Pr_Occ_75_percent_of_budget, file="CIdata_without_2015_Pr_Occ_75_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2015_Pr_Occ_100_percent_of_budget, file="CIdata_without_2015_Pr_Occ_100_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)

## Order sites based on Knapsack calculation (benefit/cost)
setorder(datatable_CIdata_without_2015, -Knapsack_profit_per_cost) # order CIdata.table by column Knapsack_profit_per_cost (highest profit/cost first)
datatable_CIdata_without_2015 # check if ordered by Knapsack
CIdata_without_2015_Knapsack_25_percent_of_budget <- datatable_CIdata_without_2015[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ] # subset CIdata.table with total_cost_25_percent
CIdata_without_2015_Knapsack_50_percent_of_budget <- datatable_CIdata_without_2015[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ]
CIdata_without_2015_Knapsack_75_percent_of_budget <- datatable_CIdata_without_2015[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
CIdata_without_2015_Knapsack_100_percent_of_budget <- datatable_CIdata_without_2015
write.table(CIdata_without_2015_Knapsack_25_percent_of_budget, file="CIdata_without_2015_Knapsack_25_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2015_Knapsack_50_percent_of_budget, file="CIdata_without_2015_Knapsack_50_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2015_Knapsack_75_percent_of_budget, file="CIdata_without_2015_Knapsack_75_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
write.table(CIdata_without_2015_Knapsack_100_percent_of_budget, file="CIdata_without_2015_Knapsack_100_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)

## Order sites based on expert opinion (managers' best guess):
# Dion: only survey sites around the edges (low elevation) and survey a small number of sites on the plateau
# Pete & Pinky: make the gridcells larger 
setorder(datatable_CIdata_without_2015, Elevation) # order CIdata.table by column Elevation in increasing order (from low elevation to high)
# 25% budget
CIdata_without_2015_Elevation_25_random_pick_above_250m <- datatable_CIdata_without_2015[sample(which(Elevation>250), size=25), ] # randomly pick 25 sites on the plateau (above 250m)
cost_survey_25_plateau_sites <- sum(CIdata_without_2015_Elevation_25_random_pick_above_250m$COST_Travel_plus_Survey)
cost_survey_25_plateau_sites
remaining_budget_25_percent <- total_budget_25_percent - cost_survey_25_plateau_sites # this is the remaining budget that can be used to survey sites at lower elevation
remaining_budget_25_percent
total_budget_25_percent # check
CIdata_without_2015_Elevation_25_percent_of_budget <- datatable_CIdata_without_2015[cumsum(COST_Travel_plus_Survey)<=remaining_budget_25_percent, ] # extract those rows with sites at lowest elevation for which cost to survey lies within the remaining budget
CIdata_without_2015_Elevation_25_percent_of_budget
write.table(CIdata_without_2015_Elevation_25_percent_of_budget, file="CIdata_without_2015_managers_guess_25_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
WPT_25_plateau <- CIdata_without_2015_Elevation_25_random_pick_above_250m$WPT # those sites will be surveyed on the plateau
WPT_25_plateau
WPT_25_low_elevation <- CIdata_without_2015_Elevation_25_percent_of_budget$WPT # those lower elevation sites will be surveyed
WPT_25_low_elevation
# 50% budget
CIdata_without_2015_Elevation_50_random_pick_above_250m <- datatable_CIdata_without_2015[sample(which(Elevation>250), size=50), ] # randomly pick 50 sites on the plateau (above 250m)
cost_survey_50_plateau_sites <- sum(CIdata_without_2015_Elevation_50_random_pick_above_250m$COST_Travel_plus_Survey)
cost_survey_50_plateau_sites
remaining_budget_50_percent <- total_budget_50_percent - cost_survey_50_plateau_sites # this is the remaining budget that can be used to survey sites at lower elevation
remaining_budget_50_percent
total_budget_50_percent # check
CIdata_without_2015_Elevation_50_percent_of_budget <- datatable_CIdata_without_2015[cumsum(COST_Travel_plus_Survey) <= remaining_budget_50_percent, ] # extract those rows with sites at lowest elevation for which cost to survey lies within the remaining budget
CIdata_without_2015_Elevation_50_percent_of_budget
write.table(CIdata_without_2015_Elevation_50_percent_of_budget, file="CIdata_without_2015_managers_guess_50_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
WPT_50_plateau <- CIdata_without_2015_Elevation_50_random_pick_above_250m$WPT # these sites will be surveyed on the plateau
WPT_50_plateau
WPT_50_low_elevation <- CIdata_without_2015_Elevation_50_percent_of_budget$WPT # these lower elevation sites will be surveyed
WPT_50_low_elevation
# 75% budget
CIdata_without_2015_Elevation_75_random_pick_above_250m <- datatable_CIdata_without_2015[sample(which(Elevation>250), size=75), ] # randomly pick 75 sites on the plateau (above 250m)
cost_survey_75_plateau_sites <- sum(CIdata_without_2015_Elevation_75_random_pick_above_250m$COST_Travel_plus_Survey)
cost_survey_75_plateau_sites
remaining_budget_75_percent <- total_budget_75_percent - cost_survey_75_plateau_sites
remaining_budget_75_percent
total_budget_75_percent # check
CIdata_without_2015_Elevation_75_percent_of_budget <- datatable_CIdata_without_2015[cumsum(COST_Travel_plus_Survey) <= remaining_budget_75_percent, ] # extract those rows with sites at lowest elevation for which cost to survey lies within the remaining budget
CIdata_without_2015_Elevation_75_percent_of_budget
write.table(CIdata_without_2015_Elevation_75_percent_of_budget, file="CIdata_without_2015_managers_guess_75_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)
WPT_75_plateau <- CIdata_without_2015_Elevation_75_random_pick_above_250m$WPT # these sites will be surveyed on the plateau
WPT_75_plateau
WPT_75_low_elevation <- CIdata_without_2015_Elevation_75_percent_of_budget$WPT # these lower elevation sites will be surveyed
WPT_75_low_elevation
# 100% budget
CIdata_without_2015_Elevation_100_percent_of_budget <- datatable_CIdata_without_2015
CIdata_without_2015_Elevation_100_percent_of_budget
write.table(CIdata_without_2015_Elevation_100_percent_of_budget, file="CIdata_without_2015_managers_guess_100_percent_of_budget.txt", sep="\t",col.names=T,row.names=F)

## Order sites randomly
random_shuffled_sites <- datatable_CIdata_without_2015[sample(nrow(datatable_CIdata_without_2015)), ]
class(random_shuffled_sites)

random_sites_25_percent <- random_shuffled_sites[cumsum(COST_Travel_plus_Survey) <= total_budget_25_percent, ]
random_sites_25_percent 
total_budget_25_percent
sum(random_sites_25_percent$COST_Travel_plus_Survey, na.rm = TRUE) # check if total cost is 25% of total budget
write.table(random_sites_25_percent, file="CIdata_without_2015_random_sites_25_percent.txt", sep="\t",col.names=T,row.names=F)

random_sites_50_percent <- random_shuffled_sites[cumsum(COST_Travel_plus_Survey) <= total_budget_50_percent, ]
random_sites_50_percent
total_budget_50_percent
sum(random_sites_50_percent$COST_Travel_plus_Survey, na.rm = TRUE) # check if total cost is 50% of total budget
write.table(random_sites_50_percent, file="CIdata_without_2015_random_sites_50_percent.txt", sep="\t",col.names=T,row.names=F)

random_sites_75_percent <- random_shuffled_sites[cumsum(COST_Travel_plus_Survey) <= total_budget_75_percent, ]
random_sites_75_percent
total_budget_75_percent
sum(random_sites_75_percent$COST_Travel_plus, na.rm = TRUE) # check if total cost is 75% of total budget
write.table(random_sites_75_percent, file="CIdata_without_2015_random_sites_75_percent.txt", sep="\t",col.names=T,row.names=F)

random_sites_100_percent <- random_shuffled_sites
random_sites_100_percent
total_budget_100_percent
sum(random_sites_100_percent$COST_Travel_plus, na.rm = TRUE) # check if total cost is 100% of total budget
write.table(random_sites_100_percent, file="CIdata_without_2015_random_sites_100_percent.txt", sep="\t",col.names=T,row.names=F)

## Order sites systematically -> algorithm that selects sites systematically for 25% - 50% - 75% - 100% of budget 
head(datatable_CIdata_without_2015) # check if CIdata has one row per waypoint
CIdata_without_2015_systematic <- datatable_CIdata_without_2015[-(1001:1017), ] # remove last 18 sites from dataframe in order to make it possible to split 

# split dataframe in 100 small dataframes based on area (dataframe 1: site 1-10, dataframe 2: site 11-20, etc)
splitted_dataframe_without_2015 <- split(CIdata_without_2015_systematic, rep(1:100, each=10))
class(splitted_dataframe_without_2015) # check class; should be a list with 100 elements, containing 10 rows each  
# define list to store shuffled dataframes for further calculation 
systematic_sites_without_2015_25_percent <- list()
systematic_sites_without_2015_50_percent <- list()
systematic_sites_without_2015_75_percent <- list()
# create lists with systematically organised sites for different budgets 
for (i in 1:100){
  kk <- splitted_dataframe_without_2015[[i]] # take the first dataframe out of the list (containing first 10 sites)
  kk <- as.data.table(kk)
  shuffle_sites <- kk[sample(nrow(kk))] # randomize sites
  shuffle_sites_25_percent <- shuffle_sites[cumsum(COST_Travel_plus_Survey) <= 0.01 * total_budget_25_percent, ] # take a number of sites with survey budget = 0.25% or
  # total budget (and do that 100 times to have a total budget of 25% of total island) -> it is done this way
  # to ensure sites are sytematically chosen across the island 
  shuffle_sites_50_percent <- shuffle_sites[cumsum(COST_Travel_plus_Survey) <= 0.01 * total_budget_50_percent, ]
  shuffle_sites_75_percent <- shuffle_sites[cumsum(COST_Travel_plus_Survey) <= 0.01 * total_budget_75_percent, ]
  systematic_sites_without_2015_25_percent[[i]] <- shuffle_sites_25_percent
  systematic_sites_without_2015_50_percent[[i]] <- shuffle_sites_50_percent
  systematic_sites_without_2015_75_percent[[i]] <- shuffle_sites_75_percent
}
systematic_sites_without_2015_25_percent #check if list is filled with correct dataframes 
CIdata_without_2015_systematic_survey_25_percent_budget <- as.data.frame(do.call("rbind", systematic_sites_without_2015_25_percent)) # convert list of dataframes into one dataframe
write.table(CIdata_without_2015_systematic_survey_25_percent_budget, file="CIdata_without_2015_systematic_survey_25_percent_budget.txt", sep="\t",col.names=T,row.names=F)
systematic_sites_without_2015_50_percent
CIdata_without_2015_systematic_survey_50_percent_budget <- as.data.frame(do.call("rbind", systematic_sites_without_2015_50_percent))
write.table(CIdata_without_2015_systematic_survey_50_percent_budget, file="CIdata_without_2015_systematic_survey_50_percent_budget.txt", sep="\t",col.names=T,row.names=F)
systematic_sites_without_2015_75_percent
CIdata_without_2015_systematic_survey_75_percent_budget <- as.data.frame(do.call("rbind", systematic_sites_without_2015_75_percent))
write.table(CIdata_without_2015_systematic_survey_75_percent_budget, file="CIdata_without_2015_systematic_survey_75_percent_budget.txt", sep="\t",col.names=T,row.names=F)
CIdata_without_2015_systematic_survey_100_percent_budget <- datatable_CIdata_without_2015
write.table(CIdata_without_2015_systematic_survey_100_percent_budget, file="CIdata_without_2015_systematic_survey_100_percent_budget.txt", sep="\t",col.names=T,row.names=F)

