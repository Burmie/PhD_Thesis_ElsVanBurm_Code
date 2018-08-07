## Load required packages
library(data.table)
library(adagio)

## Declare some vectors and lists 
profit <- vector()
predictions <- c("prediction_to_2001.Rda","prediction_to_2003.Rda", "prediction_to_2005.Rda", "prediction_to_2007.Rda", 
                 "prediction_to_2009.Rda", "prediction_to_2011.Rda","prediction_to_2013.Rda", "prediction_to_2015.Rda")
full_datasets <- c("CIdata2001","CIdata2003", "CIdata2005","CIdata2007","CIdata2009", "CIdata2011", "CIdata2013","CIdata2015")
dataframes_with_R_vs_C_value_RESCALED_COST <- list()

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
  cost <- round(df$COST_Travel_plus_Survey_rescaled) # read in cost file - rounded and multiplied by 100 to make it consistent with knapsack calculations 
  R_vs_C <- profit/cost # reward vs cost for each survey point
  df$profit_per_cost <- R_vs_C # add column 
  dataframes_with_R_vs_C_value_RESCALED_COST[[j]] <- df # add to list
  save(dataframes_with_R_vs_C_value_RESCALED_COST, file="dataframes_with_R_vs_C_values_RESCALED_COST.Rda") # dataframes_with_R_vs_C_values
  # is a list with 8 elements, each element containing values for p(occ) predicted to a specific year,
  # corresponding with that year that has been excluded from the dataset to train the model  
} 

## Define total and proportions of survey budget (based on CIdata2015 - most sites were surveyed)
df <- dataframes_with_R_vs_C_value_RESCALED_COST[[8]] # choose CIdata2015 for total budget calculation 
total_cost <- round(sum(df$COST_Travel_plus_Survey_rescaled, na.rm=TRUE)) # total cost to survey all sites once (= total budget)
total_budget_25_percent <- 0.25 * total_cost # 25% of budget
total_budget_50_percent <- 0.5 * total_cost # 50 % of budget
total_budget_75_percent <- 0.75 * total_cost # 75 % of budget
total_budget_100_percent <- total_cost # full budget

## Sites are ranked based on different algorithms
## Knapsack algorithm 
for (k in 1:length(full_datasets)){
  df <- dataframes_with_R_vs_C_value_RESCALED_COST[[k]] # load dataframe
  w <- round(as.vector(df$COST_Travel_plus_Survey_rescaled)) # vector with weights (i.e. survey cost) for each site
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
  df <- dataframes_with_R_vs_C_value_RESCALED_COST[[k]]
  df$COST_Travel_plus_Survey_rescaled <- round(df$COST_Travel_plus_Survey_rescaled)
  df <- setorder(as.data.table(df), -profit_per_cost) # Order sites in decreasing Knapsack value
  df_R_vs_C_25_percent <- df[cumsum(COST_Travel_plus_Survey_rescaled) <= total_budget_25_percent, ]
  df_R_vs_C_50_percent <- df[cumsum(COST_Travel_plus_Survey_rescaled) <= total_budget_50_percent, ]
  df_R_vs_C_75_percent <- df[cumsum(COST_Travel_plus_Survey_rescaled) <= total_budget_75_percent, ]
  WPTs_R_vs_C_25_percent <- df_R_vs_C_25_percent$WPT
  save(WPTs_R_vs_C_25_percent, file=paste("R_vs_C_WPTs_25_percent_budget_",full_datasets[k],".Rda",sep=""))
  WPTs_R_vs_C_50_percent <- df_R_vs_C_50_percent$WPT
  save(WPTs_R_vs_C_50_percent, file=paste("R_vs_C_WPTs_50_percent_budget_",full_datasets[k],".Rda",sep=""))
  WPTs_R_vs_C_75_percent <- df_R_vs_C_75_percent$WPT
  save(WPTs_R_vs_C_75_percent, file=paste("R_vs_C_WPTs_75_percent_budget_",full_datasets[k],".Rda",sep=""))
  WPTs_R_vs_C_100_percent <- df$WPT
  save(WPTs_R_vs_C_100_percent, file=paste("R_vs_C_WPTs_100_percent_budget_",full_datasets[k],".Rda",sep=""))
}

# Sites selected by probability of occurrence of supercolony 
for (k in 1:length(full_datasets)){
  df <- dataframes_with_R_vs_C_value_RESCALED_COST[[k]]
  df$COST_Travel_plus_Survey_rescaled <- round(df$COST_Travel_plus_Survey_rescaled)
  df <- setorder(as.data.table(df), -profit) # Order sites in decreasing P(occ) value
  df_Pocc_25_percent <- df[cumsum(COST_Travel_plus_Survey_rescaled) <= total_budget_25_percent, ]
  df_Pocc_50_percent <- df[cumsum(COST_Travel_plus_Survey_rescaled) <= total_budget_50_percent, ]
  df_Pocc_75_percent <- df[cumsum(COST_Travel_plus_Survey_rescaled) <= total_budget_75_percent, ]
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
  df <- dataframes_with_R_vs_C_value_RESCALED_COST[[k]]
  df$COST_Travel_plus_Survey_rescaled <- round(df$COST_Travel_plus_Survey_rescaled)
  df <- setorder(as.data.table(df), Elevation) # Order sites based on increasing Elevation (from low to high elevation)
  df_Elevation_random_pick_above_250m_25_percent <- df[sample(which(Elevation>250), size=25), ] # randomly picks 25 sites on the plateau (above 250m)
  df_Elevation_random_pick_above_250m_25_percent # those represent the sites that will be surveyed on the plateau
  #df_remaining <- df[-df_Elevation_random_pick_above_250m_25_percent$WPT, ]
  #df_remaining # those represent remaining sites after 25 sites were removed on the plateau 
  cost_survey_25_plateau_sites <- sum(df_Elevation_random_pick_above_250m_25_percent$COST_Travel_plus_Survey_rescaled)  
  cost_survey_25_plateau_sites # cost to survey 25 plateau sites
  remaining_budget_25_percent <- total_budget_25_percent - cost_survey_25_plateau_sites # this is the remaining budget that can be used to survey sites at lower elevation
  remaining_budget_25_percent
  df_Elevation_edges_25_percent_of_budget <- df[cumsum(COST_Travel_plus_Survey_rescaled)<=remaining_budget_25_percent, ] # extract those sites around the edges (lowest elevation) for which cost to survey lies within the remaining budget
  df_Elevation_edges_25_percent_of_budget
  WPTs_ExpOp_25_percent <- c(df_Elevation_random_pick_above_250m_25_percent$WPT, df_Elevation_edges_25_percent_of_budget$WPT) 
  save(WPTs_ExpOp_25_percent, file=paste("ExpOp_WPTs_25_percent_budget_",full_datasets[k],".Rda",sep=""))
  df_Elevation_random_pick_above_250m_50_percent <- df[sample(which(Elevation>250), size=50), ] # randomly picks 50 sites on the plateau (above 250m)
  df_Elevation_random_pick_above_250m_50_percent # those represent the sites that will be surveyed on the plateau
  #df_remaining <- df[-df_Elevation_random_pick_above_250m_50_percent$WPT, ]
  #df_remaining # those represent remaining sites after 25 sites were removed on the plateau 
  cost_survey_50_plateau_sites <- sum(df_Elevation_random_pick_above_250m_50_percent$COST_Travel_plus_Survey_rescaled)  
  cost_survey_50_plateau_sites # cost to survey 25 plateau sites
  remaining_budget_50_percent <- total_budget_50_percent - cost_survey_50_plateau_sites # this is the remaining budget that can be used to survey sites at lower elevation
  remaining_budget_50_percent
  df_Elevation_edges_50_percent_of_budget <- df[cumsum(COST_Travel_plus_Survey_rescaled)<=remaining_budget_50_percent, ] # extract those sites around the edges (lowest elevation) for which cost to survey lies within the remaining budget
  df_Elevation_edges_50_percent_of_budget
  WPTs_ExpOp_50_percent <- c(df_Elevation_random_pick_above_250m_50_percent$WPT, df_Elevation_edges_50_percent_of_budget$WPT) 
  save(WPTs_ExpOp_50_percent, file=paste("ExpOp_WPTs_50_percent_budget_",full_datasets[k],".Rda",sep=""))
  df_Elevation_random_pick_above_250m_75_percent <- df[sample(which(Elevation>250), size=75), ] # randomly picks 75 sites on the plateau (above 250m)
  df_Elevation_random_pick_above_250m_75_percent # those represent the sites that will be surveyed on the plateau
  #df_remaining <- df[-df_Elevation_random_pick_above_250m_75_percent$WPT, ]
  #df_remaining # those represent remaining sites after 25 sites were removed on the plateau 
  cost_survey_75_plateau_sites <- sum(df_Elevation_random_pick_above_250m_75_percent$COST_Travel_plus_Survey_rescaled)  
  cost_survey_75_plateau_sites # cost to survey 25 plateau sites
  remaining_budget_75_percent <- total_budget_75_percent - cost_survey_75_plateau_sites # this is the remaining budget that can be used to survey sites at lower elevation
  remaining_budget_75_percent
  df_Elevation_edges_75_percent_of_budget <- df[cumsum(COST_Travel_plus_Survey_rescaled)<=remaining_budget_75_percent, ] # extract those sites around the edges (lowest elevation) for which cost to survey lies within the remaining budget
  df_Elevation_edges_75_percent_of_budget
  WPTs_ExpOp_75_percent <- c(df_Elevation_random_pick_above_250m_75_percent$WPT, df_Elevation_edges_75_percent_of_budget$WPT) 
  save(WPTs_ExpOp_75_percent, file=paste("ExpOp_WPTs_75_percent_budget_",full_datasets[k],".Rda",sep=""))
  WPTs_ExpOp_100_percent <- df$WPT
  save(WPTs_ExpOp_100_percent, file=paste("ExpOp_WPTs_100_percent_budget_",full_datasets[k],".Rda",sep=""))
}  

# Evenly distributed sites 
for (k in 1:length(full_datasets)){
  Sequence_1 <- seq(0, nrow(dataframes_with_R_vs_C_value_RESCALED_COST[[k]]), 4)
  Sequence_2 <- seq(1, nrow(dataframes_with_R_vs_C_value_RESCALED_COST[[k]]), 4)
  Sequence_3 <- seq(2, nrow(dataframes_with_R_vs_C_value_RESCALED_COST[[k]]), 4)
  Reordered_sites <- c(Sequence_1, Sequence_2, Sequence_3)
  df <- dataframes_with_R_vs_C_value_RESCALED_COST[[k]]
  df$COST_Travel_plus_Survey_rescaled <- round(df$COST_Travel_plus_Survey_rescaled)
  df <- setorder(as.data.table(df), WPT) # Order sites by WPT
  df_1 <- df[Reordered_sites, ]
  df_Syst_25_percent <- df_1[cumsum(COST_Travel_plus_Survey_rescaled) <= total_budget_25_percent, ]
  WPTs_Syst_25_percent <- df_Syst_25_percent$WPT
  save(WPTs_Syst_25_percent, file=paste("Syst_WPTs_25_percent_budget_",full_datasets[k],".Rda",sep=""))
  df_Syst_50_percent <- df_1[cumsum(COST_Travel_plus_Survey_rescaled) <= total_budget_50_percent, ]
  WPTs_Syst_50_percent <- df_Syst_50_percent$WPT
  save(WPTs_Syst_50_percent, file=paste("Syst_WPTs_50_percent_budget_",full_datasets[k],".Rda",sep=""))
  df_Syst_75_percent <- df_1[cumsum(COST_Travel_plus_Survey_rescaled) <= total_budget_75_percent, ]
  WPTs_Syst_75_percent <- df_Syst_75_percent$WPT
  save(WPTs_Syst_75_percent, file=paste("Syst_WPTs_75_percent_budget_",full_datasets[k],".Rda",sep=""))
  WPTs_Syst_100_percent <- df$WPT
  save(WPTs_Syst_100_percent, file=paste("Syst_WPTs_100_percent_budget_",full_datasets[k],".Rda",sep=""))
}

# Backup evenly distributed sites 
# for (k in 1:length(full_datasets)){
#   Sequence_1 <- seq(1, (nrow(dataframes_with_R_vs_C_value_RESCALED_COST[[k]])-2), 4)
#   Sequence_2 <- c(Sequence_1, Sequence_1+1)
#   Sequence_3 <- c(Sequence_2, Sequence_2+1)
#   df <- dataframes_with_R_vs_C_value_RESCALED_COST[[k]]
#   df$COST_Travel_plus_Survey_rescaled <- round(df$COST_Travel_plus_Survey_rescaled)
#   df <- setorder(as.data.table(df), WPT) # Order sites by WPT
#   df_1 <- df[Sequence_2, ] 
#   df_Syst_25_percent <- df_1[cumsum(COST_Travel_plus_Survey_rescaled) <= total_budget_25_percent, ]
#   WPTs_Syst_25_percent <- df_Syst_25_percent$WPT
#   save(WPTs_Syst_25_percent, file=paste("Syst_WPTs_25_percent_budget_",full_datasets[k],".Rda",sep=""))
#   df_2 <- df[Sequence_2, ]
#   df_2 <- rbind(df_1, df_2)
#   df_Syst_50_percent <- df_2[cumsum(COST_Travel_plus_Survey_rescaled) <= total_budget_50_percent, ]
#   WPTs_Syst_50_percent <- df_Syst_50_percent$WPT
#   save(WPTs_Syst_50_percent, file=paste("Syst_WPTs_50_percent_budget_",full_datasets[k],".Rda",sep=""))
#   df_3 <- df[Sequence_3, ]
#   df_3 <- rbind(df_2, df_3)
#   df_Syst_75_percent <- df_3[cumsum(COST_Travel_plus_Survey_rescaled) <= total_budget_75_percent, ]
#   WPTs_Syst_75_percent <- df_Syst_75_percent$WPT
#   save(WPTs_Syst_75_percent, file=paste("Syst_WPTs_75_percent_budget_",full_datasets[k],".Rda",sep=""))
#   WPTs_Syst_100_percent <- df$WPT
#   save(WPTs_Syst_100_percent, file=paste("Syst_WPTs_100_percent_budget_",full_datasets[k],".Rda",sep=""))
# }

# Randomly distributed sites 
for (k in 1:length(full_datasets)){
  df <- as.data.table(dataframes_with_R_vs_C_value_RESCALED_COST[[k]])
  df$COST_Travel_plus_Survey_rescaled <- round(df$COST_Travel_plus_Survey_rescaled)
  kk <- sample(nrow(dataframes_with_R_vs_C_value_RESCALED_COST[[k]]))
  dk <- df[kk, ] # Randomly shuffle sites (different sites for each loop)
  df_Random_25_percent <- dk[cumsum(COST_Travel_plus_Survey_rescaled) <= total_budget_25_percent, ]
  WPTs_Random_25_percent <- df_Random_25_percent$WPT
  save(WPTs_Random_25_percent, file=paste("Random_WPTs_25_percent_budget_",full_datasets[k],".Rda",sep=""))
  df_Random_50_percent <- dk[cumsum(COST_Travel_plus_Survey_rescaled) <= total_budget_50_percent, ]
  WPTs_Random_50_percent <- df_Random_50_percent$WPT
  save(WPTs_Random_50_percent, file=paste("Random_WPTs_50_percent_budget_",full_datasets[k],".Rda",sep=""))
  df_Random_75_percent <- dk[cumsum(COST_Travel_plus_Survey_rescaled) <= total_budget_75_percent, ]
  WPTs_Random_75_percent <- df_Random_75_percent$WPT
  save(WPTs_Random_75_percent, file=paste("Random_WPTs_75_percent_budget_",full_datasets[k],".Rda",sep=""))
  WPTs_Random_100_percent <- df$WPT
  save(WPTs_Random_100_percent, file=paste("Random_WPTs_100_percent_budget_",full_datasets[k],".Rda",sep=""))
}
