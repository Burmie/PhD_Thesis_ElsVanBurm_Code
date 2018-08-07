### Here we calculate the cost to travel to and survey each site 
### Both survey and management cost are the same for each year 
### Based on the survey cost and a budget that needs to be divided between survey and management, management budget is calculated
### Management Budget = Total Budget - Survey Cost -> should be done for different total budgets
### We set total budget so that management budget equals 1,000; 2,000; 3,000 and 4,000 when 1% of sites was monitored 

# Total budget in survey minutes
total_budget <- 6045

# Declare vectors to store monitoring cost of different fractions of the island 
CIdata2001_SURVEY_COST <- vector()
CIdata2003_SURVEY_COST <- vector()
CIdata2005_SURVEY_COST <- vector()
CIdata2007_SURVEY_COST <- vector()
CIdata2009_SURVEY_COST <- vector()
CIdata2011_SURVEY_COST <- vector()
CIdata2013_SURVEY_COST <- vector()
CIdata2015_SURVEY_COST <- vector()

CIdata2001_MANAGEMENT_BUDGET <- vector()
CIdata2003_MANAGEMENT_BUDGET <- vector()
CIdata2005_MANAGEMENT_BUDGET <- vector()
CIdata2007_MANAGEMENT_BUDGET <- vector()
CIdata2009_MANAGEMENT_BUDGET <- vector()
CIdata2011_MANAGEMENT_BUDGET <- vector()
CIdata2013_MANAGEMENT_BUDGET <- vector()
CIdata2015_MANAGEMENT_BUDGET <- vector()

# Calculate monitoring cost for each fraction of the island  
for (i in 1:100){
  CIdata2001_SURVEY_COST[i] <- sum(CIdata2001_list[[i]]$COST_Travel_plus_Survey_rescaled)
  CIdata2003_SURVEY_COST[i] <- sum(CIdata2003_list[[i]]$COST_Travel_plus_Survey_rescaled)
  CIdata2005_SURVEY_COST[i] <- sum(CIdata2005_list[[i]]$COST_Travel_plus_Survey_rescaled)
  CIdata2007_SURVEY_COST[i] <- sum(CIdata2007_list[[i]]$COST_Travel_plus_Survey_rescaled)
  CIdata2009_SURVEY_COST[i] <- sum(CIdata2009_list[[i]]$COST_Travel_plus_Survey_rescaled)
  CIdata2011_SURVEY_COST[i] <- sum(CIdata2011_list[[i]]$COST_Travel_plus_Survey_rescaled)
  CIdata2013_SURVEY_COST[i] <- sum(CIdata2013_list[[i]]$COST_Travel_plus_Survey_rescaled)
  CIdata2015_SURVEY_COST[i] <- sum(CIdata2015_list[[i]]$COST_Travel_plus_Survey_rescaled)
}

# Calculate management budget, assuming a total budget in survey minutes
for (i in 1:100){
  CIdata2001_MANAGEMENT_BUDGET[i] <- total_budget - CIdata2001_SURVEY_COST[i]
  CIdata2003_MANAGEMENT_BUDGET[i] <- total_budget - CIdata2003_SURVEY_COST[i]
  CIdata2005_MANAGEMENT_BUDGET[i] <- total_budget - CIdata2005_SURVEY_COST[i]
  CIdata2007_MANAGEMENT_BUDGET[i] <- total_budget - CIdata2007_SURVEY_COST[i]
  CIdata2009_MANAGEMENT_BUDGET[i] <- total_budget - CIdata2009_SURVEY_COST[i]
  CIdata2011_MANAGEMENT_BUDGET[i] <- total_budget - CIdata2011_SURVEY_COST[i]
  CIdata2013_MANAGEMENT_BUDGET[i] <- total_budget - CIdata2013_SURVEY_COST[i]
  CIdata2015_MANAGEMENT_BUDGET[i] <- total_budget - CIdata2015_SURVEY_COST[i]
}






################################################# Archive ######################################################################

# Calculate survey and travel cost for each data collection method (survey cost)
CIdata2001_1_percent_SURVEY_COST <- sum(CIdata2001_1_percent$COST_Travel_plus_Survey_rescaled)
CIdata2003_1_percent_SURVEY_COST <- sum(CIdata2003_1_percent$COST_Travel_plus_Survey_rescaled)
CIdata2005_1_percent_SURVEY_COST <- sum(CIdata2005_1_percent$COST_Travel_plus_Survey_rescaled)                             
CIdata2007_1_percent_SURVEY_COST <- sum(CIdata2007_1_percent$COST_Travel_plus_Survey_rescaled)
CIdata2009_1_percent_SURVEY_COST <- sum(CIdata2009_1_percent$COST_Travel_plus_Survey_rescaled)
CIdata2011_1_percent_SURVEY_COST <- sum(CIdata2011_1_percent$COST_Travel_plus_Survey_rescaled)
CIdata2013_1_percent_SURVEY_COST <- sum(CIdata2013_1_percent$COST_Travel_plus_Survey_rescaled)
CIdata2015_1_percent_SURVEY_COST <- sum(CIdata2015_1_percent$COST_Travel_plus_Survey_rescaled)

CIdata2001_5_percent_SURVEY_COST <- sum(CIdata2001_5_percent$COST_Travel_plus_Survey_rescaled)
CIdata2003_5_percent_SURVEY_COST <- sum(CIdata2003_5_percent$COST_Travel_plus_Survey_rescaled)
CIdata2005_5_percent_SURVEY_COST <- sum(CIdata2005_5_percent$COST_Travel_plus_Survey_rescaled)                             
CIdata2007_5_percent_SURVEY_COST <- sum(CIdata2007_5_percent$COST_Travel_plus_Survey_rescaled)
CIdata2009_5_percent_SURVEY_COST <- sum(CIdata2009_5_percent$COST_Travel_plus_Survey_rescaled)
CIdata2011_5_percent_SURVEY_COST <- sum(CIdata2011_5_percent$COST_Travel_plus_Survey_rescaled)
CIdata2013_5_percent_SURVEY_COST <- sum(CIdata2013_5_percent$COST_Travel_plus_Survey_rescaled)
CIdata2015_5_percent_SURVEY_COST <- sum(CIdata2015_5_percent$COST_Travel_plus_Survey_rescaled)

CIdata2001_10_percent_SURVEY_COST <- sum(CIdata2001_10_percent$COST_Travel_plus_Survey_rescaled)
CIdata2003_10_percent_SURVEY_COST <- sum(CIdata2003_10_percent$COST_Travel_plus_Survey_rescaled)
CIdata2005_10_percent_SURVEY_COST <- sum(CIdata2005_10_percent$COST_Travel_plus_Survey_rescaled)                             
CIdata2007_10_percent_SURVEY_COST <- sum(CIdata2007_10_percent$COST_Travel_plus_Survey_rescaled)
CIdata2009_10_percent_SURVEY_COST <- sum(CIdata2009_10_percent$COST_Travel_plus_Survey_rescaled)
CIdata2011_10_percent_SURVEY_COST <- sum(CIdata2011_10_percent$COST_Travel_plus_Survey_rescaled)
CIdata2013_10_percent_SURVEY_COST <- sum(CIdata2013_10_percent$COST_Travel_plus_Survey_rescaled)
CIdata2015_10_percent_SURVEY_COST <- sum(CIdata2015_10_percent$COST_Travel_plus_Survey_rescaled)

CIdata2001_20_percent_SURVEY_COST <- sum(CIdata2001_20_percent$COST_Travel_plus_Survey_rescaled)
CIdata2003_20_percent_SURVEY_COST <- sum(CIdata2003_20_percent$COST_Travel_plus_Survey_rescaled)
CIdata2005_20_percent_SURVEY_COST <- sum(CIdata2005_20_percent$COST_Travel_plus_Survey_rescaled)                             
CIdata2007_20_percent_SURVEY_COST <- sum(CIdata2007_20_percent$COST_Travel_plus_Survey_rescaled)
CIdata2009_20_percent_SURVEY_COST <- sum(CIdata2009_20_percent$COST_Travel_plus_Survey_rescaled)
CIdata2011_20_percent_SURVEY_COST <- sum(CIdata2011_20_percent$COST_Travel_plus_Survey_rescaled)
CIdata2013_20_percent_SURVEY_COST <- sum(CIdata2013_20_percent$COST_Travel_plus_Survey_rescaled)
CIdata2015_20_percent_SURVEY_COST <- sum(CIdata2015_20_percent$COST_Travel_plus_Survey_rescaled)

CIdata2001_30_percent_SURVEY_COST <- sum(CIdata2001_30_percent$COST_Travel_plus_Survey_rescaled)
CIdata2003_30_percent_SURVEY_COST <- sum(CIdata2003_30_percent$COST_Travel_plus_Survey_rescaled)
CIdata2005_30_percent_SURVEY_COST <- sum(CIdata2005_30_percent$COST_Travel_plus_Survey_rescaled)                             
CIdata2007_30_percent_SURVEY_COST <- sum(CIdata2007_30_percent$COST_Travel_plus_Survey_rescaled)
CIdata2009_30_percent_SURVEY_COST <- sum(CIdata2009_30_percent$COST_Travel_plus_Survey_rescaled)
CIdata2011_30_percent_SURVEY_COST <- sum(CIdata2011_30_percent$COST_Travel_plus_Survey_rescaled)
CIdata2013_30_percent_SURVEY_COST <- sum(CIdata2013_30_percent$COST_Travel_plus_Survey_rescaled)
CIdata2015_30_percent_SURVEY_COST <- sum(CIdata2015_30_percent$COST_Travel_plus_Survey_rescaled)

CIdata2001_40_percent_SURVEY_COST <- sum(CIdata2001_40_percent$COST_Travel_plus_Survey_rescaled)
CIdata2003_40_percent_SURVEY_COST <- sum(CIdata2003_40_percent$COST_Travel_plus_Survey_rescaled)
CIdata2005_40_percent_SURVEY_COST <- sum(CIdata2005_40_percent$COST_Travel_plus_Survey_rescaled)                             
CIdata2007_40_percent_SURVEY_COST <- sum(CIdata2007_40_percent$COST_Travel_plus_Survey_rescaled)
CIdata2009_40_percent_SURVEY_COST <- sum(CIdata2009_40_percent$COST_Travel_plus_Survey_rescaled)
CIdata2011_40_percent_SURVEY_COST <- sum(CIdata2011_40_percent$COST_Travel_plus_Survey_rescaled)
CIdata2013_40_percent_SURVEY_COST <- sum(CIdata2013_40_percent$COST_Travel_plus_Survey_rescaled)
CIdata2015_40_percent_SURVEY_COST <- sum(CIdata2015_40_percent$COST_Travel_plus_Survey_rescaled)

CIdata2001_50_percent_SURVEY_COST <- sum(CIdata2001_50_percent$COST_Travel_plus_Survey_rescaled)
CIdata2003_50_percent_SURVEY_COST <- sum(CIdata2003_50_percent$COST_Travel_plus_Survey_rescaled)
CIdata2005_50_percent_SURVEY_COST <- sum(CIdata2005_50_percent$COST_Travel_plus_Survey_rescaled)                             
CIdata2007_50_percent_SURVEY_COST <- sum(CIdata2007_50_percent$COST_Travel_plus_Survey_rescaled)
CIdata2009_50_percent_SURVEY_COST <- sum(CIdata2009_50_percent$COST_Travel_plus_Survey_rescaled)
CIdata2011_50_percent_SURVEY_COST <- sum(CIdata2011_50_percent$COST_Travel_plus_Survey_rescaled)
CIdata2013_50_percent_SURVEY_COST <- sum(CIdata2013_50_percent$COST_Travel_plus_Survey_rescaled)
CIdata2015_50_percent_SURVEY_COST <- sum(CIdata2015_50_percent$COST_Travel_plus_Survey_rescaled)

CIdata2001_60_percent_SURVEY_COST <- sum(CIdata2001_60_percent$COST_Travel_plus_Survey_rescaled)
CIdata2003_60_percent_SURVEY_COST <- sum(CIdata2003_60_percent$COST_Travel_plus_Survey_rescaled)
CIdata2005_60_percent_SURVEY_COST <- sum(CIdata2005_60_percent$COST_Travel_plus_Survey_rescaled)                             
CIdata2007_60_percent_SURVEY_COST <- sum(CIdata2007_60_percent$COST_Travel_plus_Survey_rescaled)
CIdata2009_60_percent_SURVEY_COST <- sum(CIdata2009_60_percent$COST_Travel_plus_Survey_rescaled)
CIdata2011_60_percent_SURVEY_COST <- sum(CIdata2011_60_percent$COST_Travel_plus_Survey_rescaled)
CIdata2013_60_percent_SURVEY_COST <- sum(CIdata2013_60_percent$COST_Travel_plus_Survey_rescaled)
CIdata2015_60_percent_SURVEY_COST <- sum(CIdata2015_60_percent$COST_Travel_plus_Survey_rescaled)

CIdata2001_70_percent_SURVEY_COST <- sum(CIdata2001_70_percent$COST_Travel_plus_Survey_rescaled)
CIdata2003_70_percent_SURVEY_COST <- sum(CIdata2003_70_percent$COST_Travel_plus_Survey_rescaled)
CIdata2005_70_percent_SURVEY_COST <- sum(CIdata2005_70_percent$COST_Travel_plus_Survey_rescaled)                             
CIdata2007_70_percent_SURVEY_COST <- sum(CIdata2007_70_percent$COST_Travel_plus_Survey_rescaled)
CIdata2009_70_percent_SURVEY_COST <- sum(CIdata2009_70_percent$COST_Travel_plus_Survey_rescaled)
CIdata2011_70_percent_SURVEY_COST <- sum(CIdata2011_70_percent$COST_Travel_plus_Survey_rescaled)
CIdata2013_70_percent_SURVEY_COST <- sum(CIdata2013_70_percent$COST_Travel_plus_Survey_rescaled)
CIdata2015_70_percent_SURVEY_COST <- sum(CIdata2015_70_percent$COST_Travel_plus_Survey_rescaled)

CIdata2001_80_percent_SURVEY_COST <- sum(CIdata2001_80_percent$COST_Travel_plus_Survey_rescaled)
CIdata2003_80_percent_SURVEY_COST <- sum(CIdata2003_80_percent$COST_Travel_plus_Survey_rescaled)
CIdata2005_80_percent_SURVEY_COST <- sum(CIdata2005_80_percent$COST_Travel_plus_Survey_rescaled)                             
CIdata2007_80_percent_SURVEY_COST <- sum(CIdata2007_80_percent$COST_Travel_plus_Survey_rescaled)
CIdata2009_80_percent_SURVEY_COST <- sum(CIdata2009_80_percent$COST_Travel_plus_Survey_rescaled)
CIdata2011_80_percent_SURVEY_COST <- sum(CIdata2011_80_percent$COST_Travel_plus_Survey_rescaled)
CIdata2013_80_percent_SURVEY_COST <- sum(CIdata2013_80_percent$COST_Travel_plus_Survey_rescaled)
CIdata2015_80_percent_SURVEY_COST <- sum(CIdata2015_80_percent$COST_Travel_plus_Survey_rescaled)

CIdata2001_90_percent_SURVEY_COST <- sum(CIdata2001_90_percent$COST_Travel_plus_Survey_rescaled)
CIdata2003_90_percent_SURVEY_COST <- sum(CIdata2003_90_percent$COST_Travel_plus_Survey_rescaled)
CIdata2005_90_percent_SURVEY_COST <- sum(CIdata2005_90_percent$COST_Travel_plus_Survey_rescaled)                             
CIdata2007_90_percent_SURVEY_COST <- sum(CIdata2007_90_percent$COST_Travel_plus_Survey_rescaled)
CIdata2009_90_percent_SURVEY_COST <- sum(CIdata2009_90_percent$COST_Travel_plus_Survey_rescaled)
CIdata2011_90_percent_SURVEY_COST <- sum(CIdata2011_90_percent$COST_Travel_plus_Survey_rescaled)
CIdata2013_90_percent_SURVEY_COST <- sum(CIdata2013_90_percent$COST_Travel_plus_Survey_rescaled)
CIdata2015_90_percent_SURVEY_COST <- sum(CIdata2015_90_percent$COST_Travel_plus_Survey_rescaled)

CIdata2001_100_percent_SURVEY_COST <- sum(CIdata2001_100_percent$COST_Travel_plus_Survey_rescaled)
CIdata2003_100_percent_SURVEY_COST <- sum(CIdata2003_100_percent$COST_Travel_plus_Survey_rescaled)
CIdata2005_100_percent_SURVEY_COST <- sum(CIdata2005_100_percent$COST_Travel_plus_Survey_rescaled)                             
CIdata2007_100_percent_SURVEY_COST <- sum(CIdata2007_100_percent$COST_Travel_plus_Survey_rescaled)
CIdata2009_100_percent_SURVEY_COST <- sum(CIdata2009_100_percent$COST_Travel_plus_Survey_rescaled)
CIdata2011_100_percent_SURVEY_COST <- sum(CIdata2011_100_percent$COST_Travel_plus_Survey_rescaled)
CIdata2013_100_percent_SURVEY_COST <- sum(CIdata2013_100_percent$COST_Travel_plus_Survey_rescaled)
CIdata2015_100_percent_SURVEY_COST <- sum(CIdata2015_100_percent$COST_Travel_plus_Survey_rescaled)

# Calculate management budget, assuming a total budget in survey minutes
CIdata2001_1_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2001_1_percent_SURVEY_COST
CIdata2001_2003_1_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2003_1_percent_SURVEY_COST
CIdata2001_2003_2005_1_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2005_1_percent_SURVEY_COST                              
CIdata2001_2003_2005_2007_1_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2007_1_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_1_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2009_1_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_1_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2011_1_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_2013_1_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2013_1_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_2013_2015_1_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2015_1_percent_SURVEY_COST

CIdata2001_5_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2001_5_percent_SURVEY_COST
CIdata2001_2003_5_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2003_5_percent_SURVEY_COST
CIdata2001_2003_2005_5_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2005_5_percent_SURVEY_COST                              
CIdata2001_2003_2005_2007_5_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2007_5_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_5_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2009_5_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_5_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2011_5_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_2013_5_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2013_5_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_2013_2015_5_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2015_5_percent_SURVEY_COST

CIdata2001_10_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2001_10_percent_SURVEY_COST
CIdata2001_2003_10_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2003_10_percent_SURVEY_COST
CIdata2001_2003_2005_10_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2005_10_percent_SURVEY_COST                              
CIdata2001_2003_2005_2007_10_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2007_10_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_10_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2009_10_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_10_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2011_10_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_2013_10_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2013_10_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_2013_2015_10_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2015_10_percent_SURVEY_COST

CIdata2001_20_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2001_20_percent_SURVEY_COST
CIdata2001_2003_20_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2001_20_percent_SURVEY_COST
CIdata2001_2003_2005_20_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2001_20_percent_SURVEY_COST                            
CIdata2001_2003_2005_2007_20_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2001_20_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_20_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2001_20_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_20_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2001_20_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_2013_20_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2001_20_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_2013_2015_20_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2001_20_percent_SURVEY_COST

CIdata2001_30_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2001_30_percent_SURVEY_COST
CIdata2001_2003_30_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2003_30_percent_SURVEY_COST
CIdata2001_2003_2005_30_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2005_30_percent_SURVEY_COST                             
CIdata2001_2003_2005_2007_30_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2007_30_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_30_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2009_30_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_30_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2011_30_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_2013_30_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2013_30_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_2013_2015_30_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2015_30_percent_SURVEY_COST

CIdata2001_40_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2001_40_percent_SURVEY_COST
CIdata2001_2003_40_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2003_40_percent_SURVEY_COST
CIdata2001_2003_2005_40_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2005_40_percent_SURVEY_COST                            
CIdata2001_2003_2005_2007_40_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2007_40_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_40_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2009_40_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_40_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2011_40_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_2013_40_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2013_40_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_2013_2015_40_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2015_40_percent_SURVEY_COST

CIdata2001_50_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2001_50_percent_SURVEY_COST
CIdata2001_2003_50_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2003_50_percent_SURVEY_COST
CIdata2001_2003_2005_50_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2005_50_percent_SURVEY_COST                             
CIdata2001_2003_2005_2007_50_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2007_50_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_50_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2009_50_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_50_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2011_50_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_2013_50_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2013_50_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_2013_2015_50_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2015_50_percent_SURVEY_COST

CIdata2001_60_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2001_60_percent_SURVEY_COST
CIdata2001_2003_60_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2003_60_percent_SURVEY_COST 
CIdata2001_2003_2005_60_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2005_60_percent_SURVEY_COST                             
CIdata2001_2003_2005_2007_60_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2007_60_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_60_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2009_60_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_60_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2011_60_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_2013_60_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2013_60_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_2013_2015_60_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2015_60_percent_SURVEY_COST

CIdata2001_70_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2001_70_percent_SURVEY_COST
CIdata2001_2003_70_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2003_70_percent_SURVEY_COST
CIdata2001_2003_2005_70_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2005_70_percent_SURVEY_COST                             
CIdata2001_2003_2005_2007_70_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2007_70_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_70_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2009_70_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_70_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2011_70_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_2013_70_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2013_70_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_2013_2015_70_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2015_70_percent_SURVEY_COST

CIdata2001_80_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2001_80_percent_SURVEY_COST
CIdata2001_2003_80_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2003_80_percent_SURVEY_COST
CIdata2001_2003_2005_80_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2005_80_percent_SURVEY_COST                             
CIdata2001_2003_2005_2007_80_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2007_80_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_80_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2009_80_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_80_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2011_80_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_2013_80_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2013_80_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_2013_2015_80_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2015_80_percent_SURVEY_COST

CIdata2001_90_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2001_90_percent_SURVEY_COST
CIdata2001_2003_90_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2003_90_percent_SURVEY_COST
CIdata2001_2003_2005_90_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2005_90_percent_SURVEY_COST                             
CIdata2001_2003_2005_2007_90_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2007_90_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_90_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2009_90_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_90_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2011_90_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_2013_90_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2013_90_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_2013_2015_90_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2015_90_percent_SURVEY_COST

CIdata2001_100_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2001_100_percent_SURVEY_COST
CIdata2001_2003_100_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2003_100_percent_SURVEY_COST
CIdata2001_2003_2005_100_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2005_100_percent_SURVEY_COST                             
CIdata2001_2003_2005_2007_100_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2007_100_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_100_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2009_100_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_100_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2011_100_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_2013_100_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2013_100_percent_SURVEY_COST
CIdata2001_2003_2005_2007_2009_2011_2013_2015_100_percent_MANAGEMENT_BUDGET <- 7000 - CIdata2015_100_percent_SURVEY_COST


