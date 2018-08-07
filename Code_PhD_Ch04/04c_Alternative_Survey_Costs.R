### Here we calculate the cost to travel to and survey each site 
### Both survey and management cost are the same for each year 
### Based on the survey cost and a budget that needs to be divided between survey and management, management budget is calculated
### Management Budget = Total Budget - Survey Cost -> should be done for different total budgets
### We set total budget so that management budget equals 1,000; 2,000; 3,000 and 4,000 when 1% of sites was monitored 

# Total budget in survey minutes
total_budget <- 6000

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

## MONITORING COST 
# # Calculate monitoring cost for each fraction of the island - actual monitoring cost 
# for (i in 1:100){
#   CIdata2001_SURVEY_COST[i] <- sum(CIdata2001_list[[i]]$COST_Travel_plus_Survey)
#   CIdata2003_SURVEY_COST[i] <- sum(CIdata2003_list[[i]]$COST_Travel_plus_Survey)
#   CIdata2005_SURVEY_COST[i] <- sum(CIdata2005_list[[i]]$COST_Travel_plus_Survey)
#   CIdata2007_SURVEY_COST[i] <- sum(CIdata2007_list[[i]]$COST_Travel_plus_Survey)
#   CIdata2009_SURVEY_COST[i] <- sum(CIdata2009_list[[i]]$COST_Travel_plus_Survey)
#   CIdata2011_SURVEY_COST[i] <- sum(CIdata2011_list[[i]]$COST_Travel_plus_Survey)
#   CIdata2013_SURVEY_COST[i] <- sum(CIdata2013_list[[i]]$COST_Travel_plus_Survey)
#   CIdata2015_SURVEY_COST[i] <- sum(CIdata2015_list[[i]]$COST_Travel_plus_Survey)
# }

# # Calculate monitoring cost for each fraction of the island - rescaled monitoring cost
# for (i in 1:100){
#   CIdata2001_SURVEY_COST[i] <- sum(CIdata2001_list[[i]]$COST_Travel_plus_Survey_rescaled)
#   CIdata2003_SURVEY_COST[i] <- sum(CIdata2003_list[[i]]$COST_Travel_plus_Survey_rescaled)
#   CIdata2005_SURVEY_COST[i] <- sum(CIdata2005_list[[i]]$COST_Travel_plus_Survey_rescaled)
#   CIdata2007_SURVEY_COST[i] <- sum(CIdata2007_list[[i]]$COST_Travel_plus_Survey_rescaled)
#   CIdata2009_SURVEY_COST[i] <- sum(CIdata2009_list[[i]]$COST_Travel_plus_Survey_rescaled)
#   CIdata2011_SURVEY_COST[i] <- sum(CIdata2011_list[[i]]$COST_Travel_plus_Survey_rescaled)
#   CIdata2013_SURVEY_COST[i] <- sum(CIdata2013_list[[i]]$COST_Travel_plus_Survey_rescaled)
#   CIdata2015_SURVEY_COST[i] <- sum(CIdata2015_list[[i]]$COST_Travel_plus_Survey_rescaled)
# }

# Calculate monitoring cost for each fraction of the island - intermediate monitoring cost: between rescaled and zero
for (i in 1:100){
  CIdata2001_SURVEY_COST[i] <- sum(CIdata2001_list[[i]]$COST_Travel_plus_Survey_rescaled-2)
  CIdata2003_SURVEY_COST[i] <- sum(CIdata2003_list[[i]]$COST_Travel_plus_Survey_rescaled-2)
  CIdata2005_SURVEY_COST[i] <- sum(CIdata2005_list[[i]]$COST_Travel_plus_Survey_rescaled-2)
  CIdata2007_SURVEY_COST[i] <- sum(CIdata2007_list[[i]]$COST_Travel_plus_Survey_rescaled-2)
  CIdata2009_SURVEY_COST[i] <- sum(CIdata2009_list[[i]]$COST_Travel_plus_Survey_rescaled-2)
  CIdata2011_SURVEY_COST[i] <- sum(CIdata2011_list[[i]]$COST_Travel_plus_Survey_rescaled-2)
  CIdata2013_SURVEY_COST[i] <- sum(CIdata2013_list[[i]]$COST_Travel_plus_Survey_rescaled-2)
  CIdata2015_SURVEY_COST[i] <- sum(CIdata2015_list[[i]]$COST_Travel_plus_Survey_rescaled-2)
}

# # Calculate monitoring cost for each fraction of the island - zero
# for (i in 1:100){
#   CIdata2001_SURVEY_COST[i] <- 0
#   CIdata2003_SURVEY_COST[i] <- 0
#   CIdata2005_SURVEY_COST[i] <- 0
#   CIdata2007_SURVEY_COST[i] <- 0
#   CIdata2009_SURVEY_COST[i] <- 0
#   CIdata2011_SURVEY_COST[i] <- 0
#   CIdata2013_SURVEY_COST[i] <- 0
#   CIdata2015_SURVEY_COST[i] <- 0
# }


## MANAGEMENT COST 
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


