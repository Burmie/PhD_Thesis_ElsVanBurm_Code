setwd("D:\\Users\\evanburm\\Dropbox\\Christmas Island\\Ch03_Cost_effective_survey_methods_for_invasive_species\\WPTs_REAL_COST")

# Define some vectors 
full_datasets <- c("CIdata2001","CIdata2003", "CIdata2005","CIdata2007","CIdata2009", "CIdata2011", "CIdata2013","CIdata2015")
percent_of_budget <- c("25", "50", "75")
survey_strategies <- c("Knapsack","R_vs_C","Pocc","ExpOp","Syst","Random")


## Evaluation matrix
# 2001
Evaluation_survey_method_2001 <- as.data.frame(matrix(NA, nrow = nrow(CIdata2001), ncol = 4)) # create data.frame 
Evaluation_survey_method_2001[ , 1] <- CIdata2001[ , "WPT"]
Evaluation_survey_method_2001[ , 2] <- CIdata2001[ , "X_MARK"]
Evaluation_survey_method_2001[ , 3] <- CIdata2001[ , "Y_MARK"]
Evaluation_survey_method_2001[ , 4] <- CIdata2001[ , "p_a_SC"]
colnames(Evaluation_survey_method_2001) <- c("WPT", "X_MARK","Y_MARK","SC_2001")
Evaluation_survey_method_2001
# 2003
Evaluation_survey_method_2003 <- as.data.frame(matrix(NA, nrow = nrow(CIdata2003), ncol = 4)) # create data.frame 
Evaluation_survey_method_2003[ , 1] <- CIdata2003[ , "WPT"]
Evaluation_survey_method_2003[ , 2] <- CIdata2003[ , "X_MARK"]
Evaluation_survey_method_2003[ , 3] <- CIdata2003[ , "Y_MARK"]
Evaluation_survey_method_2003[ , 4] <- CIdata2003[ , "p_a_SC"]
colnames(Evaluation_survey_method_2003) <- c("WPT", "X_MARK","Y_MARK","SC_2003")
Evaluation_survey_method_2003
# 2005
Evaluation_survey_method_2005 <- as.data.frame(matrix(NA, nrow = nrow(CIdata2005), ncol = 4)) # create data.frame 
Evaluation_survey_method_2005[ , 1] <- CIdata2005[ , "WPT"]
Evaluation_survey_method_2005[ , 2] <- CIdata2005[ , "X_MARK"]
Evaluation_survey_method_2005[ , 3] <- CIdata2005[ , "Y_MARK"]
Evaluation_survey_method_2005[ , 4] <- CIdata2005[ , "p_a_SC"]
colnames(Evaluation_survey_method_2005) <- c("WPT", "X_MARK","Y_MARK","SC_2005")
Evaluation_survey_method_2005
# 2007
Evaluation_survey_method_2007 <- as.data.frame(matrix(NA, nrow = nrow(CIdata2007), ncol = 4)) # create data.frame 
Evaluation_survey_method_2007[ , 1] <- CIdata2007[ , "WPT"]
Evaluation_survey_method_2007[ , 2] <- CIdata2007[ , "X_MARK"]
Evaluation_survey_method_2007[ , 3] <- CIdata2007[ , "Y_MARK"]
Evaluation_survey_method_2007[ , 4] <- CIdata2007[ , "p_a_SC"]
colnames(Evaluation_survey_method_2007) <- c("WPT", "X_MARK","Y_MARK","SC_2007")
Evaluation_survey_method_2007
# 2009
Evaluation_survey_method_2009 <- as.data.frame(matrix(NA, nrow = nrow(CIdata2009), ncol = 4)) # create data.frame 
Evaluation_survey_method_2009[ , 1] <- CIdata2009[ , "WPT"]
Evaluation_survey_method_2009[ , 2] <- CIdata2009[ , "X_MARK"]
Evaluation_survey_method_2009[ , 3] <- CIdata2009[ , "Y_MARK"]
Evaluation_survey_method_2009[ , 4] <- CIdata2009[ , "p_a_SC"]
colnames(Evaluation_survey_method_2009) <- c("WPT", "X_MARK","Y_MARK","SC_2009")
Evaluation_survey_method_2009
# 2011
Evaluation_survey_method_2011 <- as.data.frame(matrix(NA, nrow = nrow(CIdata2011), ncol = 4)) # create data.frame 
Evaluation_survey_method_2011[ , 1] <- CIdata2011[ , "WPT"]
Evaluation_survey_method_2011[ , 2] <- CIdata2011[ , "X_MARK"]
Evaluation_survey_method_2011[ , 3] <- CIdata2011[ , "Y_MARK"]
Evaluation_survey_method_2011[ , 4] <- CIdata2011[ , "p_a_SC"]
colnames(Evaluation_survey_method_2011) <- c("WPT", "X_MARK","Y_MARK","SC_2011")
Evaluation_survey_method_2011
# 2013
Evaluation_survey_method_2013 <- as.data.frame(matrix(NA, nrow = nrow(CIdata2013), ncol = 4)) # create data.frame 
Evaluation_survey_method_2013[ , 1] <- CIdata2013[ , "WPT"]
Evaluation_survey_method_2013[ , 2] <- CIdata2013[ , "X_MARK"]
Evaluation_survey_method_2013[ , 3] <- CIdata2013[ , "Y_MARK"]
Evaluation_survey_method_2013[ , 4] <- CIdata2013[ , "p_a_SC"]
colnames(Evaluation_survey_method_2013) <- c("WPT", "X_MARK","Y_MARK","SC_2013")
Evaluation_survey_method_2013
# 2015
Evaluation_survey_method_2015 <- as.data.frame(matrix(NA, nrow = nrow(CIdata2015), ncol = 4)) # create data.frame 
Evaluation_survey_method_2015[ , 1] <- CIdata2015[ , "WPT"]
Evaluation_survey_method_2015[ , 2] <- CIdata2015[ , "X_MARK"]
Evaluation_survey_method_2015[ , 3] <- CIdata2015[ , "Y_MARK"]
Evaluation_survey_method_2015[ , 4] <- CIdata2015[ , "p_a_SC"]
colnames(Evaluation_survey_method_2015) <- c("WPT", "X_MARK","Y_MARK","SC_2015")
Evaluation_survey_method_2015
# Create dataframe to store survey performance results for 25%
SC_detected_25_percent <- as.data.frame(matrix(NA, nrow=length(full_datasets), ncol=length(survey_strategies))) # Perforamnce evaluation across survey strategies for 25% of budget
rownames(SC_detected_25_percent) <- full_datasets
colnames(SC_detected_25_percent) <- survey_strategies
# Create dataframe to store survey performance results for 50%
SC_detected_50_percent <- as.data.frame(matrix(NA, nrow=length(full_datasets), ncol=length(survey_strategies))) # Perforamnce evaluation across survey strategies for 25% of budget
rownames(SC_detected_50_percent) <- full_datasets
colnames(SC_detected_50_percent) <- survey_strategies
# Create dataframe to store survey performance results for 75%
SC_detected_75_percent <- as.data.frame(matrix(NA, nrow=length(full_datasets), ncol=length(survey_strategies))) # Perforamnce evaluation across survey strategies for 25% of budget
rownames(SC_detected_75_percent) <- full_datasets
colnames(SC_detected_75_percent) <- survey_strategies
# Create dataframe to store survey performance results for 100%
SC_detected_100_percent <- as.data.frame(matrix(NA, nrow=length(full_datasets), ncol=length(survey_strategies))) # Perforamnce evaluation across survey strategies for 25% of budget
rownames(SC_detected_100_percent) <- full_datasets
colnames(SC_detected_100_percent) <- survey_strategies


#### 25%
### 2001
## Knapsack 
a <- load("WPTs_for_KNAPSACK_CIdata2001_25_percent_budget.Rda")
WPTs_25_percent_KNAPSACK <- sort(WPTs_25_percent_KNAPSACK)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2001$WPT)) {
  for (j in 1:length(WPTs_25_percent_KNAPSACK)){
    if (Evaluation_survey_method_2001$WPT[i] == WPTs_25_percent_KNAPSACK[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2001$SC_2001[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2001$SC_2001))
SC_detected_KNAPSACK_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_25_percent
SC_detected_25_percent$Knapsack[1] <- SC_detected_KNAPSACK_25_percent

## R_vs_C 
a <- load("R_vs_C_WPTs_25_percent_budget_CIdata2001.Rda")
WPTs_R_vs_C_survey_method_25_percent <- WPTs_R_vs_C_25_percent
WPTs_R_vs_C_survey_method_25_percent <- sort(WPTs_R_vs_C_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2001$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_25_percent)){
    if (Evaluation_survey_method_2001$WPT[i] == WPTs_R_vs_C_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2001$SC_2001[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2001$SC_2001))
SC_detected_R_vs_C_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_25_percent
SC_detected_25_percent$R_vs_C[1] <- SC_detected_R_vs_C_survey_25_percent

## POcc
a <- load("Pocc_WPTs_25_percent_budget_CIdata2001.Rda")
WPTs_POCC_survey_method_25_percent <- WPTs_Pocc_25_percent
WPTs_POCC_survey_method_25_percent <- sort(WPTs_POCC_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2001$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_25_percent)){
    if (Evaluation_survey_method_2001$WPT[i] == WPTs_POCC_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2001$SC_2001[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2001$SC_2001))
SC_detected_POCC_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_25_percent
SC_detected_25_percent$Pocc[1] <- SC_detected_POCC_survey_25_percent

## ExpOp
a <- load("ExpOp_WPTs_25_percent_budget_CIdata2001.Rda")
WPTs_EXPOP_survey_method_25_percent <- WPTs_ExpOp_25_percent
WPTs_EXPOP_survey_method_25_percent <- sort(WPTs_EXPOP_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2001$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_25_percent)){
    if (Evaluation_survey_method_2001$WPT[i] == WPTs_EXPOP_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2001$SC_2001[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2001$SC_2001))
SC_detected_EXPOP_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_25_percent
SC_detected_25_percent$ExpOp[1] <- SC_detected_EXPOP_survey_25_percent

## Syst
a <- load("Syst_WPTs_25_percent_budget_CIdata2001.Rda")
WPTs_Syst_survey_method_25_percent <- WPTs_Syst_25_percent
WPTs_Syst_survey_method_25_percent <- sort(WPTs_Syst_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2001$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_25_percent)){
    if (Evaluation_survey_method_2001$WPT[i] == WPTs_Syst_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2001$SC_2001[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2001$SC_2001))
SC_detected_Syst_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_25_percent
SC_detected_25_percent$Syst[1] <- SC_detected_Syst_survey_25_percent

## Random
a <- load("Random_WPTs_25_percent_budget_CIdata2001.Rda")
WPTs_Random_survey_method_25_percent <- WPTs_Random_25_percent
WPTs_Random_survey_method_25_percent <- sort(WPTs_Random_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2001$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_25_percent)){
    if (Evaluation_survey_method_2001$WPT[i] == WPTs_Random_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2001$SC_2001[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2001$SC_2001))
SC_detected_Random_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_25_percent
SC_detected_25_percent$Random[1] <- SC_detected_Random_survey_25_percent

### 2003
## Knapsack 
a <- load("WPTs_for_KNAPSACK_CIdata2003_25_percent_budget.Rda")
WPTs_25_percent_KNAPSACK <- sort(WPTs_25_percent_KNAPSACK)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2003$WPT)) {
  for (j in 1:length(WPTs_25_percent_KNAPSACK)){
    if (Evaluation_survey_method_2003$WPT[i] == WPTs_25_percent_KNAPSACK[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2003$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2003$SC_2003))
SC_detected_KNAPSACK_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_25_percent
SC_detected_25_percent$Knapsack[2] <- SC_detected_KNAPSACK_25_percent

## R_vs_C 
a <- load("R_vs_C_WPTs_25_percent_budget_CIdata2003.Rda")
WPTs_R_vs_C_survey_method_25_percent <- WPTs_R_vs_C_25_percent
WPTs_R_vs_C_survey_method_25_percent <- sort(WPTs_R_vs_C_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2003$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_25_percent)){
    if (Evaluation_survey_method_2003$WPT[i] == WPTs_R_vs_C_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2003$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2003$SC_2003))
SC_detected_R_vs_C_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_25_percent
SC_detected_25_percent$R_vs_C[2] <- SC_detected_R_vs_C_survey_25_percent

## POcc
a <- load("Pocc_WPTs_25_percent_budget_CIdata2003.Rda")
WPTs_POCC_survey_method_25_percent <- WPTs_Pocc_25_percent
WPTs_POCC_survey_method_25_percent <- sort(WPTs_POCC_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2003$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_25_percent)){
    if (Evaluation_survey_method_2003$WPT[i] == WPTs_POCC_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2003$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2003$SC_2003))
SC_detected_POCC_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_25_percent
SC_detected_25_percent$Pocc[2] <- SC_detected_POCC_survey_25_percent

## ExpOp
a <- load("ExpOp_WPTs_25_percent_budget_CIdata2003.Rda")
WPTs_EXPOP_survey_method_25_percent <- WPTs_ExpOp_25_percent
WPTs_EXPOP_survey_method_25_percent <- sort(WPTs_EXPOP_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2003$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_25_percent)){
    if (Evaluation_survey_method_2003$WPT[i] == WPTs_EXPOP_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2003$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2003$SC_2003))
SC_detected_EXPOP_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_25_percent
SC_detected_25_percent$ExpOp[2] <- SC_detected_EXPOP_survey_25_percent

## Syst
a <- load("Syst_WPTs_25_percent_budget_CIdata2003.Rda")
WPTs_Syst_survey_method_25_percent <- WPTs_Syst_25_percent
WPTs_Syst_survey_method_25_percent <- sort(WPTs_Syst_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2003$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_25_percent)){
    if (Evaluation_survey_method_2003$WPT[i] == WPTs_Syst_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2003$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2003$SC_2003))
SC_detected_Syst_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_25_percent
SC_detected_25_percent$Syst[2] <- SC_detected_Syst_survey_25_percent

## Random
a <- load("Random_WPTs_25_percent_budget_CIdata2003.Rda")
WPTs_Random_survey_method_25_percent <- WPTs_Random_25_percent
WPTs_Random_survey_method_25_percent <- sort(WPTs_Random_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2003$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_25_percent)){
    if (Evaluation_survey_method_2003$WPT[i] == WPTs_Random_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2003$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2003$SC_2003))
SC_detected_Random_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_25_percent
SC_detected_25_percent$Random[2] <- SC_detected_Random_survey_25_percent

### 2005
## Knapsack 
a <- load("WPTs_for_KNAPSACK_CIdata2005_25_percent_budget.Rda")
WPTs_25_percent_KNAPSACK <- sort(WPTs_25_percent_KNAPSACK)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2005$WPT)) {
  for (j in 1:length(WPTs_25_percent_KNAPSACK)){
    if (Evaluation_survey_method_2005$WPT[i] == WPTs_25_percent_KNAPSACK[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2005$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2005$SC_2005))
SC_detected_KNAPSACK_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_25_percent
SC_detected_25_percent$Knapsack[3] <- SC_detected_KNAPSACK_25_percent

## R_vs_C 
a <- load("R_vs_C_WPTs_25_percent_budget_CIdata2005.Rda")
WPTs_R_vs_C_survey_method_25_percent <- WPTs_R_vs_C_25_percent
WPTs_R_vs_C_survey_method_25_percent <- sort(WPTs_R_vs_C_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2005$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_25_percent)){
    if (Evaluation_survey_method_2005$WPT[i] == WPTs_R_vs_C_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2005$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2005$SC_2005))
SC_detected_R_vs_C_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_25_percent
SC_detected_25_percent$R_vs_C[3] <- SC_detected_R_vs_C_survey_25_percent

## POcc
a <- load("Pocc_WPTs_25_percent_budget_CIdata2005.Rda")
WPTs_POCC_survey_method_25_percent <- WPTs_Pocc_25_percent
WPTs_POCC_survey_method_25_percent <- sort(WPTs_POCC_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2005$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_25_percent)){
    if (Evaluation_survey_method_2005$WPT[i] == WPTs_POCC_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2005$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2005$SC_2005))
SC_detected_POCC_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_25_percent
SC_detected_25_percent$Pocc[3] <- SC_detected_POCC_survey_25_percent

## ExpOp
a <- load("ExpOp_WPTs_25_percent_budget_CIdata2005.Rda")
WPTs_EXPOP_survey_method_25_percent <- WPTs_ExpOp_25_percent
WPTs_EXPOP_survey_method_25_percent <- sort(WPTs_EXPOP_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2005$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_25_percent)){
    if (Evaluation_survey_method_2005$WPT[i] == WPTs_EXPOP_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2005$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2005$SC_2005))
SC_detected_EXPOP_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_25_percent
SC_detected_25_percent$ExpOp[3] <- SC_detected_EXPOP_survey_25_percent

## Syst
a <- load("Syst_WPTs_25_percent_budget_CIdata2005.Rda")
WPTs_Syst_survey_method_25_percent <- WPTs_Syst_25_percent
WPTs_Syst_survey_method_25_percent <- sort(WPTs_Syst_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2005$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_25_percent)){
    if (Evaluation_survey_method_2005$WPT[i] == WPTs_Syst_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2005$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2005$SC_2005))
SC_detected_Syst_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_25_percent
SC_detected_25_percent$Syst[3] <- SC_detected_Syst_survey_25_percent

## Random
a <- load("Random_WPTs_25_percent_budget_CIdata2005.Rda")
WPTs_Random_survey_method_25_percent <- WPTs_Random_25_percent
WPTs_Random_survey_method_25_percent <- sort(WPTs_Random_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2005$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_25_percent)){
    if (Evaluation_survey_method_2005$WPT[i] == WPTs_Random_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2005$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2005$SC_2005))
SC_detected_Random_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_25_percent
SC_detected_25_percent$Random[3] <- SC_detected_Random_survey_25_percent

### 2007
## Knapsack 
a <- load("WPTs_for_KNAPSACK_CIdata2007_25_percent_budget.Rda")
WPTs_25_percent_KNAPSACK <- sort(WPTs_25_percent_KNAPSACK)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2007$WPT)) {
  for (j in 1:length(WPTs_25_percent_KNAPSACK)){
    if (Evaluation_survey_method_2007$WPT[i] == WPTs_25_percent_KNAPSACK[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2007$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2007$SC_2007))
SC_detected_KNAPSACK_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_25_percent
SC_detected_25_percent$Knapsack[4] <- SC_detected_KNAPSACK_25_percent

## R_vs_C 
a <- load("R_vs_C_WPTs_25_percent_budget_CIdata2007.Rda")
WPTs_R_vs_C_survey_method_25_percent <- WPTs_R_vs_C_25_percent
WPTs_R_vs_C_survey_method_25_percent <- sort(WPTs_R_vs_C_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2007$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_25_percent)){
    if (Evaluation_survey_method_2007$WPT[i] == WPTs_R_vs_C_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2007$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2007$SC_2007))
SC_detected_R_vs_C_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_25_percent
SC_detected_25_percent$R_vs_C[4] <- SC_detected_R_vs_C_survey_25_percent

## POcc
a <- load("Pocc_WPTs_25_percent_budget_CIdata2007.Rda")
WPTs_POCC_survey_method_25_percent <- WPTs_Pocc_25_percent
WPTs_POCC_survey_method_25_percent <- sort(WPTs_POCC_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2007$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_25_percent)){
    if (Evaluation_survey_method_2007$WPT[i] == WPTs_POCC_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2007$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2007$SC_2007))
SC_detected_POCC_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_25_percent
SC_detected_25_percent$Pocc[4] <- SC_detected_POCC_survey_25_percent

## ExpOp
a <- load("ExpOp_WPTs_25_percent_budget_CIdata2007.Rda")
WPTs_EXPOP_survey_method_25_percent <- WPTs_ExpOp_25_percent
WPTs_EXPOP_survey_method_25_percent <- sort(WPTs_EXPOP_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2007$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_25_percent)){
    if (Evaluation_survey_method_2007$WPT[i] == WPTs_EXPOP_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2007$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2007$SC_2007))
SC_detected_EXPOP_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_25_percent
SC_detected_25_percent$ExpOp[4] <- SC_detected_EXPOP_survey_25_percent

## Syst
a <- load("Syst_WPTs_25_percent_budget_CIdata2007.Rda")
WPTs_Syst_survey_method_25_percent <- WPTs_Syst_25_percent
WPTs_Syst_survey_method_25_percent <- sort(WPTs_Syst_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2007$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_25_percent)){
    if (Evaluation_survey_method_2007$WPT[i] == WPTs_Syst_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2007$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2007$SC_2007))
SC_detected_Syst_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_25_percent
SC_detected_25_percent$Syst[4] <- SC_detected_Syst_survey_25_percent

## Random
a <- load("Random_WPTs_25_percent_budget_CIdata2007.Rda")
WPTs_Random_survey_method_25_percent <- WPTs_Random_25_percent
WPTs_Random_survey_method_25_percent <- sort(WPTs_Random_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2007$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_25_percent)){
    if (Evaluation_survey_method_2007$WPT[i] == WPTs_Random_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2007$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2007$SC_2007))
SC_detected_Random_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_25_percent
SC_detected_25_percent$Random[4] <- SC_detected_Random_survey_25_percent


### 2009
## Knapsack 
a <- load("WPTs_for_KNAPSACK_CIdata2009_25_percent_budget.Rda")
WPTs_25_percent_KNAPSACK <- sort(WPTs_25_percent_KNAPSACK)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2009$WPT)) {
  for (j in 1:length(WPTs_25_percent_KNAPSACK)){
    if (Evaluation_survey_method_2009$WPT[i] == WPTs_25_percent_KNAPSACK[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2009$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2009$SC_2009))
SC_detected_KNAPSACK_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_25_percent
SC_detected_25_percent$Knapsack[5] <- SC_detected_KNAPSACK_25_percent

## R_vs_C 
a <- load("R_vs_C_WPTs_25_percent_budget_CIdata2009.Rda")
WPTs_R_vs_C_survey_method_25_percent <- WPTs_R_vs_C_25_percent
WPTs_R_vs_C_survey_method_25_percent <- sort(WPTs_R_vs_C_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2009$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_25_percent)){
    if (Evaluation_survey_method_2009$WPT[i] == WPTs_R_vs_C_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2009$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2009$SC_2009))
SC_detected_R_vs_C_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_25_percent
SC_detected_25_percent$R_vs_C[5] <- SC_detected_R_vs_C_survey_25_percent

## POcc
a <- load("Pocc_WPTs_25_percent_budget_CIdata2009.Rda")
WPTs_POCC_survey_method_25_percent <- WPTs_Pocc_25_percent
WPTs_POCC_survey_method_25_percent <- sort(WPTs_POCC_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2009$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_25_percent)){
    if (Evaluation_survey_method_2009$WPT[i] == WPTs_POCC_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2009$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2009$SC_2009))
SC_detected_POCC_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_25_percent
SC_detected_25_percent$Pocc[5] <- SC_detected_POCC_survey_25_percent

## ExpOp
a <- load("ExpOp_WPTs_25_percent_budget_CIdata2009.Rda")
WPTs_EXPOP_survey_method_25_percent <- WPTs_ExpOp_25_percent
WPTs_EXPOP_survey_method_25_percent <- sort(WPTs_EXPOP_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2009$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_25_percent)){
    if (Evaluation_survey_method_2009$WPT[i] == WPTs_EXPOP_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2009$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2009$SC_2009))
SC_detected_EXPOP_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_25_percent
SC_detected_25_percent$ExpOp[5] <- SC_detected_EXPOP_survey_25_percent

## Syst
a <- load("Syst_WPTs_25_percent_budget_CIdata2009.Rda")
WPTs_Syst_survey_method_25_percent <- WPTs_Syst_25_percent
WPTs_Syst_survey_method_25_percent <- sort(WPTs_Syst_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2009$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_25_percent)){
    if (Evaluation_survey_method_2009$WPT[i] == WPTs_Syst_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2009$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2009$SC_2009))
SC_detected_Syst_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_25_percent
SC_detected_25_percent$Syst[5] <- SC_detected_Syst_survey_25_percent

## Random
a <- load("Random_WPTs_25_percent_budget_CIdata2009.Rda")
WPTs_Random_survey_method_25_percent <- WPTs_Random_25_percent
WPTs_Random_survey_method_25_percent <- sort(WPTs_Random_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2009$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_25_percent)){
    if (Evaluation_survey_method_2009$WPT[i] == WPTs_Random_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2009$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2009$SC_2009))
SC_detected_Random_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_25_percent
SC_detected_25_percent$Random[5] <- SC_detected_Random_survey_25_percent


### 2011
## Knapsack 
a <- load("WPTs_for_KNAPSACK_CIdata2011_25_percent_budget.Rda")
WPTs_25_percent_KNAPSACK <- sort(WPTs_25_percent_KNAPSACK)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2011$WPT)) {
  for (j in 1:length(WPTs_25_percent_KNAPSACK)){
    if (Evaluation_survey_method_2011$WPT[i] == WPTs_25_percent_KNAPSACK[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2011$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2011$SC_2011))
SC_detected_KNAPSACK_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_25_percent
SC_detected_25_percent$Knapsack[6] <- SC_detected_KNAPSACK_25_percent

## R_vs_C 
a <- load("R_vs_C_WPTs_25_percent_budget_CIdata2011.Rda")
WPTs_R_vs_C_survey_method_25_percent <- WPTs_R_vs_C_25_percent
WPTs_R_vs_C_survey_method_25_percent <- sort(WPTs_R_vs_C_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2011$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_25_percent)){
    if (Evaluation_survey_method_2011$WPT[i] == WPTs_R_vs_C_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2011$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2011$SC_2011))
SC_detected_R_vs_C_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_25_percent
SC_detected_25_percent$R_vs_C[6] <- SC_detected_R_vs_C_survey_25_percent

## POcc
a <- load("Pocc_WPTs_25_percent_budget_CIdata2011.Rda")
WPTs_POCC_survey_method_25_percent <- WPTs_Pocc_25_percent
WPTs_POCC_survey_method_25_percent <- sort(WPTs_POCC_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2011$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_25_percent)){
    if (Evaluation_survey_method_2011$WPT[i] == WPTs_POCC_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2011$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2011$SC_2011))
SC_detected_POCC_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_25_percent
SC_detected_25_percent$Pocc[6] <- SC_detected_POCC_survey_25_percent

## ExpOp
a <- load("ExpOp_WPTs_25_percent_budget_CIdata2011.Rda")
WPTs_EXPOP_survey_method_25_percent <- WPTs_ExpOp_25_percent
WPTs_EXPOP_survey_method_25_percent <- sort(WPTs_EXPOP_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2011$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_25_percent)){
    if (Evaluation_survey_method_2011$WPT[i] == WPTs_EXPOP_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2011$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2011$SC_2011))
SC_detected_EXPOP_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_25_percent
SC_detected_25_percent$ExpOp[6] <- SC_detected_EXPOP_survey_25_percent

## Syst
a <- load("Syst_WPTs_25_percent_budget_CIdata2011.Rda")
WPTs_Syst_survey_method_25_percent <- WPTs_Syst_25_percent
WPTs_Syst_survey_method_25_percent <- sort(WPTs_Syst_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2011$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_25_percent)){
    if (Evaluation_survey_method_2011$WPT[i] == WPTs_Syst_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2011$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2011$SC_2011))
SC_detected_Syst_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_25_percent
SC_detected_25_percent$Syst[6] <- SC_detected_Syst_survey_25_percent

## Random
a <- load("Random_WPTs_25_percent_budget_CIdata2011.Rda")
WPTs_Random_survey_method_25_percent <- WPTs_Random_25_percent
WPTs_Random_survey_method_25_percent <- sort(WPTs_Random_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2011$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_25_percent)){
    if (Evaluation_survey_method_2011$WPT[i] == WPTs_Random_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2011$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2011$SC_2011))
SC_detected_Random_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_25_percent
SC_detected_25_percent$Random[6] <- SC_detected_Random_survey_25_percent


### 2013
## Knapsack 
a <- load("WPTs_for_KNAPSACK_CIdata2013_25_percent_budget.Rda")
WPTs_25_percent_KNAPSACK <- sort(WPTs_25_percent_KNAPSACK)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2013$WPT)) {
  for (j in 1:length(WPTs_25_percent_KNAPSACK)){
    if (Evaluation_survey_method_2013$WPT[i] == WPTs_25_percent_KNAPSACK[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2013$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2013$SC_2013))
SC_detected_KNAPSACK_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_25_percent
SC_detected_25_percent$Knapsack[7] <- SC_detected_KNAPSACK_25_percent

## R_vs_C 
a <- load("R_vs_C_WPTs_25_percent_budget_CIdata2013.Rda")
WPTs_R_vs_C_survey_method_25_percent <- WPTs_R_vs_C_25_percent
WPTs_R_vs_C_survey_method_25_percent <- sort(WPTs_R_vs_C_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2013$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_25_percent)){
    if (Evaluation_survey_method_2013$WPT[i] == WPTs_R_vs_C_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2013$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2013$SC_2013))
SC_detected_R_vs_C_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_25_percent
SC_detected_25_percent$R_vs_C[7] <- SC_detected_R_vs_C_survey_25_percent

## POcc
a <- load("Pocc_WPTs_25_percent_budget_CIdata2013.Rda")
WPTs_POCC_survey_method_25_percent <- WPTs_Pocc_25_percent
WPTs_POCC_survey_method_25_percent <- sort(WPTs_POCC_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2013$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_25_percent)){
    if (Evaluation_survey_method_2013$WPT[i] == WPTs_POCC_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2013$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2013$SC_2013))
SC_detected_POCC_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_25_percent
SC_detected_25_percent$Pocc[7] <- SC_detected_POCC_survey_25_percent

## ExpOp
a <- load("ExpOp_WPTs_25_percent_budget_CIdata2013.Rda")
WPTs_EXPOP_survey_method_25_percent <- WPTs_ExpOp_25_percent
WPTs_EXPOP_survey_method_25_percent <- sort(WPTs_EXPOP_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2013$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_25_percent)){
    if (Evaluation_survey_method_2013$WPT[i] == WPTs_EXPOP_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2013$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2013$SC_2013))
SC_detected_EXPOP_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_25_percent
SC_detected_25_percent$ExpOp[7] <- SC_detected_EXPOP_survey_25_percent

## Syst
a <- load("Syst_WPTs_25_percent_budget_CIdata2013.Rda")
WPTs_Syst_survey_method_25_percent <- WPTs_Syst_25_percent
WPTs_Syst_survey_method_25_percent <- sort(WPTs_Syst_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2013$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_25_percent)){
    if (Evaluation_survey_method_2013$WPT[i] == WPTs_Syst_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2013$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2013$SC_2013))
SC_detected_Syst_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_25_percent
SC_detected_25_percent$Syst[7] <- SC_detected_Syst_survey_25_percent

## Random
a <- load("Random_WPTs_25_percent_budget_CIdata2013.Rda")
WPTs_Random_survey_method_25_percent <- WPTs_Random_25_percent
WPTs_Random_survey_method_25_percent <- sort(WPTs_Random_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2013$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_25_percent)){
    if (Evaluation_survey_method_2013$WPT[i] == WPTs_Random_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2013$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2013$SC_2013))
SC_detected_Random_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_25_percent
SC_detected_25_percent$Random[7] <- SC_detected_Random_survey_25_percent



### 2015
## Knapsack 
a <- load("WPTs_for_KNAPSACK_CIdata2015_25_percent_budget.Rda")
WPTs_25_percent_KNAPSACK <- sort(WPTs_25_percent_KNAPSACK)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2015$WPT)) {
  for (j in 1:length(WPTs_25_percent_KNAPSACK)){
    if (Evaluation_survey_method_2015$WPT[i] == WPTs_25_percent_KNAPSACK[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2015$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2015$SC_2015))
SC_detected_KNAPSACK_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_25_percent
SC_detected_25_percent$Knapsack[8] <- SC_detected_KNAPSACK_25_percent

## R_vs_C 
a <- load("R_vs_C_WPTs_25_percent_budget_CIdata2015.Rda")
WPTs_R_vs_C_survey_method_25_percent <- WPTs_R_vs_C_25_percent
WPTs_R_vs_C_survey_method_25_percent <- sort(WPTs_R_vs_C_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2015$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_25_percent)){
    if (Evaluation_survey_method_2015$WPT[i] == WPTs_R_vs_C_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2015$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2015$SC_2015))
SC_detected_R_vs_C_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_25_percent
SC_detected_25_percent$R_vs_C[8] <- SC_detected_R_vs_C_survey_25_percent

## POcc
a <- load("Pocc_WPTs_25_percent_budget_CIdata2015.Rda")
WPTs_POCC_survey_method_25_percent <- WPTs_Pocc_25_percent
WPTs_POCC_survey_method_25_percent <- sort(WPTs_POCC_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2015$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_25_percent)){
    if (Evaluation_survey_method_2015$WPT[i] == WPTs_POCC_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2015$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2015$SC_2015))
SC_detected_POCC_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_25_percent
SC_detected_25_percent$Pocc[8] <- SC_detected_POCC_survey_25_percent

## ExpOp
a <- load("ExpOp_WPTs_25_percent_budget_CIdata2015.Rda")
WPTs_EXPOP_survey_method_25_percent <- WPTs_ExpOp_25_percent
WPTs_EXPOP_survey_method_25_percent <- sort(WPTs_EXPOP_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2015$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_25_percent)){
    if (Evaluation_survey_method_2015$WPT[i] == WPTs_EXPOP_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2015$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2015$SC_2015))
SC_detected_EXPOP_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_25_percent
SC_detected_25_percent$ExpOp[8] <- SC_detected_EXPOP_survey_25_percent

## Syst
a <- load("Syst_WPTs_25_percent_budget_CIdata2015.Rda")
WPTs_Syst_survey_method_25_percent <- WPTs_Syst_25_percent
WPTs_Syst_survey_method_25_percent <- sort(WPTs_Syst_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2015$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_25_percent)){
    if (Evaluation_survey_method_2015$WPT[i] == WPTs_Syst_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2015$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2015$SC_2015))
SC_detected_Syst_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_25_percent
SC_detected_25_percent$Syst[8] <- SC_detected_Syst_survey_25_percent

## Random
a <- load("Random_WPTs_25_percent_budget_CIdata2015.Rda")
WPTs_Random_survey_method_25_percent <- WPTs_Random_25_percent
WPTs_Random_survey_method_25_percent <- sort(WPTs_Random_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2015$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_25_percent)){
    if (Evaluation_survey_method_2015$WPT[i] == WPTs_Random_survey_method_25_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2015$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2015$SC_2015))
SC_detected_Random_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_25_percent
SC_detected_25_percent$Random[8] <- SC_detected_Random_survey_25_percent




#### 50%
### 2001
## Knapsack 
a <- load("WPTs_for_KNAPSACK_CIdata2001_50_percent_budget.Rda")
WPTs_50_percent_KNAPSACK <- sort(WPTs_50_percent_KNAPSACK)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2001$WPT)) {
  for (j in 1:length(WPTs_50_percent_KNAPSACK)){
    if (Evaluation_survey_method_2001$WPT[i] == WPTs_50_percent_KNAPSACK[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2001$SC_2001[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2001$SC_2001))
SC_detected_KNAPSACK_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_50_percent
SC_detected_50_percent$Knapsack[1] <- SC_detected_KNAPSACK_50_percent

## R_vs_C 
a <- load("R_vs_C_WPTs_50_percent_budget_CIdata2001.Rda")
WPTs_R_vs_C_survey_method_50_percent <- WPTs_R_vs_C_50_percent
WPTs_R_vs_C_survey_method_50_percent <- sort(WPTs_R_vs_C_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2001$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_50_percent)){
    if (Evaluation_survey_method_2001$WPT[i] == WPTs_R_vs_C_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2001$SC_2001[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2001$SC_2001))
SC_detected_R_vs_C_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_50_percent
SC_detected_50_percent$R_vs_C[1] <- SC_detected_R_vs_C_survey_50_percent

## POcc
a <- load("Pocc_WPTs_50_percent_budget_CIdata2001.Rda")
WPTs_POCC_survey_method_50_percent <- WPTs_Pocc_50_percent
WPTs_POCC_survey_method_50_percent <- sort(WPTs_POCC_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2001$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_50_percent)){
    if (Evaluation_survey_method_2001$WPT[i] == WPTs_POCC_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2001$SC_2001[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2001$SC_2001))
SC_detected_POCC_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_50_percent
SC_detected_50_percent$Pocc[1] <- SC_detected_POCC_survey_50_percent

## ExpOp
a <- load("ExpOp_WPTs_50_percent_budget_CIdata2001.Rda")
WPTs_EXPOP_survey_method_50_percent <- WPTs_ExpOp_50_percent
WPTs_EXPOP_survey_method_50_percent <- sort(WPTs_EXPOP_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2001$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_50_percent)){
    if (Evaluation_survey_method_2001$WPT[i] == WPTs_EXPOP_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2001$SC_2001[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2001$SC_2001))
SC_detected_EXPOP_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_50_percent
SC_detected_50_percent$ExpOp[1] <- SC_detected_EXPOP_survey_50_percent

## Syst
a <- load("Syst_WPTs_50_percent_budget_CIdata2001.Rda")
WPTs_Syst_survey_method_50_percent <- WPTs_Syst_50_percent
WPTs_Syst_survey_method_50_percent <- sort(WPTs_Syst_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2001$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_50_percent)){
    if (Evaluation_survey_method_2001$WPT[i] == WPTs_Syst_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2001$SC_2001[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2001$SC_2001))
SC_detected_Syst_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_50_percent
SC_detected_50_percent$Syst[1] <- SC_detected_Syst_survey_50_percent

## Random
a <- load("Random_WPTs_50_percent_budget_CIdata2001.Rda")
WPTs_Random_survey_method_50_percent <- WPTs_Random_50_percent
WPTs_Random_survey_method_50_percent <- sort(WPTs_Random_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2001$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_50_percent)){
    if (Evaluation_survey_method_2001$WPT[i] == WPTs_Random_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2001$SC_2001[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2001$SC_2001))
SC_detected_Random_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_50_percent
SC_detected_50_percent$Random[1] <- SC_detected_Random_survey_50_percent

### 2003
## Knapsack 
a <- load("WPTs_for_KNAPSACK_CIdata2003_50_percent_budget.Rda")
WPTs_50_percent_KNAPSACK <- sort(WPTs_50_percent_KNAPSACK)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2003$WPT)) {
  for (j in 1:length(WPTs_50_percent_KNAPSACK)){
    if (Evaluation_survey_method_2003$WPT[i] == WPTs_50_percent_KNAPSACK[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2003$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2003$SC_2003))
SC_detected_KNAPSACK_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_50_percent
SC_detected_50_percent$Knapsack[2] <- SC_detected_KNAPSACK_50_percent
## R_vs_C 
a <- load("R_vs_C_WPTs_50_percent_budget_CIdata2003.Rda")
WPTs_R_vs_C_survey_method_50_percent <- WPTs_R_vs_C_50_percent
WPTs_R_vs_C_survey_method_50_percent <- sort(WPTs_R_vs_C_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2003$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_50_percent)){
    if (Evaluation_survey_method_2003$WPT[i] == WPTs_R_vs_C_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2003$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2003$SC_2003))
SC_detected_R_vs_C_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_50_percent
SC_detected_50_percent$R_vs_C[2] <- SC_detected_R_vs_C_survey_50_percent

## POcc
a <- load("Pocc_WPTs_50_percent_budget_CIdata2003.Rda")
WPTs_POCC_survey_method_50_percent <- WPTs_Pocc_50_percent
WPTs_POCC_survey_method_50_percent <- sort(WPTs_POCC_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2003$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_50_percent)){
    if (Evaluation_survey_method_2003$WPT[i] == WPTs_POCC_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2003$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2003$SC_2003))
SC_detected_POCC_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_50_percent
SC_detected_50_percent$Pocc[2] <- SC_detected_POCC_survey_50_percent

## ExpOp
a <- load("ExpOp_WPTs_50_percent_budget_CIdata2003.Rda")
WPTs_EXPOP_survey_method_50_percent <- WPTs_ExpOp_50_percent
WPTs_EXPOP_survey_method_50_percent <- sort(WPTs_EXPOP_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2003$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_50_percent)){
    if (Evaluation_survey_method_2003$WPT[i] == WPTs_EXPOP_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2003$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2003$SC_2003))
SC_detected_EXPOP_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_50_percent
SC_detected_50_percent$ExpOp[2] <- SC_detected_EXPOP_survey_50_percent

## Syst
a <- load("Syst_WPTs_50_percent_budget_CIdata2003.Rda")
WPTs_Syst_survey_method_50_percent <- WPTs_Syst_50_percent
WPTs_Syst_survey_method_50_percent <- sort(WPTs_Syst_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2003$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_50_percent)){
    if (Evaluation_survey_method_2003$WPT[i] == WPTs_Syst_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2003$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2003$SC_2003))
SC_detected_Syst_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_50_percent
SC_detected_50_percent$Syst[2] <- SC_detected_Syst_survey_50_percent

## Random
a <- load("Random_WPTs_50_percent_budget_CIdata2003.Rda")
WPTs_Random_survey_method_50_percent <- WPTs_Random_50_percent
WPTs_Random_survey_method_50_percent <- sort(WPTs_Random_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2003$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_50_percent)){
    if (Evaluation_survey_method_2003$WPT[i] == WPTs_Random_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2003$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2003$SC_2003))
SC_detected_Random_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_50_percent
SC_detected_50_percent$Random[2] <- SC_detected_Random_survey_50_percent

### 2005
## Knapsack 
a <- load("WPTs_for_KNAPSACK_CIdata2005_50_percent_budget.Rda")
WPTs_50_percent_KNAPSACK <- sort(WPTs_50_percent_KNAPSACK)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2005$WPT)) {
  for (j in 1:length(WPTs_50_percent_KNAPSACK)){
    if (Evaluation_survey_method_2005$WPT[i] == WPTs_50_percent_KNAPSACK[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2005$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2005$SC_2005))
SC_detected_KNAPSACK_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_50_percent
SC_detected_50_percent$Knapsack[3] <- SC_detected_KNAPSACK_50_percent

## R_vs_C 
a <- load("R_vs_C_WPTs_50_percent_budget_CIdata2005.Rda")
WPTs_R_vs_C_survey_method_50_percent <- WPTs_R_vs_C_50_percent
WPTs_R_vs_C_survey_method_50_percent <- sort(WPTs_R_vs_C_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2005$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_50_percent)){
    if (Evaluation_survey_method_2005$WPT[i] == WPTs_R_vs_C_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2005$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2005$SC_2005))
SC_detected_R_vs_C_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_50_percent
SC_detected_50_percent$R_vs_C[3] <- SC_detected_R_vs_C_survey_50_percent

## POcc
a <- load("Pocc_WPTs_50_percent_budget_CIdata2005.Rda")
WPTs_POCC_survey_method_50_percent <- WPTs_Pocc_50_percent
WPTs_POCC_survey_method_50_percent <- sort(WPTs_POCC_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2005$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_50_percent)){
    if (Evaluation_survey_method_2005$WPT[i] == WPTs_POCC_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2005$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2005$SC_2005))
SC_detected_POCC_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_50_percent
SC_detected_50_percent$Pocc[3] <- SC_detected_POCC_survey_50_percent

## ExpOp
a <- load("ExpOp_WPTs_50_percent_budget_CIdata2005.Rda")
WPTs_EXPOP_survey_method_50_percent <- WPTs_ExpOp_50_percent
WPTs_EXPOP_survey_method_50_percent <- sort(WPTs_EXPOP_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2005$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_50_percent)){
    if (Evaluation_survey_method_2005$WPT[i] == WPTs_EXPOP_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2005$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2005$SC_2005))
SC_detected_EXPOP_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_50_percent
SC_detected_50_percent$ExpOp[3] <- SC_detected_EXPOP_survey_50_percent

## Syst
a <- load("Syst_WPTs_50_percent_budget_CIdata2005.Rda")
WPTs_Syst_survey_method_50_percent <- WPTs_Syst_50_percent
WPTs_Syst_survey_method_50_percent <- sort(WPTs_Syst_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2005$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_50_percent)){
    if (Evaluation_survey_method_2005$WPT[i] == WPTs_Syst_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2005$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2005$SC_2005))
SC_detected_Syst_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_50_percent
SC_detected_50_percent$Syst[3] <- SC_detected_Syst_survey_50_percent

## Random
a <- load("Random_WPTs_50_percent_budget_CIdata2005.Rda")
WPTs_Random_survey_method_50_percent <- WPTs_Random_50_percent
WPTs_Random_survey_method_50_percent <- sort(WPTs_Random_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2005$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_50_percent)){
    if (Evaluation_survey_method_2005$WPT[i] == WPTs_Random_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2005$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2005$SC_2005))
SC_detected_Random_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_50_percent
SC_detected_50_percent$Random[3] <- SC_detected_Random_survey_50_percent

### 2007
## Knapsack 
a <- load("WPTs_for_KNAPSACK_CIdata2007_50_percent_budget.Rda")
WPTs_50_percent_KNAPSACK <- sort(WPTs_50_percent_KNAPSACK)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2007$WPT)) {
  for (j in 1:length(WPTs_50_percent_KNAPSACK)){
    if (Evaluation_survey_method_2007$WPT[i] == WPTs_50_percent_KNAPSACK[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2007$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2007$SC_2007))
SC_detected_KNAPSACK_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_50_percent
SC_detected_50_percent$Knapsack[4] <- SC_detected_KNAPSACK_50_percent

## R_vs_C 
a <- load("R_vs_C_WPTs_50_percent_budget_CIdata2007.Rda")
WPTs_R_vs_C_survey_method_50_percent <- WPTs_R_vs_C_50_percent
WPTs_R_vs_C_survey_method_50_percent <- sort(WPTs_R_vs_C_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2007$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_50_percent)){
    if (Evaluation_survey_method_2007$WPT[i] == WPTs_R_vs_C_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2007$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2007$SC_2007))
SC_detected_R_vs_C_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_50_percent
SC_detected_50_percent$R_vs_C[4] <- SC_detected_R_vs_C_survey_50_percent

## POcc
a <- load("Pocc_WPTs_50_percent_budget_CIdata2007.Rda")
WPTs_POCC_survey_method_50_percent <- WPTs_Pocc_50_percent
WPTs_POCC_survey_method_50_percent <- sort(WPTs_POCC_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2007$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_50_percent)){
    if (Evaluation_survey_method_2007$WPT[i] == WPTs_POCC_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2007$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2007$SC_2007))
SC_detected_POCC_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_50_percent
SC_detected_50_percent$Pocc[4] <- SC_detected_POCC_survey_50_percent

## ExpOp
a <- load("ExpOp_WPTs_50_percent_budget_CIdata2007.Rda")
WPTs_EXPOP_survey_method_50_percent <- WPTs_ExpOp_50_percent
WPTs_EXPOP_survey_method_50_percent <- sort(WPTs_EXPOP_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2007$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_50_percent)){
    if (Evaluation_survey_method_2007$WPT[i] == WPTs_EXPOP_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2007$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2007$SC_2007))
SC_detected_EXPOP_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_50_percent
SC_detected_50_percent$ExpOp[4] <- SC_detected_EXPOP_survey_50_percent

## Syst
a <- load("Syst_WPTs_50_percent_budget_CIdata2007.Rda")
WPTs_Syst_survey_method_50_percent <- WPTs_Syst_50_percent
WPTs_Syst_survey_method_50_percent <- sort(WPTs_Syst_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2007$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_50_percent)){
    if (Evaluation_survey_method_2007$WPT[i] == WPTs_Syst_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2007$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2007$SC_2007))
SC_detected_Syst_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_50_percent
SC_detected_50_percent$Syst[4] <- SC_detected_Syst_survey_50_percent

## Random
a <- load("Random_WPTs_50_percent_budget_CIdata2007.Rda")
WPTs_Random_survey_method_50_percent <- WPTs_Random_50_percent
WPTs_Random_survey_method_50_percent <- sort(WPTs_Random_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2007$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_50_percent)){
    if (Evaluation_survey_method_2007$WPT[i] == WPTs_Random_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2007$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2007$SC_2007))
SC_detected_Random_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_50_percent
SC_detected_50_percent$Random[4] <- SC_detected_Random_survey_50_percent


### 2009
## Knapsack 
a <- load("WPTs_for_KNAPSACK_CIdata2009_50_percent_budget.Rda")
WPTs_50_percent_KNAPSACK <- sort(WPTs_50_percent_KNAPSACK)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2009$WPT)) {
  for (j in 1:length(WPTs_50_percent_KNAPSACK)){
    if (Evaluation_survey_method_2009$WPT[i] == WPTs_50_percent_KNAPSACK[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2009$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2009$SC_2009))
SC_detected_KNAPSACK_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_50_percent
SC_detected_50_percent$Knapsack[5] <- SC_detected_KNAPSACK_50_percent

## R_vs_C 
a <- load("R_vs_C_WPTs_50_percent_budget_CIdata2009.Rda")
WPTs_R_vs_C_survey_method_50_percent <- WPTs_R_vs_C_50_percent
WPTs_R_vs_C_survey_method_50_percent <- sort(WPTs_R_vs_C_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2009$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_50_percent)){
    if (Evaluation_survey_method_2009$WPT[i] == WPTs_R_vs_C_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2009$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2009$SC_2009))
SC_detected_R_vs_C_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_50_percent
SC_detected_50_percent$R_vs_C[5] <- SC_detected_R_vs_C_survey_50_percent

## POcc
a <- load("Pocc_WPTs_50_percent_budget_CIdata2009.Rda")
WPTs_POCC_survey_method_50_percent <- WPTs_Pocc_50_percent
WPTs_POCC_survey_method_50_percent <- sort(WPTs_POCC_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2009$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_50_percent)){
    if (Evaluation_survey_method_2009$WPT[i] == WPTs_POCC_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2009$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2009$SC_2009))
SC_detected_POCC_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_50_percent
SC_detected_50_percent$Pocc[5] <- SC_detected_POCC_survey_50_percent

## ExpOp
a <- load("ExpOp_WPTs_50_percent_budget_CIdata2009.Rda")
WPTs_EXPOP_survey_method_50_percent <- WPTs_ExpOp_50_percent
WPTs_EXPOP_survey_method_50_percent <- sort(WPTs_EXPOP_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2009$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_50_percent)){
    if (Evaluation_survey_method_2009$WPT[i] == WPTs_EXPOP_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2009$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2009$SC_2009))
SC_detected_EXPOP_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_50_percent
SC_detected_50_percent$ExpOp[5] <- SC_detected_EXPOP_survey_50_percent

## Syst
a <- load("Syst_WPTs_50_percent_budget_CIdata2009.Rda")
WPTs_Syst_survey_method_50_percent <- WPTs_Syst_50_percent
WPTs_Syst_survey_method_50_percent <- sort(WPTs_Syst_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2009$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_50_percent)){
    if (Evaluation_survey_method_2009$WPT[i] == WPTs_Syst_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2009$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2009$SC_2009))
SC_detected_Syst_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_50_percent
SC_detected_50_percent$Syst[5] <- SC_detected_Syst_survey_50_percent

## Random
a <- load("Random_WPTs_50_percent_budget_CIdata2009.Rda")
WPTs_Random_survey_method_50_percent <- WPTs_Random_50_percent
WPTs_Random_survey_method_50_percent <- sort(WPTs_Random_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2009$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_50_percent)){
    if (Evaluation_survey_method_2009$WPT[i] == WPTs_Random_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2009$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2009$SC_2009))
SC_detected_Random_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_50_percent
SC_detected_50_percent$Random[5] <- SC_detected_Random_survey_50_percent


### 2011
## Knapsack 
a <- load("WPTs_for_KNAPSACK_CIdata2011_50_percent_budget.Rda")
WPTs_50_percent_KNAPSACK <- sort(WPTs_50_percent_KNAPSACK)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2011$WPT)) {
  for (j in 1:length(WPTs_50_percent_KNAPSACK)){
    if (Evaluation_survey_method_2011$WPT[i] == WPTs_50_percent_KNAPSACK[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2011$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2011$SC_2011))
SC_detected_KNAPSACK_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_50_percent
SC_detected_50_percent$Knapsack[6] <- SC_detected_KNAPSACK_50_percent

## R_vs_C 
a <- load("R_vs_C_WPTs_50_percent_budget_CIdata2011.Rda")
WPTs_R_vs_C_survey_method_50_percent <- WPTs_R_vs_C_50_percent
WPTs_R_vs_C_survey_method_50_percent <- sort(WPTs_R_vs_C_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2011$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_50_percent)){
    if (Evaluation_survey_method_2011$WPT[i] == WPTs_R_vs_C_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2011$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2011$SC_2011))
SC_detected_R_vs_C_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_50_percent
SC_detected_50_percent$R_vs_C[6] <- SC_detected_R_vs_C_survey_50_percent

## POcc
a <- load("Pocc_WPTs_50_percent_budget_CIdata2011.Rda")
WPTs_POCC_survey_method_50_percent <- WPTs_Pocc_50_percent
WPTs_POCC_survey_method_50_percent <- sort(WPTs_POCC_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2011$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_50_percent)){
    if (Evaluation_survey_method_2011$WPT[i] == WPTs_POCC_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2011$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2011$SC_2011))
SC_detected_POCC_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_50_percent
SC_detected_50_percent$Pocc[6] <- SC_detected_POCC_survey_50_percent

## ExpOp
a <- load("ExpOp_WPTs_50_percent_budget_CIdata2011.Rda")
WPTs_EXPOP_survey_method_50_percent <- WPTs_ExpOp_50_percent
WPTs_EXPOP_survey_method_50_percent <- sort(WPTs_EXPOP_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2011$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_50_percent)){
    if (Evaluation_survey_method_2011$WPT[i] == WPTs_EXPOP_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2011$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2011$SC_2011))
SC_detected_EXPOP_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_50_percent
SC_detected_50_percent$ExpOp[6] <- SC_detected_EXPOP_survey_50_percent

## Syst
a <- load("Syst_WPTs_50_percent_budget_CIdata2011.Rda")
WPTs_Syst_survey_method_50_percent <- WPTs_Syst_50_percent
WPTs_Syst_survey_method_50_percent <- sort(WPTs_Syst_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2011$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_50_percent)){
    if (Evaluation_survey_method_2011$WPT[i] == WPTs_Syst_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2011$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2011$SC_2011))
SC_detected_Syst_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_50_percent
SC_detected_50_percent$Syst[6] <- SC_detected_Syst_survey_50_percent

## Random
a <- load("Random_WPTs_50_percent_budget_CIdata2011.Rda")
WPTs_Random_survey_method_50_percent <- WPTs_Random_50_percent
WPTs_Random_survey_method_50_percent <- sort(WPTs_Random_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2011$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_50_percent)){
    if (Evaluation_survey_method_2011$WPT[i] == WPTs_Random_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2011$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2011$SC_2011))
SC_detected_Random_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_50_percent
SC_detected_50_percent$Random[6] <- SC_detected_Random_survey_50_percent


### 2013
## Knapsack 
a <- load("WPTs_for_KNAPSACK_CIdata2011_50_percent_budget.Rda")
WPTs_50_percent_KNAPSACK <- sort(WPTs_50_percent_KNAPSACK)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2013$WPT)) {
  for (j in 1:length(WPTs_50_percent_KNAPSACK)){
    if (Evaluation_survey_method_2013$WPT[i] == WPTs_50_percent_KNAPSACK[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2013$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2013$SC_2013))
SC_detected_KNAPSACK_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_50_percent
SC_detected_50_percent$Knapsack[7] <- SC_detected_KNAPSACK_50_percent

## R_vs_C 
a <- load("R_vs_C_WPTs_50_percent_budget_CIdata2013.Rda")
WPTs_R_vs_C_survey_method_50_percent <- WPTs_R_vs_C_50_percent
WPTs_R_vs_C_survey_method_50_percent <- sort(WPTs_R_vs_C_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2013$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_50_percent)){
    if (Evaluation_survey_method_2013$WPT[i] == WPTs_R_vs_C_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2013$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2013$SC_2013))
SC_detected_R_vs_C_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_50_percent
SC_detected_50_percent$R_vs_C[7] <- SC_detected_R_vs_C_survey_50_percent

## POcc
a <- load("Pocc_WPTs_50_percent_budget_CIdata2013.Rda")
WPTs_POCC_survey_method_50_percent <- WPTs_Pocc_50_percent
WPTs_POCC_survey_method_50_percent <- sort(WPTs_POCC_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2013$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_50_percent)){
    if (Evaluation_survey_method_2013$WPT[i] == WPTs_POCC_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2013$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2013$SC_2013))
SC_detected_POCC_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_50_percent
SC_detected_50_percent$Pocc[7] <- SC_detected_POCC_survey_50_percent

## ExpOp
a <- load("ExpOp_WPTs_50_percent_budget_CIdata2013.Rda")
WPTs_EXPOP_survey_method_50_percent <- WPTs_ExpOp_50_percent
WPTs_EXPOP_survey_method_50_percent <- sort(WPTs_EXPOP_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2013$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_50_percent)){
    if (Evaluation_survey_method_2013$WPT[i] == WPTs_EXPOP_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2013$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2013$SC_2013))
SC_detected_EXPOP_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_50_percent
SC_detected_50_percent$ExpOp[7] <- SC_detected_EXPOP_survey_50_percent

## Syst
a <- load("Syst_WPTs_50_percent_budget_CIdata2013.Rda")
WPTs_Syst_survey_method_50_percent <- WPTs_Syst_50_percent
WPTs_Syst_survey_method_50_percent <- sort(WPTs_Syst_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2013$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_50_percent)){
    if (Evaluation_survey_method_2013$WPT[i] == WPTs_Syst_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2013$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2013$SC_2013))
SC_detected_Syst_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_50_percent
SC_detected_50_percent$Syst[7] <- SC_detected_Syst_survey_50_percent

## Random
a <- load("Random_WPTs_50_percent_budget_CIdata2013.Rda")
WPTs_Random_survey_method_50_percent <- WPTs_Random_50_percent
WPTs_Random_survey_method_50_percent <- sort(WPTs_Random_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2013$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_50_percent)){
    if (Evaluation_survey_method_2013$WPT[i] == WPTs_Random_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2013$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2013$SC_2013))
SC_detected_Random_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_50_percent
SC_detected_50_percent$Random[7] <- SC_detected_Random_survey_50_percent



### 2015
## Knapsack 
a <- load("WPTs_for_KNAPSACK_CIdata2011_50_percent_budget.Rda")
WPTs_50_percent_KNAPSACK <- sort(WPTs_50_percent_KNAPSACK)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2015$WPT)) {
  for (j in 1:length(WPTs_50_percent_KNAPSACK)){
    if (Evaluation_survey_method_2015$WPT[i] == WPTs_50_percent_KNAPSACK[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2015$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2015$SC_2015))
SC_detected_KNAPSACK_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_50_percent
SC_detected_50_percent$Knapsack[8] <- SC_detected_KNAPSACK_50_percent

## R_vs_C 
a <- load("R_vs_C_WPTs_50_percent_budget_CIdata2015.Rda")
WPTs_R_vs_C_survey_method_50_percent <- WPTs_R_vs_C_50_percent
WPTs_R_vs_C_survey_method_50_percent <- sort(WPTs_R_vs_C_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2015$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_50_percent)){
    if (Evaluation_survey_method_2015$WPT[i] == WPTs_R_vs_C_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2015$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2015$SC_2015))
SC_detected_R_vs_C_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_50_percent
SC_detected_50_percent$R_vs_C[8] <- SC_detected_R_vs_C_survey_50_percent

## POcc
a <- load("Pocc_WPTs_50_percent_budget_CIdata2015.Rda")
WPTs_POCC_survey_method_50_percent <- WPTs_Pocc_50_percent
WPTs_POCC_survey_method_50_percent <- sort(WPTs_POCC_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2015$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_50_percent)){
    if (Evaluation_survey_method_2015$WPT[i] == WPTs_POCC_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2015$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2015$SC_2015))
SC_detected_POCC_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_50_percent
SC_detected_50_percent$Pocc[8] <- SC_detected_POCC_survey_50_percent

## ExpOp
a <- load("ExpOp_WPTs_50_percent_budget_CIdata2015.Rda")
WPTs_EXPOP_survey_method_50_percent <- WPTs_ExpOp_50_percent
WPTs_EXPOP_survey_method_50_percent <- sort(WPTs_EXPOP_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2015$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_50_percent)){
    if (Evaluation_survey_method_2015$WPT[i] == WPTs_EXPOP_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2015$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2015$SC_2015))
SC_detected_EXPOP_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_50_percent
SC_detected_50_percent$ExpOp[8] <- SC_detected_EXPOP_survey_50_percent

## Syst
a <- load("Syst_WPTs_50_percent_budget_CIdata2015.Rda")
WPTs_Syst_survey_method_50_percent <- WPTs_Syst_50_percent
WPTs_Syst_survey_method_50_percent <- sort(WPTs_Syst_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2015$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_50_percent)){
    if (Evaluation_survey_method_2015$WPT[i] == WPTs_Syst_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2015$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2015$SC_2015))
SC_detected_Syst_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_50_percent
SC_detected_50_percent$Syst[8] <- SC_detected_Syst_survey_50_percent

## Random
a <- load("Random_WPTs_50_percent_budget_CIdata2015.Rda")
WPTs_Random_survey_method_50_percent <- WPTs_Random_50_percent
WPTs_Random_survey_method_50_percent <- sort(WPTs_Random_survey_method_50_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2015$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_50_percent)){
    if (Evaluation_survey_method_2015$WPT[i] == WPTs_Random_survey_method_50_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2015$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2015$SC_2015))
SC_detected_Random_survey_50_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_50_percent
SC_detected_50_percent$Random[8] <- SC_detected_Random_survey_50_percent


#### 75%
### 2001
## Knapsack 
a <- load("WPTs_for_KNAPSACK_CIdata2001_75_percent_budget.Rda")
WPTs_75_percent_KNAPSACK <- sort(WPTs_75_percent_KNAPSACK)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2001$WPT)) {
  for (j in 1:length(WPTs_75_percent_KNAPSACK)){
    if (Evaluation_survey_method_2001$WPT[i] == WPTs_75_percent_KNAPSACK[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2001$SC_2001[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2001$SC_2001))
SC_detected_KNAPSACK_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_75_percent
SC_detected_75_percent$Knapsack[1] <- SC_detected_KNAPSACK_75_percent

## R_vs_C 
a <- load("R_vs_C_WPTs_75_percent_budget_CIdata2001.Rda")
WPTs_R_vs_C_survey_method_75_percent <- WPTs_R_vs_C_75_percent
WPTs_R_vs_C_survey_method_75_percent <- sort(WPTs_R_vs_C_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2001$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_75_percent)){
    if (Evaluation_survey_method_2001$WPT[i] == WPTs_R_vs_C_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2001$SC_2001[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2001$SC_2001))
SC_detected_R_vs_C_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_75_percent
SC_detected_75_percent$R_vs_C[1] <- SC_detected_R_vs_C_survey_75_percent

## POcc
a <- load("Pocc_WPTs_75_percent_budget_CIdata2001.Rda")
WPTs_POCC_survey_method_75_percent <- WPTs_Pocc_75_percent
WPTs_POCC_survey_method_75_percent <- sort(WPTs_POCC_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2001$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_75_percent)){
    if (Evaluation_survey_method_2001$WPT[i] == WPTs_POCC_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2001$SC_2001[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2001$SC_2001))
SC_detected_POCC_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_75_percent
SC_detected_75_percent$Pocc[1] <- SC_detected_POCC_survey_75_percent

## ExpOp
a <- load("ExpOp_WPTs_75_percent_budget_CIdata2001.Rda")
WPTs_EXPOP_survey_method_75_percent <- WPTs_ExpOp_75_percent
WPTs_EXPOP_survey_method_75_percent <- sort(WPTs_EXPOP_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2001$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_75_percent)){
    if (Evaluation_survey_method_2001$WPT[i] == WPTs_EXPOP_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2001$SC_2001[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2001$SC_2001))
SC_detected_EXPOP_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_75_percent
SC_detected_75_percent$ExpOp[1] <- SC_detected_EXPOP_survey_75_percent

## Syst
a <- load("Syst_WPTs_75_percent_budget_CIdata2001.Rda")
WPTs_Syst_survey_method_75_percent <- WPTs_Syst_75_percent
WPTs_Syst_survey_method_75_percent <- sort(WPTs_Syst_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2001$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_75_percent)){
    if (Evaluation_survey_method_2001$WPT[i] == WPTs_Syst_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2001$SC_2001[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2001$SC_2001))
SC_detected_Syst_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_75_percent
SC_detected_75_percent$Syst[1] <- SC_detected_Syst_survey_75_percent

## Random
a <- load("Random_WPTs_75_percent_budget_CIdata2001.Rda")
WPTs_Random_survey_method_75_percent <- WPTs_Random_75_percent
WPTs_Random_survey_method_75_percent <- sort(WPTs_Random_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2001$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_75_percent)){
    if (Evaluation_survey_method_2001$WPT[i] == WPTs_Random_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2001$SC_2001[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2001$SC_2001))
SC_detected_Random_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_75_percent
SC_detected_75_percent$Random[1] <- SC_detected_Random_survey_75_percent

### 2003
## Knapsack 
a <- load("WPTs_for_KNAPSACK_CIdata2003_75_percent_budget.Rda")
WPTs_75_percent_KNAPSACK <- sort(WPTs_75_percent_KNAPSACK)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2003$WPT)) {
  for (j in 1:length(WPTs_75_percent_KNAPSACK)){
    if (Evaluation_survey_method_2003$WPT[i] == WPTs_75_percent_KNAPSACK[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2003$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2003$SC_2003))
SC_detected_KNAPSACK_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_75_percent
SC_detected_75_percent$Knapsack[2] <- SC_detected_KNAPSACK_75_percent

## R_vs_C 
a <- load("R_vs_C_WPTs_75_percent_budget_CIdata2003.Rda")
WPTs_R_vs_C_survey_method_75_percent <- WPTs_R_vs_C_75_percent
WPTs_R_vs_C_survey_method_75_percent <- sort(WPTs_R_vs_C_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2003$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_75_percent)){
    if (Evaluation_survey_method_2003$WPT[i] == WPTs_R_vs_C_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2003$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2003$SC_2003))
SC_detected_R_vs_C_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_75_percent
SC_detected_75_percent$R_vs_C[2] <- SC_detected_R_vs_C_survey_75_percent

## POcc
a <- load("Pocc_WPTs_75_percent_budget_CIdata2003.Rda")
WPTs_POCC_survey_method_75_percent <- WPTs_Pocc_75_percent
WPTs_POCC_survey_method_75_percent <- sort(WPTs_POCC_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2003$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_75_percent)){
    if (Evaluation_survey_method_2003$WPT[i] == WPTs_POCC_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2003$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2003$SC_2003))
SC_detected_POCC_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_75_percent
SC_detected_75_percent$Pocc[2] <- SC_detected_POCC_survey_75_percent

## ExpOp
a <- load("ExpOp_WPTs_75_percent_budget_CIdata2003.Rda")
WPTs_EXPOP_survey_method_75_percent <- WPTs_ExpOp_75_percent
WPTs_EXPOP_survey_method_75_percent <- sort(WPTs_EXPOP_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2003$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_75_percent)){
    if (Evaluation_survey_method_2003$WPT[i] == WPTs_EXPOP_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2003$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2003$SC_2003))
SC_detected_EXPOP_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_75_percent
SC_detected_75_percent$ExpOp[2] <- SC_detected_EXPOP_survey_75_percent

## Syst
a <- load("Syst_WPTs_75_percent_budget_CIdata2003.Rda")
WPTs_Syst_survey_method_75_percent <- WPTs_Syst_75_percent
WPTs_Syst_survey_method_75_percent <- sort(WPTs_Syst_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2003$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_75_percent)){
    if (Evaluation_survey_method_2003$WPT[i] == WPTs_Syst_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2003$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2003$SC_2003))
SC_detected_Syst_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_75_percent
SC_detected_75_percent$Syst[2] <- SC_detected_Syst_survey_75_percent

## Random
a <- load("Random_WPTs_75_percent_budget_CIdata2003.Rda")
WPTs_Random_survey_method_75_percent <- WPTs_Random_75_percent
WPTs_Random_survey_method_75_percent <- sort(WPTs_Random_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2003$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_75_percent)){
    if (Evaluation_survey_method_2003$WPT[i] == WPTs_Random_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2003$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2003$SC_2003))
SC_detected_Random_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_75_percent
SC_detected_75_percent$Random[2] <- SC_detected_Random_survey_75_percent

### 2005
## Knapsack 
a <- load("WPTs_for_KNAPSACK_CIdata2005_75_percent_budget.Rda")
WPTs_75_percent_KNAPSACK <- sort(WPTs_75_percent_KNAPSACK)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2005$WPT)) {
  for (j in 1:length(WPTs_75_percent_KNAPSACK)){
    if (Evaluation_survey_method_2005$WPT[i] == WPTs_75_percent_KNAPSACK[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2005$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2005$SC_2005))
SC_detected_KNAPSACK_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_75_percent
SC_detected_75_percent$Knapsack[3] <- SC_detected_KNAPSACK_75_percent

## R_vs_C 
a <- load("R_vs_C_WPTs_75_percent_budget_CIdata2005.Rda")
WPTs_R_vs_C_survey_method_75_percent <- WPTs_R_vs_C_75_percent
WPTs_R_vs_C_survey_method_75_percent <- sort(WPTs_R_vs_C_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2005$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_75_percent)){
    if (Evaluation_survey_method_2005$WPT[i] == WPTs_R_vs_C_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2005$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2005$SC_2005))
SC_detected_R_vs_C_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_75_percent
SC_detected_75_percent$R_vs_C[3] <- SC_detected_R_vs_C_survey_75_percent

## POcc
a <- load("Pocc_WPTs_75_percent_budget_CIdata2005.Rda")
WPTs_POCC_survey_method_75_percent <- WPTs_Pocc_75_percent
WPTs_POCC_survey_method_75_percent <- sort(WPTs_POCC_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2005$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_75_percent)){
    if (Evaluation_survey_method_2005$WPT[i] == WPTs_POCC_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2005$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2005$SC_2005))
SC_detected_POCC_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_75_percent
SC_detected_75_percent$Pocc[3] <- SC_detected_POCC_survey_75_percent

## ExpOp
a <- load("ExpOp_WPTs_75_percent_budget_CIdata2005.Rda")
WPTs_EXPOP_survey_method_75_percent <- WPTs_ExpOp_75_percent
WPTs_EXPOP_survey_method_75_percent <- sort(WPTs_EXPOP_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2005$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_75_percent)){
    if (Evaluation_survey_method_2005$WPT[i] == WPTs_EXPOP_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2005$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2005$SC_2005))
SC_detected_EXPOP_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_75_percent
SC_detected_75_percent$ExpOp[3] <- SC_detected_EXPOP_survey_75_percent

## Syst
a <- load("Syst_WPTs_75_percent_budget_CIdata2005.Rda")
WPTs_Syst_survey_method_75_percent <- WPTs_Syst_75_percent
WPTs_Syst_survey_method_75_percent <- sort(WPTs_Syst_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2005$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_75_percent)){
    if (Evaluation_survey_method_2005$WPT[i] == WPTs_Syst_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2005$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2005$SC_2005))
SC_detected_Syst_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_75_percent
SC_detected_75_percent$Syst[3] <- SC_detected_Syst_survey_75_percent

## Random
a <- load("Random_WPTs_75_percent_budget_CIdata2005.Rda")
WPTs_Random_survey_method_75_percent <- WPTs_Random_75_percent
WPTs_Random_survey_method_75_percent <- sort(WPTs_Random_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2005$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_75_percent)){
    if (Evaluation_survey_method_2005$WPT[i] == WPTs_Random_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2005$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2005$SC_2005))
SC_detected_Random_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_75_percent
SC_detected_75_percent$Random[3] <- SC_detected_Random_survey_75_percent

### 2007
## Knapsack 
a <- load("WPTs_for_KNAPSACK_CIdata2007_75_percent_budget.Rda")
WPTs_75_percent_KNAPSACK <- sort(WPTs_75_percent_KNAPSACK)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2007$WPT)) {
  for (j in 1:length(WPTs_75_percent_KNAPSACK)){
    if (Evaluation_survey_method_2007$WPT[i] == WPTs_75_percent_KNAPSACK[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2007$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2007$SC_2007))
SC_detected_KNAPSACK_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_75_percent
SC_detected_75_percent$Knapsack[4] <- SC_detected_KNAPSACK_75_percent

## R_vs_C 
a <- load("R_vs_C_WPTs_75_percent_budget_CIdata2007.Rda")
WPTs_R_vs_C_survey_method_75_percent <- WPTs_R_vs_C_75_percent
WPTs_R_vs_C_survey_method_75_percent <- sort(WPTs_R_vs_C_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2007$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_75_percent)){
    if (Evaluation_survey_method_2007$WPT[i] == WPTs_R_vs_C_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2007$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2007$SC_2007))
SC_detected_R_vs_C_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_75_percent
SC_detected_75_percent$R_vs_C[4] <- SC_detected_R_vs_C_survey_75_percent

## POcc
a <- load("Pocc_WPTs_75_percent_budget_CIdata2007.Rda")
WPTs_POCC_survey_method_75_percent <- WPTs_Pocc_75_percent
WPTs_POCC_survey_method_75_percent <- sort(WPTs_POCC_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2007$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_75_percent)){
    if (Evaluation_survey_method_2007$WPT[i] == WPTs_POCC_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2007$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2007$SC_2007))
SC_detected_POCC_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_75_percent
SC_detected_75_percent$Pocc[4] <- SC_detected_POCC_survey_75_percent

## ExpOp
a <- load("ExpOp_WPTs_75_percent_budget_CIdata2007.Rda")
WPTs_EXPOP_survey_method_75_percent <- WPTs_ExpOp_75_percent
WPTs_EXPOP_survey_method_75_percent <- sort(WPTs_EXPOP_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2007$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_75_percent)){
    if (Evaluation_survey_method_2007$WPT[i] == WPTs_EXPOP_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2007$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2007$SC_2007))
SC_detected_EXPOP_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_75_percent
SC_detected_75_percent$ExpOp[4] <- SC_detected_EXPOP_survey_75_percent

## Syst
a <- load("Syst_WPTs_75_percent_budget_CIdata2007.Rda")
WPTs_Syst_survey_method_75_percent <- WPTs_Syst_75_percent
WPTs_Syst_survey_method_75_percent <- sort(WPTs_Syst_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2007$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_75_percent)){
    if (Evaluation_survey_method_2007$WPT[i] == WPTs_Syst_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2007$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2007$SC_2007))
SC_detected_Syst_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_75_percent
SC_detected_75_percent$Syst[4] <- SC_detected_Syst_survey_75_percent

## Random
a <- load("Random_WPTs_75_percent_budget_CIdata2007.Rda")
WPTs_Random_survey_method_75_percent <- WPTs_Random_75_percent
WPTs_Random_survey_method_75_percent <- sort(WPTs_Random_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2007$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_75_percent)){
    if (Evaluation_survey_method_2007$WPT[i] == WPTs_Random_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2007$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2007$SC_2007))
SC_detected_Random_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_75_percent
SC_detected_75_percent$Random[4] <- SC_detected_Random_survey_75_percent


### 2009
## Knapsack 
a <- load("WPTs_for_KNAPSACK_CIdata2009_75_percent_budget.Rda")
WPTs_75_percent_KNAPSACK <- sort(WPTs_75_percent_KNAPSACK)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2009$WPT)) {
  for (j in 1:length(WPTs_75_percent_KNAPSACK)){
    if (Evaluation_survey_method_2009$WPT[i] == WPTs_75_percent_KNAPSACK[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2009$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2009$SC_2009))
SC_detected_KNAPSACK_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_75_percent
SC_detected_75_percent$Knapsack[5] <- SC_detected_KNAPSACK_75_percent

## R_vs_C 
a <- load("R_vs_C_WPTs_75_percent_budget_CIdata2009.Rda")
WPTs_R_vs_C_survey_method_75_percent <- WPTs_R_vs_C_75_percent
WPTs_R_vs_C_survey_method_75_percent <- sort(WPTs_R_vs_C_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2009$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_75_percent)){
    if (Evaluation_survey_method_2009$WPT[i] == WPTs_R_vs_C_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2009$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2009$SC_2009))
SC_detected_R_vs_C_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_75_percent
SC_detected_75_percent$R_vs_C[5] <- SC_detected_R_vs_C_survey_75_percent

## POcc
a <- load("Pocc_WPTs_75_percent_budget_CIdata2009.Rda")
WPTs_POCC_survey_method_75_percent <- WPTs_Pocc_75_percent
WPTs_POCC_survey_method_75_percent <- sort(WPTs_POCC_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2009$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_75_percent)){
    if (Evaluation_survey_method_2009$WPT[i] == WPTs_POCC_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2009$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2009$SC_2009))
SC_detected_POCC_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_75_percent
SC_detected_75_percent$Pocc[5] <- SC_detected_POCC_survey_75_percent

## ExpOp
a <- load("ExpOp_WPTs_75_percent_budget_CIdata2009.Rda")
WPTs_EXPOP_survey_method_75_percent <- WPTs_ExpOp_75_percent
WPTs_EXPOP_survey_method_75_percent <- sort(WPTs_EXPOP_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2009$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_75_percent)){
    if (Evaluation_survey_method_2009$WPT[i] == WPTs_EXPOP_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2009$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2009$SC_2009))
SC_detected_EXPOP_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_75_percent
SC_detected_75_percent$ExpOp[5] <- SC_detected_EXPOP_survey_75_percent

## Syst
a <- load("Syst_WPTs_75_percent_budget_CIdata2009.Rda")
WPTs_Syst_survey_method_75_percent <- WPTs_Syst_75_percent
WPTs_Syst_survey_method_75_percent <- sort(WPTs_Syst_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2009$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_75_percent)){
    if (Evaluation_survey_method_2009$WPT[i] == WPTs_Syst_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2009$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2009$SC_2009))
SC_detected_Syst_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_75_percent
SC_detected_75_percent$Syst[5] <- SC_detected_Syst_survey_75_percent

## Random
a <- load("Random_WPTs_75_percent_budget_CIdata2009.Rda")
WPTs_Random_survey_method_75_percent <- WPTs_Random_75_percent
WPTs_Random_survey_method_75_percent <- sort(WPTs_Random_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2009$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_75_percent)){
    if (Evaluation_survey_method_2009$WPT[i] == WPTs_Random_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2009$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2009$SC_2009))
SC_detected_Random_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_75_percent
SC_detected_75_percent$Random[5] <- SC_detected_Random_survey_75_percent


### 2011
## Knapsack 
a <- load("WPTs_for_KNAPSACK_CIdata2011_75_percent_budget.Rda")
WPTs_75_percent_KNAPSACK <- sort(WPTs_75_percent_KNAPSACK)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2011$WPT)) {
  for (j in 1:length(WPTs_75_percent_KNAPSACK)){
    if (Evaluation_survey_method_2011$WPT[i] == WPTs_75_percent_KNAPSACK[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2011$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2011$SC_2011))
SC_detected_KNAPSACK_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_75_percent
SC_detected_75_percent$Knapsack[6] <- SC_detected_KNAPSACK_75_percent

## R_vs_C 
a <- load("R_vs_C_WPTs_75_percent_budget_CIdata2011.Rda")
WPTs_R_vs_C_survey_method_75_percent <- WPTs_R_vs_C_75_percent
WPTs_R_vs_C_survey_method_75_percent <- sort(WPTs_R_vs_C_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2011$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_75_percent)){
    if (Evaluation_survey_method_2011$WPT[i] == WPTs_R_vs_C_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2011$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2011$SC_2011))
SC_detected_R_vs_C_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_75_percent
SC_detected_75_percent$R_vs_C[6] <- SC_detected_R_vs_C_survey_75_percent

## POcc
a <- load("Pocc_WPTs_75_percent_budget_CIdata2011.Rda")
WPTs_POCC_survey_method_75_percent <- WPTs_Pocc_75_percent
WPTs_POCC_survey_method_75_percent <- sort(WPTs_POCC_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2011$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_75_percent)){
    if (Evaluation_survey_method_2011$WPT[i] == WPTs_POCC_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2011$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2011$SC_2011))
SC_detected_POCC_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_75_percent
SC_detected_75_percent$Pocc[6] <- SC_detected_POCC_survey_75_percent

## ExpOp
a <- load("ExpOp_WPTs_75_percent_budget_CIdata2011.Rda")
WPTs_EXPOP_survey_method_75_percent <- WPTs_ExpOp_75_percent
WPTs_EXPOP_survey_method_75_percent <- sort(WPTs_EXPOP_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2011$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_75_percent)){
    if (Evaluation_survey_method_2011$WPT[i] == WPTs_EXPOP_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2011$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2011$SC_2011))
SC_detected_EXPOP_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_75_percent
SC_detected_75_percent$ExpOp[6] <- SC_detected_EXPOP_survey_75_percent

## Syst
a <- load("Syst_WPTs_75_percent_budget_CIdata2011.Rda")
WPTs_Syst_survey_method_75_percent <- WPTs_Syst_75_percent
WPTs_Syst_survey_method_75_percent <- sort(WPTs_Syst_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2011$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_75_percent)){
    if (Evaluation_survey_method_2011$WPT[i] == WPTs_Syst_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2011$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2011$SC_2011))
SC_detected_Syst_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_75_percent
SC_detected_75_percent$Syst[6] <- SC_detected_Syst_survey_75_percent

## Random
a <- load("Random_WPTs_75_percent_budget_CIdata2011.Rda")
WPTs_Random_survey_method_75_percent <- WPTs_Random_75_percent
WPTs_Random_survey_method_75_percent <- sort(WPTs_Random_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2011$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_75_percent)){
    if (Evaluation_survey_method_2011$WPT[i] == WPTs_Random_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2011$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2011$SC_2011))
SC_detected_Random_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_75_percent
SC_detected_75_percent$Random[6] <- SC_detected_Random_survey_75_percent


### 2013
## Knapsack 
a <- load("WPTs_for_KNAPSACK_CIdata2013_75_percent_budget.Rda")
WPTs_75_percent_KNAPSACK <- sort(WPTs_75_percent_KNAPSACK)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2013$WPT)) {
  for (j in 1:length(WPTs_75_percent_KNAPSACK)){
    if (Evaluation_survey_method_2013$WPT[i] == WPTs_75_percent_KNAPSACK[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2013$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2013$SC_2013))
SC_detected_KNAPSACK_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_75_percent
SC_detected_75_percent$Knapsack[7] <- SC_detected_KNAPSACK_75_percent

## R_vs_C 
a <- load("R_vs_C_WPTs_75_percent_budget_CIdata2013.Rda")
WPTs_R_vs_C_survey_method_75_percent <- WPTs_R_vs_C_75_percent
WPTs_R_vs_C_survey_method_75_percent <- sort(WPTs_R_vs_C_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2013$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_75_percent)){
    if (Evaluation_survey_method_2013$WPT[i] == WPTs_R_vs_C_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2013$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2013$SC_2013))
SC_detected_R_vs_C_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_75_percent
SC_detected_75_percent$R_vs_C[7] <- SC_detected_R_vs_C_survey_75_percent

## POcc
a <- load("Pocc_WPTs_75_percent_budget_CIdata2013.Rda")
WPTs_POCC_survey_method_75_percent <- WPTs_Pocc_75_percent
WPTs_POCC_survey_method_75_percent <- sort(WPTs_POCC_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2013$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_75_percent)){
    if (Evaluation_survey_method_2013$WPT[i] == WPTs_POCC_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2013$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2013$SC_2013))
SC_detected_POCC_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_75_percent
SC_detected_75_percent$Pocc[7] <- SC_detected_POCC_survey_75_percent

## ExpOp
a <- load("ExpOp_WPTs_75_percent_budget_CIdata2013.Rda")
WPTs_EXPOP_survey_method_75_percent <- WPTs_ExpOp_75_percent
WPTs_EXPOP_survey_method_75_percent <- sort(WPTs_EXPOP_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2013$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_75_percent)){
    if (Evaluation_survey_method_2013$WPT[i] == WPTs_EXPOP_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2013$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2013$SC_2013))
SC_detected_EXPOP_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_75_percent
SC_detected_75_percent$ExpOp[7] <- SC_detected_EXPOP_survey_75_percent

## Syst
a <- load("Syst_WPTs_75_percent_budget_CIdata2013.Rda")
WPTs_Syst_survey_method_75_percent <- WPTs_Syst_75_percent
WPTs_Syst_survey_method_75_percent <- sort(WPTs_Syst_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2013$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_75_percent)){
    if (Evaluation_survey_method_2013$WPT[i] == WPTs_Syst_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2013$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2013$SC_2013))
SC_detected_Syst_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_75_percent
SC_detected_75_percent$Syst[7] <- SC_detected_Syst_survey_75_percent

## Random
a <- load("Random_WPTs_75_percent_budget_CIdata2013.Rda")
WPTs_Random_survey_method_75_percent <- WPTs_Random_75_percent
WPTs_Random_survey_method_75_percent <- sort(WPTs_Random_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2013$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_75_percent)){
    if (Evaluation_survey_method_2013$WPT[i] == WPTs_Random_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2013$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2013$SC_2013))
SC_detected_Random_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_75_percent
SC_detected_75_percent$Random[7] <- SC_detected_Random_survey_75_percent



### 2015
## Knapsack 
a <- load("WPTs_for_KNAPSACK_CIdata2015_75_percent_budget.Rda")
WPTs_75_percent_KNAPSACK <- sort(WPTs_75_percent_KNAPSACK)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2015$WPT)) {
  for (j in 1:length(WPTs_75_percent_KNAPSACK)){
    if (Evaluation_survey_method_2015$WPT[i] == WPTs_75_percent_KNAPSACK[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2015$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2015$SC_2015))
SC_detected_KNAPSACK_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_75_percent
SC_detected_75_percent$Knapsack[8] <- SC_detected_KNAPSACK_75_percent

## R_vs_C 
a <- load("R_vs_C_WPTs_75_percent_budget_CIdata2015.Rda")
WPTs_R_vs_C_survey_method_75_percent <- WPTs_R_vs_C_75_percent
WPTs_R_vs_C_survey_method_75_percent <- sort(WPTs_R_vs_C_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2015$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_75_percent)){
    if (Evaluation_survey_method_2015$WPT[i] == WPTs_R_vs_C_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2015$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2015$SC_2015))
SC_detected_R_vs_C_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_75_percent
SC_detected_75_percent$R_vs_C[8] <- SC_detected_R_vs_C_survey_75_percent

## POcc
a <- load("Pocc_WPTs_75_percent_budget_CIdata2015.Rda")
WPTs_POCC_survey_method_75_percent <- WPTs_Pocc_75_percent
WPTs_POCC_survey_method_75_percent <- sort(WPTs_POCC_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2015$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_75_percent)){
    if (Evaluation_survey_method_2015$WPT[i] == WPTs_POCC_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2015$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2015$SC_2015))
SC_detected_POCC_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_75_percent
SC_detected_75_percent$Pocc[8] <- SC_detected_POCC_survey_75_percent

## ExpOp
a <- load("ExpOp_WPTs_75_percent_budget_CIdata2015.Rda")
WPTs_EXPOP_survey_method_75_percent <- WPTs_ExpOp_75_percent
WPTs_EXPOP_survey_method_75_percent <- sort(WPTs_EXPOP_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2015$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_75_percent)){
    if (Evaluation_survey_method_2015$WPT[i] == WPTs_EXPOP_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2015$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2015$SC_2015))
SC_detected_EXPOP_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_75_percent
SC_detected_75_percent$ExpOp[8] <- SC_detected_EXPOP_survey_75_percent

## Syst
a <- load("Syst_WPTs_75_percent_budget_CIdata2015.Rda")
WPTs_Syst_survey_method_75_percent <- WPTs_Syst_75_percent
WPTs_Syst_survey_method_75_percent <- sort(WPTs_Syst_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2015$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_75_percent)){
    if (Evaluation_survey_method_2015$WPT[i] == WPTs_Syst_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2015$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2015$SC_2015))
SC_detected_Syst_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_75_percent
SC_detected_75_percent$Syst[8] <- SC_detected_Syst_survey_75_percent

## Random
a <- load("Random_WPTs_75_percent_budget_CIdata2015.Rda")
WPTs_Random_survey_method_75_percent <- WPTs_Random_75_percent
WPTs_Random_survey_method_75_percent <- sort(WPTs_Random_survey_method_75_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2015$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_75_percent)){
    if (Evaluation_survey_method_2015$WPT[i] == WPTs_Random_survey_method_75_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2015$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2015$SC_2015))
SC_detected_Random_survey_75_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_75_percent
SC_detected_75_percent$Random[8] <- SC_detected_Random_survey_75_percent





# Save and plot results 
SC_detected_25_percent
SC_detected_50_percent
SC_detected_75_percent
save(SC_detected_25_percent, file="SC_detected_25_percent_REAL_COST.Rda")
save(SC_detected_50_percent, file="SC_detected_50_percent_REAL_COST.Rda")
save(SC_detected_75_percent, file="SC_detected_75_percent_REAL_COST.Rda")

# Plot separate performance for each multiyear dataset 
par(mfrow=c(2,2))
x <- 1:length(survey_strategies)
plot(x, SC_detected_25_percent[1, ], ylim=c(0,1), pch=16, xlab = "Survey strategies")
plot(x, SC_detected_25_percent[2, ], ylim=c(0,1), pch=14, xlab = "Survey strategies")
plot(x, SC_detected_25_percent[3, ], ylim=c(0,1), pch=12, xlab = "Survey strategies")
plot(x, SC_detected_25_percent[4, ], ylim=c(0,1), pch=10, xlab = "Survey strategies")
plot(x, SC_detected_25_percent[5, ], ylim=c(0,1), pch=8, xlab = "Survey strategies")
plot(x, SC_detected_25_percent[6, ], ylim=c(0,1), pch=6, xlab = "Survey strategies")
plot(x, SC_detected_25_percent[7, ], ylim=c(0,1), pch=4, xlab = "Survey strategies")
plot(x, SC_detected_25_percent[8, ], ylim=c(0,1), pch=2, xlab = "Survey strategies")

par(mfrow=c(2,2))
x <- 1:length(survey_strategies)
plot(x, SC_detected_50_percent[1, ], ylim=c(0,1), pch=16, xlab = "Survey strategies")
plot(x, SC_detected_50_percent[2, ], ylim=c(0,1), pch=14, xlab = "Survey strategies")
plot(x, SC_detected_50_percent[3, ], ylim=c(0,1), pch=12, xlab = "Survey strategies")
plot(x, SC_detected_50_percent[4, ], ylim=c(0,1), pch=10, xlab = "Survey strategies")
plot(x, SC_detected_50_percent[5, ], ylim=c(0,1), pch=8, xlab = "Survey strategies")
plot(x, SC_detected_50_percent[6, ], ylim=c(0,1), pch=6, xlab = "Survey strategies")
plot(x, SC_detected_50_percent[7, ], ylim=c(0,1), pch=4, xlab = "Survey strategies")
plot(x, SC_detected_50_percent[8, ], ylim=c(0,1), pch=2, xlab = "Survey strategies")

par(mfrow=c(2,2))
x <- 1:length(survey_strategies)
plot(x, SC_detected_75_percent[1, ], ylim=c(0,1), pch=16, xlab = "Survey strategies")
plot(x, SC_detected_75_percent[2, ], ylim=c(0,1), pch=14, xlab = "Survey strategies")
plot(x, SC_detected_75_percent[3, ], ylim=c(0,1), pch=12, xlab = "Survey strategies")
plot(x, SC_detected_75_percent[4, ], ylim=c(0,1), pch=10, xlab = "Survey strategies")
plot(x, SC_detected_75_percent[5, ], ylim=c(0,1), pch=8, xlab = "Survey strategies")
plot(x, SC_detected_75_percent[6, ], ylim=c(0,1), pch=6, xlab = "Survey strategies")
plot(x, SC_detected_75_percent[7, ], ylim=c(0,1), pch=4, xlab = "Survey strategies")
plot(x, SC_detected_75_percent[8, ], ylim=c(0,1), pch=2, xlab = "Survey strategies")

# Calculate mean for 25%
mean_knapsack_25_percent <- mean(SC_detected_25_percent$Knapsack)
mean_R_vs_C_25_percent <- mean(SC_detected_25_percent$R_vs_C)
mean_Pocc_25_percent <- mean(SC_detected_25_percent$Pocc)
mean_ExpOp_25_percent <- mean(SC_detected_25_percent$ExpOp)
mean_Syst_25_percent <- mean(SC_detected_25_percent$Syst)
mean_Random_25_percent <- mean(SC_detected_25_percent$Random)
averages_25_percent <- c(mean_knapsack_25_percent, mean_R_vs_C_25_percent, mean_Pocc_25_percent, mean_ExpOp_25_percent, mean_Syst_25_percent, mean_Random_25_percent)

# Calculate mean for 50%
mean_knapsack_50_percent <- mean(SC_detected_50_percent$Knapsack)
mean_R_vs_C_50_percent <- mean(SC_detected_50_percent$R_vs_C)
mean_Pocc_50_percent <- mean(SC_detected_50_percent$Pocc)
mean_ExpOp_50_percent <- mean(SC_detected_50_percent$ExpOp)
mean_Syst_50_percent <- mean(SC_detected_50_percent$Syst)
mean_Random_50_percent <- mean(SC_detected_50_percent$Random)
averages_50_percent <- c(mean_knapsack_50_percent, mean_R_vs_C_50_percent, mean_Pocc_50_percent, mean_ExpOp_50_percent, mean_Syst_50_percent, mean_Random_50_percent)

# Calculate mean for 75%
mean_knapsack_75_percent <- mean(SC_detected_75_percent$Knapsack)
mean_R_vs_C_75_percent <- mean(SC_detected_75_percent$R_vs_C)
mean_Pocc_75_percent <- mean(SC_detected_75_percent$Pocc)
mean_ExpOp_75_percent <- mean(SC_detected_75_percent$ExpOp)
mean_Syst_75_percent <- mean(SC_detected_75_percent$Syst)
mean_Random_75_percent <- mean(SC_detected_75_percent$Random)
averages_75_percent <- c(mean_knapsack_75_percent, mean_R_vs_C_75_percent, mean_Pocc_75_percent, mean_ExpOp_75_percent, mean_Syst_75_percent, mean_Random_75_percent)

# Plot average performance over different years 
x <- 1:length(survey_strategies)
plot(x, averages_25_percent, ylim=c(0,1), pch=16, xlab = "Survey strategies")

x <- 1:length(survey_strategies)
plot(x, averages_50_percent, ylim=c(0,1), pch=16, xlab = "Survey strategies")

x <- 1:length(survey_strategies)
plot(x, averages_75_percent, ylim=c(0,1), pch=16, xlab = "Survey strategies")











##### Archive: calculate resuls for 100% (not needed) - and plot with error bars - this is not the way to go ##############################

#### 100%
### 2001
## R_vs_C 
a <- load("R_vs_C_WPTs_100_percent_budget_CIdata2001.Rda")
WPTs_R_vs_C_survey_method_100_percent <- WPTs_R_vs_C_100_percent
WPTs_R_vs_C_survey_method_100_percent <- sort(WPTs_R_vs_C_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2001$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_100_percent)){
    if (Evaluation_survey_method_2001$WPT[i] == WPTs_R_vs_C_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2001$SC_2001[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2001$SC_2001))
SC_detected_R_vs_C_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_100_percent
SC_detected_100_percent$R_vs_C[1] <- SC_detected_R_vs_C_survey_100_percent

## POcc
a <- load("Pocc_WPTs_100_percent_budget_CIdata2001.Rda")
WPTs_POCC_survey_method_100_percent <- WPTs_Pocc_100_percent
WPTs_POCC_survey_method_100_percent <- sort(WPTs_POCC_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2001$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_100_percent)){
    if (Evaluation_survey_method_2001$WPT[i] == WPTs_POCC_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2001$SC_2001[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2001$SC_2001))
SC_detected_POCC_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_100_percent
SC_detected_100_percent$Pocc[1] <- SC_detected_POCC_survey_100_percent

## ExpOp
a <- load("ExpOp_WPTs_100_percent_budget_CIdata2001.Rda")
WPTs_EXPOP_survey_method_100_percent <- WPTs_ExpOp_100_percent
WPTs_EXPOP_survey_method_100_percent <- sort(WPTs_EXPOP_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2001$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_100_percent)){
    if (Evaluation_survey_method_2001$WPT[i] == WPTs_EXPOP_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2001$SC_2001[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2001$SC_2001))
SC_detected_EXPOP_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_100_percent
SC_detected_100_percent$ExpOp[1] <- SC_detected_EXPOP_survey_100_percent

## Syst
a <- load("Syst_WPTs_100_percent_budget_CIdata2001.Rda")
WPTs_Syst_survey_method_100_percent <- WPTs_Syst_100_percent
WPTs_Syst_survey_method_100_percent <- sort(WPTs_Syst_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2001$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_100_percent)){
    if (Evaluation_survey_method_2001$WPT[i] == WPTs_Syst_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2001$SC_2001[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2001$SC_2001))
SC_detected_Syst_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_100_percent
SC_detected_100_percent$Syst[1] <- SC_detected_Syst_survey_100_percent

## Random
a <- load("Random_WPTs_100_percent_budget_CIdata2001.Rda")
WPTs_Random_survey_method_100_percent <- WPTs_Random_100_percent
WPTs_Random_survey_method_100_percent <- sort(WPTs_Random_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2001$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_100_percent)){
    if (Evaluation_survey_method_2001$WPT[i] == WPTs_Random_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2001$SC_2001[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2001$SC_2001))
SC_detected_Random_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_100_percent
SC_detected_100_percent$Random[1] <- SC_detected_Random_survey_100_percent

### 2003
## R_vs_C 
a <- load("R_vs_C_WPTs_100_percent_budget_CIdata2003.Rda")
WPTs_R_vs_C_survey_method_100_percent <- WPTs_R_vs_C_100_percent
WPTs_R_vs_C_survey_method_100_percent <- sort(WPTs_R_vs_C_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2003$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_100_percent)){
    if (Evaluation_survey_method_2003$WPT[i] == WPTs_R_vs_C_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2003$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2003$SC_2003))
SC_detected_R_vs_C_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_100_percent
SC_detected_100_percent$R_vs_C[2] <- SC_detected_R_vs_C_survey_100_percent

## POcc
a <- load("Pocc_WPTs_100_percent_budget_CIdata2003.Rda")
WPTs_POCC_survey_method_100_percent <- WPTs_Pocc_100_percent
WPTs_POCC_survey_method_100_percent <- sort(WPTs_POCC_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2003$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_100_percent)){
    if (Evaluation_survey_method_2003$WPT[i] == WPTs_POCC_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2003$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2003$SC_2003))
SC_detected_POCC_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_100_percent
SC_detected_100_percent$Pocc[2] <- SC_detected_POCC_survey_100_percent

## ExpOp
a <- load("ExpOp_WPTs_100_percent_budget_CIdata2003.Rda")
WPTs_EXPOP_survey_method_100_percent <- WPTs_ExpOp_100_percent
WPTs_EXPOP_survey_method_100_percent <- sort(WPTs_EXPOP_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2003$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_100_percent)){
    if (Evaluation_survey_method_2003$WPT[i] == WPTs_EXPOP_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2003$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2003$SC_2003))
SC_detected_EXPOP_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_100_percent
SC_detected_100_percent$ExpOp[2] <- SC_detected_EXPOP_survey_100_percent

## Syst
a <- load("Syst_WPTs_100_percent_budget_CIdata2003.Rda")
WPTs_Syst_survey_method_100_percent <- WPTs_Syst_100_percent
WPTs_Syst_survey_method_100_percent <- sort(WPTs_Syst_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2003$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_100_percent)){
    if (Evaluation_survey_method_2003$WPT[i] == WPTs_Syst_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2003$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2003$SC_2003))
SC_detected_Syst_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_100_percent
SC_detected_100_percent$Syst[2] <- SC_detected_Syst_survey_100_percent

## Random
a <- load("Random_WPTs_100_percent_budget_CIdata2003.Rda")
WPTs_Random_survey_method_100_percent <- WPTs_Random_100_percent
WPTs_Random_survey_method_100_percent <- sort(WPTs_Random_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2003$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_100_percent)){
    if (Evaluation_survey_method_2003$WPT[i] == WPTs_Random_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2003$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2003$SC_2003))
SC_detected_Random_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_100_percent
SC_detected_100_percent$Random[2] <- SC_detected_Random_survey_100_percent

### 2005
## R_vs_C 
a <- load("R_vs_C_WPTs_100_percent_budget_CIdata2005.Rda")
WPTs_R_vs_C_survey_method_100_percent <- WPTs_R_vs_C_100_percent
WPTs_R_vs_C_survey_method_100_percent <- sort(WPTs_R_vs_C_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2005$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_100_percent)){
    if (Evaluation_survey_method_2005$WPT[i] == WPTs_R_vs_C_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2005$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2005$SC_2005))
SC_detected_R_vs_C_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_100_percent
SC_detected_100_percent$R_vs_C[3] <- SC_detected_R_vs_C_survey_100_percent

## POcc
a <- load("Pocc_WPTs_100_percent_budget_CIdata2005.Rda")
WPTs_POCC_survey_method_100_percent <- WPTs_Pocc_100_percent
WPTs_POCC_survey_method_100_percent <- sort(WPTs_POCC_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2005$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_100_percent)){
    if (Evaluation_survey_method_2005$WPT[i] == WPTs_POCC_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2005$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2005$SC_2005))
SC_detected_POCC_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_100_percent
SC_detected_100_percent$Pocc[3] <- SC_detected_POCC_survey_100_percent

## ExpOp
a <- load("ExpOp_WPTs_100_percent_budget_CIdata2005.Rda")
WPTs_EXPOP_survey_method_100_percent <- WPTs_ExpOp_100_percent
WPTs_EXPOP_survey_method_100_percent <- sort(WPTs_EXPOP_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2005$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_100_percent)){
    if (Evaluation_survey_method_2005$WPT[i] == WPTs_EXPOP_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2005$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2005$SC_2005))
SC_detected_EXPOP_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_100_percent
SC_detected_100_percent$ExpOp[3] <- SC_detected_EXPOP_survey_100_percent

## Syst
a <- load("Syst_WPTs_100_percent_budget_CIdata2005.Rda")
WPTs_Syst_survey_method_100_percent <- WPTs_Syst_100_percent
WPTs_Syst_survey_method_100_percent <- sort(WPTs_Syst_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2005$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_100_percent)){
    if (Evaluation_survey_method_2005$WPT[i] == WPTs_Syst_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2005$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2005$SC_2005))
SC_detected_Syst_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_100_percent
SC_detected_100_percent$Syst[3] <- SC_detected_Syst_survey_100_percent

## Random
a <- load("Random_WPTs_100_percent_budget_CIdata2005.Rda")
WPTs_Random_survey_method_100_percent <- WPTs_Random_100_percent
WPTs_Random_survey_method_100_percent <- sort(WPTs_Random_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2005$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_100_percent)){
    if (Evaluation_survey_method_2005$WPT[i] == WPTs_Random_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2005$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2005$SC_2005))
SC_detected_Random_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_100_percent
SC_detected_100_percent$Random[3] <- SC_detected_Random_survey_100_percent

### 2007
## R_vs_C 
a <- load("R_vs_C_WPTs_100_percent_budget_CIdata2007.Rda")
WPTs_R_vs_C_survey_method_100_percent <- WPTs_R_vs_C_100_percent
WPTs_R_vs_C_survey_method_100_percent <- sort(WPTs_R_vs_C_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2007$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_100_percent)){
    if (Evaluation_survey_method_2007$WPT[i] == WPTs_R_vs_C_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2007$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2007$SC_2007))
SC_detected_R_vs_C_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_100_percent
SC_detected_100_percent$R_vs_C[4] <- SC_detected_R_vs_C_survey_100_percent

## POcc
a <- load("Pocc_WPTs_100_percent_budget_CIdata2007.Rda")
WPTs_POCC_survey_method_100_percent <- WPTs_Pocc_100_percent
WPTs_POCC_survey_method_100_percent <- sort(WPTs_POCC_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2007$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_100_percent)){
    if (Evaluation_survey_method_2007$WPT[i] == WPTs_POCC_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2007$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2007$SC_2007))
SC_detected_POCC_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_100_percent
SC_detected_100_percent$Pocc[4] <- SC_detected_POCC_survey_100_percent

## ExpOp
a <- load("ExpOp_WPTs_100_percent_budget_CIdata2007.Rda")
WPTs_EXPOP_survey_method_100_percent <- WPTs_ExpOp_100_percent
WPTs_EXPOP_survey_method_100_percent <- sort(WPTs_EXPOP_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2007$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_100_percent)){
    if (Evaluation_survey_method_2007$WPT[i] == WPTs_EXPOP_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2007$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2007$SC_2007))
SC_detected_EXPOP_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_100_percent
SC_detected_100_percent$ExpOp[4] <- SC_detected_EXPOP_survey_100_percent

## Syst
a <- load("Syst_WPTs_100_percent_budget_CIdata2007.Rda")
WPTs_Syst_survey_method_100_percent <- WPTs_Syst_100_percent
WPTs_Syst_survey_method_100_percent <- sort(WPTs_Syst_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2007$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_100_percent)){
    if (Evaluation_survey_method_2007$WPT[i] == WPTs_Syst_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2007$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2007$SC_2007))
SC_detected_Syst_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_100_percent
SC_detected_100_percent$Syst[4] <- SC_detected_Syst_survey_100_percent

## Random
a <- load("Random_WPTs_100_percent_budget_CIdata2007.Rda")
WPTs_Random_survey_method_100_percent <- WPTs_Random_100_percent
WPTs_Random_survey_method_100_percent <- sort(WPTs_Random_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2007$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_100_percent)){
    if (Evaluation_survey_method_2007$WPT[i] == WPTs_Random_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2007$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2007$SC_2007))
SC_detected_Random_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_100_percent
SC_detected_100_percent$Random[4] <- SC_detected_Random_survey_100_percent


### 2009
## R_vs_C 
a <- load("R_vs_C_WPTs_100_percent_budget_CIdata2009.Rda")
WPTs_R_vs_C_survey_method_100_percent <- WPTs_R_vs_C_100_percent
WPTs_R_vs_C_survey_method_100_percent <- sort(WPTs_R_vs_C_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2009$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_100_percent)){
    if (Evaluation_survey_method_2009$WPT[i] == WPTs_R_vs_C_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2009$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2009$SC_2009))
SC_detected_R_vs_C_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_100_percent
SC_detected_100_percent$R_vs_C[5] <- SC_detected_R_vs_C_survey_100_percent

## POcc
a <- load("Pocc_WPTs_100_percent_budget_CIdata2009.Rda")
WPTs_POCC_survey_method_100_percent <- WPTs_Pocc_100_percent
WPTs_POCC_survey_method_100_percent <- sort(WPTs_POCC_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2009$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_100_percent)){
    if (Evaluation_survey_method_2009$WPT[i] == WPTs_POCC_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2009$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2009$SC_2009))
SC_detected_POCC_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_100_percent
SC_detected_100_percent$Pocc[5] <- SC_detected_POCC_survey_100_percent

## ExpOp
a <- load("ExpOp_WPTs_100_percent_budget_CIdata2009.Rda")
WPTs_EXPOP_survey_method_100_percent <- WPTs_ExpOp_100_percent
WPTs_EXPOP_survey_method_100_percent <- sort(WPTs_EXPOP_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2009$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_100_percent)){
    if (Evaluation_survey_method_2009$WPT[i] == WPTs_EXPOP_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2009$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2009$SC_2009))
SC_detected_EXPOP_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_100_percent
SC_detected_100_percent$ExpOp[5] <- SC_detected_EXPOP_survey_100_percent

## Syst
a <- load("Syst_WPTs_100_percent_budget_CIdata2009.Rda")
WPTs_Syst_survey_method_100_percent <- WPTs_Syst_100_percent
WPTs_Syst_survey_method_100_percent <- sort(WPTs_Syst_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2009$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_100_percent)){
    if (Evaluation_survey_method_2009$WPT[i] == WPTs_Syst_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2009$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2009$SC_2009))
SC_detected_Syst_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_100_percent
SC_detected_100_percent$Syst[5] <- SC_detected_Syst_survey_100_percent

## Random
a <- load("Random_WPTs_100_percent_budget_CIdata2009.Rda")
WPTs_Random_survey_method_100_percent <- WPTs_Random_100_percent
WPTs_Random_survey_method_100_percent <- sort(WPTs_Random_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2009$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_100_percent)){
    if (Evaluation_survey_method_2009$WPT[i] == WPTs_Random_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2009$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2009$SC_2009))
SC_detected_Random_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_100_percent
SC_detected_100_percent$Random[5] <- SC_detected_Random_survey_100_percent


### 2011
## R_vs_C 
a <- load("R_vs_C_WPTs_100_percent_budget_CIdata2011.Rda")
WPTs_R_vs_C_survey_method_100_percent <- WPTs_R_vs_C_100_percent
WPTs_R_vs_C_survey_method_100_percent <- sort(WPTs_R_vs_C_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2011$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_100_percent)){
    if (Evaluation_survey_method_2011$WPT[i] == WPTs_R_vs_C_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2011$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2011$SC_2011))
SC_detected_R_vs_C_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_100_percent
SC_detected_100_percent$R_vs_C[6] <- SC_detected_R_vs_C_survey_100_percent

## POcc
a <- load("Pocc_WPTs_100_percent_budget_CIdata2011.Rda")
WPTs_POCC_survey_method_100_percent <- WPTs_Pocc_100_percent
WPTs_POCC_survey_method_100_percent <- sort(WPTs_POCC_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2011$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_100_percent)){
    if (Evaluation_survey_method_2011$WPT[i] == WPTs_POCC_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2011$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2011$SC_2011))
SC_detected_POCC_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_100_percent
SC_detected_100_percent$Pocc[6] <- SC_detected_POCC_survey_100_percent

## ExpOp
a <- load("ExpOp_WPTs_100_percent_budget_CIdata2011.Rda")
WPTs_EXPOP_survey_method_100_percent <- WPTs_ExpOp_100_percent
WPTs_EXPOP_survey_method_100_percent <- sort(WPTs_EXPOP_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2011$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_100_percent)){
    if (Evaluation_survey_method_2011$WPT[i] == WPTs_EXPOP_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2011$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2011$SC_2011))
SC_detected_EXPOP_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_100_percent
SC_detected_100_percent$ExpOp[6] <- SC_detected_EXPOP_survey_100_percent

## Syst
a <- load("Syst_WPTs_100_percent_budget_CIdata2011.Rda")
WPTs_Syst_survey_method_100_percent <- WPTs_Syst_100_percent
WPTs_Syst_survey_method_100_percent <- sort(WPTs_Syst_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2011$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_100_percent)){
    if (Evaluation_survey_method_2011$WPT[i] == WPTs_Syst_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2011$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2011$SC_2011))
SC_detected_Syst_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_100_percent
SC_detected_100_percent$Syst[6] <- SC_detected_Syst_survey_100_percent

## Random
a <- load("Random_WPTs_100_percent_budget_CIdata2011.Rda")
WPTs_Random_survey_method_100_percent <- WPTs_Random_100_percent
WPTs_Random_survey_method_100_percent <- sort(WPTs_Random_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2011$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_100_percent)){
    if (Evaluation_survey_method_2011$WPT[i] == WPTs_Random_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2011$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2011$SC_2011))
SC_detected_Random_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_100_percent
SC_detected_100_percent$Random[6] <- SC_detected_Random_survey_100_percent


### 2013
## R_vs_C 
a <- load("R_vs_C_WPTs_100_percent_budget_CIdata2013.Rda")
WPTs_R_vs_C_survey_method_100_percent <- WPTs_R_vs_C_100_percent
WPTs_R_vs_C_survey_method_100_percent <- sort(WPTs_R_vs_C_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2013$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_100_percent)){
    if (Evaluation_survey_method_2013$WPT[i] == WPTs_R_vs_C_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2013$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2013$SC_2013))
SC_detected_R_vs_C_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_100_percent
SC_detected_100_percent$R_vs_C[7] <- SC_detected_R_vs_C_survey_100_percent

## POcc
a <- load("Pocc_WPTs_100_percent_budget_CIdata2013.Rda")
WPTs_POCC_survey_method_100_percent <- WPTs_Pocc_100_percent
WPTs_POCC_survey_method_100_percent <- sort(WPTs_POCC_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2013$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_100_percent)){
    if (Evaluation_survey_method_2013$WPT[i] == WPTs_POCC_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2013$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2013$SC_2013))
SC_detected_POCC_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_100_percent
SC_detected_100_percent$Pocc[7] <- SC_detected_POCC_survey_100_percent

## ExpOp
a <- load("ExpOp_WPTs_100_percent_budget_CIdata2013.Rda")
WPTs_EXPOP_survey_method_100_percent <- WPTs_ExpOp_100_percent
WPTs_EXPOP_survey_method_100_percent <- sort(WPTs_EXPOP_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2013$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_100_percent)){
    if (Evaluation_survey_method_2013$WPT[i] == WPTs_EXPOP_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2013$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2013$SC_2013))
SC_detected_EXPOP_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_100_percent
SC_detected_100_percent$ExpOp[7] <- SC_detected_EXPOP_survey_100_percent

## Syst
a <- load("Syst_WPTs_100_percent_budget_CIdata2013.Rda")
WPTs_Syst_survey_method_100_percent <- WPTs_Syst_100_percent
WPTs_Syst_survey_method_100_percent <- sort(WPTs_Syst_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2013$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_100_percent)){
    if (Evaluation_survey_method_2013$WPT[i] == WPTs_Syst_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2013$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2013$SC_2013))
SC_detected_Syst_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_100_percent
SC_detected_100_percent$Syst[7] <- SC_detected_Syst_survey_100_percent

## Random
a <- load("Random_WPTs_100_percent_budget_CIdata2013.Rda")
WPTs_Random_survey_method_100_percent <- WPTs_Random_100_percent
WPTs_Random_survey_method_100_percent <- sort(WPTs_Random_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2013$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_100_percent)){
    if (Evaluation_survey_method_2013$WPT[i] == WPTs_Random_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2013$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2013$SC_2013))
SC_detected_Random_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_100_percent
SC_detected_100_percent$Random[7] <- SC_detected_Random_survey_100_percent



### 2015
## R_vs_C 
a <- load("R_vs_C_WPTs_100_percent_budget_CIdata2015.Rda")
WPTs_R_vs_C_survey_method_100_percent <- WPTs_R_vs_C_100_percent
WPTs_R_vs_C_survey_method_100_percent <- sort(WPTs_R_vs_C_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2015$WPT)) {
  for (j in 1:length(WPTs_R_vs_C_survey_method_100_percent)){
    if (Evaluation_survey_method_2015$WPT[i] == WPTs_R_vs_C_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2015$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2015$SC_2015))
SC_detected_R_vs_C_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_R_vs_C_survey_100_percent
SC_detected_100_percent$R_vs_C[8] <- SC_detected_R_vs_C_survey_100_percent

## POcc
a <- load("Pocc_WPTs_100_percent_budget_CIdata2015.Rda")
WPTs_POCC_survey_method_100_percent <- WPTs_Pocc_100_percent
WPTs_POCC_survey_method_100_percent <- sort(WPTs_POCC_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2015$WPT)) {
  for (j in 1:length(WPTs_POCC_survey_method_100_percent)){
    if (Evaluation_survey_method_2015$WPT[i] == WPTs_POCC_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2015$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2015$SC_2015))
SC_detected_POCC_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_POCC_survey_100_percent
SC_detected_100_percent$Pocc[8] <- SC_detected_POCC_survey_100_percent

## ExpOp
a <- load("ExpOp_WPTs_100_percent_budget_CIdata2015.Rda")
WPTs_EXPOP_survey_method_100_percent <- WPTs_ExpOp_100_percent
WPTs_EXPOP_survey_method_100_percent <- sort(WPTs_EXPOP_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2015$WPT)) {
  for (j in 1:length(WPTs_EXPOP_survey_method_100_percent)){
    if (Evaluation_survey_method_2015$WPT[i] == WPTs_EXPOP_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2015$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2015$SC_2015))
SC_detected_EXPOP_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_EXPOP_survey_100_percent
SC_detected_100_percent$ExpOp[8] <- SC_detected_EXPOP_survey_100_percent

## Syst
a <- load("Syst_WPTs_100_percent_budget_CIdata2015.Rda")
WPTs_Syst_survey_method_100_percent <- WPTs_Syst_100_percent
WPTs_Syst_survey_method_100_percent <- sort(WPTs_Syst_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2015$WPT)) {
  for (j in 1:length(WPTs_Syst_survey_method_100_percent)){
    if (Evaluation_survey_method_2015$WPT[i] == WPTs_Syst_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2015$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2015$SC_2015))
SC_detected_Syst_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Syst_survey_100_percent
SC_detected_100_percent$Syst[8] <- SC_detected_Syst_survey_100_percent

## Random
a <- load("Random_WPTs_100_percent_budget_CIdata2015.Rda")
WPTs_Random_survey_method_100_percent <- WPTs_Random_100_percent
WPTs_Random_survey_method_100_percent <- sort(WPTs_Random_survey_method_100_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method_2015$WPT)) {
  for (j in 1:length(WPTs_Random_survey_method_100_percent)){
    if (Evaluation_survey_method_2015$WPT[i] == WPTs_Random_survey_method_100_percent[j]) 
    {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method_2015$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method_2015$SC_2015))
SC_detected_Random_survey_100_percent <- aa[2, 2]/ll[2, 2]
SC_detected_Random_survey_100_percent
SC_detected_100_percent$Random[8] <- SC_detected_Random_survey_100_percent


# Calculate mean and quantiles for 25%
mean_R_vs_C_25_percent <- mean(SC_detected_25_percent$R_vs_C)
UQ_R_vs_C_25_percent <- quantile(SC_detected_25_percent$R_vs_C, 0.975)
LQ_R_vs_C_25_percent <- quantile(SC_detected_25_percent$R_vs_C, 0.025)

mean_Pocc_25_percent <- mean(SC_detected_25_percent$Pocc)
UQ_Pocc_25_percent <- quantile(SC_detected_25_percent$Pocc, 0.975)
LQ_Pocc_25_percent <- quantile(SC_detected_25_percent$Pocc, 0.025)

mean_ExpOp_25_percent <- mean(SC_detected_25_percent$ExpOp)
UQ_ExpOp_25_percent <- quantile(SC_detected_25_percent$ExpOp, 0.975)
LQ_ExpOp_25_percent <- quantile(SC_detected_25_percent$ExpOp, 0.025)

mean_Syst_25_percent <- mean(SC_detected_25_percent$Syst)
UQ_Syst_25_percent <- quantile(SC_detected_25_percent$Syst, 0.975)
LQ_Syst_25_percent <- quantile(SC_detected_25_percent$Syst, 0.025)

mean_Random_25_percent <- mean(SC_detected_25_percent$Random)
UQ_Random_25_percent <- quantile(SC_detected_25_percent$Random, 0.975)
LQ_Random_25_percent <- quantile(SC_detected_25_percent$Random, 0.025)

averages_25_percent <- c(mean_R_vs_C_25_percent, mean_Pocc_25_percent, mean_ExpOp_25_percent, mean_Syst_25_percent, mean_Random_25_percent)
UQ_25_percent <- c(UQ_R_vs_C_25_percent, UQ_Pocc_25_percent, UQ_ExpOp_25_percent, UQ_Syst_25_percent, UQ_Random_25_percent)
LQ_25_percent <- c(LQ_R_vs_C_25_percent, LQ_Pocc_25_percent, LQ_ExpOp_25_percent, LQ_Syst_25_percent, LQ_Random_25_percent)

# Calculate mean and quantiles for 50%
mean_R_vs_C_50_percent <- mean(SC_detected_50_percent$R_vs_C)
UQ_R_vs_C_50_percent <- quantile(SC_detected_50_percent$R_vs_C, 0.975)
LQ_R_vs_C_50_percent <- quantile(SC_detected_50_percent$R_vs_C, 0.025)

mean_Pocc_50_percent <- mean(SC_detected_50_percent$Pocc)
UQ_Pocc_50_percent <- quantile(SC_detected_50_percent$Pocc, 0.975)
LQ_Pocc_50_percent <- quantile(SC_detected_50_percent$Pocc, 0.025)

mean_ExpOp_50_percent <- mean(SC_detected_50_percent$ExpOp)
UQ_ExpOp_50_percent <- quantile(SC_detected_50_percent$ExpOp, 0.975)
LQ_ExpOp_50_percent <- quantile(SC_detected_50_percent$ExpOp, 0.025)

mean_Syst_50_percent <- mean(SC_detected_50_percent$Syst)
UQ_Syst_50_percent <- quantile(SC_detected_50_percent$Syst, 0.975)
LQ_Syst_50_percent <- quantile(SC_detected_50_percent$Syst, 0.025)

mean_Random_50_percent <- mean(SC_detected_50_percent$Random)
UQ_Random_50_percent <- quantile(SC_detected_50_percent$Random, 0.975)
LQ_Random_50_percent <- quantile(SC_detected_50_percent$Random, 0.025)

averages_50_percent <- c(mean_R_vs_C_50_percent, mean_Pocc_50_percent, mean_ExpOp_50_percent, mean_Syst_50_percent, mean_Random_50_percent)
UQ_50_percent <- c(UQ_R_vs_C_50_percent, UQ_Pocc_50_percent, UQ_ExpOp_50_percent, UQ_Syst_50_percent, UQ_Random_50_percent)
LQ_50_percent <- c(LQ_R_vs_C_50_percent, LQ_Pocc_50_percent, LQ_ExpOp_50_percent, LQ_Syst_50_percent, LQ_Random_50_percent)

# Calculate mean and quantiles for 75%
mean_R_vs_C_75_percent <- mean(SC_detected_75_percent$R_vs_C)
UQ_R_vs_C_75_percent <- quantile(SC_detected_75_percent$R_vs_C, 0.975)
LQ_R_vs_C_75_percent <- quantile(SC_detected_75_percent$R_vs_C, 0.025)

mean_Pocc_75_percent <- mean(SC_detected_75_percent$Pocc)
UQ_Pocc_75_percent <- quantile(SC_detected_75_percent$Pocc, 0.975)
LQ_Pocc_75_percent <- quantile(SC_detected_75_percent$Pocc, 0.025)

mean_ExpOp_75_percent <- mean(SC_detected_75_percent$ExpOp)
UQ_ExpOp_75_percent <- quantile(SC_detected_75_percent$ExpOp, 0.975)
LQ_ExpOp_75_percent <- quantile(SC_detected_75_percent$ExpOp, 0.025)

mean_Syst_75_percent <- mean(SC_detected_75_percent$Syst)
UQ_Syst_75_percent <- quantile(SC_detected_75_percent$Syst, 0.975)
LQ_Syst_75_percent <- quantile(SC_detected_75_percent$Syst, 0.025)

mean_Random_75_percent <- mean(SC_detected_75_percent$Random)
UQ_Random_75_percent <- quantile(SC_detected_75_percent$Random, 0.975)
LQ_Random_75_percent <- quantile(SC_detected_75_percent$Random, 0.025)

averages_75_percent <- c(mean_R_vs_C_75_percent, mean_Pocc_75_percent, mean_ExpOp_75_percent, mean_Syst_75_percent, mean_Random_75_percent)
UQ_75_percent <- c(UQ_R_vs_C_75_percent, UQ_Pocc_75_percent, UQ_ExpOp_75_percent, UQ_Syst_75_percent, UQ_Random_75_percent)
LQ_75_percent <- c(LQ_R_vs_C_75_percent, LQ_Pocc_75_percent, LQ_ExpOp_75_percent, LQ_Syst_75_percent, LQ_Random_75_percent)

# Calculate mean and quantiles for 100%
mean_R_vs_C_100_percent <- mean(SC_detected_100_percent$R_vs_C)
UQ_R_vs_C_100_percent <- quantile(SC_detected_100_percent$R_vs_C, 0.975)
LQ_R_vs_C_100_percent <- quantile(SC_detected_100_percent$R_vs_C, 0.025)

mean_Pocc_100_percent <- mean(SC_detected_100_percent$Pocc)
UQ_Pocc_100_percent <- quantile(SC_detected_100_percent$Pocc, 0.975)
LQ_Pocc_100_percent <- quantile(SC_detected_100_percent$Pocc, 0.025)

mean_ExpOp_100_percent <- mean(SC_detected_100_percent$ExpOp)
UQ_ExpOp_100_percent <- quantile(SC_detected_100_percent$ExpOp, 0.975)
LQ_ExpOp_100_percent <- quantile(SC_detected_100_percent$ExpOp, 0.025)

mean_Syst_100_percent <- mean(SC_detected_100_percent$Syst)
UQ_Syst_100_percent <- quantile(SC_detected_100_percent$Syst, 0.975)
LQ_Syst_100_percent <- quantile(SC_detected_100_percent$Syst, 0.025)

mean_Random_100_percent <- mean(SC_detected_100_percent$Random)
UQ_Random_100_percent <- quantile(SC_detected_100_percent$Random, 0.975)
LQ_Random_100_percent <- quantile(SC_detected_100_percent$Random, 0.025)

averages_100_percent <- c(mean_R_vs_C_100_percent, mean_Pocc_100_percent, mean_ExpOp_100_percent, mean_Syst_100_percent, mean_Random_100_percent)
UQ_100_percent <- c(UQ_R_vs_C_100_percent, UQ_Pocc_100_percent, UQ_ExpOp_100_percent, UQ_Syst_100_percent, UQ_Random_100_percent)
LQ_100_percent <- c(LQ_R_vs_C_100_percent, LQ_Pocc_100_percent, LQ_ExpOp_100_percent, LQ_Syst_100_percent, LQ_Random_100_percent)


# Plot
x <- 1:length(survey_strategies)
plot(x, averages_25_percent, ylim=c(0,1), pch=1, xlab = "Survey strategies")
segments(x, UQ_25_percent, x, LQ_25_percent)

x <- 1:length(survey_strategies)
plot(x, averages_50_percent, ylim=c(0,1), pch=1, xlab = "Survey strategies")
segments(x, UQ_50_percent, x, LQ_50_percent)

x <- 1:length(survey_strategies)
plot(x, averages_75_percent, ylim=c(0,1), pch=1, xlab = "Survey strategies")
segments(x, UQ_75_percent, x, LQ_75_percent)

x <- 1:length(survey_strategies)
plot(x, averages_100_percent, ylim=c(0,1), pch=1, xlab = "Survey strategies")
segments(x, UQ_100_percent, x, LQ_100_percent)

























########################################################## Archive ##########################################################################
# Load required packages
library(raster)  
library(maptools)  
library(rgdal)
library(sp) 
library(rgeos)
library(spatstat)

# Create modelraster
modelraster <- raster(nrow=180,ncol=210)
extent(modelraster) <- c(558000,579000,8831000,8849000)
modelraster[] <- NA
summary(modelraster)

## Read in data from all years 
CIdata <- read.table("CIdata_all_years_with_Predictors_and_Travel_and_Survey_cost.txt", sep="\t", header=TRUE)

################################### Evaluate survey performance using 2001 supercolony distribution ####################################
unique(CIdata$Year)
CIdata2001 <- subset(CIdata,CIdata$Year == 2001)
# Create data.frame with WPT, X-mark, Y-mark, SC_2001 (1 or 0)
Evaluation_survey_method <- as.data.frame(matrix(NA, nrow = nrow(CIdata2001), ncol = 4)) # create data.frame 
Evaluation_survey_method[ , 1] <- CIdata2001[ , "WPT"]
Evaluation_survey_method[ , 2] <- CIdata2001[ , "X_MARK"]
Evaluation_survey_method[ , 3] <- CIdata2001[ , "Y_MARK"]
Evaluation_survey_method[ , 4] <- CIdata2001[ , "p_a_SC"]
colnames(Evaluation_survey_method) <- c("WPT", "X_MARK","Y_MARK","SC_2001")
Evaluation_survey_method
# Create dataframe to store survey performance results
SC_detected_2001 <- as.data.frame(matrix(NA, nrow=4, ncol=5))
rownames(SC_detected_2001) <- c("25% total budget", "50% total budget", "75% total budget", "100% total budget")
colnames(SC_detected_2001) <- c("RANDOM_SURVEY", "SYSTEMATIC_SURVEY", "MANAGERS_OPINION", "PR_OCC", "R_vs_C")

# Random survey 
a <- read.table("CIdata_without_2001_random_sites_25_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2001_random_sites_25_percent.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_25_percent <- a[ ,"WPT"]
WPTs_RANDOM_survey_method_25_percent <- sort(WPTs_RANDOM_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_25_percent)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_25_percent[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2001[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2001))
SC_detected_RANDOM_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_25_percent
SC_detected_2001$RANDOM_SURVEY[1] <- SC_detected_RANDOM_survey_25_percent


b <- read.table("CIdata_without_2001_random_sites_50_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2001_random_sites_50_percent.pdf")
plot(b$X_MARK, b$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_50_percent <- b[ ,"WPT"]
WPTs_RANDOM_survey_method_50_percent <- sort(WPTs_RANDOM_survey_method_50_percent)
SC_vector_b <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_50_percent[j]) {SC_vector_b <- c(SC_vector_b, Evaluation_survey_method$SC_2001[i])}
  }
}
SC_vector_b
bb <- as.data.frame(table(SC_vector_b))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2001))
SC_detected_RANDOM_survey_50_percent <- bb[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_50_percent
SC_detected_2001$RANDOM_SURVEY[2] <- SC_detected_RANDOM_survey_50_percent


c <- read.table("CIdata_without_2001_random_sites_75_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2001_random_sites_75_percent.pdf")
plot(c$X_MARK, c$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_75_percent <- c[ ,"WPT"]
WPTs_RANDOM_survey_method_75_percent <- sort(WPTs_RANDOM_survey_method_75_percent)
SC_vector_c <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_75_percent[j]) {SC_vector_c <- c(SC_vector_c, Evaluation_survey_method$SC_2001[i])}
  }
}
SC_vector_c
cc <- as.data.frame(table(SC_vector_c))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2001))
SC_detected_RANDOM_survey_75_percent <- cc[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_75_percent
SC_detected_2001$RANDOM_SURVEY[3] <- SC_detected_RANDOM_survey_75_percent

d <- read.table("CIdata_without_2001_random_sites_100_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2001_random_sites_100_percent.pdf")
plot(d$X_MARK, d$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_100_percent <- d[ ,"WPT"]
WPTs_RANDOM_survey_method_100_percent <- sort(WPTs_RANDOM_survey_method_100_percent)
SC_vector_d <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_100_percent[j]) {SC_vector_d <- c(SC_vector_d, Evaluation_survey_method$SC_2001[i])}
  }
}
SC_vector_d
dd <- as.data.frame(table(SC_vector_d))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2001))
SC_detected_RANDOM_survey_100_percent <- dd[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_100_percent
SC_detected_2001$RANDOM_SURVEY[4] <- SC_detected_RANDOM_survey_100_percent


SC_detected_2001 # check



# Systematic survey
e <- read.table("CIdata_without_2001_systematic_survey_25_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2001_systematic_survey_25_percent_budget.pdf")
plot(e$X_MARK, e$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_25_percent <- e[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_25_percent <- sort(WPTs_SYSTEMATIC_survey_method_25_percent)
SC_vector_e <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_25_percent[j]) {SC_vector_e <- c(SC_vector_e, Evaluation_survey_method$SC_2001[i])}
  }
}
SC_vector_e
ee <- as.data.frame(table(SC_vector_e))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2001))
SC_detected_SYSTEMATIC_survey_25_percent <- ee[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_25_percent
SC_detected_2001$SYSTEMATIC_SURVEY[1] <- SC_detected_SYSTEMATIC_survey_25_percent

f <- read.table("CIdata_without_2001_systematic_survey_50_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2001_systematic_survey_50_percent_budget.pdf")
plot(f$X_MARK, f$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_50_percent <- f[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_50_percent <- sort(WPTs_SYSTEMATIC_survey_method_50_percent)
SC_vector_f <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_50_percent[j]) {SC_vector_f <- c(SC_vector_f, Evaluation_survey_method$SC_2001[i])}
  }
}
SC_vector_f
ff <- as.data.frame(table(SC_vector_f))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2001))
SC_detected_SYSTEMATIC_survey_50_percent <- ff[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_50_percent
SC_detected_2001$SYSTEMATIC_SURVEY[2] <- SC_detected_SYSTEMATIC_survey_50_percent

g <- read.table("CIdata_without_2001_systematic_survey_75_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2001_systematic_survey_75_percent_budget.pdf")
plot(g$X_MARK, g$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_75_percent <- g[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_75_percent <- sort(WPTs_SYSTEMATIC_survey_method_75_percent)
SC_vector_g <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_75_percent[j]) {SC_vector_g <- c(SC_vector_g, Evaluation_survey_method$SC_2001[i])}
  }
}
SC_vector_g
gg <- as.data.frame(table(SC_vector_g))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2001))
SC_detected_SYSTEMATIC_survey_75_percent <- gg[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_75_percent
SC_detected_2001$SYSTEMATIC_SURVEY[3] <- SC_detected_SYSTEMATIC_survey_75_percent

h <- read.table("CIdata_without_2001_systematic_survey_100_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2001_systematic_survey_100_percent_budget.pdf")
plot(h$X_MARK, h$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_100_percent <- h[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_100_percent <- sort(WPTs_SYSTEMATIC_survey_method_100_percent)
SC_vector_h <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_100_percent[j]) {SC_vector_h <- c(SC_vector_h, Evaluation_survey_method$SC_2001[i])}
  }
}
SC_vector_h
hh <- as.data.frame(table(SC_vector_h))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2001))
SC_detected_SYSTEMATIC_survey_100_percent <- hh[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_100_percent
SC_detected_2001$SYSTEMATIC_SURVEY[4] <- SC_detected_SYSTEMATIC_survey_100_percent


SC_detected_2001 # check


# Survey based on manager's opinion
i <- read.table("CIdata_without_2001_managers_guess_25_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2001_managers_guess_25_percent_of_budget.pdf")
plot(i$X_MARK, i$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_25_percent <- i[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_25_percent <- sort(WPTs_MANAGERS_OPINION_method_25_percent)
SC_vector_i <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_25_percent)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_25_percent[j]) {SC_vector_i <- c(SC_vector_i, Evaluation_survey_method$SC_2001[i])}
  }
}
SC_vector_i
ii <- as.data.frame(table(SC_vector_i))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2001))
SC_detected_MANAGERS_OPINION_25_percent <- ii[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_25_percent
SC_detected_2001$MANAGERS_OPINION[1] <- SC_detected_MANAGERS_OPINION_25_percent


j <- read.table("CIdata_without_2001_managers_guess_50_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2001_managers_guess_50_percent_of_budget.pdf")
plot(j$X_MARK, j$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_50_percent <- j[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_50_percent <- sort(WPTs_MANAGERS_OPINION_method_50_percent)
SC_vector_j <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_50_percent[j]) {SC_vector_j <- c(SC_vector_j, Evaluation_survey_method$SC_2001[i])}
  }
}
SC_vector_j
jj <- as.data.frame(table(SC_vector_j))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2001))
SC_detected_MANAGERS_OPINION_50_percent <- jj[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_50_percent
SC_detected_2001$MANAGERS_OPINION[2] <- SC_detected_MANAGERS_OPINION_50_percent

k <- read.table("CIdata_without_2001_managers_guess_75_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2001_managers_guess_75_percent_of_budget.pdf")
plot(k$X_MARK, k$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_75_percent <- k[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_75_percent <- sort(WPTs_MANAGERS_OPINION_method_75_percent)
SC_vector_k <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_75_percent[j]) {SC_vector_k <- c(SC_vector_k, Evaluation_survey_method$SC_2001[i])}
  }
}
SC_vector_k
kk <- as.data.frame(table(SC_vector_k))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2001))
SC_detected_MANAGERS_OPINION_75_percent <- kk[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_75_percent
SC_detected_2001$MANAGERS_OPINION[3] <- SC_detected_MANAGERS_OPINION_75_percent

m <- read.table("CIdata_without_2001_managers_guess_100_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2001_managers_guess_100_percent_of_budget.pdf")
plot(m$X_MARK, m$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_100_percent <- m[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_100_percent <- sort(WPTs_MANAGERS_OPINION_method_100_percent)
SC_vector_m <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_100_percent[j]) {SC_vector_m <- c(SC_vector_m, Evaluation_survey_method$SC_2001[i])}
  }
}
SC_vector_m
mm <- as.data.frame(table(SC_vector_m))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2001))
SC_detected_MANAGERS_OPINION_100_percent <- mm[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_100_percent
SC_detected_2001$MANAGERS_OPINION[4] <- SC_detected_MANAGERS_OPINION_100_percent

SC_detected_2001 # check

# Survey based on Pr_occ
n <- read.table("CIdata_without_2001_Pr_Occ_25_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2001_Pr_Occ_25_percent_of_budget.pdf")
plot(n$X_MARK, n$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_25_percent <- n[ ,"WPT"]
WPTs_PR_OCC_method_25_percent <- sort(WPTs_PR_OCC_method_25_percent)
SC_vector_n <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_25_percent)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_25_percent[j]) {SC_vector_n <- c(SC_vector_n, Evaluation_survey_method$SC_2001[i])}
  }
}
SC_vector_n
nn <- as.data.frame(table(SC_vector_n))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2001))
SC_detected_PR_OCC_25_percent <- nn[2, 2]/ll[2, 2]
SC_detected_PR_OCC_25_percent
SC_detected_2001$PR_OCC[1] <- SC_detected_PR_OCC_25_percent

o <- read.table("CIdata_without_2001_Pr_Occ_50_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2001_Pr_Occ_50_percent_of_budget.pdf")
plot(o$X_MARK, o$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_50_percent <- o[ ,"WPT"]
WPTs_PR_OCC_method_50_percent <- sort(WPTs_PR_OCC_method_50_percent)
SC_vector_o <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_50_percent[j]) {SC_vector_o <- c(SC_vector_o, Evaluation_survey_method$SC_2001[i])}
  }
}
SC_vector_o
oo <- as.data.frame(table(SC_vector_o))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2001))
SC_detected_PR_OCC_50_percent <- oo[2, 2]/ll[2, 2]
SC_detected_PR_OCC_50_percent
SC_detected_2001$PR_OCC[2] <- SC_detected_PR_OCC_50_percent

p <- read.table("CIdata_without_2001_Pr_Occ_75_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2001_Pr_Occ_75_percent_of_budget.pdf")
plot(p$X_MARK, p$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_75_percent <- p[ ,"WPT"]
WPTs_PR_OCC_method_75_percent <- sort(WPTs_PR_OCC_method_75_percent)
SC_vector_p <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_75_percent[j]) {SC_vector_p <- c(SC_vector_p, Evaluation_survey_method$SC_2001[i])}
  }
}
SC_vector_p
pp <- as.data.frame(table(SC_vector_p))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2001))
SC_detected_PR_OCC_75_percent <- pp[2, 2]/ll[2, 2]
SC_detected_PR_OCC_75_percent
SC_detected_2001$PR_OCC[3] <- SC_detected_PR_OCC_75_percent

q <- read.table("CIdata_without_2001_Pr_Occ_100_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2001_Pr_Occ_100_percent_of_budget.pdf")
plot(q$X_MARK, q$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_100_percent <- q[ ,"WPT"]
WPTs_PR_OCC_method_100_percent <- sort(WPTs_PR_OCC_method_100_percent)
SC_vector_q <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_100_percent[j]) {SC_vector_q <- c(SC_vector_q, Evaluation_survey_method$SC_2001[i])}
  }
}
SC_vector_q
qq <- as.data.frame(table(SC_vector_q))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2001))
SC_detected_PR_OCC_100_percent <- qq[2, 2]/ll[2, 2]
SC_detected_PR_OCC_100_percent
SC_detected_2001$PR_OCC[4] <- SC_detected_PR_OCC_100_percent

SC_detected_2001 # check

# Survey based on Knapsack 
r <- read.table("CIdata_without_2001_Knapsack_25_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2001_Knapsack_25_percent_of_budget.pdf")
plot(r$X_MARK, r$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_25_percent <- r[ ,"WPT"]
WPTs_KNAPSACK_method_25_percent <- sort(WPTs_KNAPSACK_method_25_percent)
SC_vector_r <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_25_percent)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_25_percent[j]) {SC_vector_r <- c(SC_vector_r, Evaluation_survey_method$SC_2001[i])}
  }
}
SC_vector_r
rr <- as.data.frame(table(SC_vector_r))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2001))
SC_detected_KNAPSACK_25_percent <- rr[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_25_percent
SC_detected_2001$KNAPSACK[1] <- SC_detected_KNAPSACK_25_percent

s <- read.table("CIdata_without_2001_Knapsack_50_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2001_Knapsack_50_percent_of_budget.pdf")
plot(s$X_MARK, s$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_50_percent <- s[ ,"WPT"]
WPTs_KNAPSACK_method_50_percent <- sort(WPTs_KNAPSACK_method_50_percent)
SC_vector_s <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_50_percent[j]) {SC_vector_s <- c(SC_vector_s, Evaluation_survey_method$SC_2001[i])}
  }
}
SC_vector_s
ss <- as.data.frame(table(SC_vector_s))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2001))
SC_detected_KNAPSACK_50_percent <- ss[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_50_percent
SC_detected_2001$KNAPSACK[2] <- SC_detected_KNAPSACK_50_percent

t <- read.table("CIdata_without_2001_Knapsack_75_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2001_Knapsack_75_percent_of_budget.pdf")
plot(t$X_MARK, t$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_75_percent <- t[ ,"WPT"]
WPTs_KNAPSACK_method_75_percent <- sort(WPTs_KNAPSACK_method_75_percent)
SC_vector_t <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_75_percent[j]) {SC_vector_t <- c(SC_vector_t, Evaluation_survey_method$SC_2001[i])}
  }
}
SC_vector_t
tt <- as.data.frame(table(SC_vector_t))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2001))
SC_detected_KNAPSACK_75_percent <- tt[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_75_percent
SC_detected_2001$KNAPSACK[3] <- SC_detected_KNAPSACK_75_percent

u <- read.table("CIdata_without_2001_Knapsack_100_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2001_Knapsack_100_percent_of_budget.pdf")
plot(u$X_MARK, u$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_100_percent <- u[ ,"WPT"]
WPTs_KNAPSACK_method_100_percent <- sort(WPTs_KNAPSACK_method_100_percent)
SC_vector_u <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_100_percent[j]) {SC_vector_u <- c(SC_vector_u, Evaluation_survey_method$SC_2001[i])}
  }
}
SC_vector_u
uu <- as.data.frame(table(SC_vector_u))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2001))
SC_detected_KNAPSACK_100_percent <- uu[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_100_percent
SC_detected_2001$KNAPSACK[4] <- SC_detected_KNAPSACK_100_percent

SC_detected_2001 # check

# Write results
write.table(SC_detected_2001, file="SC_detected_2001.txt",sep="\t",col.names=T,row.names=F)

# Plot results
x <- c(25, 50, 75, 100)
y <- c(SC_detected_2001[ ,1])
plot(x, y, xlim=c(0,100), ylim=c(0,1), xlab="% of total survey effort (island wide survey)", ylab="Effectiveness (% SC found)", main="Performance based on 2001 supercolony distribution")
points(x, SC_detected_2001[ , 2], pch=2)
points(x, SC_detected_2001[ , 3], pch=3)
points(x, SC_detected_2001[ , 4], pch=4)
points(x, SC_detected_2001[ , 5], pch=5)



################################### Evaluate survey performance using 2003 supercolony distribution ####################################
unique(CIdata$Year)
CIdata2003 <- subset(CIdata,CIdata$Year == 2003)
# Create data.frame with WPT, X-mark, Y-mark, SC_2003 (1 or 0)
Evaluation_survey_method <- as.data.frame(matrix(NA, nrow = nrow(CIdata2003), ncol = 4)) # create data.frame 
Evaluation_survey_method[ , 1] <- CIdata2003[ , "WPT"]
Evaluation_survey_method[ , 2] <- CIdata2003[ , "X_MARK"]
Evaluation_survey_method[ , 3] <- CIdata2003[ , "Y_MARK"]
Evaluation_survey_method[ , 4] <- CIdata2003[ , "p_a_SC"]
colnames(Evaluation_survey_method) <- c("WPT", "X_MARK","Y_MARK","SC_2003")
Evaluation_survey_method
# Create dataframe to store survey performance results
SC_detected_2003 <- as.data.frame(matrix(NA, nrow=4, ncol=5))
rownames(SC_detected_2003) <- c("25% total budget", "50% total budget", "75% total budget", "100% total budget")
colnames(SC_detected_2003) <- c("RANDOM_SURVEY", "SYSTEMATIC_SURVEY", "MANAGERS_OPINION", "PR_OCC", "KNAPSACK")

# Random survey 
a <- read.table("CIdata_without_2003_random_sites_25_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2003_random_sites_25_percent.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_25_percent <- a[ ,"WPT"]
WPTs_RANDOM_survey_method_25_percent <- sort(WPTs_RANDOM_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_25_percent[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2003))
SC_detected_RANDOM_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_25_percent
SC_detected_2003$RANDOM_SURVEY[1] <- SC_detected_RANDOM_survey_25_percent


b <- read.table("CIdata_without_2003_random_sites_50_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2003_random_sites_50_percent.pdf")
plot(b$X_MARK, b$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_50_percent <- b[ ,"WPT"]
WPTs_RANDOM_survey_method_50_percent <- sort(WPTs_RANDOM_survey_method_50_percent)
SC_vector_b <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_50_percent[j]) {SC_vector_b <- c(SC_vector_b, Evaluation_survey_method$SC_2003[i])}
  }
}
SC_vector_b
bb <- as.data.frame(table(SC_vector_b))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2003))
SC_detected_RANDOM_survey_50_percent <- bb[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_50_percent
SC_detected_2003$RANDOM_SURVEY[2] <- SC_detected_RANDOM_survey_50_percent


c <- read.table("CIdata_without_2003_random_sites_75_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2003_random_sites_75_percent.pdf")
plot(c$X_MARK, c$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_75_percent <- c[ ,"WPT"]
WPTs_RANDOM_survey_method_75_percent <- sort(WPTs_RANDOM_survey_method_75_percent)
SC_vector_c <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_75_percent[j]) {SC_vector_c <- c(SC_vector_c, Evaluation_survey_method$SC_2003[i])}
  }
}
SC_vector_c
cc <- as.data.frame(table(SC_vector_c))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2003))
SC_detected_RANDOM_survey_75_percent <- cc[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_75_percent
SC_detected_2003$RANDOM_SURVEY[3] <- SC_detected_RANDOM_survey_75_percent

d <- read.table("CIdata_without_2003_random_sites_100_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2003_random_sites_100_percent.pdf")
plot(d$X_MARK, d$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_100_percent <- d[ ,"WPT"]
WPTs_RANDOM_survey_method_100_percent <- sort(WPTs_RANDOM_survey_method_100_percent)
SC_vector_d <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_100_percent[j]) {SC_vector_d <- c(SC_vector_d, Evaluation_survey_method$SC_2003[i])}
  }
}
SC_vector_d
dd <- as.data.frame(table(SC_vector_d))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2003))
SC_detected_RANDOM_survey_100_percent <- dd[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_100_percent
SC_detected_2003$RANDOM_SURVEY[4] <- SC_detected_RANDOM_survey_100_percent


SC_detected_2003 # check



# Systematic survey
e <- read.table("CIdata_without_2003_systematic_survey_25_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2003_systematic_survey_25_percent_budget.pdf")
plot(e$X_MARK, e$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_25_percent <- e[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_25_percent <- sort(WPTs_SYSTEMATIC_survey_method_25_percent)
SC_vector_e <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_25_percent[j]) {SC_vector_e <- c(SC_vector_e, Evaluation_survey_method$SC_2003[i])}
  }
}
SC_vector_e
ee <- as.data.frame(table(SC_vector_e))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2003))
SC_detected_SYSTEMATIC_survey_25_percent <- ee[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_25_percent
SC_detected_2003$SYSTEMATIC_SURVEY[1] <- SC_detected_SYSTEMATIC_survey_25_percent

f <- read.table("CIdata_without_2003_systematic_survey_50_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2003_systematic_survey_50_percent_budget.pdf")
plot(f$X_MARK, f$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_50_percent <- f[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_50_percent <- sort(WPTs_SYSTEMATIC_survey_method_50_percent)
SC_vector_f <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_50_percent[j]) {SC_vector_f <- c(SC_vector_f, Evaluation_survey_method$SC_2003[i])}
  }
}
SC_vector_f
ff <- as.data.frame(table(SC_vector_f))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2003))
SC_detected_SYSTEMATIC_survey_50_percent <- ff[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_50_percent
SC_detected_2003$SYSTEMATIC_SURVEY[2] <- SC_detected_SYSTEMATIC_survey_50_percent

g <- read.table("CIdata_without_2003_systematic_survey_75_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2003_systematic_survey_75_percent_budget.pdf")
plot(g$X_MARK, g$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_75_percent <- g[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_75_percent <- sort(WPTs_SYSTEMATIC_survey_method_75_percent)
SC_vector_g <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_75_percent[j]) {SC_vector_g <- c(SC_vector_g, Evaluation_survey_method$SC_2003[i])}
  }
}
SC_vector_g
gg <- as.data.frame(table(SC_vector_g))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2003))
SC_detected_SYSTEMATIC_survey_75_percent <- gg[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_75_percent
SC_detected_2003$SYSTEMATIC_SURVEY[3] <- SC_detected_SYSTEMATIC_survey_75_percent

h <- read.table("CIdata_without_2003_systematic_survey_100_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2003_systematic_survey_100_percent_budget.pdf")
plot(h$X_MARK, h$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_100_percent <- h[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_100_percent <- sort(WPTs_SYSTEMATIC_survey_method_100_percent)
SC_vector_h <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_100_percent[j]) {SC_vector_h <- c(SC_vector_h, Evaluation_survey_method$SC_2003[i])}
  }
}
SC_vector_h
hh <- as.data.frame(table(SC_vector_h))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2003))
SC_detected_SYSTEMATIC_survey_100_percent <- hh[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_100_percent
SC_detected_2003$SYSTEMATIC_SURVEY[4] <- SC_detected_SYSTEMATIC_survey_100_percent


SC_detected_2003 # check


# Survey based on manager's opinion
i <- read.table("CIdata_without_2003_managers_guess_25_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2003_managers_guess_25_percent_of_budget.pdf")
plot(i$X_MARK, i$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_25_percent <- i[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_25_percent <- sort(WPTs_MANAGERS_OPINION_method_25_percent)
SC_vector_i <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_25_percent[j]) {SC_vector_i <- c(SC_vector_i, Evaluation_survey_method$SC_2003[i])}
  }
}
SC_vector_i
ii <- as.data.frame(table(SC_vector_i))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2003))
SC_detected_MANAGERS_OPINION_25_percent <- ii[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_25_percent
SC_detected_2003$MANAGERS_OPINION[1] <- SC_detected_MANAGERS_OPINION_25_percent


j <- read.table("CIdata_without_2003_managers_guess_50_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2003_managers_guess_50_percent_of_budget.pdf")
plot(j$X_MARK, j$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_50_percent <- j[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_50_percent <- sort(WPTs_MANAGERS_OPINION_method_50_percent)
SC_vector_j <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_50_percent[j]) {SC_vector_j <- c(SC_vector_j, Evaluation_survey_method$SC_2003[i])}
  }
}
SC_vector_j
jj <- as.data.frame(table(SC_vector_j))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2003))
SC_detected_MANAGERS_OPINION_50_percent <- jj[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_50_percent
SC_detected_2003$MANAGERS_OPINION[2] <- SC_detected_MANAGERS_OPINION_50_percent

k <- read.table("CIdata_without_2003_managers_guess_75_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2003_managers_guess_75_percent_of_budget.pdf")
plot(k$X_MARK, k$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_75_percent <- k[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_75_percent <- sort(WPTs_MANAGERS_OPINION_method_75_percent)
SC_vector_k <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_75_percent[j]) {SC_vector_k <- c(SC_vector_k, Evaluation_survey_method$SC_2003[i])}
  }
}
SC_vector_k
kk <- as.data.frame(table(SC_vector_k))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2003))
SC_detected_MANAGERS_OPINION_75_percent <- kk[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_75_percent
SC_detected_2003$MANAGERS_OPINION[3] <- SC_detected_MANAGERS_OPINION_75_percent

m <- read.table("CIdata_without_2003_managers_guess_100_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2003_managers_guess_100_percent_of_budget.pdf")
plot(m$X_MARK, m$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_100_percent <- m[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_100_percent <- sort(WPTs_MANAGERS_OPINION_method_100_percent)
SC_vector_m <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_100_percent[j]) {SC_vector_m <- c(SC_vector_m, Evaluation_survey_method$SC_2003[i])}
  }
}
SC_vector_m
mm <- as.data.frame(table(SC_vector_m))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2003))
SC_detected_MANAGERS_OPINION_100_percent <- mm[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_100_percent
SC_detected_2003$MANAGERS_OPINION[4] <- SC_detected_MANAGERS_OPINION_100_percent

SC_detected_2003 # check

# Survey based on Pr_occ
n <- read.table("CIdata_without_2003_Pr_Occ_25_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2003_Pr_Occ_25_percent_of_budget.pdf")
plot(n$X_MARK, n$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_25_percent <- n[ ,"WPT"]
WPTs_PR_OCC_method_25_percent <- sort(WPTs_PR_OCC_method_25_percent)
SC_vector_n <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_25_percent[j]) {SC_vector_n <- c(SC_vector_n, Evaluation_survey_method$SC_2003[i])}
  }
}
SC_vector_n
nn <- as.data.frame(table(SC_vector_n))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2003))
SC_detected_PR_OCC_25_percent <- nn[2, 2]/ll[2, 2]
SC_detected_PR_OCC_25_percent
SC_detected_2003$PR_OCC[1] <- SC_detected_PR_OCC_25_percent

o <- read.table("CIdata_without_2003_Pr_Occ_50_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2003_Pr_Occ_50_percent_of_budget.pdf")
plot(o$X_MARK, o$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_50_percent <- o[ ,"WPT"]
WPTs_PR_OCC_method_50_percent <- sort(WPTs_PR_OCC_method_50_percent)
SC_vector_o <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_50_percent[j]) {SC_vector_o <- c(SC_vector_o, Evaluation_survey_method$SC_2003[i])}
  }
}
SC_vector_o
oo <- as.data.frame(table(SC_vector_o))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2003))
SC_detected_PR_OCC_50_percent <- oo[2, 2]/ll[2, 2]
SC_detected_PR_OCC_50_percent
SC_detected_2003$PR_OCC[2] <- SC_detected_PR_OCC_50_percent

p <- read.table("CIdata_without_2003_Pr_Occ_75_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2003_Pr_Occ_75_percent_of_budget.pdf")
plot(p$X_MARK, p$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_75_percent <- p[ ,"WPT"]
WPTs_PR_OCC_method_75_percent <- sort(WPTs_PR_OCC_method_75_percent)
SC_vector_p <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_75_percent[j]) {SC_vector_p <- c(SC_vector_p, Evaluation_survey_method$SC_2003[i])}
  }
}
SC_vector_p
pp <- as.data.frame(table(SC_vector_p))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2003))
SC_detected_PR_OCC_75_percent <- pp[2, 2]/ll[2, 2]
SC_detected_PR_OCC_75_percent
SC_detected_2003$PR_OCC[3] <- SC_detected_PR_OCC_75_percent

q <- read.table("CIdata_without_2003_Pr_Occ_100_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2003_Pr_Occ_100_percent_of_budget.pdf")
plot(q$X_MARK, q$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_100_percent <- q[ ,"WPT"]
WPTs_PR_OCC_method_100_percent <- sort(WPTs_PR_OCC_method_100_percent)
SC_vector_q <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_100_percent[j]) {SC_vector_q <- c(SC_vector_q, Evaluation_survey_method$SC_2003[i])}
  }
}
SC_vector_q
qq <- as.data.frame(table(SC_vector_q))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2003))
SC_detected_PR_OCC_100_percent <- qq[2, 2]/ll[2, 2]
SC_detected_PR_OCC_100_percent
SC_detected_2003$PR_OCC[4] <- SC_detected_PR_OCC_100_percent

SC_detected_2003 # check

# Survey based on Knapsack 
r <- read.table("CIdata_without_2003_Knapsack_25_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2003_Knapsack_25_percent_of_budget.pdf")
plot(r$X_MARK, r$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_25_percent <- r[ ,"WPT"]
WPTs_KNAPSACK_method_25_percent <- sort(WPTs_KNAPSACK_method_25_percent)
SC_vector_r <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_25_percent[j]) {SC_vector_r <- c(SC_vector_r, Evaluation_survey_method$SC_2003[i])}
  }
}
SC_vector_r
rr <- as.data.frame(table(SC_vector_r))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2003))
SC_detected_KNAPSACK_25_percent <- rr[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_25_percent
SC_detected_2003$KNAPSACK[1] <- SC_detected_KNAPSACK_25_percent

s <- read.table("CIdata_without_2003_Knapsack_50_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2003_Knapsack_50_percent_of_budget.pdf")
plot(s$X_MARK, s$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_50_percent <- s[ ,"WPT"]
WPTs_KNAPSACK_method_50_percent <- sort(WPTs_KNAPSACK_method_50_percent)
SC_vector_s <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_50_percent[j]) {SC_vector_s <- c(SC_vector_s, Evaluation_survey_method$SC_2003[i])}
  }
}
SC_vector_s
ss <- as.data.frame(table(SC_vector_s))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2003))
SC_detected_KNAPSACK_50_percent <- ss[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_50_percent
SC_detected_2003$KNAPSACK[2] <- SC_detected_KNAPSACK_50_percent

t <- read.table("CIdata_without_2003_Knapsack_75_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2003_Knapsack_75_percent_of_budget.pdf")
plot(t$X_MARK, t$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_75_percent <- t[ ,"WPT"]
WPTs_KNAPSACK_method_75_percent <- sort(WPTs_KNAPSACK_method_75_percent)
SC_vector_t <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_75_percent[j]) {SC_vector_t <- c(SC_vector_t, Evaluation_survey_method$SC_2003[i])}
  }
}
SC_vector_t
tt <- as.data.frame(table(SC_vector_t))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2003))
SC_detected_KNAPSACK_75_percent <- tt[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_75_percent
SC_detected_2003$KNAPSACK[3] <- SC_detected_KNAPSACK_75_percent

u <- read.table("CIdata_without_2003_Knapsack_100_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2003_Knapsack_100_percent_of_budget.pdf")
plot(u$X_MARK, u$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_100_percent <- u[ ,"WPT"]
WPTs_KNAPSACK_method_100_percent <- sort(WPTs_KNAPSACK_method_100_percent)
SC_vector_u <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_100_percent[j]) {SC_vector_u <- c(SC_vector_u, Evaluation_survey_method$SC_2003[i])}
  }
}
SC_vector_u
uu <- as.data.frame(table(SC_vector_u))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2003))
SC_detected_KNAPSACK_100_percent <- uu[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_100_percent
SC_detected_2003$KNAPSACK[4] <- SC_detected_KNAPSACK_100_percent

SC_detected_2003 # check

# Write results
write.table(SC_detected_2003, file="SC_detected_2003.txt",sep="\t",col.names=T,row.names=F)

# Plot results
x <- c(25, 50, 75, 100)
y <- c(SC_detected_2003[ ,1])
plot(x, y, xlim=c(0,100), ylim=c(0,1), xlab="% of total survey effort (island wide survey)", ylab="Effectiveness (% SC found)", main="Performance based on 2003 supercolony distribution")
points(x, SC_detected_2003[ , 2], pch=2)
points(x, SC_detected_2003[ , 3], pch=3)
points(x, SC_detected_2003[ , 4], pch=4)
points(x, SC_detected_2003[ , 5], pch=5)




################################### Evaluate survey performance using 2005 supercolony distribution ####################################
unique(CIdata$Year)
CIdata2005 <- subset(CIdata,CIdata$Year == 2005)
# Create data.frame with WPT, X-mark, Y-mark, SC_2005 (1 or 0)
Evaluation_survey_method <- as.data.frame(matrix(NA, nrow = nrow(CIdata2005), ncol = 4)) # create data.frame 
Evaluation_survey_method[ , 1] <- CIdata2005[ , "WPT"]
Evaluation_survey_method[ , 2] <- CIdata2005[ , "X_MARK"]
Evaluation_survey_method[ , 3] <- CIdata2005[ , "Y_MARK"]
Evaluation_survey_method[ , 4] <- CIdata2005[ , "p_a_SC"]
colnames(Evaluation_survey_method) <- c("WPT", "X_MARK","Y_MARK","SC_2005")
Evaluation_survey_method
# Create dataframe to store survey performance results
SC_detected_2005 <- as.data.frame(matrix(NA, nrow=4, ncol=5))
rownames(SC_detected_2005) <- c("25% total budget", "50% total budget", "75% total budget", "100% total budget")
colnames(SC_detected_2005) <- c("RANDOM_SURVEY", "SYSTEMATIC_SURVEY", "MANAGERS_OPINION", "PR_OCC", "KNAPSACK")

# Random survey 
a <- read.table("CIdata_without_2005_random_sites_25_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2005_random_sites_25_percent.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_25_percent <- a[ ,"WPT"]
WPTs_RANDOM_survey_method_25_percent <- sort(WPTs_RANDOM_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_25_percent[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2005))
SC_detected_RANDOM_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_25_percent
SC_detected_2005$RANDOM_SURVEY[1] <- SC_detected_RANDOM_survey_25_percent


b <- read.table("CIdata_without_2005_random_sites_50_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2005_random_sites_50_percent.pdf")
plot(b$X_MARK, b$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_50_percent <- b[ ,"WPT"]
WPTs_RANDOM_survey_method_50_percent <- sort(WPTs_RANDOM_survey_method_50_percent)
SC_vector_b <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_50_percent[j]) {SC_vector_b <- c(SC_vector_b, Evaluation_survey_method$SC_2005[i])}
  }
}
SC_vector_b
bb <- as.data.frame(table(SC_vector_b))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2005))
SC_detected_RANDOM_survey_50_percent <- bb[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_50_percent
SC_detected_2005$RANDOM_SURVEY[2] <- SC_detected_RANDOM_survey_50_percent


c <- read.table("CIdata_without_2005_random_sites_75_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2005_random_sites_75_percent.pdf")
plot(c$X_MARK, c$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_75_percent <- c[ ,"WPT"]
WPTs_RANDOM_survey_method_75_percent <- sort(WPTs_RANDOM_survey_method_75_percent)
SC_vector_c <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_75_percent[j]) {SC_vector_c <- c(SC_vector_c, Evaluation_survey_method$SC_2005[i])}
  }
}
SC_vector_c
cc <- as.data.frame(table(SC_vector_c))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2005))
SC_detected_RANDOM_survey_75_percent <- cc[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_75_percent
SC_detected_2005$RANDOM_SURVEY[3] <- SC_detected_RANDOM_survey_75_percent

d <- read.table("CIdata_without_2005_random_sites_100_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2005_random_sites_100_percent.pdf")
plot(d$X_MARK, d$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_100_percent <- d[ ,"WPT"]
WPTs_RANDOM_survey_method_100_percent <- sort(WPTs_RANDOM_survey_method_100_percent)
SC_vector_d <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_100_percent[j]) {SC_vector_d <- c(SC_vector_d, Evaluation_survey_method$SC_2005[i])}
  }
}
SC_vector_d
dd <- as.data.frame(table(SC_vector_d))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2005))
SC_detected_RANDOM_survey_100_percent <- dd[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_100_percent
SC_detected_2005$RANDOM_SURVEY[4] <- SC_detected_RANDOM_survey_100_percent


SC_detected_2005 # check



# Systematic survey
e <- read.table("CIdata_without_2005_systematic_survey_25_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2005_systematic_survey_25_percent_budget.pdf")
plot(e$X_MARK, e$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_25_percent <- e[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_25_percent <- sort(WPTs_SYSTEMATIC_survey_method_25_percent)
SC_vector_e <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_25_percent[j]) {SC_vector_e <- c(SC_vector_e, Evaluation_survey_method$SC_2005[i])}
  }
}
SC_vector_e
ee <- as.data.frame(table(SC_vector_e))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2005))
SC_detected_SYSTEMATIC_survey_25_percent <- ee[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_25_percent
SC_detected_2005$SYSTEMATIC_SURVEY[1] <- SC_detected_SYSTEMATIC_survey_25_percent

f <- read.table("CIdata_without_2005_systematic_survey_50_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2005_systematic_survey_50_percent_budget.pdf")
plot(f$X_MARK, f$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_50_percent <- f[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_50_percent <- sort(WPTs_SYSTEMATIC_survey_method_50_percent)
SC_vector_f <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_50_percent[j]) {SC_vector_f <- c(SC_vector_f, Evaluation_survey_method$SC_2005[i])}
  }
}
SC_vector_f
ff <- as.data.frame(table(SC_vector_f))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2005))
SC_detected_SYSTEMATIC_survey_50_percent <- ff[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_50_percent
SC_detected_2005$SYSTEMATIC_SURVEY[2] <- SC_detected_SYSTEMATIC_survey_50_percent


g <- read.table("CIdata_without_2005_systematic_survey_75_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2005_systematic_survey_75_percent_budget.pdf")
plot(g$X_MARK, g$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_75_percent <- g[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_75_percent <- sort(WPTs_SYSTEMATIC_survey_method_75_percent)
SC_vector_g <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_75_percent[j]) {SC_vector_g <- c(SC_vector_g, Evaluation_survey_method$SC_2005[i])}
  }
}
SC_vector_g
gg <- as.data.frame(table(SC_vector_g))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2005))
SC_detected_SYSTEMATIC_survey_75_percent <- gg[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_75_percent
SC_detected_2005$SYSTEMATIC_SURVEY[3] <- SC_detected_SYSTEMATIC_survey_75_percent

h <- read.table("CIdata_without_2005_systematic_survey_100_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2005_systematic_survey_100_percent_budget.pdf")
plot(h$X_MARK, h$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_100_percent <- h[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_100_percent <- sort(WPTs_SYSTEMATIC_survey_method_100_percent)
SC_vector_h <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_100_percent[j]) {SC_vector_h <- c(SC_vector_h, Evaluation_survey_method$SC_2005[i])}
  }
}
SC_vector_h
hh <- as.data.frame(table(SC_vector_h))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2005))
SC_detected_SYSTEMATIC_survey_100_percent <- hh[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_100_percent
SC_detected_2005$SYSTEMATIC_SURVEY[4] <- SC_detected_SYSTEMATIC_survey_100_percent


SC_detected_2005 # check


# Survey based on manager's opinion
i <- read.table("CIdata_without_2005_managers_guess_25_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2005_managers_guess_25_percent_of_budget.pdf")
plot(i$X_MARK, i$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_25_percent <- i[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_25_percent <- sort(WPTs_MANAGERS_OPINION_method_25_percent)
SC_vector_i <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_25_percent[j]) {SC_vector_i <- c(SC_vector_i, Evaluation_survey_method$SC_2005[i])}
  }
}
SC_vector_i
ii <- as.data.frame(table(SC_vector_i))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2005))
SC_detected_MANAGERS_OPINION_25_percent <- ii[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_25_percent
SC_detected_2005$MANAGERS_OPINION[1] <- SC_detected_MANAGERS_OPINION_25_percent


j <- read.table("CIdata_without_2005_managers_guess_50_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2005_managers_guess_50_percent_of_budget.pdf")
plot(j$X_MARK, j$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_50_percent <- j[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_50_percent <- sort(WPTs_MANAGERS_OPINION_method_50_percent)
SC_vector_j <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_50_percent[j]) {SC_vector_j <- c(SC_vector_j, Evaluation_survey_method$SC_2005[i])}
  }
}
SC_vector_j
jj <- as.data.frame(table(SC_vector_j))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2005))
SC_detected_MANAGERS_OPINION_50_percent <- jj[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_50_percent
SC_detected_2005$MANAGERS_OPINION[2] <- SC_detected_MANAGERS_OPINION_50_percent

k <- read.table("CIdata_without_2005_managers_guess_75_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2005_managers_guess_75_percent_of_budget.pdf")
plot(k$X_MARK, k$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_75_percent <- k[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_75_percent <- sort(WPTs_MANAGERS_OPINION_method_75_percent)
SC_vector_k <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_75_percent[j]) {SC_vector_k <- c(SC_vector_k, Evaluation_survey_method$SC_2005[i])}
  }
}
SC_vector_k
kk <- as.data.frame(table(SC_vector_k))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2005))
SC_detected_MANAGERS_OPINION_75_percent <- kk[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_75_percent
SC_detected_2005$MANAGERS_OPINION[3] <- SC_detected_MANAGERS_OPINION_75_percent

m <- read.table("CIdata_without_2005_managers_guess_100_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2005_managers_guess_100_percent_of_budget.pdf")
plot(m$X_MARK, m$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_100_percent <- m[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_100_percent <- sort(WPTs_MANAGERS_OPINION_method_100_percent)
SC_vector_m <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_100_percent[j]) {SC_vector_m <- c(SC_vector_m, Evaluation_survey_method$SC_2005[i])}
  }
}
SC_vector_m
mm <- as.data.frame(table(SC_vector_m))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2005))
SC_detected_MANAGERS_OPINION_100_percent <- mm[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_100_percent
SC_detected_2005$MANAGERS_OPINION[4] <- SC_detected_MANAGERS_OPINION_100_percent

SC_detected_2005 # check

# Survey based on Pr_occ
n <- read.table("CIdata_without_2005_Pr_Occ_25_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2005_Pr_Occ_25_percent_of_budget.pdf")
plot(n$X_MARK, n$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_25_percent <- n[ ,"WPT"]
WPTs_PR_OCC_method_25_percent <- sort(WPTs_PR_OCC_method_25_percent)
SC_vector_n <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_25_percent[j]) {SC_vector_n <- c(SC_vector_n, Evaluation_survey_method$SC_2005[i])}
  }
}
SC_vector_n
nn <- as.data.frame(table(SC_vector_n))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2005))
SC_detected_PR_OCC_25_percent <- nn[2, 2]/ll[2, 2]
SC_detected_PR_OCC_25_percent
SC_detected_2005$PR_OCC[1] <- SC_detected_PR_OCC_25_percent

o <- read.table("CIdata_without_2005_Pr_Occ_50_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2005_Pr_Occ_50_percent_of_budget.pdf")
plot(o$X_MARK, o$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_50_percent <- o[ ,"WPT"]
WPTs_PR_OCC_method_50_percent <- sort(WPTs_PR_OCC_method_50_percent)
SC_vector_o <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_50_percent[j]) {SC_vector_o <- c(SC_vector_o, Evaluation_survey_method$SC_2005[i])}
  }
}
SC_vector_o
oo <- as.data.frame(table(SC_vector_o))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2005))
SC_detected_PR_OCC_50_percent <- oo[2, 2]/ll[2, 2]
SC_detected_PR_OCC_50_percent
SC_detected_2005$PR_OCC[2] <- SC_detected_PR_OCC_50_percent

p <- read.table("CIdata_without_2005_Pr_Occ_75_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2005_Pr_Occ_75_percent_of_budget.pdf")
plot(p$X_MARK, p$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_75_percent <- p[ ,"WPT"]
WPTs_PR_OCC_method_75_percent <- sort(WPTs_PR_OCC_method_75_percent)
SC_vector_p <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_75_percent[j]) {SC_vector_p <- c(SC_vector_p, Evaluation_survey_method$SC_2005[i])}
  }
}
SC_vector_p
pp <- as.data.frame(table(SC_vector_p))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2005))
SC_detected_PR_OCC_75_percent <- pp[2, 2]/ll[2, 2]
SC_detected_PR_OCC_75_percent
SC_detected_2005$PR_OCC[3] <- SC_detected_PR_OCC_75_percent

q <- read.table("CIdata_without_2005_Pr_Occ_100_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2005_Pr_Occ_100_percent_of_budget.pdf")
plot(q$X_MARK, q$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_100_percent <- q[ ,"WPT"]
WPTs_PR_OCC_method_100_percent <- sort(WPTs_PR_OCC_method_100_percent)
SC_vector_q <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_100_percent[j]) {SC_vector_q <- c(SC_vector_q, Evaluation_survey_method$SC_2005[i])}
  }
}
SC_vector_q
qq <- as.data.frame(table(SC_vector_q))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2005))
SC_detected_PR_OCC_100_percent <- qq[2, 2]/ll[2, 2]
SC_detected_PR_OCC_100_percent
SC_detected_2005$PR_OCC[4] <- SC_detected_PR_OCC_100_percent

SC_detected_2005 # check

# Survey based on Knapsack 
r <- read.table("CIdata_without_2005_Knapsack_25_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2005_Knapsack_25_percent_of_budget.pdf")
plot(r$X_MARK, r$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_25_percent <- r[ ,"WPT"]
WPTs_KNAPSACK_method_25_percent <- sort(WPTs_KNAPSACK_method_25_percent)
SC_vector_r <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_25_percent[j]) {SC_vector_r <- c(SC_vector_r, Evaluation_survey_method$SC_2005[i])}
  }
}
SC_vector_r
rr <- as.data.frame(table(SC_vector_r))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2005))
SC_detected_KNAPSACK_25_percent <- rr[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_25_percent
SC_detected_2005$KNAPSACK[1] <- SC_detected_KNAPSACK_25_percent

s <- read.table("CIdata_without_2005_Knapsack_50_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2005_Knapsack_50_percent_of_budget.pdf")
plot(s$X_MARK, s$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_50_percent <- s[ ,"WPT"]
WPTs_KNAPSACK_method_50_percent <- sort(WPTs_KNAPSACK_method_50_percent)
SC_vector_s <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_50_percent[j]) {SC_vector_s <- c(SC_vector_s, Evaluation_survey_method$SC_2005[i])}
  }
}
SC_vector_s
ss <- as.data.frame(table(SC_vector_s))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2005))
SC_detected_KNAPSACK_50_percent <- ss[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_50_percent
SC_detected_2005$KNAPSACK[2] <- SC_detected_KNAPSACK_50_percent

t <- read.table("CIdata_without_2005_Knapsack_75_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2005_Knapsack_75_percent_of_budget.pdf")
plot(t$X_MARK, t$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_75_percent <- t[ ,"WPT"]
WPTs_KNAPSACK_method_75_percent <- sort(WPTs_KNAPSACK_method_75_percent)
SC_vector_t <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_75_percent[j]) {SC_vector_t <- c(SC_vector_t, Evaluation_survey_method$SC_2005[i])}
  }
}
SC_vector_t
tt <- as.data.frame(table(SC_vector_t))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2005))
SC_detected_KNAPSACK_75_percent <- tt[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_75_percent
SC_detected_2005$KNAPSACK[3] <- SC_detected_KNAPSACK_75_percent

u <- read.table("CIdata_without_2005_Knapsack_100_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2005_Knapsack_100_percent_of_budget.pdf")
plot(u$X_MARK, u$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_100_percent <- u[ ,"WPT"]
WPTs_KNAPSACK_method_100_percent <- sort(WPTs_KNAPSACK_method_100_percent)
SC_vector_u <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_100_percent[j]) {SC_vector_u <- c(SC_vector_u, Evaluation_survey_method$SC_2005[i])}
  }
}
SC_vector_u
uu <- as.data.frame(table(SC_vector_u))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2005))
SC_detected_KNAPSACK_100_percent <- uu[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_100_percent
SC_detected_2005$KNAPSACK[4] <- SC_detected_KNAPSACK_100_percent

SC_detected_2005 # check

# Write results
write.table(SC_detected_2005, file="SC_detected_2005.txt",sep="\t",col.names=T,row.names=F)

# Plot results
x <- c(25, 50, 75, 100)
y <- c(SC_detected_2005[ ,1])
plot(x, y, xlim=c(0,100), ylim=c(0,1), xlab="% of total survey effort (island wide survey)", ylab="Effectiveness (% SC found)", main="Performance based on 2005 supercolony distribution")
points(x, SC_detected_2005[ , 2], pch=2)
points(x, SC_detected_2005[ , 3], pch=3)
points(x, SC_detected_2005[ , 4], pch=4)
points(x, SC_detected_2005[ , 5], pch=5)


################################### Evaluate survey performance using 2007 supercolony distribution ####################################
unique(CIdata$Year)
CIdata2007 <- subset(CIdata,CIdata$Year == 2007)
# Create data.frame with WPT, X-mark, Y-mark, SC_2007 (1 or 0)
Evaluation_survey_method <- as.data.frame(matrix(NA, nrow = nrow(CIdata2007), ncol = 4)) # create data.frame 
Evaluation_survey_method[ , 1] <- CIdata2007[ , "WPT"]
Evaluation_survey_method[ , 2] <- CIdata2007[ , "X_MARK"]
Evaluation_survey_method[ , 3] <- CIdata2007[ , "Y_MARK"]
Evaluation_survey_method[ , 4] <- CIdata2007[ , "p_a_SC"]
colnames(Evaluation_survey_method) <- c("WPT", "X_MARK","Y_MARK","SC_2007")
Evaluation_survey_method
# Create dataframe to store survey performance results
SC_detected_2007 <- as.data.frame(matrix(NA, nrow=4, ncol=5))
rownames(SC_detected_2007) <- c("25% total budget", "50% total budget", "75% total budget", "100% total budget")
colnames(SC_detected_2007) <- c("RANDOM_SURVEY", "SYSTEMATIC_SURVEY", "MANAGERS_OPINION", "PR_OCC", "KNAPSACK")

# Random survey 
a <- read.table("CIdata_without_2007_random_sites_25_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2007_random_sites_25_percent.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_25_percent <- a[ ,"WPT"]
WPTs_RANDOM_survey_method_25_percent <- sort(WPTs_RANDOM_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_25_percent[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2007))
SC_detected_RANDOM_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_25_percent
SC_detected_2007$RANDOM_SURVEY[1] <- SC_detected_RANDOM_survey_25_percent


b <- read.table("CIdata_without_2007_random_sites_50_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2007_random_sites_50_percent.pdf")
plot(b$X_MARK, b$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_50_percent <- b[ ,"WPT"]
WPTs_RANDOM_survey_method_50_percent <- sort(WPTs_RANDOM_survey_method_50_percent)
SC_vector_b <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_50_percent[j]) {SC_vector_b <- c(SC_vector_b, Evaluation_survey_method$SC_2007[i])}
  }
}
SC_vector_b
bb <- as.data.frame(table(SC_vector_b))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2007))
SC_detected_RANDOM_survey_50_percent <- bb[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_50_percent
SC_detected_2007$RANDOM_SURVEY[2] <- SC_detected_RANDOM_survey_50_percent


c <- read.table("CIdata_without_2007_random_sites_75_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2007_random_sites_75_percent.pdf")
plot(c$X_MARK, c$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_75_percent <- c[ ,"WPT"]
WPTs_RANDOM_survey_method_75_percent <- sort(WPTs_RANDOM_survey_method_75_percent)
SC_vector_c <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_75_percent[j]) {SC_vector_c <- c(SC_vector_c, Evaluation_survey_method$SC_2007[i])}
  }
}
SC_vector_c
cc <- as.data.frame(table(SC_vector_c))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2007))
SC_detected_RANDOM_survey_75_percent <- cc[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_75_percent
SC_detected_2007$RANDOM_SURVEY[3] <- SC_detected_RANDOM_survey_75_percent

d <- read.table("CIdata_without_2007_random_sites_100_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2007_random_sites_100_percent.pdf")
plot(d$X_MARK, d$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_100_percent <- d[ ,"WPT"]
WPTs_RANDOM_survey_method_100_percent <- sort(WPTs_RANDOM_survey_method_100_percent)
SC_vector_d <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_100_percent[j]) {SC_vector_d <- c(SC_vector_d, Evaluation_survey_method$SC_2007[i])}
  }
}
SC_vector_d
dd <- as.data.frame(table(SC_vector_d))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2007))
SC_detected_RANDOM_survey_100_percent <- dd[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_100_percent
SC_detected_2007$RANDOM_SURVEY[4] <- SC_detected_RANDOM_survey_100_percent


SC_detected_2007 # check



# Systematic survey
e <- read.table("CIdata_without_2007_systematic_survey_25_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2007_systematic_survey_25_percent_budget.pdf")
plot(e$X_MARK, e$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_25_percent <- e[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_25_percent <- sort(WPTs_SYSTEMATIC_survey_method_25_percent)
SC_vector_e <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_25_percent[j]) {SC_vector_e <- c(SC_vector_e, Evaluation_survey_method$SC_2007[i])}
  }
}
SC_vector_e
ee <- as.data.frame(table(SC_vector_e))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2007))
SC_detected_SYSTEMATIC_survey_25_percent <- ee[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_25_percent
SC_detected_2007$SYSTEMATIC_SURVEY[1] <- SC_detected_SYSTEMATIC_survey_25_percent

f <- read.table("CIdata_without_2007_systematic_survey_50_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2007_systematic_survey_50_percent_budget.pdf")
plot(f$X_MARK, f$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_50_percent <- f[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_50_percent <- sort(WPTs_SYSTEMATIC_survey_method_50_percent)
SC_vector_f <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_50_percent[j]) {SC_vector_f <- c(SC_vector_f, Evaluation_survey_method$SC_2007[i])}
  }
}
SC_vector_f
ff <- as.data.frame(table(SC_vector_f))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2007))
SC_detected_SYSTEMATIC_survey_50_percent <- ff[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_50_percent
SC_detected_2007$SYSTEMATIC_SURVEY[2] <- SC_detected_SYSTEMATIC_survey_50_percent

g <- read.table("CIdata_without_2007_systematic_survey_75_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2007_systematic_survey_75_percent_budget.pdf")
plot(g$X_MARK, g$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_75_percent <- g[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_75_percent <- sort(WPTs_SYSTEMATIC_survey_method_75_percent)
SC_vector_g <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_75_percent[j]) {SC_vector_g <- c(SC_vector_g, Evaluation_survey_method$SC_2007[i])}
  }
}
SC_vector_g
gg <- as.data.frame(table(SC_vector_g))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2007))
SC_detected_SYSTEMATIC_survey_75_percent <- gg[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_75_percent
SC_detected_2007$SYSTEMATIC_SURVEY[3] <- SC_detected_SYSTEMATIC_survey_75_percent

h <- read.table("CIdata_without_2007_systematic_survey_100_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2007_systematic_survey_100_percent_budget.pdf")
plot(h$X_MARK, h$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_100_percent <- h[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_100_percent <- sort(WPTs_SYSTEMATIC_survey_method_100_percent)
SC_vector_h <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_100_percent[j]) {SC_vector_h <- c(SC_vector_h, Evaluation_survey_method$SC_2007[i])}
  }
}
SC_vector_h
hh <- as.data.frame(table(SC_vector_h))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2007))
SC_detected_SYSTEMATIC_survey_100_percent <- hh[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_100_percent
SC_detected_2007$SYSTEMATIC_SURVEY[4] <- SC_detected_SYSTEMATIC_survey_100_percent


SC_detected_2007 # check


# Survey based on manager's opinion
i <- read.table("CIdata_without_2007_managers_guess_25_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2007_managers_guess_25_percent_of_budget.pdf")
plot(i$X_MARK, i$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_25_percent <- i[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_25_percent <- sort(WPTs_MANAGERS_OPINION_method_25_percent)
SC_vector_i <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_25_percent[j]) {SC_vector_i <- c(SC_vector_i, Evaluation_survey_method$SC_2007[i])}
  }
}
SC_vector_i
ii <- as.data.frame(table(SC_vector_i))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2007))
SC_detected_MANAGERS_OPINION_25_percent <- ii[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_25_percent
SC_detected_2007$MANAGERS_OPINION[1] <- SC_detected_MANAGERS_OPINION_25_percent


j <- read.table("CIdata_without_2007_managers_guess_50_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2007_managers_guess_50_percent_of_budget.pdf")
plot(j$X_MARK, j$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_50_percent <- j[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_50_percent <- sort(WPTs_MANAGERS_OPINION_method_50_percent)
SC_vector_j <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_50_percent[j]) {SC_vector_j <- c(SC_vector_j, Evaluation_survey_method$SC_2007[i])}
  }
}
SC_vector_j
jj <- as.data.frame(table(SC_vector_j))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2007))
SC_detected_MANAGERS_OPINION_50_percent <- jj[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_50_percent
SC_detected_2007$MANAGERS_OPINION[2] <- SC_detected_MANAGERS_OPINION_50_percent

k <- read.table("CIdata_without_2007_managers_guess_75_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2007_managers_guess_75_percent_of_budget.pdf")
plot(k$X_MARK, k$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_75_percent <- k[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_75_percent <- sort(WPTs_MANAGERS_OPINION_method_75_percent)
SC_vector_k <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_75_percent[j]) {SC_vector_k <- c(SC_vector_k, Evaluation_survey_method$SC_2007[i])}
  }
}
SC_vector_k
kk <- as.data.frame(table(SC_vector_k))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2007))
SC_detected_MANAGERS_OPINION_75_percent <- kk[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_75_percent
SC_detected_2007$MANAGERS_OPINION[3] <- SC_detected_MANAGERS_OPINION_75_percent

m <- read.table("CIdata_without_2007_managers_guess_100_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2007_managers_guess_100_percent_of_budget.pdf")
plot(m$X_MARK, m$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_100_percent <- m[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_100_percent <- sort(WPTs_MANAGERS_OPINION_method_100_percent)
SC_vector_m <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_100_percent[j]) {SC_vector_m <- c(SC_vector_m, Evaluation_survey_method$SC_2007[i])}
  }
}
SC_vector_m
mm <- as.data.frame(table(SC_vector_m))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2007))
SC_detected_MANAGERS_OPINION_100_percent <- mm[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_100_percent
SC_detected_2007$MANAGERS_OPINION[4] <- SC_detected_MANAGERS_OPINION_100_percent

SC_detected_2007 # check

# Survey based on Pr_occ
n <- read.table("CIdata_without_2007_Pr_Occ_25_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2007_Pr_Occ_25_percent_of_budget.pdf")
plot(n$X_MARK, n$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_25_percent <- n[ ,"WPT"]
WPTs_PR_OCC_method_25_percent <- sort(WPTs_PR_OCC_method_25_percent)
SC_vector_n <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_25_percent[j]) {SC_vector_n <- c(SC_vector_n, Evaluation_survey_method$SC_2007[i])}
  }
}
SC_vector_n
nn <- as.data.frame(table(SC_vector_n))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2007))
SC_detected_PR_OCC_25_percent <- nn[2, 2]/ll[2, 2]
SC_detected_PR_OCC_25_percent
SC_detected_2007$PR_OCC[1] <- SC_detected_PR_OCC_25_percent

o <- read.table("CIdata_without_2007_Pr_Occ_50_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2007_Pr_Occ_50_percent_of_budget.pdf")
plot(o$X_MARK, o$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_50_percent <- o[ ,"WPT"]
WPTs_PR_OCC_method_50_percent <- sort(WPTs_PR_OCC_method_50_percent)
SC_vector_o <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_50_percent[j]) {SC_vector_o <- c(SC_vector_o, Evaluation_survey_method$SC_2007[i])}
  }
}
SC_vector_o
oo <- as.data.frame(table(SC_vector_o))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2007))
SC_detected_PR_OCC_50_percent <- oo[2, 2]/ll[2, 2]
SC_detected_PR_OCC_50_percent
SC_detected_2007$PR_OCC[2] <- SC_detected_PR_OCC_50_percent

p <- read.table("CIdata_without_2007_Pr_Occ_75_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2007_Pr_Occ_75_percent_of_budget.pdf")
plot(p$X_MARK, p$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_75_percent <- p[ ,"WPT"]
WPTs_PR_OCC_method_75_percent <- sort(WPTs_PR_OCC_method_75_percent)
SC_vector_p <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_75_percent[j]) {SC_vector_p <- c(SC_vector_p, Evaluation_survey_method$SC_2007[i])}
  }
}
SC_vector_p
pp <- as.data.frame(table(SC_vector_p))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2007))
SC_detected_PR_OCC_75_percent <- pp[2, 2]/ll[2, 2]
SC_detected_PR_OCC_75_percent
SC_detected_2007$PR_OCC[3] <- SC_detected_PR_OCC_75_percent

q <- read.table("CIdata_without_2007_Pr_Occ_100_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2007_Pr_Occ_100_percent_of_budget.pdf")
plot(q$X_MARK, q$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_100_percent <- q[ ,"WPT"]
WPTs_PR_OCC_method_100_percent <- sort(WPTs_PR_OCC_method_100_percent)
SC_vector_q <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_100_percent[j]) {SC_vector_q <- c(SC_vector_q, Evaluation_survey_method$SC_2007[i])}
  }
}
SC_vector_q
qq <- as.data.frame(table(SC_vector_q))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2007))
SC_detected_PR_OCC_100_percent <- qq[2, 2]/ll[2, 2]
SC_detected_PR_OCC_100_percent
SC_detected_2007$PR_OCC[4] <- SC_detected_PR_OCC_100_percent

SC_detected_2007 # check

# Survey based on Knapsack 
r <- read.table("CIdata_without_2007_Knapsack_25_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2007_Knapsack_25_percent_of_budget.pdf")
plot(r$X_MARK, r$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_25_percent <- r[ ,"WPT"]
WPTs_KNAPSACK_method_25_percent <- sort(WPTs_KNAPSACK_method_25_percent)
SC_vector_r <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_25_percent[j]) {SC_vector_r <- c(SC_vector_r, Evaluation_survey_method$SC_2007[i])}
  }
}
SC_vector_r
rr <- as.data.frame(table(SC_vector_r))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2007))
SC_detected_KNAPSACK_25_percent <- rr[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_25_percent
SC_detected_2007$KNAPSACK[1] <- SC_detected_KNAPSACK_25_percent

s <- read.table("CIdata_without_2007_Knapsack_50_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2007_Knapsack_50_percent_of_budget.pdf")
plot(s$X_MARK, s$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_50_percent <- s[ ,"WPT"]
WPTs_KNAPSACK_method_50_percent <- sort(WPTs_KNAPSACK_method_50_percent)
SC_vector_s <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_50_percent[j]) {SC_vector_s <- c(SC_vector_s, Evaluation_survey_method$SC_2007[i])}
  }
}
SC_vector_s
ss <- as.data.frame(table(SC_vector_s))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2007))
SC_detected_KNAPSACK_50_percent <- ss[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_50_percent
SC_detected_2007$KNAPSACK[2] <- SC_detected_KNAPSACK_50_percent

t <- read.table("CIdata_without_2007_Knapsack_75_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2007_Knapsack_75_percent_of_budget.pdf")
plot(t$X_MARK, t$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_75_percent <- t[ ,"WPT"]
WPTs_KNAPSACK_method_75_percent <- sort(WPTs_KNAPSACK_method_75_percent)
SC_vector_t <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_75_percent[j]) {SC_vector_t <- c(SC_vector_t, Evaluation_survey_method$SC_2007[i])}
  }
}
SC_vector_t
tt <- as.data.frame(table(SC_vector_t))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2007))
SC_detected_KNAPSACK_75_percent <- tt[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_75_percent
SC_detected_2007$KNAPSACK[3] <- SC_detected_KNAPSACK_75_percent

u <- read.table("CIdata_without_2007_Knapsack_100_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2007_Knapsack_100_percent_of_budget.pdf")
plot(u$X_MARK, u$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_100_percent <- u[ ,"WPT"]
WPTs_KNAPSACK_method_100_percent <- sort(WPTs_KNAPSACK_method_100_percent)
SC_vector_u <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_100_percent[j]) {SC_vector_u <- c(SC_vector_u, Evaluation_survey_method$SC_2007[i])}
  }
}
SC_vector_u
uu <- as.data.frame(table(SC_vector_u))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2007))
SC_detected_KNAPSACK_100_percent <- uu[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_100_percent
SC_detected_2007$KNAPSACK[4] <- SC_detected_KNAPSACK_100_percent

SC_detected_2007 # check

# Write results
write.table(SC_detected_2007, file="SC_detected_2007.txt",sep="\t",col.names=T,row.names=F)

# Plot results
x <- c(25, 50, 75, 100)
y <- c(SC_detected_2007[ ,1])
plot(x, y, xlim=c(0,100), ylim=c(0,1), xlab="% of total survey effort (island wide survey)", ylab="Effectiveness (% SC found)", main="Performance based on 2007 supercolony distribution")
points(x, SC_detected_2007[ , 2], pch=2)
points(x, SC_detected_2007[ , 3], pch=3)
points(x, SC_detected_2007[ , 4], pch=4)
points(x, SC_detected_2007[ , 5], pch=5)




################################### Evaluate survey performance using 2009 supercolony distribution ####################################
unique(CIdata$Year)
CIdata2009 <- subset(CIdata,CIdata$Year == 2009)
# Create data.frame with WPT, X-mark, Y-mark, SC_2009 (1 or 0)
Evaluation_survey_method <- as.data.frame(matrix(NA, nrow = nrow(CIdata2009), ncol = 4)) # create data.frame 
Evaluation_survey_method[ , 1] <- CIdata2009[ , "WPT"]
Evaluation_survey_method[ , 2] <- CIdata2009[ , "X_MARK"]
Evaluation_survey_method[ , 3] <- CIdata2009[ , "Y_MARK"]
Evaluation_survey_method[ , 4] <- CIdata2009[ , "p_a_SC"]
colnames(Evaluation_survey_method) <- c("WPT", "X_MARK","Y_MARK","SC_2009")
Evaluation_survey_method
# Create dataframe to store survey performance results
SC_detected_2009 <- as.data.frame(matrix(NA, nrow=4, ncol=5))
rownames(SC_detected_2009) <- c("25% total budget", "50% total budget", "75% total budget", "100% total budget")
colnames(SC_detected_2009) <- c("RANDOM_SURVEY", "SYSTEMATIC_SURVEY", "MANAGERS_OPINION", "PR_OCC", "KNAPSACK")

# Random survey 
a <- read.table("CIdata_without_2009_random_sites_25_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2009_random_sites_25_percent.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_25_percent <- a[ ,"WPT"]
WPTs_RANDOM_survey_method_25_percent <- sort(WPTs_RANDOM_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_25_percent[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2009))
SC_detected_RANDOM_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_25_percent
SC_detected_2009$RANDOM_SURVEY[1] <- SC_detected_RANDOM_survey_25_percent


b <- read.table("CIdata_without_2009_random_sites_50_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2009_random_sites_50_percent.pdf")
plot(b$X_MARK, b$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_50_percent <- b[ ,"WPT"]
WPTs_RANDOM_survey_method_50_percent <- sort(WPTs_RANDOM_survey_method_50_percent)
SC_vector_b <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_50_percent[j]) {SC_vector_b <- c(SC_vector_b, Evaluation_survey_method$SC_2009[i])}
  }
}
SC_vector_b
bb <- as.data.frame(table(SC_vector_b))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2009))
SC_detected_RANDOM_survey_50_percent <- bb[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_50_percent
SC_detected_2009$RANDOM_SURVEY[2] <- SC_detected_RANDOM_survey_50_percent


c <- read.table("CIdata_without_2009_random_sites_75_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2009_random_sites_75_percent.pdf")
plot(c$X_MARK, c$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_75_percent <- c[ ,"WPT"]
WPTs_RANDOM_survey_method_75_percent <- sort(WPTs_RANDOM_survey_method_75_percent)
SC_vector_c <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_75_percent[j]) {SC_vector_c <- c(SC_vector_c, Evaluation_survey_method$SC_2009[i])}
  }
}
SC_vector_c
cc <- as.data.frame(table(SC_vector_c))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2009))
SC_detected_RANDOM_survey_75_percent <- cc[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_75_percent
SC_detected_2009$RANDOM_SURVEY[3] <- SC_detected_RANDOM_survey_75_percent

d <- read.table("CIdata_without_2009_random_sites_100_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2009_random_sites_100_percent.pdf")
plot(d$X_MARK, d$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_100_percent <- d[ ,"WPT"]
WPTs_RANDOM_survey_method_100_percent <- sort(WPTs_RANDOM_survey_method_100_percent)
SC_vector_d <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_100_percent[j]) {SC_vector_d <- c(SC_vector_d, Evaluation_survey_method$SC_2009[i])}
  }
}
SC_vector_d
dd <- as.data.frame(table(SC_vector_d))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2009))
SC_detected_RANDOM_survey_100_percent <- dd[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_100_percent
SC_detected_2009$RANDOM_SURVEY[4] <- SC_detected_RANDOM_survey_100_percent


SC_detected_2009 # check



# Systematic survey
e <- read.table("CIdata_without_2009_systematic_survey_25_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2009_systematic_survey_25_percent_budget.pdf")
plot(e$X_MARK, e$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_25_percent <- e[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_25_percent <- sort(WPTs_SYSTEMATIC_survey_method_25_percent)
SC_vector_e <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_25_percent)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_25_percent[j]) {SC_vector_e <- c(SC_vector_e, Evaluation_survey_method$SC_2009[i])}
  }
}
SC_vector_e
ee <- as.data.frame(table(SC_vector_e))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2009))
SC_detected_SYSTEMATIC_survey_25_percent <- ee[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_25_percent
SC_detected_2009$SYSTEMATIC_SURVEY[1] <- SC_detected_SYSTEMATIC_survey_25_percent

f <- read.table("CIdata_without_2009_systematic_survey_50_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2009_systematic_survey_50_percent_budget.pdf")
plot(f$X_MARK, f$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_50_percent <- f[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_50_percent <- sort(WPTs_SYSTEMATIC_survey_method_50_percent)
SC_vector_f <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_50_percent[j]) {SC_vector_f <- c(SC_vector_f, Evaluation_survey_method$SC_2009[i])}
  }
}
SC_vector_f
ff <- as.data.frame(table(SC_vector_f))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2009))
SC_detected_SYSTEMATIC_survey_50_percent <- ff[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_50_percent
SC_detected_2009$SYSTEMATIC_SURVEY[2] <- SC_detected_SYSTEMATIC_survey_50_percent

g <- read.table("CIdata_without_2009_systematic_survey_75_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2009_systematic_survey_75_percent_budget.pdf")
plot(g$X_MARK, g$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_75_percent <- g[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_75_percent <- sort(WPTs_SYSTEMATIC_survey_method_75_percent)
SC_vector_g <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_75_percent[j]) {SC_vector_g <- c(SC_vector_g, Evaluation_survey_method$SC_2009[i])}
  }
}
SC_vector_g
gg <- as.data.frame(table(SC_vector_g))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2009))
SC_detected_SYSTEMATIC_survey_75_percent <- gg[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_75_percent
SC_detected_2009$SYSTEMATIC_SURVEY[3] <- SC_detected_SYSTEMATIC_survey_75_percent

h <- read.table("CIdata_without_2009_systematic_survey_100_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2009_systematic_survey_100_percent_budget.pdf")
plot(h$X_MARK, h$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_100_percent <- h[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_100_percent <- sort(WPTs_SYSTEMATIC_survey_method_100_percent)
SC_vector_h <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_100_percent[j]) {SC_vector_h <- c(SC_vector_h, Evaluation_survey_method$SC_2009[i])}
  }
}
SC_vector_h
hh <- as.data.frame(table(SC_vector_h))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2009))
SC_detected_SYSTEMATIC_survey_100_percent <- hh[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_100_percent
SC_detected_2009$SYSTEMATIC_SURVEY[4] <- SC_detected_SYSTEMATIC_survey_100_percent


SC_detected_2009 # check


# Survey based on manager's opinion
i <- read.table("CIdata_without_2009_managers_guess_25_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2009_managers_guess_25_percent_of_budget.pdf")
plot(i$X_MARK, i$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_25_percent <- i[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_25_percent <- sort(WPTs_MANAGERS_OPINION_method_25_percent)
SC_vector_i <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_25_percent)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_25_percent[j]) {SC_vector_i <- c(SC_vector_i, Evaluation_survey_method$SC_2009[i])}
  }
}
SC_vector_i
ii <- as.data.frame(table(SC_vector_i))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2009))
SC_detected_MANAGERS_OPINION_25_percent <- ii[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_25_percent
SC_detected_2009$MANAGERS_OPINION[1] <- SC_detected_MANAGERS_OPINION_25_percent


j <- read.table("CIdata_without_2009_managers_guess_50_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2009_managers_guess_50_percent_of_budget.pdf")
plot(j$X_MARK, j$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_50_percent <- j[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_50_percent <- sort(WPTs_MANAGERS_OPINION_method_50_percent)
SC_vector_j <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_50_percent[j]) {SC_vector_j <- c(SC_vector_j, Evaluation_survey_method$SC_2009[i])}
  }
}
SC_vector_j
jj <- as.data.frame(table(SC_vector_j))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2009))
SC_detected_MANAGERS_OPINION_50_percent <- jj[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_50_percent
SC_detected_2009$MANAGERS_OPINION[2] <- SC_detected_MANAGERS_OPINION_50_percent

k <- read.table("CIdata_without_2009_managers_guess_75_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2009_managers_guess_75_percent_of_budget.pdf")
plot(k$X_MARK, k$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_75_percent <- k[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_75_percent <- sort(WPTs_MANAGERS_OPINION_method_75_percent)
SC_vector_k <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_75_percent[j]) {SC_vector_k <- c(SC_vector_k, Evaluation_survey_method$SC_2009[i])}
  }
}
SC_vector_k
kk <- as.data.frame(table(SC_vector_k))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2009))
SC_detected_MANAGERS_OPINION_75_percent <- kk[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_75_percent
SC_detected_2009$MANAGERS_OPINION[3] <- SC_detected_MANAGERS_OPINION_75_percent

m <- read.table("CIdata_without_2009_managers_guess_100_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2009_managers_guess_100_percent_of_budget.pdf")
plot(m$X_MARK, m$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_100_percent <- m[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_100_percent <- sort(WPTs_MANAGERS_OPINION_method_100_percent)
SC_vector_m <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_100_percent[j]) {SC_vector_m <- c(SC_vector_m, Evaluation_survey_method$SC_2009[i])}
  }
}
SC_vector_m
mm <- as.data.frame(table(SC_vector_m))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2009))
SC_detected_MANAGERS_OPINION_100_percent <- mm[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_100_percent
SC_detected_2009$MANAGERS_OPINION[4] <- SC_detected_MANAGERS_OPINION_100_percent

SC_detected_2009 # check

# Survey based on Pr_occ
n <- read.table("CIdata_without_2009_Pr_Occ_25_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2009_Pr_Occ_25_percent_of_budget.pdf")
plot(n$X_MARK, n$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_25_percent <- n[ ,"WPT"]
WPTs_PR_OCC_method_25_percent <- sort(WPTs_PR_OCC_method_25_percent)
SC_vector_n <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_25_percent)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_25_percent[j]) {SC_vector_n <- c(SC_vector_n, Evaluation_survey_method$SC_2009[i])}
  }
}
SC_vector_n
nn <- as.data.frame(table(SC_vector_n))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2009))
SC_detected_PR_OCC_25_percent <- nn[2, 2]/ll[2, 2]
SC_detected_PR_OCC_25_percent
SC_detected_2009$PR_OCC[1] <- SC_detected_PR_OCC_25_percent

o <- read.table("CIdata_without_2009_Pr_Occ_50_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2009_Pr_Occ_50_percent_of_budget.pdf")
plot(o$X_MARK, o$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_50_percent <- o[ ,"WPT"]
WPTs_PR_OCC_method_50_percent <- sort(WPTs_PR_OCC_method_50_percent)
SC_vector_o <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_50_percent[j]) {SC_vector_o <- c(SC_vector_o, Evaluation_survey_method$SC_2009[i])}
  }
}
SC_vector_o
oo <- as.data.frame(table(SC_vector_o))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2009))
SC_detected_PR_OCC_50_percent <- oo[2, 2]/ll[2, 2]
SC_detected_PR_OCC_50_percent
SC_detected_2009$PR_OCC[2] <- SC_detected_PR_OCC_50_percent

p <- read.table("CIdata_without_2009_Pr_Occ_75_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2009_Pr_Occ_75_percent_of_budget.pdf")
plot(p$X_MARK, p$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_75_percent <- p[ ,"WPT"]
WPTs_PR_OCC_method_75_percent <- sort(WPTs_PR_OCC_method_75_percent)
SC_vector_p <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_75_percent[j]) {SC_vector_p <- c(SC_vector_p, Evaluation_survey_method$SC_2009[i])}
  }
}
SC_vector_p
pp <- as.data.frame(table(SC_vector_p))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2009))
SC_detected_PR_OCC_75_percent <- pp[2, 2]/ll[2, 2]
SC_detected_PR_OCC_75_percent
SC_detected_2009$PR_OCC[3] <- SC_detected_PR_OCC_75_percent

q <- read.table("CIdata_without_2009_Pr_Occ_100_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2009_Pr_Occ_100_percent_of_budget.pdf")
plot(q$X_MARK, q$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_100_percent <- q[ ,"WPT"]
WPTs_PR_OCC_method_100_percent <- sort(WPTs_PR_OCC_method_100_percent)
SC_vector_q <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_100_percent[j]) {SC_vector_q <- c(SC_vector_q, Evaluation_survey_method$SC_2009[i])}
  }
}
SC_vector_q
qq <- as.data.frame(table(SC_vector_q))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2009))
SC_detected_PR_OCC_100_percent <- qq[2, 2]/ll[2, 2]
SC_detected_PR_OCC_100_percent
SC_detected_2009$PR_OCC[4] <- SC_detected_PR_OCC_100_percent

SC_detected_2009 # check

# Survey based on Knapsack 
r <- read.table("CIdata_without_2009_Knapsack_25_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2009_Knapsack_25_percent_of_budget.pdf")
plot(r$X_MARK, r$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_25_percent <- r[ ,"WPT"]
WPTs_KNAPSACK_method_25_percent <- sort(WPTs_KNAPSACK_method_25_percent)
SC_vector_r <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_25_percent[j]) {SC_vector_r <- c(SC_vector_r, Evaluation_survey_method$SC_2009[i])}
  }
}
SC_vector_r
rr <- as.data.frame(table(SC_vector_r))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2009))
SC_detected_KNAPSACK_25_percent <- rr[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_25_percent
SC_detected_2009$KNAPSACK[1] <- SC_detected_KNAPSACK_25_percent

s <- read.table("CIdata_without_2009_Knapsack_50_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2009_Knapsack_50_percent_of_budget.pdf")
plot(s$X_MARK, s$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_50_percent <- s[ ,"WPT"]
WPTs_KNAPSACK_method_50_percent <- sort(WPTs_KNAPSACK_method_50_percent)
SC_vector_s <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_50_percent[j]) {SC_vector_s <- c(SC_vector_s, Evaluation_survey_method$SC_2009[i])}
  }
}
SC_vector_s
ss <- as.data.frame(table(SC_vector_s))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2009))
SC_detected_KNAPSACK_50_percent <- ss[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_50_percent
SC_detected_2009$KNAPSACK[2] <- SC_detected_KNAPSACK_50_percent

t <- read.table("CIdata_without_2009_Knapsack_75_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2009_Knapsack_75_percent_of_budget.pdf")
plot(t$X_MARK, t$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_75_percent <- t[ ,"WPT"]
WPTs_KNAPSACK_method_75_percent <- sort(WPTs_KNAPSACK_method_75_percent)
SC_vector_t <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_75_percent[j]) {SC_vector_t <- c(SC_vector_t, Evaluation_survey_method$SC_2009[i])}
  }
}
SC_vector_t
tt <- as.data.frame(table(SC_vector_t))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2009))
SC_detected_KNAPSACK_75_percent <- tt[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_75_percent
SC_detected_2009$KNAPSACK[3] <- SC_detected_KNAPSACK_75_percent

u <- read.table("CIdata_without_2009_Knapsack_100_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2009_Knapsack_100_percent_of_budget.pdf")
plot(u$X_MARK, u$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_100_percent <- u[ ,"WPT"]
WPTs_KNAPSACK_method_100_percent <- sort(WPTs_KNAPSACK_method_100_percent)
SC_vector_u <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_100_percent[j]) {SC_vector_u <- c(SC_vector_u, Evaluation_survey_method$SC_2009[i])}
  }
}
SC_vector_u
uu <- as.data.frame(table(SC_vector_u))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2009))
SC_detected_KNAPSACK_100_percent <- uu[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_100_percent
SC_detected_2009$KNAPSACK[4] <- SC_detected_KNAPSACK_100_percent

SC_detected_2009 # check

# Write results
write.table(SC_detected_2009, file="SC_detected_2009.txt",sep="\t",col.names=T,row.names=F)

# Plot results
x <- c(25, 50, 75, 100)
y <- c(SC_detected_2009[ ,1])
plot(x, y, xlim=c(0,100), ylim=c(0,1), xlab="% of total survey effort (island wide survey)", ylab="Effectiveness (% SC found)", main="Performance based on 2009 supercolony distribution")
points(x, SC_detected_2009[ , 2], pch=2)
points(x, SC_detected_2009[ , 3], pch=3)
points(x, SC_detected_2009[ , 4], pch=4)
points(x, SC_detected_2009[ , 5], pch=5)


################################### Evaluate survey performance using 2011 supercolony distribution ####################################
unique(CIdata$Year)
CIdata2011 <- subset(CIdata,CIdata$Year == 2011)
# Create data.frame with WPT, X-mark, Y-mark, SC_2011 (1 or 0)
Evaluation_survey_method <- as.data.frame(matrix(NA, nrow = nrow(CIdata2011), ncol = 4)) # create data.frame 
Evaluation_survey_method[ , 1] <- CIdata2011[ , "WPT"]
Evaluation_survey_method[ , 2] <- CIdata2011[ , "X_MARK"]
Evaluation_survey_method[ , 3] <- CIdata2011[ , "Y_MARK"]
Evaluation_survey_method[ , 4] <- CIdata2011[ , "p_a_SC"]
colnames(Evaluation_survey_method) <- c("WPT", "X_MARK","Y_MARK","SC_2011")
Evaluation_survey_method
# Create dataframe to store survey performance results
SC_detected_2011 <- as.data.frame(matrix(NA, nrow=4, ncol=5))
rownames(SC_detected_2011) <- c("25% total budget", "50% total budget", "75% total budget", "100% total budget")
colnames(SC_detected_2011) <- c("RANDOM_SURVEY", "SYSTEMATIC_SURVEY", "MANAGERS_OPINION", "PR_OCC", "KNAPSACK")

# Random survey 
a <- read.table("CIdata_without_2011_random_sites_25_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2011_random_sites_25_percent.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_25_percent <- a[ ,"WPT"]
WPTs_RANDOM_survey_method_25_percent <- sort(WPTs_RANDOM_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_25_percent[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2011))
SC_detected_RANDOM_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_25_percent
SC_detected_2011$RANDOM_SURVEY[1] <- SC_detected_RANDOM_survey_25_percent


b <- read.table("CIdata_without_2011_random_sites_50_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2011_random_sites_50_percent.pdf")
plot(b$X_MARK, b$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_50_percent <- b[ ,"WPT"]
WPTs_RANDOM_survey_method_50_percent <- sort(WPTs_RANDOM_survey_method_50_percent)
SC_vector_b <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_50_percent[j]) {SC_vector_b <- c(SC_vector_b, Evaluation_survey_method$SC_2011[i])}
  }
}
SC_vector_b
bb <- as.data.frame(table(SC_vector_b))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2011))
SC_detected_RANDOM_survey_50_percent <- bb[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_50_percent
SC_detected_2011$RANDOM_SURVEY[2] <- SC_detected_RANDOM_survey_50_percent


c <- read.table("CIdata_without_2011_random_sites_75_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2011_random_sites_75_percent.pdf")
plot(c$X_MARK, c$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_75_percent <- c[ ,"WPT"]
WPTs_RANDOM_survey_method_75_percent <- sort(WPTs_RANDOM_survey_method_75_percent)
SC_vector_c <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_75_percent[j]) {SC_vector_c <- c(SC_vector_c, Evaluation_survey_method$SC_2011[i])}
  }
}
SC_vector_c
cc <- as.data.frame(table(SC_vector_c))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2011))
SC_detected_RANDOM_survey_75_percent <- cc[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_75_percent
SC_detected_2011$RANDOM_SURVEY[3] <- SC_detected_RANDOM_survey_75_percent

d <- read.table("CIdata_without_2011_random_sites_100_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2011_random_sites_100_percent.pdf")
plot(d$X_MARK, d$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_100_percent <- d[ ,"WPT"]
WPTs_RANDOM_survey_method_100_percent <- sort(WPTs_RANDOM_survey_method_100_percent)
SC_vector_d <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_100_percent[j]) {SC_vector_d <- c(SC_vector_d, Evaluation_survey_method$SC_2011[i])}
  }
}
SC_vector_d
dd <- as.data.frame(table(SC_vector_d))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2011))
SC_detected_RANDOM_survey_100_percent <- dd[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_100_percent
SC_detected_2011$RANDOM_SURVEY[4] <- SC_detected_RANDOM_survey_100_percent


SC_detected_2011 # check



# Systematic survey
e <- read.table("CIdata_without_2011_systematic_survey_25_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2011_systematic_survey_25_percent_budget.pdf")
plot(e$X_MARK, e$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_25_percent <- e[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_25_percent <- sort(WPTs_SYSTEMATIC_survey_method_25_percent)
SC_vector_e <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_25_percent[j]) {SC_vector_e <- c(SC_vector_e, Evaluation_survey_method$SC_2011[i])}
  }
}
SC_vector_e
ee <- as.data.frame(table(SC_vector_e))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2011))
SC_detected_SYSTEMATIC_survey_25_percent <- ee[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_25_percent
SC_detected_2011$SYSTEMATIC_SURVEY[1] <- SC_detected_SYSTEMATIC_survey_25_percent

f <- read.table("CIdata_without_2011_systematic_survey_50_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2011_systematic_survey_50_percent_budget.pdf")
plot(f$X_MARK, f$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_50_percent <- f[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_50_percent <- sort(WPTs_SYSTEMATIC_survey_method_50_percent)
SC_vector_f <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_50_percent[j]) {SC_vector_f <- c(SC_vector_f, Evaluation_survey_method$SC_2011[i])}
  }
}
SC_vector_f
ff <- as.data.frame(table(SC_vector_f))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2011))
SC_detected_SYSTEMATIC_survey_50_percent <- ff[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_50_percent
SC_detected_2011$SYSTEMATIC_SURVEY[2] <- SC_detected_SYSTEMATIC_survey_50_percent

g <- read.table("CIdata_without_2011_systematic_survey_75_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2011_systematic_survey_75_percent_budget.pdf")
plot(g$X_MARK, g$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_75_percent <- g[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_75_percent <- sort(WPTs_SYSTEMATIC_survey_method_75_percent)
SC_vector_g <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_75_percent[j]) {SC_vector_g <- c(SC_vector_g, Evaluation_survey_method$SC_2011[i])}
  }
}
SC_vector_g
gg <- as.data.frame(table(SC_vector_g))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2011))
SC_detected_SYSTEMATIC_survey_75_percent <- gg[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_75_percent
SC_detected_2011$SYSTEMATIC_SURVEY[3] <- SC_detected_SYSTEMATIC_survey_75_percent

h <- read.table("CIdata_without_2011_systematic_survey_100_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2011_systematic_survey_100_percent_budget.pdf")
plot(h$X_MARK, h$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_100_percent <- h[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_100_percent <- sort(WPTs_SYSTEMATIC_survey_method_100_percent)
SC_vector_h <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_100_percent[j]) {SC_vector_h <- c(SC_vector_h, Evaluation_survey_method$SC_2011[i])}
  }
}
SC_vector_h
hh <- as.data.frame(table(SC_vector_h))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2011))
SC_detected_SYSTEMATIC_survey_100_percent <- hh[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_100_percent
SC_detected_2011$SYSTEMATIC_SURVEY[4] <- SC_detected_SYSTEMATIC_survey_100_percent


SC_detected_2011 # check


# Survey based on manager's opinion
i <- read.table("CIdata_without_2011_managers_guess_25_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2011_managers_guess_25_percent_of_budget.pdf")
plot(i$X_MARK, i$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_25_percent <- i[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_25_percent <- sort(WPTs_MANAGERS_OPINION_method_25_percent)
SC_vector_i <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_25_percent)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_25_percent[j]) {SC_vector_i <- c(SC_vector_i, Evaluation_survey_method$SC_2011[i])}
  }
}
SC_vector_i
ii <- as.data.frame(table(SC_vector_i))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2011))
SC_detected_MANAGERS_OPINION_25_percent <- ii[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_25_percent
SC_detected_2011$MANAGERS_OPINION[1] <- SC_detected_MANAGERS_OPINION_25_percent


j <- read.table("CIdata_without_2011_managers_guess_50_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2011_managers_guess_50_percent_of_budget.pdf")
plot(j$X_MARK, j$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_50_percent <- j[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_50_percent <- sort(WPTs_MANAGERS_OPINION_method_50_percent)
SC_vector_j <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_50_percent[j]) {SC_vector_j <- c(SC_vector_j, Evaluation_survey_method$SC_2011[i])}
  }
}
SC_vector_j
jj <- as.data.frame(table(SC_vector_j))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2011))
SC_detected_MANAGERS_OPINION_50_percent <- jj[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_50_percent
SC_detected_2011$MANAGERS_OPINION[2] <- SC_detected_MANAGERS_OPINION_50_percent

k <- read.table("CIdata_without_2011_managers_guess_75_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2011_managers_guess_75_percent_of_budget.pdf")
plot(k$X_MARK, k$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_75_percent <- k[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_75_percent <- sort(WPTs_MANAGERS_OPINION_method_75_percent)
SC_vector_k <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_75_percent[j]) {SC_vector_k <- c(SC_vector_k, Evaluation_survey_method$SC_2011[i])}
  }
}
SC_vector_k
kk <- as.data.frame(table(SC_vector_k))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2011))
SC_detected_MANAGERS_OPINION_75_percent <- kk[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_75_percent
SC_detected_2011$MANAGERS_OPINION[3] <- SC_detected_MANAGERS_OPINION_75_percent

m <- read.table("CIdata_without_2011_managers_guess_100_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2011_managers_guess_100_percent_of_budget.pdf")
plot(m$X_MARK, m$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_100_percent <- m[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_100_percent <- sort(WPTs_MANAGERS_OPINION_method_100_percent)
SC_vector_m <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_100_percent[j]) {SC_vector_m <- c(SC_vector_m, Evaluation_survey_method$SC_2011[i])}
  }
}
SC_vector_m
mm <- as.data.frame(table(SC_vector_m))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2011))
SC_detected_MANAGERS_OPINION_100_percent <- mm[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_100_percent
SC_detected_2011$MANAGERS_OPINION[4] <- SC_detected_MANAGERS_OPINION_100_percent

SC_detected_2011 # check

# Survey based on Pr_occ
n <- read.table("CIdata_without_2011_Pr_Occ_25_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2011_Pr_Occ_25_percent_of_budget.pdf")
plot(n$X_MARK, n$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_25_percent <- n[ ,"WPT"]
WPTs_PR_OCC_method_25_percent <- sort(WPTs_PR_OCC_method_25_percent)
SC_vector_n <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_25_percent)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_25_percent[j]) {SC_vector_n <- c(SC_vector_n, Evaluation_survey_method$SC_2011[i])}
  }
}
SC_vector_n
nn <- as.data.frame(table(SC_vector_n))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2011))
SC_detected_PR_OCC_25_percent <- nn[2, 2]/ll[2, 2]
SC_detected_PR_OCC_25_percent
SC_detected_2011$PR_OCC[1] <- SC_detected_PR_OCC_25_percent

o <- read.table("CIdata_without_2011_Pr_Occ_50_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2011_Pr_Occ_50_percent_of_budget.pdf")
plot(o$X_MARK, o$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_50_percent <- o[ ,"WPT"]
WPTs_PR_OCC_method_50_percent <- sort(WPTs_PR_OCC_method_50_percent)
SC_vector_o <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_50_percent[j]) {SC_vector_o <- c(SC_vector_o, Evaluation_survey_method$SC_2011[i])}
  }
}
SC_vector_o
oo <- as.data.frame(table(SC_vector_o))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2011))
SC_detected_PR_OCC_50_percent <- oo[2, 2]/ll[2, 2]
SC_detected_PR_OCC_50_percent
SC_detected_2011$PR_OCC[2] <- SC_detected_PR_OCC_50_percent

p <- read.table("CIdata_without_2011_Pr_Occ_75_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2011_Pr_Occ_75_percent_of_budget.pdf")
plot(p$X_MARK, p$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_75_percent <- p[ ,"WPT"]
WPTs_PR_OCC_method_75_percent <- sort(WPTs_PR_OCC_method_75_percent)
SC_vector_p <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_75_percent[j]) {SC_vector_p <- c(SC_vector_p, Evaluation_survey_method$SC_2011[i])}
  }
}
SC_vector_p
pp <- as.data.frame(table(SC_vector_p))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2011))
SC_detected_PR_OCC_75_percent <- pp[2, 2]/ll[2, 2]
SC_detected_PR_OCC_75_percent
SC_detected_2011$PR_OCC[3] <- SC_detected_PR_OCC_75_percent

q <- read.table("CIdata_without_2011_Pr_Occ_100_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2011_Pr_Occ_100_percent_of_budget.pdf")
plot(q$X_MARK, q$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_100_percent <- q[ ,"WPT"]
WPTs_PR_OCC_method_100_percent <- sort(WPTs_PR_OCC_method_100_percent)
SC_vector_q <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_100_percent[j]) {SC_vector_q <- c(SC_vector_q, Evaluation_survey_method$SC_2011[i])}
  }
}
SC_vector_q
qq <- as.data.frame(table(SC_vector_q))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2011))
SC_detected_PR_OCC_100_percent <- qq[2, 2]/ll[2, 2]
SC_detected_PR_OCC_100_percent
SC_detected_2011$PR_OCC[4] <- SC_detected_PR_OCC_100_percent

SC_detected_2011 # check

# Survey based on Knapsack 
r <- read.table("CIdata_without_2011_Knapsack_25_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2011_Knapsack_25_percent_of_budget.pdf")
plot(r$X_MARK, r$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_25_percent <- r[ ,"WPT"]
WPTs_KNAPSACK_method_25_percent <- sort(WPTs_KNAPSACK_method_25_percent)
SC_vector_r <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_25_percent[j]) {SC_vector_r <- c(SC_vector_r, Evaluation_survey_method$SC_2011[i])}
  }
}
SC_vector_r
rr <- as.data.frame(table(SC_vector_r))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2011))
SC_detected_KNAPSACK_25_percent <- rr[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_25_percent
SC_detected_2011$KNAPSACK[1] <- SC_detected_KNAPSACK_25_percent

s <- read.table("CIdata_without_2011_Knapsack_50_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2011_Knapsack_50_percent_of_budget.pdf")
plot(s$X_MARK, s$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_50_percent <- s[ ,"WPT"]
WPTs_KNAPSACK_method_50_percent <- sort(WPTs_KNAPSACK_method_50_percent)
SC_vector_s <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_50_percent[j]) {SC_vector_s <- c(SC_vector_s, Evaluation_survey_method$SC_2011[i])}
  }
}
SC_vector_s
ss <- as.data.frame(table(SC_vector_s))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2011))
SC_detected_KNAPSACK_50_percent <- ss[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_50_percent
SC_detected_2011$KNAPSACK[2] <- SC_detected_KNAPSACK_50_percent

t <- read.table("CIdata_without_2011_Knapsack_75_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2011_Knapsack_75_percent_of_budget.pdf")
plot(t$X_MARK, t$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_75_percent <- t[ ,"WPT"]
WPTs_KNAPSACK_method_75_percent <- sort(WPTs_KNAPSACK_method_75_percent)
SC_vector_t <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_75_percent[j]) {SC_vector_t <- c(SC_vector_t, Evaluation_survey_method$SC_2011[i])}
  }
}
SC_vector_t
tt <- as.data.frame(table(SC_vector_t))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2011))
SC_detected_KNAPSACK_75_percent <- tt[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_75_percent
SC_detected_2011$KNAPSACK[3] <- SC_detected_KNAPSACK_75_percent

u <- read.table("CIdata_without_2011_Knapsack_100_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2011_Knapsack_100_percent_of_budget.pdf")
plot(u$X_MARK, u$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_100_percent <- u[ ,"WPT"]
WPTs_KNAPSACK_method_100_percent <- sort(WPTs_KNAPSACK_method_100_percent)
SC_vector_u <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_100_percent[j]) {SC_vector_u <- c(SC_vector_u, Evaluation_survey_method$SC_2011[i])}
  }
}
SC_vector_u
uu <- as.data.frame(table(SC_vector_u))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2011))
SC_detected_KNAPSACK_100_percent <- uu[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_100_percent
SC_detected_2011$KNAPSACK[4] <- SC_detected_KNAPSACK_100_percent

SC_detected_2011 # check

# Write results
write.table(SC_detected_2011, file="SC_detected_2011.txt",sep="\t",col.names=T,row.names=F)

# Plot results
x <- c(25, 50, 75, 100)
y <- c(SC_detected_2011[ ,1])
plot(x, y, xlim=c(0,100), ylim=c(0,1), xlab="% of total survey effort (island wide survey)", ylab="Effectiveness (% SC found)", main="Performance based on 2011 supercolony distribution")
points(x, SC_detected_2011[ , 2], pch=2)
points(x, SC_detected_2011[ , 3], pch=3)
points(x, SC_detected_2011[ , 4], pch=4)
points(x, SC_detected_2011[ , 5], pch=5)



################################### Evaluate survey performance using 2013 supercolony distribution ####################################
unique(CIdata$Year)
CIdata2013 <- subset(CIdata,CIdata$Year == 2013)
# Create data.frame with WPT, X-mark, Y-mark, SC_2013 (1 or 0)
Evaluation_survey_method <- as.data.frame(matrix(NA, nrow = nrow(CIdata2013), ncol = 4)) # create data.frame 
Evaluation_survey_method[ , 1] <- CIdata2013[ , "WPT"]
Evaluation_survey_method[ , 2] <- CIdata2013[ , "X_MARK"]
Evaluation_survey_method[ , 3] <- CIdata2013[ , "Y_MARK"]
Evaluation_survey_method[ , 4] <- CIdata2013[ , "p_a_SC"]
colnames(Evaluation_survey_method) <- c("WPT", "X_MARK","Y_MARK","SC_2013")
Evaluation_survey_method
# Create dataframe to store survey performance results
SC_detected_2013 <- as.data.frame(matrix(NA, nrow=4, ncol=5))
rownames(SC_detected_2013) <- c("25% total budget", "50% total budget", "75% total budget", "100% total budget")
colnames(SC_detected_2013) <- c("RANDOM_SURVEY", "SYSTEMATIC_SURVEY", "MANAGERS_OPINION", "PR_OCC", "KNAPSACK")

# Random survey 
a <- read.table("CIdata_without_2013_random_sites_25_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2013_random_sites_25_percent.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_25_percent <- a[ ,"WPT"]
WPTs_RANDOM_survey_method_25_percent <- sort(WPTs_RANDOM_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_25_percent[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2013))
SC_detected_RANDOM_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_25_percent
SC_detected_2013$RANDOM_SURVEY[1] <- SC_detected_RANDOM_survey_25_percent


b <- read.table("CIdata_without_2013_random_sites_50_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2013_random_sites_50_percent.pdf")
plot(b$X_MARK, b$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_50_percent <- b[ ,"WPT"]
WPTs_RANDOM_survey_method_50_percent <- sort(WPTs_RANDOM_survey_method_50_percent)
SC_vector_b <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_50_percent[j]) {SC_vector_b <- c(SC_vector_b, Evaluation_survey_method$SC_2013[i])}
  }
}
SC_vector_b
bb <- as.data.frame(table(SC_vector_b))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2013))
SC_detected_RANDOM_survey_50_percent <- bb[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_50_percent
SC_detected_2013$RANDOM_SURVEY[2] <- SC_detected_RANDOM_survey_50_percent


c <- read.table("CIdata_without_2013_random_sites_75_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2013_random_sites_75_percent.pdf")
plot(c$X_MARK, c$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_75_percent <- c[ ,"WPT"]
WPTs_RANDOM_survey_method_75_percent <- sort(WPTs_RANDOM_survey_method_75_percent)
SC_vector_c <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_75_percent[j]) {SC_vector_c <- c(SC_vector_c, Evaluation_survey_method$SC_2013[i])}
  }
}
SC_vector_c
cc <- as.data.frame(table(SC_vector_c))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2013))
SC_detected_RANDOM_survey_75_percent <- cc[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_75_percent
SC_detected_2013$RANDOM_SURVEY[3] <- SC_detected_RANDOM_survey_75_percent

d <- read.table("CIdata_without_2013_random_sites_100_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2013_random_sites_100_percent.pdf")
plot(d$X_MARK, d$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_100_percent <- d[ ,"WPT"]
WPTs_RANDOM_survey_method_100_percent <- sort(WPTs_RANDOM_survey_method_100_percent)
SC_vector_d <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_100_percent[j]) {SC_vector_d <- c(SC_vector_d, Evaluation_survey_method$SC_2013[i])}
  }
}
SC_vector_d
dd <- as.data.frame(table(SC_vector_d))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2013))
SC_detected_RANDOM_survey_100_percent <- dd[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_100_percent
SC_detected_2013$RANDOM_SURVEY[4] <- SC_detected_RANDOM_survey_100_percent


SC_detected_2013 # check



# Systematic survey
e <- read.table("CIdata_without_2013_systematic_survey_25_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2013_systematic_survey_25_percent_budget.pdf")
plot(e$X_MARK, e$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_25_percent <- e[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_25_percent <- sort(WPTs_SYSTEMATIC_survey_method_25_percent)
SC_vector_e <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_25_percent[j]) {SC_vector_e <- c(SC_vector_e, Evaluation_survey_method$SC_2013[i])}
  }
}
SC_vector_e
ee <- as.data.frame(table(SC_vector_e))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2013))
SC_detected_SYSTEMATIC_survey_25_percent <- ee[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_25_percent
SC_detected_2013$SYSTEMATIC_SURVEY[1] <- SC_detected_SYSTEMATIC_survey_25_percent

f <- read.table("CIdata_without_2013_systematic_survey_50_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2013_systematic_survey_50_percent_budget.pdf")
plot(f$X_MARK, f$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_50_percent <- f[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_50_percent <- sort(WPTs_SYSTEMATIC_survey_method_50_percent)
SC_vector_f <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_50_percent[j]) {SC_vector_f <- c(SC_vector_f, Evaluation_survey_method$SC_2013[i])}
  }
}
SC_vector_f
ff <- as.data.frame(table(SC_vector_f))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2013))
SC_detected_SYSTEMATIC_survey_50_percent <- ff[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_50_percent
SC_detected_2013$SYSTEMATIC_SURVEY[2] <- SC_detected_SYSTEMATIC_survey_50_percent

g <- read.table("CIdata_without_2013_systematic_survey_75_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2013_systematic_survey_75_percent_budget.pdf")
plot(g$X_MARK, g$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_75_percent <- g[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_75_percent <- sort(WPTs_SYSTEMATIC_survey_method_75_percent)
SC_vector_g <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_75_percent[j]) {SC_vector_g <- c(SC_vector_g, Evaluation_survey_method$SC_2013[i])}
  }
}
SC_vector_g
gg <- as.data.frame(table(SC_vector_g))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2013))
SC_detected_SYSTEMATIC_survey_75_percent <- gg[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_75_percent
SC_detected_2013$SYSTEMATIC_SURVEY[3] <- SC_detected_SYSTEMATIC_survey_75_percent

h <- read.table("CIdata_without_2013_systematic_survey_100_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2013_systematic_survey_100_percent_budget.pdf")
plot(h$X_MARK, h$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_100_percent <- h[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_100_percent <- sort(WPTs_SYSTEMATIC_survey_method_100_percent)
SC_vector_h <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_100_percent[j]) {SC_vector_h <- c(SC_vector_h, Evaluation_survey_method$SC_2013[i])}
  }
}
SC_vector_h
hh <- as.data.frame(table(SC_vector_h))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2013))
SC_detected_SYSTEMATIC_survey_100_percent <- hh[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_100_percent
SC_detected_2013$SYSTEMATIC_SURVEY[4] <- SC_detected_SYSTEMATIC_survey_100_percent


SC_detected_2013 # check


# Survey based on manager's opinion
i <- read.table("CIdata_without_2013_managers_guess_25_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2013_managers_guess_25_percent_of_budget.pdf")
plot(i$X_MARK, i$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_25_percent <- i[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_25_percent <- sort(WPTs_MANAGERS_OPINION_method_25_percent)
SC_vector_i <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_25_percent[j]) {SC_vector_i <- c(SC_vector_i, Evaluation_survey_method$SC_2013[i])}
  }
}
SC_vector_i
ii <- as.data.frame(table(SC_vector_i))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2013))
SC_detected_MANAGERS_OPINION_25_percent <- ii[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_25_percent
SC_detected_2013$MANAGERS_OPINION[1] <- SC_detected_MANAGERS_OPINION_25_percent


j <- read.table("CIdata_without_2013_managers_guess_50_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2013_managers_guess_50_percent_of_budget.pdf")
plot(j$X_MARK, j$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_50_percent <- j[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_50_percent <- sort(WPTs_MANAGERS_OPINION_method_50_percent)
SC_vector_j <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_50_percent[j]) {SC_vector_j <- c(SC_vector_j, Evaluation_survey_method$SC_2013[i])}
  }
}
SC_vector_j
jj <- as.data.frame(table(SC_vector_j))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2013))
SC_detected_MANAGERS_OPINION_50_percent <- jj[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_50_percent
SC_detected_2013$MANAGERS_OPINION[2] <- SC_detected_MANAGERS_OPINION_50_percent

k <- read.table("CIdata_without_2013_managers_guess_75_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2013_managers_guess_75_percent_of_budget.pdf")
plot(k$X_MARK, k$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_75_percent <- k[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_75_percent <- sort(WPTs_MANAGERS_OPINION_method_75_percent)
SC_vector_k <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_75_percent[j]) {SC_vector_k <- c(SC_vector_k, Evaluation_survey_method$SC_2013[i])}
  }
}
SC_vector_k
kk <- as.data.frame(table(SC_vector_k))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2013))
SC_detected_MANAGERS_OPINION_75_percent <- kk[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_75_percent
SC_detected_2013$MANAGERS_OPINION[3] <- SC_detected_MANAGERS_OPINION_75_percent

m <- read.table("CIdata_without_2013_managers_guess_100_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2013_managers_guess_100_percent_of_budget.pdf")
plot(m$X_MARK, m$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_100_percent <- m[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_100_percent <- sort(WPTs_MANAGERS_OPINION_method_100_percent)
SC_vector_m <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_100_percent[j]) {SC_vector_m <- c(SC_vector_m, Evaluation_survey_method$SC_2013[i])}
  }
}
SC_vector_m
mm <- as.data.frame(table(SC_vector_m))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2013))
SC_detected_MANAGERS_OPINION_100_percent <- mm[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_100_percent
SC_detected_2013$MANAGERS_OPINION[4] <- SC_detected_MANAGERS_OPINION_100_percent

SC_detected_2013 # check

# Survey based on Pr_occ
n <- read.table("CIdata_without_2013_Pr_Occ_25_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2013_Pr_Occ_25_percent_of_budget.pdf")
plot(n$X_MARK, n$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_25_percent <- n[ ,"WPT"]
WPTs_PR_OCC_method_25_percent <- sort(WPTs_PR_OCC_method_25_percent)
SC_vector_n <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_25_percent[j]) {SC_vector_n <- c(SC_vector_n, Evaluation_survey_method$SC_2013[i])}
  }
}
SC_vector_n
nn <- as.data.frame(table(SC_vector_n))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2013))
SC_detected_PR_OCC_25_percent <- nn[2, 2]/ll[2, 2]
SC_detected_PR_OCC_25_percent
SC_detected_2013$PR_OCC[1] <- SC_detected_PR_OCC_25_percent

o <- read.table("CIdata_without_2013_Pr_Occ_50_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2013_Pr_Occ_50_percent_of_budget.pdf")
plot(o$X_MARK, o$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_50_percent <- o[ ,"WPT"]
WPTs_PR_OCC_method_50_percent <- sort(WPTs_PR_OCC_method_50_percent)
SC_vector_o <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_50_percent[j]) {SC_vector_o <- c(SC_vector_o, Evaluation_survey_method$SC_2013[i])}
  }
}
SC_vector_o
oo <- as.data.frame(table(SC_vector_o))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2013))
SC_detected_PR_OCC_50_percent <- oo[2, 2]/ll[2, 2]
SC_detected_PR_OCC_50_percent
SC_detected_2013$PR_OCC[2] <- SC_detected_PR_OCC_50_percent

p <- read.table("CIdata_without_2013_Pr_Occ_75_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2013_Pr_Occ_75_percent_of_budget.pdf")
plot(p$X_MARK, p$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_75_percent <- p[ ,"WPT"]
WPTs_PR_OCC_method_75_percent <- sort(WPTs_PR_OCC_method_75_percent)
SC_vector_p <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_75_percent[j]) {SC_vector_p <- c(SC_vector_p, Evaluation_survey_method$SC_2013[i])}
  }
}
SC_vector_p
pp <- as.data.frame(table(SC_vector_p))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2013))
SC_detected_PR_OCC_75_percent <- pp[2, 2]/ll[2, 2]
SC_detected_PR_OCC_75_percent
SC_detected_2013$PR_OCC[3] <- SC_detected_PR_OCC_75_percent

q <- read.table("CIdata_without_2013_Pr_Occ_100_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2013_Pr_Occ_100_percent_of_budget.pdf")
plot(q$X_MARK, q$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_100_percent <- q[ ,"WPT"]
WPTs_PR_OCC_method_100_percent <- sort(WPTs_PR_OCC_method_100_percent)
SC_vector_q <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_100_percent[j]) {SC_vector_q <- c(SC_vector_q, Evaluation_survey_method$SC_2013[i])}
  }
}
SC_vector_q
qq <- as.data.frame(table(SC_vector_q))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2013))
SC_detected_PR_OCC_100_percent <- qq[2, 2]/ll[2, 2]
SC_detected_PR_OCC_100_percent
SC_detected_2013$PR_OCC[4] <- SC_detected_PR_OCC_100_percent

SC_detected_2013 # check

# Survey based on Knapsack 
r <- read.table("CIdata_without_2013_Knapsack_25_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2013_Knapsack_25_percent_of_budget.pdf")
plot(r$X_MARK, r$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_25_percent <- r[ ,"WPT"]
WPTs_KNAPSACK_method_25_percent <- sort(WPTs_KNAPSACK_method_25_percent)
SC_vector_r <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_25_percent[j]) {SC_vector_r <- c(SC_vector_r, Evaluation_survey_method$SC_2013[i])}
  }
}
SC_vector_r
rr <- as.data.frame(table(SC_vector_r))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2013))
SC_detected_KNAPSACK_25_percent <- rr[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_25_percent
SC_detected_2013$KNAPSACK[1] <- SC_detected_KNAPSACK_25_percent

s <- read.table("CIdata_without_2013_Knapsack_50_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2013_Knapsack_50_percent_of_budget.pdf")
plot(s$X_MARK, s$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_50_percent <- s[ ,"WPT"]
WPTs_KNAPSACK_method_50_percent <- sort(WPTs_KNAPSACK_method_50_percent)
SC_vector_s <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_50_percent[j]) {SC_vector_s <- c(SC_vector_s, Evaluation_survey_method$SC_2013[i])}
  }
}
SC_vector_s
ss <- as.data.frame(table(SC_vector_s))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2013))
SC_detected_KNAPSACK_50_percent <- ss[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_50_percent
SC_detected_2013$KNAPSACK[2] <- SC_detected_KNAPSACK_50_percent

t <- read.table("CIdata_without_2013_Knapsack_75_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2013_Knapsack_75_percent_of_budget.pdf")
plot(t$X_MARK, t$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_75_percent <- t[ ,"WPT"]
WPTs_KNAPSACK_method_75_percent <- sort(WPTs_KNAPSACK_method_75_percent)
SC_vector_t <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_75_percent[j]) {SC_vector_t <- c(SC_vector_t, Evaluation_survey_method$SC_2013[i])}
  }
}
SC_vector_t
tt <- as.data.frame(table(SC_vector_t))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2013))
SC_detected_KNAPSACK_75_percent <- tt[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_75_percent
SC_detected_2013$KNAPSACK[3] <- SC_detected_KNAPSACK_75_percent

u <- read.table("CIdata_without_2013_Knapsack_100_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2013_Knapsack_100_percent_of_budget.pdf")
plot(u$X_MARK, u$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_100_percent <- u[ ,"WPT"]
WPTs_KNAPSACK_method_100_percent <- sort(WPTs_KNAPSACK_method_100_percent)
SC_vector_u <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_100_percent[j]) {SC_vector_u <- c(SC_vector_u, Evaluation_survey_method$SC_2013[i])}
  }
}
SC_vector_u
uu <- as.data.frame(table(SC_vector_u))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2013))
SC_detected_KNAPSACK_100_percent <- uu[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_100_percent
SC_detected_2013$KNAPSACK[4] <- SC_detected_KNAPSACK_100_percent

SC_detected_2013 # check

# Write results
write.table(SC_detected_2013, file="SC_detected_2013.txt",sep="\t",col.names=T,row.names=F)

# Plot results
x <- c(25, 50, 75, 100)
y <- c(SC_detected_2013[ ,1])
plot(x, y, xlim=c(0,100), ylim=c(0,1), xlab="% of total survey effort (island wide survey)", ylab="Effectiveness (% SC found)", main="Performance based on 2013 supercolony distribution")
points(x, SC_detected_2013[ , 2], pch=2)
points(x, SC_detected_2013[ , 3], pch=3)
points(x, SC_detected_2013[ , 4], pch=4)
points(x, SC_detected_2013[ , 5], pch=5)



################################### Evaluate survey performance using 2015 supercolony distribution ####################################
unique(CIdata$Year)
CIdata2015 <- subset(CIdata,CIdata$Year == 2015)
# Create data.frame with WPT, X-mark, Y-mark, SC_2015 (1 or 0)
Evaluation_survey_method <- as.data.frame(matrix(NA, nrow = nrow(CIdata2015), ncol = 4)) # create data.frame 
Evaluation_survey_method[ , 1] <- CIdata2015[ , "WPT"]
Evaluation_survey_method[ , 2] <- CIdata2015[ , "X_MARK"]
Evaluation_survey_method[ , 3] <- CIdata2015[ , "Y_MARK"]
Evaluation_survey_method[ , 4] <- CIdata2015[ , "p_a_SC"]
colnames(Evaluation_survey_method) <- c("WPT", "X_MARK","Y_MARK","SC_2015")
Evaluation_survey_method
# Create dataframe to store survey performance results
SC_detected_2015 <- as.data.frame(matrix(NA, nrow=4, ncol=5))
rownames(SC_detected_2015) <- c("25% total budget", "50% total budget", "75% total budget", "100% total budget")
colnames(SC_detected_2015) <- c("RANDOM_SURVEY", "SYSTEMATIC_SURVEY", "MANAGERS_OPINION", "PR_OCC", "KNAPSACK")

# Random survey 
a <- read.table("CIdata_without_2015_random_sites_25_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2015_random_sites_25_percent.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_25_percent <- a[ ,"WPT"]
WPTs_RANDOM_survey_method_25_percent <- sort(WPTs_RANDOM_survey_method_25_percent)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_25_percent[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2015))
SC_detected_RANDOM_survey_25_percent <- aa[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_25_percent
SC_detected_2015$RANDOM_SURVEY[1] <- SC_detected_RANDOM_survey_25_percent


b <- read.table("CIdata_without_2015_random_sites_50_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2015_random_sites_50_percent.pdf")
plot(b$X_MARK, b$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_50_percent <- b[ ,"WPT"]
WPTs_RANDOM_survey_method_50_percent <- sort(WPTs_RANDOM_survey_method_50_percent)
SC_vector_b <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_50_percent[j]) {SC_vector_b <- c(SC_vector_b, Evaluation_survey_method$SC_2015[i])}
  }
}
SC_vector_b
bb <- as.data.frame(table(SC_vector_b))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2015))
SC_detected_RANDOM_survey_50_percent <- bb[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_50_percent
SC_detected_2015$RANDOM_SURVEY[2] <- SC_detected_RANDOM_survey_50_percent


c <- read.table("CIdata_without_2015_random_sites_75_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2015_random_sites_75_percent.pdf")
plot(c$X_MARK, c$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_75_percent <- c[ ,"WPT"]
WPTs_RANDOM_survey_method_75_percent <- sort(WPTs_RANDOM_survey_method_75_percent)
SC_vector_c <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_75_percent[j]) {SC_vector_c <- c(SC_vector_c, Evaluation_survey_method$SC_2015[i])}
  }
}
SC_vector_c
cc <- as.data.frame(table(SC_vector_c))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2015))
SC_detected_RANDOM_survey_75_percent <- cc[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_75_percent
SC_detected_2015$RANDOM_SURVEY[3] <- SC_detected_RANDOM_survey_75_percent

d <- read.table("CIdata_without_2015_random_sites_100_percent.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2015_random_sites_100_percent.pdf")
plot(d$X_MARK, d$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_RANDOM_survey_method_100_percent <- d[ ,"WPT"]
WPTs_RANDOM_survey_method_100_percent <- sort(WPTs_RANDOM_survey_method_100_percent)
SC_vector_d <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_RANDOM_survey_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_RANDOM_survey_method_100_percent[j]) {SC_vector_d <- c(SC_vector_d, Evaluation_survey_method$SC_2015[i])}
  }
}
SC_vector_d
dd <- as.data.frame(table(SC_vector_d))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2015))
SC_detected_RANDOM_survey_100_percent <- dd[2, 2]/ll[2, 2]
SC_detected_RANDOM_survey_100_percent
SC_detected_2015$RANDOM_SURVEY[4] <- SC_detected_RANDOM_survey_100_percent


SC_detected_2015 # check



# Systematic survey
e <- read.table("CIdata_without_2015_systematic_survey_25_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2015_systematic_survey_25_percent_budget.pdf")
plot(e$X_MARK, e$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_25_percent <- e[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_25_percent <- sort(WPTs_SYSTEMATIC_survey_method_25_percent)
SC_vector_e <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_25_percent[j]) {SC_vector_e <- c(SC_vector_e, Evaluation_survey_method$SC_2015[i])}
  }
}
SC_vector_e
ee <- as.data.frame(table(SC_vector_e))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2015))
SC_detected_SYSTEMATIC_survey_25_percent <- ee[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_25_percent
SC_detected_2015$SYSTEMATIC_SURVEY[1] <- SC_detected_SYSTEMATIC_survey_25_percent

f <- read.table("CIdata_without_2015_systematic_survey_50_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2015_systematic_survey_50_percent_budget.pdf")
plot(f$X_MARK, f$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_50_percent <- f[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_50_percent <- sort(WPTs_SYSTEMATIC_survey_method_50_percent)
SC_vector_f <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_50_percent[j]) {SC_vector_f <- c(SC_vector_f, Evaluation_survey_method$SC_2015[i])}
  }
}
SC_vector_f
ff <- as.data.frame(table(SC_vector_f))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2015))
SC_detected_SYSTEMATIC_survey_50_percent <- ff[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_50_percent
SC_detected_2015$SYSTEMATIC_SURVEY[2] <- SC_detected_SYSTEMATIC_survey_50_percent

g <- read.table("CIdata_without_2015_systematic_survey_75_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2015_systematic_survey_75_percent_budget.pdf")
plot(g$X_MARK, g$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_75_percent <- g[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_75_percent <- sort(WPTs_SYSTEMATIC_survey_method_75_percent)
SC_vector_g <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_75_percent[j]) {SC_vector_g <- c(SC_vector_g, Evaluation_survey_method$SC_2015[i])}
  }
}
SC_vector_g
gg <- as.data.frame(table(SC_vector_g))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2015))
SC_detected_SYSTEMATIC_survey_75_percent <- gg[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_75_percent
SC_detected_2015$SYSTEMATIC_SURVEY[3] <- SC_detected_SYSTEMATIC_survey_75_percent

h <- read.table("CIdata_without_2015_systematic_survey_100_percent_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2015_systematic_survey_100_percent_budget.pdf")
plot(h$X_MARK, h$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_SYSTEMATIC_survey_method_100_percent <- h[ ,"WPT"]
WPTs_SYSTEMATIC_survey_method_100_percent <- sort(WPTs_SYSTEMATIC_survey_method_100_percent)
SC_vector_h <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_SYSTEMATIC_survey_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_SYSTEMATIC_survey_method_100_percent[j]) {SC_vector_h <- c(SC_vector_h, Evaluation_survey_method$SC_2015[i])}
  }
}
SC_vector_h
hh <- as.data.frame(table(SC_vector_h))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2015))
SC_detected_SYSTEMATIC_survey_100_percent <- hh[2, 2]/ll[2, 2]
SC_detected_SYSTEMATIC_survey_100_percent
SC_detected_2015$SYSTEMATIC_SURVEY[4] <- SC_detected_SYSTEMATIC_survey_100_percent


SC_detected_2015 # check


# Survey based on manager's opinion
i <- read.table("CIdata_without_2015_managers_guess_25_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2015_managers_guess_25_percent_of_budget.pdf")
plot(i$X_MARK, i$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_25_percent <- i[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_25_percent <- sort(WPTs_MANAGERS_OPINION_method_25_percent)
SC_vector_i <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_25_percent[j]) {SC_vector_i <- c(SC_vector_i, Evaluation_survey_method$SC_2015[i])}
  }
}
SC_vector_i
ii <- as.data.frame(table(SC_vector_i))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2015))
SC_detected_MANAGERS_OPINION_25_percent <- ii[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_25_percent
SC_detected_2015$MANAGERS_OPINION[1] <- SC_detected_MANAGERS_OPINION_25_percent


j <- read.table("CIdata_without_2015_managers_guess_50_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2015_managers_guess_50_percent_of_budget.pdf")
plot(j$X_MARK, j$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_50_percent <- j[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_50_percent <- sort(WPTs_MANAGERS_OPINION_method_50_percent)
SC_vector_j <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_50_percent[j]) {SC_vector_j <- c(SC_vector_j, Evaluation_survey_method$SC_2015[i])}
  }
}
SC_vector_j
jj <- as.data.frame(table(SC_vector_j))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2015))
SC_detected_MANAGERS_OPINION_50_percent <- jj[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_50_percent
SC_detected_2015$MANAGERS_OPINION[2] <- SC_detected_MANAGERS_OPINION_50_percent

k <- read.table("CIdata_without_2015_managers_guess_75_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2015_managers_guess_75_percent_of_budget.pdf")
plot(k$X_MARK, k$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_75_percent <- k[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_75_percent <- sort(WPTs_MANAGERS_OPINION_method_75_percent)
SC_vector_k <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_75_percent[j]) {SC_vector_k <- c(SC_vector_k, Evaluation_survey_method$SC_2015[i])}
  }
}
SC_vector_k
kk <- as.data.frame(table(SC_vector_k))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2015))
SC_detected_MANAGERS_OPINION_75_percent <- kk[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_75_percent
SC_detected_2015$MANAGERS_OPINION[3] <- SC_detected_MANAGERS_OPINION_75_percent

m <- read.table("CIdata_without_2015_managers_guess_100_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2015_managers_guess_100_percent_of_budget.pdf")
plot(m$X_MARK, m$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_MANAGERS_OPINION_method_100_percent <- m[ ,"WPT"]
WPTs_MANAGERS_OPINION_method_100_percent <- sort(WPTs_MANAGERS_OPINION_method_100_percent)
SC_vector_m <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_MANAGERS_OPINION_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_MANAGERS_OPINION_method_100_percent[j]) {SC_vector_m <- c(SC_vector_m, Evaluation_survey_method$SC_2015[i])}
  }
}
SC_vector_m
mm <- as.data.frame(table(SC_vector_m))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2015))
SC_detected_MANAGERS_OPINION_100_percent <- mm[2, 2]/ll[2, 2]
SC_detected_MANAGERS_OPINION_100_percent
SC_detected_2015$MANAGERS_OPINION[4] <- SC_detected_MANAGERS_OPINION_100_percent

SC_detected_2015 # check

# Survey based on Pr_occ
n <- read.table("CIdata_without_2015_Pr_Occ_25_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2015_Pr_Occ_25_percent_of_budget.pdf")
plot(n$X_MARK, n$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_25_percent <- n[ ,"WPT"]
WPTs_PR_OCC_method_25_percent <- sort(WPTs_PR_OCC_method_25_percent)
SC_vector_n <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_25_percent[j]) {SC_vector_n <- c(SC_vector_n, Evaluation_survey_method$SC_2015[i])}
  }
}
SC_vector_n
nn <- as.data.frame(table(SC_vector_n))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2015))
SC_detected_PR_OCC_25_percent <- nn[2, 2]/ll[2, 2]
SC_detected_PR_OCC_25_percent
SC_detected_2015$PR_OCC[1] <- SC_detected_PR_OCC_25_percent

o <- read.table("CIdata_without_2015_Pr_Occ_50_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2015_Pr_Occ_50_percent_of_budget.pdf")
plot(o$X_MARK, o$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_50_percent <- o[ ,"WPT"]
WPTs_PR_OCC_method_50_percent <- sort(WPTs_PR_OCC_method_50_percent)
SC_vector_o <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_50_percent[j]) {SC_vector_o <- c(SC_vector_o, Evaluation_survey_method$SC_2015[i])}
  }
}
SC_vector_o
oo <- as.data.frame(table(SC_vector_o))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2015))
SC_detected_PR_OCC_50_percent <- oo[2, 2]/ll[2, 2]
SC_detected_PR_OCC_50_percent
SC_detected_2015$PR_OCC[2] <- SC_detected_PR_OCC_50_percent

p <- read.table("CIdata_without_2015_Pr_Occ_75_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2015_Pr_Occ_75_percent_of_budget.pdf")
plot(p$X_MARK, p$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_75_percent <- p[ ,"WPT"]
WPTs_PR_OCC_method_75_percent <- sort(WPTs_PR_OCC_method_75_percent)
SC_vector_p <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_75_percent[j]) {SC_vector_p <- c(SC_vector_p, Evaluation_survey_method$SC_2015[i])}
  }
}
SC_vector_p
pp <- as.data.frame(table(SC_vector_p))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2015))
SC_detected_PR_OCC_75_percent <- pp[2, 2]/ll[2, 2]
SC_detected_PR_OCC_75_percent
SC_detected_2015$PR_OCC[3] <- SC_detected_PR_OCC_75_percent

q <- read.table("CIdata_without_2015_Pr_Occ_100_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2015_Pr_Occ_100_percent_of_budget.pdf")
plot(q$X_MARK, q$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_PR_OCC_method_100_percent <- q[ ,"WPT"]
WPTs_PR_OCC_method_100_percent <- sort(WPTs_PR_OCC_method_100_percent)
SC_vector_q <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_PR_OCC_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_PR_OCC_method_100_percent[j]) {SC_vector_q <- c(SC_vector_q, Evaluation_survey_method$SC_2015[i])}
  }
}
SC_vector_q
qq <- as.data.frame(table(SC_vector_q))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2015))
SC_detected_PR_OCC_100_percent <- qq[2, 2]/ll[2, 2]
SC_detected_PR_OCC_100_percent
SC_detected_2015$PR_OCC[4] <- SC_detected_PR_OCC_100_percent

SC_detected_2015 # check

# Survey based on Knapsack 
r <- read.table("CIdata_without_2015_Knapsack_25_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2015_Knapsack_25_percent_of_budget.pdf")
plot(r$X_MARK, r$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_25_percent <- r[ ,"WPT"]
WPTs_KNAPSACK_method_25_percent <- sort(WPTs_KNAPSACK_method_25_percent)
SC_vector_r <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_25_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_25_percent[j]) {SC_vector_r <- c(SC_vector_r, Evaluation_survey_method$SC_2015[i])}
  }
}
SC_vector_r
rr <- as.data.frame(table(SC_vector_r))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2015))
SC_detected_KNAPSACK_25_percent <- rr[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_25_percent
SC_detected_2015$KNAPSACK[1] <- SC_detected_KNAPSACK_25_percent

s <- read.table("CIdata_without_2015_Knapsack_50_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2015_Knapsack_50_percent_of_budget.pdf")
plot(s$X_MARK, s$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_50_percent <- s[ ,"WPT"]
WPTs_KNAPSACK_method_50_percent <- sort(WPTs_KNAPSACK_method_50_percent)
SC_vector_s <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_50_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_50_percent[j]) {SC_vector_s <- c(SC_vector_s, Evaluation_survey_method$SC_2015[i])}
  }
}
SC_vector_s
ss <- as.data.frame(table(SC_vector_s))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2015))
SC_detected_KNAPSACK_50_percent <- ss[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_50_percent
SC_detected_2015$KNAPSACK[2] <- SC_detected_KNAPSACK_50_percent

t <- read.table("CIdata_without_2015_Knapsack_75_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2015_Knapsack_75_percent_of_budget.pdf")
plot(t$X_MARK, t$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_75_percent <- t[ ,"WPT"]
WPTs_KNAPSACK_method_75_percent <- sort(WPTs_KNAPSACK_method_75_percent)
SC_vector_t <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_75_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_75_percent[j]) {SC_vector_t <- c(SC_vector_t, Evaluation_survey_method$SC_2015[i])}
  }
}
SC_vector_t
tt <- as.data.frame(table(SC_vector_t))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2015))
SC_detected_KNAPSACK_75_percent <- tt[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_75_percent
SC_detected_2015$KNAPSACK[3] <- SC_detected_KNAPSACK_75_percent

u <- read.table("CIdata_without_2015_Knapsack_100_percent_of_budget.txt", header=TRUE, sep="\t")
pdf("CIdata_without_2015_Knapsack_100_percent_of_budget.pdf")
plot(u$X_MARK, u$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_KNAPSACK_method_100_percent <- u[ ,"WPT"]
WPTs_KNAPSACK_method_100_percent <- sort(WPTs_KNAPSACK_method_100_percent)
SC_vector_u <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_KNAPSACK_method_100_percent)){
    #print(paste("i = ", i, sep=""))
    #print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_KNAPSACK_method_100_percent[j]) {SC_vector_u <- c(SC_vector_u, Evaluation_survey_method$SC_2015[i])}
  }
}
SC_vector_u
uu <- as.data.frame(table(SC_vector_u))
ll <- as.data.frame(table(Evaluation_survey_method$SC_2015))
SC_detected_KNAPSACK_100_percent <- uu[2, 2]/ll[2, 2]
SC_detected_KNAPSACK_100_percent
SC_detected_2015$KNAPSACK[4] <- SC_detected_KNAPSACK_100_percent

SC_detected_2015 # check

# Write results
write.table(SC_detected_2015, file="SC_detected_2015.txt",sep="\t",col.names=T,row.names=F)

# Plot results
x <- c(25, 50, 75, 100)
y <- c(SC_detected_2015[ ,1])
plot(x, y, xlim=c(0,100), ylim=c(0,1), xlab="% of total survey effort (island wide survey)", ylab="Effectiveness (% SC found)", main="Performance based on 2015 supercolony distribution")
points(x, SC_detected_2015[ , 2], pch=2)
points(x, SC_detected_2015[ , 3], pch=3)
points(x, SC_detected_2015[ , 4], pch=4)
points(x, SC_detected_2015[ , 5], pch=5)


################################################# Calculate mean SC detected across all years with uncertainty ########################################
SC_detected_2001 <- read.table("SC_detected_2001.txt", sep="\t", header=TRUE)
SC_detected_2003 <- read.table("SC_detected_2003.txt", sep="\t", header=TRUE)
SC_detected_2005 <- read.table("SC_detected_2005.txt", sep="\t", header=TRUE)
SC_detected_2007 <- read.table("SC_detected_2007.txt", sep="\t", header=TRUE)
SC_detected_2009 <- read.table("SC_detected_2009.txt", sep="\t", header=TRUE)
SC_detected_2011 <- read.table("SC_detected_2011.txt", sep="\t", header=TRUE)
SC_detected_2013 <- read.table("SC_detected_2013.txt", sep="\t", header=TRUE)
SC_detected_2015 <- read.table("SC_detected_2015.txt", sep="\t", header=TRUE)

# Declare vectors to store % SC detected values 
random_survey_25 <- vector()
random_survey_50 <- vector()
random_survey_75 <- vector()
random_survey_100 <- vector()
syst_survey_25 <- vector()
syst_survey_50 <- vector()
syst_survey_75 <- vector()
syst_survey_100 <- vector()
man_opinion_25 <- vector()
man_opinion_50 <- vector()
man_opinion_75 <- vector()
man_opinion_100 <- vector()
pr_occ_25 <- vector()
pr_occ_50 <- vector()
pr_occ_75 <- vector()
pr_occ_100 <- vector()
knapsack_25 <- vector()
knapsack_50 <- vector()
knapsack_75 <- vector()
knapsack_100 <- vector()

# Fill vectors with % SC detected values 
random_survey_25 <- c(random_survey_25, SC_detected_2001[1,1])
random_survey_25 <- c(random_survey_25, SC_detected_2003[1,1])
random_survey_25 <- c(random_survey_25, SC_detected_2005[1,1])
random_survey_25 <- c(random_survey_25, SC_detected_2007[1,1])
random_survey_25 <- c(random_survey_25, SC_detected_2009[1,1])
random_survey_25 <- c(random_survey_25, SC_detected_2011[1,1])
random_survey_25 <- c(random_survey_25, SC_detected_2013[1,1])
random_survey_25 <- c(random_survey_25, SC_detected_2015[1,1])

syst_survey_25 <- c(syst_survey_25, SC_detected_2001[1,2])
syst_survey_25 <- c(syst_survey_25, SC_detected_2003[1,2])
syst_survey_25 <- c(syst_survey_25, SC_detected_2005[1,2])
syst_survey_25 <- c(syst_survey_25, SC_detected_2007[1,2])
syst_survey_25 <- c(syst_survey_25, SC_detected_2009[1,2])
syst_survey_25 <- c(syst_survey_25, SC_detected_2011[1,2])
syst_survey_25 <- c(syst_survey_25, SC_detected_2013[1,2])
syst_survey_25 <- c(syst_survey_25, SC_detected_2015[1,2])

man_opinion_25 <- c(man_opinion_25, SC_detected_2001[1,3])
man_opinion_25 <- c(man_opinion_25, SC_detected_2003[1,3])
man_opinion_25 <- c(man_opinion_25, SC_detected_2005[1,3])
man_opinion_25 <- c(man_opinion_25, SC_detected_2007[1,3])
man_opinion_25 <- c(man_opinion_25, SC_detected_2009[1,3])
man_opinion_25 <- c(man_opinion_25, SC_detected_2011[1,3])
man_opinion_25 <- c(man_opinion_25, SC_detected_2013[1,3])
man_opinion_25 <- c(man_opinion_25, SC_detected_2015[1,3])

pr_occ_25 <- c(pr_occ_25, SC_detected_2001[1,4])
pr_occ_25 <- c(pr_occ_25, SC_detected_2003[1,4])
pr_occ_25 <- c(pr_occ_25, SC_detected_2005[1,4])
pr_occ_25 <- c(pr_occ_25, SC_detected_2007[1,4])
pr_occ_25 <- c(pr_occ_25, SC_detected_2009[1,4])
pr_occ_25 <- c(pr_occ_25, SC_detected_2011[1,4])
pr_occ_25 <- c(pr_occ_25, SC_detected_2013[1,4])
pr_occ_25 <- c(pr_occ_25, SC_detected_2015[1,4])

knapsack_25 <- c(knapsack_25, SC_detected_2001[1,5])
knapsack_25 <- c(knapsack_25, SC_detected_2003[1,5])
knapsack_25 <- c(knapsack_25, SC_detected_2005[1,5])
knapsack_25 <- c(knapsack_25, SC_detected_2007[1,5])
knapsack_25 <- c(knapsack_25, SC_detected_2009[1,5])
knapsack_25 <- c(knapsack_25, SC_detected_2011[1,5])
knapsack_25 <- c(knapsack_25, SC_detected_2013[1,5])
knapsack_25 <- c(knapsack_25, SC_detected_2015[1,5])


random_survey_50 <- c(random_survey_50, SC_detected_2001[2,1])
random_survey_50 <- c(random_survey_50, SC_detected_2003[2,1])
random_survey_50 <- c(random_survey_50, SC_detected_2005[2,1])
random_survey_50 <- c(random_survey_50, SC_detected_2007[2,1])
random_survey_50 <- c(random_survey_50, SC_detected_2009[2,1])
random_survey_50 <- c(random_survey_50, SC_detected_2011[2,1])
random_survey_50 <- c(random_survey_50, SC_detected_2013[2,1])
random_survey_50 <- c(random_survey_50, SC_detected_2015[2,1])

syst_survey_50 <- c(syst_survey_50, SC_detected_2001[2,2])
syst_survey_50 <- c(syst_survey_50, SC_detected_2003[2,2])
syst_survey_50 <- c(syst_survey_50, SC_detected_2005[2,2])
syst_survey_50 <- c(syst_survey_50, SC_detected_2007[2,2])
syst_survey_50 <- c(syst_survey_50, SC_detected_2009[2,2])
syst_survey_50 <- c(syst_survey_50, SC_detected_2011[2,2])
syst_survey_50 <- c(syst_survey_50, SC_detected_2013[2,2])
syst_survey_50 <- c(syst_survey_50, SC_detected_2015[2,2])

man_opinion_50 <- c(man_opinion_50, SC_detected_2001[2,3])
man_opinion_50 <- c(man_opinion_50, SC_detected_2003[2,3])
man_opinion_50 <- c(man_opinion_50, SC_detected_2005[2,3])
man_opinion_50 <- c(man_opinion_50, SC_detected_2007[2,3])
man_opinion_50 <- c(man_opinion_50, SC_detected_2009[2,3])
man_opinion_50 <- c(man_opinion_50, SC_detected_2011[2,3])
man_opinion_50 <- c(man_opinion_50, SC_detected_2013[2,3])
man_opinion_50 <- c(man_opinion_50, SC_detected_2015[2,3])

pr_occ_50 <- c(pr_occ_50, SC_detected_2001[2,4])
pr_occ_50 <- c(pr_occ_50, SC_detected_2003[2,4])
pr_occ_50 <- c(pr_occ_50, SC_detected_2005[2,4])
pr_occ_50 <- c(pr_occ_50, SC_detected_2007[2,4])
pr_occ_50 <- c(pr_occ_50, SC_detected_2009[2,4])
pr_occ_50 <- c(pr_occ_50, SC_detected_2011[2,4])
pr_occ_50 <- c(pr_occ_50, SC_detected_2013[2,4])
pr_occ_50 <- c(pr_occ_50, SC_detected_2015[2,4])

knapsack_50 <- c(knapsack_50, SC_detected_2001[2,5])
knapsack_50 <- c(knapsack_50, SC_detected_2003[2,5])
knapsack_50 <- c(knapsack_50, SC_detected_2005[2,5])
knapsack_50 <- c(knapsack_50, SC_detected_2007[2,5])
knapsack_50 <- c(knapsack_50, SC_detected_2009[2,5])
knapsack_50 <- c(knapsack_50, SC_detected_2011[2,5])
knapsack_50 <- c(knapsack_50, SC_detected_2013[2,5])
knapsack_50 <- c(knapsack_50, SC_detected_2015[2,5])


random_survey_75 <- c(random_survey_75, SC_detected_2001[3,1])
random_survey_75 <- c(random_survey_75, SC_detected_2003[3,1])
random_survey_75 <- c(random_survey_75, SC_detected_2005[3,1])
random_survey_75 <- c(random_survey_75, SC_detected_2007[3,1])
random_survey_75 <- c(random_survey_75, SC_detected_2009[3,1])
random_survey_75 <- c(random_survey_75, SC_detected_2011[3,1])
random_survey_75 <- c(random_survey_75, SC_detected_2013[3,1])
random_survey_75 <- c(random_survey_75, SC_detected_2015[3,1])

syst_survey_75 <- c(syst_survey_75, SC_detected_2001[3,2])
syst_survey_75 <- c(syst_survey_75, SC_detected_2003[3,2])
syst_survey_75 <- c(syst_survey_75, SC_detected_2005[3,2])
syst_survey_75 <- c(syst_survey_75, SC_detected_2007[3,2])
syst_survey_75 <- c(syst_survey_75, SC_detected_2009[3,2])
syst_survey_75 <- c(syst_survey_75, SC_detected_2011[3,2])
syst_survey_75 <- c(syst_survey_75, SC_detected_2013[3,2])
syst_survey_75 <- c(syst_survey_75, SC_detected_2015[3,2])

man_opinion_75 <- c(man_opinion_75, SC_detected_2001[3,3])
man_opinion_75 <- c(man_opinion_75, SC_detected_2003[3,3])
man_opinion_75 <- c(man_opinion_75, SC_detected_2005[3,3])
man_opinion_75 <- c(man_opinion_75, SC_detected_2007[3,3])
man_opinion_75 <- c(man_opinion_75, SC_detected_2009[3,3])
man_opinion_75 <- c(man_opinion_75, SC_detected_2011[3,3])
man_opinion_75 <- c(man_opinion_75, SC_detected_2013[3,3])
man_opinion_75 <- c(man_opinion_75, SC_detected_2015[3,3])

pr_occ_75 <- c(pr_occ_75, SC_detected_2001[3,4])
pr_occ_75 <- c(pr_occ_75, SC_detected_2003[3,4])
pr_occ_75 <- c(pr_occ_75, SC_detected_2005[3,4])
pr_occ_75 <- c(pr_occ_75, SC_detected_2007[3,4])
pr_occ_75 <- c(pr_occ_75, SC_detected_2009[3,4])
pr_occ_75 <- c(pr_occ_75, SC_detected_2011[3,4])
pr_occ_75 <- c(pr_occ_75, SC_detected_2013[3,4])
pr_occ_75 <- c(pr_occ_75, SC_detected_2015[3,4])

knapsack_75 <- c(knapsack_75, SC_detected_2001[3,5])
knapsack_75 <- c(knapsack_75, SC_detected_2003[3,5])
knapsack_75 <- c(knapsack_75, SC_detected_2005[3,5])
knapsack_75 <- c(knapsack_75, SC_detected_2007[3,5])
knapsack_75 <- c(knapsack_75, SC_detected_2009[3,5])
knapsack_75 <- c(knapsack_75, SC_detected_2011[3,5])
knapsack_75 <- c(knapsack_75, SC_detected_2013[3,5])
knapsack_75 <- c(knapsack_75, SC_detected_2015[3,5])


random_survey_100 <- c(random_survey_100, SC_detected_2001[4,1])
random_survey_100 <- c(random_survey_100, SC_detected_2003[4,1])
random_survey_100 <- c(random_survey_100, SC_detected_2005[4,1])
random_survey_100 <- c(random_survey_100, SC_detected_2007[4,1])
random_survey_100 <- c(random_survey_100, SC_detected_2009[4,1])
random_survey_100 <- c(random_survey_100, SC_detected_2011[4,1])
random_survey_100 <- c(random_survey_100, SC_detected_2013[4,1])
random_survey_100 <- c(random_survey_100, SC_detected_2015[4,1])

syst_survey_100 <- c(syst_survey_100, SC_detected_2001[4,2])
syst_survey_100 <- c(syst_survey_100, SC_detected_2003[4,2])
syst_survey_100 <- c(syst_survey_100, SC_detected_2005[4,2])
syst_survey_100 <- c(syst_survey_100, SC_detected_2007[4,2])
syst_survey_100 <- c(syst_survey_100, SC_detected_2009[4,2])
syst_survey_100 <- c(syst_survey_100, SC_detected_2011[4,2])
syst_survey_100 <- c(syst_survey_100, SC_detected_2013[4,2])
syst_survey_100 <- c(syst_survey_100, SC_detected_2015[4,2])

man_opinion_100 <- c(man_opinion_100, SC_detected_2001[4,3])
man_opinion_100 <- c(man_opinion_100, SC_detected_2003[4,3])
man_opinion_100 <- c(man_opinion_100, SC_detected_2005[4,3])
man_opinion_100 <- c(man_opinion_100, SC_detected_2007[4,3])
man_opinion_100 <- c(man_opinion_100, SC_detected_2009[4,3])
man_opinion_100 <- c(man_opinion_100, SC_detected_2011[4,3])
man_opinion_100 <- c(man_opinion_100, SC_detected_2013[4,3])
man_opinion_100 <- c(man_opinion_100, SC_detected_2015[4,3])

pr_occ_100 <- c(pr_occ_100, SC_detected_2001[4,4])
pr_occ_100 <- c(pr_occ_100, SC_detected_2003[4,4])
pr_occ_100 <- c(pr_occ_100, SC_detected_2005[4,4])
pr_occ_100 <- c(pr_occ_100, SC_detected_2007[4,4])
pr_occ_100 <- c(pr_occ_100, SC_detected_2009[4,4])
pr_occ_100 <- c(pr_occ_100, SC_detected_2011[4,4])
pr_occ_100 <- c(pr_occ_100, SC_detected_2013[4,4])
pr_occ_100 <- c(pr_occ_100, SC_detected_2015[4,4])

knapsack_100 <- c(knapsack_100, SC_detected_2001[4,5])
knapsack_100 <- c(knapsack_100, SC_detected_2003[4,5])
knapsack_100 <- c(knapsack_100, SC_detected_2005[4,5])
knapsack_100 <- c(knapsack_100, SC_detected_2007[4,5])
knapsack_100 <- c(knapsack_100, SC_detected_2009[4,5])
knapsack_100 <- c(knapsack_100, SC_detected_2011[4,5])
knapsack_100 <- c(knapsack_100, SC_detected_2013[4,5])
knapsack_100 <- c(knapsack_100, SC_detected_2015[4,5])

# Calculate average % SC detected with upper and lower quartile per survey method 
average_random_survey_25 <- mean(random_survey_25)
average_random_survey_50 <- mean(random_survey_50)
average_random_survey_75 <- mean(random_survey_75)
average_random_survey_100 <- mean(random_survey_100)
UQ_random_survey_25 <- quantile(random_survey_25, 0.975)
UQ_random_survey_50 <- quantile(random_survey_50, 0.975)
UQ_random_survey_75 <- quantile(random_survey_75, 0.975)
UQ_random_survey_100 <- quantile(random_survey_100, 0.975)
LQ_random_survey_25 <- quantile(random_survey_25, 0.025)
LQ_random_survey_50 <- quantile(random_survey_50, 0.025)
LQ_random_survey_75 <- quantile(random_survey_75, 0.025)
LQ_random_survey_100 <- quantile(random_survey_100, 0.025)

average_syst_survey_25 <- mean(syst_survey_25)
average_syst_survey_50 <- mean(syst_survey_50)
average_syst_survey_75 <- mean(syst_survey_75)
average_syst_survey_100 <- mean(syst_survey_100)
UQ_syst_survey_25 <- quantile(syst_survey_25, 0.975)
UQ_syst_survey_50 <- quantile(syst_survey_50, 0.975)
UQ_syst_survey_75 <- quantile(syst_survey_75, 0.975)
UQ_syst_survey_100 <- quantile(syst_survey_100, 0.975)
LQ_syst_survey_25 <- quantile(syst_survey_25, 0.025)
LQ_syst_survey_50 <- quantile(syst_survey_50, 0.025)
LQ_syst_survey_75 <- quantile(syst_survey_75, 0.025)
LQ_syst_survey_100 <- quantile(syst_survey_100, 0.025)

average_man_opinion_25 <- mean(man_opinion_25)
average_man_opinion_50 <- mean(man_opinion_50)
average_man_opinion_75 <- mean(man_opinion_75)
average_man_opinion_100 <- mean(man_opinion_100)
UQ_man_opinion_25 <- quantile(man_opinion_25, 0.975)
UQ_man_opinion_50 <- quantile(man_opinion_50, 0.975)
UQ_man_opinion_75 <- quantile(man_opinion_75, 0.975)
UQ_man_opinion_100 <- quantile(man_opinion_100, 0.975)
LQ_man_opinion_25 <- quantile(man_opinion_25, 0.025)
LQ_man_opinion_50 <- quantile(man_opinion_50, 0.025)
LQ_man_opinion_75 <- quantile(man_opinion_75, 0.025)
LQ_man_opinion_100 <- quantile(man_opinion_100, 0.025)

average_pr_occ_25 <- mean(pr_occ_25)
average_pr_occ_50 <- mean(pr_occ_50)
average_pr_occ_75 <- mean(pr_occ_75)
average_pr_occ_100 <- mean(pr_occ_100)
UQ_pr_occ_25 <- quantile(pr_occ_25, 0.975)
UQ_pr_occ_50 <- quantile(pr_occ_50, 0.975)
UQ_pr_occ_75 <- quantile(pr_occ_75, 0.975)
UQ_pr_occ_100 <- quantile(pr_occ_100, 0.975)
LQ_pr_occ_25 <- quantile(pr_occ_25, 0.025)
LQ_pr_occ_50 <- quantile(pr_occ_50, 0.025)
LQ_pr_occ_75 <- quantile(pr_occ_75, 0.025)
LQ_pr_occ_100 <- quantile(pr_occ_100, 0.025)

average_knapsack_25 <- mean(knapsack_25)
average_knapsack_50 <- mean(knapsack_50)
average_knapsack_75 <- mean(knapsack_75)
average_knapsack_100 <- mean(knapsack_100)
UQ_knapsack_25 <- quantile(knapsack_25, 0.975)
UQ_knapsack_50 <- quantile(knapsack_50, 0.975)
UQ_knapsack_75 <- quantile(knapsack_75, 0.975)
UQ_knapsack_100 <- quantile(knapsack_100, 0.975)
LQ_knapsack_25 <- quantile(knapsack_25, 0.025)
LQ_knapsack_50 <- quantile(knapsack_50, 0.025)
LQ_knapsack_75 <- quantile(knapsack_75, 0.025)
LQ_knapsack_100 <- quantile(knapsack_100, 0.025)

average_random_survey <- c(average_random_survey_25, average_random_survey_50, average_random_survey_75, average_random_survey_100)
UQ_random_survey <- c(UQ_random_survey_25, UQ_random_survey_50, UQ_random_survey_75, UQ_random_survey_100)
LQ_random_survey <- c(LQ_random_survey_25, LQ_random_survey_50, LQ_random_survey_75, LQ_random_survey_100)
average_random_survey_with_uncertainty <- matrix(NA, nrow=length(average_random_survey), ncol=3) # declare matrix to store average SC detected results with uncertainty
average_random_survey_with_uncertainty[ , 1] <- average_random_survey # first column are average values of SC detected
average_random_survey_with_uncertainty[ , 2] <- UQ_random_survey # second column are upper quantile values
average_random_survey_with_uncertainty[ , 3] <- LQ_random_survey # third column are lower quantile values 
average_random_survey_with_uncertainty # have a look


average_systematic_survey <- c(average_syst_survey_25, average_syst_survey_50, average_syst_survey_75, average_syst_survey_100)
UQ_systematic_survey <- c(UQ_syst_survey_25, UQ_syst_survey_50, UQ_syst_survey_75, UQ_syst_survey_100)
LQ_systematic_survey <- c(LQ_syst_survey_25, LQ_syst_survey_50, LQ_syst_survey_75, LQ_syst_survey_100)
average_systematic_survey_with_uncertainty <- matrix(NA, nrow=length(average_systematic_survey), ncol=3) # declare matrix to store average SC detected results with uncertainty
average_systematic_survey_with_uncertainty[ , 1] <- average_systematic_survey # first column are average values of SC detected
average_systematic_survey_with_uncertainty[ , 2] <- UQ_systematic_survey # second column are upper quantile values
average_systematic_survey_with_uncertainty[ , 3] <- LQ_systematic_survey # third column are lower quantile values 
average_systematic_survey_with_uncertainty # have a look

average_man_opinion <- c(average_man_opinion_25, average_man_opinion_50, average_man_opinion_75, average_knapsack_100)
UQ_man_opinion <- c(UQ_man_opinion_25, UQ_man_opinion_50, UQ_man_opinion_75, UQ_man_opinion_100)
LQ_man_opinion <- c(LQ_man_opinion_25, LQ_man_opinion_50, LQ_man_opinion_75, LQ_man_opinion_100)
average_man_opinion_with_uncertainty <- matrix(NA, nrow=length(average_man_opinion), ncol=3) # declare matrix to store average SC detected results with uncertainty
average_man_opinion_with_uncertainty[ , 1] <- average_man_opinion # first column are average values of SC detected
average_man_opinion_with_uncertainty[ , 2] <- UQ_man_opinion # second column are upper quantile values
average_man_opinion_with_uncertainty[ , 3] <- LQ_man_opinion # third column are lower quantile values 
average_man_opinion_with_uncertainty # have a look

average_pr_occ <- c(average_pr_occ_25, average_pr_occ_50, average_pr_occ_75, average_pr_occ_100)
UQ_pr_occ <- c(UQ_pr_occ_25, UQ_pr_occ_50, UQ_pr_occ_75, UQ_pr_occ_100)
LQ_pr_occ <- c(LQ_pr_occ_25, LQ_pr_occ_50, LQ_pr_occ_75, LQ_pr_occ_100)
average_pr_occ_with_uncertainty <- matrix(NA, nrow=length(average_pr_occ), ncol=3) # declare matrix to store average SC detected results with uncertainty
average_pr_occ_with_uncertainty[ , 1] <- average_pr_occ # first column are average values of SC detected
average_pr_occ_with_uncertainty[ , 2] <- UQ_pr_occ # second column are upper quantile values
average_pr_occ_with_uncertainty[ , 3] <- LQ_pr_occ # third column are lower quantile values 
average_pr_occ_with_uncertainty # have a look

average_knapsack <- c(average_knapsack_25, average_knapsack_50, average_knapsack_75, average_knapsack_100)
UQ_knapsack <- c(UQ_knapsack_25, UQ_knapsack_50, UQ_knapsack_75, UQ_knapsack_100)
LQ_knapsack <- c(LQ_knapsack_25, LQ_knapsack_50, LQ_knapsack_75, LQ_knapsack_100)
average_knapsack_with_uncertainty <- matrix(NA, nrow=length(average_knapsack), ncol=3) # declare matrix to store average SC detected results with uncertainty
average_knapsack_with_uncertainty[ , 1] <- average_knapsack # first column are average values of SC detected
average_knapsack_with_uncertainty[ , 2] <- UQ_knapsack # second column are upper quantile values
average_knapsack_with_uncertainty[ , 3] <- LQ_knapsack # third column are lower quantile values 
average_knapsack_with_uncertainty # have a look

# Save values
write.table(average_random_survey_with_uncertainty, file="SC_detected_average_random_survey.txt",sep="\t",col.names=c("average % SC detected", "UQ", "LQ"),row.names=c("25%", "50%", "75%", "100%"))
write.table(average_systematic_survey_with_uncertainty, file="SC_detected_average_systematic_survey.txt",sep="\t",col.names=c("average % SC detected", "UQ", "LQ"),row.names=c("25%", "50%", "75%", "100%"))
write.table(average_man_opinion_with_uncertainty, file="SC_detected_average_man_opinion.txt",sep="\t",col.names=c("average % SC detected", "UQ", "LQ"),row.names=c("25%", "50%", "75%", "100%"))
write.table(average_pr_occ_with_uncertainty, file="SC_detected_average_pr_occ.txt",sep="\t",col.names=c("average % SC detected", "UQ", "LQ"),row.names=c("25%", "50%", "75%", "100%"))
write.table(average_knapsack_with_uncertainty, file="SC_detected_average_knapsack.txt",sep="\t",col.names=c("average % SC detected", "UQ", "LQ"),row.names=c("25%", "50%", "75%", "100%"))

# Plot
x <- c(25,50,75,100)
plot(x, average_random_survey, ylim=c(0,1), pch=1, xlab="% of total budget")
segments(x, UQ_random_survey, x, LQ_random_survey)
plot(x, average_systematic_survey, ylim=c(0,1), pch=2, xlab="% of total budget")
segments(x, UQ_systematic_survey, x, LQ_systematic_survey)
plot(x, average_man_opinion, ylim=c(0,1), pch=3, xlab="% of total budget")
segments(x, UQ_man_opinion, x, LQ_man_opinion)
plot(x, average_pr_occ, ylim=c(0,1), pch=4, xlab="% of total budget")
segments(x, UQ_pr_occ, x, LQ_pr_occ)
plot(x, average_knapsack, ylim=c(0,1), pch=5, xlab="% of total budget")
segments(x, UQ_knapsack, x, LQ_knapsack)



### Plot mean and 95% CI for final results thesis 
par(mfrow=c(2,3)) 
# WPTs REAL COST
setwd("D:\\Users\\evanburm\\Dropbox\\Christmas Island\\Ch03_Cost_effective_survey_methods_for_invasive_species\\WPTs_REAL_COST")
load("SC_detected_25_percent_REAL_COST.Rda")
load("SC_detected_50_percent_REAL_COST.Rda")
load("SC_detected_75_percent_REAL_COST.Rda")
# PLOT 25 %
mean_knapsack_25_percent <- mean(SC_detected_25_percent[ , 1])
UQ_KP_25 <- quantile(SC_detected_25_percent[ , 1], 0.975)
LQ_KP_25 <- quantile(SC_detected_25_percent[ , 1], 0.025)
mean_greedy_25_percent <- mean(SC_detected_25_percent[ , 2])
UQ_greedy_25 <- quantile(SC_detected_25_percent[ , 2], 0.975)
LQ_greedy_25 <- quantile(SC_detected_25_percent[ , 2], 0.025)
mean_reward_25_percent <- mean(SC_detected_25_percent[ , 3])
UQ_reward_25 <- quantile(SC_detected_25_percent[ , 3], 0.975)
LQ_reward_25 <- quantile(SC_detected_25_percent[ , 3], 0.025)
mean_expop_25_percent <- mean(SC_detected_25_percent[ , 4])
UQ_expop_25 <- quantile(SC_detected_25_percent[ , 4], 0.975)
LQ_expop_25 <- quantile(SC_detected_25_percent[ , 4], 0.025)
mean_syst_25_percent <- mean(SC_detected_25_percent[ , 5])
UQ_syst_25 <- quantile(SC_detected_25_percent[ , 5], 0.975)
LQ_syst_25 <- quantile(SC_detected_25_percent[ , 5], 0.025)
mean_random_25_percent <- mean(SC_detected_25_percent[ , 6])
UQ_random_25 <- quantile(SC_detected_25_percent[ , 6], 0.975)
LQ_random_25 <- quantile(SC_detected_25_percent[ , 6], 0.025)

x <- 1:6
plot(x, c(mean_knapsack_25_percent, mean_greedy_25_percent, mean_reward_25_percent, mean_expop_25_percent,
          mean_syst_25_percent, mean_random_25_percent), ylim=c(0,1), pch=19, ylab="Percentage of SC found")
segments(x, c(UQ_KP_25, UQ_greedy_25, UQ_reward_25, UQ_expop_25, UQ_syst_25, UQ_random_25), 
         x, c(LQ_KP_25, LQ_greedy_25, LQ_reward_25, LQ_expop_25, LQ_syst_25, LQ_random_25))

# PLOT 50 %
mean_knapsack_50_percent <- mean(SC_detected_50_percent[ , 1])
UQ_KP_50 <- quantile(SC_detected_50_percent[ , 1], 0.975)
LQ_KP_50 <- quantile(SC_detected_50_percent[ , 1], 0.025)
mean_greedy_50_percent <- mean(SC_detected_50_percent[ , 2])
UQ_greedy_50 <- quantile(SC_detected_50_percent[ , 2], 0.975)
LQ_greedy_50 <- quantile(SC_detected_50_percent[ , 2], 0.025)
mean_reward_50_percent <- mean(SC_detected_50_percent[ , 3])
UQ_reward_50 <- quantile(SC_detected_50_percent[ , 3], 0.975)
LQ_reward_50 <- quantile(SC_detected_50_percent[ , 3], 0.025)
mean_expop_50_percent <- mean(SC_detected_50_percent[ , 4])
UQ_expop_50 <- quantile(SC_detected_50_percent[ , 4], 0.975)
LQ_expop_50 <- quantile(SC_detected_50_percent[ , 4], 0.025)
mean_syst_50_percent <- mean(SC_detected_50_percent[ , 5])
UQ_syst_50 <- quantile(SC_detected_50_percent[ , 5], 0.975)
LQ_syst_50 <- quantile(SC_detected_50_percent[ , 5], 0.025)
mean_random_50_percent <- mean(SC_detected_50_percent[ , 6])
UQ_random_50 <- quantile(SC_detected_50_percent[ , 6], 0.975)
LQ_random_50 <- quantile(SC_detected_50_percent[ , 6], 0.025)

x <- 1:6
plot(x, c(mean_knapsack_50_percent, mean_greedy_50_percent, mean_reward_50_percent, mean_expop_50_percent,
          mean_syst_50_percent, mean_random_50_percent), ylim=c(0,1), pch=19, ylab="Percentage of SC found")
segments(x, c(UQ_KP_50, UQ_greedy_50, UQ_reward_50, UQ_expop_50, UQ_syst_50, UQ_random_50), 
         x, c(LQ_KP_50, LQ_greedy_50, LQ_reward_50, LQ_expop_50, LQ_syst_50, LQ_random_50))

# PLOT 50 %
mean_knapsack_75_percent <- mean(SC_detected_75_percent[ , 1])
UQ_KP_75 <- quantile(SC_detected_75_percent[ , 1], 0.975)
LQ_KP_75 <- quantile(SC_detected_75_percent[ , 1], 0.025)
mean_greedy_75_percent <- mean(SC_detected_75_percent[ , 2])
UQ_greedy_75 <- quantile(SC_detected_75_percent[ , 2], 0.975)
LQ_greedy_75 <- quantile(SC_detected_75_percent[ , 2], 0.025)
mean_reward_75_percent <- mean(SC_detected_75_percent[ , 3])
UQ_reward_75 <- quantile(SC_detected_75_percent[ , 3], 0.975)
LQ_reward_75 <- quantile(SC_detected_75_percent[ , 3], 0.025)
mean_expop_75_percent <- mean(SC_detected_75_percent[ , 4])
UQ_expop_75 <- quantile(SC_detected_75_percent[ , 4], 0.975)
LQ_expop_75 <- quantile(SC_detected_75_percent[ , 4], 0.025)
mean_syst_75_percent <- mean(SC_detected_75_percent[ , 5])
UQ_syst_75 <- quantile(SC_detected_75_percent[ , 5], 0.975)
LQ_syst_75 <- quantile(SC_detected_75_percent[ , 5], 0.025)
mean_random_75_percent <- mean(SC_detected_75_percent[ , 6])
UQ_random_75 <- quantile(SC_detected_75_percent[ , 6], 0.975)
LQ_random_75 <- quantile(SC_detected_75_percent[ , 6], 0.025)

x <- 1:6
plot(x, c(mean_knapsack_75_percent, mean_greedy_75_percent, mean_reward_75_percent, mean_expop_75_percent,
          mean_syst_75_percent, mean_random_75_percent), ylim=c(0,1), pch=19, ylab="Percentage of SC found")
segments(x, c(UQ_KP_75, UQ_greedy_75, UQ_reward_75, UQ_expop_75, UQ_syst_75, UQ_random_75), 
         x, c(LQ_KP_75, LQ_greedy_75, LQ_reward_75, LQ_expop_75, LQ_syst_75, LQ_random_75))

# WPTs RESCALED COST
setwd("D:\\Users\\evanburm\\Dropbox\\Christmas Island\\Ch03_Cost_effective_survey_methods_for_invasive_species\\WPTs_RESCALED_COST")
load("SC_detected_25_percent_RESCALED_COST.Rda")
load("SC_detected_50_percent_RESCALED_COST.Rda")
load("SC_detected_75_percent_RESCALED_COST.Rda")
# PLOT 25 %
mean_knapsack_25_percent <- mean(SC_detected_25_percent[ , 1])
UQ_KP_25 <- quantile(SC_detected_25_percent[ , 1], 0.975)
LQ_KP_25 <- quantile(SC_detected_25_percent[ , 1], 0.025)
mean_greedy_25_percent <- mean(SC_detected_25_percent[ , 2])
UQ_greedy_25 <- quantile(SC_detected_25_percent[ , 2], 0.975)
LQ_greedy_25 <- quantile(SC_detected_25_percent[ , 2], 0.025)
mean_reward_25_percent <- mean(SC_detected_25_percent[ , 3])
UQ_reward_25 <- quantile(SC_detected_25_percent[ , 3], 0.975)
LQ_reward_25 <- quantile(SC_detected_25_percent[ , 3], 0.025)
mean_expop_25_percent <- mean(SC_detected_25_percent[ , 4])
UQ_expop_25 <- quantile(SC_detected_25_percent[ , 4], 0.975)
LQ_expop_25 <- quantile(SC_detected_25_percent[ , 4], 0.025)
mean_syst_25_percent <- mean(SC_detected_25_percent[ , 5])
UQ_syst_25 <- quantile(SC_detected_25_percent[ , 5], 0.975)
LQ_syst_25 <- quantile(SC_detected_25_percent[ , 5], 0.025)
mean_random_25_percent <- mean(SC_detected_25_percent[ , 6])
UQ_random_25 <- quantile(SC_detected_25_percent[ , 6], 0.975)
LQ_random_25 <- quantile(SC_detected_25_percent[ , 6], 0.025)

x <- 1:6
plot(x, c(mean_knapsack_25_percent, mean_greedy_25_percent, mean_reward_25_percent, mean_expop_25_percent,
          mean_syst_25_percent, mean_random_25_percent), ylim=c(0,1), pch=19, ylab="Percentage of SC found")
segments(x, c(UQ_KP_25, UQ_greedy_25, UQ_reward_25, UQ_expop_25, UQ_syst_25, UQ_random_25), 
         x, c(LQ_KP_25, LQ_greedy_25, LQ_reward_25, LQ_expop_25, LQ_syst_25, LQ_random_25))

# PLOT 50 %
mean_knapsack_50_percent <- mean(SC_detected_50_percent[ , 1])
UQ_KP_50 <- quantile(SC_detected_50_percent[ , 1], 0.975)
LQ_KP_50 <- quantile(SC_detected_50_percent[ , 1], 0.025)
mean_greedy_50_percent <- mean(SC_detected_50_percent[ , 2])
UQ_greedy_50 <- quantile(SC_detected_50_percent[ , 2], 0.975)
LQ_greedy_50 <- quantile(SC_detected_50_percent[ , 2], 0.025)
mean_reward_50_percent <- mean(SC_detected_50_percent[ , 3])
UQ_reward_50 <- quantile(SC_detected_50_percent[ , 3], 0.975)
LQ_reward_50 <- quantile(SC_detected_50_percent[ , 3], 0.025)
mean_expop_50_percent <- mean(SC_detected_50_percent[ , 4])
UQ_expop_50 <- quantile(SC_detected_50_percent[ , 4], 0.975)
LQ_expop_50 <- quantile(SC_detected_50_percent[ , 4], 0.025)
mean_syst_50_percent <- mean(SC_detected_50_percent[ , 5])
UQ_syst_50 <- quantile(SC_detected_50_percent[ , 5], 0.975)
LQ_syst_50 <- quantile(SC_detected_50_percent[ , 5], 0.025)
mean_random_50_percent <- mean(SC_detected_50_percent[ , 6])
UQ_random_50 <- quantile(SC_detected_50_percent[ , 6], 0.975)
LQ_random_50 <- quantile(SC_detected_50_percent[ , 6], 0.025)

x <- 1:6
plot(x, c(mean_knapsack_50_percent, mean_greedy_50_percent, mean_reward_50_percent, mean_expop_50_percent,
          mean_syst_50_percent, mean_random_50_percent), ylim=c(0,1), pch=19, ylab="Percentage of SC found")
segments(x, c(UQ_KP_50, UQ_greedy_50, UQ_reward_50, UQ_expop_50, UQ_syst_50, UQ_random_50), 
         x, c(LQ_KP_50, LQ_greedy_50, LQ_reward_50, LQ_expop_50, LQ_syst_50, LQ_random_50))

# PLOT 50 %
mean_knapsack_75_percent <- mean(SC_detected_75_percent[ , 1])
UQ_KP_75 <- quantile(SC_detected_75_percent[ , 1], 0.975)
LQ_KP_75 <- quantile(SC_detected_75_percent[ , 1], 0.025)
mean_greedy_75_percent <- mean(SC_detected_75_percent[ , 2])
UQ_greedy_75 <- quantile(SC_detected_75_percent[ , 2], 0.975)
LQ_greedy_75 <- quantile(SC_detected_75_percent[ , 2], 0.025)
mean_reward_75_percent <- mean(SC_detected_75_percent[ , 3])
UQ_reward_75 <- quantile(SC_detected_75_percent[ , 3], 0.975)
LQ_reward_75 <- quantile(SC_detected_75_percent[ , 3], 0.025)
mean_expop_75_percent <- mean(SC_detected_75_percent[ , 4])
UQ_expop_75 <- quantile(SC_detected_75_percent[ , 4], 0.975)
LQ_expop_75 <- quantile(SC_detected_75_percent[ , 4], 0.025)
mean_syst_75_percent <- mean(SC_detected_75_percent[ , 5])
UQ_syst_75 <- quantile(SC_detected_75_percent[ , 5], 0.975)
LQ_syst_75 <- quantile(SC_detected_75_percent[ , 5], 0.025)
mean_random_75_percent <- mean(SC_detected_75_percent[ , 6])
UQ_random_75 <- quantile(SC_detected_75_percent[ , 6], 0.975)
LQ_random_75 <- quantile(SC_detected_75_percent[ , 6], 0.025)

x <- 1:6
plot(x, c(mean_knapsack_75_percent, mean_greedy_75_percent, mean_reward_75_percent, mean_expop_75_percent,
          mean_syst_75_percent, mean_random_75_percent), ylim=c(0,1), pch=19, ylab="Percentage of SC found")
segments(x, c(UQ_KP_75, UQ_greedy_75, UQ_reward_75, UQ_expop_75, UQ_syst_75, UQ_random_75), 
         x, c(LQ_KP_75, LQ_greedy_75, LQ_reward_75, LQ_expop_75, LQ_syst_75, LQ_random_75))

