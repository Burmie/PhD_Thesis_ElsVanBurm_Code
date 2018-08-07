### Here we evaluate the proportion of supercolonies found using each model (to calculate Knapsack value)
### The performance of each model is evaluated using the supercolony distribution map of the next year (e.g. SC map of 2005 was used
### to evaluate when the model was built using data from 2001 and 2003). 
### Model validation is done here with a management perspective in mind - management focus 
### The entire analyses should be done for different total budgets 

## Create data frame to store end results (proportion of supercolonies found) 
percentage_of_total_survey_budget <- 1:100 # fractions of survey budget
evaluation_years <- c("2005", "2007", "2009", "2011", "2013", "2015")
Performance_evaluation <- as.data.frame(matrix(NA, nrow=length(evaluation_years), ncol=length(percentage_of_total_survey_budget)))
colnames(Performance_evaluation) <- percentage_of_total_survey_budget
rownames(Performance_evaluation) <- evaluation_years
Performance_evaluation

# Create list to store all evaluation matrices
evaluation_list <- list()
# Fill evaluation list 
for (i in evaluation_years){ 
  for (j in percentage_of_total_survey_budget) {
  CIdata_year <- get(paste("CIdata",i, sep=""))
  # Create evaluation matrices: data.frames with WPT, X-mark, Y-mark, p_a_SC_evaluation_year (1 or 0) 
  Evaluation_survey_method <- as.data.frame(matrix(NA, nrow = nrow(CIdata_year), ncol = 4)) # create data.frame 
  Evaluation_survey_method[ , 1] <- CIdata_year[ , "WPT"]
  Evaluation_survey_method[ , 2] <- CIdata_year[ , "X_MARK"]
  Evaluation_survey_method[ , 3] <- CIdata_year[ , "Y_MARK"]
  Evaluation_survey_method[ , 4] <- CIdata_year[ , "p_a_SC"]
  colnames(Evaluation_survey_method) <- c("WPT", "X_MARK","Y_MARK",(paste("SC_",i,sep="")))
  #print(Evaluation_survey_method)
    
  if (i == "2005") 
    {kk <- paste("CIdata_2001_2003_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")}
      if (i == "2007") 
        {kk <- paste("CIdata_2001_2003_2005_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")}
          if (i == "2009") 
            {kk <- paste("CIdata_2001_2003_2005_2007_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")}
              if (i == "2011") 
                {kk <- paste("CIdata_2001_2003_2005_2007_2009_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")}
                  if (i == "2013") 
                    {kk <- paste("CIdata_2001_2003_2005_2007_2009_2011_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")}
                      if (i == "2015") 
                        {kk <- paste("CIdata_2001_2003_2005_2007_2009_2011_2013_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")}
  
    #print(kk)
    a <- read.table(kk, header=TRUE, sep="\t")
    #plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords", main=kk)
    WPTs_SURVEYED <- a[ ,"WPT"]
    WPTs_SURVEYED <- sort(WPTs_SURVEYED)
    SC_vector_a <- vector()
    for (k in 1:nrow(Evaluation_survey_method)) {
      if (length(WPTs_SURVEYED) == 0) {SC_vector_a <- 0}
        else {
          for (l in 1:length(WPTs_SURVEYED)){
            if (WPTs_SURVEYED[l])
              if (Evaluation_survey_method$WPT[k] == WPTs_SURVEYED[l]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method[k, 4])}
          }
        }
    }
    #SC_vector_a
    aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a 
    ll <- as.data.frame(table(Evaluation_survey_method[ , 4])) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 
    SC_detected <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in SC_vector_a vs SC in evaluation dataset (we are assuming perfect detection)
    #SC_detected # check ratio
    Performance_evaluation[(which(evaluation_years==i)), (which(percentage_of_total_survey_budget==j))] <- SC_detected # add to the final table the percentage of SC found 
    # when a certain % sites is surveyed and these data is used to fit a logistic regression model and Knapsack formulation 
    # is used to determine which sites to visit for management 
    # based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
    # previous year are added to the model too 
  }
}

Performance_evaluation
save(Performance_evaluation, file="Performance_evaluation_Total_BUDGET_1000_MonCost_actual.Rda")
write.table(Performance_evaluation, file="Performance_evaluation_Total_BUDGET_1000_MonCost_actual.txt", sep="\t", col.names = T, row.names = F)










#########################################################################################################################################
###################################################### Archive: 25% of data #############################################################
#########################################################################################################################################

############################# Evaluate performance of model built with 2001 data and using 2003 SC distribution to evaluate #############
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

a <- read.table("CIdata2001_25_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2001_25_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2001_25_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2001_25_percent_SURVEYED <- sort(WPTs_CIdata2001_25_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2001_25_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2001_25_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_25_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2003)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2001_25_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_25_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2001_25_percent_SURVEYED # check ratio
Performance_evaluation[1, 1] <- SC_detected_CIdata2001_25_percent_SURVEYED # add to the final table the percentage of SC found 
# when 25% of 2001 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 

############################# Evaluate performance of model built with 2001 and 2003 data and using 2005 SC distribution to evaluate #############
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

a <- read.table("CIdata2003_25_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2003_25_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2003_25_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2003_25_percent_SURVEYED <- sort(WPTs_CIdata2003_25_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2003_25_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2003_25_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_25_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2005)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2003_25_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_25_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2003_25_percent_SURVEYED # check ratio
Performance_evaluation[2, 1] <- SC_detected_CIdata2003_25_percent_SURVEYED # add to the final table the percentage of SC found 
# when 25% of 2001 and 2003 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 

############################# Evaluate performance of model built with 2001 - 2003 - 2005 data and using 2007 SC distribution to evaluate #############
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

a <- read.table("CIdata2005_25_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2005_25_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2005_25_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2005_25_percent_SURVEYED <- sort(WPTs_CIdata2005_25_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2005_25_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2005_25_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_25_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2007)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2005_25_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_25_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2005_25_percent_SURVEYED # check ratio
Performance_evaluation[3, 1] <- SC_detected_CIdata2005_25_percent_SURVEYED # add to the final table the percentage of SC found 
# when 25% of 2001 - 2005 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 


############################# Evaluate performance of model built with 2001 - 2003 - 2005 - 2007 data and using 2009 SC distribution to evaluate #############
unique(CIdata$Year)
CIdata2009 <- subset(CIdata,CIdata$Year == 2009)
CIdata2009 <- subset(CIdata2009, !duplicated(WPT))
# Create data.frame with WPT, X-mark, Y-mark, SC_2009 (1 or 0)
Evaluation_survey_method <- as.data.frame(matrix(NA, nrow = nrow(CIdata2009), ncol = 4)) # create data.frame 
Evaluation_survey_method[ , 1] <- CIdata2009[ , "WPT"]
Evaluation_survey_method[ , 2] <- CIdata2009[ , "X_MARK"]
Evaluation_survey_method[ , 3] <- CIdata2009[ , "Y_MARK"]
Evaluation_survey_method[ , 4] <- CIdata2009[ , "p_a_SC"]
colnames(Evaluation_survey_method) <- c("WPT", "X_MARK","Y_MARK","SC_2009")
Evaluation_survey_method

a <- read.table("CIdata2007_25_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2007_25_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2007_25_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2007_25_percent_SURVEYED <- sort(WPTs_CIdata2007_25_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2007_25_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2007_25_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_25_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2009)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2007_25_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_25_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2007_25_percent_SURVEYED # check ratio
Performance_evaluation[4, 1] <- SC_detected_CIdata2007_25_percent_SURVEYED # add to the final table the percentage of SC found 
# when 25% of 2001 - 2007 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 


############################# Evaluate performance of model built with 2001 - 2003 - 2005 - 2007 - 2009 data and using 2011 SC distribution to evaluate #############
unique(CIdata$Year)
CIdata2011 <- subset(CIdata,CIdata$Year == 2011)
CIdata2011 <- subset(CIdata2011, !duplicated(WPT))
# Create data.frame with WPT, X-mark, Y-mark, SC_2011 (1 or 0)
Evaluation_survey_method <- as.data.frame(matrix(NA, nrow = nrow(CIdata2011), ncol = 4)) # create data.frame 
Evaluation_survey_method[ , 1] <- CIdata2011[ , "WPT"]
Evaluation_survey_method[ , 2] <- CIdata2011[ , "X_MARK"]
Evaluation_survey_method[ , 3] <- CIdata2011[ , "Y_MARK"]
Evaluation_survey_method[ , 4] <- CIdata2011[ , "p_a_SC"]
colnames(Evaluation_survey_method) <- c("WPT", "X_MARK","Y_MARK","SC_2011")
Evaluation_survey_method

a <- read.table("CIdata2009_25_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2009_25_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2009_25_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2009_25_percent_SURVEYED <- sort(WPTs_CIdata2009_25_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2009_25_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2009_25_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_25_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2011)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2009_25_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_25_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2009_25_percent_SURVEYED # check ratio
Performance_evaluation[5, 1] <- SC_detected_CIdata2009_25_percent_SURVEYED # add to the final table the percentage of SC found 
# when 25% of 2001 - 2009 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 

############################# Evaluate performance of model built with 2001 - 2003 - 2005 - 2007 - 2009 - 2011 data and using 2013 SC distribution to evaluate #############
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

a <- read.table("CIdata2011_25_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2011_25_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2011_25_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2011_25_percent_SURVEYED <- sort(WPTs_CIdata2011_25_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2011_25_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2011_25_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_25_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2013)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2011_25_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_25_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2011_25_percent_SURVEYED # check ratio
Performance_evaluation[6, 1] <- SC_detected_CIdata2011_25_percent_SURVEYED # add to the final table the percentage of SC found 
# when 25% of 2001 - 2011 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 


############################# Evaluate performance of model built with 2001 - 2003 - 2005 - 2007 - 2009 - 2011 - 2013 data and using 2015 SC distribution to evaluate #############
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

a <- read.table("CIdata2013_25_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2013_25_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2013_25_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2013_25_percent_SURVEYED <- sort(WPTs_CIdata2013_25_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2013_25_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2013_25_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_25_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2015)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2013_25_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_25_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2013_25_percent_SURVEYED # check ratio

Performance_evaluation[7, 1] <- SC_detected_CIdata2013_25_percent_SURVEYED # add to the final table the percentage of SC found 
# when 25% of 2001 - 2011 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 





#########################################################################################################################################
###################################################### 50% of data ######################################################################
#########################################################################################################################################

############################# Evaluate performance of model built with 2001 data and using 2003 SC distribution to evaluate #############
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

a <- read.table("CIdata2001_50_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2001_50_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2001_50_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2001_50_percent_SURVEYED <- sort(WPTs_CIdata2001_50_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2001_50_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2001_50_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_50_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2003)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2001_50_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_50_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2001_50_percent_SURVEYED # check ratio
Performance_evaluation[1, 2] <- SC_detected_CIdata2001_50_percent_SURVEYED # add to the final table the percentage of SC found 
# when 50% of 2001 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 

############################# Evaluate performance of model built with 2001 and 2003 data and using 2005 SC distribution to evaluate #############
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

a <- read.table("CIdata2003_50_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2003_50_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2003_50_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2003_50_percent_SURVEYED <- sort(WPTs_CIdata2003_50_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2003_50_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2003_50_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_50_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2005)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2003_50_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_50_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2003_50_percent_SURVEYED # check ratio
Performance_evaluation[2, 2] <- SC_detected_CIdata2003_50_percent_SURVEYED # add to the final table the percentage of SC found 
# when 50% of 2001 and 2003 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 

############################# Evaluate performance of model built with 2001 - 2003 - 2005 data and using 2007 SC distribution to evaluate #############
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

a <- read.table("CIdata2005_50_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2005_50_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2005_50_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2005_50_percent_SURVEYED <- sort(WPTs_CIdata2005_50_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2005_50_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2005_50_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_50_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2007)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2005_50_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_50_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2005_50_percent_SURVEYED # check ratio
Performance_evaluation[3, 2] <- SC_detected_CIdata2005_50_percent_SURVEYED # add to the final table the percentage of SC found 
# when 50% of 2001 - 2005 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 


############################# Evaluate performance of model built with 2001 - 2003 - 2005 - 2007 data and using 2009 SC distribution to evaluate #############
unique(CIdata$Year)
CIdata2009 <- subset(CIdata,CIdata$Year == 2009)
CIdata2009 <- subset(CIdata2009, !duplicated(WPT))
# Create data.frame with WPT, X-mark, Y-mark, SC_2009 (1 or 0)
Evaluation_survey_method <- as.data.frame(matrix(NA, nrow = nrow(CIdata2009), ncol = 4)) # create data.frame 
Evaluation_survey_method[ , 1] <- CIdata2009[ , "WPT"]
Evaluation_survey_method[ , 2] <- CIdata2009[ , "X_MARK"]
Evaluation_survey_method[ , 3] <- CIdata2009[ , "Y_MARK"]
Evaluation_survey_method[ , 4] <- CIdata2009[ , "p_a_SC"]
colnames(Evaluation_survey_method) <- c("WPT", "X_MARK","Y_MARK","SC_2009")
Evaluation_survey_method

a <- read.table("CIdata2007_50_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2007_50_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2007_50_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2007_50_percent_SURVEYED <- sort(WPTs_CIdata2007_50_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2007_50_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2007_50_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_50_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2009)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2007_50_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_50_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2007_50_percent_SURVEYED # check ratio
Performance_evaluation[4, 2] <- SC_detected_CIdata2007_50_percent_SURVEYED # add to the final table the percentage of SC found 
# when 50% of 2001 - 2007 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 


############################# Evaluate performance of model built with 2001 - 2003 - 2005 - 2007 - 2009 data and using 2011 SC distribution to evaluate #############
unique(CIdata$Year)
CIdata2011 <- subset(CIdata,CIdata$Year == 2011)
CIdata2011 <- subset(CIdata2011, !duplicated(WPT))
# Create data.frame with WPT, X-mark, Y-mark, SC_2011 (1 or 0)
Evaluation_survey_method <- as.data.frame(matrix(NA, nrow = nrow(CIdata2011), ncol = 4)) # create data.frame 
Evaluation_survey_method[ , 1] <- CIdata2011[ , "WPT"]
Evaluation_survey_method[ , 2] <- CIdata2011[ , "X_MARK"]
Evaluation_survey_method[ , 3] <- CIdata2011[ , "Y_MARK"]
Evaluation_survey_method[ , 4] <- CIdata2011[ , "p_a_SC"]
colnames(Evaluation_survey_method) <- c("WPT", "X_MARK","Y_MARK","SC_2011")
Evaluation_survey_method

a <- read.table("CIdata2009_50_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2009_50_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2009_50_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2009_50_percent_SURVEYED <- sort(WPTs_CIdata2009_50_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2009_50_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2009_50_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_50_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2011)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2009_50_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_50_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2009_50_percent_SURVEYED # check ratio
Performance_evaluation[5, 2] <- SC_detected_CIdata2009_50_percent_SURVEYED # add to the final table the percentage of SC found 
# when 50% of 2001 - 2009 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 

############################# Evaluate performance of model built with 2001 - 2003 - 2005 - 2007 - 2009 - 2011 data and using 2013 SC distribution to evaluate #############
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

a <- read.table("CIdata2011_50_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2011_50_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2011_50_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2011_50_percent_SURVEYED <- sort(WPTs_CIdata2011_50_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2011_50_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2011_50_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_50_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2013)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2011_50_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_50_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2011_50_percent_SURVEYED # check ratio
Performance_evaluation[6, 2] <- SC_detected_CIdata2011_50_percent_SURVEYED # add to the final table the percentage of SC found 
# when 50% of 2001 - 2011 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 


############################# Evaluate performance of model built with 2001 - 2003 - 2005 - 2007 - 2009 - 2011 - 2013 data and using 2015 SC distribution to evaluate #############
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

a <- read.table("CIdata2013_50_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2013_50_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2013_50_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2013_50_percent_SURVEYED <- sort(WPTs_CIdata2013_50_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2013_50_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2013_50_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_50_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2015)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2013_50_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_50_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2013_50_percent_SURVEYED # check ratio

Performance_evaluation[7, 2] <- SC_detected_CIdata2013_50_percent_SURVEYED # add to the final table the percentage of SC found 
# when 50% of 2001 - 2011 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 


##########################################################################################################################################
############################################### 75% of data ##############################################################################
##########################################################################################################################################

############################# Evaluate performance of model built with 2001 data and using 2003 SC distribution to evaluate #############
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

a <- read.table("CIdata2001_75_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2001_75_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2001_75_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2001_75_percent_SURVEYED <- sort(WPTs_CIdata2001_75_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2001_75_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2001_75_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_75_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2003)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2001_75_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_75_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2001_75_percent_SURVEYED # check ratio
Performance_evaluation[1, 3] <- SC_detected_CIdata2001_75_percent_SURVEYED # add to the final table the percentage of SC found 
# when 75% of 2001 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 

############################# Evaluate performance of model built with 2001 and 2003 data and using 2005 SC distribution to evaluate #############
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

a <- read.table("CIdata2003_75_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2003_75_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2003_75_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2003_75_percent_SURVEYED <- sort(WPTs_CIdata2003_75_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2003_75_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2003_75_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_75_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2005)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2003_75_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_75_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2003_75_percent_SURVEYED # check ratio
Performance_evaluation[2, 3] <- SC_detected_CIdata2003_75_percent_SURVEYED # add to the final table the percentage of SC found 
# when 75% of 2001 and 2003 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 

############################# Evaluate performance of model built with 2001 - 2003 - 2005 data and using 2007 SC distribution to evaluate #############
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

a <- read.table("CIdata2005_75_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2005_75_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2005_75_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2005_75_percent_SURVEYED <- sort(WPTs_CIdata2005_75_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2005_75_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2005_75_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_75_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2007)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2005_75_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_75_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2005_75_percent_SURVEYED # check ratio
Performance_evaluation[3, 3] <- SC_detected_CIdata2005_75_percent_SURVEYED # add to the final table the percentage of SC found 
# when 75% of 2001 - 2005 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 


############################# Evaluate performance of model built with 2001 - 2003 - 2005 - 2007 data and using 2009 SC distribution to evaluate #############
unique(CIdata$Year)
CIdata2009 <- subset(CIdata,CIdata$Year == 2009)
CIdata2009 <- subset(CIdata2009, !duplicated(WPT))
# Create data.frame with WPT, X-mark, Y-mark, SC_2009 (1 or 0)
Evaluation_survey_method <- as.data.frame(matrix(NA, nrow = nrow(CIdata2009), ncol = 4)) # create data.frame 
Evaluation_survey_method[ , 1] <- CIdata2009[ , "WPT"]
Evaluation_survey_method[ , 2] <- CIdata2009[ , "X_MARK"]
Evaluation_survey_method[ , 3] <- CIdata2009[ , "Y_MARK"]
Evaluation_survey_method[ , 4] <- CIdata2009[ , "p_a_SC"]
colnames(Evaluation_survey_method) <- c("WPT", "X_MARK","Y_MARK","SC_2009")
Evaluation_survey_method

a <- read.table("CIdata2007_75_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2007_75_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2007_75_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2007_75_percent_SURVEYED <- sort(WPTs_CIdata2007_75_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2007_75_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2007_75_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_75_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2009)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2007_75_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_75_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2007_75_percent_SURVEYED # check ratio
Performance_evaluation[4, 3] <- SC_detected_CIdata2007_75_percent_SURVEYED # add to the final table the percentage of SC found 
# when 75% of 2001 - 2007 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 


############################# Evaluate performance of model built with 2001 - 2003 - 2005 - 2007 - 2009 data and using 2011 SC distribution to evaluate #############
unique(CIdata$Year)
CIdata2011 <- subset(CIdata,CIdata$Year == 2011)
CIdata2011 <- subset(CIdata2011, !duplicated(WPT))
# Create data.frame with WPT, X-mark, Y-mark, SC_2011 (1 or 0)
Evaluation_survey_method <- as.data.frame(matrix(NA, nrow = nrow(CIdata2011), ncol = 4)) # create data.frame 
Evaluation_survey_method[ , 1] <- CIdata2011[ , "WPT"]
Evaluation_survey_method[ , 2] <- CIdata2011[ , "X_MARK"]
Evaluation_survey_method[ , 3] <- CIdata2011[ , "Y_MARK"]
Evaluation_survey_method[ , 4] <- CIdata2011[ , "p_a_SC"]
colnames(Evaluation_survey_method) <- c("WPT", "X_MARK","Y_MARK","SC_2011")
Evaluation_survey_method

a <- read.table("CIdata2009_75_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2009_75_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2009_75_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2009_75_percent_SURVEYED <- sort(WPTs_CIdata2009_75_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2009_75_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2009_75_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_75_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2011)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2009_75_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_75_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2009_75_percent_SURVEYED # check ratio
Performance_evaluation[5, 3] <- SC_detected_CIdata2009_75_percent_SURVEYED # add to the final table the percentage of SC found 
# when 75% of 2001 - 2009 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 

############################# Evaluate performance of model built with 2001 - 2003 - 2005 - 2007 - 2009 - 2011 data and using 2013 SC distribution to evaluate #############
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

a <- read.table("CIdata2011_75_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2011_75_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2011_75_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2011_75_percent_SURVEYED <- sort(WPTs_CIdata2011_75_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2011_75_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2011_75_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_75_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2013)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2011_75_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_75_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2011_75_percent_SURVEYED # check ratio
Performance_evaluation[6, 3] <- SC_detected_CIdata2011_75_percent_SURVEYED # add to the final table the percentage of SC found 
# when 75% of 2001 - 2011 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 


############################# Evaluate performance of model built with 2001 - 2003 - 2005 - 2007 - 2009 - 2011 - 2013 data and using 2015 SC distribution to evaluate #############
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

a <- read.table("CIdata2013_75_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2013_75_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2013_75_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2013_75_percent_SURVEYED <- sort(WPTs_CIdata2013_75_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2013_75_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2013_75_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_75_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2015)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2013_75_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_75_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2013_75_percent_SURVEYED # check ratio

Performance_evaluation[7, 3] <- SC_detected_CIdata2013_75_percent_SURVEYED # add to the final table the percentage of SC found 
# when 75% of 2001 - 2011 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 



########################################################################################################################################
############################################### 100% data ##############################################################################
########################################################################################################################################

############################# Evaluate performance of model built with 2001 data and using 2003 SC distribution to evaluate #############
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

a <- read.table("CIdata2001_100_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2001_100_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2001_100_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2001_100_percent_SURVEYED <- sort(WPTs_CIdata2001_100_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2001_100_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2001_100_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2003[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_100_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2003)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2001_100_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_100_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2001_100_percent_SURVEYED # check ratio
Performance_evaluation[1, 4] <- SC_detected_CIdata2001_100_percent_SURVEYED # add to the final table the percentage of SC found 
# when 100% of 2001 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 

############################# Evaluate performance of model built with 2001 and 2003 data and using 2005 SC distribution to evaluate #############
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

a <- read.table("CIdata2003_100_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2003_100_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2003_100_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2003_100_percent_SURVEYED <- sort(WPTs_CIdata2003_100_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2003_100_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2003_100_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2005[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_100_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2005)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2003_100_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_100_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2003_100_percent_SURVEYED # check ratio
Performance_evaluation[2, 4] <- SC_detected_CIdata2003_100_percent_SURVEYED # add to the final table the percentage of SC found 
# when 100% of 2001 and 2003 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 

############################# Evaluate performance of model built with 2001 - 2003 - 2005 data and using 2007 SC distribution to evaluate #############
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

a <- read.table("CIdata2005_100_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2005_100_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2005_100_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2005_100_percent_SURVEYED <- sort(WPTs_CIdata2005_100_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2005_100_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2005_100_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2007[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_100_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2007)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2005_100_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_100_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2005_100_percent_SURVEYED # check ratio
Performance_evaluation[3, 4] <- SC_detected_CIdata2005_100_percent_SURVEYED # add to the final table the percentage of SC found 
# when 100% of 2001 - 2005 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 


############################# Evaluate performance of model built with 2001 - 2003 - 2005 - 2007 data and using 2009 SC distribution to evaluate #############
unique(CIdata$Year)
CIdata2009 <- subset(CIdata,CIdata$Year == 2009)
CIdata2009 <- subset(CIdata2009, !duplicated(WPT))
# Create data.frame with WPT, X-mark, Y-mark, SC_2009 (1 or 0)
Evaluation_survey_method <- as.data.frame(matrix(NA, nrow = nrow(CIdata2009), ncol = 4)) # create data.frame 
Evaluation_survey_method[ , 1] <- CIdata2009[ , "WPT"]
Evaluation_survey_method[ , 2] <- CIdata2009[ , "X_MARK"]
Evaluation_survey_method[ , 3] <- CIdata2009[ , "Y_MARK"]
Evaluation_survey_method[ , 4] <- CIdata2009[ , "p_a_SC"]
colnames(Evaluation_survey_method) <- c("WPT", "X_MARK","Y_MARK","SC_2009")
Evaluation_survey_method

a <- read.table("CIdata2007_100_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2007_100_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2007_100_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2007_100_percent_SURVEYED <- sort(WPTs_CIdata2007_100_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2007_100_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2007_100_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2009[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_100_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2009)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2007_100_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_100_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2007_100_percent_SURVEYED # check ratio
Performance_evaluation[4, 4] <- SC_detected_CIdata2007_100_percent_SURVEYED # add to the final table the percentage of SC found 
# when 100% of 2001 - 2007 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 


############################# Evaluate performance of model built with 2001 - 2003 - 2005 - 2007 - 2009 data and using 2011 SC distribution to evaluate #############
unique(CIdata$Year)
CIdata2011 <- subset(CIdata,CIdata$Year == 2011)
CIdata2011 <- subset(CIdata2011, !duplicated(WPT))
# Create data.frame with WPT, X-mark, Y-mark, SC_2011 (1 or 0)
Evaluation_survey_method <- as.data.frame(matrix(NA, nrow = nrow(CIdata2011), ncol = 4)) # create data.frame 
Evaluation_survey_method[ , 1] <- CIdata2011[ , "WPT"]
Evaluation_survey_method[ , 2] <- CIdata2011[ , "X_MARK"]
Evaluation_survey_method[ , 3] <- CIdata2011[ , "Y_MARK"]
Evaluation_survey_method[ , 4] <- CIdata2011[ , "p_a_SC"]
colnames(Evaluation_survey_method) <- c("WPT", "X_MARK","Y_MARK","SC_2011")
Evaluation_survey_method

a <- read.table("CIdata2009_100_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2009_100_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2009_100_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2009_100_percent_SURVEYED <- sort(WPTs_CIdata2009_100_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2009_100_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2009_100_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2011[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # the 1s and 0s that are detected during the survey (assuming perfect detection)
# creates a table with Frequencies of 0s and 1s in SC_vector_a -> basically here the 1s and 0s are values 
# obtained from WPTs that are visited during the survey assuming perfect detection  (i.e. 1s and 0s come from the evaluation dataset)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2011)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2009_100_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_100_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2009_100_percent_SURVEYED # check ratio
Performance_evaluation[5, 4] <- SC_detected_CIdata2009_100_percent_SURVEYED # add to the final table the percentage of SC found 
# when 100% of 2001 - 2009 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 

############################# Evaluate performance of model built with 2001 - 2003 - 2005 - 2007 - 2009 - 2011 data and using 2013 SC distribution to evaluate #############
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

a <- read.table("CIdata2011_100_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2011_100_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2011_100_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2011_100_percent_SURVEYED <- sort(WPTs_CIdata2011_100_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2011_100_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2011_100_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2013[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_100_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2013)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2011_100_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_100_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2011_100_percent_SURVEYED # check ratio
Performance_evaluation[6, 4] <- SC_detected_CIdata2011_100_percent_SURVEYED # add to the final table the percentage of SC found 
# when 100% of 2001 - 2011 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 


############################# Evaluate performance of model built with 2001 - 2003 - 2005 - 2007 - 2009 - 2011 - 2013 data and using 2015 SC distribution to evaluate #############
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

a <- read.table("CIdata2013_100_percent_SURVEYED_sitesTOmanage.txt", header=TRUE, sep="\t")
pdf("CIdata2013_100_percent_SURVEYED_sitesTOmanage.pdf")
plot(a$X_MARK, a$Y_MARK, xlab="X-coords", ylab="Y-coords")
dev.off()
WPTs_CIdata2013_100_percent_SURVEYED <- a[ ,"WPT"]
WPTs_CIdata2013_100_percent_SURVEYED <- sort(WPTs_CIdata2013_100_percent_SURVEYED)
SC_vector_a <- vector()
for (i in 1:length(Evaluation_survey_method$WPT)) {
  for (j in 1:length(WPTs_CIdata2013_100_percent_SURVEYED)){
    print(paste("i = ", i, sep=""))
    print(paste("j = ", j, sep=""))
    if (Evaluation_survey_method$WPT[i] == WPTs_CIdata2013_100_percent_SURVEYED[j]) {SC_vector_a <- c(SC_vector_a, Evaluation_survey_method$SC_2015[i])}
  }
}
SC_vector_a
aa <- as.data.frame(table(SC_vector_a)) # creates a table with Frequencies of 0s and 1s in SC_vector_a (or in CIdata2001_100_percent)
ll <- as.data.frame(table(Evaluation_survey_method$SC_2015)) # creates a table with Frequencies of 0s and 1s in the evaluation dataset 2003 distribution
SC_detected_CIdata2013_100_percent_SURVEYED <- aa[2, 2]/ll[2, 2] # calculates the ratio of SC detected in CIdata2001_100_percent over SC in 2003 SC distribution (we are assuming perfect detection)
SC_detected_CIdata2013_100_percent_SURVEYED # check ratio

Performance_evaluation[7, 4] <- SC_detected_CIdata2013_100_percent_SURVEYED # add to the final table the percentage of SC found 
# when 100% of 2001 - 2011 data is used to build a SDM and Knapsack formulation is used to determine which sites to visit for management 
# based on a total budget that has to be divided between survey and management -> yearly budget and each year the data from 
# previous year are added to the model too 


####### WRITE FILES #########
write.table(Performance_evaluation, file="Performance_evaluation_TOTAL_BUDGET_20000.txt", sep="\t")


