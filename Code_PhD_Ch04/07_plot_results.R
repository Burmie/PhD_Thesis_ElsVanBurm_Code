### PLOT AVERAGE RESULTS across indpendent test datasets 

## 00_ZERO_MONITORING_COST (equals constant management budget)
# Total budget 6,000
setwd("Total_budget_6000/00_Zero_monitoring_cost")
average_6000_00 <- vector()

x <- 1:100

for (u in 1:100){
  load("Performance_evaluation_CONSTANT_MANAGEMENT_BUDGET_6000.Rda")
  average_6000_00[u] <- mean(Performance_evaluation[ , u])
  
}

plot(x, average_6000_00, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 6000 survey minutes_zero monitoring cost")
#plot(x, average_6000_00, xlim=c(1,20), ylim=c(0,1), pch=20, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 6000 survey minutes_zero monitoring cost")
for (k in 1:6){
  lines(x, Performance_evaluation[k, ], col="grey")
}

# Calculate number (and percentage of total number) of sites visited for management
average_number_of_sites_6000_00 <- vector()
average_percentage_of_sites_6000_00 <- vector()

for (j in 1:100){
  a <- nrow(read.table((paste("CIdata_2001_2003_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  b <- nrow(read.table((paste("CIdata_2001_2003_2005_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  c <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  d <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  e <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  f <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_2013_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  average_number_of_sites_6000_00[j] <- (a+b+c+d+e+f)/6
  average_percentage_of_sites_6000_00[j] <- ((a+b+c+d+e+f)/6)/837
}

# Total budget 5,000
setwd("../../Total_budget_5000/00_Zero_monitoring_cost")
average_5000_00 <- vector()

x <- 1:100

for (u in 1:100){
  load("Performance_evaluation_CONSTANT_MANAGEMENT_BUDGET_5000.Rda")
  average_5000_00[u] <- mean(Performance_evaluation[ , u])
}

plot(x, average_5000_00, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 5000 survey minutes_zero monitoring cost")
#plot(x, average_5000_00, xlim=c(1,20), ylim=c(0,1), pch=20, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 5000 survey minutes_zero monitoring cost")
for (k in 1:6){
  lines(x, Performance_evaluation[k, ], col="grey")
}


# Calculate number (and percentage of total number) of sites visited for management
average_number_of_sites_5000_00 <- vector()
average_percentage_of_sites_5000_00 <- vector()

for (j in 1:100){
  a <- nrow(read.table((paste("CIdata_2001_2003_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  b <- nrow(read.table((paste("CIdata_2001_2003_2005_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  c <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  d <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  e <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  f <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_2013_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  average_number_of_sites_5000_00[j] <- (a+b+c+d+e+f)/6
  average_percentage_of_sites_5000_00[j] <- ((a+b+c+d+e+f)/6)/837
}

# Total budget 4,000
setwd("../../Total_budget_4000/00_Zero_monitoring_cost")
average_4000_00 <- vector()
se_average_4000_00 <- vector()
average_and_se <- matrix(NA, 100, 2)

x <- 1:100

for (u in 1:100){
  load("Performance_evaluation_CONSTANT_MANAGEMENT_BUDGET_4000.Rda")
  average_4000_00[u] <- mean(Performance_evaluation[ , u])
}

for (u in 1:100){
  load("Performance_evaluation_CONSTANT_MANAGEMENT_BUDGET_4000.Rda")
  se_average_4000_00[u] <- sd(Performance_evaluation[ , u])/sqrt(length(Performance_evaluation[ , u]))
}

average_and_se[ ,1] <- average_4000_00
average_and_se[ ,2] <- se_average_4000_00

plot(x, average_4000_00, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 4000 survey minutes_zero monitoring cost")
#plot(x, average_4000_00, xlim=c(1,20), ylim=c(0,1), pch=20, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 4000 survey minutes_zero monitoring cost")
for (k in 1:6){
  lines(x, Performance_evaluation[k, ], col="grey")
}


# Calculate number (and percentage of total number) of sites visited for management
average_number_of_sites_4000_00 <- vector()
average_percentage_of_sites_4000_00 <- vector()

for (j in 1:100){
  a <- nrow(read.table((paste("CIdata_2001_2003_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  b <- nrow(read.table((paste("CIdata_2001_2003_2005_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  c <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  d <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  e <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  f <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_2013_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  average_number_of_sites_4000_00[j] <- (a+b+c+d+e+f)/6
  average_percentage_of_sites_4000_00[j] <- ((a+b+c+d+e+f)/6)/837
}

# Total budget 3,000
setwd("../../Total_budget_3000/00_Zero_monitoring_cost")
average_3000_00 <- vector()

x <- 1:100

for (u in 1:100){
  load("Performance_evaluation_CONSTANT_MANAGEMENT_BUDGET_3000.Rda")
  average_3000_00[u] <- mean(Performance_evaluation[ , u])
}

plot(x, average_3000_00, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 3000 survey minutes_zero monitoring cost")
#plot(x, average_3000_00, xlim=c(1,20), ylim=c(0,1), pch=20, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 3000 survey minutes_zero monitoring cost")
for (k in 1:6){
  lines(x, Performance_evaluation[k, ], col="grey")
}



# Calculate number (and percentage of total number) of sites visited for management
average_number_of_sites_3000_00 <- vector()
average_percentage_of_sites_3000_00 <- vector()

for (j in 1:100){
  a <- nrow(read.table((paste("CIdata_2001_2003_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  b <- nrow(read.table((paste("CIdata_2001_2003_2005_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  c <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  d <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  e <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  f <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_2013_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  average_number_of_sites_3000_00[j] <- (a+b+c+d+e+f)/6
  average_percentage_of_sites_3000_00[j] <- ((a+b+c+d+e+f)/6)/837
}

# Total budget 2,000
setwd("../../Total_budget_2000/00_Zero_monitoring_cost")
average_2000_00 <- vector()

x <- 1:100

for (u in 1:100){
  load("Performance_evaluation_CONSTANT_MANAGEMENT_BUDGET_2000.Rda")
  average_2000_00[u] <- mean(Performance_evaluation[ , u])
}

plot(x, average_2000_00, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 2000 survey minutes_zero monitoring cost")
#plot(x, average_2000_00, xlim=c(1,20), ylim=c(0,1), pch=20, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 2000 survey minutes_zero monitoring cost")
for (k in 1:6){
  lines(x, Performance_evaluation[k, ], col="grey")
}



# Calculate number (and percentage of total number) of sites visited for management
average_number_of_sites_2000_00 <- vector()
average_percentage_of_sites_2000_00 <- vector()

for (j in 1:100){
  a <- nrow(read.table((paste("CIdata_2001_2003_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  b <- nrow(read.table((paste("CIdata_2001_2003_2005_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  c <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  d <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  e <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  f <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_2013_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  average_number_of_sites_2000_00[j] <- (a+b+c+d+e+f)/6
  average_percentage_of_sites_2000_00[j] <- ((a+b+c+d+e+f)/6)/837
}

# Total budget 1,000
setwd("../../Total_budget_1000/00_Zero_monitoring_cost")
average_1000_00 <- vector()

x <- 1:100

for (u in 1:100){
  load("Performance_evaluation_CONSTANT_MANAGEMENT_BUDGET_1000.Rda")
  average_1000_00[u] <- mean(Performance_evaluation[ , u])
}

plot(x, average_1000_00, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 1000 survey minutes_zero monitoring cost")
#plot(x, average_1000_00, xlim=c(1,20), ylim=c(0,1), pch=20, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 1000 survey minutes_zero monitoring cost")
for (k in 1:6){
  lines(x, Performance_evaluation[k, ], col="grey")
}



# Calculate number (and percentage of total number) of sites visited for management
average_number_of_sites_1000_00 <- vector()
average_percentage_of_sites_1000_00 <- vector()

for (j in 1:100){
  a <- nrow(read.table((paste("CIdata_2001_2003_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  b <- nrow(read.table((paste("CIdata_2001_2003_2005_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  c <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  d <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  e <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  f <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_2013_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  average_number_of_sites_1000_00[j] <- (a+b+c+d+e+f)/6
  average_percentage_of_sites_1000_00[j] <- ((a+b+c+d+e+f)/6)/837
}


## 01_INTERMEDIATE_MONITORING_COST
# Total budget 6,000
setwd("../../Total_budget_6000/01_Intermediate_monitoring_cost")
average_6000_01 <- vector()

x <- 1:100

for (u in 1:100){
  load("Performance_evaluation_Total_BUDGET_6000_MonCost_intermediate.Rda")
  average_6000_01[u] <- mean(Performance_evaluation[ , u])
}

plot(x, average_6000_01, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 6000 survey minutes_intermediate monitoring cost")
#plot(x, average_6000_01, xlim=c(1,20), ylim=c(0,1), pch=20, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 6000 survey minutes_intermediate monitoring cost")
for (k in 1:6){
  lines(x, Performance_evaluation[k, ], col="grey")
}



# Calculate number (and percentage of total number) of sites visited for management
average_number_of_sites_6000_01 <- vector()
average_percentage_of_sites_6000_01 <- vector()

for (j in 1:100){
  a <- nrow(read.table((paste("CIdata_2001_2003_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  b <- nrow(read.table((paste("CIdata_2001_2003_2005_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  c <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  d <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  e <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  f <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_2013_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  average_number_of_sites_6000_01[j] <- (a+b+c+d+e+f)/6
  average_percentage_of_sites_6000_01[j] <- ((a+b+c+d+e+f)/6)/837
}


# Total budget 5,000
setwd("../../Total_budget_5000/01_Intermediate_monitoring_cost")
average_5000_01 <- vector()

x <- 1:100

for (u in 1:100){
  load("Performance_evaluation_Total_BUDGET_5000_MonCost_intermediate.Rda")
  average_5000_01[u] <- mean(Performance_evaluation[ , u])
}

plot(x, average_5000_01, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 5000 survey minutes_intermediate monitoring cost")
plot(x, average_5000_01, xlim=c(1,20), ylim=c(0,1), pch=20, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 5000 survey minutes_intermediate monitoring cost")

# Calculate number (and percentage of total number) of sites visited for management
average_number_of_sites_5000_01 <- vector()
average_percentage_of_sites_5000_01 <- vector()

for (j in 1:100){
  a <- nrow(read.table((paste("CIdata_2001_2003_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  b <- nrow(read.table((paste("CIdata_2001_2003_2005_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  c <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  d <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  e <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  f <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_2013_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  average_number_of_sites_5000_01[j] <- (a+b+c+d+e+f)/6
  average_percentage_of_sites_5000_01[j] <- ((a+b+c+d+e+f)/6)/837
}


# Total budget 4,000
setwd("../../Total_budget_4000/01_Intermediate_monitoring_cost")
average_4000_01 <- vector()

x <- 1:100

for (u in 1:100){
  load("Performance_evaluation_Total_BUDGET_4000_MonCost_intermediate.Rda")
  average_4000_01[u] <- mean(Performance_evaluation[ , u])
}

plot(x, average_4000_01, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 4000 survey minutes_intermediate monitoring cost")
#plot(x, average_4000_01, xlim=c(1,20), ylim=c(0,1), pch=20, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 4000 survey minutes_intermediate monitoring cost")
for (k in 1:6){
  lines(x, Performance_evaluation[k, ], col="grey")
}



# Calculate number (and percentage of total number) of sites visited for management
average_number_of_sites_4000_01 <- vector()
average_percentage_of_sites_4000_01 <- vector()

for (j in 1:100){
  a <- nrow(read.table((paste("CIdata_2001_2003_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  b <- nrow(read.table((paste("CIdata_2001_2003_2005_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  c <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  d <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  e <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  f <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_2013_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  average_number_of_sites_4000_01[j] <- (a+b+c+d+e+f)/6
  average_percentage_of_sites_4000_01[j] <- ((a+b+c+d+e+f)/6)/837
}


# Total budget 3,000
setwd("../../Total_budget_3000/01_Intermediate_monitoring_cost")
average_3000_01 <- vector()

x <- 1:100

for (u in 1:100){
  load("Performance_evaluation_Total_BUDGET_3000_MonCost_intermediate.Rda")
  average_3000_01[u] <- mean(Performance_evaluation[ , u])
}

plot(x, average_3000_01, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites surveyed", ylab="%SC managed", main="Total budget 3000 survey minutes_intermediate monitoring cost")
#plot(x, average_3000_01, xlim=c(1,20), ylim=c(0,1), pch=20, xlab="% of sites surveyed", ylab="%SC managed", main="Total budget 3000 survey minutes_intermediate monitoring cost")
for (k in 1:6){
  lines(x, Performance_evaluation[k, ], col="grey")
}



# Calculate number (and percentage of total number) of sites visited for management
average_number_of_sites_3000_01 <- vector()
average_percentage_of_sites_3000_01 <- vector()

for (j in 1:100){
  a <- nrow(read.table((paste("CIdata_2001_2003_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  b <- nrow(read.table((paste("CIdata_2001_2003_2005_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  c <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  d <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  e <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  f <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_2013_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  average_number_of_sites_3000_01[j] <- (a+b+c+d+e+f)/6
  average_percentage_of_sites_3000_01[j] <- ((a+b+c+d+e+f)/6)/837
}

# Total budget 2,000
setwd("../../Total_budget_2000/01_Intermediate_monitoring_cost")
average_2000_01 <- vector()

x <- 1:100

for (u in 1:100){
  load("Performance_evaluation_Total_BUDGET_2000_MonCost_intermediate.Rda")
  average_2000_01[u] <- mean(Performance_evaluation[ , u])
}

plot(x, average_2000_01, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 2000 survey minutes_intermediate monitoring cost")
#plot(x, average_2000_01, xlim=c(1,20), ylim=c(0,1), pch=20, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 2000 survey minutes_intermediate monitoring cost")
for (k in 1:6){
  lines(x, Performance_evaluation[k, ], col="grey")
}



# Calculate number (and percentage of total number) of sites visited for management
average_number_of_sites_2000_01 <- vector()
average_percentage_of_sites_2000_01 <- vector()

for (j in 1:100){
  a <- nrow(read.table((paste("CIdata_2001_2003_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  b <- nrow(read.table((paste("CIdata_2001_2003_2005_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  c <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  d <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  e <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  f <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_2013_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  average_number_of_sites_2000_01[j] <- (a+b+c+d+e+f)/6
  average_percentage_of_sites_2000_01[j] <- ((a+b+c+d+e+f)/6)/837
}


# Total budget 1,000
setwd("../../Total_budget_1000/01_Intermediate_monitoring_cost")
average_1000_01 <- vector()

x <- 1:100

for (u in 1:100){
  load("Performance_evaluation_Total_BUDGET_1000_MonCost_intermediate.Rda")
  average_1000_01[u] <- mean(Performance_evaluation[ , u])
}

plot(x, average_1000_01, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 1000 survey minutes_intermediate monitoring cost")
#plot(x, average_1000_01, xlim=c(1,20), ylim=c(0,1), pch=20, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 1000 survey minutes_intermediate monitoring cost")
for (k in 1:6){
  lines(x, Performance_evaluation[k, ], col="grey")
}



# Calculate number (and percentage of total number) of sites visited for management
average_number_of_sites_1000_01 <- vector()
average_percentage_of_sites_1000_01 <- vector()

for (j in 1:100){
  a <- nrow(read.table((paste("CIdata_2001_2003_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  b <- nrow(read.table((paste("CIdata_2001_2003_2005_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  c <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  d <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  e <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  f <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_2013_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  average_number_of_sites_1000_01[j] <- (a+b+c+d+e+f)/6
  average_percentage_of_sites_1000_01[j] <- ((a+b+c+d+e+f)/6)/837
}



## 02_RESCALED_MONITORING_COST
# Total budget 6,000
setwd("../../Total_budget_6000/02_Rescaled_monitoring_cost")
average_6000_02 <- vector()

x <- 1:100

for (u in 1:100){
  load("Performance_evaluation_Total_BUDGET_6000_MonCost_rescaled.Rda")
  average_6000_02[u] <- mean(Performance_evaluation[ , u])
}

plot(x, average_6000_02, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 6000 survey minutes_rescaled monitoring cost")
plot(x, average_6000_02, xlim=c(1,20), ylim=c(0,1), pch=20, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 6000 survey minutes_rescaled monitoring cost")

# Calculate number (and percentage of total number) of sites visited for management
average_number_of_sites_6000_02 <- vector()
average_percentage_of_sites_6000_02 <- vector()

for (j in 1:100){
  a <- nrow(read.table((paste("CIdata_2001_2003_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  b <- nrow(read.table((paste("CIdata_2001_2003_2005_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  c <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  d <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  e <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  f <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_2013_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  average_number_of_sites_6000_02[j] <- (a+b+c+d+e+f)/6
  average_percentage_of_sites_6000_02[j] <- ((a+b+c+d+e+f)/6)/837
}


# Total budget 5,000
setwd("../../Total_budget_5000/02_Rescaled_monitoring_cost")
average_5000_02 <- vector()

x <- 1:100

for (u in 1:100){
  load("Performance_evaluation_Total_BUDGET_5000_MonCost_rescaled.Rda")
  average_5000_02[u] <- mean(Performance_evaluation[ , u])
}

plot(x, average_5000_02, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 5000 survey minutes_rescaled monitoring cost")
#plot(x, average_5000_02, xlim=c(1,20), ylim=c(0,1), pch=20, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 5000 survey minutes_rescaled monitoring cost")
for (k in 1:6){
  lines(x, Performance_evaluation[k, ], col="grey")
}



# Calculate number (and percentage of total number) of sites visited for management
average_number_of_sites_5000_02 <- vector()
average_percentage_of_sites_5000_02 <- vector()

for (j in 1:100){
  a <- nrow(read.table((paste("CIdata_2001_2003_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  b <- nrow(read.table((paste("CIdata_2001_2003_2005_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  c <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  d <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  e <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  f <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_2013_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  average_number_of_sites_5000_02[j] <- (a+b+c+d+e+f)/6
  average_percentage_of_sites_5000_02[j] <- ((a+b+c+d+e+f)/6)/837
}


# Total budget 4,000
setwd("../../Total_budget_4000/02_Rescaled_monitoring_cost")
average_4000_02 <- vector()

x <- 1:100

for (u in 1:100){
  load("Performance_evaluation_Total_BUDGET_4000_MonCost_rescaled.Rda")
  average_4000_02[u] <- mean(Performance_evaluation[ , u])
}

plot(x, average_4000_02, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites surveyed", ylab="%SC managed", main="Total budget 4000 survey minutes_rescaled monitoring cost")
#plot(x, average_4000_02, xlim=c(1,20), ylim=c(0,1), pch=20, xlab="% of sites surveyed", ylab="%SC managed", main="Total budget 4000 survey minutes_rescaled monitoring cost")
for (k in 1:6){
  lines(x, Performance_evaluation[k, ], col="grey")
}


# Calculate number (and percentage of total number) of sites visited for management
average_number_of_sites_4000_02 <- vector()
average_percentage_of_sites_4000_02 <- vector()

for (j in 1:100){
  a <- nrow(read.table((paste("CIdata_2001_2003_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  b <- nrow(read.table((paste("CIdata_2001_2003_2005_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  c <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  d <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  e <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  f <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_2013_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  average_number_of_sites_4000_02[j] <- (a+b+c+d+e+f)/6
  average_percentage_of_sites_4000_02[j] <- ((a+b+c+d+e+f)/6)/837
}

# Total budget 3,000
setwd("../../Total_budget_3000/02_Rescaled_monitoring_cost")
average_3000_02 <- vector()

x <- 1:100

for (u in 1:100){
  load("Performance_evaluation_Total_BUDGET_3000_MonCost_rescaled.Rda")
  average_3000_02[u] <- mean(Performance_evaluation[ , u])
}

plot(x, average_3000_02, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites surveyed", ylab="%SC managed", main="Total budget 3000 survey minutes_rescaled monitoring cost")
#plot(x, average_3000_02, xlim=c(1,20), ylim=c(0,1), pch=20, xlab="% of sites surveyed", ylab="%SC managed", main="Total budget 3000 survey minutes_rescaled monitoring cost")
for (k in 1:6){
  lines(x, Performance_evaluation[k, ], col="grey")
}



# Calculate number (and percentage of total number) of sites visited for management
average_number_of_sites_3000_02 <- vector()
average_percentage_of_sites_3000_02 <- vector()

for (j in 1:100){
  a <- nrow(read.table((paste("CIdata_2001_2003_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  b <- nrow(read.table((paste("CIdata_2001_2003_2005_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  c <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  d <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  e <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  f <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_2013_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  average_number_of_sites_3000_02[j] <- (a+b+c+d+e+f)/6
  average_percentage_of_sites_3000_02[j] <- ((a+b+c+d+e+f)/6)/837
}


# Total budget 2,000
setwd("../../Total_budget_2000/02_Rescaled_monitoring_cost")
average_2000_02 <- vector()

x <- 1:100

for (u in 1:100){
  load("Performance_evaluation_Total_BUDGET_2000_MonCost_rescaled.Rda")
  average_2000_02[u] <- mean(Performance_evaluation[ , u])
}

plot(x, average_2000_02, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites surveyed", ylab="%SC managed", main="Total budget 2000 survey minutes_rescaled monitoring cost")
#plot(x, average_2000_02, xlim=c(1,20), ylim=c(0,1), pch=20, xlab="% of sites surveyed", ylab="%SC managed", main="Total budget 2000 survey minutes_rescaled monitoring cost")
for (k in 1:6){
  lines(x, Performance_evaluation[k, ], col="grey")
}



# Calculate number (and percentage of total number) of sites visited for management
average_number_of_sites_2000_02 <- vector()
average_percentage_of_sites_2000_02 <- vector()

for (j in 1:100){
  a <- nrow(read.table((paste("CIdata_2001_2003_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  b <- nrow(read.table((paste("CIdata_2001_2003_2005_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  c <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  d <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  e <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  f <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_2013_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  average_number_of_sites_2000_02[j] <- (a+b+c+d+e+f)/6
  average_percentage_of_sites_2000_02[j] <- ((a+b+c+d+e+f)/6)/837
}

# Total budget 1,000
setwd("../../Total_budget_1000/02_Rescaled_monitoring_cost")
average_1000_02 <- vector()

x <- 1:100

for (u in 1:100){
  load("Performance_evaluation_Total_BUDGET_1000_MonCost_rescaled.Rda")
  average_1000_02[u] <- mean(Performance_evaluation[ , u])
}

plot(x, average_1000_02, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 1000 survey minutes_rescaled monitoring cost")
#plot(x, average_1000_02, xlim=c(1,20), ylim=c(0,1), pch=20, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 1000 survey minutes_rescaled monitoring cost")
for (k in 1:6){
  lines(x, Performance_evaluation[k, ], col="grey")
}



# Calculate number (and percentage of total number) of sites visited for management
average_number_of_sites_1000_02 <- vector()
average_percentage_of_sites_1000_02 <- vector()

for (j in 1:100){
  a <- nrow(read.table((paste("CIdata_2001_2003_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  b <- nrow(read.table((paste("CIdata_2001_2003_2005_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  c <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  d <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  e <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  f <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_2013_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  average_number_of_sites_1000_02[j] <- (a+b+c+d+e+f)/6
  average_percentage_of_sites_1000_02[j] <- ((a+b+c+d+e+f)/6)/837
}


## 03_ACTUAL_MONITORING_COST
# Total budget 6,000
setwd("../../Total_budget_6000/03_Actual_monitoring_cost")
average_6000_03 <- vector()

x <- 1:100

for (u in 1:100){
  load("Performance_evaluation_Total_BUDGET_6000_MonCost_actual.Rda")
  average_6000_03[u] <- mean(Performance_evaluation[ , u])
}

plot(x, average_6000_03, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 6000 survey minutes_actual monitoring cost")
#plot(x, average_6000_03, xlim=c(1,20), ylim=c(0,1), pch=20, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 6000 survey minutes_actual monitoring cost")
for (k in 1:6){
  lines(x, Performance_evaluation[k, ], col="grey")
}



# Calculate number (and percentage of total number) of sites visited for management
average_number_of_sites_6000_03 <- vector()
average_percentage_of_sites_6000_03 <- vector()

for (j in 1:100){
  a <- nrow(read.table((paste("CIdata_2001_2003_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  b <- nrow(read.table((paste("CIdata_2001_2003_2005_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  c <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  d <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  e <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  f <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_2013_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  average_number_of_sites_6000_03[j] <- (a+b+c+d+e+f)/6
  average_percentage_of_sites_6000_03[j] <- ((a+b+c+d+e+f)/6)/837
}


# Total budget 5,000
setwd("../../Total_budget_5000/03_Actual_monitoring_cost")
average_5000_03 <- vector()

x <- 1:100

for (u in 1:100){
  load("Performance_evaluation_Total_BUDGET_5000_MonCost_actual.Rda")
  average_5000_03[u] <- mean(Performance_evaluation[ , u])
}

plot(x, average_5000_03, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 5000 survey minutes_actual monitoring cost")
#plot(x, average_5000_03, xlim=c(1,20), ylim=c(0,1), pch=20, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 5000 survey minutes_actual monitoring cost")
for (k in 1:6){
  lines(x, Performance_evaluation[k, ], col="grey")
}



# Calculate number (and percentage of total number) of sites visited for management
average_number_of_sites_5000_03 <- vector()
average_percentage_of_sites_5000_03 <- vector()

for (j in 1:100){
  a <- nrow(read.table((paste("CIdata_2001_2003_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  b <- nrow(read.table((paste("CIdata_2001_2003_2005_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  c <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  d <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  e <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  f <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_2013_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  average_number_of_sites_5000_03[j] <- (a+b+c+d+e+f)/6
  average_percentage_of_sites_5000_03[j] <- ((a+b+c+d+e+f)/6)/837
}

# Total budget 4,000
setwd("../../Total_budget_4000/03_Actual_monitoring_cost")
average_4000_03 <- vector()

x <- 1:100

for (u in 1:100){
  load("Performance_evaluation_Total_BUDGET_4000_MonCost_actual.Rda")
  average_4000_03[u] <- mean(Performance_evaluation[ , u])
}

plot(x, average_4000_03, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 4000 survey minutes_actual monitoring cost")
#plot(x, average_4000_03, xlim=c(1,20), ylim=c(0,1), pch=20, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 4000 survey minutes_actual monitoring cost")
for (k in 1:6){
  lines(x, Performance_evaluation[k, ], col="grey")
}



# Calculate number (and percentage of total number) of sites visited for management
average_number_of_sites_4000_03 <- vector()
average_percentage_of_sites_4000_03 <- vector()

for (j in 1:100){
  a <- nrow(read.table((paste("CIdata_2001_2003_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  b <- nrow(read.table((paste("CIdata_2001_2003_2005_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  c <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  d <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  e <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  f <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_2013_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  average_number_of_sites_4000_03[j] <- (a+b+c+d+e+f)/6
  average_percentage_of_sites_4000_03[j] <- ((a+b+c+d+e+f)/6)/837
}


# Total budget 3,000
setwd("../../Total_budget_3000/03_Actual_monitoring_cost")
average_3000_03 <- vector()

x <- 1:100

for (u in 1:100){
  load("Performance_evaluation_Total_BUDGET_3000_MonCost_actual.Rda")
  average_3000_03[u] <- mean(Performance_evaluation[ , u])
}

plot(x, average_3000_03, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 3000 survey minutes_actual monitoring cost")
#plot(x, average_3000_03, xlim=c(1,20), ylim=c(0,1), pch=20, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 3000 survey minutes_actual monitoring cost")
for (k in 1:6){
  lines(x, Performance_evaluation[k, ], col="grey")
}



# Calculate number (and percentage of total number) of sites visited for management
average_number_of_sites_3000_03 <- vector()
average_percentage_of_sites_3000_03 <- vector()

for (j in 1:100){
  a <- nrow(read.table((paste("CIdata_2001_2003_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  b <- nrow(read.table((paste("CIdata_2001_2003_2005_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  c <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  d <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  e <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  f <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_2013_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  average_number_of_sites_3000_03[j] <- (a+b+c+d+e+f)/6
  average_percentage_of_sites_3000_03[j] <- ((a+b+c+d+e+f)/6)/837
}

# Total budget 2,000
setwd("../../Total_budget_2000/03_Actual_monitoring_cost")
average_2000_03 <- vector()

x <- 1:100

for (u in 1:100){
  load("Performance_evaluation_Total_BUDGET_2000_MonCost_actual.Rda")
  average_2000_03[u] <- mean(Performance_evaluation[ , u])
}

plot(x, average_2000_03, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 2000 survey minutes_actual monitoring cost")
#plot(x, average_2000_03, xlim=c(1,20), ylim=c(0,1), pch=20, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 2000 survey minutes_actual monitoring cost")
for (k in 1:6){
  lines(x, Performance_evaluation[k, ], col="grey")
}



# Calculate number (and percentage of total number) of sites visited for management
average_number_of_sites_2000_03 <- vector()
average_percentage_of_sites_2000_03 <- vector()

for (j in 1:100){
  a <- nrow(read.table((paste("CIdata_2001_2003_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  b <- nrow(read.table((paste("CIdata_2001_2003_2005_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  c <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  d <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  e <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  f <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_2013_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  average_number_of_sites_2000_03[j] <- (a+b+c+d+e+f)/6
  average_percentage_of_sites_2000_03[j] <- ((a+b+c+d+e+f)/6)/837
}


# Total budget 1,000
setwd("../../Total_budget_1000/03_Actual_monitoring_cost")
average_1000_03 <- vector()

x <- 1:100

for (u in 1:100){
  load("Performance_evaluation_Total_BUDGET_1000_MonCost_actual.Rda")
  average_1000_03[u] <- mean(Performance_evaluation[ , u])
}

plot(x, average_1000_03, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 1000 survey minutes_actual monitoring cost")
#plot(x, average_1000_03, xlim=c(1,20), ylim=c(0,1), pch=20, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 1000 survey minutes_actual monitoring cost")
for (k in 1:6){
  lines(x, Performance_evaluation[k, ], col="grey")
}



# Calculate number (and percentage of total number) of sites visited for management
average_number_of_sites_1000_03 <- vector()
average_percentage_of_sites_1000_03 <- vector()

for (j in 1:100){
  a <- nrow(read.table((paste("CIdata_2001_2003_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  b <- nrow(read.table((paste("CIdata_2001_2003_2005_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  c <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  d <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  e <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  f <- nrow(read.table((paste("CIdata_2001_2003_2005_2007_2009_2011_2013_",j,"_percent_SURVEYED_sitesTOmanage.txt", sep="")), header=TRUE, sep="\t"))
  average_number_of_sites_1000_03[j] <- (a+b+c+d+e+f)/6
  average_percentage_of_sites_1000_03[j] <- ((a+b+c+d+e+f)/6)/837
}





### COMBINE 4 SCENARIOS OF MONITORING COST IN 1 GRAPH 
## Performance vs % sites monitored (ie actual amount spent on monitoring)
## Number of sites visited for management vs % sites monitored
## Performance vs fraction of total budget spent on monitoring 
## Percentage of sites visited for managment vs % sites monitored

## Run this separately for each scenario of survey cost- so that CIdata2001_SURVEY_COST is correct 
## (this means run script 04c_Alternative_Survey_Costs first)



# total budget 6000 minutes 
total_budget <- 6000

x <- 1:100

plot(x, average_6000_00, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 6,000 survey minutes")
lines(x, average_6000_01, type="l", lty=2)
lines(x, average_6000_02, type="l", lty=3)
lines(x, average_6000_03, type="l", lty=4)

plot(x, average_number_of_sites_6000_00, type="l", xlim=c(0,100), ylim=c(0,900), xlab="% of sites monitored", ylab="Number of sites visited for management", main="Total budget 6,000 survey minutes")
lines(x, average_number_of_sites_6000_01, type="l", lty=2)
lines(x, average_number_of_sites_6000_02, type="l", lty=3)
lines(x, average_number_of_sites_6000_03, type="l", lty=4)

plot(x, average_percentage_of_sites_6000_00, type="l", xlim=c(0,100), ylim=c(0,1), xlab="% of sites monitored", ylab="Percentage of sites visited for management", main="Total budget 6,000 survey minutes")
lines(x, average_percentage_of_sites_6000_01, type="l", lty=2)
lines(x, average_percentage_of_sites_6000_02, type="l", lty=3)
lines(x, average_percentage_of_sites_6000_03, type="l", lty=4)

## PLOT PERFORMANCE VS FRACTION OF TOTAL BUDGET SPENT ON MONITORING (optimum should shift to left when comparing across increasing total budget)
# Declare vector to store fraction of total budget spent on monitoring
FRACTION_SURVEY_COST <- vector()

# plot % SC managed vs fraction of budget spent on monitoring - this needs to be done separately for each (of the 3) monitoring cost scenario
# actual (03), rescaled (02) and intermediate (01) monitoring cost  
# we don't want to plot this for the scenario where monitoring cost is zero (and management budget is constant)

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

for (i in 1:100){
  FRACTION_SURVEY_COST[i] <- CIdata2001_SURVEY_COST[i]/total_budget # survey cost for 2001 but does not matter which year because equal survey cost in each year
}
y <- FRACTION_SURVEY_COST
plot(y, average_6000_01, type="l", xlim=c(0,1), ylim=c(0,1), pch=1, xlab="Fraction of total budget spent on monitoring", ylab="%SC managed", main="Total budget 6,000 survey minutes- intermediate monitoring cost", lty=2)

# Calculate monitoring cost for each fraction of the island - rescaled monitoring cost
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

for (i in 1:100){
  FRACTION_SURVEY_COST[i] <- CIdata2001_SURVEY_COST[i]/total_budget # survey cost for 2001 but does not matter which year because equal survey cost in each year
}
y <- FRACTION_SURVEY_COST
plot(y, average_6000_02, type="l", xlim=c(0,1), ylim=c(0,1), pch=1, xlab="Fraction of total budget spent on monitoring", ylab="%SC managed", main="Total budget 6,000 survey minutes- rescaled monitoring cost")

# Calculate monitoring cost for each fraction of the island - actual monitoring cost
for (i in 1:100){
  CIdata2001_SURVEY_COST[i] <- sum(CIdata2001_list[[i]]$COST_Travel_plus_Survey)
  CIdata2003_SURVEY_COST[i] <- sum(CIdata2003_list[[i]]$COST_Travel_plus_Survey)
  CIdata2005_SURVEY_COST[i] <- sum(CIdata2005_list[[i]]$COST_Travel_plus_Survey)
  CIdata2007_SURVEY_COST[i] <- sum(CIdata2007_list[[i]]$COST_Travel_plus_Survey)
  CIdata2009_SURVEY_COST[i] <- sum(CIdata2009_list[[i]]$COST_Travel_plus_Survey)
  CIdata2011_SURVEY_COST[i] <- sum(CIdata2011_list[[i]]$COST_Travel_plus_Survey)
  CIdata2013_SURVEY_COST[i] <- sum(CIdata2013_list[[i]]$COST_Travel_plus_Survey)
  CIdata2015_SURVEY_COST[i] <- sum(CIdata2015_list[[i]]$COST_Travel_plus_Survey)
}
for (i in 1:100){
  FRACTION_SURVEY_COST[i] <- CIdata2001_SURVEY_COST[i]/total_budget # survey cost for 2001 but does not matter which year because equal survey cost in each year
}
y <- FRACTION_SURVEY_COST
plot(y, average_6000_03, type="l", xlim=c(0,1), ylim=c(0,1), pch=1, xlab="Fraction of total budget spent on monitoring", ylab="%SC managed", main="Total budget 6,000 survey minutes- actual monitoring cost")




# total budget 5000 minutes 
total_budget <- 5000

x <- 1:100

plot(x, average_5000_00, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 5,000 survey minutes")
lines(x, average_5000_01, type="l", lty=2)
lines(x, average_5000_02, type="l", lty=3)
lines(x, average_5000_03, type="l", lty=4)

plot(x, average_number_of_sites_5000_00, type="l", xlim=c(0,100), ylim=c(0,900), xlab="% of sites monitored", ylab="Number of sites visited for management", main="Total budget 5,000 survey minutes")
lines(x, average_number_of_sites_5000_01, type="l", lty=2)
lines(x, average_number_of_sites_5000_02, type="l", lty=3)
lines(x, average_number_of_sites_5000_03, type="l", lty=4)

plot(x, average_percentage_of_sites_5000_00, type="l", xlim=c(0,100), ylim=c(0,1), xlab="% of sites monitored", ylab="Percentage of sites visited for management", main="Total budget 5,000 survey minutes")
lines(x, average_percentage_of_sites_5000_01, type="l", lty=2)
lines(x, average_percentage_of_sites_5000_02, type="l", lty=3)
lines(x, average_percentage_of_sites_5000_03, type="l", lty=4)

## PLOT PERFORMANCE VS FRACTION OF TOTAL BUDGET SPENT ON MONITORING (optimum should shift to left when comparing across increasing total budget)
# Declare vector to store fraction of total budget spent on monitoring
FRACTION_SURVEY_COST <- vector()

# plot % SC managed vs fraction of budget spent on monitoring - this needs to be done separately for each (of the 3) monitoring cost scenario
# actual (03), rescaled (02) and intermediate (01) monitoring cost  
# we don't want to plot this for the scenario where monitoring cost is zero (and management budget is constant)

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

for (i in 1:100){
  FRACTION_SURVEY_COST[i] <- CIdata2001_SURVEY_COST[i]/total_budget # survey cost for 2001 but does not matter which year because equal survey cost in each year
}
y <- FRACTION_SURVEY_COST
plot(y, average_5000_01, type="l", xlim=c(0,1), ylim=c(0,1), pch=1, xlab="Fraction of total budget spent on monitoring", ylab="%SC managed", main="Total budget 5,000 survey minutes- intermediate monitoring cost")

# Calculate monitoring cost for each fraction of the island - rescaled monitoring cost
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

for (i in 1:100){
  FRACTION_SURVEY_COST[i] <- CIdata2001_SURVEY_COST[i]/total_budget # survey cost for 2001 but does not matter which year because equal survey cost in each year
}
y <- FRACTION_SURVEY_COST
plot(y, average_5000_02, type="l", xlim=c(0,1), ylim=c(0,1), pch=1, xlab="Fraction of total budget spent on monitoring", ylab="%SC managed", main="Total budget 5,000 survey minutes- rescaled monitoring cost")

# Calculate monitoring cost for each fraction of the island - actual monitoring cost
for (i in 1:100){
  CIdata2001_SURVEY_COST[i] <- sum(CIdata2001_list[[i]]$COST_Travel_plus_Survey)
  CIdata2003_SURVEY_COST[i] <- sum(CIdata2003_list[[i]]$COST_Travel_plus_Survey)
  CIdata2005_SURVEY_COST[i] <- sum(CIdata2005_list[[i]]$COST_Travel_plus_Survey)
  CIdata2007_SURVEY_COST[i] <- sum(CIdata2007_list[[i]]$COST_Travel_plus_Survey)
  CIdata2009_SURVEY_COST[i] <- sum(CIdata2009_list[[i]]$COST_Travel_plus_Survey)
  CIdata2011_SURVEY_COST[i] <- sum(CIdata2011_list[[i]]$COST_Travel_plus_Survey)
  CIdata2013_SURVEY_COST[i] <- sum(CIdata2013_list[[i]]$COST_Travel_plus_Survey)
  CIdata2015_SURVEY_COST[i] <- sum(CIdata2015_list[[i]]$COST_Travel_plus_Survey)
}
for (i in 1:100){
  FRACTION_SURVEY_COST[i] <- CIdata2001_SURVEY_COST[i]/total_budget # survey cost for 2001 but does not matter which year because equal survey cost in each year
}
y <- FRACTION_SURVEY_COST
plot(y, average_5000_03, type="l", xlim=c(0,1), ylim=c(0,1), pch=1, xlab="Fraction of total budget spent on monitoring", ylab="%SC managed", main="Total budget 5,000 survey minutes- intermediate monitoring cost")


# total budget 4000 minutes 
total_budget <- 4000

x <- 1:100

plot(x, average_4000_00, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 4,000 survey minutes")
lines(x, average_4000_01, type="l", lty=2)
lines(x, average_4000_02, type="l", lty=3)
lines(x, average_4000_03, type="l", lty=4)

plot(x, average_number_of_sites_4000_00, type="l", xlim=c(0,100), ylim=c(0,900), xlab="% of sites monitored", ylab="Number of sites visited for management", main="Total budget 4,000 survey minutes")
lines(x, average_number_of_sites_4000_01, type="l", lty=2)
lines(x, average_number_of_sites_4000_02, type="l", lty=3)
lines(x, average_number_of_sites_4000_03, type="l", lty=4)

plot(x, average_percentage_of_sites_4000_00, type="l", xlim=c(0,100), ylim=c(0,1), xlab="% of sites monitored", ylab="Percentage of sites visited for management", main="Total budget 4,000 survey minutes")
lines(x, average_percentage_of_sites_4000_01, type="l", lty=2)
lines(x, average_percentage_of_sites_4000_02, type="l", lty=3)
lines(x, average_percentage_of_sites_4000_03, type="l", lty=4)

## PLOT PERFORMANCE VS FRACTION OF TOTAL BUDGET SPENT ON MONITORING (optimum should shift to left when comparing across increasing total budget)
# Declare vector to store fraction of total budget spent on monitoring
FRACTION_SURVEY_COST <- vector()

# plot % SC managed vs fraction of budget spent on monitoring - this needs to be done separately for each (of the 3) monitoring cost scenario
# actual (03), rescaled (02) and intermediate (01) monitoring cost  
# we don't want to plot this for the scenario where monitoring cost is zero (and management budget is constant)

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

for (i in 1:100){
  FRACTION_SURVEY_COST[i] <- CIdata2001_SURVEY_COST[i]/total_budget # survey cost for 2001 but does not matter which year because equal survey cost in each year
}
y <- FRACTION_SURVEY_COST
plot(y, average_4000_01, type="l", xlim=c(0,1), ylim=c(0,1), pch=1, xlab="Fraction of total budget spent on monitoring", ylab="%SC managed", main="Total budget 4,000 survey minutes- intermediate monitoring cost")

# Calculate monitoring cost for each fraction of the island - rescaled monitoring cost
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

for (i in 1:100){
  FRACTION_SURVEY_COST[i] <- CIdata2001_SURVEY_COST[i]/total_budget # survey cost for 2001 but does not matter which year because equal survey cost in each year
}
y <- FRACTION_SURVEY_COST
plot(y, average_4000_02, type="l", xlim=c(0,1), ylim=c(0,1), pch=1, xlab="Fraction of total budget spent on monitoring", ylab="%SC managed", main="Total budget 4,000 survey minutes- rescaled monitoring cost")

# Calculate monitoring cost for each fraction of the island - actual monitoring cost
for (i in 1:100){
  CIdata2001_SURVEY_COST[i] <- sum(CIdata2001_list[[i]]$COST_Travel_plus_Survey)
  CIdata2003_SURVEY_COST[i] <- sum(CIdata2003_list[[i]]$COST_Travel_plus_Survey)
  CIdata2005_SURVEY_COST[i] <- sum(CIdata2005_list[[i]]$COST_Travel_plus_Survey)
  CIdata2007_SURVEY_COST[i] <- sum(CIdata2007_list[[i]]$COST_Travel_plus_Survey)
  CIdata2009_SURVEY_COST[i] <- sum(CIdata2009_list[[i]]$COST_Travel_plus_Survey)
  CIdata2011_SURVEY_COST[i] <- sum(CIdata2011_list[[i]]$COST_Travel_plus_Survey)
  CIdata2013_SURVEY_COST[i] <- sum(CIdata2013_list[[i]]$COST_Travel_plus_Survey)
  CIdata2015_SURVEY_COST[i] <- sum(CIdata2015_list[[i]]$COST_Travel_plus_Survey)
}
for (i in 1:100){
  FRACTION_SURVEY_COST[i] <- CIdata2001_SURVEY_COST[i]/total_budget # survey cost for 2001 but does not matter which year because equal survey cost in each year
}
y <- FRACTION_SURVEY_COST
plot(y, average_4000_03, type="l", xlim=c(0,1), ylim=c(0,1), pch=1, xlab="Fraction of total budget spent on monitoring", ylab="%SC managed", main="Total budget 4,000 survey minutes- actual monitoring cost")


# total budget 3000 minutes 
total_budget <- 3000

x <- 1:100

plot(x, average_3000_00, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 3,000 survey minutes")
lines(x, average_3000_01, type="l", lty=2)
lines(x, average_3000_02, type="l", lty=3)
lines(x, average_3000_03, type="l", lty=4)

plot(x, average_number_of_sites_3000_00, type="l", xlim=c(0,100), ylim=c(0,900), xlab="% of sites monitored", ylab="Number of sites visited for management", main="Total budget 3,000 survey minutes")
lines(x, average_number_of_sites_3000_01, type="l", lty=2)
lines(x, average_number_of_sites_3000_02, type="l", lty=3)
lines(x, average_number_of_sites_3000_03, type="l", lty=4)

plot(x, average_percentage_of_sites_3000_00, type="l", xlim=c(0,100), ylim=c(0,1), xlab="% of sites monitored", ylab="Percentage of sites visited for management", main="Total budget 3,000 survey minutes")
lines(x, average_percentage_of_sites_3000_01, type="l", lty=2)
lines(x, average_percentage_of_sites_3000_02, type="l", lty=3)
lines(x, average_percentage_of_sites_3000_03, type="l", lty=4)

## PLOT PERFORMANCE VS FRACTION OF TOTAL BUDGET SPENT ON MONITORING (optimum should shift to left when comparing across increasing total budget)
# Declare vector to store fraction of total budget spent on monitoring
FRACTION_SURVEY_COST <- vector()

# plot % SC managed vs fraction of budget spent on monitoring - this needs to be done separately for each (of the 3) monitoring cost scenario
# actual (03), rescaled (02) and intermediate (01) monitoring cost  
# we don't want to plot this for the scenario where monitoring cost is zero (and management budget is constant)

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

for (i in 1:100){
  FRACTION_SURVEY_COST[i] <- CIdata2001_SURVEY_COST[i]/total_budget # survey cost for 2001 but does not matter which year because equal survey cost in each year
}
y <- FRACTION_SURVEY_COST
plot(y, average_3000_01, type="l", xlim=c(0,1), ylim=c(0,1), pch=1, xlab="Fraction of total budget spent on monitoring", ylab="%SC managed", main="Total budget 3,000 survey minutes- intermediate monitoring cost")

# Calculate monitoring cost for each fraction of the island - rescaled monitoring cost
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

for (i in 1:100){
  FRACTION_SURVEY_COST[i] <- CIdata2001_SURVEY_COST[i]/total_budget # survey cost for 2001 but does not matter which year because equal survey cost in each year
}
y <- FRACTION_SURVEY_COST
plot(y, average_3000_02, type="l", xlim=c(0,1), ylim=c(0,1), pch=1, xlab="Fraction of total budget spent on monitoring", ylab="%SC managed", main="Total budget 3,000 survey minutes- rescaled monitoring cost")

# Calculate monitoring cost for each fraction of the island - actual monitoring cost
for (i in 1:100){
  CIdata2001_SURVEY_COST[i] <- sum(CIdata2001_list[[i]]$COST_Travel_plus_Survey)
  CIdata2003_SURVEY_COST[i] <- sum(CIdata2003_list[[i]]$COST_Travel_plus_Survey)
  CIdata2005_SURVEY_COST[i] <- sum(CIdata2005_list[[i]]$COST_Travel_plus_Survey)
  CIdata2007_SURVEY_COST[i] <- sum(CIdata2007_list[[i]]$COST_Travel_plus_Survey)
  CIdata2009_SURVEY_COST[i] <- sum(CIdata2009_list[[i]]$COST_Travel_plus_Survey)
  CIdata2011_SURVEY_COST[i] <- sum(CIdata2011_list[[i]]$COST_Travel_plus_Survey)
  CIdata2013_SURVEY_COST[i] <- sum(CIdata2013_list[[i]]$COST_Travel_plus_Survey)
  CIdata2015_SURVEY_COST[i] <- sum(CIdata2015_list[[i]]$COST_Travel_plus_Survey)
}
for (i in 1:100){
  FRACTION_SURVEY_COST[i] <- CIdata2001_SURVEY_COST[i]/total_budget # survey cost for 2001 but does not matter which year because equal survey cost in each year
}
y <- FRACTION_SURVEY_COST
plot(y, average_3000_03, type="l", xlim=c(0,1), ylim=c(0,1), pch=1, xlab="Fraction of total budget spent on monitoring", ylab="%SC managed", main="Total budget 3,000 survey minutes- actual monitoring cost")


# total budget 2000 minutes 
total_budget <- 2000

x <- 1:100

plot(x, average_2000_00, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 2,000 survey minutes")
lines(x, average_2000_01, type="l", lty=2)
lines(x, average_2000_02, type="l", lty=3)
lines(x, average_2000_03, type="l", lty=4)

plot(x, average_number_of_sites_2000_00, type="l", xlim=c(0,100), ylim=c(0,900), xlab="% of sites monitored", ylab="Number of sites visited for management", main="Total budget 2,000 survey minutes")
lines(x, average_number_of_sites_2000_01, type="l", lty=2)
lines(x, average_number_of_sites_2000_02, type="l", lty=3)
lines(x, average_number_of_sites_2000_03, type="l", lty=4)

plot(x, average_percentage_of_sites_2000_00, type="l", xlim=c(0,100), ylim=c(0,1), xlab="% of sites monitored", ylab="Percentage of sites visited for management", main="Total budget 2,000 survey minutes")
lines(x, average_percentage_of_sites_2000_01, type="l", lty=2)
lines(x, average_percentage_of_sites_2000_02, type="l", lty=3)
lines(x, average_percentage_of_sites_2000_03, type="l", lty=4)

## PLOT PERFORMANCE VS FRACTION OF TOTAL BUDGET SPENT ON MONITORING (optimum should shift to left when comparing across increasing total budget)
# Declare vector to store fraction of total budget spent on monitoring
FRACTION_SURVEY_COST <- vector()

# plot % SC managed vs fraction of budget spent on monitoring - this needs to be done separately for each (of the 3) monitoring cost scenario
# actual (03), rescaled (02) and intermediate (01) monitoring cost  
# we don't want to plot this for the scenario where monitoring cost is zero (and management budget is constant)

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

for (i in 1:100){
  FRACTION_SURVEY_COST[i] <- CIdata2001_SURVEY_COST[i]/total_budget # survey cost for 2001 but does not matter which year because equal survey cost in each year
}
y <- FRACTION_SURVEY_COST
plot(y, average_2000_01, type="l", xlim=c(0,1), ylim=c(0,1), pch=1, xlab="Fraction of total budget spent on monitoring", ylab="%SC managed", main="Total budget 2,000 survey minutes- intermediate monitoring cost")

# Calculate monitoring cost for each fraction of the island - rescaled monitoring cost
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

for (i in 1:100){
  FRACTION_SURVEY_COST[i] <- CIdata2001_SURVEY_COST[i]/total_budget # survey cost for 2001 but does not matter which year because equal survey cost in each year
}
y <- FRACTION_SURVEY_COST
plot(y, average_2000_02, type="l", xlim=c(0,1), ylim=c(0,1), pch=1, xlab="Fraction of total budget spent on monitoring", ylab="%SC managed", main="Total budget 2,000 survey minutes- rescaled monitoring cost")

# Calculate monitoring cost for each fraction of the island - actual monitoring cost
for (i in 1:100){
  CIdata2001_SURVEY_COST[i] <- sum(CIdata2001_list[[i]]$COST_Travel_plus_Survey)
  CIdata2003_SURVEY_COST[i] <- sum(CIdata2003_list[[i]]$COST_Travel_plus_Survey)
  CIdata2005_SURVEY_COST[i] <- sum(CIdata2005_list[[i]]$COST_Travel_plus_Survey)
  CIdata2007_SURVEY_COST[i] <- sum(CIdata2007_list[[i]]$COST_Travel_plus_Survey)
  CIdata2009_SURVEY_COST[i] <- sum(CIdata2009_list[[i]]$COST_Travel_plus_Survey)
  CIdata2011_SURVEY_COST[i] <- sum(CIdata2011_list[[i]]$COST_Travel_plus_Survey)
  CIdata2013_SURVEY_COST[i] <- sum(CIdata2013_list[[i]]$COST_Travel_plus_Survey)
  CIdata2015_SURVEY_COST[i] <- sum(CIdata2015_list[[i]]$COST_Travel_plus_Survey)
}
for (i in 1:100){
  FRACTION_SURVEY_COST[i] <- CIdata2001_SURVEY_COST[i]/total_budget # survey cost for 2001 but does not matter which year because equal survey cost in each year
}
y <- FRACTION_SURVEY_COST
plot(y, average_2000_03, type="l", xlim=c(0,1), ylim=c(0,1), pch=1, xlab="Fraction of total budget spent on monitoring", ylab="%SC managed", main="Total budget 2,000 survey minutes- actual monitoring cost")


# total budget 1000 minutes 
total_budget <- 1000

x <- 1:100

plot(x, average_1000_00, type="l", xlim=c(0,100), ylim=c(0,1), pch=1, xlab="% of sites monitored", ylab="%SC managed", main="Total budget 1,000 survey minutes")
lines(x, average_1000_01, type="l", lty=2)
lines(x, average_1000_02, type="l", lty=3)
lines(x, average_1000_03, type="l", lty=4)

plot(x, average_number_of_sites_1000_00, type="l", xlim=c(0,100), ylim=c(0,900), xlab="% of sites monitored", ylab="Number of sites visited for management", main="Total budget 1,000 survey minutes")
lines(x, average_number_of_sites_1000_01, type="l", lty=2)
lines(x, average_number_of_sites_1000_02, type="l", lty=3)
lines(x, average_number_of_sites_1000_03, type="l", lty=4)

plot(x, average_percentage_of_sites_1000_00, type="l", xlim=c(0,100), ylim=c(0,1), xlab="% of sites monitored", ylab="Percentage of sites visited for management", main="Total budget 1,000 survey minutes")
lines(x, average_percentage_of_sites_1000_01, type="l", lty=2)
lines(x, average_percentage_of_sites_1000_02, type="l", lty=3)
lines(x, average_percentage_of_sites_1000_03, type="l", lty=4)

## PLOT PERFORMANCE VS FRACTION OF TOTAL BUDGET SPENT ON MONITORING (optimum should shift to left when comparing across increasing total budget)
# Declare vector to store fraction of total budget spent on monitoring
FRACTION_SURVEY_COST <- vector()

# plot % SC managed vs fraction of budget spent on monitoring - this needs to be done separately for each (of the 3) monitoring cost scenario
# actual (03), rescaled (02) and intermediate (01) monitoring cost  
# we don't want to plot this for the scenario where monitoring cost is zero (and management budget is constant)

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

for (i in 1:100){
  FRACTION_SURVEY_COST[i] <- CIdata2001_SURVEY_COST[i]/total_budget # survey cost for 2001 but does not matter which year because equal survey cost in each year
}
y <- FRACTION_SURVEY_COST
plot(y, average_1000_01, type="l", xlim=c(0,1), ylim=c(0,1), pch=1, xlab="Fraction of total budget spent on monitoring", ylab="%SC managed", main="Total budget 1,000 survey minutes- intermediate monitoring cost")

# Calculate monitoring cost for each fraction of the island - rescaled monitoring cost
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

for (i in 1:100){
  FRACTION_SURVEY_COST[i] <- CIdata2001_SURVEY_COST[i]/total_budget # survey cost for 2001 but does not matter which year because equal survey cost in each year
}
y <- FRACTION_SURVEY_COST
plot(y, average_1000_02, type="l", xlim=c(0,1), ylim=c(0,1), pch=1, xlab="Fraction of total budget spent on monitoring", ylab="%SC managed", main="Total budget 1,000 survey minutes- rescaled monitoring cost")

# Calculate monitoring cost for each fraction of the island - actual monitoring cost
for (i in 1:100){
  CIdata2001_SURVEY_COST[i] <- sum(CIdata2001_list[[i]]$COST_Travel_plus_Survey)
  CIdata2003_SURVEY_COST[i] <- sum(CIdata2003_list[[i]]$COST_Travel_plus_Survey)
  CIdata2005_SURVEY_COST[i] <- sum(CIdata2005_list[[i]]$COST_Travel_plus_Survey)
  CIdata2007_SURVEY_COST[i] <- sum(CIdata2007_list[[i]]$COST_Travel_plus_Survey)
  CIdata2009_SURVEY_COST[i] <- sum(CIdata2009_list[[i]]$COST_Travel_plus_Survey)
  CIdata2011_SURVEY_COST[i] <- sum(CIdata2011_list[[i]]$COST_Travel_plus_Survey)
  CIdata2013_SURVEY_COST[i] <- sum(CIdata2013_list[[i]]$COST_Travel_plus_Survey)
  CIdata2015_SURVEY_COST[i] <- sum(CIdata2015_list[[i]]$COST_Travel_plus_Survey)
}
for (i in 1:100){
  FRACTION_SURVEY_COST[i] <- CIdata2001_SURVEY_COST[i]/total_budget # survey cost for 2001 but does not matter which year because equal survey cost in each year
}
y <- FRACTION_SURVEY_COST
plot(y, average_1000_03, type="l", xlim=c(0,1), ylim=c(0,1), pch=1, xlab="Fraction of total budget spent on monitoring", ylab="%SC managed", main="Total budget 1,000 survey minutes- actual monitoring cost")






###### All plots on one page 
