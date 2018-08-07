## Code to calculate the probability of improvement for different management AND data scenarios
####################################################################################################################
## The different data scenarios are created by increasing the # of years.
## This script uses the previously calculated minocc estimates for different run_names (sim_nr and data_scen).
## To get an idea of optimal allocation of resources between monitoring and management, we introduced a new variable 'probability of improvement'. 
## Probability of improvement = proportion of simulations for which the relative measure of metapopulation viability 
## (change_in_minocc) is positive = proportions of simulations for which the minocc_wl exceeds minocc_cc.
## The relative measure of metapopulation viability or extinction risk (change_in_minocc) was derived by subtracting the
## estimates of minocc for the current conditions (cc) from the corresponding estimates of minocc for each wetland 
## creation scenario (0w-20w). 
## So: 'change in minocc' = 'minocc_scenario - minocc_cc' and the 
## probability of improvement = ratio of positive values of change_in_minocc to total number of change_in_minocc values 
## (= total # of minocc estimates = total # of parameter estimates). 
## Probability of improvement can be calculated for different wetland creation scenarios (probability of improvement increases with 
## increased number of wetlands created = a TRUE improvement) and for different data scenarios (probability of improvement changes with increased
## data collection = an increase in CERTAINTY about the improvement). 
####################################################################################################################

## Global variables (some of them are already defined in simulation script), necessary to run this script
  number_of_simulations <- 20 # number of simulations
  nyears <- c(2,4,6,20,50) # number of years
  #nclusters <- 14  # number of clusters
  nwetlands <- 20 # max number of wetlands created

## Define management and data scenarios
  management.scenarios <- NULL # declare vector to store names of different management scenarios
  for (i in 0:nwetlands){management.scenarios <- c(management.scenarios,paste(i,"w",sep=""))} # create vector with names of different management scenarios
  data.scenarios_yr <- NULL # declare vector to store the different data scenarios
  for (i in nyears){data.scenarios_yr <- c(data.scenarios_yr,paste(i,"yr",sep=""))}# create vector with the different data scenarios (start with emtpty vector = NULL)
  #data.scenarios_cl <- NULL # declare vector to store the different data scenarios
  #for (i in 1:nclusters) {data.scenarios_cl <- c(data.scenarios_cl,paste(i,"cl",sep=""))} # create vector with the different data scenarios (start with emtpty vector = NULL)

## Probability of improvement matrix: calculate probability of improvement for each wetland scenario per data.scenario and simulation (= run_name)
# INCREASING # YEARS
col.pal <- c("#707883","#007bc4","#e65714","#369a3d","#f4e0c8","#cde7f6",
            "#42b4ac","#bf2e7b","#03d1de","#7d63a9","#b0001e","#002455","#d39815","#a9d1e7",
            "#38175a","#00727c","#ec83ad","#63001e","#006a35","#586695","#ffbd00","#8ab28d",
            "#ffd16e","#87a2b0","#c8c866","#b2a9e7","#629c33","#815e4a","#c8f57d","#c88dac",
            "#895227","#1b75c4","#ccc8a4","#424252","#fff200")
allmatrices_yr <- list()
#filename <- ("Primp_yr_plot.pdf")
#pdf(filename)
par(mfrow=c(2,3))

for (sim.nr in 1:number_of_simulations) {
  Primp_yr <- Positive_fraction <- matrix(data = NA, nrow = length(data.scenarios_yr), ncol = length(management.scenarios)) # declare matrix to store probability of improvement for different data and wetland creation scenarios
  colnames(Primp_yr) <- management.scenarios
  colnames(Positive_fraction) <- management.scenarios
  rownames(Primp_yr) <- data.scenarios_yr
  rownames(Positive_fraction) <- data.scenarios_yr
  for (col in 1:length(management.scenarios)) {
    scen <- management.scenarios[col]
    for (row in 1:length(data.scenarios_yr)) {
      ds <- data.scenarios_yr[row]
      run_name <- paste("sim",sim.nr,"_",ds,".Rda",sep="")
      minocc <- read.csv(paste("../04_predictions/",run_name,".minocc_predictions.",scen,".csv",sep=""), header = TRUE) [,1] # reads in minocc estimates for the different run_names and wetland scenarios
      minocc.cc <- read.csv(paste("../04_predictions/",run_name,".minocc_predictions.cc.csv",sep=""), header = TRUE) [,1] # reads in minocc estimates for the different data scenarios for current conditions (no riparian reserve - 30 wetlands)
      change_in_minocc <- data.frame(minocc - minocc.cc) # dataframe with the relative measure of metapopulation viability or extinction risk (change_in_minocc), derived by subtracting the estimates of minocc for each parameter combination of cc from the corresponding estimates of minocc for each wetland creation scenario (0w, 1w, 2w, 3w, 4w, 5w, 6w, 7w, 8w). 
      npositives <- nrow(subset(change_in_minocc, change_in_minocc > 0)) # counts the number of positive values in change_in_minocc dataframe
      nzeros <- nrow(subset(change_in_minocc, change_in_minocc == 0)) # counts the number of zeros in change_in_minocc dataframe 
      prob.improvement <- (npositives + (0.5 * nzeros)) / nrow(change_in_minocc) # calculates prob of improvement
      Positive_fraction[row,col] <- npositives/nrow(change_in_minocc)
      Primp_yr[row,col] <- prob.improvement
    }
    cat("Positive fraction for sim",sim.nr,"\n")
    print(Positive_fraction)
  }
  # store matrix for future reference
  allmatrices_yr[[sim.nr]] <- Primp_yr # add Primp_yr to the list
  # now plot primp vs data scenario separately for each simulation
  plot(0,0, xlim=c(1,length(nyears)), ylim=c(0,1), xlab="Years of data", ylab="Probability of improvement")
  for (column in 1:length(management.scenarios)) {
    xvals <- NULL
    yvals <- NULL
    for (row in 1:length(data.scenarios_yr)) {
      ds <- data.scenarios_yr[row]
      xvals <- c(xvals,row)
      yvals <- c(yvals,Primp_yr[row,column])
    }
    lines (xvals,yvals,col=col.pal[column])
  }
  title(main=paste("sim",sim.nr,sep=""))
}
#plot(0,0,xlim=c(0,1),ylim=c(0,1)) # create separate plot for legend
#legend("topright",c("8 w","7 w","6 w","5 w","4 w","3 w","2 w","1 w","0 w"),text.col=col.pal[9:1])
#dev.off()





## Code for calculating the total cost (monitoring + management) to achieve different probabilities of improvement
####################################################################################################################
## This analysis can be done the other way around by calculating how we can achieve the highest probability of 
## improvement for the lowest cost. This gives us the 'best' (cheapest) strategy (= how much monitoring
## should we do) for different budgets (e.g. for low budget we achieve highest prob of imp by doing this amount of monitoring, 
## while for high budget it is 'best' (= we achieve the highest prob of imp) by monitoring that much).
## This code provides us a plot with probability of improvement on the X-axis and total cost (AU$) on the Y-axis. 
####################################################################################################################

## Global variables
max_monitoring <- max(nyears)
nsites <- 167
nsurveys_per_year <- 5
cost_site_surv <- 193 # cost per survey per site = AUD 193 
total_monitoring_cost <- cost_site_surv * nsites * nsurveys_per_year * max_monitoring
cost_wetland_creation <- 350000 # cost per newly created wetland 

## Cost matrix
# INCREASING # YEARS
all_cost_matrices_yr <- list()
#filename <- 'costplot_yrs.pdf'
#pdf(filename)
par(mfrow=c(2,3))
col.pal <- c("#707883","#007bc4","#e65714","#369a3d","#f4e0c8","#cde7f6",
            "#42b4ac","#bf2e7b","#03d1de","#7d63a9","#b0001e","#002455","#d39815","#a9d1e7",
            "#38175a","#00727c","#ec83ad","#63001e","#006a35","#586695","#ffbd00","#8ab28d",
            "#ffd16e","#87a2b0","#c8c866","#b2a9e7","#629c33","#815e4a","#c8f57d","#c88dac",
            "#895227","#1b75c4","#ccc8a4","#424252","#fff200")
pch.pal <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
for (sim.nr in 1:number_of_simulations) {
  cost_matrix_yr <- matrix(data=NA,nrow=length(data.scenarios_yr), ncol=length(management.scenarios)) # declare matrix to store cumulative cost for monitoring per cluster 
  for (nw in 0:nwetlands) { # loops through wetland scenarios
    wetland_cost <- cost_wetland_creation * nw # calculates cumulative cost per wetland
    for (yrs in 1:length(nyears)) { # loops through years      
      monitoring_cost_year <- cost_site_surv * nsites * nsurveys_per_year # calculate cumulative cost for monitoring per year
      cost_matrix_yr[yrs,nw+1] <- monitoring_cost_year * nyears[yrs] + wetland_cost # fills matrix with total cost for monitoring and management
    }
  }
  
  colnames(cost_matrix_yr) <- management.scenarios
  rownames(cost_matrix_yr) <- data.scenarios_yr
  # store for future reference
  all_cost_matrices_yr[[sim.nr]] <- cost_matrix_yr # save cost_matrix_yr in global variabele all_cost_matrices_yr[[sim.nr]]
  # use the correct Primp matrix
  Primp_yr <- allmatrices_yr[[sim.nr]] # get Primp_yr from global variabele allmatrices_yr
  # now plot primp vs total cost of monitoring and management (or the other way around)
  plot(0,0,xlim=c(0,1),ylim=c(0,max(cost_matrix_yr)),xlab="Probability of improvement",ylab="Total cost AUD",main=paste("sim",sim.nr)) # declare plot
  for (i in 1:length(nyears)){
    points(Primp_yr[i,],cost_matrix_yr[i,], col=col.pal[i], pch=pch.pal[i]) # loops through years (rows) to create plot
    } 
  }

#plot(0,0,type='n',xlim=c(0,1),ylim=c(0,1)) # create separate plot for legend
#legend('topright',c("1 yr","2 yr","3 yr","4 yr","5 yr","6 yr"),col=col.pal[1:6],text.col=col.pal[1:6],pch=pch.pal[1:6])
#dev.off()

## Calculate mean primp over all simulations
primp_array <- array() # declare array to store primp matrices for each simulation
matrix_vector <- vector() # declare vector to store all primp matrices as a vector (to feed into array)
for (m in 1:number_of_simulations){ 
  allmatrices_yr[[m]]
  matrix_vector <- c(matrix_vector,allmatrices_yr[[m]])
}
primp_array <- array(matrix_vector,dim=c(length(nyears),nwetlands+1,number_of_simulations)) # fill array with primp matrices for all simulations
primp_array # have a look
# calculate mean, UQ and LQ of respective elements of all matrices, creating mean, UQ, LQ matrices with same dimensions
rows <- length(nyears) # 2,4,6,20,50 years
cols <- nwetlands+1 # 9 columns (because 0w is also a wetland creation scenario)
v <- vector()
mean_matrix <- matrix(data=NA,nrow=length(nyears),ncol=nwetlands+1)
UQ_matrix <- matrix(data=NA,nrow=length(nyears),ncol=nwetlands+1)
LQ_matrix <- matrix(data=NA,nrow=length(nyears),ncol=nwetlands+1)
for (i in 1:rows){
  for (j in 1:cols){
    v <- primp_array[i,j,]
    mean_v <- mean(v)
    UQ_v <- quantile(v,0.975)
    LQ_v <- quantile(v,0.025)
    mean_matrix[i,j] <- mean_v
    UQ_matrix[i,j] <- UQ_v
    LQ_matrix[i,j] <- LQ_v
  }
}
mean_matrix
UQ_matrix
LQ_matrix

cost_matrix_yr # get cost matrix previously calculated

# plot the mean primp (averaged over all simulations) vs total cost of monitoring and management - first for 1 to 6 years, then separately for 20 years added to plot
#filename <- 'meanprimp_vs_cost.pdf'
#pdf(filename)
par(mfrow=c(2,3))

col.pal <- c("#707883","#007bc4","#e65714","#369a3d",
             "#42b4ac","#bf2e7b","#03d1de","#7d63a9","#b0001e","#002455","#d39815","#a9d1e7",
             "#38175a","#00727c","#ec83ad","#63001e","#006a35","#586695","#ffbd00","#8ab28d",
             "#ffd16e","#87a2b0","#c8c866","#b2a9e7","#629c33","#815e4a","#c8f57d","#c88dac",
             "#895227","#1b75c4","#ccc8a4","#424252","#fff200")
pch.pal <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
color_palet_for_legend <- c(2,3,4,5)

plot(0,0,type='n',xlim=c(0,1),ylim=c(0,1))# create separate plot for legend
legend('topright',c("4 yr","6 yr","20yr","50yr"),col=col.pal[color_palet_for_legend],text.col=col.pal[color_palet_for_legend],pch=pch.pal[color_palet_for_legend])
plot(0,0,xlim=c(5.5,7.5),ylim=c(0,1),xlab="Total cost monitoring and management (log10 AUD)",ylab="Probability of improvement")
for (i in 2:length(nyears)){
  points((log10(cost_matrix_yr[i,])),mean_matrix[i,], col=col.pal[i], pch=pch.pal[i])
  lines((log10(cost_matrix_yr[i,])),mean_matrix[i,], col=col.pal[i], pch=pch.pal[i])
  #segments(cost_matrix_yr[i,],UQ_matrix[i,],cost_matrix_yr[i,],LQ_matrix[i,])
  #plot(cost_matrix_yr[i,],mean_matrix[i,],xlim=c(0,max(cost_matrix_yr)),ylim=c(0,1),xlab="Total cost monitoring and management AUD",ylab="Probability of improvement",col=col.pal[i], pch=pch.pal[i]) # create separate plot for every monitoring scenario 
  #segments(cost_matrix_yr[i,],UQ_matrix[i,],cost_matrix_yr[i,],LQ_matrix[i,])
} 






##############  THIS SHOULDN'T BE NEEDED IN THIS SCRIPT #####################################

###################################################### this part adds 20 years to costplot ###########################################
nyears <- 20
total_monitoring_cost <- cost_site_surv * nsites * nsurveys_per_year * nyears
total_monitoring_cost
# create cost matrix for 20 years with 1 row and 9 columns
cost_matrix_20yrs <- matrix(data=NA, nrow=1,ncol=(nwetlands+1))
for (j in 0:nwetlands){
  cost_matrix_20yrs[,j+1] <- total_monitoring_cost + j * cost_wetland_creation
}
cost_matrix_20yrs
# primp matrix 20 years with 1 row and 9 columns
allmatrices_yr_20 <- list()
for (sim.nr in 1:number_of_simulations) {
  Primp_yr_20 <- matrix(data = NA, nrow = 1, ncol = nwetlands+1) # declare matrix to store probability of improvement for different data and wetland creation scenarios
  for (col in 1:(nwetlands+1)) {
  	cat("sim.nr =",sim.nr,"; col =",col,"\n")
    scen <- management.scenarios[col]
    run_name <- paste("sim",sim.nr,"_20yr.Rda",sep="")
    minocc <- read.csv(paste("../04_predictions/20sims_1to6yr_20yr_50yr/",run_name,".minocc_predictions.",scen,".csv",sep=""), header = TRUE) [,1] # reads in minocc estimates for the different run_names and wetland scenarios
    minocc.cc <- read.csv(paste("../04_predictions/20sims_1to6yr_20yr_50yr/",run_name,".minocc_predictions.cc.csv",sep=""), header = TRUE) [,1] # reads in minocc estimates for the different data scenarios for current conditions (no riparian reserve - 30 wetlands)
    change_in_minocc <- data.frame(minocc - minocc.cc) # dataframe with the relative measure of metapopulation viability or extinction risk (change_in_minocc), derived by subtracting the estimates of minocc for each parameter combination of cc from the corresponding estimates of minocc for each wetland creation scenario (0w, 1w, 2w, 3w, 4w, 5w, 6w, 7w, 8w). 
    #cat("change in minocc =",change_in_minocc,"\n")
    npositives <- nrow(subset(change_in_minocc, change_in_minocc > 0)) # counts the number of positive values in change_in_minocc dataframe
    nzeros <- nrow(subset(change_in_minocc, change_in_minocc == 0)) # counts the number of zeros in change_in_minocc dataframe 
    prob.improvement <- (npositives + (0.5 * nzeros)) / nrow(change_in_minocc) # calculates prob of improvement
    cat("primp =",prob.improvement,"\n")
    Primp_yr_20[,col] <- prob.improvement
  }  
  # store matrix for future reference
  allmatrices_yr_20[[sim.nr]] <- Primp_yr_20 # add Primp_yr to the list
}  

primp_array_20 <- array() # declare array to store primp matrices for each simulation
matrix_vector <- vector() # declare vector to store all primp matrices as a vector (to feed into array)
for (m in 1:number_of_simulations){ 
  allmatrices_yr_20[[m]]
  matrix_vector <- c(matrix_vector,allmatrices_yr_20[[m]])
}
primp_array_20 <- array(matrix_vector,dim=c(1,nwetlands+1,number_of_simulations))
primp_array_20
# calculate mean, UQ and LQ of respective elements of all matrices, creating mean, UQ, LQ matrices with same dimensions
rows <- 1
cols <- nwetlands+1
v <- vector()
mean_matrix_20yrs <- matrix(data=NA,nrow=1,ncol=nwetlands+1)
UQ_matrix_20yrs <- matrix(data=NA,nrow=1,ncol=nwetlands+1)
LQ_matrix_20yrs <- matrix(data=NA,nrow=1,ncol=nwetlands+1)
for (i in 1:rows){
  for (j in 1:cols){
    v <- primp_array_20[i,j,]
    mean_v <- mean(v)
    UQ_v <- quantile(v,0.975)
    LQ_v <- quantile(v,0.025)
    mean_matrix_20yrs[i,j] <- mean_v
    UQ_matrix_20yrs[i,j] <- UQ_v
    LQ_matrix_20yrs[i,j] <- LQ_v
  }
}
mean_matrix_20yrs
UQ_matrix_20yrs
LQ_matrix_20yrs
cost_matrix_20yrs
# add 20 years plot to existing plot
points((log10(cost_matrix_20yrs[1,])),mean_matrix_20yrs[1,],col=col.pal[7] , pch=pch.pal[7])
lines((log10(cost_matrix_20yrs[1,])),mean_matrix_20yrs[1,],col=col.pal[7] , pch=pch.pal[7]) # add 20 years in same plot
#plot(cost_matrix_20yrs[1,],mean_matrix_20yrs[1,],xlim=c(0,max(cost_matrix_20yrs)),ylim=c(0,1),xlab="Total cost monitoring and management AUD",ylab="Probability of improvement",col=col.pal[7] , pch=pch.pal[7]) # add separate plot for 20 years
#segments(cost_matrix_20yrs[1,],UQ_matrix_20yrs[1,],cost_matrix_20yrs[1,],LQ_matrix_20yrs[1,])
############################################################ Script to add 20 years to costplot ends here ##########################





###################################################### this part adds 50 years to costplot ###########################################
nyears <- 50
total_monitoring_cost <- cost_site_surv * nsites * nsurveys_per_year * nyears
total_monitoring_cost
# create cost matrix for 50 years with 1 row and 9 columns
cost_matrix_50yrs <- matrix(data=NA, nrow=1,ncol=(nwetlands+1))
for (j in 0:nwetlands){
  cost_matrix_50yrs[,j+1] <- total_monitoring_cost + j * cost_wetland_creation
}
cost_matrix_50yrs
# primp matrix 50 years with 1 row and 9 columns
allmatrices_yr_50 <- list()
for (sim.nr in 2:number_of_simulations) {
  Primp_yr_50 <- matrix(data = NA, nrow = 1, ncol = nwetlands+1) # declare matrix to store probability of improvement for different data and wetland creation scenarios
  for (col in 1:(nwetlands+1)) {
    cat("sim.nr =",sim.nr,"; col =",col,"\n")
    scen <- management.scenarios[col]
    run_name <- paste("sim",sim.nr,"_50yr.Rda",sep="")
    minocc <- read.csv(paste("../04_predictions/20sims_1to6yr_20yr_50yr/",run_name,".minocc_predictions.",scen,".csv",sep=""), header = TRUE) [,1] # reads in minocc estimates for the different run_names and wetland scenarios
    minocc.cc <- read.csv(paste("../04_predictions/20sims_1to6yr_20yr_50yr/",run_name,".minocc_predictions.cc.csv",sep=""), header = TRUE) [,1] # reads in minocc estimates for the different data scenarios for current conditions (no riparian reserve - 30 wetlands)
    change_in_minocc <- data.frame(minocc - minocc.cc) # dataframe with the relative measure of metapopulation viability or extinction risk (change_in_minocc), derived by subtracting the estimates of minocc for each parameter combination of cc from the corresponding estimates of minocc for each wetland creation scenario (0w, 1w, 2w, 3w, 4w, 5w, 6w, 7w, 8w). 
    #cat("change in minocc =",change_in_minocc,"\n")
    npositives <- nrow(subset(change_in_minocc, change_in_minocc > 0)) # counts the number of positive values in change_in_minocc dataframe
    nzeros <- nrow(subset(change_in_minocc, change_in_minocc == 0)) # counts the number of zeros in change_in_minocc dataframe 
    prob.improvement <- (npositives + (0.5 * nzeros)) / nrow(change_in_minocc) # calculates prob of improvement
    cat("primp =",prob.improvement,"\n")
    Primp_yr_50[,col] <- prob.improvement
  }  
  # store matrix for future reference
  allmatrices_yr_50[[sim.nr]] <- Primp_yr_50 # add Primp_yr to the list
}  

primp_array_50 <- array() # declare array to store primp matrices for each simulation
matrix_vector <- vector() # declare vector to store all primp matrices as a vector (to feed into array)
for (m in 2:number_of_simulations){ 
  allmatrices_yr_50[[m]]
  matrix_vector <- c(matrix_vector,allmatrices_yr_50[[m]])
}
primp_array_50 <- array(matrix_vector,dim=c(1,nwetlands+1,(number_of_simulations-1)))
primp_array_50
# calculate mean, UQ and LQ of respective elements of all matrices, creating mean, UQ, LQ matrices with same dimensions
rows <- 1
cols <- nwetlands+1
v <- vector()
mean_matrix_50yrs <- matrix(data=NA,nrow=1,ncol=nwetlands+1)
UQ_matrix_50yrs <- matrix(data=NA,nrow=1,ncol=nwetlands+1)
LQ_matrix_50yrs <- matrix(data=NA,nrow=1,ncol=nwetlands+1)
for (i in 1:rows){
  for (j in 1:cols){
    v <- primp_array_50[i,j,]
    mean_v <- mean(v)
    UQ_v <- quantile(v,0.975)
    LQ_v <- quantile(v,0.025)
    mean_matrix_50yrs[i,j] <- mean_v
    UQ_matrix_50yrs[i,j] <- UQ_v
    LQ_matrix_50yrs[i,j] <- LQ_v
  }
}
mean_matrix_50yrs
UQ_matrix_50yrs
LQ_matrix_50yrs
cost_matrix_50yrs
# add 50 years plot to existing plot
points((log10(cost_matrix_50yrs[1,])),mean_matrix_50yrs[1,],col=col.pal[8], pch=pch.pal[8])
lines((log10(cost_matrix_50yrs[1,])),mean_matrix_50yrs[1,],col=col.pal[8], pch=pch.pal[8]) # add 20 years in same plot
#plot(cost_matrix_50yrs[1,],mean_matrix_50yrs[1,],xlim=c(0,max(cost_matrix_50yrs)),ylim=c(0,1),xlab="Total cost monitoring and management AUD",ylab="Probability of improvement",col=col.pal[8] , pch=pch.pal[8]) # add separate plot for 20 years
#segments(cost_matrix_50yrs[1,],UQ_matrix_50yrs[1,],cost_matrix_50yrs[1,],LQ_matrix_50yrs[1,])
############################################################ Script to add 50 years to costplot ends here ##########################
#dev.off()










