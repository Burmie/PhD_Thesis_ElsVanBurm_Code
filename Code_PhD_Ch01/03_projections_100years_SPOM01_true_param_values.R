## Code for simulating the dynamics of the Donnybrook metapopulation 
###########################################################################################################
## The following R code was used to simulate the dynamics of the Donnybrook
## metapopulation using the 'true' parameter values (used to simulate the data)
## for extinction and colonisation. 
## That way, the accurracy of different monitoring regimes in predicting metapopulation 
## persistence can be evaluated.
## The measure of metapopulation persistence is minimum occupancy (minocc), 
## which is the mean minimum number of occupied wetlands across 100 years and the probability of
## quasi extionction (qe), which is the proportion of 500 simulations in which quasi extinction occurred. 
## The metapopulation had gone quasi-extinct if fewer than 3 wetlands were occupied at any 
## timestep.500 simulations are ran for each set of parameter estimates across 100 years 
## and the minimum number of occupied 
## wetlands across 100 years is recorded. Then this minimum number is averaged over the 
## 500 simulations. This is minocc. 
##########################################################################################################

## True parameter values
alpha.gamma <- -1.95
alpha.epsilon <- 0
ncombinations <- length(alpha.gamma)

  ## Number of sites
  nsites <- 30
  ## Initial occupancy status of each site
  # All occupied at first time-step
  initocc <- rep(1, nsites) 
  
  #### Set some parameters for the simulations ####
  ## No. of iterations
  ITERS <- 500
  ## No. of years
  YEARS <- 100
	## Quasi-extinction threshold (number of occupied wetlands)
	qe.thr <- 3
  
  #### Metapopulation model ####
  ## Each call to the 'metapopmodel' function performs a single iteration of the 
  ## model, and returns the minimum number of occupied patches (across years).
  metapopmodel <- function(alpha.gamma, alpha.epsilon){
    # Preallocate arrays
    occ <- matrix(nrow=YEARS,ncol=nsites)
    # Start the model (parameters at first time-step)
    occ[1, ] <- initocc
    # Dynamics
    for (t in 2:YEARS){
      # Specify logistic model for the probability of colonisation
      logit.gam <- alpha.gamma
      gamsim <- 1/(1+exp(-logit.gam)) # Inverse logit applied to each element of logit.gam
      # Specify logistic model for the probability of extinction
      logit.eps <- alpha.epsilon 
      epssim <- 1/(1+exp(-logit.eps)) # Inverse logit applied to each element of logit.eps
      # Probability of occupancy in each year a function of occupancy status and 
      #  extinction and colonisation probabilities
      poccsim <- occ[t-1, ] * (1-epssim) + (1-occ[t-1, ]) * gamsim
      # Occupancy in each year a Bernoulli variable, with probability poccsim
      occ[t, ] <- rbinom(nsites, 1, poccsim)
    }
    # Minimum number occupied over all timesteps
    minocc <- min(rowSums(occ))
    return(minocc) 
  }
  
  #### Free up RAM ####
  gc()
  
  #### Iterate the simulation ####
  results <- sapply(1:ncombinations, function(x) {
    ## Progress indicator
    cat("   Parameter combination ", x, " of ", ncombinations, "\n")  
    ## Simulate dynamics of the metapopulation for YEARS years, ITERS times, for 
    ## parameter combination x.
    ## x refers to the parameter combination being used in the simulation, and 
    ## ranges from 1 to the number of combinations (see ?sapply)
    replicate(ITERS, metapopmodel(alpha.gamma[x], alpha.epsilon[x]))
  })
  
  #### Calculate summaries and write to file ####
  ## Mean minimum number of patches occupied
  mean_minocc <- colMeans(results)  
	## Probability of quasi-extinction
	prqe <- colMeans(results < qe.thr)
  ## Export
  filename = paste("SPOM01_TRUE_PARAM_VALUES.projections.csv",sep="")
  write.csv(data.frame(minocc=results, mean_minocc=mean_minocc, qe=prqe), file=filename, 
          row.names=FALSE)            
  