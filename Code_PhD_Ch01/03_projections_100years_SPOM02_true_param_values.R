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
alpha.gamma <- -3.5
alpha.epsilon <- 7
beta.gamma1 <- 0.15
beta.epsilon1 <- -1
beta.epsilon2 <- -0.05
ncombinations <- length(alpha.gamma)
       
    ## Effective area of each wetland
    effareasim <- c(5.360669453, 5.871327269, 6.135450711, 5.345168107, 5.119908918, 
                    5.751070282, 5.542636071, 3.091042453, 2.898028875, 5.086644042, 
                    4.724211935, 4.997349522, 2.952680924, 7.367708572, 4.920772999, 
                    4.196033535, 2.663938084, 2.585241998, 1.671152932, 2.4489199, 
                    4.280332699, 6.651571874, 5.563185302, 2.802901033, 5.177057053, 
                    2.813810557, 4.857729272, 8.454253392, 7.491560547, 6.534446616)
    ## Aquatic vegetation cover for each wetland
    aqvegsim <- c(75.000, 78.333, 35.333, 23.333, 56.667, 35.333, 35.333, 23.333, 
                  28.667, 35.333, 35.333, 36.667, 0.000, 0.333, 3.333, 0.333, 0, 0, 
                  0.000, 0.000, 26.667, 0, 5.000, 0, 0, 0, 10, 3.667, 15, 11.667)
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
    metapopmodel <- function(alpha.gamma, alpha.epsilon, beta.gamma1, beta.epsilon1, beta.epsilon2){
      # Preallocate arrays
      occ <- matrix(nrow=YEARS,ncol=nsites)
      # Start the model (parameters at first time-step)
      occ[1, ] <- initocc
      # Dynamics
      for (t in 2:YEARS){
        # Specify logistic model for the probability of colonisation
        logit.gam <- alpha.gamma + beta.gamma1*effareasim
        gamsim <- 1/(1+exp(-logit.gam)) # Inverse logit applied to each element of logit.gam
        # Specify logistic model for the probability of extinction
        logit.eps <- alpha.epsilon + beta.epsilon1*effareasim + beta.epsilon2*aqvegsim 
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
      replicate(ITERS, metapopmodel(alpha.gamma[x], alpha.epsilon[x], beta.gamma1[x], 
                                    beta.epsilon1[x], beta.epsilon2[x]))
    })
    
    #### Calculate summaries and write to file ####
    ## Mean minimum number of patches occupied
    mean_minocc <- colMeans(results)  
  	## Probability of quasi-extinction
  	prqe <- colMeans(results < qe.thr)
    ## Export
    filename = paste("SPOM02_TRUE_PARAM_VALUES_projections.csv",sep="")
    write.csv(data.frame(minocc=results, mean_minocc=mean_minocc, qe=prqe), file=filename, 
          row.names=FALSE)               

