## Code to simulate data for a multi-season (dynamic) occupancy model  
#################################################################################################################
## This code is based on Heard et al. (2013), which modeled the metapopulation dynamics of the growling grass frog. 
## Modeling: covariates and data (= observations or detections) are given and we estimate the parameters of a model.
## Simulating data: covariates and parameter estimates are given and we simulate the data (= observations or detections).
## In this study we simulated observation data (detections) with similar set up as Heard et al. (2013). The only difference 
## is the number of sampled sites (which is here always 167, Geoff had many NA's in his dataset) and surveys 
## per year. In this simulation we are doing 5 surveys at each of the 167 sites per year. 

## How does it work?
# Load data into R with 'source' command. Fix data structure because source command fills data matrices by column and this is 
# different from how they are stored in datafile (compare R matrix with Excel matrix).
# The code is written as modules (functions) to keep an overview. Every 'program' is written as a function and the entire code consists of local and global variables. 

## Process model: 
# covariates (equal to Heard et al.2013): effarea, aqveg, conn (connectivity will be recalculated for 
# each year based on occupancy (Z) in previous year) => to calculate psi (occ prob), gamma (col prob), epsilon (ext prob)
# alpha_psi, beta_psi1, beta_psi2, beta_psi3 = regression coefficients of psi_1 (inital occupancy probability)
# alpha_gam, beta_gam = regression coefficients of gam_t
# alpha_eps, beta_eps1, beta_eps2, beta_eps3 = regression coefficients of eps_t
# calculate psi (occ prob) at each site for each year based on gamma and epsilon
# calculate gamma at each site for each year 
# calculate epsilon at each site for each year 

## Detection model (to get a more accurate estimate of gamma (col.prob) and epsilon (ext.prob) because we want 
## to account for imperfect detection):
# covariates: effort,date,night (we provide values for each survey) => to calculate p
# alpha_p, beta_p1, beta_p2, beta_p3 = regression coefficients of p
# calculate p at each site for each survey 

## True state:
# calculate Z at each site for each year based on psi (runif(psi))

## Simulated observations:
# calculate D (detection history) at each site for each survey [based on p*Z]

## load the data. The data are stored as a list in WinBUGS format. When they are read into R with the 'source' command, 
## the matrix will be filled by column and this does not correspond with the true data file. The data structure 
## needs to be fixed when read into R with 'source' command.

d <- source("modeldata.txt")$value # reads datafile into a variable called "d" 
d$occ <- matrix(d$occ,nrow=nrow(d$occ),ncol=ncol(d$occ),byrow=T)  		# fix data structure (matrix is filled BY COL during 'sourcing' and this should be BY ROW to correspond to the original dataset)
d$Y <- matrix(d$Y,nrow=nrow(d$Y),ncol=ncol(d$Y),byrow=T)  						# fix data structure
d$effort <- matrix(d$effort,nrow=nrow(d$effort),ncol=ncol(d$effort),byrow=T)	# fix data structure
d$date <- matrix(d$date,nrow=nrow(d$date),ncol=ncol(d$date),byrow=T)  			# fix data structure
d$night <- matrix(d$night,nrow=nrow(d$night),ncol=ncol(d$night),byrow=T)		# fix data structure
str(d) # check data structure: d is a LIST

## Global variables
K <- 5   # nsurveys = number of visits (surveys) per site per year: this is what we have changed from Heard et al. 2013
number_of_simulations <- 1 # number of simulations
nyears <- d$nyears # number of years

## FUNCTION FOR DATA SIMULATION
simulate_data <- function (orig_data,nsurv_per_year,number_of_years) {  # takes original dataset (= d, global var), number of surveys per year (= K, global var) and number of years (= nyears, global var) to perform simulation 
  ## local variables (= used only in this function) 
    # components of the data list
    S <- orig_data$nsites  
    dist <- orig_data$dist
    effarea <- as.matrix(orig_data$effarea)
    aqveg <- as.matrix(orig_data$aqveg)
    effort <- matrix(sample(orig_data$effort, orig_data$nsites*nsurv_per_year*number_of_years, replace=T), nrow=orig_data$nsites, ncol=nsurv_per_year*number_of_years) 
    date <- matrix(sample(orig_data$date, orig_data$nsites*nsurv_per_year*number_of_years, replace=T), nrow=orig_data$nsites, ncol=nsurv_per_year*number_of_years)
    night <- matrix(sample(orig_data$night, orig_data$nsites*nsurv_per_year*number_of_years, replace=T), nrow=orig_data$nsites, ncol=nsurv_per_year*number_of_years)
    # parameters for calculating connectivity
    r  <- 0.10026       # parameters of the distance weighting function
    v  <- 0.719877      # parameters of the distance weighting function
    cutdist <- 1000     # neighbourhood region (1000m)
    
  ## simulation parameter values (values obtained by using Geoff's Bayesian inference)
  alpha_psi<- -8.5; beta_psi1<- 0.7; beta_psi2<- 0.07  # regression coefficients for psi1 
  alpha_p<- -0.6; beta_p1<- 1.3; beta_p2<- -0.3; beta_p3<- 1.8  # regression coefficients for p
  alpha_gam <- -3.5; beta_gam <- 7.5 # regression coefficients for gamma
  alpha_eps <- 7; beta_eps1<- -1; beta_eps2 <- -0.05; beta_eps3 <- -5 # regression coefficients for epsilon
  ## simulation parameter values for a constant model 
  #alpha_psi <- 0.2; beta_psi1<- 0; beta_psi2<- 0  # regression coefficients for psi1 
  #alpha_p <- 1.0; beta_p1<- 0; beta_p2<- 0; beta_p3<- 0  # regression coefficients for p
  #alpha_gam <- -0.5; beta_gam <- 0 # regression coefficients for gamma
  #alpha_eps <- 0.5; beta_eps1<- 0; beta_eps2 <- 0; beta_eps3 <- 0 # regression coefficients for epsilon
  
  ## declare matrices
  connw <- array(NA,c(number_of_years,S,S))
  conn <- psi <- Z <- matrix(NA,number_of_years,S) 
  Z_t <- matrix(NA,S,number_of_years)
  p <- D <- matrix(NA,nsurv_per_year*number_of_years,S) 
  p_t <- D_t <- matrix(NA,S,nsurv_per_year*number_of_years)
  gam <- eps <- psi <- matrix(NA,number_of_years,S)
  
  ## YEAR 1
  # calculate occupancy probability in year 1 (psi_1) at each site as a function of effarea and aqveg only 
  # without using connectivity as a covariate (because connectivity is
  # based on occupancy of neighbours in previous time-step and if we want to include this we need to set arbitrary
  # values for this occupancy - will be too complicated)   
  for (s in 1:S) {
    psi[1,s] <- 1/(1+exp(-alpha_psi-beta_psi1*effarea[s,]-beta_psi2*aqveg[s,])) # psi1 based on regression coefficients and covariates         
    # occupancy state of sites   
    Z[1,s] <- as.numeric(runif(1)<psi[1,s])
    # probability of detection at each site for each survey  
    for (k in 1:nsurv_per_year){
      p[k,s] <- 1/(1+exp(-alpha_p-beta_p1*((effort[s,k]-64)/83)-beta_p2*((date[s,k]-132)/45)-beta_p3*night[s,k])) # det prob at each site for each survey                     
      tmp <- p[k,s]*Z[1,s] # det prob multiplied by occ status at each site for each survey
      # detections     
      D[k,s] <- as.numeric(runif(1)<tmp) # detection data at sampled sites                  
    }
  }
  Z_t[,1] <- t(Z)[,1] # transpose matrix so it has the right dimensions to use in the model 
  D_t[,1:nsurv_per_year] <- t(D)[,1:nsurv_per_year] # transpose matrix so it has the right dimensions to use in the model 
  p_t[,1:nsurv_per_year] <- t(p)[,1:nsurv_per_year] # transpose matrix so it has the right dimensions to use in the model 
  
  ## REMAINING YEARS
  for (t in 2:number_of_years){
    # calculate connectivity for each site in year t (conn[t, s])
    for (s in 1:S){          
      for (j in 1:S){
        # contribution of wetland j to s. 'occ' and 'dist' are data. 
        # As above, simply change 'occ[j, 2]' to 'o[j, 2]' for stochastic realistions of connnectivity
        connw[t, s, j] <- (cutdist >= dist[s,j]) * (Z[t-1,j] * ((r * dist[s, j])^(-v))) 
      }
      # sum and remove 'self connectivity', as above
      conn[t, s]  <- log(sum(connw[t, s, ]) - (Z[t-1,s] * (((r * dist[s, s])^(-v)))) + 1)
    }  
    # calculate occupancy probability in year t (psit)  
    for (s in 1:S) {
      gam[t,s] <- 1/(1+exp(-alpha_gam-beta_gam*conn[t,s])) # gamma based on regression coefficients and covariates
      eps[t,s] <- 1/(1+exp(-alpha_eps-beta_eps1*effarea[s,]-beta_eps2*aqveg[s,]-beta_eps3*conn[t,s])) # epsilon based on regression coefficients and covariates
      psi[t,s] <- Z[t-1,s]*(1-eps[t,s]) + (1 - Z[t-1,s]) * gam[t,s] # psi based on eps and gam
      # occupancy state of sites  
      Z[t,s] <- as.numeric(runif(1)<psi[t,s])
      # probability of detection at each site for each survey  
      for (k in 1:nsurv_per_year){
        p[(t-1)*nsurv_per_year+k,s] <- 1/(1+exp(-alpha_p-beta_p1*((effort[s,k]-64)/83)-beta_p2*((date[s,k]-132)/45)-beta_p3*night[s,k])) # det prob at each site for each survey                     
        tmp <- p[(t-1)*nsurv_per_year+k,s]*Z[t,s] # det prob multiplied by occ prob at each site for each survey
        # detections     
        D[(t-1)*nsurv_per_year+k,s] <- as.numeric(runif(1)<tmp) # detection data at sampled sites                  
      }  
    }  
  Z_t[,t] <- t(Z)[,t] # transpose matrix so it has the right dimensions to use in the model
  D_t[,(t-1)*nsurv_per_year+k] <- t(D)[,(t-1)*nsurv_per_year+k] # transpose matrix so it has the right dimensions to use in the model
  p_t[,(t-1)*nsurv_per_year+k] <- t(p)[,(t-1)*nsurv_per_year+k] # transpose matrix so it has the right dimensions to use in the model
  } 
  return(list(occ = Z_t, Y = D_t, effort = effort, date = date, night = night)) # returns a list with 2 components that have changed from Geoff's dataset
}

## FUNCTION FOR CREATING SUBSETS OF DATA
# function for subsets of years
overwrite_data_with_simulated_data = function (orig_data, simul_data, number_of_years, nsurv_per_year) {
  out <- orig_data # make a copy of the original dataset so we don't destroy the original dataset
  out$nyears <- number_of_years # component 'nyears' in original dataset is replaced 
  out$nsurveys <- nsurv_per_year*number_of_years # component 'nsurveys' in original dataset is replaced 
  out$year <- sort(c(rep(1:number_of_years,nsurv_per_year))) # component 'year' is replaced and depends on number_of_years and nsurv_per_year
  out$occ <- as.matrix(simul_data$occ[,1:number_of_years]) # component 'occ' is replaced by the occupancy (Z_t) calculated in the simulate_data function
  out$Y <- simul_data$Y[,1:(number_of_years*nsurv_per_year)] # component 'Y' is replaced by the Y (D_t) calculated in the simulate_data function
  out$effort <- simul_data$effort[,1:(number_of_years*nsurv_per_year)]
  out$date <- simul_data$date[,1:(number_of_years*nsurv_per_year)]
  out$night <- simul_data$night[,1:(number_of_years*nsurv_per_year)]
  # standardize covariates process model
  out$effarea <- as.vector(scale(out$effarea))
  out$aqveg <- as.vector(scale(out$aqveg))
  # calculate connectivity
  conn <- matrix(NA,out$nyears,out$nsites) # declare connectivity matrix 
  connw <- array(NA,c(out$nyears,out$nsites,out$nsites)) # connectivity weighting function 
  # connectivity first timestep 
  for (s in 1:out$nsites){
    for (j in 1:out$nsites){
      # contribution of wetland j to i. 'occ' and 'dist' are data. 
      # Note, to include stochastic realisations of connectivity, simply change 'occ[j, 1]' to 'o[j, 1]'
      connw[1, s, j] <- (cutdist >= out$dist[s, j]) * (out$occ[j, 1] * ((r * out$dist[s, j])^(-v))) 
    }
    # sum the contributions, take away contribution of wetland i to itself, and log transform
    conn[1, s]  <- log(sum(connw[1, s, ]) - (out$occ[s,1] * (((r * out$dist[s, s])^(-v)))) + 1)  
    # connectivity next timesteps     
    for (t in 2:out$nyears){   # calculate connectivity for each site in year t (conn[s,t])
      for (j in 1:out$nsites){     
        connw[t,s,j]<- (cutdist >= out$dist[s, j]) * (out$occ[j, t-1] * ((r * out$dist[s, j])^(-v)))   # contribution of wetland j to s
      }    
      conn[t,s]  <- log(sum(connw[t,s, ]) - (out$occ[s, t-1] * (((r * out$dist[s, s])^(-v)))) + 1)       # sum and remove 'self connectivity'
    }  
  }
  out$kk <- ((conn)-mean(conn, na.rm=TRUE))/sd(conn, na.rm=TRUE) # Standardize connectivity matrix 
  
  return(out)
}

## HERE'S WHERE THE ACTION STARTS
# SUBSETS OF YEARS
for (sim.nr in 1:number_of_simulations){
  # simulate data first
  simul_d = simulate_data(orig_data = d, nsurv_per_year = K, number_of_years = nyears)  
  # then do subsetting for different nyr, saving each subset
  for (nyr in 1:nyears){
    x = overwrite_data_with_simulated_data(orig_data = d, simul_data = simul_d, number_of_years = nyr, nsurv_per_year = K)
    save(x,file=paste("sim",sim.nr,"_",nyr,"yr.Rda",sep=""))
  }
}
