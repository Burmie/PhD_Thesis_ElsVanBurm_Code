### This script simulates a 6th monitoring regime (monitoring 500 sites for 5 years), developed separately from 
### the other monitoring regimes due to large computational demand.

### Here, we also investigate whether spatial monitoring can replace temporal monitoring  
### to parameterise a DYNAMIC OCCUPANCY MODEL, when the model is used to predict metapopulation persistence. 
### We investigated this for 5 different metapopulation dynamics scenarios: 
### SPOM01: metapopulation dynamics are driven by constant probabilities of colonisation and extinction  
### P(occ) was choosen first, P(ext) and P(col) are calculated based on P(occ) = P(col)/[P(col) + P(ext)],
### which is true when the metapopulation has reached equilibrium (only then P(ext) and P(col) are constant).
### For SPOM01: psi1 = 1/(1+exp(-alpha_psi)), psi = gam/gam+eps, gam = 1/(1+exp(-alpha_gam)), eps = 1/(1+exp(-alpha_eps))
### In SPOM02, the probabilities of extinction and colonisation are a function of site dependent variables only (wetlands area and
### vegetation cover). P(ext) is a function of aquatic vegetation cover and wetland area, P(col) is a function of wetland area.
### In order to use realistic regression coefficients for P(col), plot relationship between
### P(col) and wetland area, pick a value for regression coefficient, vary wetland area and check whether P(col) gets a sensible value.
### For SPOM02: psi1 = 1/(1+exp(-alpha_psi)), gam = 1/(1+exp(-alpha_gam-beta_gam2*effarea)),
### eps = 1/(1+exp(-alpha_eps-beta_eps1*effarea-beta_eps2*aqveg))
### In SPOM03 the P(ext) and P(col) are function of site dependent variables (wetland area and vegetation cover) 
### and connectivity (dependent on occupancy status of neighbouring sites and distance).
### For SPOM03: psi1 = 1/(1+exp(-alpha_psi)), gam = 1/(1+exp(-alpha_gam-beta_gam*conn)),
### eps = 1/(1+exp(-alpha_eps-beta_eps1*effarea-beta_eps2*aqveg-beta_eps3*conn)).
### In SPOM04, P(col) and P(ext) depend on spatial variables (wetland characteristics) and temporal variables (weather). 
### Rainfall was added to SPOM03 and a realistic relationship between P(col) and P(ext) and rainfall was plotted.
### For SPOM04: psi1 = 1/(1+exp(-alpha_psi)), gam = 1/(1+exp(-alpha_gam-beta_gam*conn-beta_gam4*rainfall),
### eps = 1/(1+exp(-alpha_eps-beta_eps1*effarea-beta_eps2*aqveg-beta_eps3*conn-beta_eps4*rainfall)).
### In SPOM05, metapopulation dynamics depended on temporal variables only. P(col) and P(ext) were a function of rainfall.
### For SPOM05: psi1 = 1/(1+exp(-alpha_psi)), gam = 1/(1+exp(-alpha_gam-beta_gam4*rainfall),
### eps = 1/(1+exp(-alpha_eps-beta_eps4*rainfall)).
### 
### Parameter values for SPOM01 were choosen so that psi = 0.18 (average occupancy of SPOM02 and SPOM03).
###
### NB: in a simulation study; simulate occupancy status of sites (Z or occ) and detections (D) via
### a randomised process using probability of occupancy (P(occ) or psi) and  
### probability of detection (P(det) or p). P(occ) and P(det) can be constant or can be 
### a function of covariates.
###
### VERY IMPORTANT IN THIS CODE: OCCUPANCY AND DETECTION MATRICES HAVE DIFFERENT INDICES THAN OTHER MATRICES!!!! 
### In this script p is kept constant.
### Investigate the trade-off by using a fixed budget (e.g. AUD 1,000,000) and assume cost per
### site per survey is AUD 200. 
######################################################################################################################
## number of simulations
nsimulations <- 20

## maximal dataset
nsites <- 500
nyears <- 5
nsurveys <-2

## different models for which data are simulated
SPOMS <- c("SPOM01","SPOM02","SPOM03","SPOM04","SPOM05")

## read in the data needed for simulation (covariables)
effarea <- runif(nsites, 1, 11)   # new 'effarea' with nsites elements (max # sites) taken from the original data (in loge square metres)
aqveg <- runif(nsites, 0, 80)     # new 'aqveg' with nsites elements (max # sites) taken from the original data (in % cover)
dist <- distance_matrix_500_sites # !!! run file "00_500_sites_simulation.R" first
rainfall <- runif(nyears,200,600)  

## monitoring scenarios
monitoring_scenarios <- c("scen5")      

## FUNCTION TO SIMULATE MAXIMAL DATASET FOR DIFFERENT SPOMs
simulate_data <- function (model){
  alpha_gam = -3.5; beta_gam2 = 7.5; beta_gam1 = 0; beta_gam3 = 0; alpha_eps = 7; beta_eps1 = -1; beta_eps2 = -0.05; beta_eps3 = 0; beta_eps4 = 0; p = 0.5
  if (model == "SPOM01") 
    {alpha_gam = -1.95; beta_gam2 = 0; beta_gam1 = 0; alpha_eps = -0.1; beta_eps1 = 0; beta_eps2 = 0; beta_eps4 = 0}
      if (model == "SPOM02")
        {beta_gam2 = 0; beta_gam1 = 0.15}
          if (model == "SPOM03")
            {beta_eps3 = -6}
              else if (model == "SPOM04")
                {beta_eps3 = -6; beta_gam3 = 0.001; beta_eps4 = -0.001}
                  else if (model == "SPOM05")
                    {beta_gam2 = 0; beta_gam3 = 0.009; beta_eps1 = 0; beta_eps2 = 0; beta_eps4 = -0.02}
  
  gam <- eps <- psi <- conn <- matrix(NA,nyears,nsites)                 # P(col) and P(ext) and P(occ)
  occ <- matrix(NA,nsites,nyears)                                       # declare occupancy matrix 
  D <- matrix(NA,nsites,nyears*nsurveys)                                # declare detections matrix
  connw <- array(NA,c(nyears,nsites,nsites))                            # connectivity weighting function
  # parameters for calculating connectivity
  r  <- 0.10026                                       # parameters of the distance weighting function
  v  <- 0.719877                                      # parameters of the distance weighting function
  cutdist <- 1000                                     # neighbourhood region (1000m)
    
  # Year 1
  for (s in 1:nsites) {
    psi[1, s] <- 0.2                                                              # Constant P(occ) in first time step          
    occ[s, 1] <- as.numeric(runif(1) < psi[1, s])                                # occupancy state of sites                 
    tmp <- rep(p*occ[s, 1],nsurveys)                                             # P(det) multiplied with occupancy status at each site 
    D[s, (1:nsurveys)] <- as.numeric(runif(nsurveys) < tmp)                      # detections at sampled sites                  
  }
    
  # Following years
  for (t in 2:nyears){  
    for (s in 1:nsites){                                                                # calculate connectivity for each site in year t (conn[s,t])
      for (j in 1:nsites){     
        connw[t,s,j]<- (cutdist >= dist[s, j]) * (occ[j, t-1] * ((r * dist[s, j])^(-v)))   # contribution of wetland j to s
      }    
      conn[t,s]  <- log(sum(connw[t,s, ]) - (occ[s, t-1] * (((r * dist[s, s])^(-v)))) + 1)       # sum and remove 'self connectivity'
    }  
    for (s in 1:nsites) {
      gam[t, s] <- 1/(1+exp(-alpha_gam - beta_gam2 * conn[t, s] - beta_gam1 * effarea[s] - beta_gam3 * rainfall[t]))                         # P(col) based on regression coefficients and covariates
      eps[t, s] <- 1/(1+exp(-alpha_eps - beta_eps1 * effarea[s] - beta_eps2 * aqveg[s] - beta_eps3 * conn[t, s] - beta_eps4 * rainfall[t])) # P(ext) based on regression coefficients and covariates
      psi[t, s] <- occ[s, t-1] * (1 - eps[t, s]) + (1 - occ[s, t-1]) * gam[t, s]                                                            # P(occ) based on P(col) and P(ext)    
      occ[s, t] <- as.numeric(runif(1) < psi[t, s])                                                                                         # occupancy state of sites       
      tmp <- p*occ[s, t]                                                                                                                    # P(det) multiplied with occupancy status at each site 
      for (k in 1:nsurveys){    
        D[s,(t-1)*nsurveys+k] <- as.numeric(runif(1) < tmp)                                                                                 # detection data at sampled sites  
      }
    }  
  }  
    
  year <- sort(c(rep(1:nyears,nsurveys)))
    
  x <- list()
  x$nyears <- nyears
  x$nsites <- nsites
  x$nsurveys <- nsurveys
  x$year <- year
  x$dist <- dist
  x$effarea <- effarea
  x$aqveg <- aqveg
  x$rainfall <- rainfall
  x$occ <- occ
  x$D <- D  
  save(x,file=paste(SPOM,"_sim",sim,".Rda",sep=""))
}

## FUNCTION TO CREATE SUBSETS OF THE DATA
create_subsets <- function(simul_data, monitoring_scenario){
  if (monitoring_scenario == "scen5")
  {nyr=5; nsts=500; nsrvys=2}
  else {
    if (monitoring_scenario == "scen1")
    {nyr=100; nsts=25; nsrvys=2}
    else {
      if (monitoring_scenario == "scen2")
      {nyr=50; nsts=50; nsrvys=2}
      else {
        if (monitoring_scenario == "scen3")
        {nyr=25; nsts=100; nsrvys=2}
        else {
          if(monitoring_scenario == "scen4")
          {nyr=20; nsts=125; nsrvys=2}
        }  
      }
    }
  }
  subset <- simul_data 
  subset$nyears <- nyr
  subset$nsites <- nsts
  subset$nsurveys <- nsrvys
  year <- sort(c(rep(1:nyr,nsrvys)))
  subset$year <- year
  subset$dist <- subset$dist[1:nsts,1:nsts]
  subset$effarea <- simul_data$effarea[1:nsts]
  subset$aqveg <- simul_data$aqveg[1:nsts]
  subset$rainfall <- simul_data$rainfall[1:nyr]
  subset$occ <- simul_data$occ[1:nsts,1:nyr]
  subset$D <- simul_data$D[1:nsts,1:(nyr*nsrvys)]
  return(subset)
}

## HERE'S WHERE THE ACTION STARTS 
for (sim in 1:nsimulations){
  cat("now starting simulation",sim,"\n")
  # simulate maximal dataset for the different SPOMS
    for (SPOM in SPOMS){
      cat("now running",SPOM,"for maximal dataset\n")
      simulate_data(model = SPOM)
    }
  # subsetting data for different monitoring scenarios (trade-offs) + save each subset 
    for(SPOM in SPOMS){
      for (scen in monitoring_scenarios){
        cat("now running for",SPOM,"scenario_",scen,"\n")
      load(file=paste(SPOM,"_sim",sim,".Rda",sep="")) 
      y <- create_subsets(simul_data = x, monitoring_scenario = scen)
      save(y,file=paste(SPOM,"_",scen,"_sim",sim,".Rda",sep=""))
      }
    }
}