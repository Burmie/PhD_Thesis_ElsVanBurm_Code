## Code to fit the model using jags (package R2jags)
## This script will be called from a corresponding perl script that creates a 
## unique run_name (.Rda-file) at the start of this script 
################################################################################

load(run_name) # run_name = .Rda-file

## Build the model (process and observation model)
metapop.model <- function(){
  ## priors
  alpha.psi ~ dnorm(0, 0.000001)                                    
  alpha.gamma ~ dnorm(0, 0.000001)
  beta.gamma3 ~ dnorm(0, 0.01)
  alpha.epsilon ~ dnorm(0, 0.000001)
  beta.epsilon4 ~ dnorm(0, 0.01)
  ## specify parameters for calculating connectivity
  # parameters of the distance weighting function
  r  <- 0.10026
  v  <- 0.719877
  # neighbourhood region (1000m)
  cutdist  <- 1000
  ## Process model
  # begin by calculating connectivity for each site at the first time-step (conn[1, i])
  for (i in 1:nsites){
    for (j in 1:nsites){
      # contribution of wetland j to i. 'occ' and 'dist' are data. 
      # Note, to include stochastic realisations of connectivity, simply change 'occ[j, 1]' to 'o[j, 1]'
      connw[1, i, j] <- step(cutdist - dist[i, j]) * (occ[j, 1] * (pow(r * dist[i, j], -v))) 
      
    }
    # sum the contributions, take away contribution of wetland i to itself, and log transform
    conn[1, i]  <- log(sum(connw[1, i, ]) - (occ[i,1] * (pow(r * dist[i, i], -v))) + 1)  
    # logistic model for the probability of occupancy in the first year (psi)
    logit(psi[i])  <- alpha.psi 
    # actual occupancy in first year a Bernoulli variable, with probability 'psi'
    o[i, 1] ~ dbern(min(.999,max(.001,psi[i])))
    # measure connectivity for each site in the each year after the first (conn[t, i])
    for(t in 2:nyears){
      for (j in 1:nsites){
        # contribution of wetland j to i. As above, simply change 'occ[j, t-1]' to 'o[j, t-1]' for stochastic realistions of connnectivity
        connw[t, i, j] <- step(cutdist - dist[i, j]) * (occ[j, t-1] * (pow(r * dist[i, j], -v)))
      }
      # sum and remove 'self connectivity', as above
      conn[t, i]  <- log(sum(connw[t, i, ]) - (occ[i, t-1] * (pow(r * dist[i, i], -v))) + 1) 
      # logistic model for the probability of colonisation (gamma) at each time step after the first
      logit(gamma[i, t]) <- alpha.gamma + beta.gamma3 * rainfall[t]
      # logistic model for the probability of extinction (epsilon) at each time step after the first
      logit(epsilon[i, t]) <- alpha.epsilon + beta.epsilon4 * rainfall[t]
      # probability of occupancy in each year after the first a function of previous occupancy status and either gamma or epsilon
      O[i, t] <- min(.999, max(.001, o[i, t-1] * (1 - epsilon[i, t]) + (1 - o[i, t-1]) * gamma[i, t]))
      # actual occupancy in each year after the first a Bernoulli variable, with probability 'O[i, t]'
      o[i, t] ~ dbern(O[i, t])
    }
  }
  ## Observation model (with constant P(det))  
  for(ii in 1:nsites){
    for(jj in 1:(nsurveys*nyears)){
      ## logistic model for the probability of detection (with z-transformation of predictor variables)
      #logit(p[ii, jj]) <- alpha.p + beta.p1 * ((effort[ii, jj] - 64) / 83) + beta.p2 * ((date[ii, jj] - 132) / 45)+ beta.p3 * night[ii, jj]
      ## probability of detection a function of occupancy
      p[ii, jj] <- 0.5
      Py[ii, jj] <- min(.999, max(.001, o[ii, year[jj]]*p[ii, jj]))
      ## actual detection a Bernoulli variable, with probability 'Py[ii, jj]'
      D[ii,jj] ~ dbern(Py[ii, jj])
    }
  }
  ## close model
}

## Fit the model 
## install.packages('R2jags')
library(R2jags)
set.seed(123)
## set initial values for MCMC
inits <- function() {
  o=matrix(1, nrow=y$nsites, ncol=y$nyears)
  D=o[,y$year]
  D[!is.na(y$D)] <- NA
  list(
    alpha.psi=0,                                    
    alpha.gamma=rnorm(1),
    beta.gamma3=rnorm(1),
    alpha.epsilon=rnorm(1),
    beta.epsilon4=rnorm(1),
    alpha.p=rnorm(1),
    o=o,
    D=matrix(D, ncol=(y$nsurveys*y$nyears))
  )
}
## fit model and save 
if(exists("y") == F){y = x}
fit <- jags(data=y, inits=inits, parameters.to.save=c("alpha.psi","alpha.gamma","beta.gamma3","alpha.epsilon","beta.epsilon4"),
            model.file=metapop.model, n.chains=3, n.iter=200000, n.burnin=180000, n.thin=10)
save(fit,file=paste(run_name,"_model.Rda",sep=""))
## show model output
print(fit)
str(fit$BUGSoutput)

## Assign names to the post-burnin samples of parameter's posterior distribution to predict extinction risk of a simulated metapopulation (and save as a csv-file to enable postprocessing e.g. vioplot) 
# These files can also be loaded into the next file to predict extinction risk (minocc) of a simulated metapopulation. 
alpha.psi   <- fit$BUGSoutput$sims.list$alpha.psi
alpha.gamma <- fit$BUGSoutput$sims.list$alpha.gamma
alpha.epsilon <- fit$BUGSoutput$sims.list$alpha.epsilon
beta.epsilon4 <- fit$BUGSoutput$sims.list$beta.epsilon4
beta.gamma3   <- fit$BUGSoutput$sims.list$beta.gamma3

write.csv(alpha.psi, file =paste(run_name,"_alpha.psi.txt",sep=""))
write.csv(alpha.gamma, file =paste(run_name,"_alpha.gamma.txt",sep=""))
write.csv(alpha.epsilon, file =paste(run_name,"_alpha.epsilon.txt",sep=""))
write.csv(beta.epsilon4, file = paste(run_name,"_beta.epsilon4.txt",sep=""))
write.csv(beta.gamma3, file = paste(run_name,"_beta.gamma3.txt",sep=""))

