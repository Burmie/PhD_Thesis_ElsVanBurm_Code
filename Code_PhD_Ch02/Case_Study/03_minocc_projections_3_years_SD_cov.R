run_name <- "3_years"
  
## Code for simulating the dynamics of the Donnybrook metapopulation 
#####################################################################################
## The following R code was used to simulate the dynamics of the Donnybrook
## metapopulation for each of the 6000 estimates of the parameters of the models
## for extinction and colonisation (as derived using the code in 'metapopmodel_jags').
## This code simulates the dynamics of the metapopulation under current conditions
## and the creation of new wetlands under a 200 m riparian reserve. Simulations 
## for the riparian reserve required removing
## those wetlands lost under a 200 m reserve, while wetland creation was simulated
## by simply adding in the details of the new wetlands. The relative measure of
## metapopulation viability or extinction risk (change_in_minocc) was derived by subtracting the
## estimates of minocc for each parameter combination derived using this
## code (cc) from the corresponding estimates of minocc for each wetland creation scenario
## (0w - 20w). 
## In order to run this for different data scenarios (run_names): run the entire script as a 
## function that can be called for different data scenarios (run_names). 
#####################################################################################

# perl script transforms this file into Rda file with run_name on top each script, so it can loop through run_names

# load mean and sd of covariables in training data (data is called "x")
# in order to run projections with standardized covariables (subtracting mean and dividing sd from training data)
load("../02_model_fit/data_3_years.Rda")

mean_effarea <- x$mean_effarea
sd_effarea <- x$sd_effarea
#a <- function(k) (k - mean_effarea)/sd_effarea

mean_aqveg <- x$mean_aqveg
sd_aqveg <- x$sd_aqveg
#b <- function(l) (l - mean_aqveg)/sd_aqveg

mean_conn <- x$mean_conn
sd_conn <- x$sd_conn 
#c <- function(m) (m - mean_conn)/sd_conn


# define function for simulation of future dynamics
simulate_wetland_scenarios <- function (run_name){ 

  ## Define files containing postburnin parameter values
	ag <- paste("../02_model_fit/",run_name,"_alpha.gamma.txt",sep="")
	ae <- paste("../02_model_fit/",run_name,"_alpha.epsilon.txt",sep="")
	bg <- paste("../02_model_fit/",run_name,"_beta.gamma1.txt",sep="")
	be1 <- paste("../02_model_fit/",run_name,"_beta.epsilon1.txt",sep="")
	be2 <- paste("../02_model_fit/",run_name,"_beta.epsilon2.txt",sep="")
	be3 <- paste("../02_model_fit/",run_name,"_beta.epsilon3.txt",sep="")
  
  ## Load parameter combinations
  alpha.gamma <- read.table(ag, header = F, sep="")[,2]
  alpha.epsilon <- read.table(ae, header = F, sep="")[,2]
  beta.gamma1 <- read.table(bg, header = F, sep="")[,2]
  beta.epsilon1 <- read.table(be1, header = F, sep="")[,2]
  beta.epsilon2 <- read.table(be2, header = F, sep="")[,2]
  beta.epsilon3 <- read.table(be3, header = F, sep="")[,2]
  ncombinations <- length(alpha.gamma)
  
  # reduce length of vector to 1000 to reduce computation time
  params_to_keep = c(1:ncombinations)
  params_to_keep = sort(sample(params_to_keep,size=10))
  alpha.gamma <- alpha.gamma[params_to_keep]
  alpha.epsilon <- alpha.epsilon[params_to_keep]
  beta.gamma1 <- beta.gamma1[params_to_keep]
  beta.epsilon1 <- beta.epsilon1[params_to_keep]
  beta.epsilon2 <- beta.epsilon2[params_to_keep]
  beta.epsilon3 <- beta.epsilon3[params_to_keep]
  ncombinations <- length(alpha.gamma)
  cat("   running",ncombinations,"parameter combinations\n")
  



  
  # loop through all wetland creation conditions for this run name
  names.conditions = c("cc","0w","1w","2w","3w","4w","5w","6w","7w","8w","9w","10w","11w","12w","13w","14w","15w","16w","17w","18w","19w","20w")
  for (condition in names.conditions) {
    
    cat("  now starting condition",condition,"\n")
       
    ########################################################################
    ####################### CURRENT CONDITIONS #############################
    ########################################################################
      ## Effective area of each wetland
      effareasim <- (c(5.360669453, 5.871327269, 6.135450711, 5.345168107, 5.119908918, 
                      5.751070282, 5.542636071, 3.091042453, 2.898028875, 5.086644042, 
                      4.724211935, 4.997349522, 2.952680924, 7.367708572, 4.920772999, 
                      4.196033535, 2.663938084, 2.585241998, 1.671152932, 2.4489199, 
                      4.280332699, 6.651571874, 5.563185302, 2.802901033, 5.177057053, 
                      2.813810557, 4.857729272, 8.454253392, 7.491560547, 6.534446616) - 5.6)/2.1
      #effareasim_cc <- sapply(effareasim, a)
      ## Aquatic vegetation cover for each wetland
      aqvegsim <- (c(75.000, 78.333, 35.333, 23.333, 56.667, 35.333, 35.333, 23.333, 
                    28.667, 35.333, 35.333, 36.667, 0.000, 0.333, 3.333, 0.333, 0, 0, 
                    0.000, 0.000, 26.667, 0, 5.000, 0, 0, 0, 10, 3.667, 15, 11.667)- 22)/19.4
      #aqvegsim_cc <- sapply(aqvegsim, b)
      ## X and Y coordinates of wetland centres
      coords <- structure(list(x = c(319745L, 319825L, 320076L, 318829L, 318914L, 
                                     320298L, 320449L, 318824L, 319181L, 320629L, 
                                     320871L, 319139L, 319739L, 319807L, 318679L, 
                                     319367L, 319379L, 319412L, 319555L, 320002L, 
                                     318798L, 318815L, 320117L, 318640L, 318815L, 
                                     319937L, 320073L, 320748L, 321259L, 318368L),
                               y = c(5843011L, 5843192L, 5843333L, 5843122L, 5843400L, 
                                     5843617L, 5843799L, 5842962L, 5842400L, 5843883L, 
                                     5843812L, 5842892L, 5843382L, 5843041L, 5842887L, 
                                     5841926L, 5842372L, 5842609L, 5843220L, 5842925L, 
                                     5842397L, 5842281L, 5842962L, 5842560L, 5842048L, 
                                     5842555L, 5842668L, 5843374L, 5843767L, 5842686L)), 
                          .Names = c("x", "y"),
                          class = "data.frame", row.names = c(NA, -30L))
      ## Number of sites
      nsites <- nrow(coords)
      ## Initial occupancy status of each site
      # All occupied at first time-step
      initocc <- rep(1, nsites) 
    ###############################################################################
    ####################### 200M BUFFER + 0 WETLANDS ##############################
    ###############################################################################
    if (condition == "0w") {
      ##effective area of each wetland
      effareasim <- (c(5.360669453,5.871327269,6.135450711,5.345168107,5.119908918,
                      5.751070282,5.542636071,3.091042453,2.898028875,5.086644042,
                      4.724211935,4.997349522,2.952680924,7.367708572,4.920772999,
                      4.196033535,2.663938084,2.585241998) - 5.6)/2.1
      #effareasim <- sapply(effareasim, a)
      ##aquatic vegetation cover for each wetland
      aqvegsim <- (c(75.000,78.333,35.333,23.333,56.667,35.333,35.333,23.333,28.667,35.333,35.333,36.667,0.000,0.333,3.333,0.333,0,0)- 22)/19.4
      #aqvegsim <- sapply(aqvegsim, b)
      ##x and y coordinates of site centres
      coords <- structure(list(x = c(319745L,319825L,320076L,318829L,318914L,320298L,320449L,318824L,319181L,320629L,320871L,319139L,319739L,319807L,318679L,319367L,319379L,319412L),
                               y = c(5843011L,5843192L,5843333L,5843122L,5843400L,5843617L,5843799L,5842962L,5842400L,5843883L,5843812L,5842892L,5843382L,5843041L,5842887L,5841926L,5842372L,5842609L)), 
                          .Names = c("x", "y"), class = "data.frame", row.names = c(NA, -18L))
      ##number of sites
      nsites <- nrow(coords)
      ##initial occupancy status of each site
      initocc <- rep(1, nsites) #all occupied at first timestep
    }
    ###############################################################################
    ####################### 200M BUFFER + 1 WETLANDS ##############################
    ###############################################################################
    if (condition == "1w") {
      ##effective area of each wetland
      effareasim <- (c(5.360669453,5.871327269,6.135450711,5.345168107,5.119908918,
                      5.751070282,5.542636071,3.091042453,2.898028875,5.086644042,
                      4.724211935,4.997349522,2.952680924,7.367708572,4.920772999,
                      4.196033535,2.663938084,2.585241998,5.348174123)- 5.6)/2.1
      #effareasim <- sapply(effareasim, a)
      ##aquatic vegetation cover for each wetland
      aqvegsim <- (c(75.000,78.333,35.333,23.333,56.667,35.333,35.333,23.333,28.667,35.333,35.333,36.667,0.000,0.333,3.333,0.333,0,0,60)- 22)/19.4
      #aqvegsim <- sapply(aqvegsim, b)
      ##x and y coordinates of site centres
      coords <- structure(list(x = c(319745L,319825L,320076L,318829L,318914L,320298L,320449L,318824L,319181L,320629L,320871L,319139L,319739L,319807L,318679L,319367L,319379L,319412L,319857L),
                               y = c(5843011L,5843192L,5843333L,5843122L,5843400L,5843617L,5843799L,5842962L,5842400L,5843883L,5843812L,5842892L,5843382L,5843041L,5842887L,5841926L,5842372L,5842609L,5843295L)), 
                          .Names = c("x", "y"), class = "data.frame", row.names = c(NA, -19L))
      ##number of sites
      nsites <- nrow(coords)
      ##initial occupancy status of each site
      initocc <- rep(1, nsites) #all occupied at first timestep
    }
    ###############################################################################
    ####################### 200M BUFFER + 2 WETLANDS ##############################
    ###############################################################################
    if (condition == "2w") {
      ##effective area of each wetland
      effareasim <- (c(5.360669453,5.871327269,6.135450711,5.345168107,5.119908918,
                      5.751070282,5.542636071,3.091042453,2.898028875,5.086644042,
                      4.724211935,4.997349522,2.952680924,7.367708572,4.920772999,
                      4.196033535,2.663938084,2.585241998,5.348174123,5.348174123)- 5.6)/2.1
      #effareasim <- sapply(effareasim, a)
      ##aquatic vegetation cover for each wetland
      aqvegsim <- (c(75.000,78.333,35.333,23.333,56.667,35.333,35.333,23.333,28.667,35.333,35.333,36.667,0.000,0.333,3.333,0.333,0,0,60,60)- 22)/19.4
      #aqvegsim <- sapply(aqvegsim, b)
      ##x and y coordinates of site centres
      coords <- structure(list(x = c(319745L,319825L,320076L,318829L,318914L,320298L,320449L,318824L,319181L,320629L,320871L,319139L,319739L,319807L,318679L,319367L,319379L,319412L,319857L,319914L),
                               y = c(5843011L,5843192L,5843333L,5843122L,5843400L,5843617L,5843799L,5842962L,5842400L,5843883L,5843812L,5842892L,5843382L,5843041L,5842887L,5841926L,5842372L,5842609L,5843295L,5843287L)), 
                          .Names = c("x", "y"), class = "data.frame", row.names = c(NA, -20L))
      ##number of sites
      nsites <- nrow(coords)
      ##initial occupancy status of each site
      initocc <- rep(1, nsites) #all occupied at first timestep
    }
    ###############################################################################
    ####################### 200M BUFFER + 3 WETLANDS ##############################
    ###############################################################################
    if (condition == "3w") {
      ##effective area of each wetland
      effareasim <- (c(5.360669453,5.871327269,6.135450711,5.345168107,5.119908918,
                      5.751070282,5.542636071,3.091042453,2.898028875,5.086644042,
                      4.724211935,4.997349522,2.952680924,7.367708572,4.920772999,
                      4.196033535,2.663938084,2.585241998,5.348174123,5.348174123,
                      5.348174123)- 5.6)/2.1
      #effareasim <- sapply(effareasim, a)
      ##aquatic vegetation cover for each wetland
      aqvegsim <- (c(75.000,78.333,35.333,23.333,56.667,35.333,35.333,23.333,28.667,35.333,35.333,36.667,0.000,0.333,3.333,0.333,0,0,60,60,60)- 22)/19.4
      #aqvegsim <- sapply(aqvegsim, b)
      ##x and y coordinates of site centres
      coords <- structure(list(x = c(319745L,319825L,320076L,318829L,318914L,320298L,320449L,318824L,319181L,320629L,320871L,319139L,319739L,319807L,318679L,319367L,319379L,319412L,319857L,319914L,319961L),
                               y = c(5843011L,5843192L,5843333L,5843122L,5843400L,5843617L,5843799L,5842962L,5842400L,5843883L,5843812L,5842892L,5843382L,5843041L,5842887L,5841926L,5842372L,5842609L,5843295L,5843287L,5843252L)), 
                          .Names = c("x", "y"), class = "data.frame", row.names = c(NA, -21L))
      ##number of sites
      nsites <- nrow(coords)
      ##initial occupancy status of each site
      initocc <- rep(1, nsites) #all occupied at first timestep
    }
    ###############################################################################
    ####################### 200M BUFFER + 4 WETLANDS ##############################
    ###############################################################################
    if (condition == "4w") {
      ##effective area of each wetland
      effareasim <- (c(5.360669453,5.871327269,6.135450711,5.345168107,5.119908918,
                      5.751070282,5.542636071,3.091042453,2.898028875,5.086644042,
                      4.724211935,4.997349522,2.952680924,7.367708572,4.920772999,
                      4.196033535,2.663938084,2.585241998,5.348174123,5.348174123,
                      5.348174123,5.348174123)- 5.6)/2.1
      #effareasim <- sapply(effareasim, a)
      ##aquatic vegetation cover for each wetland
      aqvegsim <- (c(75.000,78.333,35.333,23.333,56.667,35.333,35.333,23.333,28.667,35.333,35.333,36.667,0.000,0.333,3.333,0.333,0,0,60,60,60,60)- 22)/19.4
      #aqvegsim <- sapply(aqvegsim, b)
      ##x and y coordinates of site centres
      coords <- structure(list(x = c(319745L,319825L,320076L,318829L,318914L,320298L,320449L,318824L,319181L,320629L,320871L,319139L,319739L,319807L,318679L,319367L,319379L,319412L,319857L,319914L,319961L,319865L),
                               y = c(5843011L,5843192L,5843333L,5843122L,5843400L,5843617L,5843799L,5842962L,5842400L,5843883L,5843812L,5842892L,5843382L,5843041L,5842887L,5841926L,5842372L,5842609L,5843295L,5843287L,5843252L,5843239L)), 
                          .Names = c("x", "y"), class = "data.frame", row.names = c(NA, -22L))
      ##number of sites
      nsites <- nrow(coords)
      ##initial occupancy status of each site
      initocc <- rep(1, nsites) #all occupied at first timestep
    }
    ###############################################################################
    ####################### 200M BUFFER + 5 WETLANDS ##############################
    ###############################################################################
    if (condition == "5w") {
      ##effective area of each wetland
      effareasim <- (c(5.360669453,5.871327269,6.135450711,5.345168107,5.119908918,
                      5.751070282,5.542636071,3.091042453,2.898028875,5.086644042,
                      4.724211935,4.997349522,2.952680924,7.367708572,4.920772999,
                      4.196033535,2.663938084,2.585241998,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123)- 5.6)/2.1
      #effareasim <- sapply(effareasim, a)
      ##aquatic vegetation cover for each wetland
      aqvegsim <- (c(75.000,78.333,35.333,23.333,56.667,35.333,35.333,23.333,28.667,35.333,35.333,36.667,0.000,0.333,3.333,0.333,0,0,60,60,60,60,60)- 22)/19.4
      #aqvegsim <- sapply(aqvegsim, b)
      ##x and y coordinates of site centres
      coords <- structure(list(x = c(319745L,319825L,320076L,318829L,318914L,320298L,320449L,318824L,319181L,320629L,320871L,319139L,319739L,319807L,318679L,319367L,319379L,319412L,319857L,319914L,319961L,319865L,320019L),
                               y = c(5843011L,5843192L,5843333L,5843122L,5843400L,5843617L,5843799L,5842962L,5842400L,5843883L,5843812L,5842892L,5843382L,5843041L,5842887L,5841926L,5842372L,5842609L,5843295L,5843287L,5843252L,5843239L,5843238L)), 
                          .Names = c("x", "y"), class = "data.frame", row.names = c(NA, -23L))
      ##number of sites
      nsites <- nrow(coords)
      ##initial occupancy status of each site
      initocc <- rep(1, nsites) #all occupied at first timestep
    }
    ###############################################################################
    ####################### 200M BUFFER + 6 WETLANDS ##############################
    ###############################################################################
    if (condition == "6w") {
      ##effective area of each wetland
      effareasim <- (c(5.360669453,5.871327269,6.135450711,5.345168107,5.119908918,
                      5.751070282,5.542636071,3.091042453,2.898028875,5.086644042,
                      4.724211935,4.997349522,2.952680924,7.367708572,4.920772999,
                      4.196033535,2.663938084,2.585241998,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123)- 5.6)/2.1
      #effareasim <- sapply(effareasim, a)
      ##aquatic vegetation cover for each wetland
      aqvegsim <- (c(75.000,78.333,35.333,23.333,56.667,35.333,35.333,23.333,28.667,35.333,35.333,36.667,0.000,0.333,3.333,0.333,0,0,60,60,60,60,60,60)- 22)/19.4
      #aqvegsim <- sapply(aqvegsim, b)
      ##x and y coordinates of site centres
      coords <- structure(list(x = c(319745L,319825L,320076L,318829L,318914L,320298L,320449L,318824L,319181L,320629L,320871L,319139L,319739L,319807L,318679L,319367L,319379L,319412L,319857L,319914L,319961L,319865L,320019L,319869L),
                               y = c(5843011L,5843192L,5843333L,5843122L,5843400L,5843617L,5843799L,5842962L,5842400L,5843883L,5843812L,5842892L,5843382L,5843041L,5842887L,5841926L,5842372L,5842609L,5843295L,5843287L,5843252L,5843239L,5843238L,5843183L)), 
                          .Names = c("x", "y"), class = "data.frame", row.names = c(NA, -24L))
      ##number of sites
      nsites <- nrow(coords)
      ##initial occupancy status of each site
      initocc <- rep(1, nsites) #all occupied at first timestep
    }
    ###############################################################################
    ####################### 200M BUFFER + 7 WETLANDS ##############################
    ###############################################################################
    if (condition == "7w") {
      ##effective area of each wetland
      effareasim <- (c(5.360669453,5.871327269,6.135450711,5.345168107,5.119908918,
                      5.751070282,5.542636071,3.091042453,2.898028875,5.086644042,
                      4.724211935,4.997349522,2.952680924,7.367708572,4.920772999,
                      4.196033535,2.663938084,2.585241998,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123)- 5.6)/2.1
      #effareasim <- sapply(effareasim, a)
      ##aquatic vegetation cover for each wetland
      aqvegsim <- (c(75.000,78.333,35.333,23.333,56.667,35.333,35.333,23.333,28.667,35.333,35.333,36.667,0.000,0.333,3.333,0.333,0,0,60,60,60,60,60,60,60)- 22)/19.4
      #aqvegsim <- sapply(aqvegsim, b)
      ##x and y coordinates of site centres
      coords <- structure(list(x = c(319745L,319825L,320076L,318829L,318914L,320298L,320449L,318824L,319181L,320629L,320871L,319139L,319739L,319807L,318679L,319367L,319379L,319412L,319857L,319914L,319961L,319865L,320019L,319869L,320072L),
                               y = c(5843011L,5843192L,5843333L,5843122L,5843400L,5843617L,5843799L,5842962L,5842400L,5843883L,5843812L,5842892L,5843382L,5843041L,5842887L,5841926L,5842372L,5842609L,5843295L,5843287L,5843252L,5843239L,5843238L,5843183L,5843259L)), 
                          .Names = c("x", "y"), class = "data.frame", row.names = c(NA, -25L))
      ##number of sites
      nsites <- nrow(coords)
      ##initial occupancy status of each site
      initocc <- rep(1, nsites) #all occupied at first timestep
    }
    ###############################################################################
    ####################### 200M BUFFER + 8 WETLANDS ##############################
    ###############################################################################
    if (condition == "8w") {
      ##effective area of each wetland
      effareasim <- (c(5.360669453,5.871327269,6.135450711,5.345168107,5.119908918,
                      5.751070282,5.542636071,3.091042453,2.898028875,5.086644042,
                      4.724211935,4.997349522,2.952680924,7.367708572,4.920772999,
                      4.196033535,2.663938084,2.585241998,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123,
                      5.348174123)- 5.6)/2.1
      #effareasim <- sapply(effareasim, a)
      ##aquatic vegetation cover for each wetland
      aqvegsim <- (c(75.000,78.333,35.333,23.333,56.667,35.333,35.333,23.333,28.667,35.333,35.333,36.667,0.000,0.333,3.333,0.333,0,0,60,60,60,60,60,60,60,60)- 22)/19.4
      #aqvegsim <- sapply(aqvegsim, b)
      ##x and y coordinates of site centres
      coords <- structure(list(x = c(319745L,319825L,320076L,318829L,318914L,320298L,320449L,318824L,319181L,320629L,320871L,319139L,319739L,319807L,318679L,319367L,319379L,319412L,319857L,319914L,319961L,319865L,320019L,319869L,320072L,320109L),
                               y = c(5843011L,5843192L,5843333L,5843122L,5843400L,5843617L,5843799L,5842962L,5842400L,5843883L,5843812L,5842892L,5843382L,5843041L,5842887L,5841926L,5842372L,5842609L,5843295L,5843287L,5843252L,5843239L,5843238L,5843183L,5843259L,5843300L)), 
                          .Names = c("x", "y"), class = "data.frame", row.names = c(NA, -26L))
      ##number of sites
      nsites <- nrow(coords)
      ##initial occupancy status of each site
      initocc <- rep(1, nsites) #all occupied at first timestep
    }
    ###############################################################################
    ####################### 200M BUFFER + 9 WETLANDS ##############################
    ###############################################################################
    if (condition == "9w") {
      ##effective area of each wetland
      effareasim <- (c(5.360669453,5.871327269,6.135450711,5.345168107,5.119908918,
                      5.751070282,5.542636071,3.091042453,2.898028875,5.086644042,
                      4.724211935,4.997349522,2.952680924,7.367708572,4.920772999,
                      4.196033535,2.663938084,2.585241998,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123,
                      5.348174123,5.348174123)- 5.6)/2.1
      #effareasim <- sapply(effareasim, a)
      ##aquatic vegetation cover for each wetland
      aqvegsim <- (c(75.000,78.333,35.333,23.333,56.667,35.333,35.333,23.333,28.667,35.333,35.333,36.667,0.000,0.333,3.333,0.333,0,0,60,60,60,60,60,60,60,60,60)- 22)/19.4
      #aqvegsim <- sapply(aqvegsim, b)
      ##x and y coordinates of site centres
      coords <- structure(list(x = c(319745L,319825L,320076L,318829L,318914L,320298L,320449L,318824L,319181L,320629L,320871L,319139L,319739L,319807L,318679L,319367L,319379L,319412L,319857L,319914L,319961L,319865L,320019L,319869L,320072L,320109L,319250L),
                               y = c(5843011L,5843192L,5843333L,5843122L,5843400L,5843617L,5843799L,5842962L,5842400L,5843883L,5843812L,5842892L,5843382L,5843041L,5842887L,5841926L,5842372L,5842609L,5843295L,5843287L,5843252L,5843239L,5843238L,5843183L,5843259L,5843300L,5842800L)), 
                          .Names = c("x", "y"), class = "data.frame", row.names = c(NA, -27L))
      ##number of sites
      nsites <- nrow(coords)
      ##initial occupancy status of each site
      initocc <- rep(1, nsites) #all occupied at first timestep
    }
    ###############################################################################
    ####################### 200M BUFFER + 10 WETLANDS ##############################
    ###############################################################################
    if (condition == "10w") {
      ##effective area of each wetland
      effareasim <- (c(5.360669453,5.871327269,6.135450711,5.345168107,5.119908918,
                      5.751070282,5.542636071,3.091042453,2.898028875,5.086644042,
                      4.724211935,4.997349522,2.952680924,7.367708572,4.920772999,
                      4.196033535,2.663938084,2.585241998,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123)- 5.6)/2.1
      #effareasim <- sapply(effareasim, a)
      ##aquatic vegetation cover for each wetland
      aqvegsim <- (c(75.000,78.333,35.333,23.333,56.667,35.333,35.333,23.333,28.667,35.333,35.333,36.667,
                          0.000,0.333,3.333,0.333,0,0,60,60,60,60,60,60,60,60,60,60)- 22)/19.4
      #aqvegsim <- sapply(aqvegsim, b)
      ##x and y coordinates of site centres
      coords <- structure(list(x = c(319745L,319825L,320076L,318829L,318914L,320298L,320449L,318824L,319181L,320629L,320871L,319139L,319739L,319807L,318679L,319367L,319379L,319412L,319857L,319914L,319961L,319865L,320019L,319869L,320072L,320109L,319250L,319280L),
                               y = c(5843011L,5843192L,5843333L,5843122L,5843400L,5843617L,5843799L,5842962L,5842400L,5843883L,5843812L,5842892L,5843382L,5843041L,5842887L,5841926L,5842372L,5842609L,5843295L,5843287L,5843252L,5843239L,5843238L,5843183L,5843259L,5843300L,5842800L,5842850L)), 
                          .Names = c("x", "y"), class = "data.frame", row.names = c(NA, -28L))
      ##number of sites
      nsites <- nrow(coords)
      ##initial occupancy status of each site
      initocc <- rep(1, nsites) #all occupied at first timestep
    }   
    ###############################################################################
    ####################### 200M BUFFER + 11 WETLANDS ##############################
    ###############################################################################
    if (condition == "11w") {
      ##effective area of each wetland
      effareasim <- (c(5.360669453,5.871327269,6.135450711,5.345168107,5.119908918,
                      5.751070282,5.542636071,3.091042453,2.898028875,5.086644042,
                      4.724211935,4.997349522,2.952680924,7.367708572,4.920772999,
                      4.196033535,2.663938084,2.585241998,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123)- 5.6)/2.1
      #effareasim <- sapply(effareasim, a)
      ##aquatic vegetation cover for each wetland
      aqvegsim <- (c(75.000,78.333,35.333,23.333,56.667,35.333,35.333,23.333,28.667,35.333,35.333,36.667,
                    0.000,0.333,3.333,0.333,0,0,60,60,60,60,60,60,60,60,60,60,60)- 22)/19.4
      #aqvegsim <- sapply(aqvegsim, b)
      ##x and y coordinates of site centres
      coords <- structure(list(x = c(319745L,319825L,320076L,318829L,318914L,320298L,320449L,318824L,319181L,320629L,320871L,319139L,319739L,319807L,318679L,319367L,319379L,319412L,319857L,319914L,319961L,319865L,320019L,319869L,320072L,320109L,319250L,319280L,319300L),
                               y = c(5843011L,5843192L,5843333L,5843122L,5843400L,5843617L,5843799L,5842962L,5842400L,5843883L,5843812L,5842892L,5843382L,5843041L,5842887L,5841926L,5842372L,5842609L,5843295L,5843287L,5843252L,5843239L,5843238L,5843183L,5843259L,5843300L,5842800L,5842850L,5842800L)), 
                          .Names = c("x", "y"), class = "data.frame", row.names = c(NA, -29L))
      ##number of sites
      nsites <- nrow(coords)
      ##initial occupancy status of each site
      initocc <- rep(1, nsites) #all occupied at first timestep
    }   
    ###############################################################################
    ####################### 200M BUFFER + 12 WETLANDS ##############################
    ###############################################################################
    if (condition == "12w") {
      ##effective area of each wetland
      effareasim <- (c(5.360669453,5.871327269,6.135450711,5.345168107,5.119908918,
                      5.751070282,5.542636071,3.091042453,2.898028875,5.086644042,
                      4.724211935,4.997349522,2.952680924,7.367708572,4.920772999,
                      4.196033535,2.663938084,2.585241998,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123)- 5.6)/2.1
      #effareasim <- sapply(effareasim, a)
      ##aquatic vegetation cover for each wetland
      aqvegsim <- (c(75.000,78.333,35.333,23.333,56.667,35.333,35.333,23.333,28.667,35.333,35.333,36.667,
                    0.000,0.333,3.333,0.333,0,0,60,60,60,60,60,60,60,60,60,60,60,60)- 22)/19.4
      #aqvegsim <- sapply(aqvegsim, b)
      ##x and y coordinates of site centres
      coords <- structure(list(x = c(319745L,319825L,320076L,318829L,318914L,320298L,320449L,318824L,319181L,320629L,320871L,319139L,319739L,319807L,318679L,319367L,319379L,319412L,319857L,319914L,319961L,319865L,320019L,319869L,320072L,320109L,319250L,319280L,319300L,319350L),
                               y = c(5843011L,5843192L,5843333L,5843122L,5843400L,5843617L,5843799L,5842962L,5842400L,5843883L,5843812L,5842892L,5843382L,5843041L,5842887L,5841926L,5842372L,5842609L,5843295L,5843287L,5843252L,5843239L,5843238L,5843183L,5843259L,5843300L,5842800L,5842850L,5842800L,5842830L)), 
                          .Names = c("x", "y"), class = "data.frame", row.names = c(NA, -30L))
      ##number of sites
      nsites <- nrow(coords)
      ##initial occupancy status of each site
      initocc <- rep(1, nsites) #all occupied at first timestep
    }   
    ###############################################################################
    ####################### 200M BUFFER + 13 WETLANDS ##############################
    ###############################################################################
    if (condition == "13w") {
      ##effective area of each wetland
      effareasim <- (c(5.360669453,5.871327269,6.135450711,5.345168107,5.119908918,
                      5.751070282,5.542636071,3.091042453,2.898028875,5.086644042,
                      4.724211935,4.997349522,2.952680924,7.367708572,4.920772999,
                      4.196033535,2.663938084,2.585241998,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123,
                      5.348174123)- 5.6)/2.1
      #effareasim <- sapply(effareasim, a)
      ##aquatic vegetation cover for each wetland
      aqvegsim <- (c(75.000,78.333,35.333,23.333,56.667,35.333,35.333,23.333,28.667,35.333,35.333,36.667,
                    0.000,0.333,3.333,0.333,0,0,60,60,60,60,60,60,60,60,60,60,60,60,60)- 22)/19.4
      #aqvegsim <- sapply(aqvegsim, b)
      ##x and y coordinates of site centres
      coords <- structure(list(x = c(319745L,319825L,320076L,318829L,318914L,320298L,320449L,318824L,319181L,320629L,320871L,319139L,319739L,319807L,318679L,319367L,319379L,319412L,319857L,319914L,319961L,319865L,320019L,319869L,320072L,320109L,319250L,319280L,319300L,319350L,319400L),
                               y = c(5843011L,5843192L,5843333L,5843122L,5843400L,5843617L,5843799L,5842962L,5842400L,5843883L,5843812L,5842892L,5843382L,5843041L,5842887L,5841926L,5842372L,5842609L,5843295L,5843287L,5843252L,5843239L,5843238L,5843183L,5843259L,5843300L,5842800L,5842850L,5842800L,5842830L,5842780L)), 
                          .Names = c("x", "y"), class = "data.frame", row.names = c(NA, -31L))
      ##number of sites
      nsites <- nrow(coords)
      ##initial occupancy status of each site
      initocc <- rep(1, nsites) #all occupied at first timestep
    }
    ###############################################################################
    ####################### 200M BUFFER + 14 WETLANDS ##############################
    ###############################################################################
    if (condition == "14w") {
      ##effective area of each wetland
      effareasim <- (c(5.360669453,5.871327269,6.135450711,5.345168107,5.119908918,
                      5.751070282,5.542636071,3.091042453,2.898028875,5.086644042,
                      4.724211935,4.997349522,2.952680924,7.367708572,4.920772999,
                      4.196033535,2.663938084,2.585241998,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123,
                      5.348174123,5.348174123)- 5.6)/2.1
      #effareasim <- sapply(effareasim, a)
      ##aquatic vegetation cover for each wetland
      aqvegsim <- (c(75.000,78.333,35.333,23.333,56.667,35.333,35.333,23.333,28.667,35.333,35.333,36.667,
                    0.000,0.333,3.333,0.333,0,0,60,60,60,60,60,60,60,60,60,60,60,60,60,60)- 22)/19.4
      #aqvegsim <- sapply(aqvegsim, b)
      ##x and y coordinates of site centres
      coords <- structure(list(x = c(319745L,319825L,320076L,318829L,318914L,320298L,320449L,318824L,319181L,320629L,320871L,319139L,319739L,319807L,318679L,319367L,319379L,319412L,319857L,319914L,319961L,319865L,320019L,319869L,320072L,320109L,319250L,319280L,319300L,319350L,319400L,319380L),
                               y = c(5843011L,5843192L,5843333L,5843122L,5843400L,5843617L,5843799L,5842962L,5842400L,5843883L,5843812L,5842892L,5843382L,5843041L,5842887L,5841926L,5842372L,5842609L,5843295L,5843287L,5843252L,5843239L,5843238L,5843183L,5843259L,5843300L,5842800L,5842850L,5842800L,5842830L,5842780L,5842700L)), 
                          .Names = c("x", "y"), class = "data.frame", row.names = c(NA, -32L))
      ##number of sites
      nsites <- nrow(coords)
      ##initial occupancy status of each site
      initocc <- rep(1, nsites) #all occupied at first timestep
    }
    ###############################################################################
    ####################### 200M BUFFER + 15 WETLANDS ##############################
    ###############################################################################
    if (condition == "15w") {
      ##effective area of each wetland
      effareasim <- (c(5.360669453,5.871327269,6.135450711,5.345168107,5.119908918,
                      5.751070282,5.542636071,3.091042453,2.898028875,5.086644042,
                      4.724211935,4.997349522,2.952680924,7.367708572,4.920772999,
                      4.196033535,2.663938084,2.585241998,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123)- 5.6)/2.1
      #effareasim <- sapply(effareasim, a)
      ##aquatic vegetation cover for each wetland
      aqvegsim <- (c(75.000,78.333,35.333,23.333,56.667,35.333,35.333,23.333,28.667,35.333,35.333,36.667,
                          0.000,0.333,3.333,0.333,0,0,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60)- 22)/19.4
      #aqvegsim <- sapply(aqvegsim, b)
      ##x and y coordinates of site centres
      coords <- structure(list(x = c(319745L,319825L,320076L,318829L,318914L,320298L,320449L,318824L,319181L,320629L,320871L,319139L,319739L,319807L,318679L,319367L,319379L,319412L,319857L,319914L,319961L,319865L,320019L,319869L,320072L,320109L,319250L,319280L,319300L,319350L,319400L,319380L,319600L),
                               y = c(5843011L,5843192L,5843333L,5843122L,5843400L,5843617L,5843799L,5842962L,5842400L,5843883L,5843812L,5842892L,5843382L,5843041L,5842887L,5841926L,5842372L,5842609L,5843295L,5843287L,5843252L,5843239L,5843238L,5843183L,5843259L,5843300L,5842800L,5842850L,5842800L,5842830L,5842780L,5842700L,5842700L)), 
                          .Names = c("x", "y"), class = "data.frame", row.names = c(NA, -33L))
      ##number of sites
      nsites <- nrow(coords)
      ##initial occupancy status of each site
      initocc <- rep(1, nsites) #all occupied at first timestep
    }
    ###############################################################################
    ####################### 200M BUFFER + 16 WETLANDS ##############################
    ###############################################################################
    if (condition == "16w") {
      ##effective area of each wetland
      effareasim <- (c(5.360669453,5.871327269,6.135450711,5.345168107,5.119908918,
                      5.751070282,5.542636071,3.091042453,2.898028875,5.086644042,
                      4.724211935,4.997349522,2.952680924,7.367708572,4.920772999,
                      4.196033535,2.663938084,2.585241998,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123)- 5.6)/2.1
      #effareasim <- sapply(effareasim, a)
      ##aquatic vegetation cover for each wetland
      aqvegsim <- (c(75.000,78.333,35.333,23.333,56.667,35.333,35.333,23.333,28.667,35.333,35.333,36.667,
                    0.000,0.333,3.333,0.333,0,0,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60)- 22)/19.4
      #aqvegsim <- sapply(aqvegsim, b)
      ##x and y coordinates of site centres
      coords <- structure(list(x = c(319745L,319825L,320076L,318829L,318914L,320298L,320449L,318824L,319181L,320629L,320871L,319139L,319739L,319807L,318679L,319367L,319379L,319412L,319857L,319914L,319961L,319865L,320019L,319869L,320072L,320109L,319250L,319280L,319300L,319350L,319400L,319380L,319600L,319600L),
                               y = c(5843011L,5843192L,5843333L,5843122L,5843400L,5843617L,5843799L,5842962L,5842400L,5843883L,5843812L,5842892L,5843382L,5843041L,5842887L,5841926L,5842372L,5842609L,5843295L,5843287L,5843252L,5843239L,5843238L,5843183L,5843259L,5843300L,5842800L,5842850L,5842800L,5842830L,5842780L,5842700L,5842700L,5842800L)), 
                          .Names = c("x", "y"), class = "data.frame", row.names = c(NA, -34L))
      ##number of sites
      nsites <- nrow(coords)
      ##initial occupancy status of each site
      initocc <- rep(1, nsites) #all occupied at first timestep
    }
    ###############################################################################
    ####################### 200M BUFFER + 17 WETLANDS ##############################
    ###############################################################################
    if (condition == "17w") {
      ##effective area of each wetland
      effareasim <- (c(5.360669453,5.871327269,6.135450711,5.345168107,5.119908918,
                      5.751070282,5.542636071,3.091042453,2.898028875,5.086644042,
                      4.724211935,4.997349522,2.952680924,7.367708572,4.920772999,
                      4.196033535,2.663938084,2.585241998,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123)- 5.6)/2.1
      #effareasim <- sapply(effareasim, a)
      ##aquatic vegetation cover for each wetland
      aqvegsim <- (c(75.000,78.333,35.333,23.333,56.667,35.333,35.333,23.333,28.667,35.333,35.333,36.667,
                    0.000,0.333,3.333,0.333,0,0,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60)- 22)/19.4
      #aqvegsim <- sapply(aqvegsim, b)
      ##x and y coordinates of site centres
      coords <- structure(list(x = c(319745L,319825L,320076L,318829L,318914L,320298L,320449L,318824L,319181L,320629L,320871L,319139L,319739L,319807L,318679L,319367L,319379L,319412L,319857L,319914L,319961L,319865L,320019L,319869L,320072L,320109L,319250L,319280L,319300L,319350L,319400L,319380L,319600L,319600L,319650L),
                               y = c(5843011L,5843192L,5843333L,5843122L,5843400L,5843617L,5843799L,5842962L,5842400L,5843883L,5843812L,5842892L,5843382L,5843041L,5842887L,5841926L,5842372L,5842609L,5843295L,5843287L,5843252L,5843239L,5843238L,5843183L,5843259L,5843300L,5842800L,5842850L,5842800L,5842830L,5842780L,5842700L,5842700L,5842800L,5842800L)), 
                          .Names = c("x", "y"), class = "data.frame", row.names = c(NA, -35L))
      ##number of sites
      nsites <- nrow(coords)
      ##initial occupancy status of each site
      initocc <- rep(1, nsites) #all occupied at first timestep
    } 
    ###############################################################################
    ####################### 200M BUFFER + 18 WETLANDS ##############################
    ###############################################################################
    if (condition == "18w") {
      ##effective area of each wetland
      effareasim <- (c(5.360669453,5.871327269,6.135450711,5.345168107,5.119908918,
                      5.751070282,5.542636071,3.091042453,2.898028875,5.086644042,
                      4.724211935,4.997349522,2.952680924,7.367708572,4.920772999,
                      4.196033535,2.663938084,2.585241998,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123,
                      5.348174123)- 5.6)/2.1
      #effareasim <- sapply(effareasim, a)
      ##aquatic vegetation cover for each wetland
      aqvegsim <- (c(75.000,78.333,35.333,23.333,56.667,35.333,35.333,23.333,28.667,35.333,35.333,36.667,
                    0.000,0.333,3.333,0.333,0,0,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60)- 22)/19.4
      #aqvegsim <- sapply(aqvegsim, b)
      ##x and y coordinates of site centres
      coords <- structure(list(x = c(319745L,319825L,320076L,318829L,318914L,320298L,320449L,318824L,319181L,320629L,320871L,319139L,319739L,319807L,318679L,319367L,319379L,319412L,319857L,319914L,319961L,319865L,320019L,319869L,320072L,320109L,319250L,319280L,319300L,319350L,319400L,319380L,319600L,319600L,319650L,319660L),
                               y = c(5843011L,5843192L,5843333L,5843122L,5843400L,5843617L,5843799L,5842962L,5842400L,5843883L,5843812L,5842892L,5843382L,5843041L,5842887L,5841926L,5842372L,5842609L,5843295L,5843287L,5843252L,5843239L,5843238L,5843183L,5843259L,5843300L,5842800L,5842850L,5842800L,5842830L,5842780L,5842700L,5842700L,5842800L,5842800L,5842850L)), 
                          .Names = c("x", "y"), class = "data.frame", row.names = c(NA, -36L))
      ##number of sites
      nsites <- nrow(coords)
      ##initial occupancy status of each site
      initocc <- rep(1, nsites) #all occupied at first timestep
    } 
    ###############################################################################
    ####################### 200M BUFFER + 19 WETLANDS ##############################
    ###############################################################################
    if (condition == "19w") {
      ##effective area of each wetland
      effareasim <- (c(5.360669453,5.871327269,6.135450711,5.345168107,5.119908918,
                      5.751070282,5.542636071,3.091042453,2.898028875,5.086644042,
                      4.724211935,4.997349522,2.952680924,7.367708572,4.920772999,
                      4.196033535,2.663938084,2.585241998,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123,
                      5.348174123,5.348174123)- 5.6)/2.1
      #effareasim <- sapply(effareasim, a)
      ##aquatic vegetation cover for each wetland
      aqvegsim <- (c(75.000,78.333,35.333,23.333,56.667,35.333,35.333,23.333,28.667,35.333,35.333,36.667,
                    0.000,0.333,3.333,0.333,0,0,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60)- 22)/19.4
      #aqvegsim <- sapply(aqvegsim, b)
      ##x and y coordinates of site centres
      coords <- structure(list(x = c(319745L,319825L,320076L,318829L,318914L,320298L,320449L,318824L,319181L,320629L,320871L,319139L,319739L,319807L,318679L,319367L,319379L,319412L,319857L,319914L,319961L,319865L,320019L,319869L,320072L,320109L,319250L,319280L,319300L,319350L,319400L,319380L,319600L,319600L,319650L,319660L,319700L),
                               y = c(5843011L,5843192L,5843333L,5843122L,5843400L,5843617L,5843799L,5842962L,5842400L,5843883L,5843812L,5842892L,5843382L,5843041L,5842887L,5841926L,5842372L,5842609L,5843295L,5843287L,5843252L,5843239L,5843238L,5843183L,5843259L,5843300L,5842800L,5842850L,5842800L,5842830L,5842780L,5842700L,5842700L,5842800L,5842800L,5842850L,5842900L)), 
                          .Names = c("x", "y"), class = "data.frame", row.names = c(NA, -37L))
      ##number of sites
      nsites <- nrow(coords)
      ##initial occupancy status of each site
      initocc <- rep(1, nsites) #all occupied at first timestep
    } 
    ###############################################################################
    ####################### 200M BUFFER + 20 WETLANDS ##############################
    ###############################################################################
    if (condition == "20w") {
      ##effective area of each wetland
      effareasim <- (c(5.360669453,5.871327269,6.135450711,5.345168107,5.119908918,
                      5.751070282,5.542636071,3.091042453,2.898028875,5.086644042,
                      4.724211935,4.997349522,2.952680924,7.367708572,4.920772999,
                      4.196033535,2.663938084,2.585241998,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123,5.348174123,5.348174123,
                      5.348174123,5.348174123,5.348174123)- 5.6)/2.1
      #effareasim <- sapply(effareasim, a)
      ##aquatic vegetation cover for each wetland
      aqvegsim <- (c(75.000,78.333,35.333,23.333,56.667,35.333,35.333,23.333,28.667,35.333,35.333,36.667,
                    0.000,0.333,3.333,0.333,0,0,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60)- 22)/19.4
      #aqvegsim <- sapply(aqvegsim, b)
      ##x and y coordinates of site centres
      coords <- structure(list(x = c(319745L,319825L,320076L,318829L,318914L,320298L,320449L,318824L,319181L,320629L,320871L,319139L,319739L,319807L,318679L,319367L,319379L,319412L,319857L,319914L,319961L,319865L,320019L,319869L,320072L,320109L,319250L,319280L,319300L,319350L,319400L,319380L,319600L,319600L,319650L,319660L,319700L,319700L),
                               y = c(5843011L,5843192L,5843333L,5843122L,5843400L,5843617L,5843799L,5842962L,5842400L,5843883L,5843812L,5842892L,5843382L,5843041L,5842887L,5841926L,5842372L,5842609L,5843295L,5843287L,5843252L,5843239L,5843238L,5843183L,5843259L,5843300L,5842800L,5842850L,5842800L,5842830L,5842780L,5842700L,5842700L,5842800L,5842800L,5842850L,5842900L,5842950L)), 
                          .Names = c("x", "y"), class = "data.frame", row.names = c(NA, -38L))
      ##number of sites
      nsites <- nrow(coords)
      ##initial occupancy status of each site
      initocc <- rep(1, nsites) #all occupied at first timestep
    } 
    #### Set some parameters for the simulations ####
    ## No. of iterations
    ITERS <- 500
    ## No. of years
    YEARS <- 30
    
    #### Set up connectivity measure ####
    ## Neighbourhood radius
    cutdist <- 1000 # in units that coordinates were supplied in
    ## Measure distances between sites
    distsim <- as.matrix(dist(coords, diag=TRUE, upper=TRUE))
    ## Round up to nearest 10 metres (assuming coords units = metres)
    distsim <- ceiling(distsim/10) * 10
    ## Set diagonals to NA (to prevent including 'self-connectivity')
    diag(distsim) <- NA
    ## Determine neighbours for each site (cells with TRUE are neighbours)
    nbr <- distsim <= cutdist & distsim != 0
    ## Calculate distance weighting
    distweight <- (0.10026*distsim)^-0.719877
    ## Apply distance weighting to neighbours only
    nbr.distweight <- nbr*distweight
    
    #### Metapopulation model ####
    ## Each call to the 'metapopmodel' function performs a single iteration of the 
    ## model, and returns the minimum number of occupied patches (across years).
    metapopmodel <- function(alpha.gamma, alpha.epsilon, beta.gamma1, beta.epsilon1, beta.epsilon2, beta.epsilon3){
      # Preallocate arrays
      occ <- consim <- gamsim <- epssim <- poccsim <- array(dim=c(YEARS, nsites))
      conwsim <- array(dim=c(YEARS, nsites, nsites))
      nocc <- vector(length=YEARS)
      # Start the model (parameters at first time-step)
      occ[1, ] <- initocc
      nocc[1] <- sum(occ[1, ])
      # Dynamics
      for (t in 2:YEARS){
        # Calculate contribution of each neighbour to connectivity at each time-step
        conwsim[t, , ] <- nbr.distweight * occ[t-1, ]
        # Calculate connectivity
        consim[t, ] <- ((log(colSums(conwsim[t, , ], na.rm=TRUE) + 1))-0.13)/0.16
        #consim[t, ] <- apply(consim[t, ], c)
        # Specify logistic model for the probability of colonisation
        logit.gam <- alpha.gamma + beta.gamma1*consim[t, ]
        gamsim[t, ] <- sapply(logit.gam, plogis) # Inverse logit applied to each element of logit.gam
        # Specify logistic model for the probability of extinction
        logit.eps <- alpha.epsilon + beta.epsilon1*effareasim + beta.epsilon2*aqvegsim + beta.epsilon3*consim[t, ]
        epssim[t, ] <- sapply(logit.eps, plogis) # Inverse logit applied to each element of logit.eps
        # Probability of occupancy in each year a function of occupancy status and 
        #  extinction and colonisation probabilities
        poccsim[t, ] <- occ[t-1, ] * (1-epssim[t, ]) + (1-occ[t-1, ]) * gamsim[t, ]
        # Occupancy in each year a Bernoulli variable, with probability poccsim
        occ[t, ] <- rbinom(nsites, 1, poccsim[t, ])
      }
      # Number of patches occupied at each timestep
      nocc <- rowSums(occ) 
      # Minimum number occupied over all timesteps
      minocc <- min(nocc)
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
                                    beta.epsilon1[x], beta.epsilon2[x], beta.epsilon3[x]))
    })
    
    #### Calculate summaries and write to file ####
    ## Mean minimum number of patches occupied
    mean_minocc <- colMeans(results)
    ## Export
    filename = paste(run_name,".minocc_predictions.",condition,".csv",sep="")
    write.csv(data.frame(minocc=mean_minocc), file=filename, 
              row.names=FALSE)
    }              
}

cat("now simulating for run name:",run_name,"\n")
simulate_wetland_scenarios (run_name)
