## This script rearranges sites so that they are situated at increasing distance from site 13 (most western site)
#######################################################################################################################
coords <- read.table("Coords_monitored_sites.txt",header=T,sep="\t")

to_calculate_distance_from_site_13 <- matrix(NA, 167, 2) # site 13 is most western placed site, declare a matrix with 167 rows and 2 columns
to_calculate_distance_from_site_13[, 1] <- coords[,2] - coords[13,2] # take the x-coordinate of each site minus the x-coordinate of site 13 and put in first column of new matrix
to_calculate_distance_from_site_13[, 2] <- coords[,3] - coords[13,3] # take the y-coordinate of each site minus the y-coordinate site 13 and put in second column of new matrix 
to_calculate_distance_from_site_13

distance_to_the_power <- to_calculate_distance_from_site_13^2 # raise that matrix to the power 2
distance_to_the_power

distance_from_site_13 <- sqrt(distance_to_the_power[,1] + distance_to_the_power[,2]) # distance between each site and site 13 using Pythagoras 
distance_from_site_13

first_5_sites <- which(distance_from_site_13 %in% sort(distance_from_site_13)[1:5]) # 5 sites that are closest to site 13 
first_5_sites
site_6_25 <- which(distance_from_site_13 %in% sort(distance_from_site_13)[6:25]) # next ones
site_6_25
site_26_50 <- which(distance_from_site_13 %in% sort(distance_from_site_13)[26:50]) # next ones
site_26_50
site_51_100 <- which(distance_from_site_13 %in% sort(distance_from_site_13)[51:100]) # next ones
site_51_100
site_101_125 <- which(distance_from_site_13 %in% sort(distance_from_site_13)[101:125]) # next ones
site_101_125
site_126_167 <- which(distance_from_site_13 %in% sort(distance_from_site_13)[126:167]) # next ones - not included in monitoring scenario but needed for modelling purposes
site_126_167

new_coords_matrix <- matrix(NA, nrow=167, ncol=2) # create new matrix with coordinates ordered from closest to furthest from site 13
new_coords_matrix[1:5,1] <- coords[first_5_sites,2] # these are the 5 sites that are closest to site 13 but they are not given in ascending order
new_coords_matrix[1:5,2] <- coords[first_5_sites,3] 
plot(new_coords_matrix[1:5,1],new_coords_matrix[1:5,2],xlab="x-coordinate first 5 sites (mon_scen0)", ylab="y-coordinate first 5 sites (mon_scen0)",xlim=c(314000,328000),ylim=c(5825000,5850000)) # plot first 5 sites (mon_scen0)
new_coords_matrix[6:25,1] <- coords[site_6_25,2] # these are the 25 sites that are closest to site 13 but they are not given in ascending order
new_coords_matrix[6:25,2] <- coords[site_6_25,3] 
plot(new_coords_matrix[1:25,1],new_coords_matrix[1:25,2],xlab="x-coordinate first 25 sites (mon_scen1)", ylab="y-coordinate first 25 sites (mon_scen1)",xlim=c(314000,328000),ylim=c(5825000,5850000)) # plot first 25 sites (mon_scen1)
new_coords_matrix[26:50,1] <- coords[site_26_50,2]
new_coords_matrix[26:50,2] <- coords[site_26_50,3]
plot(new_coords_matrix[1:50,1],new_coords_matrix[1:50,2],xlab="x-coordinate first 50 sites (mon_scen2)", ylab="y-coordinate first 50 sites (mon_scen2)",xlim=c(314000,328000),ylim=c(5825000,5850000)) # plot first 50 sites (mon_scen2)
new_coords_matrix[51:100,1] <- coords[site_51_100,2]
new_coords_matrix[51:100,2] <- coords[site_51_100,3]
plot(new_coords_matrix[1:100,1],new_coords_matrix[1:100,2],xlab="x-coordinate first 100 sites (mon_scen3)", ylab="y-coordinate first 100 sites (mon_scen3)",xlim=c(314000,328000),ylim=c(5825000,5850000)) # plot first 100 sites (mon_scen3)
new_coords_matrix[101:125,1] <- coords[site_101_125,2]
new_coords_matrix[101:125,2] <- coords[site_101_125,3]
plot(new_coords_matrix[1:125,1],new_coords_matrix[1:125,2],xlab="x-coordinate first 125 sites (mon_scen4)", ylab="y-coordinate first 125 sites (mon_scen4)",xlim=c(314000,328000),ylim=c(5825000,5850000)) # plot first 125 sites (mon_scen4)
new_coords_matrix[126:167,1] <- coords[site_126_167,2]
new_coords_matrix[126:167,2] <- coords[site_126_167,3]
plot(new_coords_matrix[1:167,1],new_coords_matrix[1:167,2],xlab="x-coordinate first 167 sites (no mon_scen)", ylab="y-coordinate first 167 sites (no mon_scen)",xlim=c(314000,328000),ylim=c(5825000,5850000)) # plot first 167 sites (no mon_scen)

new_coords_matrix

new_distance_matrix <- as.matrix(dist(new_coords_matrix, upper = TRUE))# calculate pairwise euclidian distance between newly ordered sites 
diag(new_distance_matrix) <- 10
new_distance_matrix <- 10*(round(new_distance_matrix/10)) # to increment distances by 10

cat(new_distance_matrix, file="00_125_sites_new_distance_matrix.txt", sep=",")
