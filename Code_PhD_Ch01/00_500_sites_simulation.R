# Create new sites so we can run monitoring regime with only 5 years and 500 sites 
x_0 <- 314000 # x_min of coords 
x_1 <- 326000 # x_med of coords
x_2 <- 362000 # x_max of coords

y_0 <- 5825000 # y_min of coords
y_1 <- 5845000 # y_med of coords
y_2 <- 5905000 # y_max of coords

# Generate 375 points in the 3 quadrants surrounding the original sites (to arrive a total of 500 sites)
quadrant_1_x <- sample(x_1:x_2,125)
quadrant_1_y <- sample(y_0:y_1,125)
quadrant_2_x <- sample(x_1:x_2,125)
quadrant_2_y <- sample(y_1:y_2,125)
quadrant_3_x <- sample(x_0:x_1,125)
quadrant_3_y <- sample(y_1:y_2,125)

plot(quadrant_1_x,quadrant_1_y, xlim=c(314000,362000), ylim=c(5825000,5905000))
points(quadrant_2_x,quadrant_2_y)
points(quadrant_3_x,quadrant_3_y)

# Read in the original 125 sites
original_sites <- new_coords_matrix[1:125,] # !! run file "00_125_sites_create_new_coords_and_distance_matrix.R"first
original_sites_x <- original_sites[,1]
original_sites_y <- original_sites[,2]
points(original_sites_x,original_sites_y)

# Create new matrix with coordinates of all 500 sites
coords_500_sites <- matrix(NA, nrow=500, ncol=2)
coords_500_sites[1:125, ] <- new_coords_matrix[1:125,]
coords_500_sites[126:250, 1] <- quadrant_1_x
coords_500_sites[126:250, 2] <- quadrant_1_y
coords_500_sites[251:375, 1] <- quadrant_2_x
coords_500_sites[251:375, 2] <- quadrant_2_y
coords_500_sites[376:500, 1] <- quadrant_3_x
coords_500_sites[376:500, 2] <- quadrant_3_y
coords_500_sites

# Calculate pairwise distance between all coordinates
distance_matrix_500_sites <-as.matrix(dist(coords_500_sites, upper = TRUE))
diag(distance_matrix_500_sites) <- 10
distance_matrix_500_sites <- 10*(round(distance_matrix_500_sites/10)) # to increment distances by 10
distance_matrix_500_sites
cat(distance_matrix_500_sites, file="00_500_sites_distance_matrix.txt", sep=",")
