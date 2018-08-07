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
## Save nbr.distweight matrix to load into projections file 
save(nbr.distweight, file = "nbr.distweight")
