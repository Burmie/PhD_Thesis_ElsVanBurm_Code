data.scenarios <- c(3,6)
wetland.scenarios <- c("0w","1w","2w","3w","4w","5w","6w","7w","8w","9w","10w","11w","12w","13w","14w","15w","16w","17w","18w","19w","20w")

primp_matrix <- cost_matrix <- matrix(NA, nrow=length(data.scenarios), ncol=length(wetland.scenarios))

for (k in 1:length(data.scenarios)){
  for (l in 1:length(wetland.scenarios)){
    rownames(primp_matrix) <- data.scenarios
    colnames(primp_matrix) <- wetland.scenarios
    i <- data.scenarios[k]
    j <- wetland.scenarios[l]
    minocc.wl <- read.csv(paste("../03_minocc_projections/",i,"_years.minocc_predictions.",j,".csv",sep="")) 
    minocc.cc <- read.csv(paste("../03_minocc_projections/",i,"_years.minocc_predictions.cc.csv",sep=""))
    change_in_minocc <- data.frame(minocc.wl - minocc.cc)
    npositives <- nrow(subset(change_in_minocc, change_in_minocc > 0)) # counts the number of positive values in change_in_minocc dataframe
    nzeros <- nrow(subset(change_in_minocc, change_in_minocc == 0)) # counts the number of zeros in change_in_minocc dataframe 
    prob.improvement <- (npositives + (0.5 * nzeros)) / nrow(change_in_minocc) # calculates prob of improvement
    primp_matrix[k,l] <- prob.improvement
  }
}
primp_matrix


for (l in 0:(length(wetland.scenarios)-1)){
  cost_matrix[1,(l+1)] <- 116390 + (l * 350000)  
  cost_matrix[2,(l+1)] <- 265740 + (l * 350000)
  rownames(cost_matrix) <- data.scenarios
  colnames(cost_matrix) <- wetland.scenarios
}
cost_matrix

xvals_3 <- cost_matrix[1,]
yvals_3 <- primp_matrix[1,]
plot(xvals_3,yvals_3,xlim=c(0,max(cost_matrix)), ylim=c(0,1), xlab="Total cost monitoring and management (AUD)", ylab="Probability of improvement", pch=1, col=2)
lines(xvals_3,yvals_3,xlim=c(0,max(cost_matrix)), ylim=c(0,1), xlab="Total cost monitoring and management (AUD)", ylab="Probability of improvement", pch=1, col=2)

xvals_6 <- cost_matrix[2,]
yvals_6 <- primp_matrix[2,]
points(xvals_6,yvals_6, pch=2, col=3)
lines(xvals_6,yvals_6, pch=2, col=3)
legend('topleft',c("3 years", "6 years"), col=c(2,3), text.col=c(2,3), pch=c(1,2))
