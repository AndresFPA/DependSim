#HHG
HHG_sim <- function(SimData){
  dist_x <- as.matrix(dist(SimData$x, diag = TRUE, upper = TRUE))
  dist_y <- as.matrix(dist(SimData$y, diag = TRUE, upper = TRUE))
  #10000 permutation for N<100 and 1000 for N>100
  if (nrow(SimData) < 100) {
    res <- hhg.test(dist_x, dist_y, nr.perm = 10000)
    p_value <- res$perm.pval.hhg.sl
  } else {
    res <- hhg.test(dist_x, dist_y, nr.perm = 1000)
    p_value <- res$perm.pval.hhg.sl
  }
  return(p_value)
}







