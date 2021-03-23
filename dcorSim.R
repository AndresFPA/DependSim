#Distance Correlation
dcor_sim <- function(SimData){
  #10000 permutations to obatin p value
  res <- dcor.test(SimData$x, SimData$y, R = 10000)
  p_value <- res$p.value
  return(p_value)
}
