#HSIC
help("dHSIC")
HSIC_sim <- function(SimData){
  res <- dhsic.test(SimData$x, SimData$y, B = 10000)
  p_value <- res$p.value
  return(p_value)
}

