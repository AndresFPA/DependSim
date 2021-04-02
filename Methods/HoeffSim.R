#Hoeffding D
help("hoeffd")
Hoeff_sim <- function(SimData){
  res <- hoeffd(as.matrix(SimData))
  p_value <- res$P[1, 2]
  return(p_value)
}


