MI_sim <- function(SimData) {
  mi_coeff <- cminjk(SimData)[1, 2]
  # Do a permutation test
  Y <- SimData[, 2] # Get Y as an individual variable
  nReplicates <- 10000
  PermutValues <- numeric(nReplicates)
  nElem <- dim(SimData)[1] # number of cases
  for (i in 1:nReplicates) {
    # get a permutation from the values 1:nElem
    permutation <- sample(nElem, nElem, replace = F)
    # use the permutation of indices (1:nElem) to permute the values of one of the columns
    PermData <- cbind(SimData[, 1], Y[permutation])
    PermutValues[i] <- cminjk(PermData)[1, 2]
  }
  p_value <- mean(abs(PermutValues) > abs(mi_coeff))
  return(p_value)
}
