SimulationCell <- function(Design = Design, RowOfDesign = 1, K = 1000) {
  # Input arguments:
  # Design = designmatrix
  # RowOfDesign: number that refers to the row of the design matrix = one cell
  # K: Total number of replications = number of data sets generated in one cell
  # Create matrix or dataframe to store the results:
  MyResult <- matrix(NA, nrow = K, ncol = 9)
  # create a loop over the replications k = 1 to K:
  tmp <- proc.time()
  for (k in 1:K) {
    # Generate data
    # set a random number seed to be able to replicate the result exactly
    set.seed((k + 1000) * RowOfDesign)
    SimDat <- do.call(DataGeneration, Design[RowOfDesign, ])
    # Analyze data set with each method
    MyAnalysisResult1 <- classics(SimDat)$Pearson
    MyAnalysisResult2 <- classics(SimDat)$Kendall
    MyAnalysisResult3 <- classics(SimDat)$Spearman
    MyAnalysisResult4 <- HHG_sim(SimDat)
    MyAnalysisResult5 <- HSIC_sim(SimDat)
    MyAnalysisResult6 <- Hoeff_sim(SimDat)
    MyAnalysisResult7 <- dcor_sim(SimDat)
    MyAnalysisResult8 <- MI_sim(SimDat)
    MyAnalysisResult9 <- min(MyAnalysisResult1, MyAnalysisResult4) * 2 #HHG and Pearson with Bonferroni correction
    
    # Evaluate the analysis results
    MyResult1 <- EvaluationPC(MyAnalysisResult1)
    MyResult2 <- EvaluationPC(MyAnalysisResult2)
    MyResult3 <- EvaluationPC(MyAnalysisResult3)
    MyResult4 <- EvaluationPC(MyAnalysisResult4)
    MyResult5 <- EvaluationPC(MyAnalysisResult5)
    MyResult6 <- EvaluationPC(MyAnalysisResult6)
    MyResult7 <- EvaluationPC(MyAnalysisResult7)
    MyResult8 <- EvaluationPC(MyAnalysisResult8)
    MyResult9 <- EvaluationPC(MyAnalysisResult9)

    # store the results in the right row k of the result matrix:
    MyResult[k, ] <- c(
      MyResult1, MyResult2,
      MyResult3, MyResult4,
      MyResult5, MyResult6,
      MyResult7, MyResult8,
      MyResult9
    )
  }
  # save the time to run the analyses of K data sets in one cell of the design.
  time <- proc.time() - tmp
  return(MyResult)
}

#Check it works
Row <- 1
system.time(MyResult <- SimulationCell(Design = Design, RowOfDesign = 7, K = 10))
# Write output of one cell of the design
save(MyResult, file = paste("MyResult", "Row", Row,".Rdata" , sep =""))
#optional to save timing of analyses of K replications in 1 cell
save(time, file = paste("Time", "Row", Row, ".Rdata" , sep =""))


