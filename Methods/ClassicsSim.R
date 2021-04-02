#Pearson, Kendall, Spearman
classics <- function(SimData){
  pear <- cor.test(SimData$x, SimData$y, method = "pearson")
  kend <- cor.test(SimData$x, SimData$y, method = "kendall")
  spea <- cor.test(SimData$x, SimData$y, method = "spearman")
  classic_list <- list(pear$p.value, kend$p.value, spea$p.value)
  names(classic_list) <- c("Pearson", "Kendall", "Spearman")
  return(classic_list)
}



