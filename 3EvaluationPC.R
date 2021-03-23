#Evaluation Criterion: 
#to determine the power of the method: 
#is the p-value of the test significant at 0.05 level?
#if yes, then the test rejected the null-hypothesis of independence
#i.e., the alternative hypothesis is correctly accepted (dependence)
#if no, then the null hypothesis is incorrectly not rejected.

EvaluationPC <- function(MyAnalysisResult){
  res <- ifelse(MyAnalysisResult < 0.05, 1, 0)
  return(res)
}

