#Comparison of dependence tests
# Variable x is randomly generated
# Variable y is generated based on one type of relationship with y (linear, quedratic, etc.)
# noise is the error introduced while creating the relationship (high, medium, low)
# samp is the sample size (10, 20, 35, 50, 75, 100, 150, 500)
DataGeneration <- function(samp, noise, relationship){
  x <- rnorm(samp) 
  if(relationship == "linear"){
    y <- linear(x, noise)
  } else if(relationship == "quadratic"){
    y <- quadratic(x, noise)
  } else if(relationship == "exponential"){
    y <- exponential(x, noise)
  } else if(relationship == "sinwave"){
    y <- sinwave(x, noise)
  } else if(relationship == "cross"){
    y <- cross(x, noise)
  } else if(relationship == "no_relationship"){
    y <- no_relationship(x, noise)
  }
  XY <- cbind(x, y)
  dat <- data.frame(XY)
  return(dat) 
}


