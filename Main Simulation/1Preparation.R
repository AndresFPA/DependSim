#Preparation of the analysis
#Define relationships
  #Noise level (High, Medium, Low) were defined in "noise.R"
linear <- function(x, noise = 0){
  if (noise == "High"){
    error <- rnorm(length(x), 0, 4.583259)
  } else if (noise == "Medium"){
    error <- rnorm(length(x), 0, 2.3100389)
  } else if (noise == "Low"){
    error <- rnorm(length(x), 0, 1.1631811)
  } else {
    error <- rnorm(length(x), 0, noise)
  }
  y <- (x*2) + error
}

quadratic <- function(x, noise = 0){
  if (noise == "High"){
    error <- rnorm(length(x), 0, 3.997505)
  } else if (noise == "Medium"){
    error <- rnorm(length(x), 0, 1.5391051)
  } else if (noise == "Low") {
    error <- rnorm(length(x), 0, 0.8560691)
  } else {
    error <- rnorm(length(x), 0, noise)
  }
  y <- (-1*x^2) + error
}

exponential <- function(x, noise = 0){
  if (noise == "High"){
    error <- rnorm(length(x), 0, 4.583592)
  } else if (noise == "Medium"){
    error <- rnorm(length(x), 0, 2.3321780)
  } else if (noise == "Low") {
    error <- rnorm(length(x), 0, 1.2121817)
  } else {
    error <- rnorm(length(x), 0, noise)
  }
  y <- -exp(-x) + error
  return(y)
}

sinwave <- function(x, noise = 0){
  if (noise == "High"){
    error <- rnorm(length(x), 0, 2.291796)
  } else if (noise == "Medium"){
    error <- rnorm(length(x), 0, 0.7670125)
  } else if (noise == "Low") {
    error <- rnorm(length(x), 0, 0.4103573)
  } else {
    error <- rnorm(length(x), 0, noise)
  }
  y <- sin(3*x) + error
  return(y)
}

cross <- function(x, noise = 0){
  if (noise == "High"){
    error <- rnorm(length(x), 0, 1)
  } else if (noise == "Medium"){
    error <- rnorm(length(x), 0, 0.6)
  } else if (noise == "Low") {
    error <- rnorm(length(x), 0, 0.3)
  } else {
    error <- rnorm(length(x), 0, noise)
  }
  y <- x*sign(rnorm(length(x),0,1)) + error
  return(y)
}

### Initialize the factors of your design:
samp <- c(10, 20, 35, 50, 75, 100, 150, 500)
noise <- c("Low", "Medium", "High")
relationship <- c("linear", "quadratic", "exponential", "sinwave", "cross")

##And create the simulation design matrix (full factorial)
# Design is a data.frame with all possible combinations of the factor levels
# Each row of the design matrix represents a cell of your simulation design
Design <- expand.grid(samp = samp, noise = noise, relationship = relationship)

###Preparation of the analysis:
# Install the relevant R packages:
install.packages("HHG")
install.packages("dHSIC")
install.packages("energy")
install.packages("mpmi")
install.packages("Hmisc")
install.packages("entropy")
install.packages("np")

#Always use library() to activate the package
library(HHG)
library(dHSIC)
library(mpmi)
library(energy)
library(Hmisc)
library(entropy)

### Source the relevant R functions
source("2DataGeneration.R")
source("3EvaluationPC.R")
source("4SimulationCell.R")
source("HHGSim.R")
source("HSICSim.R")
source("ClassicsSim.R")
source("HoeffSim.R")
source("dcorSim.R")
source("MISim.R")

#For Type I Error rate
no_relationship <- function(x, noise){
  y <- rnorm(length(x))
  return(y)
}

### Initialize the factors of your design:
samp <- c(10, 20, 35, 50, 75, 100, 150, 500)
relationship <- "no_relationship"
noise <- 0

Design_no <- expand.grid(samp = samp, relationship = relationship, noise = noise)
