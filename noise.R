setwd("C:/Users/Andres/Desktop/Leiden/Thesis/sim/Mine/noise")
#Load required package for R2 estimate
library(np)

#Create function to estimate R2
R2_estimate <- function(x, y) {
  bw <- npregbw(formula = y ~ x)
  mod.1 <- npreg(bws = bw, gradients = FALSE, residuals = FALSE)
  return(mod.1$R2)
}

#Define functions to optimize noise levels

linear_noise <- function(noise, wanted_R2, samp_size){
  x <- rnorm(samp_size)
  error <- rnorm(length(x), 0, noise)
  y <- (x * 2) + error
  R2 <- R2_estimate(x, y)
  (R2 - wanted_R2)^2
}

quadratic_noise <- function(noise, wanted_R2, samp_size){
  x <- rnorm(samp_size)
  error <- rnorm(length(x), 0, noise)
  y <- (-1*x^2) + error
  R2 <- R2_estimate(x, y)
  (R2 - wanted_R2)^2
}

exponential_noise <- function(noise, wanted_R2, samp_size){
  x <- rnorm(samp_size)
  error <- rnorm(length(x), 0, noise)
  y <- -exp(-x) + error
  R2 <- R2_estimate(x, y)
  (R2 - wanted_R2)^2
}

sine_noise <- function(noise, wanted_R2, samp_size){
  x <- rnorm(samp_size)
  error <- rnorm(length(x), 0, noise)
  y <- sin(3*x) + error
  R2 <- R2_estimate(x, y)
  (R2 - wanted_R2)^2
}

##------------------------------------------------------------------------------------------------##
##---------------------------Optimization sd value for desired R^2--------------------------------##
##------------------------------------------------------------------------------------------------##

#Define the desired R2 values
noises_r2 <- c(0.1, 0.45, 0.75)

##---------------------------------------Linear Noise---------------------------------------------##
#Create an empty vector to store the sd
noises_sd_lin <- c()

#Initiate loop to get all results
system.time(for(j in seq_along(noises_r2)){
  noise <- optimize(linear_noise, c(0, 6), wanted_R2 = noises_r2[j], samp_size = 10, tol = 0.001)$minimum
  noises_sd_lin[j] <- noise
})

final_linear <- data.frame(noises_sd_lin)
save(final_linear, file = "final_linear.Rdata")

##---------------------------------------Quadratic Noise------------------------------------------##
#Create an empty vector to store the sd
noises_sd_quad <- c()

#Initiate loop to get all results
system.time(for(j in seq_along(noises_r2)){
  a <- optimize(quadratic_noise, c(0, 6), wanted_R2 = noises_r2[j], samp_size = 10, tol = 0.001)$minimum
  noises_sd_quad[j] <- a
})


#define names for the three results
final_quad <- data.frame(noises_sd_quad)
save(final_quad, file = "final_quadratic.Rdata")

##---------------------------------------Exponential Noise----------------------------------------##
#Create an empty vector to store the sd
noises_sd_exponen <- c()

#Initiate loop to get all results
system.time(for(j in seq_along(noises_r2)){
  a <- optimize(exponential_noise, c(0, 6), wanted_R2 = noises_r2[j], samp_size = 10, tol = 0.001)$minimum
  noises_sd_exponen[j] <- a
})

final_exponen <- data.frame(noises_sd_exponen)
save(final_exponen, file = "final_exponen.Rdata")

##---------------------------------------Sinewave Noise-------------------------------------------##
#Create an empty vector to store the sd
noises_sd_sine <- c()

#Initiate loop to get all results
system.time(for(j in seq_along(noises_r2)){
  a <- optimize(sine_noise, c(0, 6), wanted_R2 = noises_r2[j], samp_size = 10, tol = 0.001)$minimum
  noises_sd_sine[j] <- a
})

final_sine <- data.frame(noises_sd_sine)
save(final_sine, file = "final_sine.Rdata")

##--------------------------------------Final Noise All-------------------------------------------##
final_cross <- c(1, 0.6, 0.3) #Typically used values in simulations for cross relationship
final_noises <- cbind(final_linear, final_exponen, final_quad, final_sine, final_cross) #Get a final matrix with all noises
row.names(final_noises) <- c("High", "Medium", "Low")
colnames(final_noises) <- c('Linear', 'Quadratic', 'Exponential', 'Sine', 'Cross')
final_noises <- t(final_noises)
save(final_noises, file = "final_noises.Rdata") #Save the noises


