#This document is to calculate the bias and variance

#x-value range from 1-30 with increment 1
x <- seq(1,29,1)

#the actual function
real <- function(x){
  y <- ((-(1*10^-5))*x^5 + 0.0006*x^4 - 0.0125*x^3 + 0.11*x^2 - 0.35*x +0.33)
}

#Real function y-values
y = real(x)

#This function generates the y-vales for all Samples
sample.line <- function(x){
  b <- tenSamples[1,1]
  a <- tenSamples[1,2]
  yS <- a*x + b
  return(yS)
}

#Bias component linear lines
linear.regression <- function(x){
    b <- tenSamples[11,1]
    a <- tenSamples[11,2]
    yLR <- a*x +b
    return(yLR)
  }

quad.regression <- function(x){
  a <- twentyfiveSamples[26,1]
  b <- twentyfiveSamples[26,2]
  c <- twentyfiveSamples[26,3]
}

bias <- function(x){
  bias <- (y - linear.regression(x)^2)
  bias.2 <- sum(bias)
  return(bias.2)
}




