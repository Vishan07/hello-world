#This document is to calculate the bias and variance

#x-value range from 1-30 with increment 1
xBV <- seq(1,29,1)

#the actual function
real <- function(xBV){
  y <- ((-(1*10^-5))*xBV^5 + 0.0006*xBV^4 - 0.0125*xBV^3 + 0.11*xBV^2 - 0.35*xBV +0.33)
}

#Real function y-values
y = real(xBV)

#This function generates the y-vales for all Samples
linear.regression <- function(xBV,n){
  if (n==10){
    b <- tenSamples[11,1]
    a <- tenSamples[11,2]
  }
  if (n==25){
    b <- twentyfiveSamples[26,1]
    a <- twentyfiveSamples[26,2]
  }
  if (n==250){
    b <- twofiftySamples[251,1]
    a <- twofiftySamples[251,2]
  }
  yR <- a* xBV + b
  return(yR)
}

var10l <- var(linear.regression(xBV,10))
var25l <- var(linear.regression(xBV,25))
var250l <- var(linear.regression(xBV,250))

#Bias component linear lines
linear.regression10 <- function(xBV){
    b <- tenSamples[11,1]
    a <- tenSamples[11,2]
    yLR <- a*xBV +b
    return(yLR)
  }

linear.regression25 <- function(xBV){
  b <- twentyfiveSamples[26,1]
  a <- twentyfiveSamples[26,2]
  yLR <- a*xBV +b
  return(yLR)
}

linear.regression250 <- function(xBV){
  b <- twofiftySamples[251,1]
  a <- twofiftySamples[251,2]
  yLR <- a*xBV +b
  return(yLR)
}

#Bias component quad lines ####
quad.regression <- function(xBV){
  c <- twentyfiveSamples[26,1]
  a <- twentyfiveSamples[26,2]
  b <- twentyfiveSamples[26,3]
  yQR <- (a*x)^2 + b*xBV + c
  return(yQR)
}

#functions for the linear biases
biasLinear10 <- function(xBV){
  bias <- (y - linear.regression10(xBV))^2
  bias.2 <- sum(bias)/10
  return(bias.2)
}

biasLinear25 <- function(xBV){
  bias <- (y - linear.regression25(xBV))^2
  bias.2 <- sum(bias)/25
  return(bias.2)
}

biasLinear250 <- function(xBV){
  bias <- (y - linear.regression250(xBV))^2
  bias.2 <- sum(bias)/250
  return(bias.2)
}

#hier gaat iets fout.. 
biasQuad <- function(xBV){
  bias <- (y-quad.regression(x))^2
  bias.2 <- sum(bias)
  return(bias.2)
}




