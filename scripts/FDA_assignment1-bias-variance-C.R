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
cub.regression <- function(xBV,n){
  if (n==10){
    d <- tenSamples[11,1]
    c <- tenSamples[11,2]
    b <- tenSamples[11,3]
    a <- tenSamples[11,4]
  }
  if (n==25){
    d <- twentyfiveSamples[26,1]
    c <- twentyfiveSamples[26,2]
    b <- twentyfiveSamples[26,3]
    a <- twentyfiveSamples[26,4]
  }
  if (n==250){
    d <- twofiftySamples[251,1]
    c <- twofiftySamples[251,2]
    b <- twofiftySamples[251,3]
    a <- twofiftySamples[251,4]
  }
  yR <- a * (xBV^3) + b * (xBV^2) + c*xBV +d
  yR <- yR/n
  return(yR)
}

var10c <- var(cub.regression(xBV,10))
var25c <- var(cub.regression(xBV,25))
var250c <- var(cub.regression(xBV,250))

#Bias component quad lines
biasCub10 <- function(xBV){
  bias <- (y-cub.regression(xBV,10))^2
  bias.2 <- sum(bias)/10
  return(bias.2)
}
biasCub25 <- function(xBV){
  bias <- (y-cub.regression(xBV,25))^2
  bias.2 <- sum(bias)/25
  return(bias.2)
}
biasCub250 <- function(xBV){
  bias <- (y-cub.regression(xBV,250))^2
  bias.2 <- sum(bias)/250
  return(bias.2)
}




