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
quad.regression <- function(xBV,n){
  if (n==10){
    c <- tenSamples[11,1]
    b <- tenSamples[11,2]
    a <- tenSamples[11,3]
  }
  if (n==25){
    c <- twentyfiveSamples[26,1]
    b <- twentyfiveSamples[26,2]
    a <- twentyfiveSamples[26,3]
  }
  if (n==250){
    c <- twofiftySamples[251,1]
    b <- twofiftySamples[251,2]
    a <- twofiftySamples[251,3]
  }
  yR <- (a* xBV)^2 + b*xBV +c
  yR <- yR/n
  return(yR)
}

var10q <- var(quad.regression(xBV,10))
var25q <- var(quad.regression(xBV,25))
var250q <- var(quad.regression(xBV,250))

#Bias component quad lines
biasQuad10 <- function(xBV){
  bias <- (y-quad.regression(xBV,10))^2
  bias.2 <- sum(bias)/10
  return(bias.2)
}
biasQuad25 <- function(xBV){
  bias <- (y-quad.regression(xBV,25))^2
  bias.2 <- sum(bias)/25
  return(bias.2)
}
biasQuad250 <- function(xBV){
  bias <- (y-quad.regression(xBV,250))^2
  bias.2 <- sum(bias)/250
  return(bias.2)
}





