#dimensions vector
dimensions <- seq(from= 1, to=64, by=1)

#general functions
#parameter a
a <- 5
#1. general function
func <- function(a,x){
  yVector <- sapply(x, function(x) exp(-a*distance(x)))
  return(yVector)
}

#2. distance function (helper function for the general function)
distance <- function(x){
  xSquare <- sapply(x, function(x) x^2)
  sumSquares <- sum(xSquare)
  euclDist <- sqrt(sumSquares)
  return(euclDist)
}

#3. distance per row
#distRow <- function(x){
  
#}

#graph the standard function
#range for graph
range <- seq(-1,1,0.01)
xRange = range
yRange = func(a,xRange)
#plot(xRange, yRange,type="l" , xlim=c(-1,1), ylim=c(0,1), lwd=4, col="blue", xlab="x", ylab="y = exp(-a*||x||)")

#samples
randomSample <- function(){
  x <- runif(1,-1,1)
  return(x)
}

#create samples with different dimension sizes
#create a function n=nr dimensions
drawSamples <- function(n){
  #draw 1000 samples
  samples <- matrix(vector(), nrow=1000, ncol=n)
  for (i in 1:1000){
    dim <- vector(length=n)
    for (j in 1:n){
      dim[j] <- randomSample()
    }
    #create an empty vector with size d (dimension)
    samples[i,] <-  c(dim)
  }
  return(samples)
}

#nearest neighbour
findNN <- function(x){
  NN <- min(x^2)
  return(NN)
}

samples1D = drawSamples(1)
oneDy = func(a,samples1D)
#plot(samples1D, oneDy)





