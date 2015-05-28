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
  #xSquare <- apply(x,1:2, function(x) x^2)
  xSquare <- x^2
  sumSquares <- sum(xSquare)
  euclDist <- sqrt(sumSquares)
  return(euclDist)
}

#3. distance per row
distRow <- function(x){
  matDist <- apply(x, 1, distance)
  return(matDist)
}

# 4. nearest neighbour -- Hoofdfunctie
NN <- function(n){
  #PSUEDO
  #matrix (met de dimensies) - drawSamples
  #bereken distRow
  #geef which.min(distRow) <- geeft rij aan van de laagste
  # geef matrix[which.min(distRow),]
  matDim <- drawSamples(n)
  distRow <- distRow(matDim)
  #nearest <- matDim[which.min(distRow),] --- geeft de nearest coordinaten
  nearest <- min(distRow)
  return(nearest)
}

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

#5. HET EXPERIMENT
# De functie returned de 1-nearest neighbour van elke dimensie
predictions <- function(dimensions){
  vec <- vector()
  for (i in dimensions){
    vec[i] = NN(i)
  }
  return(vec)
}

plot(dimensions, predictions(dimensions),xlab="Dimensions",ylab="Distance 1-Nearest Neighbour")



