#dimensions vector, dimensions run from 1-32, with interval 1
dimensions <- seq(from= 1, to=32, by=1)

#general functions
#parameter a - chosen for its output in the regular function
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

#3. distance per row - takes the samples matrix and returns the distance per sample
distRow <- function(x){
  matDist <- apply(x, 1, distance)
  return(matDist)
}

# 4. nearest neighbour -- Hoofdfunctie
NN <- function(n){
  #create matrix with n dimensions
  matDim <- drawSamples(n)
  distRow <- distRow(matDim)
  #nearest <- matDim[which.min(distRow),] --- geeft de nearest coordinaten
  nearest <- min(distRow)
  return(nearest)
}

#graph the standard function (zie firstPlot.jpeg)
#range for graph
range <- seq(-1,1,0.01)
xRange = range
yRange = func(a,xRange)


#draw samples function
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

#voor de sensitivity
repeatExp <- function(x){
  vec <- vector()
  for (i in 1:100){
    vec[i] <- NN(x)
  }
  return(vec)
}

#Geeft de y waarden van de Nearest Neighbours per dimensie
predictions2 <- function(dimensions) {
  testvar <- predictions(dimensions)
  vec <- vector()
  for (i in dimensions) {
    vec[i] <- exp(-5 * NN(i))
  }
  return(vec)
}

#outputs the basic function
#plot(xRange, yRange,type="l" , xlim=c(-1,1), ylim=c(0,1), lwd=4, col="blue", xlab="x", ylab="y = exp(-a*||x||)")

#Outputs the distance of the 1-Nearest Neighbour per dimension
#plot(dimensions, predictions(dimensions),xlab="Dimensions",ylab="Distance 1-Nearest Neighbour")

#Puntje 3 van de opdracht
#Geeft de plot waar de output het verschil is tussen de voorspelde waarde en de eigenlijke y
plot(dimensions, 1-predictions2(dimensions),xlab="Dimensions",ylab="Distance from Y0 (delta Y)", col="red", lwd=2)
