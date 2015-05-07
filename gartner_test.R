#the range that x can take
x <- seq(1,29,0.2)

#a third order function that shows the real value
real <- function(x){
  y <- ((-(1*10^-5))*x^5 + 0.0006*x^4 - 0.0125*x^3 + 0.11*x^2 - 0.35*x +0.33)
  y <- y
}

y = real(x)


randomPoints <- function(x){
  for (i in 1:20){
    xSample <- sample(x, 20, replace=TRUE, prob=NULL)
    ySample <- real(xSample)
    sampleMean <- c(mean(xSample), mean(ySample))
    mat[i,] <- sampleMean
    return(sampleMean)
  }
}

drawSamples <- function(x,n){
  mat <- matrix(, nrow=n, ncol=2)
  for (i in 1:n){
    mat[i,] <- randomPoints(x)
  }
  return(mat)
}

tenSamples <- drawSamples(x,10)
hundredSamples <- drawSamples(x,100)
thousandSamples <- drawSamples(x,1000)
