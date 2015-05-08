#the range that x can take
x <- seq(1,29,0.2)

#a third order function that shows the real value
real <- function(x){
  y <- ((-(1*10^-5))*x^5 + 0.0006*x^4 - 0.0125*x^3 + 0.11*x^2 - 0.35*x +0.33)
  y <- y
}

y = real(x)

plot(x,y, axes=T, ylim=c(0,2), typ ='l', ann=F, lwd=2)
par(tcl= -0.2)
axis(1, at=seq(-15,15, by=1),labels=F,lwd=1,lwd.ticks=1)

title(main="Actual averages", sub="x-seed", ylab="Averages (mu)")

randomPoints <- function(x){
  for (i in 1:20){
    xSample <- sample(x, 20, replace=TRUE, prob=NULL)
    ySample <- real(xSample)
    sampleMean <- c(mean(xSample), mean(ySample))
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

#real function
linear.model = lm(y~x)
abline(linear.model, col="blue", lwd=2)
#linear 10 samples
points(tenSamples)
linear.prediction10 <- lm(tenSamples[,2]~tenSamples[,1])
abline(linear.prediction10, col="red", lwd=2)
#linear 100 samples
points(hundredSamples, col="green")
linear.prediction100 <- lm(hundredSamples[,2]~hundredSamples[,1])
abline(linear.prediction100, col="darkgreen", lwd=2)
#linear 1000 samples
points(thousandSamples, col="yellow")
linear.prediction1000 <- lm(thousandSamples[,2]~thousandSamples[,1])
abline(linear.prediction1000, col="purple", lwd=2)

