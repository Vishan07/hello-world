#x-value range increments with 0.2
x <- seq(1,29,0.2)

#the actual function
real <- function(x){
  y <- ((-(1*10^-5))*x^5 + 0.0006*x^4 - 0.0125*x^3 + 0.11*x^2 - 0.35*x +0.33)
  y <- y
}

y = real(x)

plot(x,y, axes=T, ylim=c(-0.5,2), typ ='l', ann=F, lwd=3)
par(tcl= -0.3, bg= "aliceblue")
axis(1, at=seq(0,30, by=1),labels=F,lwd=1,lwd.ticks=1)

title(main="Sample Averages", sub="X-values", ylab="Averages (mu)")

#real function
real.function = lm(y~x)
abline(real.function, col="black", lwd=5)

xAll = vector()
yAll = vector()

oneSample <- function(x,n){
  for (i in 1:n){
    xSample <- sample(x, 20, replace=TRUE, prob = NULL)    
    
    #order samples (otherwise messy lines)
    xSample <- sort(xSample, decreasing=FALSE)
    ySample <- real(xSample)
    
    #points(xSample,ySample) is a test if the points are actually on the line
    points(xSample,ySample)
    linear.model = lm(ySample~xSample)
    #abline(linear.model, col="red", lwd=1)
    
    fit2 <- lm(ySample~xSample + I(xSample^2))
    #show the individual lines of the second order regression lines
    #points(xSample, predict(fit2), type="l", col="#FF0A85", lwd=1)
    
    fit3 <- lm(ySample~xSample + I(xSample^2) + I(xSample^3))
    #show the individual lines of the third order regression lines
    #points(xSample,predict(fit3), type="l", col="#66FF33", lwd=1)
    
    #Defines all the x-values from the samples and sorts them for smooth lines
    xAll = c(xAll, xSample)
    xAll = sort(xAll)
    yAll = real(xAll)
  }
  #Linear regression lines
  avgReg = lm(yAll~xAll)
  if (n==10) {abline(avgReg, col="darkgreen", lwd=5)}
  if (n ==100) {abline(avgReg, col="darkgreen", lwd=5)}
  if (n == 1000) {abline(avgReg, col="darkgreen", lwd=5)}
  #quadratic regression lines
  xAll2 = xAll^2
  avgReg2 = lm(yAll~xAll + I(xAll2))
  if (n==10) {lines(xAll, predict(avgReg2), col="#0A47FF", lwd=5)}
  if (n ==100) {lines(xAll, predict(avgReg2), col="#0A47FF", lwd=5)}
  if (n == 1000) {lines(xAll, predict(avgReg2), col="#0A47FF", lwd=5)}
  #cubic regression lines
  xAll2 = xAll^2
  xAll3 = xAll^3
  avgReg3 = lm(yAll~xAll + I(xAll2) + I(xAll3))
  if (n==10) {lines(xAll, predict(avgReg3), col="darkorange", lwd=5)}
  if (n ==100) {lines(xAll, predict(avgReg3), col="darkorange", lwd=5)}
  if (n == 1000) {lines(xAll, predict(avgReg3), col="darkorange", lwd=5)}
}

#run the function
tenSamples = oneSample(x,10)
#hundredSamples = oneSample(x,100)
#thousandSamples = oneSample(x,1000)
