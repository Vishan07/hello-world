
#the range that x can take
x <- seq(0,5,0.01)

#a third order function that shows the real value
real <- function(x){
  y <- 9*x^2 -50*x +50
  y <- y
}

y = real(x)
plot(x,y, axes=T, ylim=c(-20,50), typ ='l', ann=F, lwd=2)
par(tcl= -0.2)
axis(1, at=seq(-15,15, by=1),labels=F,lwd=1,lwd.ticks=1)

title(main="Actual averages", sub="x-seed", ylab="Averages (mu)")

linear.model = lm(y~x)
abline(linear.model, col="blue")

#10 samples
#for (i in 1:10){
  #Takes a sample (red dots)
#  s <- sample(x, 10, replace=FALSE, prob= NULL)
#  rSample <- real(s)
#  points(s, rSample, col="red")
#  linear2 = lm(rSample~s)
#  abline(linear2, col="purple")
#}

#quadratic is yellow
q <- sample(x, 10, replace=FALSE, prob=NULL) #this is in essence x
qSample <- real(q) #is in essentie y
points(q, qSample, col="black")
quad <- lm(qSample ~ poly(q,3,raw=FALSE))
#lines(cubic, col="darkgreen"
yellowQuad <- lines(predict(quad), col="yellow", lwd=2)

#cubic line is red
c <- sample(x, 10, replace=FALSE, prob=NULL)
cSample <- real(c) #is in essentie x
points(c, cSample, col="green")
cubic <- lm(cSample ~ poly(c,3,raw=FALSE))
#lines(cubic, col="darkgreen"
redCubic <- lines(predict(cubic), col="red", lwd=2)

#smooth <- scatter.smooth(c, cSample, span=2/3, degree = 1, family=c("symmetric", "gaussian"))
#lines(c, smooth, col="darkgreen")
