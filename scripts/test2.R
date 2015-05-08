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

