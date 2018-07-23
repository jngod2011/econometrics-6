# Stochastische Prozesse
library(cwhtool)

for(i in 1:100) {
  plot(1,type="n",xlim=c(-4,4),ylim=c(-1,1),axes=F,xlab="Rendite",ylab="")
  box()
  axis(1)
  abline(h=0)
  abline(v=0)
  points(rnorm(1),0,lwd=15)
  delayt(0.3)
  }
  
tt <- seq(0,60,length=61)  
for(i in 1:100) {
  x <- 10*exp(c(0,cumsum(rnorm(60)/10)))
  plot(tt,x,type="l",xlim=c(0,60),ylim=c(0,60),xlab="Zeit",ylab="Kurs")
  delayt(0.3)
  }


tt <- seq(0,60,length=61)  
for(i in 1:100) {
  x <- 10*exp(c(0,cumsum(rnorm(60)/10)))
  plot(tt,x,type="l",xlim=c(0,60),ylim=c(0,60),xlab="Zeit",ylab="Kurs")
  abline(v=40,lty="dashed")
  points(40,x[41],lwd=15)
  delayt(0.3)
  }
