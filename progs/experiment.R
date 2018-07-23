library(MASS)
library(grDevices)
graphics.off()

# If you use R on a Windows system
if (Sys.info()["sysname"] == "Windows") {
  windows(7,6,xpos=360,ypos=10)
  windows(3,2.6,xpos=10,ypos=10)
  windows(3,2.6,xpos=10,ypos=330)
}
# If you use R on a Linux system
if (Sys.info()["sysname"] == "Linux") {
  x11(width=7,height = 6,xpos=360,ypos=10)
  x11(width=3,height=2.6,xpos=10,ypos=10)
  x11(width=3,height=2.6,xpos=10,ypos=330)
}
# If you use R on a Mac OS
if (Sys.info()["sysname"] == "Darwin") {
  quartz(width=7,height=6)
  quartz(width=3,height=2.6)
  quartz(width=3,height=2.6)
}


# Illustration for the estimated regression line

x <- c(10,30,50,25,7.50,42.50,35,40,25,12.50,60,47.50,45,27.50,15,20,47.50,32.50,37.50,20)
y <- c(2,3,7,2,2.5,6,5,4,6,1,7,5.5,7,4.5,1.5,4,9,3,6.5,2.5)
alpha <- 0.5
beta <- 0.1
alphadach <- rep(NA,1000)
betadach <- rep(NA,1000)

# Actually observed sample

dev.set(2)
par(xaxs="i",yaxs="i",mar=c(4.2,4,1,1))
plot(x,y,ylim=c(0,9),xlim=c(0,65),pch=19,xlab="Billing amount x",ylab="Gratuity y")
abline(alpha,beta,col="red",lwd=2)
obj <- lm(y~x)
alphadach[1] <- obj$coef[1]
betadach[1] <- obj$coef[2]
abline(obj)
dev.set(3)
par(mar=c(4,4,1,1))
truehist(alphadach[1],xlim=c(-2.5,3),ylim=c(0,150),breaks=seq(-2.5,3,length=21),prob=F,main="",xlab=expression(hat(alpha)),ylab="frequency")
dev.set(4)
par(mar=c(4,4,1,1))
truehist(betadach[1],xlim=c(0.02,0.16),ylim=c(0,150),breaks=seq(0.02,0.18,length=21),prob=F,main="",xlab=expression(hat(beta)),ylab="frequency")
dev.set(2)
locator(1)

for (i in 2:1000) {
  u <- rnorm(20)*sqrt(2)
  y <- alpha+beta*x+u
  
  obj <- lm(y~x)
  alphadach[i] <- obj$coef[1]
  betadach[i] <- obj$coef[2]
  
  dev.set(2)
  par(xaxs="i",yaxs="i",mar=c(4.2,4,1,1))
  plot(x,y,ylim=c(0,9),xlim=c(0,65),pch=19,xlab="Billing amount x",ylab="Gratuity y")
  abline(alpha,beta,col="red",lwd=2)
  abline(obj)
  
  if(i<10) locator(1)
  if(i>0) 
    {
    dev.set(3)
    par(mar=c(4,4,1,1))
    alphadach[alphadach < -2.5] <- NA
    alphadach[alphadach > 3] <- NA
    truehist(alphadach[1:i],xlim=c(-2.5,3),ylim=c(0,150),breaks=seq(-2.5,3,length=21),prob=F,main="",xlab=expression(hat(alpha)),ylab="frequency")
    }
  if(i>2)
    {
    dev.set(4)
    par(mar=c(4,4,1,1))
    betadach[betadach < 0.02] <- NA
    betadach[betadach > 0.16] <- NA
    truehist(betadach[1:i],xlim=c(0.02,0.16),ylim=c(0,150),breaks=seq(0.02,0.18,length=21),prob=F,main="",xlab=expression(hat(beta)),ylab="frequency")
    }
  }


# Distribution of the estimators

dev.set(3)
locator(1)
abline(v=0.5,col="red",lwd=5)
locator(1)
g <- seq(-2.5,3,length=150)
lines(g,1000*0.275*dnorm(g,mean=mean(alphadach,na.rm=T),sd=sd(alphadach,na.rm=T)),lwd=3)

dev.set(4)
locator(1)
abline(v=0.1,col="red",lwd=5)
locator(1)
g <- seq(0.02,0.18,length=150)
lines(g,1000*0.008*dnorm(g,mean=mean(betadach,na.rm=T),sd=sd(betadach,na.rm=T)),lwd=3)
