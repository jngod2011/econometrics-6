# Illustration of the distribution of the OLS
# estimators in case of non-normal disturbances

library(MASS)

N <- 10000 # number of simulation runs
adach <- rep(NA,N)
bdach <- rep(NA,N)

uu <- function(n) return(rexp(n,rate=1)-1) # shifted exponential distribution
#uu <- function(n) return((runif(n)<0.5)*2-1) # two point distribution on -1 and +1
#uu <- function(n) return(rt(n,df=3)) # t-distribution with df=3 degrees of freedom

n <- 100
x <- c(10,30,50,25,7.50,42.50,35,40,25,12.50,60,47.50,45,27.50,15,20,47.50,32.50,37.50,20)
if(n>20) x <- c(x,(runif(n-20)*55+5))

for(i in 1:N) {

  u <- uu(n)
  y <- 0.5+0.1*x+u
  
  Sxy <- sum((x-mean(x))*(y-mean(y)))
  Sxx <- sum((x-mean(x))^2)
  
  bdach[i] <- Sxy/Sxx
  adach[i] <- mean(y)-bdach[i]*mean(x)
  }  

u <- uu(10000)
truehist(u,main="Distribution of disturbances",xlim=c(-max(u),max(u)))
abline(v=0,col="red",lwd=3)
g <- seq(-max(u),max(u),length=300)
lines(g,dnorm(g,mean=mean(u),sd=sd(u)))
aux <- locator(1)

windows(10,6)
par(mfrow=c(1,2))

truehist(adach,main="Distribution of alpha-hat")
g <- seq(min(adach),max(adach),length=300)
lines(g,dnorm(g,mean=mean(adach),sd=sd(adach)),lwd=4)

truehist(bdach,main="Distribution of beta-hat")
g <- seq(min(bdach),max(bdach),length=300)
lines(g,dnorm(g,mean=mean(bdach),sd=sd(bdach)),lwd=4)
