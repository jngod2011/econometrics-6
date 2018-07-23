# Illustration zum F-Test

library(MASS)
library(hdrcde)

R <- 500
x <- cbind(1,mvrnorm(n=30,c(1,2),matrix(c(1,0.8,0.8,1),2,2)))
beta0 <- c(3,0,0)

z <- matrix(NA,R,2)
for(i in 1:R) {
  y <- x%*%beta0+rnorm(30)
  betahat <- solve(t(x)%*%x)%*%t(x)%*%y
  z[i,] <- betahat[2:3]
  }

plot(z,xlab=expression(hat(beta)[1]),ylab=expression(hat(beta)[2]),ylim=c(-1.2,1.2),xlim=c(-1.2,1.2))
abline(h=0)
abline(v=0)

