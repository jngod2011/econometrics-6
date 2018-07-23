# Intervallschätzung

library(MASS)
par(mfrow=c(2,1))

# Illustration zur Schätzgeraden

x <- c(10,30,50,25,7.50,42.50,35,40,25,12.50,60,47.50,45,27.50,15,20,47.50,32.50,37.50,20)
Sxx <- sum((x-mean(x))^2)
alpha <- 0.5
beta <- 0.1
sigma2 <- 2
beta.lower <- rep(NA,100)
beta.upper <- rep(NA,100)

for (i in 1:100) {
  u <- rnorm(20)*sqrt(sigma2)
  y <- alpha+beta*x+u
  obj <- lm(y~x)
  uhat <- obj$residuals
  beta.lower[i] <- obj$coef[2]-1.96*sqrt(sigma2/Sxx)
  beta.upper[i] <- obj$coef[2]+1.96*sqrt(sigma2/Sxx)
  if(i<10 | i==100) {
    plot(x,y,ylim=c(0,9),xlim=c(0,65),pch=19,xlab="Rechnungsbetrag x",ylab="Trinkgeld y",main="Stichprobe")
    abline(obj)
    plot(c(beta.lower[1],beta.upper[1]),c(1,1),xlim=c(-0.1,0.3),ylim=c(0,100),type="l",main="0.95-Intervall")
    abline(v=beta,col="red",lwd=3)
    for(j in 1:i) lines(c(beta.lower[j],beta.upper[j]),c(j,j))
    vvv <- locator(1)
  }
 }
  
print(sum(beta.lower<beta & beta.upper>beta)/100)
