# Motivation for OLS

library(MASS)
set.seed(1234)

x <- c(10,30,50,25,7.50,42.50,35,40,25,12.50,60,47.50,45,27.50,15,20,47.50,32.50,37.50,20)
y <- c(2,3,7,2,2.5,6,5,4,6,1,7,5.5,7,4.5,1.5,4,9,3,6.5,2.5)

plot(x,y,pch=19,xlim=c(0,60),ylim=c(0,10),xlab="Billing amount x",ylab="Tip y")
locator(1)

for(i in 1:5){
  c1 <- mvrnorm(1,c(0.5,0.12),matrix(c(0.6,-0.018,-0.018,0.0008),2,2))
  abline(a=c1[1],b=c1[2])
  locator(1)
}

plot(x,y,pch=19,xlim=c(0,60),ylim=c(0,10),xlab="Billing amount x",ylab="Tip y")
c1 <- mvrnorm(1,c(0.5,0.12),matrix(c(0.6,-0.018,-0.018,0.0008),2,2))
abline(a=c1[1],b=c1[2])
locator(1)

for(i in 1:20){
  lines(c(x[i],x[i]),c(c1[1]+c1[2]*x[i],y[i]))
  points(x[i],c1[1]+c1[2]*x[i],pch=19,col="red")
}
locator(1)

# Least Squares solution
Sxy <- sum((x-mean(x))*(y-mean(y)))
Sxx <- sum((x-mean(x))^2)
betahat <- Sxy/Sxx
alphahat <- mean(y)-betahat*mean(x)
plot(x,y,pch=19,xlim=c(0,60),ylim=c(0,10),xlab="Billing amount x",ylab="Tip y")
abline(a=alphahat,b=betahat)
for(i in 1:20){
  lines(c(x[i],x[i]),c(alphahat+betahat*x[i],y[i]))
  points(x[i],alphahat+betahat*x[i],pch=19,col="red")
}
(alphahat)
(betahat)

lm(y~x)
summary(lm(y~x))
