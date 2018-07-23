# Prognose-Intervalle

library(MASS)
set.seed(1234)

x <- c(10,30,50,25,7.50,42.50,35,40,25,12.50,60,47.50,45,27.50,15,20,47.50,32.50,37.50,20)
y <- c(2,3,7,2,2.5,6,5,4,6,1,7,5.5,7,4.5,1.5,4,9,3,6.5,2.5)

TT <- length(x)
Sxx <- sum((x-mean(x))^2)

plot(x,y,xlim=c(0,60),ylim=c(0,10),xlab="Billing amount x",ylab="Tip y")
r0 <- lm(y~x)
abline(r0)
a1 <- coefficients(r0)[1]
b1 <- coefficients(r0)[2]
s1 <- summary(r0)$sigma

x0 <- seq(0,60,length=61)
Vy0 <- s1^2*(1+1/TT+(x0-mean(x))^2/Sxx)
lines(a1+b1*x0+qt(0.975,TT-2)*sqrt(Vy0))
lines(a1+b1*x0-qt(0.975,TT-2)*sqrt(Vy0))
