# Lineare Regression bei I(0)- und I(1)-Prozessen

library(lmtest)
library(MASS)

# Standard-t-Test bei I(0)-Prozessen
N <- 1000
pv <- rep(NA,N)
for(i in 1:N) {
  TT <- 20
  x <- rnorm(TT)
  y <- rnorm(TT)
  pv[i] <- coeftest(lm(y~x))[2,4]
  }
print("Fehlerwahrscheinlichkeit erster Art:")
print(sum(pv<0.05)/N)

# Standard-t-Test bei I(1)-Prozessen
N <- 1000
pv <- rep(NA,N)
for(i in 1:N) {
  TT <- 20
  x <- cumsum(rnorm(TT))
  y <- cumsum(rnorm(TT))
  pv[i] <- coeftest(lm(y~x))[2,4]
  }
print("Fehlerwahrscheinlichkeit erster Art:")
print(sum(pv<0.05)/N)
