# Waterfilter example

# Graphics
preis <- c(24.2,25.5,26.8,26.4,25.2,24.4,26.2,26.1,27.4,28.4,29.8,31.3,32.2,32.4,33.2,34,33.7,32.8,31.3,30.9,30,28.3,27.5,26.8)
absatz <- c(1990,1630,1570,1960,2150,2450,2210,2400,2200,1270,1250,1500,1700,1450,1480,1450,1000,1080,1270,1520,1820,1660,1500,1810)
n <- length(preis)
regr <- lm(absatz~preis)
uhat <- residuals(regr)
plot(preis,absatz,xlab="Price",ylab="Demand",pch=19)
v <- locator(1)
abline(regr)
v <- locator(1)
lines(preis,absatz,type="b")

# Time series of residuals
plot(1:n,uhat,t="b",xlab="Month",ylab="Residual")
abline(h=0)

# Scatterplot
plot(uhat[-n],uhat[-1],xlab="lagged residual",ylab="residual",pch=19)
abline(h=0)
abline(v=0)

# Autokorrelation of residuals
print(summary(lm(uhat[-1]~uhat[-n])))

# Hildreth-Lu
X <- cbind(1,preis)
Y <- absatz
z <- matrix(NA,199,2)
rho0 <- seq(-0.99,0.99,length=199)
for(i in 1:199) {
  P <- diag(n)
  P[1,1] <- sqrt(1-rho0[i]^2)
  for(j in 2:n) P[j,(j-1)] <- -rho0[i]
  P <- P/sqrt(1-rho0[i]^2)
  Ystar <- P%*%Y
  Xstar <- P%*%X
  regr <- lm(Ystar~Xstar-1)
  uhatstar <- residuals(regr)
  z[i,] <- c(rho0[i],sum(uhatstar^2)*(1-rho0[i]^2))
  }
