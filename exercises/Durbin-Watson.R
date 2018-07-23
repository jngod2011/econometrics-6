# Erstellen von autokorrelierten Fehlern
n <- 1000
sigmaE <- 2
rho <- 0.99
u <- rep(0, n + 1)
set.seed(01052018)
for(i in 2:(n + 1)) u[i] <- rho*u[i-1] + rnorm(1, 0, sigmaE)
u <- u[-1]
plot(u, type = "l", xlab = expression(t),
     ylab = expression(u[t]))
abline(h = 0)
mean(u)

# Mittelwert recht groß und nicht 0, aber für T=100'000
n <- 100000
sigmaE <- 2
rho <- 0.9
u <- rep(0, n + 1)
set.seed(01052018)
for(i in 2:(n + 1)) u[i] <- rho*u[i-1] + rnorm(1, 0, sigmaE)
u <- u[-1]
plot(u, type = "l", xlab = expression(t),
     ylab = expression(u[t]))
abline(h = 0)
mean(u)
# Erkennbar, dass Erwartungswert von u_t eigentlich Null

# Regressionsmodell Beispiel
n <- 100
sigmaE <- 2
rho <- 0.7
u <- rep(0, n + 1)
set.seed(01052018)
for(i in 2:(n + 1)) u[i] <- rho*u[i-1] + rnorm(1, 0, sigmaE)
u <- u[-1]
plot(u, type = "l", xlab = expression(t),
     ylab = expression(u[t]))
plot(u[-n], u[-1], xlab = expression(u[t-1]),
     ylab = expression(u[t]), asp = 1)
b <- c(5, 0.4)
x <- 1:n
X <- cbind(1, x)
y <- b[1] + b[2]*x + u
plot(x, y)
lm1 <- lm(y~x)
abline(lm1, col = 2)

# Klassisch OLS
summary(lm1)
# 95%-Konfidenzintervalle
5.999378 + c(-1,1)*qt(p = 0.975, df = n - 2)*0.496817
0.399233 + c(-1,1)*qt(p = 0.975, df = n - 2)*0.008541
mean(u)

# Einseitiger Durbin-Watson-Test
uHat <- lm1$residuals
plot(uHat, type = "l")
lines(u, col = 2)
(d <- sum((uHat[-1] - uHat[-n])^2)/sum(uHat^2))

# Regression uHat_t = rho * uHat_{t-1} + epsilon_t
lm(uHat[-1] ~ uHat[-n] - 1)

# Vergleich von Teststatistik und geschätztem rho aus Regression
2*(1 - 0.5412)
d

# Kritische Werte:
# 1.654 und 1.694 --> H0 ablehnen

# GLS Ansatz mit dem (uns hier bekannten) rho:
Omega <- matrix(nrow = n, ncol = n)
for(i in 1:n) for(j in 1:n) Omega[i,j] <- rho^abs(i - j)

(bGLS <- solve(t(X)%*%solve(Omega)%*%X)%*%t(X)%*%solve(Omega)%*%y)
uHatGLS <- c(y - X%*%bGLS)
sigmaEhat <- sqrt(c((uHatGLS%*%solve(Omega)%*%uHatGLS)/(n - 2)))
vGLS <- sigmaEhat^2*solve(t(X)%*%solve(Omega)%*%X)
bGLS[1] + c(-1,1)*qt(0.975, n - 2)*sqrt(diag(vGLS)[1])
bGLS[2] + c(-1,1)*qt(0.975, n - 2)*sqrt(diag(vGLS)[2])

# Mit bekannter Varianz sigmaE
vGLS2 <- sigmaE^2*solve(t(X)%*%solve(Omega)%*%X)
bGLS[1] + c(-1,1)*qnorm(0.975)*sqrt(diag(vGLS2)[1])
bGLS[2] + c(-1,1)*qnorm(0.975)*sqrt(diag(vGLS2)[2])

# Hildreth-Lu Ansatz
rhos <- seq(-0.999, 0.999, by = 0.001)
sigmaEtemp <- numeric(length(rhos))
for(z in 1:length(rhos)){
  r <- rhos[z]
  Otemp <- matrix(nrow = n, ncol = n)
  for(i in 1:n) for(j in 1:n) Otemp[i,j] <- r^abs(i - j)
  btemp <- solve(t(X)%*%solve(Otemp)%*%X)%*%t(X)%*%solve(Otemp)%*%y
  uHattemp <- c(y - X%*%btemp)
  sigmaEtemp[z] <- ((uHattemp%*%solve(Otemp)%*%uHattemp)/(n - 2))*(1 - r^2)
}

plot(rhos, sigmaEtemp, type = "l")

rhoHL <- rhos[which.min(sigmaEtemp)]
OmegaHL <- matrix(nrow = n, ncol = n)
for(i in 1:n) for(j in 1:n) OmegaHL[i,j] <- rhoHL^abs(i - j)
(bHL <- solve(t(X)%*%solve(OmegaHL)%*%X)%*%t(X)%*%solve(OmegaHL)%*%y)
uHatHL <- c(y - X%*%bHL)
sigmaEHL <- c(sqrt((uHatHL%*%solve(OmegaHL)%*%uHatHL)/(n - 2)))
vGLSHL <- sigmaEHL^2*solve(t(X)%*%solve(OmegaHL)%*%X)
bHL[1] + c(-1,1)*qt(0.975, n - 2)*sqrt(diag(vGLSHL)[1])
bHL[2] + c(-1,1)*qt(0.975, n - 2)*sqrt(diag(vGLSHL)[2])

# Cochrane-Orcutt Ansatz
rhoCO <- numeric(20)
rhoCO[1] <- lm(uHat[-1] ~ uHat[-n] - 1)$coef
for(z in 2:20){
  OmegaCO <- matrix(nrow = n, ncol = n)
  for(i in 1:n) for(j in 1:100) OmegaCO[i,j] <- rhoCO[z-1]^abs(i - j)
  bCO <- solve(t(X)%*%solve(OmegaCO)%*%X)%*%t(X)%*%solve(OmegaCO)%*%y
  uHatCO <- c(y - X%*%bCO)
  rhoCO[z] <- lm(uHatCO[-1] ~ uHatCO[-n] - 1)$coef
}
rhoCO
bCO
# Konvergiert recht schnell
sigmaECO <- sqrt(c(uHatCO%*%solve(OmegaCO)%*%uHatCO)/(n - 2))
vCO <- sigmaECO^2*solve(t(X)%*%solve(OmegaCO)%*%X)
bCO[1] + c(-1, 1)*sqrt(vCO[1,1])*qt(0.975, 98)
bCO[2] + c(-1, 1)*sqrt(vCO[2,2])*qt(0.975, 98)

# Newey-West Ansatz
library(sandwich)
vNW <- NeweyWest(x = lm1)
lm1$coef[1] + c(-1, 1)*sqrt(vNW[1,1])*qt(0.975, 98)
lm1$coef[2] + c(-1, 1)*sqrt(vNW[2,2])*qt(0.975, 98)