setwd("C:/Users/stapperm/Documents/Econometrics 1/Code")

f <- function(n, name, pch = 19){
  x11()
  par(mar = c(5,5.2,4,1) + 0.1)
  plot(0, type = "n", xlim = c(0, 10), ylim = c(0, 10), cex.axis = 2, cex.lab = 2,
       xlab = expression(x[t]), ylab = expression(y[t]))
  x <- matrix(nrow = n, ncol = 2)
  for(i in 1:n){
    temp <- locator(n = 1)
    points(temp$x, temp$y, cex = 2, pch = pch)
    x[i,] <- c(temp$x, temp$y)
  }
  lm1 <- lm.fit(x = cbind(1, x[,1]), y = x[,2])
  
  jpeg(filename = paste(name, ".jpeg", sep = ""), width = 600, height = 600)
  par(mar = c(5,5,4,1) + 0.1)
  plot(0, type = "n", xlim = c(0, 10), ylim = c(0, 10), cex.axis = 2, cex.lab = 2,
       xlab = expression(x[t]), ylab = expression(y[t]))
  points(x, pch = pch, cex = 2)
  abline(lm1, col = 2, lwd = 2, lty = 2)
  dev.off()
  
  jpeg(filename = paste(name, "_res.jpeg", sep = ""))
  par(mar = c(5,5.2,4,1) + 0.1)
  plot(0, type = "n", xlim = c(0, 10), ylim = range(lm1$residuals), cex.axis = 2, cex.lab = 2,
       xlab = expression(x[t]), ylab = expression(y[t] - hat(y)[t]))
  points(x[,1], lm1$residuals, cex = 2, pch = pch)
  abline(h = 0, lwd = 2, col = "gray", lty = 2)
  dev.off()
  return(x)
}

f(10, "A1")
f(20, "A2")
f(25, "A3")

# For B1 measuring error:
x11()
par(mar = c(5,5.2,4,1) + 0.1)
plot(0, type = "n", xlim = c(0, 10), ylim = c(0, 10), cex.axis = 2, cex.lab = 2,
     xlab = expression(x[t]), ylab = expression(y[t]))
x <- x2 <- matrix(nrow = 15, ncol = 2)
for(i in 1:15){
  temp <- locator(n = 1)
  points(temp$x, temp$y, cex = 2, pch = 19)
  points(temp$x, temp$y + 2, cex = 2)
  x[i,] <- c(temp$x, temp$y)
  x2[i,] <- c(temp$x, temp$y + 2)
}
lm1 <- lm.fit(x = cbind(1, x[,1]), y = x[,2])
lm2 <- lm.fit(x = cbind(1, x2[,1]), y = x2[,2])

jpeg(filename = "B1.jpeg", width = 600, height = 600)
par(mar = c(5,5,4,1) + 0.1)
plot(0, type = "n", xlim = c(0, 10), ylim = c(0, 10), cex.axis = 2, cex.lab = 2,
     xlab = expression(x[t]), ylab = expression(y[t]))
points(x, pch = 19, cex = 2)
points(x2, cex = 2)
abline(lm1, col = 2, lwd = 2, lty = 2)
dev.off()

jpeg(filename = "B1_res.jpeg")
par(mar = c(5,5.2,4,1) + 0.1)
plot(0, type = "n", xlim = c(0, 10), ylim = range(c(lm1$residuals, x[,2] - lm2$fitt)),
     cex.axis = 2, cex.lab = 2,
     xlab = expression(x[t]), ylab = expression(y[t] - hat(y)[t]))
points(x[,1], lm1$residuals, cex = 2, pch = 19)
points(x2[,1], x[,2] - lm2$fitt, cex = 2)
abline(h = 0, lwd = 2, col = "gray", lty = 2)
dev.off()

f(20, "B2")
f(20, "B3")
f(70, "B4", pch = 1)
# C1 no drawable counterexample

set.seed(123)
ys <- rnorm(25, 5, 2)
jpeg("C2_plot.jpeg", width = 600, height = 600)
par(mar = c(5,5.2,4,1) + 0.1)
plot(0, type = "n", xlim = c(0, 10), ylim = c(0, 10), cex.axis = 2, cex.lab = 2,
     xlab = expression(x[t]), ylab = expression(y[t]))
points(rep(4, length(ys)), ys, pch = 1, cex = 2)
dev.off()

jpeg("C2.jpeg", width = 600, height = 600)
par(mar = c(5,5.2,4,1) + 0.1)
plot(0, type = "n", xlim = c(0, 10), ylim = c(0, 10), cex.axis = 2, cex.lab = 2,
     xlab = expression(x[t]), ylab = expression(y[t]))
points(rep(4, length(ys)), ys, pch = 1, cex = 2)
for(i in seq(0, 10, by = 2)){
  abline(a = i, b = (mean(ys) - i)/4, col = 2, lwd = 2, lty = 2)
}
dev.off()


# For LAD-Regression

x <- f(n = 15, name = "LAD")
lm1 <- lm.fit(x = cbind(1, x[,1]), y = x[,2])
coef2 <- optim(par = lm1$coef, fn = function(theta){
  sum(abs(x[,2] - cbind(1, x[,1])%*%theta))
})$par

jpeg("LADreg.jpeg", width = 600, height = 600)
par(mar = c(5,5,4,1) + 0.1)
plot(0, type = "n", xlim = c(0, 10), ylim = c(0, 10), cex.axis = 2, cex.lab = 2,
     xlab = expression(x[t]), ylab = expression(y[t]))
points(x, pch = 19, cex = 2)
abline(lm1, col = 2, lty = 2, lwd = 2)
abline(coef2, col = 4, lty = 3, lwd = 2)
legend("topleft", legend = c("OLS", "LAD"), col = c(2,4), lty = 2:3, lwd = 2, pt.cex = 1, cex = 2)
dev.off()

sum(x[,2])
sum(x[,2]^2)
sum(lm1$fitt^2)
fitt2 <- coef2[1] + coef2[2]*x[,1]
sum(fitt2^2)
x[,2]%*%fitt2
sum(lm1$fitt^2)



A <- matrix(c(1,3,2,-1,0,1,2,1,0), ncol = 3)
B <- matrix(c(1,2,3), ncol = 1)
C <- matrix(c(2,7,0,0,-1,1,3,2,1), ncol = 3)
D <- matrix(c(2,-2,0,3,1,5), ncol = 2)
E <- matrix(c(0,2,1), ncol = 3)

A%*%B
C%*%D
E%*%A
B%*%E
E%*%D


t(A+C)
t(A%*%C)
A <- matrix(c(1,2,-1,1), ncol = 2)
B <- matrix(c(0,1,-2,-3), ncol = 2)
as.fractions(solve(A))
solve(B)
as.fractions(solve(A%*%B))


Rols <- (188.77 - 50.67^2/15)/(219.53 - 50.67^2/15)
1 - (1-Rols)*14/13

Rlad <- (148.48 - 2*50.67/15*44.16 + 50.67^2/15)/(219.53 - 50.67^2/15)
1 - (1-Rlad)*14/13


# Negative R^2
f2 <- function(n, name, pch = 19){
  x11()
  par(mar = c(5,5.2,4,1) + 0.1)
  plot(0, type = "n", xlim = c(0, 10), ylim = c(0, 10), cex.axis = 2, cex.lab = 2,
       xlab = expression(x[t]), ylab = expression(y[t]))
  x <- matrix(nrow = n, ncol = 2)
  for(i in 1:n){
    temp <- locator(n = 1)
    points(temp$x, temp$y, cex = 2, pch = pch)
    x[i,] <- c(temp$x, temp$y)
  }
  lm1 <- lm.fit(x = cbind(1, x[,1]), y = x[,2])
  
  text(x = 5, y = 9, labels = paste(round(summary(lm(x[,2] ~ x[,1]))$adj.r.squared, digits = 3)), col = 2, cex = 2)
  
  jpeg(filename = paste(name, ".jpeg", sep = ""), width = 600, height = 600)
  par(mar = c(5,5,4,1) + 0.1)
  plot(0, type = "n", xlim = c(0, 10), ylim = c(0, 10), cex.axis = 2, cex.lab = 2,
       xlab = expression(x[t]), ylab = expression(y[t]))
  points(x, pch = pch, cex = 2)
  abline(lm1, col = 2, lwd = 2, lty = 2)
  dev.off()
  
  res <- c(sum(x[,2]),sum(x[,2]^2), sum(x[,2]*lm1$fitt), sum(lm1$fitt^2))
  return(res)
}


setwd("C:/Users/stapperm/Documents/Oberseminar")
library(robustbase)
rhohuber <- function(x, cc) 0.5*x^2*(abs(x) <= cc) + (cc*abs(x) - 0.5*cc^2)*(abs(x) > cc)
x <- seq(-10,10, length.out = 1001)

jpeg(filename = "Mestim.jpeg", width = 1200, height = 800)
par(mfrow = c(2,3), mar = c(5,5,4,1) + 0.1)
plot(x,rhohuber(x = x, cc = 4) , type = "l", xlab = "x", lwd = 2, cex.axis = 1.2,
     cex.lab = 1.2, main = expression(rho(x)), cex.main = 3, ylab = "")
mtext(text = "Huber", side = 2, line = 2, cex = 3)
curve(x^2/2, add = T, col = "gray", lty = 2, lwd = 2)
abline(v = c(-4, 4), col = 2, lty = 2, lwd = 2)
plot(x, Mpsi(x = x, cc = 4, psi = "huber", deriv = 0), type = "l", xlab = "x", lwd = 2, ylab = "",
     cex.lab = 1.2, cex.axis = 1.2, main = expression(psi(x)), cex.main = 3)
abline(v = c(-4, 4), col = 2, lwd = 2, lty = 2)
plot(x, Mwgt(x = x, cc = 4, psi = "huber"), type = "l", xlab = "x", lwd = 2,
     cex.lab = 1.2, cex.axis = 1.2, main = expression(w(x)), ylab = "", cex.main = 3)
abline(v = c(-4, 4), col = 2, lwd = 2, lty = 2)

plot(x, Mpsi(x = x, cc = 4, psi = "bisquare", deriv = -1), type = "l", xlab = "x", lwd = 2, ylab = "",
     cex.lab = 1.2, cex.axis = 1.2)
mtext(text = "Tukey", side = 2, line = 2, cex = 3)
abline(v = c(-4, 4), col = 2, lwd = 2, lty = 2)
plot(x, Mpsi(x = x, cc = 4, psi = "bisquare", deriv = 0), type = "l", xlab = "x", lwd = 2, ylab = "",
     cex.lab = 1.2, cex.axis = 1.2)
abline(v = c(-4, 4), col = 2, lwd = 2, lty = 2)
plot(x, Mwgt(x = x, cc = 4, psi = "bisquare"), type = "l", xlab = "x", lwd = 2, ylab = "",
     cex.lab = 1.2, cex.axis = 1.2)
abline(v = c(-4, 4), col = 2, lwd = 2, lty = 2)
dev.off()

plot(x, Mpsi(x = x, cc = 4, psi = "bisquare", deriv = 1), type = "l", xlab = "x", ylab = expression(psi(x)), lwd = 2,
     cex.lab = 1.2, cex.axis = 1.2, main = "Tukey psi")
abline(v = c(-4, 4), col = 2, lwd = 2, lty = 2)


f3 <- function(n){
  x11()
  par(mar = c(5,5.2,4,1) + 0.1)
  plot(0, type = "n", xlim = c(0, 10), ylim = c(0, 10), cex.axis = 2, cex.lab = 2,
       xlab = expression(x[t]), ylab = expression(y[t]))
  x <- matrix(nrow = n, ncol = 2)
  for(i in 1:n){
    temp <- locator(n = 1)
    points(temp$x, temp$y, cex = 2, pch = pch)
    x[i,] <- c(temp$x, temp$y)
  }
  lm1 <- lm(x[,2] ~ x[,1] - 1)
  abline(lm1, col = 2, lwd = 2, lty = 2)
  r2 <- 1 - sum((x[,2] - lm1$fitt)^2)/sum((x[,2] - mean(x[,2]))^2)
  mtext(text = paste(round(r2, 2)), side = 3, line = 2, col = 2)
}




