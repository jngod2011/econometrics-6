# Exercise: Binomial Regression

set.seed(123)
y0 <- rnorm(n = 15, mean = 1, sd = 2)
y1 <- rnorm(n = 15, mean = 5, sd = 2)

setwd("C:/Users/stapperm/Documents/Econometrics 1/Code")

png("binreg.png", width = 600, height = 600)
plot(rep(0, 15) ,y0, xlim = c(-0.5, 1.5), ylim = range(c(y0,y1)), cex = 2,
     xlab = expression(x[i]), ylab = expression(y[i]), cex.axis = 1.2, cex.lab = 1.2, pch = 4, lwd = 2)
points(rep(1, 15), y1, cex = 2, pch = 4, lwd = 2)

lines(x = c(-0.1,0.1), y = rep(mean(y0), 2), lwd = 2, col = 2)
lines(x = c(0.9,1.1), y = rep(mean(y1), 2), lwd = 2, col = 2)
text(x = -0.1, y = mean(y0) + 0.5, labels = expression(bar(y)[B]), pos = 2, col = 2, cex = 1.5)
text(x = 1.1, y = mean(y1) - 0.5, labels = expression(bar(y)[A]), pos = 4, col = 2, cex = 1.5)
lm1 <- lm.fit(x = cbind(1, rep(0:1, each = 15)), y = c(y0, y1))
abline(lm1, lwd = 2, col = 2)
dev.off()

png("binreg2.png", width = 600, height = 600)
plot(rep(-1, 15) ,y0, xlim = c(-1.5, 1.5), ylim = range(c(y0,y1)), cex = 2,
     xlab = expression(x[i]), ylab = expression(y[i]), cex.axis = 1.2, cex.lab = 1.2, pch = 4, lwd = 2)
points(rep(1, 15), y1, cex = 2, pch = 4, lwd = 2)

lines(x = c(-1.1,-0.9), y = rep(mean(y0), 2), lwd = 2, col = 2)
lines(x = c(0.9,1.1), y = rep(mean(y1), 2), lwd = 2, col = 2)
text(x = -1.1, y = mean(y0) + 0.5, labels = expression(bar(y)[B]), pos = 2, col = 2, cex = 1.5)
text(x = 1.1, y = mean(y1) - 0.5, labels = expression(bar(y)[A]), pos = 4, col = 2, cex = 1.5)
lm2 <- lm.fit(x = cbind(1, rep(c(-1,1), each = 15)), y = c(y0, y1))
lm2$coef
(mean(y1) + mean(y0))/2
abline(lm2, lwd = 2, col = 2)
dev.off()

X <- cbind(1, rep(0:1, each = 15))

t(X)%*%X



f <- function(ns, means, sds, level = 0.05){
  n <- sum(ns)
  y0 <- rnorm(ns[1], means[1], sds[1])
  y1 <- rnorm(ns[2], means[2], sds[2])
  Est <- c(mean(y0),mean(y1)-mean(y0))
  sigmaHat <- (sum((y0-mean(y0))^2) + sum((y1-mean(y1))^2))/(n-2)
  Sxx <- ns[1]*ns[2]/n
  varBhat <- sigmaHat/Sxx
  varAhat2 <- sigmaHat/ns[2]
  varAhat <- sigmaHat*(1/60 + 35^2/60^2 *60/(25*35))
  print(rbind(Est[1] + c(-1,1)*qt(p = 1-level/2, df = n - 2)*sqrt(varAhat),
              Est[2] + c(-1,1)*qt(p = 1-level/2, df = n - 2)*sqrt(varBhat)))
  return(list(Est = Est, y0 = y0, y1 = y1, varBhat = varBhat, varAhat = varAhat,
              sigmaHat= sigmaHat, Sxx = Sxx, varAhat2 = varAhat2))
}

set.seed(1234)
aufg <- f(ns = c(25,35), means = c(120, 128), sds = c(10, 10), level = 0.05)
mean(aufg$y1)
sum(aufg$y1^2)

341676-25*115.89^2 + 492363 - 35*117.58^2
14399.72/48
117.58 + c(-1,1)*sqrt(2.46)*qt(p = 0.975, df = 58)


# Exercise: Dose-Response:
dat <- matrix(c(3,9,3,5,4,12,5,9,6,14,6,16,7,22,8,18,8,24,9,22), ncol = 2, byrow = T)
colnames(dat) <- c("Dose", "Response")
dat <- as.data.frame(dat)
X <- cbind(1, dat[,1])
y <- dat[,2]
lm.fit(y = y, x = X)

# png("DoseResp.png", width = 600, height = 600)
# ggplot(data = dat, aes(x = Dose, y = Response)) + geom_point(size = 5) +
#   theme(axis.text = element_text(size = 24, face = "bold"), axis.title = element_text(size = 24))
# dev.off()
# 
# png("DoseRespFit.png", width = 600, height = 600)
# ggplot(data = dat, aes(x = Dose, y = Response)) + geom_point(size = 5) +
#   theme(axis.text = element_text(size = 24, face = "bold"), axis.title = element_text(size = 24)) +
#   geom_smooth(method=lm) 
# dev.off()

Sxx <- sum((dat[,1] - mean(dat[,1]))^2)
ggplot(data = dat, aes(x = Dose, y = Response)) + geom_point(size = 5) +
  theme(axis.text = element_text(size = 24, face = "bold"), axis.title = element_text(size = 24)) 
lm1 <- lm(dat[,2]~dat[,1])
summary(lm1)
sHat <- sum(lm1$residuals^2)/8
lm1$coef[1] + c(-1,1)*qt(0.975, df = 8)*sqrt(sHat*(1/10 + mean(dat[,1])^2/Sxx))
lm1$coef[2] + c(-1,1)*qt(0.975, df = 8)*sqrt(sHat/Sxx)

covMat <- matrix(c(sHat*(1/10 + mean(dat[,1])^2/Sxx),
rep(-sHat*mean(dat[,1])/Sxx, 2),
sHat/Sxx), ncol = 2)

covMat[1,2]/sqrt(prod(diag(covMat)))

library(ellipse)

png("ellipseDoseResp.png", width = 600, height = 600)
plot(as.matrix(ellipse(x = covMat, centre = lm1$coef, level = 0.95)), type = "l",
     xlab = expression(alpha), ylab = expression(beta))
dev.off()

png("fancyCI.png", width = 600, height = 600)
plot(dat, col = 2, pch = 19)
abline(lm1, col = 2, lwd = 2)
apply(ellipse(x = covMat, centre = lm1$coef, level = 0.95, npoints = 50), 1, abline)
abline(lm1, col = 2, lwd = 2)
points(dat, col = 2, pch = 19)
dev.off()

png("intVSslope.png", width = 900, height = 600)
plot(dat, pch = 19, cex = 2, cex.lab = 1.2, cex.axis = 1.2, xlim = c(-1, 9), ylim = c(-1, 22))
abline(h = 0, v = 0, col = "gray", lty = 2)
abline(lm(formula = Response ~ Dose, data = dat), col = 2, lwd = 2)
abline(lm(formula = Response ~ Dose - 1, data = dat), col = "blue", lwd = 2)
legend("topleft", legend = c("With intercept", "Without"), col = c(2, "blue"), lwd = 2, cex = 1.2)
# Without intercept, maximal slope of CI:
abline(0, 3.75, col = "blue", lty = 2, lwd = 2)
abline(15.1 - 3.75*5.9, 3.75, col = 2, lty = 2, lwd = 2)
dev.off()
