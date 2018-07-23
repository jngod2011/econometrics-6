setwd("C:\\Users\\stapperm\\Dropbox\\Arbeit\\Econometrics 2\\uebung\\SS2018\\Code")

set.seed(123)
x <- rweibull(n = 100, shape = 2, scale = 1)
x <- x - mean(x)
brks <- seq(-3, 5, by = 1)*sd(x)
labs <- c(expression("-3"*hat(sigma)^2),expression("-2" * hat(sigma)^2),
          expression("-" * hat(sigma)^2),"0",
          expression(hat(sigma)^2),expression("2" * hat(sigma)^2),
          expression("3" * hat(sigma)^2),expression("4" * hat(sigma)^2),
          expression("5" * hat(sigma)^2))

#png(filename = "hist8.png", width = 900, height = 600)
par(mar = c(7,7,4,2)+0.1)
h <- hist(x, breaks = brks, xlab = "", main = "Histogram of Residuals", xaxt = "n",
          cex.axis = 2, cex.lab = 2, cex.main = 2)
par(mgp = c(4,3,1))
axis(side = 1, at = brks, labels = labs, cex.axis = 2, cex.lab = 2)
mtext(text = "Residuals", side = 1, line = 5, cex = 2)
#dev.off()

#png(filename = "hist8_d.png", width = 900, height = 600)
h <- hist(x, breaks = brks, xlab = "Residuals", main = "Histogram of Residuals", xaxt = "n", freq = F)
axis(side = 1, at = brks, labels = labs)
lines(seq(brks[1], brks[length(brks)], length.out = 1000),
      dnorm(seq(brks[1], brks[9], length.out = 1000), mean = 0, sd = sd(x)),
      col = 2, lwd = 2)
#dev.off()


(m2 <- mean(x^2))
(m3 <- mean(x^3))
(m4 <- mean(x^4))

(s <- m3/(m2^(3/2))) 
(k <- m4/(m2^2))
100/6 * (s^2 + (k - 3)^2/4)

qchisq(p = 0.95, df = 2)

