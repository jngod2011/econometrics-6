f <- function(x, b) dgamma(x = x, shape = 3, rate = b) 
l <- function(x, b) prod(dgamma(x = x, shape = 3, rate = b))
ll <- function(x, b) sum(3*log(b) + 2*log(x) - 2*log(2) - b*x)
ll1 <- function(x, b) sum(3/b - x)
ll2 <- function(x, b) -3/(b^2)

set.seed(25012018)
x <- rgamma(n = 40, shape = 3, rate = 2)

bs <- seq(0.5, 3.5, length.out = 1000)

# For Wald and LR
setwd("C:\\Users\\stapperm\\Dropbox\\Arbeit\\Econometrics 1\\uebung\\WS1718\\Code")
llVal <- sapply(bs, function(b.) ll(x = x, b = b.))
ll1Val <- sapply(bs, function(b.) ll1(x = x, b = b.))

postscript("Gamma_WALD_LR.eps", width = 800, height = 600)
#postscript("Gamma_WALD_lsg.eps", width = 800, height = 600)
#postscript("Gamma_LR_lsg.eps", width = 800, height = 600)
plot(bs, (llVal + 120)/20, type = "l", lwd = 2,
     main = "Tests for Gamma Distribution", xlab = "b", ylab = "", xaxt = "n", yaxt = "n")
axis(side = 1, at = c(seq(0.5,3.5,by = 0.5), 1.2),
     labels = paste(c(seq(0.5,3.5,by = 0.5), 1.2)), cex.axis = 1.3, cex.lab = 1.3)
abline(h = 0)
lines(x = rep(3/mean(x), 2), y = c(-10000, (ll(x=x,b=3/mean(x))+120)/20))
abline(-1.2, 1, lty = 2, lwd = 2)
lines(x = rep(1.2, 2), y = c(-10000, (ll(x=x,b=1.2)+120)/20), lty = 2)
legend("bottomright", legend = c("Log-Likelihood", "Restriction"), lty = 1:2, cex = 1.3, lwd = 2)
# For Wald
#lines(x = c(rep(3/mean(x), 2)), y = c(0,-1.2 + 3/mean(x)), col = 3, lwd = 4)

# For LR
# abline(h = (ll(x=x,b=1.2)+120)/20, col = 3, lty = 2, lwd = 2)
# lines(x = rep(3/mean(x), 2), y = c((ll(x=x,b=1.2)+120)/20, (ll(x=x,b=3/mean(x))+120)/20), col = 3, lwd = 4)
dev.off()

# For LM
postscript("Gamma_LM.eps", width = 800, height = 600)
#postscript("Gamma_LM_lsg.eps", width = 800, height = 600)
plot(bs, ll1Val/50, type = "l", lwd = 2,
     main = "Tests for Gamma Distribution", xlab = "b", ylab = "",  yaxt = "n", xaxt = "n")
axis(side = 1, at = c(seq(0.5,3.5,by = 0.5), 1.2),
     labels = paste(c(seq(0.5,3.5,by = 0.5), 1.2)), cex.axis = 1.3, cex.lab = 1.3)
abline(-1.2, 1, lwd = 2, lty = 2)
abline(h = 0)
lines(bs, (llVal + 145)/220*8 - 1/2)
lines(x = rep(3/mean(x), 2), y = c(-100, (ll(x = x, b = 3/mean(x)) + 145)/220*8-1/2))
lines(x = rep(1.2,2), y = c(-100, (ll(x = x, b = 1.2) + 145)/220*8-1/2), lty = 2)
legend("top", legend = c("Score", "Log-Likelihood", "Restriction"), lty = c(1,1,2), lwd = c(2,1,2), cex = 1.3)

# lines(x = c(1.2,1.2), y = c(0, sum(3/1.2 - x))/50, col = 3, lwd = 4)
dev.off()


XX <- matrix(c(64,288.128,288.128,1842.3374), ncol = 2)
solve(XX)%*%c(6080, 27496)
(577628.1 - 6080^2/64)/(577700 - 6080^2/64)
c(b)%*%c(1,3)
