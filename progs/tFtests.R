# Multiple t-Tests vs F-Test

library(ellipse)

setwd("/Users/tredem/Dropbox/skripte/Ecmtx1/progs")
x <- read.csv2("../daten/duenger.csv",header=F)
lnp <- log(x[,1])
lnn <- log(x[,2])
lng <- log(x[,3])
regr <- lm(lng~lnp+lnn)
bhat <- coefficients(regr)
se <- summary(regr)[[4]][1:3,2]

plot(ellipse(regr,which=c(2,3)),t="l",main="t-Tests vs F-Test",
     xlab=expression(hat(beta)[1]),ylab=expression(hat(beta)[2]))
points(bhat[2],bhat[3])
abline(v=bhat[2]+qnorm(c(0.025,0.975))*se[2],lty="dashed")
abline(h=bhat[3]+qnorm(c(0.025,0.975))*se[3],lty="dashed")
locator(1)

plot(ellipse(regr,which=c(1,2)),t="l",main="t-Tests vs F-Test",
     xlab=expression(hat(alpha)),ylab=expression(hat(beta)[1]))
points(bhat[1],bhat[2])
abline(v=bhat[1]+qnorm(c(0.025,0.975))*se[1],lty="dashed")
abline(h=bhat[2]+qnorm(c(0.025,0.975))*se[2],lty="dashed")
