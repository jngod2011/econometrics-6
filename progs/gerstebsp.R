# illustration of barley example

library(rgl)
setwd("C:/Users/w_muts01/Dropbox/shared folders/Econometrics 1/progs")

# Data
x <- read.csv2("../daten/duenger.csv",header=F)
lnp <- log(x[,1])
lnn <- log(x[,2])
lng <- log(x[,3])
plot3d(lnp,lnn,lng,col="red",xlab="log p(t)",ylab="log n(t)",zlab="log g(t)",size=12)

# regression plane
regr <- lm(lng~lnp+lnn)
gridx <- seq(min(lnp),max(lnp),length=100)
gridy <- seq(min(lnn),max(lnn),length=100)
grid <- expand.grid(gridx,gridy)
names(grid) <- c("lnp","lnn")
yfit <- matrix(predict(regr,newdata=grid),100,100)
surface3d(gridx,gridy,yfit,col="light green")

# Residuals
yfit <- predict(regr,newdata=data.frame(lnp,lnn))
for(i in 1:30) lines3d(c(lnp[i],lnp[i]),c(lnn[i],lnn[i]),c(lng[i],yfit[i]),lwd=3)
