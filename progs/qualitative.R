# Ungleicher Achsenabschnitt

set.seed(123456)
alphaM <- 20
alphaF <- 6
betaM <- betaF <- 1
xM <- c(11.326,12.855,12.489,9.769,8.966,9.184,11.683,10.147,13.984,10.221,11.633,13.994,13.497,8.458,13.260,14.215,12.638,13.206,10.531,13.879,12.390,14.814,13.587,10.330,10.705)
xF <- c(15.009,18.179,18.068,12.190,14.069,15.635,12.949,18.805,18.540,14.606,12.798,12.528,15.406,14.326,18.284,18.786,18.152,13.444,18.165,15.904,14.711,14.624,18.602,15.349,17.730)
uM <- rnorm(25)*3
uF <- rnorm(25)*3
yM <- alphaM+betaM*xM+uM
yF <- alphaF+betaF*xF+uF

plot(xF,yF,xlim=c(10,20),ylim=c(0,max(yM)),xlab="Years",ylab="Wage",col="red")
points(xM,yM,col="blue")
abline(alphaF,betaF,col="red")
abline(alphaM,betaM,col="blue")
locator(1)
plot(xF,yF,xlim=c(10,20),ylim=c(0,max(yM)),xlab="Years",ylab="Wage")
points(xM,yM)
a <- locator(1)
y <- c(yM,yF)
x <- c(xM,xF)
abline(lm(y~x))
locator(1)

# Ungleiche Steigung

set.seed(123456)
alphaM <- 10
alphaF <- 10
betaM <- 1
betaF <- 0.4
xM <- c(11.326,12.855,12.489,9.769,8.966,9.184,11.683,10.147,13.984,10.221,11.633,13.994,13.497,8.458,13.260,14.215,12.638,13.206,10.531,13.879,12.390,14.814,13.587,10.330,10.705)
xF <- c(15.009,18.179,18.068,12.190,14.069,15.635,12.949,18.805,18.540,14.606,12.798,12.528,15.406,14.326,18.284,18.786,18.152,13.444,18.165,15.904,14.711,14.624,18.602,15.349,17.730)
uM <- rnorm(25)*3
uF <- rnorm(25)*3
yM <- alphaM+betaM*xM+uM
yF <- alphaF+betaF*xF+uF

plot(xF,yF,xlim=c(10,20),ylim=c(0,max(yM)),xlab="Years",ylab="Wage",col="red")
points(xM,yM,col="blue")
abline(alphaF,betaF,col="red")
abline(alphaM,betaM,col="blue")
locator(1)

plot(xF,yF,xlim=c(10,20),ylim=c(0,max(yM)),xlab="Years",ylab="Wage")
points(xM,yM)
a <- locator(1)
y <- c(yM,yF)
x <- c(xM,xF)
abline(lm(y~x))
