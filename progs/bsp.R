library(MASS)
set.seed(123)

trans3d <- function(x,y,z, pmat) 
  {
  tr <- cbind(x,y,z,1) %*% pmat
  list(x = tr[,1]/tr[,4], y= tr[,2]/tr[,4])
  }
 
windows(8,8,xpos=0,ypos=0)
x <- mvrnorm(1000,c(0.4,0.7),matrix(2*c(0.0036,-0.003,-0.003,0.01),2,2))
d <- kde2d(x[,1],x[,2],lims=c(0,1,0,1),n=51)
dz <- diag(d$z[,51:1])
persp(d,phi=30,theta=-30,xlab="alpha",ylab="beta",zlab="Loglikelihood",shade=0.5) -> res
c <- locator(1)
w1 <- which(d$z==max(d$z),arr.ind=T)[1,]
points(trans3d(d$x[w1[1]],d$y[w1[2]],max(d$z),res),col="red",pch=19,lwd=3)
c <- locator(1)

windows(8,8,xpos=450,ypos=0)
par(mar=c(4,5,2,1))
image(d,xlab="alpha",ylab="beta")
w<-which(d$z==max(d$z),arr.ind=T)[1,]
points(d$x[w[1]],d$y[w[2]],pch=19,col="black",lwd=3)
c <- locator(1)
lines(c(0,1),c(1,0),col="blue",lwd=2)
text(0.8,0.45,"g(alpha,beta)=0")
c <- locator(1)

windows(8,8,xpos=450,ypos=100)
plot(d$x,dz,type="l",xlab="alpha",ylab="restringierte Log-Likelihood",main="g(alpha,beta)=alpha+beta-1=0")
points(d$x[which.max(dz)],max(dz),pch=19,col="red",lwd=3)
c <- locator(1)

dev.set(3)
image(d,xlab="alpha",ylab="beta")
w<-which(d$z==max(d$z),arr.ind=T)[1,]
points(d$x[w[1]],d$y[w[2]],pch=19,col="black",lwd=3)
lines(c(0,1),c(1,0),col="blue",lwd=2)
text(0.8,0.45,"g(alpha,beta)=0")
points(d$x[which.max(dz)],1-d$x[which.max(dz)],pch=19,col="black",lwd=3)
c <- locator(1)

dev.set(2)
persp(d,phi=30,theta=-30,xlab="alpha",ylab="beta",zlab="Loglikelihood",shade=0.5) -> res
points(trans3d(d$x[w1[1]],d$y[w1[2]],max(d$z),res),col="red",pch=19,lwd=3)
lines(trans3d(d$x,1-d$x,dz,res),col="blue",lwd=3)
points(trans3d(d$x[which.max(dz)],1-d$x[which.max(dz)],max(dz),res),col="red",pch=19,lwd=3)
c <- locator(1)

windows(8,8,xpos=0,ypos=100)
alpha <- seq(0,1,length=51)
beta <- seq(0,1,length=51)
z <- outer(alpha,beta-1,"+")
persp(alpha,beta,z,phi=30,theta=-30,xlab="alpha",ylab="beta",zlab="alpha+beta-1") -> res
lines(trans3d(alpha,1-alpha,0,res),col="blue",lwd=3)
points(trans3d(d$x[which.max(dz)],1-d$x[which.max(dz)],0,res),col="red",pch=19,lwd=3)
points(trans3d(d$x[w1[1]],d$y[w1[2]],d$x[w1[1]]+d$y[w1[2]]-1,res),col="red",pch=19,lwd=3)


#c <- locator(1)
#windows(8,8)
#persp(d,phi=30,theta=-30,xlab="alpha",ylab="beta",zlab="Loglikelihood",shade=0.5)
#points(trans3d(d$x[w1[1]],d$y[w1[2]],max(d$z),res),col="red",pch=19,lwd=3)
#lines(trans3d(d$x,1-d$x,dz,res),col="green",lwd=3)
#points(trans3d(d$x[which.max(dz)],1-d$x[which.max(dz)],max(dz),res),col="red",pch=19,lwd=3)
