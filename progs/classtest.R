library(rgl)

set.seed(123)
x1 <- runif(50,min=0.1,max=1)
x2 <- runif(50,min=0.1,max=1)
y <- x1^0.56*x2^0.56
y <- y+rnorm(50,0,0.05)
# Disturbances with E(u)=0 and Var(u)=0.05^2

loglik <- function(param,dat) {
  x1 <- dat[,1]
  x2 <- dat[,2]
  y <- dat[,3]
  sum(log(dnorm(y,x1^param[1]*x2^param[2],0.05)))
}

score <- function(param,dat) {
  x1 <- dat[,1]
  x2 <- dat[,2]
  y <- dat[,3]
  c1 <- (y-x1^param[1]*x2^param[2])/0.05^2
  g1 <- sum(c1*x1^param[1]*x2^param[2]*log(x1))
  g2 <- sum(c1*x1^param[1]*x2^param[2]*log(x2))
  return(c(g1,g2))
}

a1 <- seq(0.4,0.6,length=30)
a2 <- seq(0.4,0.6,length=30)
a0 <- cbind(rep(a1,30),rep(a2,each=30))

lnL <- apply(a0,1,loglik,dat=cbind(x1,x2,y))
dim(lnL) <- c(30,30)
opt1 <- optim(c(0.5,0.5),loglik,control=list(fnscale=-3),dat=cbind(x1,x2,y))

# LR test

persp3d(a1,a2,lnL,col="light blue",zlim=c(-30,80))
points3d(opt1$par[1],opt1$par[2],opt1$value,col="red",size=9)
L0 <- apply(cbind(a1,1-a1),1,loglik,dat=cbind(x1,x2,y))
lines3d(a1,1-a1,L0,col="green",lwd=3)
ix0 <- which.max(L0)
points3d(a1[ix0],1-a1[ix0],L0[ix0],col="pink",size=9)

lines3d(opt1$par[1],opt1$par[2],c(opt1$value,L0[ix0]),col="red",lwd=3)
lines3d(c(a1[ix0],opt1$par[1]),c(1-a1[ix0],opt1$par[2]),L0[ix0],col="pink",lwd=3)

# Wald test

persp3d(a1,a2,lnL,col="light blue",zlim=c(-30,80))
points3d(opt1$par[1],opt1$par[2],opt1$value,col="red",size=9)
L0 <- apply(cbind(a1,1-a1),1,loglik,dat=cbind(x1,x2,y))
lines3d(a1,1-a1,L0,col="green",lwd=3)
ix0 <- which.max(L0)
points3d(a1[ix0],1-a1[ix0],L0[ix0],col="pink",size=9)

lines3d(c(0.4,0.6,0.6,0.4,0.4),c(0.4,0.4,0.6,0.6,0.4),0)
surface3d(a1,a2,150*(outer(a1,a2,"+")-1),col="light green")
lines3d(a1,1-a1,0,col="green",lwd=3)
lines3d(opt1$par[1],opt1$par[2],c(opt1$value,150*(opt1$par[1]+opt1$par[2]-1)),lwd=2)
lines3d(opt1$par[1],opt1$par[2],c(150*(opt1$par[1]+opt1$par[2]-1),0),col="red",lwd=3)
surface3d(a1,a2,matrix(0,30,30),col="antiquewhite")

# LM test

persp3d(a1,a2,lnL,col="light blue",zlim=c(-30,80))
points3d(opt1$par[1],opt1$par[2],opt1$value,col="red",size=9)
L0 <- apply(cbind(a1,1-a1),1,loglik,dat=cbind(x1,x2,y))
lines3d(a1,1-a1,L0,col="green",lwd=3)
ix0 <- which.max(L0)
points3d(a1[ix0],1-a1[ix0],L0[ix0],col="pink",size=9)

s0 <- apply(a0,1,score,dat=cbind(x1,x2,y))[1,]
dim(s0) <- c(30,30)
surface3d(a1,a2,matrix(0,30,30),col="antiquewhite")
surface3d(a1,a2,s0/10,col="light green")
S0 <- apply(cbind(a1,1-a1),1,score,dat=cbind(x1,x2,y))[1,]
lines3d(a1,1-a1,S0/10,col="green",lwd=3)
lines3d(a1[ix0],1-a1[ix0],c(L0[ix0],S0[ix0]/10))
lines3d(a1[ix0],1-a1[ix0],c(S0[ix0]/10,0),col="pink",lwd=3)

