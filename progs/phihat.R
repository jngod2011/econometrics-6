# Distribution of phihat

library(MASS)

sim.phihat <- function(phi,TT,R=5000) {
  phihat <- rep(NA,R)
  tphihat <- rep(NA,R)
  for(r in 1:R) {
    x <- filter(rnorm(TT),filter=phi,method="recursive")
    stt <- sum(x[1:(TT-1)]^2)
    phihat[r] <- (x[2:TT]%*%x[1:(TT-1)])/stt
    s2 <- sum((x[2:TT]-phihat[r]*x[1:(TT-1)])^2)/(TT-2)
    tphihat[r] <- (phihat[r]-phi)/sqrt(s2/stt)
    }
  return(list(phihat=phihat,tphihat=tphihat))
  }

# I(0) process
set.seed(123)
br1 <- seq(0.45,1,length=45)
br2 <- seq(-4,4,length=45)
g <- seq(-4,4,length=500)

par(mfrow=c(3,2))
obj <- sim.phihat(phi=0.8,TT=100)
phihat <- obj$phihat
tphihat <- obj$tphihat
truehist(phihat,breaks=br1,xlab=expression(hat(phi)),main="phi=0.8, T=100",ylim=c(0,55),xlim=c(0.55,0.95))
truehist(tphihat,breaks=br2,xlab=expression((hat(phi)-phi)/se(hat(phi))),main="phi=0.8, T=100",ylim=c(0,0.5))
lines(g,dnorm(g))

obj <- sim.phihat(phi=0.8,TT=1000)
phihat <- obj$phihat
tphihat <- obj$tphihat
truehist(phihat,breaks=br1,xlab=expression(hat(phi)),main="phi=0.8, T=1000",ylim=c(0,55),xlim=c(0.55,0.95))
truehist(tphihat,breaks=br2,xlab=expression((hat(phi)-phi)/se(hat(phi))),main="phi=0.8, T=1000",ylim=c(0,0.5))
lines(g,dnorm(g))

obj <- sim.phihat(phi=0.8,TT=10000)
phihat <- obj$phihat
tphihat <- obj$tphihat
truehist(phihat,breaks=br1,xlab=expression(hat(phi)),main="phi=0.8, T=10000",ylim=c(0,55),xlim=c(0.55,0.95))
truehist(tphihat,breaks=br2,xlab=expression((hat(phi)-phi)/se(hat(phi))),main="phi=0.8, T=10000",ylim=c(0,0.5))
lines(g,dnorm(g))

# I(1) process
set.seed(123)
br1 <- seq(0.65,1.05,length=45)
br2 <- seq(-4.5,3.5,length=45)

par(mfrow=c(3,2))
obj <- sim.phihat(phi=1,TT=100)
phihat <- obj$phihat
tphihat <- obj$tphihat
v <- locator(1)
truehist(phihat,breaks=br1,xlab=expression(hat(phi)),main="phi=1, T=100",ylim=c(0,110),xlim=c(0.85,1.05))
truehist(tphihat,breaks=br2,xlab=expression((hat(phi)-phi)/se(hat(phi))),main="phi=1, T=100",ylim=c(0,0.7))
lines(g,dnorm(g))

obj <- sim.phihat(phi=1,TT=1000)
phihat <- obj$phihat
tphihat <- obj$tphihat
truehist(phihat,breaks=br1,xlab=expression(hat(phi)),main="phi=1, T=1000",ylim=c(0,110),xlim=c(0.85,1.05))
truehist(tphihat,breaks=br2,xlab=expression((hat(phi)-phi)/se(hat(phi))),main="phi=1, T=1000",ylim=c(0,0.7))
lines(g,dnorm(g))

obj <- sim.phihat(phi=1,TT=10000)
phihat <- obj$phihat
tphihat <- obj$tphihat
truehist(phihat,breaks=br1,xlab=expression(hat(phi)),main="phi=1, T=10000",ylim=c(0,110),xlim=c(0.85,1.05))
truehist(tphihat,breaks=br2,xlab=expression((hat(phi)-phi)/se(hat(phi))),main="phi=1, T=10000",ylim=c(0,0.7))
lines(g,dnorm(g))
