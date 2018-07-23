# Illustration zum t-Test

par(cex=1.3)
curve(dnorm(x,mean=0.15,sd=0.03),n=1000,from=0,to=0.3,main=expression(H0: beta == 0.15),
      yaxs="i",ylim=c(0,15),xlab=expression(hat(beta)),ylab="Dichte (unter H0)")
text(0.15,0.6,"q")
locator(1)

q1 <- qnorm(0.025,0.15,0.03)
q2 <- qnorm(0.975,0.15,0.03)
lines(c(q1,q1),c(0,dnorm(q1,0.15,0.03)),lwd=3,col="red")
lines(c(q2,q2),c(0,dnorm(q2,0.15,0.03)),lwd=3,col="red")
locator(1)

for(i in seq(0.04,q1,length=50)){
  q1 <- i
  q2 <- 0.3-i
  lines(c(q1,q1),c(0,dnorm(q1,0.15,0.03)),lwd=5,col="red")
  lines(c(q2,q2),c(0,dnorm(q2,0.15,0.03)),lwd=5,col="red")
}
locator(1)

curve(dt(x,df=18),n=1000,from=-4,to=4,main=expression(H0: beta == 0.15),    
      yaxs="i",ylim=c(0,0.45),xlab=expression(t==(hat(beta)-q)/se(hat(beta))),ylab="Dichte von t (unter H0)")
locator(1)

q1 <- qt(0.025,df=18)
q2 <- qt(0.975,df=18)
lines(c(q1,q1),c(0,dt(q1,18)),lwd=3,col="red")
lines(c(q2,q2),c(0,dt(q2,18)),lwd=3,col="red")
locator(1)

for(i in seq(q2,4,length=50)){
  q1 <- -i
  q2 <- i
  lines(c(q1,q1),c(0,dt(q1,18)),lwd=5,col="red")
  lines(c(q2,q2),c(0,dt(q2,18)),lwd=5,col="red")
}

text(1.85,0.02,expression(t[a/2]))
text(-1.8,0.02,expression(-t[a/2]))
