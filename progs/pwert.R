# Illustration for p-value

curve(dt(x,df=18),n=1000,from=-4,to=4,main=expression(H0: beta <= 0.15),axes=F,yaxs="i",
      ylim=c(0,0.45),xlab=expression(t),ylab="Density of t (under H0)")
axis(1,labels=F)
axis(2,labels=F)
locator(1)

q1 <- qt(0.95,df=18)
lines(c(q1,q1),c(0,dt(q1,18)),col="red")
mtext(expression(t[a/2]),side=1,line=0.5,at=q1-0.1,cex=1.3)
locator(1)

tt <- 2.3
lines(c(tt,tt),c(0,dt(tt,18)))
mtext(expression(t),side=1,line=0.5,at=tt,cex=1.3)
locator(1)

for(i in seq(tt,4,length=100)) lines(c(i,i),c(0,dt(i,18)),col="blue")
lines(c(tt+0.2,3),c(dt(tt+0.3,18),0.1))
text(3,0.115,"p-value")
locator(1)


curve(dt(x,df=18),n=1000,from=-4,to=4,main=expression(H0: beta <= 0.15),axes=F,yaxs="i",
      ylim=c(0,0.45),xlab=expression(t),ylab="Density of t (under H0)")
axis(1,labels=F)
axis(2,labels=F)
q1 <- qt(0.95,df=18)
lines(c(q1,q1),c(0,dt(q1,18)),col="red")
mtext(expression(t[a/2]),side=1,line=0.5,at=q1-0.1,cex=1.3)
tt <- 0.9
lines(c(tt,tt),c(0,dt(tt,18)))
mtext(expression(t),side=1,line=0.5,at=tt,cex=1.3)
locator(1)

for(i in seq(tt,4,length=200)) lines(c(i,i),c(0,dt(i,18)),col="blue")
lines(c(2,3),c(dt(2,18),0.1))
text(3,0.115,"p-value")
