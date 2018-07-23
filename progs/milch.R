# Illustrationen zu nichtlinearen Zusammenhängen

windows(10,10)
par(cex=1.3)

f <- c(10,30,20,33,5,22,8,14,25,1,17,28)
m <- c(6525,8437,8019,8255,5335,7236,5821,7531,8320,4336,7225,8112)

plot(f,m,pch=19,xlab="Futter f",ylab="Milch m",lwd=4,main="linear")
locator(1)

plot(log(f),m,pch=19,xlab=expression(ln(f[t])),ylab=expression(m[t]),lwd=4,main="semi-logarithmisch")
locator(1)

plot(1/f,m,pch=19,xlab=expression(1/f[t]),ylab=expression(m[t]),lwd=4,main="invers")
locator(1)

plot(f,log(m),pch=19,xlab=expression(f[t]),ylab=expression(ln(m[t])),lwd=4,main="exponential")
locator(1)

plot(log(f),log(m),pch=19,xlab=expression(ln(f[t])),ylab=expression(ln(m[t])),lwd=4,main="logarithmisch")
locator(1)

plot(1/f,log(m),pch=19,xlab=expression(1/f[t]),ylab=expression(ln(m[t])),lwd=4,main="log-invers")
locator(1)

par(mfrow=c(3,2))
plot(f,m,pch=19,xlab="Futter f",ylab="Milch m",lwd=4,main="linear")
plot(log(f),m,pch=19,xlab=expression(ln(f[t])),ylab=expression(m[t]),lwd=4,main="semi-logarithmisch")
plot(1/f,m,pch=19,xlab=expression(1/f[t]),ylab=expression(m[t]),lwd=4,main="invers")
plot(f,log(m),pch=19,xlab=expression(f[t]),ylab=expression(ln(m[t])),lwd=4,main="exponential")
plot(log(f),log(m),pch=19,xlab=expression(ln(f[t])),ylab=expression(ln(m[t])),lwd=4,main="logarithmisch")
plot(1/f,log(m),pch=19,xlab=expression(1/f[t]),ylab=expression(ln(m[t])),lwd=4,main="log-invers")
