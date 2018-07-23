n <- 100
x <- runif(n)*50+10
y <- 0.5+0.1*x+rnorm(n)
xx <- x+rnorm(n)*15

plot(x,y,xlim=range(xx))
v <- locator(1)
abline(lm(y~x))
v <- locator(1)
points(xx,y,col="red")
for(i in 1:n) arrows(x[i],y[i],xx[i],y[i],lty="dotted")
v <- locator(1)
plot(x,y,xlim=range(xx))
points(xx,y,col="red")
abline(lm(y~x))
v <- locator(1)
abline(lm(y~xx),col="red")
