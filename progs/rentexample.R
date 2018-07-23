# Rent Example

# data
x <- c(0.5,1.4,1.1,2.2,1.3,3.2,3.1,4.4,3.7,3,3.5,4.1)
y <- c(16.8,16.2,15.9,15.4,16.4,13.2,12.8,12.2,15,13.6,14.1,13.3)

# plot
plot(x,y,xlab="distance to center",ylab="rent",pch=19)


# Goldfeld-Quandt-Test

# Estimation for group inner-city (Observations 1...5)
regrI <- lm(y[1:5]~x[1:5])
SuuI <- sum(residuals(regrI)^2)
print(SuuI)

# Estimation for group suburban (Observations 6...12)
regrII <- lm(y[6:12]~x[6:12])
SuuII <- sum(residuals(regrII)^2)
print(SuuII)

print(paste("Test statistic F=",(SuuII/5)/(SuuI/3)))
print(paste("5% critical value =",qf(0.95,5,3)))

# White-Test

regr <- lm(y~x)
uhat <- residuals(regr)
aux <- lm(uhat^2~x+I(x^2))
psummary(aux)
aux$

# Whites HC standard errors
X <- cbind(1,x)
betahat <- solve(t(X)%*%X)%*%t(X)%*%y
uhat <- y- X%*%betahat 
What <- diag(uhat^2)
Covbetahat <- solve(t(X)%*%X)%*%t(X)%*%What%*%X%*%solve(t(X)%*%X)
print(sqrt(diag(Covbetahat)))