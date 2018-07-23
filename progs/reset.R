# Illustrationen zum RESET

f <- c(10,30,20,33,5,22,8,14,25,1,17,28)
m <- c(6525,8437,8019,8255,5335,7236,5821,7531,8320,4336,7225,8112)

# Lineares Modell
y <- m
x <- cbind(1,f)
bhat <- solve(t(x)%*%x)%*%t(x)%*%y
yhat <- x%*%bhat
uhat <- y-yhat
Suu <- t(uhat)%*%uhat

# Erweitertes Modell
x <- cbind(1,f,yhat^2,yhat^3,yhat^4)
x <- cbind(1,f,yhat^2/1e9,yhat^3/1e12,yhat^4/1e15)
bhat2 <- solve(t(x)%*%x)%*%t(x)%*%y
yhat2 <- x%*%bhat2
uhat2 <- y-yhat2
Suu2 <- t(uhat2)%*%uhat2

Fstat <- ((Suu-Suu2)/3)/(Suu2/(12-4-1))
Fkrit <- qf(0.95,df1=3,df2=12-4-1)
pwert <- 1-pf(Fstat,df1=3,df2=12-4-1)



library(lmtest)
resettest(m~f,power=2:4)
