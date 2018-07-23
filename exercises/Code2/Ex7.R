u <- c(4, 3.5, 3.5, 2, -1, -4, -3, -2, -3, -1, 1, 3, 1, -2, -2)
par(mar = c(5,5,4,1) + 0.1)
plot(u, xlab = "t", ylab = expression(hat(u)[t]), pch = 19)
abline(h = 0)
plot(u[-length(u)], u[-1], asp = 1, xlab = expression(hat(u)[t-1]), ylab = expression(hat(u)[t]), pch = 19)

sum((u[-1] - u[-length(u)])^2)/sum(u^2)
