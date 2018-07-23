setwd("C:/Users/stapperm/Dropbox/Arbeit/Econometrics 2/uebung/SS2018/Code")

plotEco <- function(type, n, filename){
  par(mar = c(5,6,2,1) + 0.1)
  
  if(type == "resres"){
    plot(0, 0, xlim = c(-5, 5), ylim = c(-5, 5), type = "n",
         xlab = "", ylab = expression(hat(u)[t]^2), cex.axis = 1.5, cex.lab = 1.5)
    mtext(expression(hat(u)[t-1]^2), 1, 3.5, cex = 1.5)
  }
  
  if(type == "tres"){
    plot(0, 0, xlim = c(1, n), ylim = c(-5, 5), type = "n",
         xlab = expression(t),
         ylab = expression(hat(u)[t]^2), cex.axis = 1.5, cex.lab = 1.5)
  }
  
  if(type == "xy"){
    plot(0, 0, xlim = c(-5, 5), ylim = c(-5, 5), type = "n",
         xlab = expression(x[t]),
         ylab = expression(y[t]), cex.axis = 1.5, cex.lab = 1.5)
  }
  
  for(i in 1:n){
    temp <- locator(1)
    if(type == "tres") points(i, temp$y, pch = 19, cex = 1.5)
    else points(temp$x, temp$y, pch = 19, cex = 1.5)
  }
  
  dev.copy(png, paste(filename, ".png", sep = ""))
  dev.off()
}

plotEco("xy", 25, "autoco1")
plotEco("resres", 25, "autoco2")
plotEco("resres", 25, "autoco3")
plotEco("tres", 25, "autoco4")
plotEco("tres", 20, "autoco5")
plotEco("xy", 30, "autoco6")
