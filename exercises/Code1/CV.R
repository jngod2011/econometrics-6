# Plots for Normal distribution

setwd("C:/Users/stapperm/Documents/Econometrics 1/Code")

f <- function(mu = 0, sig = 1, lims = c(-3, 3), toRight = 1e16, toLeft = -1e16, cols = 2, perc = T, ...){
  fTemp <- function(x) dnorm(x = x, mean = mu, sd = sig)
  par(mar = c(5,4.5,4,1)+0.1)
  curve(fTemp, from = lims[1], to = lims[2], n = 1000,
        xlab = expression(x), ylab = expression(f(x)), lwd = 2,
        cex.axis = 1.2, cex.lab = 1.2, add = F)
  plotLims <- par("usr")
  if(!missing(toRight) && !missing(toLeft) && length(cols) == 1) cols <- rep(cols, 2)
  if(!missing(toRight) && !missing(toLeft) && length(perc) == 1) perc <- rep(perc, 2)
  if(!missing(toRight)){
    xseq <- seq(toRight, lims[2], length.out = 1000)
    XSEQ <- c(xseq, rev(xseq), toRight)
    polygon(x = XSEQ, y = c(rep(0, 1000), dnorm(rev(xseq), mean = mu, sd = sig), 0),
            col = cols[1], border = cols[1])
    if(perc[1] == T){
      pct <- round((1 - pnorm(q = toRight, mean = mu, sd = sig))*100, digits = 2)
      text(x = toRight, y = 0.4*dnorm(x = toRight, mean = mu, sd = sig) + 0.6*plotLims[4], pos = 3,
           col = cols[1], cex = 2, labels = paste(pct, "%"))
    }
    perc <- perc[-1]
    cols <- cols[-1]
  }
  
  if(!missing(toLeft)){
    xseq <- seq(toLeft, lims[1], length.out = 1000)
    XSEQ <- c(xseq, rev(xseq), toLeft)
    polygon(x = XSEQ, y = c(rep(0, 1000), dnorm(rev(xseq), mean = mu, sd = sig), 0),
            col = cols[1], border = cols[1])
    if(perc[1] == T){
      pct <- round(pnorm(q = toLeft, mean = mu, sd = sig)*100, digits = 2)
      text(x = toLeft, y = 0.4*dnorm(x = toLeft, mean = mu, sd = sig) + 0.6*plotLims[4], pos = 3,
           col = cols[1], cex = 2, labels = paste(pct, "%"))
      
    }
  }
  par(mar = c(5,4,4,1) + 0.1)
}

postscript("CV1.eps", width = 800, height = 600)
f(mu = 10, sig = 2, lims = c(4,16), toRight = qnorm(0.975, 10,2), toLeft = qnorm(0.025,10,2), cols = 2, perc = T)
dev.off()
postscript("CV2.eps", width = 800, height = 600)
f(mu = 10, sig = 2, lims = c(4,16), toLeft = qnorm(0.05,10,2), cols = 2, perc = T)
dev.off()
postscript("CV3.eps", width = 800, height = 600)
f(mu = 10, sig = 2, lims = c(4,16), toRight = qnorm(0.99, 10,2), toLeft = qnorm(0.04,10,2), cols = 2, perc = T)
dev.off()
postscript("CV4.eps", width = 800, height = 600)
f(mu = 10, sig = 2, lims = c(4,16), toRight = qnorm(0.025,10,2), cols = 2, perc = T)
dev.off()

postscript(file = "IQ.eps", width = 800, height = 500)
#png(filename = "IQ.png", width = 800, height = 600)
f <- function(x) dnorm(x = x, mean = 100, sd = 15/sqrt(4))
par(mar = c(5,4,4,1) + 0.1)
curve(f, xlim = c(80, 150), lwd = 2, cex.axis = 1.2, cex.lab = 1.2, xaxt = "n", xlab = "")
tck <- seq(80, 150, by = 5)
axis(1, at= tck, labels=FALSE)
text(x=tck, y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3])+0.001,
     labels=paste(tck), srt=45, adj=1, xpd=TRUE, cex = 1.2)
mtext(text = "x", side = 1, line = 4, cex = 1.2)
f2 <- function(x) dnorm(x = x, mean = 120, sd = 15/sqrt(4))
curve(f2, add = T, lwd = 2, col = 2)
legend("topright", legend = c(expression(H[0]), "sample"), bty = "n", lwd = 2, col = 1:2, cex = 1.2)
abline(h = 0)
dev.off()



maniF <- function(lev){
  f <- function(x) dnorm(x = x, mean = 100, sd = 15/sqrt(4))
  par(mar = c(5,4,4,1) + 0.1)
  curve(f, xlim = c(80, 150), lwd = 2, cex.axis = 1.2, cex.lab = 1.2, xaxt = "n", xlab = "",
        main = "Click me, I'm a gif", cex.main = 2)
  tck <- seq(80, 150, by = 5)
  axis(1, at= tck, labels=FALSE)
  text(x=tck, y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3])+0.001,
       labels=paste(tck), srt=45, adj=1, xpd=TRUE, cex = 1.2)
  mtext(text = "x", side = 1, line = 4, cex = 1.2)
  f2 <- function(x) dnorm(x = x, mean = 120, sd = 15/sqrt(4))
  curve(f2, add = T, lwd = 2, col = 2)
  legend("topright", legend = c(expression(H[0]), "sample"), bty = "n", lwd = 2, col = 1:2, cex = 1.2)
  abline(h = 0)
  
  col1 <- rgb(red = 0, green = 0.4, blue = 0, alpha = 0.2)
  col2 <- rgb(red = 0, green = 0, blue = 0.4, alpha = 0.2)
  critval <- qnorm(1 - lev, 100, 15/2)
  abline(v = critval)
  xseq <- seq(critval, 150, length.out = 1000)
  polygon(x = c(xseq, rev(xseq), critval), y = c(rep(0, 1000), dnorm(rev(xseq),100,7.5), 0),
          col = col1, border = col1)
  xseq <- seq(critval, 80, length.out = 1000)
  polygon(x = c(xseq, rev(xseq), critval), y = c(rep(0, 1000), dnorm(rev(xseq), 120, 7.5), 0),
          col = col2, border = col2)
  text(x = critval, y = 0.05, labels = paste(round(critval, digits = 2)), pos = 4, cex = 2)
  text(x = critval, y = 0.05, labels = "CV", pos = 2, cex = 2)
  text(x = 140, y = 0.04, labels = "Type-I error", cex = 2, col = rgb(0,0.4,0), pos = 4)
  text(x = 78, y = 0.04, labels = "Type-II error", cex = 2, col = rgb(0,0,0.4), pos = 4)
  text(x = 140, y = 0.034, labels = paste(round(lev*100, digits = 2), "%"), pos = 4, col = rgb(0,0.4,0),
       cex = 2)
  text(x = 78, y = 0.034, labels = paste(round(pnorm(critval, 120, 7.5)*100, digits = 2), "%"), pos = 4,
       col = rgb(0,0,0.4), cex = 2)
}

manipulate(
  maniF(lev),  
  lev=slider(0.001,0.25))

# Or as a gif:

setwd("C:/Users/stapperm/Documents/Econometrics 1/Code/cvgif")

png(file="example%02d.png", width=1000, heigh=800)
z <- 1
for (i in levSeq){
  temp <- strsplit(paste(z), "")[[1]]
  if(length(temp) == 1) nam <- paste("example","00",temp,".png", sep = "")
  if(length(temp) == 2) nam <- paste("example","0",temp[1],temp[2],".png", sep = "")
  if(length(temp) == 3) nam <- paste("example",temp[1], temp[2], temp[3],".png", sep = "")
  png(file=nam, width=1000, heigh=800)
  maniF(i)
  dev.off()
  z <- z+1
}

y <- rnorm(100)
x <- rexp(100, rate = 10)
plot(x,y)
sum((lm(y~x)$fitt))
sum(y)
113.66*3500 + 0.202*4443000
10*a^2 + 2*a*b*1170 + b^2*(10*415^2 + 10*1170^2)
1300000 - 2590592 + 811751.4



f <- function(n){
  Mpsi2 <- function(x, cc, psi, deriv){
    sapply(x, function(x.){
      if(abs(x.) > cc) return(2*cc*abs(x.) - cc^2)
      else return(x.^2)
    })
  }
  x <- rpois(n, 10.5)
  tf <- function(mu){
    sum(Mpsi2(x = (x - mu)/sqrt(mu), cc = 5.5, psi = "huber", deriv = -1))
  }
  grad <- function(mu){
    sum(Mpsi((x - mu)/sqrt(mu), cc = 5.5, psi = "huber"))
  }
  
  res <- constrOptim(theta = median(x), f = tf, grad = grad, ui = matrix(1, ncol = 1, nrow = 1), ci = 1e-05, outer.eps = 0.1)$par
  return(res)
}
f(1000000)
