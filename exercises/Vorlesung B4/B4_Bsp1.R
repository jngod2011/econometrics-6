setwd("C:\\Users\\stapperm\\Dropbox\\Arbeit\\Econometrics 2\\uebung\\SS2018\\Vorlesung B4")
res <- read.csv2("LifeSavings_Resid.csv")[,2]
hist(res, freq = F, breaks = 20)
f <- function(x) dnorm(x, mean(res), sd(res))
curve(f, col = 2, add = T)
# Passt nicht sehr gut...


wdh <- 10000
n <- 10000
set.seed(08052018)
runs <- replicate(wdh,{
  sqrt(n)*(mean(rbinom(n = n, size = 1, prob = 0.5)) - 0.5)/sqrt(0.25)
})

m <- ceiling(max(abs(runs)))
brks <- seq(-m - 0.125, m + 0.125, by = 0.25)
hist(runs, freq = F, breaks = brks)
curve(dnorm, add = T, col = 2)


