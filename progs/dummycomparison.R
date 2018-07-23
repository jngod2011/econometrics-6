# Comparison of joint dummy variable estimation
# and two separate estimations

# Load data
setwd("/Users/marktrede/Dropbox/skripte/ecmtx2/daten")
x <- read.csv2("lohnbsp.csv")
attach(x)

# Joint and separate estimations
print(lm(wage~education+age+sex+education*sex+age*sex))
print(lm(wage[sex==0]~education[sex==0]+age[sex==0]))
print(lm(wage[sex==1]~education[sex==1]+age[sex==1]))

# With standard errors etc.
print(summary(lm(wage~education+age+sex+education*sex+age*sex)))
print(summary(lm(wage[sex==0]~education[sex==0]+age[sex==0])))
print(summary(lm(wage[sex==1]~education[sex==1]+age[sex==1])))
