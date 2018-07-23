# Dummy-Variablen

setwd("../daten")
V <- read.csv2("lohnbsp.csv")

lm(Lohn~Geschlecht+Ausbildung+Alter+Ausbildung:Geschlecht+Alter:Geschlecht,data=V)

lm(Lohn~Ausbildung+Alter,data=V,subset=Geschlecht==0)
lm(Lohn~Ausbildung+Alter,data=V,subset=Geschlecht==1)

