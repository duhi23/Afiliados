
library(readxl)

ind <- read_excel("indicadores.xlsx", sheet=1)
yreg <- as.vector(sub_afi$ECUADOR)
datos <- ind %>% select(-Mes) %>% mutate(y=yreg)

reg1 <- lm(y~Inflacion + Petroleo + Desempleo, data = datos)
summary(reg1)

msLVS <- msmFit(reg1, k = 2, sw = rep(TRUE, 5))
summary(msLVS)

par(mar=c(3,3,3,3))
plotProb(msLVS, which=1)
plotProb(msLVS, which=2)


# MS for Value Stocks (k is number of regimes, 6 is for means of 5 variables
# + 1 for volatility)
msLVS = msmFit(olsLVS, k = 2, sw = rep(TRUE, 6))
# MS for Growth Stocks
msLGS = msmFit(olsLGS, k = 2, sw = rep(TRUE, 6))
# Obtain the results of both
summary(msLVS)
summary(msLGS)
