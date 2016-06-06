######################################################
#####     Proyeccion Afiliados por Provincia     #####
######################################################

library('readxl')
library('dplyr')
library('tidyr')

devtools::install_github("garrettgman/DSR")
library('DSR')


#data <- read_excel("Activos.xlsx", sheet = 1)
data <- read_excel("Cotizantes.xlsx", sheet = 1)
data <- data %>% select(-Periodo)
glimpse(data)

data <- ts(data, start = c(2005), frequency = 12)
cdata <- data 
data <- ts(data[1:120,], start = c(2005), frequency = 12) # retiramos ultimos 12 meses

# stl(nottem, s.window="periodic")$time.series[,"trend"]
plot(decompose(data[,10],type = "additive"))
plot(stl(data[,10], "per"))

# Correlogramas
layout(matrix(c(1,1,2,3),2,2, byrow = TRUE))
plot(data[,10], main="Serie original", ylab="Afiliados")
acf(data[,10], main="FAC", ci.col="red", ylab="ACF", 48)
pacf(data[,10], main="FACP", ci.col="red", ylab="PACF", 48)

# Serie diferenciada
layout(matrix(c(1,1,2,3),2,2, byrow = TRUE))
plot(diff(data[,10]), main="Serie diferenciada", ylab="Afiliados")
acf(diff(data[,10]), main="FAC", ci.col="red", ylab="ACF", 48)
pacf(diff(data[,10]), main="FACP", ci.col="red", ylab="PACF", 48)

# Diferenciacion estacional
layout(matrix(c(1,1,2,3),2,2, byrow = TRUE))
plot(diff(diff(data[,10]),12), main="Serie diferenciada", ylab="Afiliados")
acf(diff(diff(data[,10]),12), main="FAC", ci.col="red", ylab="ACF",48)
pacf(diff(diff(data[,10]),12), main="FACP", ci.col="red", ylab="PACF",48)


auto.arima(data[,10])
modelo <- arima(data[,10], order=c(0,1,1),seasonal=list(order=c(0,0,1), period=12))
modelo

proy <- forecast(modelo, h=60)
U = proy$upper[,2]  
L = proy$lower[,2]

plot(cbind(seq(1,132), as.vector(data[,10])), xlim=c(0,160), ylim=c(0,850000), type="o")
par(new=TRUE)
plot(cbind(seq(121,144), as.vector(proy$pred)), col="red", xlim=c(0,160), ylim=c(0,850000), type="o")
par(new=TRUE)
plot(cbind(seq(121,144), as.vector(U)), col="blue", xlim=c(0,160), ylim=c(0,850000), type="l", lty="dashed")
par(new=TRUE)
plot(cbind(seq(121,144), as.vector(L)), col="blue", xlim=c(0,160), ylim=c(0,850000), type="l", lty="dashed")


# Graficos proyecciones - Provincias


esti <- matrix(0, ncol=24, nrow=252)
for(j in 1:24){
     modelo <- auto.arima(data[,j])
     proy <- forecast(modelo, h=252)
     esti[,j] <- proy$mean
}

layout(matrix(c(1,2),1,2, byrow = TRUE))
qqnorm(proy$residuals)
qqline(proy$residuals)
hist(proy$residuals, density = TRUE)


# Escritura de pronosticos
colnames(esti) <- colnames(data)
datos <- ts(esti, start = c(2015), frequency = 12)
write.csv(datos, "proyecciones_ult.csv")




# Consideramos el periodo 2006 - 2015 & Afiliados total por provincia
data <- data %>% filter(INDEX==5, ANIO != 2016, PROVINCIA !="ECUADOR", PROVINCIA !="PARA ACTUALIZACIÓN (DNAC)") %>% 
     select(ANIO, PROVINCIA, NUM)

# Verticalizamos la información
#data <- spread(data, ANIO, NUM)
data <- spread(data, PROVINCIA, NUM)

# Solo Provincias
#data <- data %>% filter(PROVINCIA != "PARA ACTUALIZACIÓN (DNAC)", PROVINCIA != "ECUADOR")

data <- data %>% select(-ANIO)

# Convertimos en serie de tiempo
data <- ts(data, start = c(2006))

plot(data[,c(1:6)], main="Afiliados históricos")
plot(data[,c(7:12)], main="Afiliados históricos")
plot(data[,c(13:18)], main="Afiliados históricos")
plot(data[,c(19:24)], main="Afiliados históricos")


library('tseries')
library('forecast')

kpss.test(data[,10])

# Ajuste por series temporales ARIMA

media <- numeric()
varianza <- numeric()
dife <- as.vector(diff(logdata))
for(i in 1:length(dife)){
     percentil <- quantile(dife, probs = c(0.05, 0.95))
     if(dife[i]<percentil[1]){
          dife[i] <- percentil[1]
     } else if (dife[i]>percentil[2]){
          dife[i] <- percentil[2]
     }
}

hist(dife)

matdif <- matrix(dife, ncol=24, byrow = FALSE)


# for(k in 1:100){
#      muestra <- sample(dife, size=216, replace = TRUE)
#      media[k] <- mean(muestra)
#      varianza[k] <- var(muestra)
# }



# Transformacion logaritmica
logdata <- data # log(data)

set.seed(123)
matlog <- matrix(0, nrow=nrow(logdata)+25, ncol=ncol(logdata)) # +25
matlin <- matrix(0, nrow=nrow(logdata)+25, ncol=ncol(logdata)) # +25
matlog[1,] <- as.vector(logdata[1,])
matlin[1,] <- as.vector(logdata[1,])
vari <- apply(matdif, 2, sd)/sqrt(10)
media <- apply(matdif, 2, mean)/10


for(j in 1:ncol(logdata)){
     model <- auto.arima(logdata[,j])
     if(length(model$coef)==0){
          beta <- 0
     } else {
          beta <- model$coef
     }
     
     for(i in 2:(nrow(logdata)+25)){ # +25
          matlog[i,j] <- matlog[i-1,j] + (beta + rnorm(1, mean=media[j], sd=vari[j]))*(2.5/(1+exp(i/23)))
          matlin[i,j] <- matlin[i-1,j] + beta
     }
     
}

matlog
matlin

# Estimación 2015
sum(matlog[10,])
# Estimación 2025
sum(matlog[20,])
# Estimación 2030
sum(matlog[25,])
# Estimación 2035
sum(matlog[30,])
# Estimación 2040
sum(matlog[35,])

#logres <- ts(round(exp(matlog),0), start = c(2006))
logres <- ts(round(matlog,0), start = c(2006))
matlin <- ts(round(matlin,0), start = c(2006))

plot(data[,1], ylim=c(50000, 200000), col="green", ylab="Afiliados")
par(new=TRUE)
plot(logres[,1], ylim=c(50000, 200000), col="red", ylab="Afiliados")


for(j in 1:ncol(data)){
     titulo <- paste("Provincia ", colnames(data)[j])
     minimo <- min(min(data[,j]), min(logres[,j]), min(matlin[,j]))
     maximo <- max(max(data[,j]), max(logres[,j]), max(matlin[,j]))
     plot(data[,j], ylim=c(minimo, maximo), xlim=c(2005, 2040), col="blue", ylab="Afiliados", main=titulo, lwd=2)
     par(new=TRUE)
     plot(logres[,j], ylim=c(minimo, maximo), xlim=c(2005, 2040), col="red", ylab="Afiliados", main=titulo, lwd=2)
     par(new=TRUE)
     plot(matlin[,j], ylim=c(minimo, maximo), xlim=c(2005, 2040), col="green", ylab="Afiliados", main=titulo, lwd=2, lty=3)
}

colnames(logres) <- colnames(data)

# Error en el total proyectado

apply(data, 2, sum)-apply(logres[1:10,], 2, sum)
sum(apply(data, 2, sum)-apply(logres[1:10,], 2, sum))


# Estimación 2015
proy <- rbind(logres[10,], logres[20,], logres[25,], logres[30,], logres[35,])
rownames(proy) <- c("2015", "2025", "2030", "2035", "2040")

afi <- c(3526093, 5115326, 5715049, 6385085, 7133676)

apply(proy, 1, sum) - afi


##############################################################################################


info <- read_excel("proyecciones_finales.xlsx", sheet = 2)
#info <- info %>% select(-AÑO)

# Graficos PGR proyeccion por Provincia
prov <- c("PICHINCHA", "GUAYAS", "AZUAY")
for(i in 1:length(prov)){
     plot(info[[1]][1:10], info[[prov[i]]][1:10], type='b', xlim=c(2006, 2040), ylim=c(0,0.25), col='blue',
          ylab='Radio crecimiento', xlab='Año', main=prov[i])
     par(new=TRUE)
     plot(info[[1]][11:34], info[[prov[i]]][11:34], type='b', xlim=c(2006, 2040), ylim=c(0,0.25), col='red',
          ylab='Radio crecimiento', xlab='Año', main=prov[i])
}

SGO <- read_excel("proyectado_SGO.xlsx", sheet = 1)
SGO <- SGO %>% select(AÑO, PGR) %>% filter(AÑO < 2040)

plot(SGO[[1]][1:10], SGO[[2]][1:10], type='b', xlim=c(2006, 2040), ylim=c(0,0.25), col='blue',
     ylab='Radio crecimiento', xlab='Año', main="Ecuador")
par(new=TRUE)
plot(SGO[[1]][11:34], SGO[[2]][11:34], type='b', xlim=c(2006, 2040), ylim=c(0,0.25), col='red',
     ylab='Radio crecimiento', xlab='Año', main="Ecuador")

# Graficos PGR comparacion


plot(SGO[[1]], SGO[[2]], type='o', xlim=c(2006, 2040), ylim=c(0,0.25), col='blue',
     ylab='Radio crecimiento', xlab='Año', main="Tasa de crecimiento")
par(new=TRUE)
plot(info[[1]], info[["PICHINCHA"]], type='o', xlim=c(2006, 2040), ylim=c(0,0.25), col='red',
     ylab='Radio crecimiento', xlab='Año')
par(new=TRUE)
plot(info[[1]], info[["GUAYAS"]], type='o', xlim=c(2006, 2040), ylim=c(0,0.25), col='green',
     ylab='Radio crecimiento', xlab='Año')

legend(2025,0.22, c("Ecuador", "Pichincha", "Guayas"), lty=c(1,1,1), lwd=c(2,2,2),col=c("blue","red","green"))


layout(matrix(c(1,1,2,3),2,2, byrow = TRUE))
plot(data[,10], main="Serie original", ylab="Afiliados")
acf(data[,10], main="FAC", ci.col="red", ylab="ACF", 48)
pacf(data[,10], main="FACP", ci.col="red", ylab="PACF", 48)


layout(matrix(c(1,1,2,3),2,2, byrow = TRUE))
plot(diff(data[,10]), main="Serie original", ylab="Afiliados")
acf(diff(data[,10]), main="FAC", ci.col="red", ylab="ACF", 48)
pacf(diff(data[,10]), main="FACP", ci.col="red", ylab="PACF", 48)


layout(matrix(c(1,1,2,3),2,2, byrow = TRUE))
plot(diff(diff(data[,10]), 12), main="Serie original", ylab="Afiliados")
acf(diff(diff(data[,10]), 12), main="FAC", ci.col="red", ylab="ACF", 48)
pacf(diff(diff(data[,10]), 12), main="FACP", ci.col="red", ylab="PACF", 48)


mod <- Arima(data[,10], order=c(0,1,1), seasonal = list(order=c(0,0,1), period=12))
mod$aic
layout(matrix(c(1,1,2,3),2,2, byrow = TRUE))
plot(mod$residuals, main="Serie original", ylab="Afiliados")
acf(mod$residuals, main="FAC", ci.col="red", ylab="ACF", 48)
pacf(mod$residuals, main="FACP", ci.col="red", ylab="PACF", 48)



################################################
#######     Responsabilidad Patronal     #######
################################################

prod <- function(ta, pij, k){
     mult<-1
     for(j in 1:k){
          mult <- mult*((1+pij)*(1+ta))^(1/12)
     }
     return(mult)
}

simprod <- function(ta, pij, t){
     vec <- vector(length=t)
     for(k in 1:t){
          vec[k] <- prod(ta, pij, k)
     }
     return(vec)
}

simmul <- function(ta, mc, t){
     vec <- vector(length=t)
     for(k in 1:t){
          vec[k] <- (1+(k*(ta+mc)/12))
     }
     return(vec)
}

ind <- function(ta, pij, mc, t){
     val <- ifelse(simprod(ta, pij, t) - simmul(ta, mc, t)>0,1,0)
     return(val)
}

# Simulacion

plot(simprod(0.04, 0.035, 400), col="green", ylim=c(0,25), xlim=c(0,400), type='l', lwd=2, main="Simulación", ylab='')
par(new=TRUE)
plot(simmul(0.04, 0.0933, 400), col="red", ylim=c(0,25), xlim=c(0,400), type='l', lwd=2, main="Simulación", ylab='')

## Responsabilidad

ResPat <- function(xt,t){
     val <- rep(xt, t)*simprod(0.04, 0.035, t)*ind(0.04, 0.035, 0.0933, t) - rep(xt, t)*simmul(0.04, 0.0933, t)*ind(0.04, 0.035, 0.0933, t)
     return(max(sum(val), 25*366))
}

# Simulacion - Aporte promedio 25 usd - mayor a 180 meses
Rsup <- sapply(seq(180,400), function(i){ResPat(25,i)})

Res <- function(xt, t){
     sxt <- xt*t
     r <- 2*(0.0764/12)
     res <- t*sxt*r + 0.5*(366)
     return(res)
}

# Simulacion - Aporte promedio 25 usd - menor a 180 meses

Rinf <- sapply(seq(1,179), function(i){Res(25,i)})

# Curvas pagos
plot(cbind(seq(180,400), Rsup), xlim=c(0,400), ylim=c(0,30000), ylab='Valor', col="green", main='Aporte 25')
par(new=TRUE)
plot(cbind(seq(1,179), Rinf), xlim=c(0,400), ylim=c(0,30000), ylab='Valor', col="red", main='Aporte 25')
