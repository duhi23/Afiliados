######################################################
#####     Proyeccion Afiliados por Provincia     #####
#####           Estimacion por Splines           #####
######################################################

library('forecast')
library('readxl')
library('dplyr')
library('changepoint')
library('tsoutliers')
library('expsmooth')
library('fma')
ls("package:forecast")

data <- read_excel("Cotizantes.xlsx", sheet = 1)
data <- data %>% select(-Periodo)
glimpse(data)

# Analisis de perfiles

perfil <- prop.table(as.matrix(data),2)
plot(perfil[48,], type='b', col="red", ylim=c(0,0.015))
par(new=TRUE)
plot(perfil[60,], type='b', col="yellow", ylim=c(0,0.015))
par(new=TRUE)
plot(perfil[72,], type='b', col="gray", ylim=c(0,0.015))
par(new=TRUE)
plot(perfil[84,], type='b', col="green", ylim=c(0,0.015))
par(new=TRUE)
plot(perfil[96,], type='b', col="orange", ylim=c(0,0.015))
par(new=TRUE)
plot(perfil[108,], type='b', col="black", ylim=c(0,0.015))
par(new=TRUE)
plot(perfil[120,], type='b', col="darkred", ylim=c(0,0.015))
par(new=TRUE)
plot(perfil[132,], type='b', col="blue", ylim=c(0,0.015))


# Analisis de Series Temporales

data <- ts(data, start = c(2005), frequency = 12)
data12 <- ts(data[61:120,], start = c(2010), frequency = 12) # retiramos ultimos 6 meses

pred <- matrix(0, ncol=24, nrow=12)

for(j in 1:24){
     pred[,j] <- forecast(auto.arima(data12[,j]), h=12)$mean
}


colnames(pred) <- colnames(data[121:132,])

matdif <- data[121:132,] - pred

lapply(seq(1,24), function(i){auto.arima(matdif[,i])})

plot(forecast(auto.arima(matdif[,1]), h=12))

# Correccion de medias
matmean <- matrix(0, nrow=nrow(data12), ncol=ncol(data12))

for(j in 1:24){
     index <- cpt.mean(diff(data12[,j]))@cpts[1]
     if(index > 1){
          matmean[index:78, j] <- cpt.mean(diff(data12[,j]))@param.est$mean[2] - cpt.mean(diff(data12[,j]))@param.est$mean[1]
     }
}

vecmax <- apply(matmean, 2, min)
     
nmat <- as.matrix(data12)-matmean
class(nmat)

matpro <- matrix(0, nrow=42, ncol=24)
for(j in 1:24){
     matpro[,j] <- round(as.vector(forecast(auto.arima(nmat[,j]), h=42)$mean), 0)
}

colnames(matpro) <- colnames(data12)
matpro <- ts(matpro, start = c(2015, 7), frequency = 12)

plot(cbind(seq(1,132), as.vector(data[,1])), xlim=c(1,168), ylim=c(0, 220000), col="red")
par(new=TRUE)
plot(cbind(seq(126, 167), as.vector(matpro[,1])+vecmax[1]), xlim=c(1,168), ylim=c(0, 220000), col="blue")

# Ajuste por Splines
plot(splinef(data[,19], h=36, method="mle"))


# Analisis de intervencion
ls("package:changepoint")

for(i in 1:24){
     plot(forecast(auto.arima(data12[,i]), h=36))
}


vecmean <- numeric(length(data[,1]))
index <- cpt.mean(diff(data[,1]))@cpts
vecmean[1:index[1]] <- cpt.mean(diff(data[,1]))@param.est$mean[1]
vecmean[(index[1]+1):(index[2]+1)] <- cpt.mean(diff(data[,1]))@param.est$mean[2]

plot(as.vector(data[,1]) - vecmean, type='l')

plot(forecast(auto.arima(as.vector(data[,1]) - vecmean), h=24))

for(i in 1:24){
     #plot(data12[,i], main=colnames(data12)[i])
     plot(cpt.mean(data12[,i]), main=colnames(data12)[i])
     plot(cpt.mean(diff(data12[,i])), main=colnames(data12)[i])
     #print(colnames(data12)[i])
     #print(cpt.mean(diff(data12[,i]))@cpts)
}


