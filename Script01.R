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
