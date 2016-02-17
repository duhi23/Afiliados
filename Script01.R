############################################
#####     Responsabilidad Patronal     #####
############################################

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


# Simulacion
plot(simprod(0.04, 0.035, 400), col="green", ylim=c(0,25), xlim=c(0,400), type='l', lwd=2, main="Simulación", ylab='')
par(new=TRUE)
plot(simmul(0.04, 0.0933, 400), col="red", ylim=c(0,25), xlim=c(0,400), type='l', lwd=2, main="Simulación", ylab='')
