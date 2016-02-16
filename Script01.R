############################################
#####     Responsabilidad Patronal     #####
############################################

# Funciones basicas
prod <- function(ta, pij, t, k){
mult<-1	
	for(i in k:t){
		mult <- mult*(((1+ta)*(1+pij))^(1/12))
	}
return(mult)
}

simprod <- function(ta, pij, t){
	vec <- vector(length=t)
	for(j in 1:t){
		vec[j] <- prod(ta, pij, t, j)
	}
	return(vec)
}

simmul <- function(ta, mc, t){
	vec <- vector(length=t)
	for(k in 1:t){
		vec[k] <- (1+(ta+mc)/12)*(t-k)
	}
	return(vec)
}

# Interseccion graficas
plot(simprod(0.04,0.035,250), col="green", ylim=c(0,20), xlim=c(150,300))
par(new=TRUE)
plot(simmul(0.04, 0.0933, 250), col="red", ylim=c(0,20), xlim=c(150,300))


dife <- simprod(0.04,0.035,300) - simmul(0.04, 0.0933, 300)
dife[260:300]


