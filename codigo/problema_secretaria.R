# Problema secretaria simple
# solo puedo elegir en base a ranking relativo
# y solo estoy contento si elijo al mejor
# osea que maximizo la proba de elegir al mejor
ProblemaSecretaria <- function(n, nrep, proporcion) {
  
  casosfavorables <- 0
  
  for (j in 1:nrep) {
    
    lista_ordenada_candidatos <- sample(1 : n, n, replace = FALSE)
    
    r <- floor(n * proporcion)
    
    candidatos_descartados <- lista_ordenada_candidatos[1 : (r - 1)] 
    posibles_candidatos <- lista_ordenada_candidatos[r : length(lista_ordenada_candidatos)]
    k <- length(posibles_candidatos)

    for (i in 1 : k) {

      if (i == k) {
        contratado <- posibles_candidatos[1]
        break
      }
      if (posibles_candidatos[1] > max(candidatos_descartados)) {
        contratado <- posibles_candidatos[1]
        break
      } else {
        candidatos_descartados <- c(candidatos_descartados, posibles_candidatos[1])
        posibles_candidatos <- posibles_candidatos[-1]
      }
    }
    
    casosfavorables <- casosfavorables + as.integer(contratado == n)
    
  }
  
  proba <- casosfavorables / nrep
  
  return(proba)
  
}

y <- c(ProblemaSecretaria(50, 100000, 0.1),
ProblemaSecretaria(50, 100000, 0.2),
ProblemaSecretaria(50, 100000, 0.35),
ProblemaSecretaria(50, 100000, 1/exp(1)),
ProblemaSecretaria(50, 100000, 0.38),
ProblemaSecretaria(50, 100000, 0.4),
ProblemaSecretaria(50, 100000, 0.5),
ProblemaSecretaria(50, 100000, 0.7),
ProblemaSecretaria(50, 100000, 0.99))

library(ggplot2)

plot(c(0,0.1,0.2,0.35,1/exp(1),0.38,0.4,0.5,0.7,0.99,1),c(0,y,0),type="l",col="blue",xlab="Proporción de n descartada", ylab="Probabilidad de elegir al mejor", main="Secretary problem")
library(ggplot2)
qplot(c(0,0.1,0.2,0.35,1/exp(1),0.38,0.4,0.5,0.7,0.99,1),c(0,y,0),xlab="Proporción de n descartada", ylab="Probabilidad de elegir al mejor", main="Secretary problem (n=50)",geom="smooth",se=FALSE)+geom_abline(slope=1,col="red",intercept = 0,size=1)

# CAYLEY
# KEPLER
# GOOGOL (bayes) basicamente si uso una pareto con alpha tendiendo a 0 tiendo
# a la solucion original

library(LaplacesDemon) # para la distr pareto

# Otros problema secretarias
# distribucion=c("uniforme", "pareto", "beta", "exponencial")
ProblemaSecretariaB2 <- function(n, nrep, distribucion, proporcion, posta=FALSE, alpha=1,rate=1) {
  
  valorobtenido <- 0
  
  r <- floor(n * proporcion)
  
  
  for (j in 1:nrep) {
    
    if (distribucion=="uniforme") {
      lista_ordenada_candidatos <- runif(n)  
      if (posta) {
        r <- floor(sqrt(n))
      }
    }
    if (distribucion=="pareto") {
      lista_ordenada_candidatos <- rpareto(n, alpha = alpha)
      if (posta) {
        r <- floor(n*alpha^(-alpha/(alpha-1)))
      }
    }
    if (distribucion=="beta") {
      lista_ordenada_candidatos <- rbeta(n, alpha = alpha, beta = 1)
      if (posta) {
        r <- floor(n^(alpha/(alpha+1))*(gamma(1+1/alpha)/alpha)^(alpha/(alpha+1)))
      }
    }
    if (distribucion=="exponencial") {
      lista_ordenada_candidatos <- rexp(n, rate=0.1)
      if (posta) {
        r <- floor(n/log(n))
      }
    }
    
    if (r > 0) {
      candidatos_descartados <- lista_ordenada_candidatos[1 : (r - 1)] 
      posibles_candidatos <- lista_ordenada_candidatos[r : length(lista_ordenada_candidatos)]
      k <- length(posibles_candidatos)
      
      for (i in 1 : k) {
        
        if (i == k) {
          contratado <- posibles_candidatos[1]
          break
        }
        if (posibles_candidatos[1] > max(candidatos_descartados)) {
          contratado <- posibles_candidatos[1]
          break
        } else {
          candidatos_descartados <- c(candidatos_descartados, posibles_candidatos[1])
          posibles_candidatos <- posibles_candidatos[-1]
        }
      }
      
      valorobtenido <- valorobtenido + contratado 
    } else {
      valorobtenido <- lista_ordenada_candidatos[1]
    }
    
    
  }
  
  valoresperado <- valorobtenido / nrep
  
  return(valoresperado)
  
}
ProblemaSecretariaB2(200,10000,"uniforme",proporcion = 0.9)

ProporcionOptimaTeorica <- function(distribucion, n, alpha = 1) {
  if (distribucion=="uniforme") {
    r <- sqrt(n)
  }
  if (distribucion=="pareto") {
    r <- n*alpha^(-alpha/(alpha-1))
  }
  if (distribucion=="beta") {
    r <- n^(alpha/(alpha+1))*(gamma(1+1/alpha)/alpha)^(alpha/(alpha+1))
  }
  if (distribucion=="exponencial") {
    r <- n/log(n)
  }
  return(r/n)
}

n <- 200
nrep <- 10000
distribucion <- "uniforme"
secuencia<-seq(0,0.5,0.01)
y.1<-c()
for(i in 1:length(secuencia)){
  y.1 <- c(y.1, ProblemaSecretariaB2(n,nrep,distribucion,proporcion = secuencia[i]))
}
plot(secuencia,y.1,main="Secretaria (b2) - Uniforme (n=200)",type="l",lwd=2, xlab="proporcion",ylab="Valor esperado")
points(ProporcionOptimaTeorica("uniforme",n),ProblemaSecretariaB2(n,nrep,distribucion,proporcion = ProporcionOptimaTeorica("uniforme",n)),col="red")

n <- 200
nrep <- 100000
distribucion <- "pareto"
alpha <- 1.5
secuencia<-seq(0,1,0.025)
y.2<-c()
for(i in 1:length(secuencia)){
  y.2 <- c(y.2, ProblemaSecretariaB2(n,nrep,distribucion,proporcion = secuencia[i], alpha=alpha))
}
plot(secuencia,y.2,main="Secretaria (b2) - Pareto (n=200)",type="l",lwd=2,xlab="proporcion",ylab="Valor esperado")
points(ProporcionOptimaTeorica("pareto",n, alpha),ProblemaSecretariaB2(n,nrep,distribucion,proporcion = ProporcionOptimaTeorica("pareto",n,alpha),alpha=alpha),col="red")

n <- 200
nrep <- 10000
distribucion <- "beta"
alpha <- 5
secuencia<-seq(0,1,0.05)
y.3<-c()
for(i in 1:length(secuencia)){
  y.3 <- c(y.3, ProblemaSecretariaB2(n,nrep,distribucion,proporcion = secuencia[i], alpha=alpha))
}
plot(secuencia,y.3,main="Secretaria (b2) - Beta (n=200)",type="l",lwd=2,xlab="proporcion",ylab="Valor esperado")
points(ProporcionOptimaTeorica("beta",n, alpha),ProblemaSecretariaB2(n,nrep,distribucion,proporcion = ProporcionOptimaTeorica("beta",n,alpha),alpha=alpha),col="red")


