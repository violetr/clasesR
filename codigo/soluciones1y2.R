# Solucion del EJERCICIO 1 - CLASE 1 de R

# auxiliar contar grupos
ContarGrupos <- function(tiro) {
  nrogrupos <- 1
  if (length(tiro) != 1) {
    for (j in 1 : (length(tiro) - 1)) {
      if (tiro[j] != tiro[j + 1]) {
        nrogrupos <- nrogrupos + 1
      }
    }    
  }
  return(nrogrupos)
}

#1.

ProbaExcederXGrupos <- function(n, X, nrep) {
  casosfavorables <- 0
  for (j in 1:nrep) {
    tiradas <- rbinom(n, 1, 0.6) # o sample(c(0,1),n,replace=TRUE)
    casosfavorables <- casosfavorables + as.integer(ContarGrupos(tiradas) > X)
  }  
  proba <- casosfavorables / nrep
  return(proba)
}

ProbaExcederXGrupos(10,5,10000)

#2.

ProbaExactamenteXGrupos <- function(n, X, nrep) {
  casosfavorables <- 0
  for (j in 1:nrep) {
    tiradas <- rbinom(n, 1, 0.6) # o sample(c(0,1),n,replace=TRUE)
    casosfavorables <- casosfavorables + as.integer(ContarGrupos(tiradas) == X)
  }  
  proba <- casosfavorables / nrep
  return(proba)
}

# proba condicional = proba interseccion / proba condicion

ProbaExactamenteXGrupos(10,6,10000)/ProbaExcederXGrupos(10,5,10000)

# Numero esperado de grupos despues de 10 tiradas? despues de 500?

#3.
NumeroEsperadoDeGrupos <- function(n, nrep) {
  suma <- 0
  for (j in 1:nrep) {
    tiradas <- rbinom(n, 1, 0.6)
    suma <- suma + ContarGrupos(tiradas)
  }  
  numero_esperado_grupos <- suma / nrep
  return(numero_esperado_grupos)
}

NumeroEsperadoDeGrupos(10, 10000)

# otra forma (con matriz):

n=10
nrep=10000
MM <- matrix(sample(0 : 1, n*nrep, replace = TRUE, prob = c(0.4, 0.6)), nrow = nrep)
mean(apply(MM, 1, ContarGrupos))

# Solucion del EJERCICIO 2 - CLASE 1 de R

# si cambio vale TRUE entonces mi estrategia es cambiar y si es FALSE me quedo 
# con lo que elegi

montyhall <- function(cambio, nrep) {
  # cambio es bool y representa la estrategia a seguir
  # nrep es el numero de repeticiones que realizo el experimento
  gano <- 0
  for(i in 1: nrep){
    auto <- sample(c(1,2,3),1)
    elijo <- sample(c(1,2,3),1)
    if (!cambio) {
      gano <- gano + (auto == elijo)
    }else{
      if (auto == elijo) {
        abrir <- sample(setdiff(c(1,2,3),elijo),1,prob=c(0.5,0.5)) 
      } else {
        abrir <- setdiff(c(1,2,3),c(auto,elijo))
      }
      gano <- gano + (auto == setdiff(c(1,2,3),c(elijo,abrir)))
    }
  }
  return(gano/nrep)
}

montyhall(TRUE, 100000)
montyhall(FALSE, 100000)



