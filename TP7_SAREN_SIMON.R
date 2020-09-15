# TP7 - Phi(z) and root finding problem
#SENGOUT SARAVANAN 111 166 828 
#SIMON-PIERRE BOUCHER 111 062 211

# Dans le code qui va suivre nous cherchons à trouver les zéro d'une fonction donné avec la secant method 
# Dans le but de trouver les z à partir de notre 100percentile, qui nous sont donnée 
# nous appliquons le code pour la Simpson's rule 
simpson_n <- function(ftn, a, b, n = 100){ 
  n <- max(c(2*(n %/% 2), 4))
  h <- (b-a)/n
  x.vec1 <- seq(a+h, b-h, by = 2*h) 
  x.vec2 <- seq(a+2*h, b-2*h, by = 2*h) 
  f.vec1 <- sapply(x.vec1, ftn)
  f.vec2 <- sapply(x.vec2, ftn)
  S <- h/3*(ftn(a) + ftn(b) + 4*sum(f.vec1) + 2*sum(f.vec2)) 
  return(S)
}

# Lois de Gauss : densité et réaprtition
# Dans la fonction Phi nous allons inclure la fonction de simpson créer plus haut 
phi <- function(x) return(exp(-x^2/2)/sqrt(2*pi))
Phi <- function(z) {
  if (z < 0) {
    return(0.5 - simpson_n(phi, z, 0))
  } else {
    return(0.5 + simpson_n(phi, 0, z))
  }
}


# Ici on créer la Secant function 
secant1 <- function(fun, x0, x1, tol=1e-07, niter=500){
  for ( i in 1:niter ) {
    x2 <- x1-fun(x1)*(x1-x0)/(fun(x1)-fun(x0))
    if (abs(fun(x2)) < tol)
      return(x2)
    x0 <- x1
    x1 <- x2
  }
  stop("Nombre d'itérations permises dépassé")
} 


#Creation de la function Quantile à l'aide de secant et Phi 
quantile <- function(p){
  f <-function(z) Phi(z) - p
  secant1(f,x0=0,x1=3)
}

# Trouve la valeur de z en applicant la fonction quantile au pourcentage demandé dans la question 
# On va donc trouver les valeur de z pour les percentiles données 
sapply(c(0.5,0.75,0.95,0.975,0.99),quantile)
# Ce qui nous donne les valeurs de z suivangte : 
# 0.5p ----> z=0.0000000
# 0.75p ----> z=0.6744897
# 0.95p ----> z=1.6448536
# 0.975p ----> z=1.9599651 
# 0.99p ----> z=2.3263479


#Vérification de la valeur de z avec la fonction "qnorn" inclus dans r 
sapply(c(0.5,0.75,0.95,0.975,0.99),qnorm)
#Nous voyons ci-bas que les valeurs sont identique à quelques décimals 
# 0.5p ----> z=0.0000000
# 0.75p ----> z=0.6744898
# 0.95p ----> z=1.6448536 
# 0.975p ----> z=1.9599640
# 0.99p ----> z=2.3263479
