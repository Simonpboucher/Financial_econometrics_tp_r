# MINI-PROJET : GARCH AND COPULA 
#SENGOUT SARAVANAN 111 166 828 
#SIMON-PIERRE BOUCHER 111 062 211

rm(list=ls())


install.packages("graphics")
install.packages("stats")
install.packages("utils")
install.packages("quadprog")
install.packages("zoo")
install.packages("xts")
install.packages("TTR")
install.packages("quantmod")
install.packages("tseries")
install.packages("copula")
install.packages("rugarch")
install.packages("parallel")

library("graphics")
library("stats")
library("utils")
library("quadprog")
library("zoo")
library("xts")
library("TTR")
library("quantmod")
library("tseries")
library("gsl")
library("copula")
library("parallel")
library("rugarch")



#Ici nous allons chercher le cours de bourse journalier  pour le S&P500 et IBM, de 2000-01-01 à 2012-11-23
#Il s'agit de la periode total soit la période pour modéliser et faire une prédiction 
SP500 <- get.hist.quote(instrument = "^gspc", start = "2000-01-01",end= "2012-11-23", quote = "AdjClose",compression = "d") 
IBM <- get.hist.quote(instrument = "IBM", start = "2000-01-01",end= "2012-11-23", quote = "AdjClose",compression = "d") 

#Ici nous allons chercher le cours de bourse journalier  pour le S&P500 et IBM, de 2000-01-01 à 2011-11-23
#Il s'agit de la periode partiel afin de réalisé l'estimation et par la suite faire une estimation out of sample
SP500_S <- get.hist.quote(instrument = "^gspc", start = "2000-01-01",end= "2011-11-23", quote = "AdjClose",compression = "d") 
IBM_S <- get.hist.quote(instrument = "IBM", start = "2000-01-01",end= "2011-11-23", quote = "AdjClose",compression = "d") 




#Ici nous allons calculer les log rendements avec les prix de nos titres.
Tsize <- length(SP500)
ret_SP500 <- 100*diff(log(SP500))

Tsize <- length(SP500_S)
ret_SP500_S <- 100*diff(log(SP500_S))
abs_ret_SP500_S <- abs(ret_SP500)

Tsize <- length(IBM)
ret_IBM <- 100*diff(log(IBM))

Tsize <- length(IBM_S)
ret_IBM_S <-100*diff(log(IBM_S))
abs_ret_IBM_S <- abs(ret_SP500)

#Ici nous allons produire un graphique pour les rendements de chaques titres 
plot(ret_SP500_S, main="Return - S&P 500", col="black", xlab="Year", ylab="Return (%)")
plot(abs_ret_SP500_S, main="Absolute Return - S&P 500", col="black", xlab="Year", ylab="Return (%)")

plot(ret_IBM_S, main="Return - IBM", col="black", xlab="Year", ylab="Return (%)")
plot(abs_ret_IBM_S, main="Absolute Return - IBM", col="black", xlab="Year", ylab="Return (%)")


#Ici nous allons créer une fonction qui nous permets d'avoir un sommaire sur les moments de la distribution des rendements.
stat <- function(x) {
  m<-mean(x)
  v<- var(x)
  s<-(mean((x-m)^3))/(sqrt(v)^3)
  k<- (mean((x-m)^4))/v^2
  res <-c(m,v,s,k)
  names(res) <-c("Mean","Variance","Asymetry","Kurtosis")
  return(res)
}

stat(ret_SP500_S)
stat(ret_IBM_S)

# On estime le modèle GARCH (1,1) SP500 - GAUSSIAN 

#On commence par fixer une valeur de départ à nos paramètres 
TT <- length(ret_SP500_S)
yy <- matrix(0,TT,1)
yy <- ret_SP500_S
hh <- matrix(0,TT,1)
a0 <- 0.001
a1 <- 0.06
a2 <- 0.92
mu <- 0.01

#On fixe égalemnent une valeur de départ pour notre variance, nous avons choisis la variance déterminer de manière statistique
#Le choix de de la variance inconditionnel est également disponnible dans le code 
hh[1] <-(sd(yy))^2
# hh[1] <- a0/(1-a1-a2)

#Nous créons une fonction qui nous permettra de maximiser notre vraissamblance 
like <- function(pars){
  
  TT <- length(yy)
  
  a0 <- exp(pars[1])
  a1 <- exp(pars[2])
  a2 <- exp(pars[3])
  mu <- exp(pars[4])
  ans <- -1e10  
  
  llike <- matrix(0,TT,1)
  
  if (a1 + a2 < 1)
    if(a0 > 0) 
      if(a1 >= 0)
        if(a2 >= 0)  
        {
          
          hh <- matrix(0,TT,1)
          
          hh[1] <- a0/(1-a1 - a2)
          
          for (t in 2:TT){
            hh[t] <- a0 + a1*((yy[t-1]-mu)^2) + a2*hh[t-1]
            llike[t] <- -0.5*log(2*pi) - 0.5*log(hh[t]) - 0.5*(yy[t]-mu)^2 /hh[t]
          }
          
          ans <- sum(llike[2:TT])
          
        }
 
  
  return(ans)
  
}

# Dans les ligne de codes suivantes on demande à optim de maximiser notre fonction de vraissamblance 
pars0 <- c(log(a0),log(a1),log(a2),log(mu))

pars1 <- optim(pars0,like,control=list(fnscale=-1))$par

pars1[1:4] <- exp(pars1[1:4])


print(pars1)

a0_hat <- pars1[1] 
a1_hat <- pars1[2] 
a2_hat <- pars1[3] 
mu_hat <- pars1[4] 
a0_hat_SP500G <- pars1[1] 
a1_hat_SP500G <- pars1[2] 
a2_hat_SP500G <- pars1[3] 
mu_hat_SP500G <- pars1[4] 


# Ici on fixe une valeur de départ pour la variance avec la mesure inconditionnel et nous trouvons les valeurs suivante par itération
hh[1] <- a0_hat/(1-a1_hat-a2_hat)
yyy <- matrix(0,TT,1)
yyy[1] <- yy[1]
for (t in 2:TT){
  hh[t] <- a0 + a1*((yyy[t-1]-mu_hat)^2) + a2*hh[t-1]
  yyy[t] <-mu_hat + sqrt(hh[t-1])
}

var_cond_SP500_G <- hh[1:TT]








# On estime le modèle GARCH (1,1) SP500 - STUDENT 

#On commence par fixer une valeur de départ à nos paramètres 
TT <- length(ret_SP500_S)
yy <- matrix(0,TT,1)
yy <- ret_SP500_S
hh <- matrix(0,TT,1)
a0 <- 0.001
a1 <- 0.06
a2 <- 0.92
mu <- 0.01
v <- 2

#On fixe égalemnent une valeur de départ pour notre variance, nous avons choisis la variance déterminer de manière statistique
#Le choix de de la variance inconditionnel est également disponnible dans le code 
hh[1] <-sd(yy)
# hh[1] <- a0/(1-a1-a2)



like <- function(pars){
  
  TT <- length(yy)
  
  a0 <- exp(pars[1])
  a1 <- exp(pars[2])
  a2 <- exp(pars[3])
  v  <- exp(pars[4])
  mu <- exp(pars[5])
  

  
  ans <- -1e10  
  
  #Nous créons une fonction qui nous permettra de maximiser notre vraissamblance 
  llike <- matrix(0,TT,1)
  
  if (a1 + a2 < 1) 
    if (v > 2)
      if(a0 > 0) 
        if(a1 >= 0)
          if(a2 >= 0)  
          {
            
            hh <- matrix(0,TT,1)
            
            hh[1] <- a0/(1-a1 - a2)
            
            for (t in 2:TT){
              hh[t] <- a0 + a1*((yy[t-1]-mu)^2) + a2*hh[t-1]
              llike[t] <- lgamma((v+1)/2) - log((pi*v)^(0.5))-lgamma(v/2) - ((v+1)/2)*log(1+(((yy[t]-mu)^2)/hh[t])/v)
            }
            
            ans <- sum(llike[2:TT])
            
          }

  
  return(ans)
  
}
# Dans les ligne de codes suivantes on demande à optim de maximiser notre fonction de vraissamblance 
pars0 <- c(log(a0),log(a1),log(a2), log(v), log(mu))

pars1 <- optim(pars0,like,control=list(fnscale=-1))$par

pars1[1:5] <- exp(pars1[1:5])


print(pars1)

a0_hat <- pars1[1] 
a1_hat <- pars1[2] 
a2_hat <- pars1[3] 
v_hat <- pars1[4]
mu_hat <- pars1[5]
a0_hat_SP500T <- pars1[1] 
a1_hat_SP500T <- pars1[2] 
a2_hat_SP500T <- pars1[3] 
v_hat_SP500T <- pars1[4]
mu_hat_SP500T <- pars1[5]


# Ici on fixe une valeur de départ pour la variance avec la mesure inconditionnel et nous trouvons les valeurs suivante par itération
hh[1] <- a0_hat/(1-a1_hat-a2_hat)
yyy <- matrix(0,TT,1)
yyy[1] <- yy[1]
for (t in 2:TT){
  hh[t] <- a0 + a1*((yyy[t-1]-mu_hat)^2) + a2*hh[t-1]
  yyy[t] <-mu_hat + sqrt(hh[t-1])
}
var_cond_SP500_T <- hh[1:TT]






# On estime le modèle GARCH (1,1) IBM - GAUSSIEN

#On commence par fixer une valeur de départ à nos paramètres 
TT <- length(ret_IBM_S) 
yy <- matrix(0,TT,1)
yy <- ret_IBM_S
hh <- matrix(0,TT,1)
a0 <- 0.001
a1 <- 0.06
a2 <- 0.92
mu <- 0.01

#On fixe égalemnent une valeur de départ pour notre variance, nous avons choisis la variance déterminer de manière statistique
#Le choix de de la variance inconditionnel est également disponnible dans le code 
hh[1] <-sd(yy)
# hh[1] <- a0/(1-a1-a2)


like <- function(pars){
  
  TT <- length(yy)
  
  a0 <- exp(pars[1])
  a1 <- exp(pars[2])
  a2 <- exp(pars[3])
  mu <- exp(pars[4])
  
  ans <- -1e10  #penalty value
  
  #Nous créons une fonction qui nous permettra de maximiser notre vraissamblance 
  llike <- matrix(0,TT,1)
  
  if (a1 + a2 < 1)
    if(a0 > 0) 
      if(a1 >= 0)
        if(a2 >= 0)  
        {
          
          hh <- matrix(0,TT,1)
          
          hh[1] <- a0/(1-a1 - a2)
          
          for (t in 2:TT){
            hh[t] <- a0 + a1*((yy[t-1]-mu)^2) + a2*hh[t-1]
            llike[t] <- -0.5*log(2*pi) - 0.5*log(hh[t]) - 0.5*((yy[t]-mu)^2 /hh[t])
          }
          
          ans <- sum(llike[2:TT])
          
        }
  #  print(c(a0,a1,a2,ans))
  
  return(ans)
  
}
# Dans les ligne de codes suivantes on demande à optim de maximiser notre fonction de vraissamblance 
pars0 <- c(log(a0),log(a1),log(a2), log(mu))

pars1 <- optim(pars0,like,control=list(fnscale=-1))$par

pars1[1:4] <- exp(pars1[1:4])

print(pars1)
a0_hat <- pars1[1] 
a1_hat <- pars1[2] 
a2_hat <- pars1[3] 
mu_hat <- pars1[4] 
a0_hat_IBMG <- pars1[1] 
a1_hat_IBMG <- pars1[2] 
a2_hat_IBMG <- pars1[3] 
mu_hat_IBMG <- pars1[4] 

# Ici on fixe une valeur de départ pour la variance avec la mesure inconditionnel et nous trouvons les valeurs suivante par itération

hh[1] <- a0_hat/(1-a1_hat-a2_hat)
yyy <- matrix(0,TT,1)
yyy[1] <- yy[1]
for (t in 2:TT){
  hh[t] <- a0 + a1*((yyy[t-1]-mu_hat)^2) + a2*hh[t-1]
  yyy[t] <-mu_hat + sqrt(hh[t-1])
}

var_cond_IBM_G <- hh[1:TT]






# On estime le modèle GARCH (1,1) IBM - STUDENT

#On commence par fixer une valeur de départ à nos paramètres 
TT <- length(ret_IBM_S)
yy <- matrix(0,TT,1)
yy <- ret_IBM_S
hh <- matrix(0,TT,1)
a0 <- 0.001
a1 <- 0.06
a2 <- 0.92
v <- 2
mu <- 0.01
#On fixe égalemnent une valeur de départ pour notre variance, nous avons choisis la variance déterminer de manière statistique
#Le choix de de la variance inconditionnel est également disponnible dans le code 

hh[1] <-(sd(yy))^2
# hh[1] <- a0/(1-a1-a2)


#Nous créons une fonction qui nous permettra de maximiser notre vraissamblance 
like <- function(pars){
  
  TT <- length(yy)
  
  a0 <- exp(pars[1])
  a1 <- exp(pars[2])
  a2 <- exp(pars[3])
  v  <- exp(pars[4])
  mu  <- exp(pars[5])
  
  ans <- -1e10  #penalty value
  
  llike <- matrix(0,TT,1)
  
  if (a1 + a2 < 1) 
    if (v > 2)
      if(a0 > 0) 
        if(a1 >= 0)
          if(a2 >= 0)  
          {
            
            hh <- matrix(0,TT,1)
            
            hh[1] <- a0/(1-a1 - a2)
            
            for (t in 2:TT){
              
              hh[t] <- a0 + a1*((yy[t-1]-mu)^2) + a2*hh[t-1]
              llike[t] <- lgamma((v+1)/2) - log((pi*v)^(0.5))-lgamma(v/2) - ((v+1)/2)*log(1+(((yy[t]-mu)^2)/hh[t])/v)
            }
            
            ans <- sum(llike[2:TT])
            
          }
  #  print(c(a0,a1,a2,v,ans))
  
  return(ans)
  
}

# Dans les ligne de codes suivantes on demande à optim de maximiser notre fonction de vraissamblance 
pars0 <- c(log(a0),log(a1),log(a2), log(v), log(mu))

pars1 <- optim(pars0,like,control=list(fnscale=-1))$par

pars1[1:5] <- exp(pars1[1:5])


print(pars1)

a0_hat <- pars1[1] 
a1_hat <- pars1[2] 
a2_hat <- pars1[3] 
v_hat <- pars1[4] 
mu_hat <- pars1[5] 
a0_hat_IBMT <- pars1[1] 
a1_hat_IBMT <- pars1[2] 
a2_hat_IBMT <- pars1[3] 
v_hat_IBMT <- pars1[4] 
mu_hat_IBMT <- pars1[5] 

# Ici on fixe une valeur de départ pour la variance avec la mesure inconditionnel et nous trouvons les valeurs suivante par itération

hh[1] <- a0_hat/(1-a1_hat-a2_hat)
yyy <- matrix(0,TT,1)
yyy[1] <- yy[1]
for (t in 2:TT){
  hh[t] <- a0 + a1*((yyy[t-1]-mu_hat)^2) + a2*hh[t-1]
  yyy[t] <-mu_hat + sqrt(hh[t-1])
}

var_cond_IBM_T <- hh[1:TT]

# Estimation de la Copule Normal- GARCH NORMAL 
N<- 0.5
NN <- 0.5

NCopula.L <- function(cpars){
  
  theta <- cpars
  clike <- matrix(0,TT,1)
  temp <- normalCopula( param = theta, dim = 2, dispstr = "ex" )
  
  
  u1 <- pnorm((ret_IBM_S[seq(from=1,to=TT)]-mu_hat_IBMG)/sqrt(var_cond_IBM_G))
  u2 <- pnorm((ret_SP500_S[seq(from=1,to=TT)]-mu_hat_SP500G)/sqrt(var_cond_SP500_G)) 
  
  
  

  
  
  clike <-  dCopula( matrix(cbind(u1,u2),nrow = TT, ncol= 2), temp,log=FALSE) 
  
  ans <- sum(clike)
  
  return(ans)
}


NCopula.L(cpars = N )
NCopula.L(cpars = NN )

CopulParam.nn <- optimize(NCopula.L,lower=-0.98,upper=0.99, maximum = TRUE)

CopulParam.nn

u1N_N <- pnorm((ret_IBM_S[seq(from=1,to=TT)]-mu_hat_IBMG)/sqrt(var_cond_IBM_G ))
u2N_N <- pnorm((ret_SP500_S[seq(from=1,to=TT)]-mu_hat_SP500G)/sqrt(var_cond_SP500_G)) 
plot(u1N_N,u2N_N)

# Estimation de la Copule Normal - GARCH STUDENT
S<- 0.5
SS <- 0.65

NCopula.L <- function(cpars){
  
  theta <- cpars
  clike <- matrix(0,TT,1)
  temp <- normalCopula( param = theta, dim = 2, dispstr = "ex" )
  
  

  u1 <- pt((ret_IBM_S[seq(from=1,to=TT)]-mu_hat_IBMT)/sqrt(var_cond_IBM_T) ,df=v_hat_IBMT)
  u2 <- pt((ret_SP500_S[seq(from=1,to=TT)]-mu_hat_SP500T)/sqrt(var_cond_SP500_T) ,df=v_hat_SP500T) 
  
  
  clike <-  dCopula( matrix(cbind(u1,u2),nrow = TT, ncol= 2), temp,log=FALSE) 
  
  ans <- sum(clike)
  
  return(ans)
}


NCopula.L(cpars = S )
NCopula.L(cpars = SS )

CopulParam.nt <- optimize(NCopula.L,lower=-0.98,upper=0.99, maximum = TRUE)

CopulParam.nt
u1N_T <- pt((ret_IBM_S[seq(from=1,to=TT)]-mu_hat_IBMT)/sqrt(var_cond_IBM_T) ,df=v_hat_IBMT)
u2N_T <- pt((ret_SP500_S[seq(from=1,to=TT)]-mu_hat_SP500T)/sqrt(var_cond_SP500_T) ,df=v_hat_SP500T) 
plot(u1N_T,u2N_T)




# Estimation de la Copule de student -  GARCH STUDENT

S<- c(0.5,2)
SS <- c(0.5,5)

Tcopula.L <- function(cpars){
  
  theta <- cpars
  clike <- matrix(0,TT,1)
  temp <- tCopula( param = theta[1], dim = 2, df=theta[2], dispstr = "ex" )
  
  
  
  
  u1 <- pt((ret_IBM_S[seq(from=1,to=TT)]-mu_hat_IBMT)/sqrt(var_cond_IBM_T),df=v_hat_IBMT)
  u2 <- pt((ret_SP500_S[seq(from=1,to=TT)]-mu_hat_SP500T)/sqrt(var_cond_SP500_T),df=v_hat_SP500T) 
  

  
  
  clike <-  dCopula( matrix(cbind(u1,u2),nrow = TT, ncol= 2), temp,log=FALSE) 
  
  ans <- sum(clike)
  
  return(ans)
}

Tcopula.L(cpars = S )
Tcopula.L(cpars = SS )

CopulParam.tt <- optim(Tc00,Tcopula.L, method= "L-BFGS-B", lower=c(-0.98,1),upper=c(0.98,100),control=list(fnscale=-1))

CopulParam.tt
u1T_T <- pt((ret_IBM_S[seq(from=1,to=TT)]-mu_hat_IBMT)/sqrt(var_cond_IBM_T),df=v_hat_IBMT)
u2T_T <- pt((ret_SP500_S[seq(from=1,to=TT)]-mu_hat_SP500T)/sqrt(var_cond_SP500_T),df=v_hat_SP500T) 
plot(u1T_T,u2T_T)


# Estimation de la Copule de student - GARCH NORMAL 

N<- 0.6
NN <- 0.5140325

Tcopula.L <- function(cpars){
  
  theta <- cpars
  clike <- matrix(0,TT,1)
  temp <- tCopula( param = theta, dispstr = "ex" )
  
  
  
  
  u1 <- pnorm((ret_IBM_S[seq(from=1,to=TT)]-mu_hat_IBMG)/sqrt(var_cond_IBM_G))
  u2 <- pnorm((ret_SP500_S[seq(from=1,to=TT)]-mu_hat_SP500G)/sqrt(var_cond_SP500_G)) 
  
  
  
  
  clike <-  dCopula( matrix(cbind(u1,u2),nrow = TT, ncol= 2), temp,log=FALSE) 
  
  ans <- sum(clike)
  
  return(ans)
}

Tcopula.L(cpars = N )
Tcopula.L(cpars = NN )

CopulParam.tn <- optim(Tc00,Tcopula.L,method= "L-BFGS-B", lower=c(-0.98,1),upper=c(0.98,100),control=list(fnscale=-1))

u1T_G <- pnorm((ret_IBM_S[seq(from=1,to=TT)]-mu_hat_IBMG)/sqrt(var_cond_IBM_G))
u2T_G <- pnorm((ret_SP500_S[seq(from=1,to=TT)]-mu_hat_SP500G)/sqrt(var_cond_SP500_G)) 
plot(u1T_G,u2T_G)

# Estimation de la Copule de clayton  - GARCH NORMAL 
N<- 3
NN <- 2

ClayCopula.L <- function(cpars){
  
  theta <- cpars
  clike <- matrix(0,TT,1)
  temp <- claytonCopula( param = theta)
  
  
  u1 <- pnorm((ret_IBM_S[seq(from=1,to=TT)]-mu_hat_IBMG)/sqrt(var_cond_IBM_G))  
  u2 <- pnorm((ret_SP500_S[seq(from=1,to=TT)]-mu_hat_SP500G)/sqrt(var_cond_SP500_G)) 
  
  
  
  
  clike <-  dCopula( matrix(cbind(u1,u2),nrow = TT, ncol= 2), temp,log=FALSE) 
  
  ans <- sum(clike)
  
  return(ans)
}

ClayCopula.L(cpars = N )
ClayCopula.L(cpars = NN )

CopulParam.cN <- optimize(ClayCopula.L,lower=-0.98,upper=100, maximum = TRUE)
CopulParam.cN
u1C_N <- pnorm((ret_IBM_S[seq(from=1,to=TT)]-mu_hat_IBMG)/sqrt(var_cond_IBM_G))  
u2C_N <- pnorm((ret_SP500_S[seq(from=1,to=TT)]-mu_hat_SP500G)/sqrt(var_cond_SP500_G)) 
plot(u1C_N,u2C_N)

# Estimation de la Copule de clayton- GARCH STUDENT 
S<- 3
SS <- 2

ClayCopula.L <- function(cpars){
  
  theta <- cpars
  clike <- matrix(0,TT,1)
  temp <- claytonCopula( param = theta, dim = 2 )
  
  

  
  u1 <- pt((ret_IBM_S[seq(from=1,to=TT)]-mu_hat_IBMT)/sqrt(var_cond_IBM_T) ,df=v_hat_IBMT)   
  u2 <- pt((ret_SP500_S[seq(from=1,to=TT)]-mu_hat_SP500T)/sqrt(var_cond_SP500_T) ,df=v_hat_IBMT) 
  
  clike <-  dCopula( matrix(cbind(u1,u2),nrow = TT, ncol= 2), temp,log=FALSE) 
  
  ans <- sum(clike)
  
  return(ans)
}

ClayCopula.L(cpars = S )
ClayCopula.L(cpars = SS)

CopulParam.cT <- optimize(ClayCopula.L,lower=-0.98,upper=100, maximum = TRUE)
CopulParam.cT
u1C_T <- pt((ret_IBM_S[seq(from=1,to=TT)]-mu_hat_IBMT)/sqrt(var_cond_IBM_T) ,df=v_hat_IBMT)   
u2C_T <- pt((ret_SP500_S[seq(from=1,to=TT)]-mu_hat_SP500T)/sqrt(var_cond_SP500_T) ,df=v_hat_IBMT) 
plot(u1C_T, u2C_T)


# Estimation de la Copule de frank - GARCH NORMAL
N<- 3
NN <- 2
FrankCopula.L <- function(cpars){
  
  theta <- cpars
  clike <- matrix(0,TT,1)
  temp <- frankCopula( param = theta, dim = 2 )
  
  
  u1 <- pnorm((ret_IBM_S[seq(from=1,to=TT)]-mu_hat_IBMG)/sqrt(var_cond_IBM_G)  
  u2 <- pnorm((ret_SP500_S[seq(from=1,to=TT)]-mu_hat_SP500G)/sqrt(var_cond_SP500_G) 
  
  

  clike <-  dCopula( matrix(cbind(u1,u2),nrow = TT, ncol= 2), temp,log=FALSE) 
  
  ans <- sum(clike)
  
  return(ans)
  
}

FrankCopula.L(cpars = N)
FrankCopula.L(cpars = NN )

CopulParam.f <- optimize(FrankCopula.L,lower=-100,upper=100, maximum = TRUE)

u1F_N <- pnorm((ret_IBM_S[seq(from=1,to=TT)]-mu_hat_IBMG)/sqrt(var_cond_IBM_G)  
u2F_N <- pnorm((ret_SP500_S[seq(from=1,to=TT)]-mu_hat_SP500G)/sqrt(var_cond_SP500_G) 
plot(u1F_N,u2F_N)
               
# Estimation de la Copule de frank  - GARCH STUDENT 
S<- 3
SS<- 2
FrankCopula.L <- function(cpars){
  
  theta <- cpars
  clike <- matrix(0,TT,1)
  temp <- frankCopula( param = theta, dim = 2 )
  
  

  
  
  u1 <- pt((ret_IBM_S[seq(from=1,to=TT)]-mu_hat_IBMT)/sqrt(var_cond_IBM_T) ,df=v_hat_IBMT)    
  u2 <- pt((ret_SP500_S[seq(from=1,to=TT)]-mu_hat_SP500T)/sqrt(var_cond_IBM_T) ,df=v_hat_SP500T) 
  
  clike <-  dCopula( matrix(cbind(u1,u2),nrow = TT, ncol= 2), temp,log=FALSE) 
  
  ans <- sum(clike)
  
  return(ans)
}

FrankCopula.L(cpars = S)
FrankCopula.L(cpars = SS)

CopulParam.fT <- optimize(FrankCopula.L, lower=-100,upper=100, maximum = TRUE)
CopulParam.fT 

u1F_T <- pt((ret_IBM_S[seq(from=1,to=TT)]-mu_hat_IBMT)/sqrt(var_cond_IBM_T) ,df=v_hat_IBMT)    
u2F_T <- pt((ret_SP500_S[seq(from=1,to=TT)]-mu_hat_SP500T)/sqrt(var_cond_IBM_T) ,df=v_hat_SP500T) 
plot(u1F_T,u2F_T)

# Ici nous allons calculer la valeur a risque 

TT <- length(ret_SP500_S)
TT2 <- length(ret_SP500)
st<-length(ret_IBM)+1


hh[1] <- a0_hat_SP500G/(1-a1_hat_SP500G-a2_hat_SP500G)

yy <- matrix(0,TT2,1)
yy <- ret_SP500
yyy <- matrix(0,TT2,1)
yyy[1] <- yy[1]
yyy[1]
for (t in 2:TT){
  hh[t] <- a0_hat_SP500G + a1_hat_SP500G*((yyy[t-1]-mu_hat)^2) + a2_hat_SP500G*hh[t-1]
  yyy[t] <-mu_hat_SP500G + sqrt(hh[t-1])
}

Var_Cf <- hh[TT+1:TT2+1]
TT3 <- TT2 - TT
var_cond_IBM_G_F<- var_cond_IBM_G[1:TT3]
var_cond_SP500_G_F<- var_cond_SP500_G[1:TT3]



  ## Variance Cond. for model and forcasting
  

  mu_1 <- mu_hat_SP500G
  a_10 <- a0_hat_SP500G
  a_11 <- a1_hat_SP500G
  beta_1 <- a2_hat_SP500G
  
  mu_2 <- mu_hat_IBMG
  a_20 <- a0_hat_IBMG
  a_21 <- a1_hat_IBMG
  beta_2 <- a2_hat_IBMG
  
  nr_1<-length(ret_IBM)+1
  VAR.Cond <- matrix(0,nr_1,2)
  
  VAR.Cond[1,1] <- a_10/(1-a_11 - beta_1)
  VAR.Cond[1,2] <- a_20/(1-a_21 - beta_2)
  
  
  for (t in 2:nr_1) {
    
    VAR.Cond[t,1] <- a_10 + a_11*((ret_IBM[t-1]-mu_1)^2) + beta_1*VAR.Cond[t-1,1]
    VAR.Cond[t,2] <- a_20 + a_21*((ret_SP500[t-1]-mu_2)^2) + beta_2*VAR.Cond[t-1,2]
  }
  
  Var_Cm <- VAR.Cond[1:TT,]
  Var_Cf <- VAR.Cond[seq(from=TT+1,to=TT2+1),]
  
  
  ### Monte Carlo Simulation and VaR Forecasting
  
  ### Compute Value-at-risk
  B<-100000
  VaR_5pct.n <-replicate(nrow(Var_Cf),0)
  VaR_1pct.n <-replicate(nrow(Var_Cf),0)
  VaR_5pct.t <-replicate(nrow(Var_Cf),0)
  VaR_1pct.t <-replicate(nrow(Var_Cf),0)
  VaR_5pct.c <-replicate(nrow(Var_Cf),0)
  VaR_1pct.c <-replicate(nrow(Var_Cf),0)
  VaR_5pct.f <-replicate(nrow(Var_Cf),0)
  VaR_1pct.f <-replicate(nrow(Var_Cf),0)
  
#Estimation de la VaR avec copule normal
  for (j in 1:nrow(Var_Cf)) {
    portfolio <- replicate(B,0)
    
    u <- rCopula(B,normalCopula(CopulParam.nn$maximum, dim = 2))
    q <- qnorm(u)
    IBM.simulated <- q[,1]*sqrt(Var_Cf[j,1]) + mu_hat_IBMG
    SP_500.simulated <- q[,2]*sqrt(Var_Cf[j,2]) + mu_hat_SP500G
    
    portfolio <- 0.5*(IBM.simulated+SP_500.simulated)
    
    VaR_5pct.n[j] <- quantile(portfolio, probs=0.05) 
    VaR_1pct.n[j] <- quantile(portfolio, probs=0.01) 
  }
  
#Estimation de la VaR avec copule de student
  for (j in 1:nrow(Var_Cf)) {
    portfolio <- replicate(B,0)
    
    u <- rCopula(B,tCopula(CopulParam.t$par[1],dim = 2))

    q <- qnorm(u)
    IBM.simulated <- q[,1]*sqrt(Var_Cf[j,1]) + mu_hat_IBMG
    SP_500.simulated <- q[,2]*sqrt(Var_Cf[j,2]) + mu_hat_SP500G
    
    portfolio <- 0.5*(IBM.simulated+SP_500.simulated)
    
    VaR_5pct.t[j] <- quantile(portfolio, probs=0.05) 
    VaR_1pct.t[j] <- quantile(portfolio, probs=0.01)  
  }
  
  #Estimation de la VaR avec copule de clayton 
  
  for (j in 1:nrow(Var_Cf)) {
    
    portfolio <- replicate(B,0)
    
    u <- rCopula(B,claytonCopula(CopulParam.c$maximum,dim = 2))
  
    q <- qnorm(u)
    IBM.simulated <- q[,1]*sqrt(Var_Cf[j,1]) + mu_hat_IBMG
    SP_500.simulated <- q[,2]*sqrt(Var_Cf[j,2]) + mu_hat_SP500G
    
    portfolio <- 0.5*(IBM.simulated+SP_500.simulated)
    
    VaR_5pct.c[j] <- quantile(portfolio, probs=0.05) 
    VaR_1pct.c[j] <- quantile(portfolio, probs=0.01)
  }
  
  #Estimation de la VaR avec copule de frank 
  
  for (j in 1:nrow(Var_Cf)) {
    
    portfolio <- replicate(B,0)
    
    u <- rCopula(B,frankCopula(CopulParam.f$maximum,dim = 2))
    q <- qnorm(u)
    IBM.simulated <- q[,1]*sqrt(Var_Cf[j,1]) + mu_hat_SP500G
    SP_500.simulated <- q[,2]*sqrt(Var_Cf[j,2]) + mu_hat_IBMG
    
    portfolio <- 0.5*(IBM.simulated+SP_500.simulated)
    
    VaR_5pct.f[j] <- quantile(portfolio, probs=0.05) 
    VaR_1pct.f[j] <- quantile(portfolio, probs=0.01)
  }
  

  VaR_5pct.ff <-  VaR_5pct.f[1:249]
  VaR_1pct.ff <-  VaR_1pct.f[1:249]
  timex <- seq(1:249)
  portfo <- length(portf_return)
  portf_return <- 0.5*(IBM[seq(from=mm+1,to=ff)]+SP_500[seq(from=mm+1,to=ff)])
  plot(x=timex, y=portf_return,type="p",pch=19,col="black",ylim=c(-4,5), ylab="Returns")
  par(new=TRUE)
  plot(timex, VaR_5pct.ff,type="l", col="blue", ylim=c(-4,5),ylab="", xlab="")
  par(new=TRUE)
  plot(timex, VaR_1pct.ff,type="l",col="red",  ylim=c(-4,5),ylab="", xlab="")
  
  VaR_5pct.tt <-  VaR_5pct.t[1:249]
  VaR_1pct.tt <-  VaR_1pct.t[1:249]
  timex <- seq(1:249)
  portfo <- length(portf_return)
  portf_return <- 0.5*(IBM[seq(from=mm+1,to=ff)]+SP_500[seq(from=mm+1,to=ff)])
  plot(x=timex, y=portf_return,type="p",pch=19,col="black",ylim=c(-4,5), ylab="Returns")
  par(new=TRUE)
  plot(timex, VaR_5pct.tt,type="l", col="blue", ylim=c(-4,5),ylab="", xlab="")
  par(new=TRUE)
  plot(timex, VaR_1pct.tt,type="l",col="red",  ylim=c(-4,5),ylab="", xlab="")
  
  VaR_5pct.nn <-  VaR_5pct.n[1:249]
  VaR_1pct.nn <-  VaR_1pct.n[1:249]
  timex <- seq(1:249)
  portfo <- length(portf_return)
  portf_return <- 0.5*(IBM[seq(from=mm+1,to=ff)]+SP_500[seq(from=mm+1,to=ff)])
  plot(x=timex, y=portf_return,type="p",pch=19,col="black",ylim=c(-4,5), ylab="Returns")
  par(new=TRUE)
  plot(timex, VaR_5pct.nn,type="l", col="blue", ylim=c(-4,5),ylab="", xlab="")
  par(new=TRUE)
  plot(timex, VaR_1pct.nn,type="l",col="red",  ylim=c(-4,5),ylab="", xlab="")
  
  VaR_5pct.cc <-  VaR_5pct.c[1:249]
  VaR_1pct.cc <-  VaR_1pct.c[1:249]
  timex <- seq(1:249)
  portfo <- length(portf_return)
  portf_return <- 0.5*(IBM[seq(from=mm+1,to=ff)]+SP_500[seq(from=mm+1,to=ff)])
  plot(x=timex, y=portf_return,type="p",pch=19,col="black",ylim=c(-4,5), ylab="Returns")
  par(new=TRUE)
  plot(timex, VaR_5pct.cc,type="l", col="blue", ylim=c(-4,5),ylab="", xlab="")
  par(new=TRUE)
  plot(timex, VaR_1pct.cc,type="l",col="red",  ylim=c(-4,5),ylab="", xlab="")
  
  