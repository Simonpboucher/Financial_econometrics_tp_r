# TP8 - Gradient et Hessian 
#SENGOUT SARAVANAN 111 166 828 
#SIMON-PIERRE BOUCHER 111 062 211

install.packages("gridExtra")
library("gridExtra")
install.packages("lattice")
library("lattice")
install.packages('latticeExtra') 
library('latticeExtra') 
install.packages("numDeriv")
library("numDeriv")
install.packages('graphics')
library('graphics')
install.packages('sp')
library('sp')
install.packages('rasterVis')
library('rasterVis')

# Chapitre 12 - Question 5 

# Section a) 

# Section sur les Gradient 
# Dans cette partie de code nous allons créer une fonction qui permet de trouver un vecteur gradient d'une fonction 

# 1er version de la fonction gradient 
grad1 <- function(z){
  fun <- function(p) (p[1])^3 + p[1]*(p[2])^2
  f <- fun(z)
  n <- length(z)
  args <- list(eps=1e-8)
  eps <- rep(args$eps, n)
  df <- rep(NA, n)
  for (i in 1:n){
    dx <- z
    dx[i] <- dx[i] + eps[i]
    df[i] <- (fun(dx) - f)/eps[i]
  }
  return(df)
}

#On entre les coordonées demandé dans la question dans notre fonction grad
grad1(c(1,1))
#On voit que cela nous donne 4 et 2 comme valeur 

# 2er version de la fonction gradient 
# Les coordonnées sont entrer de base dans notre fonction dans x0 
f <- function(x){return(x[1]^3+x[1]*x[2]^2)}
x0 <- c(1,1)
e1 <- c(1,0)
e2 <- c(0,1)
gradient <- function(x=x0, e=1e-8){
  res1 <- (f(x+e*e1)-f(x))/e
  res2 <- (f(x+e*e2)-f(x))/e
  return(c(res1,res2))
}
#Verification en exécutant notre fonction 
gradient()
#On voit que cela nous donne 4 et 2 comme valeur 

#On vérifie la valeur du gradient en effectuant la dérivé première par rapport à x
f1 <- function(x,y){
  3*x^2 + y^2
}

f1(1,1)
#On voit ici que la valeur est 4, ce que confirme que notre fonction gradient fonction 



#On vérifie la valeur du gradient en effectuant la dérivé première par rapport à y
f2 <- function(x,y){
  2*x*y
}

f2(1,1)

#On voit ici que la valeur est 2, ce que confirme que notre fonction gradient fonction 


#On peut également vérifier le gradient directement avec la fonction "grad" inclus dans le package numDeriv
f <- function(x0){return(x0[1]^3+x0[1]*x0[2]^2)}
grad(f, c(1,1))
#On vois que nous arrivons à une valeur de 4 et 2, ce qui confirme la valeur de notre fonction 



# Section sur la Hessian   

# Dans cette partie de code nous allons créer une fonction qui permet de trouver une matrice Hessian d'une fonction

# on commence par définir la fonction de notre équation "f" 

f <- function(x){return(x[1]^3+x[1]*x[2]^2)}
x0 <- c(1,1)
e1 <- c(1,0)
e2 <- c(0,1)

#On créer ici notre fonction qui nous pernmettra de généré la matrice hessian 
hessian1 <- function(x=x0, e=0.0001){
  
  hess <- matrix(0, nrow = 2, ncol = 2)
  hess[1,1] <- (f(x+2*e*e1)-2*f(x+e*e1)+f(x))/(e^2)
  hess[2,2] <- (f(x+2*e*e2)-2*f(x+e*e2)+f(x))/(e^2)
  hess[1,2] <- (f(x+e*e1+e*e2)-f(x+e*e1)-f(x+e*e2)+f(x))/(e^2)
  hess[2,1] <- hess[1,2]
  return(hess)
}

#Verification en exécutant notre fonction 
hessian1()
#On voit que cela nous donne 6, 2, 2 et 2 comme valeur 
# Dérivé seconde par rapport à x ===== 6 
# Dérivé seconde par rapport à y ===== 2 
# Dérivé seconde par rapport à x et y ===== 2 
# Dérivé seconde par rapport à y et x ===== 2 


#On vérifie la valeur de l'Hessian en effectuant la dérivé seconde par rapport à x
f11 <- function(x,y){
  6*x
}

f11(1,1)

#On voit ici que la valeur est 6, ce que confirme que notre fonction Hessian fonction 

#On vérifie la valeur de l'Hessian en effectuant la dérivé seconde par rapport à y
f22 <- function(x,y){
  2*x
}

f22(1,1)
#On voit ici que la valeur est 2, ce que confirme que notre fonction Hessian fonction 


#On vérifie la valeur de l'Hessian en effectuant la dérivé seconde par rapport à x et y 
f12 <- function(x,y){
  2*y
}

f12(1,1)
#On voit ici que la valeur est 2, ce que confirme que notre fonction Hessian fonction 

#On vérifie la valeur de l'Hessian en effectuant la dérivé seconde par rapport à y et x 
f21 <- function(x,y){
  2*y
}

f21(1,1)
#On voit ici que la valeur est 2, ce que confirme que notre fonction Hessian fonction 


#On peut également vérifier la hessian directement avec la fonction "hessian" inclus dans le package numDeriv
f <- function(x0){return(x0[1]^3+x0[1]*x0[2]^2)}
hessian(f, x0, method="Richardson", method.args=list())
#On vois que nous arrivons à des valeurs de 4, 2, 2 et 2, ce qui confirme la valeur de notre fonction 



# section b) 

# Modification de l'Algorithme Steepest ascent method, en remplacant la fonction "grad" inclue dans r par notre fonction  que nous venons de créer 


#Code pour la fonction gradient créer plus haut, on l'appel cette fois grad2
grad2<-function(z){
  #z=c(x0,y0)
  func<-function(p) sin(p[1]^2/2-p[2]^2/4)*cos(2*p[1]-exp(p[2]))
  f <- func(z)
  n <- length(z)
  args <- list(eps=1e-8) 
  eps <- rep(args$eps, n)
  df <- rep(NA,n)
  for (i in 1:n) {
    dx <- z
    dx[i] <- dx[i] + eps[i] 
    df[i] <- (func(dx) - f)/eps[i]
  }
  return(df)
}

#Ici on créer la fonction que l'on doit estimer avec la méthode ascent 
f <- function(x0){
  sin(x0[1]^2/2-x0[2]^2/4)*cos(2*x0[1]-exp(x0[2]))
}

#Ici on reporoduit la fonction gsection qui nous est donné dans le livre de mallairdet 
gsection <- function(ftn, x.l, x.r, x.m, tol = 1e-9) {
  gr1 <- 1 + (1 + sqrt(5))/2
  f.l <- ftn(x.l)
  f.r <- ftn(x.r)
  f.m <- ftn(x.m)
  while ((x.r - x.l) > tol) {
    if ((x.r - x.m) > (x.m - x.l)) {
      y <- x.m + (x.r - x.m)/gr1
      f.y <- ftn(y)
      if (f.y >= f.m) {
        x.l <- x.m
        f.l <- f.m 
        x.m <- y 
        f.m <- f.y 
      } else {
        x.r <- y 
        f.r <- f.y 
      }
    }else{
     y <- x.m - (x.m - x.l)/gr1
     f.y <- ftn(y)
     if (f.y >= f.m) {
       x.r <- x.m
       f.r <- f.m
       x.m <- y 
       f.m <- f.y 
     } else {
       x.l <- y 
       f.l <- f.y
     }
    }
  }
  return(x.m)
}

#Ici on reporoduit la fonction line.search qui nous est donné dans le livre de mallairdet 
line.search <- function(fct, x, y, tol = 1e-9, a.max = 2^5) {
  if (sum(abs(y)) == 0) return(x) 
  g <- function(a) return(f(x + a*y))
  a.l <- 0
  g.l <- g(a.l)
  a.m <- 1
  g.m <- g(a.m)
  while ((g.m < g.l) & (a.m > tol)) {
    a.m <- a.m/2
    g.m <- g(a.m) 
  }
  if ((a.m <= tol) & (g.m < g.l)) return(x)
  a.r <- 2*a.m
  g.r <- g(a.r)
  while ((g.m < g.r) & (a.r < a.max)) {
    a.m <- a.r 
    g.m <- g.r 
    a.r <- 2*a.m 
    g.r <- g(a.r)
     
  }
  if ((a.r >= a.max) & (g.m < g.r)) return(x - a.max*y)
  a <- gsection(g, a.l, a.r, a.m)
  return(x + a*y)
}

# Ici on créer la fonction ascent dans laquel on va incorporer notre fonction gradient, la fonction à estimer, la fonction gsection et la fonction line.search
ascent <- function(f, x0, tol = 1e-9, n.max = 100) {
  n=1
  xy <- data.frame(matrix(0, n, 2))
  x <- x0
  x.old <- x
  x <- line.search(f, x, grad2(x))
  xy[1,]<-c(x[1],x[2])
  n <- 1
   cat("A l'it?ration 1 la valeur de x est:", x, "\n")
  while ((f(x) - f(x.old) > tol)&(n < n.max)) {
  
    x.old <- x
    x <- line.search(f, x, grad2(x)) 
    xy[n,]<-c(x[1],x[2])
    n <- n + 1
     cat("A l'it?ration", n-1, "la valeur de x est:", x, "\n")
  } 
  return(xy)
 }





# Afin de bien montrer l'évolution de l'algorithme, nous allons illustrer la fonction en 2 dimmenssions en ajouter deux couleur afins de facilité le contraste 
# Les couleur et les formes circulaire deviennent de plus en plus petite au fur et à mesure que nous approchons d'un optimnal 

par(mfrow=c(2,2))
x <- seq(-.5, 3,length.out = 200)
y <- seq(-.5, 2,length.out = 200)

xyz <- data.frame(matrix(0, length(x)*length(y), 3))
names(xyz) <- c('x', 'y', 'z')
n <- 0
for (i in 1:length(x)) {
  for (j in 1:length(y)) {
    n <- n + 1
    xyz[n,] <- c(x[i], y[j], f(c(x[i], y[j]))[[1]])
    
  }
}
# Par  la suite nous allons tester notre fonction ascent avec plusieurs epsilons afin de comparer les résultats
# Dans la question, il nous est demander d'utiliser deux points de départs afin de vérifier la différence de chemin de l'algorithme en fonction du point de départ

# Cas1 : epsilon est de 1e-8 

# Coordonné départ a = (0,0.5)
# Coordonné départ b = (0.1,0.3)
xy1a<-ascent(f,c(0,0.5),tol=1e-8)      #27 iterations
xy1b<-ascent(f,c(0.1,0.3),tol=1e-8)    #37 iterations
#Nous utisons la fonction spatial point afin de bien positions la convergence de notre algorithme dans un univers 3d représenté en 2d 
sp1a=SpatialPoints(xy1a)
sp1b=SpatialPoints(xy1b)
xy1a=rbind(c(0,0.5),xy1a)
xy1b=rbind(c(0.1,0.3),xy1b)
#Par la suite on utilise la fonction levelplot afin de reproduire le graphique 
plot1<-levelplot(z ~ x*y, data=xyz,cex = 1.2, xlab="X" , main="Convergence de la fonction Ascent pour Epsilon = 1e-8",region=TRUE,col.regions = c('Green','White'))+
  latticeExtra::layer(panel.lines(xy1a,col=1),sp.points(sp1a,pch=1,col=1),panel.lines(xy1b,col=1),sp.points(sp1b,pch=1,col=1))


# Cas2 : epsilon est de 1e-4

# Coordonné départ a = (0,0.5)
# Coordonné départ b = (0.1,0.3)
xy2a<-ascent(f,c(0,0.5),tol=1e-4)          #11 iterations
xy2b<-ascent(f,c(0.1,0.3),tol=1e-4)        #24 iterations
#Nous utisons la fonction spatial point afin de bien positions la convergence de notre algorithme dans un univers 3d représenté en 2d 
sp2a=SpatialPoints(xy2a)
sp2b=SpatialPoints(xy2b)
xy2a=rbind(c(0,0.5),xy2a)
xy2b=rbind(c(0.1,0.3),xy2b)
#Par la suite on utilise la fonction levelplot afin de reproduire le graphique 
plot2<-levelplot(z ~ x*y, data=xyz,cex = 1.2, xlab="X" , main="Convergence de la fonction Ascent pour Epsilon = 1e-8",region=TRUE,col.regions = c('Green','White'))+
  latticeExtra::layer(panel.lines(xy2a,col=1),sp.points(sp2a,pch=1,col=1),panel.lines(xy2b,col=1),sp.points(sp2b,pch=1,col=1))


# Cas3 : epsilon est de 1e-12

# Coordonné départ a = (0,0.5)
# Coordonné départ b = (0.1,0.3)
xy3a<-ascent(f,c(0,0.5),tol=1e-12)                #43 iterations
xy3b<-ascent(f,c(0.1,0.3),tol=1e-12)              #50 iterations
#Nous utisons la fonction spatial point afin de bien positions la convergence de notre algorithme dans un univers 3d représenté en 2d 
sp3a=SpatialPoints(xy3a)
sp3b=SpatialPoints(xy3b)
xy3a=rbind(c(0,0.5),xy3a)
xy3b=rbind(c(0.1,0.3),xy3b)
#Par la suite on utilise la fonction levelplot afin de reproduire le graphique 
plot3<-levelplot(z ~ x*y, data=xyz,cex = 1.2, xlab="X" , main="Convergence de la fonction Ascent pour Epsilon = 1e-8",region=TRUE,col.regions = c('Green','White'))+
  latticeExtra::layer(panel.lines(xy3a,col=1),sp.points(sp3a,pch=1,col=1),panel.lines(xy3b,col=1),sp.points(sp3b,pch=1,col=1))

# Cas4 : epsilon est de 1e-2

# Coordonné départ a = (0,0.5)
# Coordonné départ b = (0.1,0.3)
xy4a<-ascent(f,c(0,0.5),tol=1e-2)           #4 iterations
xy4b<-ascent(f,c(0.1,0.3),tol=1e-2)         #17 iterations
#Nous utisons la fonction spatial point afin de bien positions la convergence de notre algorithme dans un univers 3d représenté en 2d 
sp4a=SpatialPoints(xy4a)
sp4b=SpatialPoints(xy4b)
xy4a=rbind(c(0,0.5),xy4a)
xy4b=rbind(c(0.1,0.3),xy4b)
#Par la suite on utilise la fonction levelplot afin de reproduire le graphique 
plot4<-levelplot(z ~ x*y, data=xyz,cex = 1.2, xlab="X" , main="Convergence de la fonction Ascent pour Epsilon = 1e-2",region=TRUE,col.regions = c('Green','White'))+
  latticeExtra::layer(panel.lines(xy4a,col=1),sp.points(sp4a,pch=1,col=1),panel.lines(xy4b,col=1),sp.points(sp4b,pch=1,col=1))

#On va maintenant mettre les quatres graphiques montrant les iterations en même temps pour les quatres tolérences possible 
grid.arrange(plot1,plot2,plot3,plot4,ncol=2,nrow=2)


#On voit donc que au fur que nous augmentons l'epsilon l'algorithme prend plus d'iteration pour converger
#On voit également que tout dépendant de notre point de départ, on trouve un maximum différent 
