# TP5 - Secant root-finding method
#SENGOUT SARAVANAN 111 166 828 
#SIMON-PIERRE BOUCHER 111 062 211






#1er Partie : Trouver le zero de la fonction cos(x) - x 

#On créer la fonction "fixedpoint" qui contiendra notre fonction que l'on souhaite trouver le zero, dans notre cas ici : cos(x) - x 
fixedpoint <- function(x){
  cos(x) - x
}

#On créer la fonction "newtonraphson" qui nous permttra de trouver le zero de notre fonction par intération 
newtonraphson <- function(fixedpoint, x0, x1, test = 0.000000001, n = 500){
  for (i in 1:n){
    x2 <- x1 - fixedpoint(x1) / ((fixedpoint(x1)-fixedpoint(x0)) / (x1 - x0))
    if (abs(x2 - x1) < test){
      print("voici la value de x qui rend f(x) = 0")
      return(x2)
    }
    x0 <- x1
    x1 <- x2
  }
}

#Permet d'exécuter la fonction "newtonraphson" et on doit préciser que l'on veut trouver le zero de la fonction inclue dans "fixedpoint"
#On doit cependant préciser ici les borne de départ que l'on peut estimer à l'aide du graph, nous avons ici mit 0 et 4 dans notre fonction pour que le code soit exécutable 
newtonraphson(fixedpoint, 0, 4)

# voici la preuve que la valeur trouver en x donc f(x) = 0 


fixedpoint(newtonraphson(fixedpoint, 0, 4))


#2e Partie : Trouver le zero de la fonction log(x) - exp(-x) en fixant x0=1 x1=2

#On créer la fonction "fixedpoint" qui contiendra notre fonction que l'on souhaite trouver le zero, dans notre cas ici :  log(x) - exp(-x)
fixedpoint <- function(x){
  log(x) - exp(-x)
}

#On créer la fonction "newtonraphson" qui nous permttra de trouver le zero de notre fonction par intération 
newtonraphson <- function(fixedpoint, x0, x1, test = 0.000000001, n = 500){
  #On fixe une valeur de x0 = 1 et x1 = 2 comme demander dans la question 
   x0 <- 1
   x1 <- 2
  for (i in 1:n){
    x2 <- x1 - fixedpoint(x1) / ((fixedpoint(x1)-fixedpoint(x0)) / (x1 - x0))
    if (abs(x2 - x1) < test){
      print("voici la value de x qui rend f(x) = 0")
      return(x2)
    }
    x0 <- x1
    x1 <- x2
  }
}

#Permet d'exécuter la fonction "newtonraphson" et on doit préciser que l'on veut trouver le zero de la fonction inclue dans "fixedpoint"
newtonraphson(fixedpoint)

# voici la preuve que la valeur trouver en x donc f(x) = 0 
fixedpoint(newtonraphson(fixedpoint))






# 3e partie : création d'un code (secant.show) qui permet d'afficher la procédure pour trouver le zéro de cos(x) - x  

#On définie dans un premier temps une fonction "testit" qui nous permettra de faire une pose dans la boucle afin de bien visualiser le graphique 
testit <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}

#On créer la fonction "secant.show" qui permet de visualiser graphique chaque itération 

secant.show <- function(){
# Dans notre fonction "secant.show" on créer un fonction additionel "f" pour définir la fonction que l'on souhaite trouver le zero 
  f <- function(x) {
    cos(x) - x
  }

# On affiche le graph de la fonction tout simplement 
  curve(expr =f, col='blue', lwd=2, lty=2, xlim=c(-1,4))
  abline(h=0)
  abline(v=0)
 
#On défini une valeur arbitraire de départ pour mettre dans l'équation de départ de la secant method 
  x0 <- 0
  x1 <- 3
# On affiche le graph de la fonction avec la première droite de la 1er itération 
  curve(expr =f , col='blue', lwd=2, lty=2, xlim=c(-1,4))
  abline(h=0)
  abline(v=0)
  points(x0, f(x0), cex=1.25, pch=21, bg='blue', col='blue')
  points(x1, f(x1), cex=1.25, pch=21, bg='blue', col='blue')
  segments(x0, f(x0), x1, f(x1), col='red', lwd = 2)
  
  # On prend une pause dans le code afin de bien visualiser le graph 
  testit(1)
  
  # On utilise l'équation de la secant method pour trouver la prochaine valeur, soit x2
  x2 <- x1 - f(x1) / ((f(x1) - f(x0)) / (x1 - x0))
  
  
  # On affiche le graph de la fonction avec la première droite de la 2er itération 
  curve(expr =f , col='blue', lwd=2, lty=2, xlim=c(-1,4))
  abline(h=0)
  abline(v=0)
  points(x1, f(x1), cex=1.25, pch=21, bg='blue', col='blue')
  points(x2, f(x2), cex=1.25, pch=21, bg='blue', col='blue')
  segments(x1, f(x1), x2, f(x2), col='red', lwd = 2)

  # On prend une pause dans le code afin de bien visualiser le graph 
  testit(1)
  
  # On utilise l'équation de la secant method pour trouver la prochaine valeur, soit x3
  x3 <- x2 - f(x2) / ((f(x2) - f(x1)) / (x2 - x1))
  
  # On affiche le graph de la fonction avec la première droite de la 3er itération 
  curve(expr =f , col='blue', lwd=2, lty=2, xlim=c(-1,4))
  abline(h=0)
  abline(v=0)
  points(x2, f(x2), cex=1.25, pch=21, bg='blue', col='blue')
  points(x3, f(x3), cex=1.25, pch=21, bg='blue', col='blue')
  segments(x2, f(x2), x3, f(x3), col='red', lwd = 2)
  
  # On prend une pause dans le code afin de bien visualiser le graph 
  testit(1)
  
  # On utilise l'équation de la secant method pour trouver la prochaine valeur, soit x4
  x4 <- x3 - f(x3) / ((f(x3) - f(x2)) / (x3 - x2))

  
  # On affiche le graph de la fonction avec la première droite de la 4er itération 
  curve(expr =f , col='blue', lwd=2, lty=2, xlim=c(-1,4))
  abline(h=0)
  abline(v=0)
  points(x3, f(x3), cex=1.25, pch=21, bg='blue', col='blue')
  points(x4, f(x4), cex=1.25, pch=21, bg='blue', col='blue')
  segments(x3, f(x3), x4, f(x4), col='red', lwd = 2)
  # On utilise l'équation de la secant method pour trouver la prochaine valeur, soit x5 qui est la valeur de notre f(x) quand x = 0
  x5 <- x4 - f(x4) / ((f(x4) - f(x3)) / (x4 - x3))
  
  #On affiche la valeur de x5 qui est la valeur de notre f(x) quand x = 0
  print("voici la valeur de y qui rend x = 0")
  return(x5)
}


#Permet d'exécuter la fonction, pas besoin de préciser de paramètres 
secant.show()


