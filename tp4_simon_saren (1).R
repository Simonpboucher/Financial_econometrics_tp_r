
#TP3
#SENGOUT SARAVANAN 111 166 828 
#SIMON-PIERRE BOUCHER 111 062 211

#A
# Dans cette question il nous est demander de créer un programme que nous avons appelé jeux1
# il y aura 4 lancés de dé et si le nombre 6 est sortie dans au moin un lancé nous allons affiché "Win" sinon "Loose" 

# On créer une fonction jeux 1 n'ayant aucun paramètre car il y a une valeur par défaut de 4 dans le programme
jeux1 <- function(){

# Le résultat par défaut "LOSE" est enregistré par défaut en dehors de la boucle 
result <- "LOSE"

# Nous créons une boucle que va effetuer la procédure 4 fois avant que le programme quite la boucle 
for (i in 1:4){
x <- sample(1:6,size=1)      # ici nous générons une variable aléatoire entre 1 et 6 que sera loger dans la variable x
if (x == 6) {
  result <-"WIN"}           # ici nous remplacons le "LOSE" dans result par "WIN" si la variable x est égale à 6
}
print(result)               # Nous imprimons le résultat du jeux1 qui sera soit WIN ou LOSE 
}

jeux1()

#B

#ici on créer la fonction sixes qui aura comme paramètre n, qui sera le nombre de fois ou nous allons lancer le dé

sixes <- function(n){
  if(missing(n)==TRUE) { #ici on vérifie si si il manque une valeur pour n lorsque la fonction est lancer et si oui on met 4 par défaut
    n <-4
  } 
    
  result <- FALSE    # le résultat est FALSE par défaut 
  for (i in 1:n){  # nous créer une boucle qui effectura la procédure n fois 
    x <- sample(1:6,size=1)   # ici nous générons une variable aléatoire entre 1 et 6 que sera loger dans la variable x
    if (x == 6) {
      result <-TRUE}     # ici nous remplacons le "FALSE" dans result par "TRUE" si la variable x est égale à 6
  }
  print(result)    # Nous imprimons le résultat du jeux1 qui sera soit WIN ou LOSE 

  }

A <-sixes(1)

#C

#Dans cette partie il nous est demander de créer une fonction qui retournera N fois le tirage de n lance
#Nous allons définir la valeur de la variable result à 0 par défaut
result <- 0

# Nous créer une fonction que nous appeler jeux3 et qui contiendra comme paramètre N et n
# N = nombre de fois ou nous allons refaire le jeux 
# n = nombre de lancer par jeux 
jeux3  <- function(n,N){
  for(i in 1:N){        # Nous allons créer une boucle qui effectuera la procédure N fois 
    reponse <- sixes(n) # ici la réponse est une variable qui va contenir le réultat de la fonction sixes
    if (reponse == TRUE) { # Ici nous ajoutons +1 à result si la réponse contient TRUE 
      result  <-result + 1 
                             # À la fin de la boucle nous allons avoir le nombre de fois ou TRUE est sortie
    }
    print(result)
  }
  return(result/N) # Nous divisons la variable result par N afin d'avoir le pourcentage de succes 
}

jeux3(1,50)

#D
# Ici on créer une fonction sixes_sim qui nous permettra d'enregistrer les résultats de N fois le jeux sixes
sixes_sim <- function(N,n) {

# la fonction  cat nous permettra de prendre la valeur que la fonction sixes nous affichera et la transformer en string afin de l'imprimer dans un fichier texte
  cat(file="sixes_sim.txt")
  for (i in 1:N) {   # créer une boucle qui effectuera lopération N fois de la fonction sixes
    cat(file = "sixes_sim.txt", sixes(n), "\n", append = TRUE) # ici avec la fonction  cat on met le résultat de sixes dans le fichier text et on indique de changer de ligne a chaque fois 
  }
}
sixes_sim(100,4) # ici nous roulons la fonction sixes_sim qui créera le fichier texte 
result <- scan("sixes_sim.txt", what = TRUE) # ici nous allons lire le résultat dans le fichier text afin de le mettre dans la variable result

#Nous utilisons la fonction mean afin d'avoir le pourcentage d'occurence ou le résultat du jeux est TRUE
mean(result)
