# TP5 - Pascal's Triangle
#SENGOUT SARAVANAN 111 166 828 
#SIMON-PIERRE BOUCHER 111 062 211

#Dans ce tp il nous est demandé d'écrire un code qui permet de généré plusieurs lignes du triangle de pascal
#La particularité du triangle de pascal est que le sommet du triangle prend pour valeur 1 ainsi que les extrémités de chanque ligne.
#La 1er ligne sera donc             1
#La 2e ligne sera                  1 1
#La 3e ligne sera                 1 ? 1
#La 4e ligne sera               1 ?  ? 1
#La 5e ligne sera              1 ?  ?  ? 1
# Le triganle continue aussi lontemps que nécessaire et que la puissance du programme est la 
# Nous allons maintenant remplir l'intérieur du triangle
# Chaque point d'intérogation prendra comme valeur la somme des deux valeurs au dessus d'elle
# Donc pour le premier point d'intérogation sur la 3e ligne, la valeur est de 1 + 1 = 2
# Pour les deux point d'intérogation sur la 4e ligne, la veleur est de 3 pour les deux 1 + 2 = 3 et 2 + 1= 3
#Pour les trois points d'intérogation sur la 5e ligne, la première valeur est de 1 + 4 = 5, la deuxième valeur est de 3 + 3 = 6 et la dernière valeur est de 3 + 1 = 4

#Le triangle de pascal une fois remplie devrait ressembler à ceci : 
#La 1er ligne sera donc             1
#La 2e ligne sera                  1 1
#La 3e ligne sera                 1 2 1
#La 4e ligne sera               1 3  3 1
#La 5e ligne sera              1 4  6  4 1

#Nous allons maintenant créer un petit programme permettant de généré n'importequel ligne du triangle de pascal 

#Nous commencons par créer une fonction pour le triangle que nous appelerons triangle.triangle

triangle<-function(V)
{
  row = length(V)          # row est la variable contenant le numero de la th ligne 
  V.row = V[[row]]        # alors que la variable V contient les valeurs du triangle
  V.row.plus.1 = rep(1,row+1)
  if(row > 1)
  {
    for(i in 1:(row-1))
    {
      V.row.plus.1[i+1] = V.row[i]+V.row[i+1] # Ici nous venons trouvé la valeur du triangle en prenant la somme des deux immédiatement au dessus, sachant que notre position est de i + 1 alors on prend la somme de i et i+1
    }
  }
  append(V,list(V.row.plus.1))
}

V.1 = list(c(1))   # Ici on accord la valeur de 1 à la seul valeur de la ligne étant le sommet 
V.1
V.2 = triangle(V.1) # Par la suite les lignes additionels sont créer en utilisant notre fonction triangle 
V.2
V.3 = triangle(V.2)
V.3
V.4 = triangle(V.3)
V.4
V.5 = triangle(V.4)
V.5
V.6 = triangle(V.5)
V.6
V.7 = triangle(V.6)
V.7
V.8 = triangle(V.7)
V.8
V.9 = triangle(V.8)
V.9
V.10 = triangle(V.9)
V.10
V.11 = triangle(V.10) # on fait rouler la 11e ligne comme demandé
V.11                  # on obtient 1 10 45 120 210 252 210 120 45 10 1 

bin <- choose(10,0:10) # on veut vérifier si on obient le même résultat avec le coefficient binomial demande 
bin                    # on obtient 1 10 45 120 210 252 210 120 45 10 1 

# Les valeurs de la 11e ligne du coefficient sont donc identique aux valeurs du coefficient binomial 
