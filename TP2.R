

#SENGOUT Saravanan 1111 66 828 
#BOUCHER Simon-Pierre 111 062 211
#Nous créons une variable contenant une de 100 valeurs distribuées normalement centré réduite 

x <- rnorm(10,0,1)

#On affiche les valeurs générées 
x

#on fixe la première valeur comme étant x.min
x.min <- x[1]

#nous créons la boucle 
for (i in (seq_along(x[-1]) + 1)) { 
	
#on compare la valeur de x(i) avec la valeur minimum passé 	
if(x[i] < x.min)

#si la condition est satisfaite on remplace x.min par la nouvelle valeur 
x.min <- x[i]

#on affiche la valeur de x.min à chaque fin d'itération 
show(x.min)
 }

#on affiche la dernière valeur de x.min ce qui correspond au minimum de la série
x.min
