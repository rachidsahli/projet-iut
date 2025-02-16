# Objectif : Calculer les CVS et decompositionner la série

source('scripts/0_environnement.R')

# Décomposition de la série ----

dec = decompose(charbon, type = "additive") # Décomposition de la série
plot(dec)

plot(dec$random, main="Graphe des résidus",
     xlab="Année", ylab="résidus")
abline(0,0)
abline(2*sd(dec$random, na.rm=TRUE),0)
abline(-2*sd(dec$random, na.rm=TRUE),0) # Graphe des résidus


boxplot(dec$random, na.rm=TRUE,col="yellow",
        main="Boîte à moustache des résidus" )


plot(dec$trend + dec$random, xlab="", main="Série corrigée des variations saisonnières", ylab="", lwd=2)

plot(dec$trend + dec$seasonal,xlab="", lwd=2, main="Série lissée des prédictions", ylab="")

# Comparaison des deux courbes précédentes

plot(charbon, main="", ylab="", xlab="Année")
lines(dec$trend + dec$seasonal, col="red")
lines(dec$trend + dec$random, col = "green")
legend("topright", 
       legend = c("Données", "Série corrigée des variations saisonnières", "Série lissée des prédictions"),
       text.col = c("black", "red", "green"))








