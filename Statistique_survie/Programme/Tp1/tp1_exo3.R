# TP1 - Exercice 3

# Import library

library(survival)
library(dplyr)

# Import data

myel <- read.table(
  "/Users/rs777/Documents/Projet-datascience/Statistique_survie/Programme/Tp1/myel_comp.txt",
  header = TRUE
)

head(myel) # Aperçu
summary(myel) # Résumé statistique

# Variables d'intérêts : Indice, Bence-Jones et Sexe

## 2
### a

KM <- survfit(Surv(myel$T, myel$DECES) ~ 1) # Estimation de la fonction de survie

summary(KM) # Résultats

quantile(KM) # Estimation quartile et médiane
             # Q1 : 9 -- IC = [6 ; 15]
             # Médiane : 19 -- IC = [15 ; 37]
             # Q3 : 52 -- IC = [37 ; NA]

### b

table(myel$DECES, myel$BENCE_J)
prop.table(table(myel$DECES, myel$BENCE_J), margin = 2)*100








  

