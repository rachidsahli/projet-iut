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

summary(KM) # Résultats de l'estimation

quantile(KM) # Estimation quartile et médiane
# Q1 : 9 -- IC = [6 ; 15]
# Médiane : 19 -- IC = [15 ; 37]
# Q3 : 52 -- IC = [37 ; NA]

### b

addmargins(table(myel$DECES, myel$BENCE_J))
prop.table(table(myel$DECES, myel$BENCE_J), margin = 2)*100

KM_gr <- survfit(Surv(myel$T, myel$DECES) ~ myel$BENCE_J) # Estimation de la fonction de survie selon BENCE_J

summary(KM_gr, conf.type = "Plain") # Résultats de l'estimation

quantile(KM_gr, probs = 0.5) # Estimation de la mediane
                             # BENCE_J = 0 : Mediane = 16 -- IC = [11 ; 66]
                             # BENCE_J = 1 : Mediane = 37 -- IC = [19 ; 67]

summary(KM_gr,conf.type="Plain",times=12) # Survie à 12 mois par groupe
                                          # BENCE_J = 0 : 62.2 %
                                          # BENCE_J = 1 : 73.2 %

### c

plot(KM_gr, 
     mark.time = TRUE, 
     col = c(1,2),
     main = "Estimation des courbes de survie selon Bence Jones",
     cex.main = 0.9,
     xlab = "Temps",
     ylab = "Probabilité cumulée de survie"
     )
legend("topright", legend= paste0("BJ = ", levels(as.factor(myel$BENCE_J))), lty=1, col=c(1,2))

### d

survdiff(Surv(myel$T, myel$DECES) ~ myel$BENCE_J) # Test du Log-Rank
                                                  # Khi2 = 2.1 et p = 0.1
                                                  # Non significatif

### e

addmargins(table(myel$DECES, myel$SEXE))
prop.table(table(myel$DECES, myel$SEXE), margin = 2)*100

KM_sex <- survfit(Surv(myel$T, myel$DECES) ~ myel$SEXE) # Estimation de la fonction de survie selon le SEXE

summary(KM_sex, conf.type = "Plain") # Résultats de l'estimation

quantile(KM_sex, probs = 0.5) # Estimation de la mediane
                              # SEXE = 0 : Mediane = 24 -- IC = [16 ; 88]
                              # SEXE = 1 : Mediane = 17 -- IC = [11 ; 41]

summary(KM_sex,conf.type="Plain",times=12) # Survie à 12 mois par groupe
                                          # SEXE = 0 : 72.8 %
                                          # SEXE = 1 : 61.9 %

### c

plot(KM_sex, 
     mark.time = TRUE, 
     col = c(1,2),
     main = "Estimation des courbes de survie selon le sexe",
     cex.main = 0.9,
     xlab = "Temps",
     ylab = "Probabilité cumulée de survie"
)
legend("topright", legend= paste0("SEXE = ", levels(as.factor(myel$SEXE))), lty=1, col=c(1,2))

### d

survdiff(Surv(myel$T, myel$DECES) ~ myel$SEXE) # Test du Log-Rank
                                               # Khi2 = 0.4 et p = 0.5
                                               # Non significatif
