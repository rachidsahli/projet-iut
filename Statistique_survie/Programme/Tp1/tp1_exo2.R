# TP1 - Exercice 2

# Import library

library(survival)

# Import data

divorce <- read.csv(
  "/Users/rs777/Documents/Projet-datascience/Statistique_survie/Programme/Tp1/divorce.csv"
  )

## 1

KM <- survfit(Surv(divorce$time, divorce$event) ~ 1)
options(max.print = 20000) # Nb maximum de lignes

summary(KM) # Résultat de l'estimation

summary(KM, time = c(10,20)) # Estimation qu'un couple tjr marié après 10 et 20 ans
                             # 10 ans : 80,1 % -- IC : [0.786 ; 0.815]
                             # 20 ans : 67,7 % -- IC : [0.659 ; 0.696]

plot(KM,mark.time = TRUE) # Représentation graphique

## 2

quantile(KM,probs=0.25) # Estimation du premier quartile
                        # Q1 : 13,667 -- IC : [12.419 ; 15.002]

## 3

levels(as.factor(divorce$fac_heduc)) # 3 niveaux d'études

plot(survfit(Surv(divorce$time, divorce$event) ~ divorce$fac_heduc),
     col = c(1,2,3), main = "Estimation de la fonction de survie selon l'éducation du père",
     xlab  = "Temps", ylab = "Probabilité de survie") # Représentation graphique
legend("bottomleft", legend = levels(as.factor(divorce$fac_heduc)), lty = 1, col = c(1,2,3))

KM_gr <- survfit(Surv(divorce$time, divorce$event) ~ divorce$fac_heduc) # Estimation de survie
                                                                        # selon le groupe

summary(KM_gr, times = c(10,20)) # Résultat de l'estimation à 10 et 20 ans pour chaque groupe

quantile(KM_gr,probs = 0.25) # Estimation du Q1 pour chaque groupe

summary(KM_gr, times = c(10,20,30,40,50)) # Résultat de l'estimation à 10,20,30, 40 et 50 ans
                                          # ans pour chaque groupe

## 4 

survdiff(Surv(divorce$time, divorce$event) ~ divorce$fac_heduc) # Test du log-rank pour chaque groupe
