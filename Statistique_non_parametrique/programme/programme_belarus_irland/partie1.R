# Environnement ----
setwd("/Users/rs777/Documents/Statistique-non-parametrique")

source("programme/programme_belarus_irland/preliminaire.R")

# Partie 1 : Distribution de la taille

# 1 -----

## Belarus

### HISTSELECT2
HISTSELECT2(Belarus$Taille, 
            freq = FALSE, 
            col = "lightblue",
            xlab = "Taille",
            ylab = "Densité",
            main = "Distribution de la taille - Belarus") # 34 partitions
mtext("(HISTSELECT2)", side = 3, line = 0.5, cex = 0.9)

### R par défault
hist(Belarus$Taille, 
     freq = FALSE, 
     col = "blue",
     xlab = "Taille",
     ylab = "Densité",
     main = "Distribution de la taille - Belarus") # 16 classes
mtext("(R par défaut)", side = 3, line = 0.5, cex = 0.9)

## Irland

### HISTSELECT2
HISTSELECT2(Ireland$Taille, 
            freq = FALSE, 
            col = "lightgreen",
            xlab = "Taille",
            ylab = "Densité",
            main = "Distribution de la taille - Irland") # 23 partitions
mtext("(HISTSELECT2)", side = 3, line = 0.5, cex = 0.9)

### R par défault
hist(Ireland$Taille, 
     freq = FALSE, 
     col = "darkgreen",
     xlab = "Taille",
     ylab = "Densité",
     main = "Distribution de la taille - Irland") # 16 classes
mtext("(R par défaut)", side = 3, line = 0.5, cex = 0.9)



# 2 -----

## Belarus

estimation_belarus = bw.bcv(Belarus$Taille, 
                            lower = 0.01, 
                            upper = 4) # Esimation par Noyau

### HISTSELECT2

HISTSELECT2(Belarus$Taille, 
            freq = FALSE, 
            col = "lightblue",
            xlab = "Taille",
            ylab = "Densité",
            main = "Distribution de la taille - Belarus") # 34 partitions
mtext("(HISTSELECT2)", side = 3, line = 0.5, cex = 0.9)
lines(density(Belarus$Taille, 
              bw = estimation_belarus), 
              col = "red",
              lwd = 2) # Ajout de l'estimation


### R par défault
hist(Belarus$Taille, 
     freq = FALSE, 
     col = "blue",
     xlab = "Taille",
     ylab = "Densité",
     main = "Distribution de la taille - Belarus") # 16 classes
mtext("(R par défaut)", side = 3, line = 0.5, cex = 0.9)
lines(density(Belarus$Taille, 
              bw = estimation_belarus), 
              col = "red",
              lwd = 2) # Ajout de l'estimation

## Irland

estimation_ireland = bw.bcv(Ireland$Taille, 
                            lower = 0.01, 
                            upper = 4) # Esimation par Noyau

### HISTSELECT2
HISTSELECT2(Ireland$Taille, 
            freq = FALSE, 
            col = "lightgreen",
            xlab = "Taille",
            ylab = "Densité",
            main = "Distribution de la taille - Irland") # 23 partitions
mtext("(HISTSELECT2)", side = 3, line = 0.5, cex = 0.9)
lines(density(Ireland$Taille, 
              bw = estimation_ireland),
              col = "red", lwd = 2) # Ajout de l'esimation


### R par défault
hist(Ireland$Taille, 
     freq = FALSE, 
     col = "darkgreen",
     xlab = "Taille",
     ylab = "Densité",
     main = "Distribution de la taille - Irland") # 16 classes
lines(density(Ireland$Taille, 
              bw = estimation_ireland),
              col = "red", lwd = 2) # Ajout de l'esimation



# 3 -----

plot(density(Belarus$Taille, bw = estimation_belarus), 
     lwd = 2, 
     col = "blue", 
     main = "Estimation des densités des deux pays", 
     xlab = "Taille", 
     ylab = "Densité")
mtext("Estimation par noyau", side = 3, line = 0.5, cex = 0.9)
lines(density(Ireland$Taille, bw = estimation_ireland), 
      col = "red", 
      lwd = 2)

legend("topleft", 
       legend = c("Belarus", "Ireland"), 
       text.col = c("blue", "red"), 
       col = c("blue", "red"), 
       lwd = 2)



# 4 -----

## HO : Distrib de la taille égale dans les 2 pays
## H1 : Distrib de la taille différente dans les 2 pays

ks.test(Belarus$Taille,Ireland$Taille) # présence d'ex eaquo

library(robusTest) # Correction des exeaquo

Sal <- tiebreak(Data8$Taille) # Résolution des ex aequo dans un vecteur

ks.test(Sal[Data8$Pays == "Belarus"],Sal[Data8$Pays == "Ireland"]) # Test de Kolmo-Smirnov



# 5 -----

## Belarus

### Estimation par noyau pour les 2 sexes

estimation_belarus_fille = bw.bcv(Belarus_f$Taille, lower = 0.01, upper = 4)
estimation_belarus_garcons = bw.bcv(Belarus_h$Taille, lower = 0.01, upper = 4)

plot(density(Belarus_f$Taille, bw = estimation_belarus_fille), 
     lwd = 2, 
     col = "magenta", 
     main = "Estimation des densités - Belarus", 
     xlab = "Taille", 
     ylab = "Densité")
mtext("Estimation par noyau", side = 3, line = 0.5, cex = 0.9)
lines(density(Belarus_h$Taille, bw = estimation_belarus_garcons), 
      col = "orange", 
      lwd = 2)

legend("topleft", 
       legend = c("Fille", "Garons"), 
       text.col = c("magenta", "orange"), 
       col = c("magenta", "orange"), 
       lwd = 2)

## Irland

### Estimation par noyau pour les 2 sexes

estimation_ireland_fille = bw.bcv(Ireland_f$Taille, lower = 0.01, upper = 4)
estimation_ireland_garcons = bw.bcv(Ireland_h$Taille, lower = 0.01, upper = 4)

plot(density(Ireland_f$Taille, bw = estimation_ireland_fille), 
     lwd = 2, 
     col = "violet", 
     main = "Estimation des densités - Irland", 
     xlab = "Taille", 
     ylab = "Densité")
mtext("Estimation par noyau", side = 3, line = 0.5, cex = 0.9)
lines(density(Ireland_h$Taille, bw = estimation_ireland_garcons), 
      col = "black", 
      lwd = 2)

legend("topleft", 
       legend = c("Belarus fille", "Belarus garons"), 
       text.col = c("black", "violet"), 
       col = c("black", "violet"), 
       lwd = 2)



# 6 -----

## Belarus

ks.test(Belarus_f$Taille,Belarus_h$Taille) # présence d'ex eaquo

Sal <- tiebreak(Belarus$Taille) # Résolution des ex aequo dans un vecteur

ks.test(Sal[Belarus$Sexe == "Boys"],Sal[Belarus$Sexe == "Girls"]) # Test de Kolmo-Smirnov

## Irland

ks.test(Ireland_f$Taille,Ireland_h$Taille) # ex eaquo

Sal <- tiebreak(Ireland$Taille) # Résolution des ex aequo dans un vecteur

ks.test(Sal[Ireland$Sexe == "Boys"],Sal[Ireland$Sexe == "Girls"]) # Test de Kolmo-Smirnov