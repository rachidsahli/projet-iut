# Environnement ----
setwd("/Users/rs777/Documents/Statistique-non-parametrique")

source("programme/fonction_histogramme_regulier.R")

# Import data -----

Data8<-read.csv(file="data/belarus_irlande.csv")

# Suprétion premiere colonne inutile -----

Data8 <- Data8[,-1]


# Séparation des données -----

## Belarus
Belarus = Data8[(Data8$Pays=="Belarus"),]
Belarus_f = Belarus[(Belarus$Sexe=="Girls"),] # Belarus fille
Belarus_h = Belarus[(Belarus$Sexe=="Boys"),] # Belarus garçons

## Ireland
Ireland = Data8[(Data8$Pays=="Ireland"),]
Ireland_f = Ireland[(Ireland$Sexe=="Girls"),]
Ireland_h = Ireland[(Ireland$Sexe=="Boys"),]

# Graphique descriptif -----

## Nuages de points de la taille en fonction de l'âge

### Belarus
plot(Belarus$Age, Belarus$Taille,
     xlab = "Âge",
     ylab = "Taille",   
     main = "Taille en fonction de l'âge - Belarus",
     cex.main = 0.9,
     col = "blue") 

### Irland
plot(Ireland$Age, Ireland$Taille,
     xlab = "Âge",
     ylab = "Taille",   
     main = "Taille en fonction de l'âge - Irland",
     cex.main = 0.9,
     col = "darkgreen") 

## Nuages de points de la taille en fonction de l'âge par sexe

### Belarus

#### Fille
plot(Belarus_f$Age, Belarus_f$Taille,
     xlab = "Âge",
     ylab = "Taille",   
     main = "Taille en fonction de l'âge - Belarus (Fille)",
     cex.main = 0.9,
     col = "blue")

#### Garçons
plot(Belarus_h$Age, Belarus_h$Taille,
     xlab = "Âge",
     ylab = "Taille",   
     main = "Taille en fonction de l'âge - Belarus (Garçons)",
     cex.main = 0.9,
     col = "blue")

### Irland

#### Fille
plot(Ireland_f$Age, Ireland_f$Taille,
     xlab = "Âge",
     ylab = "Taille",   
     main = "Taille en fonction de l'âge - Irland (Fille)",
     cex.main = 0.9,
     col = "darkgreen")

#### Garçons
plot(Ireland_h$Age, Ireland_h$Taille,
     xlab = "Âge",
     ylab = "Taille",   
     main = "Taille en fonction de l'âge - Belarus (Garçons)",
     cex.main = 0.9,
     col = "darkgreen")

