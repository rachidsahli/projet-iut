#_______________________________________________________________________________
# COURS     : Risque banque
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
#
# PROGRAMME : 0 - ENVIRONNEMENT
#_______________________________________________________________________________



# Environnement -----

setwd("/Users/rs777/Documents/Projet-datascience/Risque_banque")

# Import library -----

library(tidyverse)      # Manipulation des données et graphiques
library(conflicted)     # Gestion des conflits de noms de fonctions entre packages
library(knitr)          # Création de tableau
library(gridExtra)      # Arrangement de graphique
library(haven)          # Lecture table SAS
library(lubridate)      # Gestion des dates
library(questionr)      # Statistiques descriptives
library(skimr)          # Statistiques descriptives
library(BioStatR)       # Mesures de liaison pour les variables quantitatives (rapport de corrélation)
library(DescTools)      # Mesures de liaison pour les variables qualitatives (Cramer, Tschuprow)
library(FactoMineR)     # Caractérisation des classes
library(naniar)         # Gestion des valeurs manquantes
library(zoo)            # Gestion des valeurs manquantes
library(corrplot)       # Corrélation
library(woeBinning)     # Discrétisation supervisée
library(smbinning)      # Discrétisation supervisée
library(caret)          # Modélisation - Général
library(pROC)           # Modélisation - Général
library(carData)        # Modélisation - Régression logistique
library(car)            # Modélisation - Régression logistique
library(blorr)          # Modélisation - Régression logistique
library(emmeans)        # Modélisation - Régression logistique
library(broom)          # Modélisation - Régression logistique
library(glmnet)         # Modélisation - Régression logistique pénalisée
library(rpart)          # Modélisation - Arbre de décision
library(rpart.plot)     # Modélisation - Arbre de décision
library(class)          # Modélisation - k plus proches voisins

# Gestion des conflits -----

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("slice", "dplyr")

# Import data -----

# periode_2021 <- read_sas("data/bt2021.sas7bdat")

# periode_2022 <- read_sas("data/bt2022.sas7bdat")

periode_2023 <- read_sas("data/bt2023.sas7bdat")

periode_reference <- read_sas("data/btref.sas7bdat")

