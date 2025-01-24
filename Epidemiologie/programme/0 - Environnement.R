#_______________________________________________________________________________
# COURS     : Epidemiologie
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
# DONNEES   : Etude ESMAA
#
# PROGRAMME : 0 - ENVIRONNEMENT
#_______________________________________________________________________________


setwd("/Users/rs777/Documents/Iut/projets but stid/2024-2025/Projet/Epidemiologie")

# Import library -----

library(tidyverse)      # Manipulation des données et graphiques
library(conflicted)     # Gestion des conflits de noms de fonctions entre packages
library(readxl)         # Lecture fichier xlsx
library(knitr)          # Création de tableau
library(gridExtra)      # Arrangement de graphique
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

# Données remplies par le médecin
analysis <- read_excel("data/ESMAA_analysis.xlsx")

# Données remplies par le patient
patient <- read_excel("data/ESMAA_patient.xlsx")
