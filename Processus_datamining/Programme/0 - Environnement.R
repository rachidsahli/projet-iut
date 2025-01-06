#_______________________________________________________________________________
# COURS     : SCORING
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
# DONNEES   : Agence de voyage
#
# PROGRAMME : 0 - ENVIRONNEMENT
#_______________________________________________________________________________


# Import library -----

library(tidyverse)      # Manipulation des données et graphiques
library(conflicted)     # Gestion des conflits de noms de fonctions entre packages
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
library(rpart)          # Modélisation - Arbre de décision
library(rpart.plot)     # Modélisation - Arbre de décision
library(class)          # Modélisation - k plus proches voisins

# Gestion des conflits -----

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("slice", "dplyr")

# Import data -----

base_2022 <- read.table("~/Documents/Iut/projets but stid/2024-2025/Processus_datamining/Processus_datamining/Data/base_tourisme_2022_12.txt", 
                        encoding = "UTF-8",
                        sep = ";",
                        header = TRUE,
                        na.strings = "")  # Année 2022

base_2023 <- read.table("~/Documents/Iut/projets but stid/2024-2025/Processus_datamining/Processus_datamining/Data/base_tourisme_2023_12.txt", 
                        encoding = "UTF-8",
                        sep = ";",
                        header = TRUE,
                        na.strings = "") # Année 2023
