#_______________________________________________________________________________
# COURS     : Risque banque
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
#
# PROGRAMME : 2 - NETTOYAGE
#_______________________________________________________________________________



# 2021

## Traitement global de la base -----

pct_miss(periode_2021) ## 0 % de valeurs manquantes total

## Valeurs manquantes par variables

skim(periode_2021) ### Audit

kable(check_vide(periode_2021), col.names = c("Variables","Valeurs vides")) # Valeur "vide"
## PD_ID_NDD (Classe de risque de défaut) : 1982 valeurs vides (0.89 %)



# 2022

## Traitement global de la base -----

pct_miss(periode_2022) ## 0 % de valeurs manquantes total

## Valeurs manquantes par variables

skim(periode_2022) ### Audit

kable(check_vide(periode_2022), col.names = c("Variables","Valeurs vides")) # Valeur "vide"
## PD_ID_NDD : 1934 valeurs vides (0.87 %)



# 2023

## Traitement global de la base -----

pct_miss(periode_2023) ## 0 % de valeurs manquantes total

## Valeurs manquantes par variables

skim(periode_2023) ### Audit

kable(check_vide(periode_2023), col.names = c("Variables","Valeurs vides")) # Valeur "vide"
## PD_ID_NDD : 4347 valeurs vides (1.91 %)



# REFERENCE

## Traitement global de la base -----

pct_miss(periode_reference) ## 0 % de valeurs manquantes total

## Valeurs manquantes par variables

skim(periode_reference) ### Audit

kable(check_vide(periode_reference), col.names = c("Variables","Valeurs vides")) # Valeur "vide"
## PD_ID_NDD : 1769 valeurs vides (0.91 %)


# TRAITEMENT DES VALEURS VIDES -----

# Afin de réduire le biais tout en prenant en compte la faible 
# proportion des valeurs manquantes. On remplace ces valeurs vides
# de manière proportionnel à la distribution existante

# 2021

periode_2021$PD_ID_NDD[periode_2021$PD_ID_NDD == ""] <- NA
prop_2021 <- prop.table(table(periode_2021$PD_ID_NDD, useNA = "no"))

set.seed(123)
periode_2021$PD_ID_NDD[is.na(periode_2021$PD_ID_NDD)] <- sample(
  as.numeric(names(prop_2021)),
  size = sum(is.na(periode_2021$PD_ID_NDD)),
  replace = TRUE,
  prob = prop_2021
)

# Vérification des distributions
prop.table(table(periode_2021$PD_ID_NDD, useNA = "no")) # Table initial
prop.table(table(periode_2021$PD_ID_NDD)) # Table après remplacement

periode_2021$PD_ID_NDD <- droplevels(periode_2021$PD_ID_NDD) # Réinitialisation des facteurs

# 2022

periode_2022$PD_ID_NDD[periode_2022$PD_ID_NDD == ""] <- NA
prop_2022 <- prop.table(table(periode_2022$PD_ID_NDD, useNA = "no"))

set.seed(123)
periode_2022$PD_ID_NDD[is.na(periode_2022$PD_ID_NDD)] <- sample(
  as.numeric(names(prop_2022)),
  size = sum(is.na(periode_2022$PD_ID_NDD)),
  replace = TRUE,
  prob = prop_2022
)

# Vérification des distributions
prop.table(table(periode_2022$PD_ID_NDD, useNA = "no")) # Table initial
prop.table(table(periode_2022$PD_ID_NDD)) # Table après remplacement

periode_2022$PD_ID_NDD <- droplevels(periode_2022$PD_ID_NDD) # Réinitialisation des facteurs


# 2023

periode_2023$PD_ID_NDD[periode_2023$PD_ID_NDD == ""] <- NA
prop_2023 <- prop.table(table(periode_2023$PD_ID_NDD, useNA = "no"))

set.seed(123)
periode_2023$PD_ID_NDD[is.na(periode_2023$PD_ID_NDD)] <- sample(
  as.numeric(names(prop_2023)),
  size = sum(is.na(periode_2023$PD_ID_NDD)),
  replace = TRUE,
  prob = prop_2023
)

# Vérification des distributions
prop.table(table(periode_2023$PD_ID_NDD, useNA = "no")) # Table initial
prop.table(table(periode_2023$PD_ID_NDD)) # Table après remplacement

periode_2023$PD_ID_NDD <- droplevels(periode_2023$PD_ID_NDD) # Réinitialisation des facteurs

# REFERENCE

periode_reference$PD_ID_NDD[periode_reference$PD_ID_NDD == ""] <- NA
prop_reference <- prop.table(table(periode_reference$PD_ID_NDD, useNA = "no"))

set.seed(123)
periode_reference$PD_ID_NDD[is.na(periode_reference$PD_ID_NDD)] <- sample(
  as.numeric(names(prop_reference)),
  size = sum(is.na(periode_reference$PD_ID_NDD)),
  replace = TRUE,
  prob = prop_reference
)

# Vérification des distributions
prop.table(table(periode_reference$PD_ID_NDD, useNA = "no")) # Table initial
prop.table(table(periode_reference$PD_ID_NDD)) # Table après remplacement

periode_reference$PD_ID_NDD <- droplevels(periode_reference$PD_ID_NDD) # Réinitialisation des facteurs