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
## PD_ID_NDD (Classe de risque de défaut) : 1982 valeurs vides



# 2022

## Traitement global de la base -----

pct_miss(periode_2022) ## 0 % de valeurs manquantes total

## Valeurs manquantes par variables

skim(periode_2022) ### Audit

kable(check_vide(periode_2022), col.names = c("Variables","Valeurs vides")) # Valeur "vide"
## PD_ID_NDD : 1934 valeurs vides



# 2023

## Traitement global de la base -----

pct_miss(periode_2023) ## 0 % de valeurs manquantes total

## Valeurs manquantes par variables

skim(periode_2023) ### Audit

kable(check_vide(periode_2023), col.names = c("Variables","Valeurs vides")) # Valeur "vide"
## PD_ID_NDD : 4347 valeurs vides



# REFERENCE

## Traitement global de la base -----

pct_miss(periode_reference) ## 0 % de valeurs manquantes total

## Valeurs manquantes par variables

skim(periode_reference) ### Audit

kable(check_vide(periode_reference), col.names = c("Variables","Valeurs vides")) # Valeur "vide"
## PD_ID_NDD : 1769 valeurs vides


# TRAITEMENT DES VALEURS VIDES -----



