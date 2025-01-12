#_______________________________________________________________________________
# COURS     : Risque banque
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
#
# PROGRAMME : 2 - NETTOYAGE
#_______________________________________________________________________________



# 2021

# Traitement global de la base -----

pct_miss(periode_2021) ## 0 % de valeurs manquantes total

## Valeurs manquantes par variables

skim(periode_2021) ### Audit

kable(check_vide(periode_2021), col.names = c("Variables","Valeurs vides")) # Valeur "vide"
# PD_ID_NDD : 1982 valeurs vides

