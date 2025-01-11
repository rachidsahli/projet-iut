#_______________________________________________________________________________
# COURS     : Risque banque
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
#
# PROGRAMME : 1 - NETTOYAGE
#_______________________________________________________________________________

# Identification des types statistiques des variables :
# - dates
# - qualitatives : variables caractères, variables numériques binaires (hors dates, identifiant et code postal)
# - quantitatives : variables numériques continues ou discrètes (hors dates, qualitatives, identifiant et code postal)

# 2021

periode_2021 <- periode_2021 %>%
  mutate(across(everything(), ~ { attr(., "label") <- NULL; . })) # Suppression des labes de toutes les var

periode_2021 <- periode_2021 %>%
  mutate(across(where(is.character), as.factor)) # Transformation en factor() des var ql

periode_2021$DS_DT_GESTIONDEF <- as.Date(periode_2021$DS_DT_GESTIONDEF) # Transformation au format date



# 2022

periode_2022 <- periode_2022 %>%
  mutate(across(everything(), ~ { attr(., "label") <- NULL; . })) # Suppression des labes de toutes les var

periode_2022 <- periode_2022 %>%
  mutate(across(where(is.character), as.factor)) # Transformation en factor() des var ql

periode_2022$DS_DT_GESTIONDEF <- as.Date(periode_2022$DS_DT_GESTIONDEF) # Transformation au format date



# 2023

periode_2023 <- periode_2023 %>%
  mutate(across(everything(), ~ { attr(., "label") <- NULL; . })) # Suppression des labes de toutes les var

periode_2023 <- periode_2023 %>%
  mutate(across(where(is.character), as.factor)) # Transformation en factor() des var ql

periode_2023$DS_DT_GESTIONDEF <- as.Date(periode_2023$DS_DT_GESTIONDEF) # Transformation au format date



# REFERENCE

periode_reference <- periode_reference %>%
  mutate(across(everything(), ~ { attr(., "label") <- NULL; . })) # Suppression des labes de toutes les var

periode_reference <- periode_reference %>%
  mutate(across(where(is.character), as.factor)) # Transformation en factor() des var ql

periode_reference$DS_DT_GESTIONDEF <- as.Date(periode_reference$DS_DT_GESTIONDEF) # Transformation au format date