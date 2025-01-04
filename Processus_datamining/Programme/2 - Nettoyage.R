#_______________________________________________________________________________
# COURS     : SCORING
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
# DONNEES   : Agence de voyage
#
# PROGRAMME : 2 - NETTOYAGE
#_______________________________________________________________________________


# Traitement global de la base -----

pct_miss(base_2022) ## 4.67 % de valeurs manquantes total

## Valeurs manquantes par variables

skim(base_2022) ### Audit

pct_miss_var(base_2022) # 20 % des variables contiennent des valeurs manquantes

gg_miss_var(base_2022, show_pct = TRUE) # valeurs manquantes en % par variables (graphique)

vis_miss(base_2022, warn_large_data = FALSE) # valeurs manquantes en % par variables (graphique)

gg_miss_upset(base_2022) # visualisation par groupe de donn?es

miss_var_summary(base_2022) # resumé des valeurs manquantes par variables

# Traitement variable par variable -----

# Suppression de la variable nationalite car inutile -----
base_2022 <- subset(base_2022, select = -nationalite)

## Imputation par la valeur 0 -----
base_2022$nb_enfants_premier_sejour[is.na(base_2022$nb_enfants_premier_sejour)] <- 0
base_2022$nb_enfants_dernier_sejour[is.na(base_2022$nb_enfants_dernier_sejour)] <- 0 # 2022

## Imputation par la valeur Aucun -----
base_2022$liste_extras <- as.character(base_2022$liste_extras) # conversion en character
base_2022$liste_extras[is.na(base_2022$liste_extras)] <- "Aucun" # imputation
base_2022$liste_extras <- as.factor(base_2022$liste_extras) # conversion en factor

## Imputation par le calcul de la moyenne -----
base_2022 <- base_2022 %>% 
  mutate(duree_sejour_moyen = if_else(
    is.na(duree_sejour_moyen),
    round((duree_sejours_2020 + duree_sejours_2021 + duree_sejours_2022) / 
            rowSums(select(., duree_sejours_2020, duree_sejours_2021, duree_sejours_2022) > 0), 2),
    duree_sejour_moyen
  ))

## Imputation par la valeur de canal_reservation_dernier_sejour -----
base_2022 <- base_2022 %>%
  mutate(canal_reservation_premier_sejour = if_else(is.na(canal_reservation_premier_sejour), 
                                                    canal_reservation_dernier_sejour, 
                                                    canal_reservation_premier_sejour))

## Suppression des observations sur les variables suivantes : 
## - nb_adultes_premier_sejour
## - nb_adultes_dernier_sejour
## - mt_extras_sejours_2021 (Pour l'instant)

# 493 observations n'ont pas de valeurs sur ces deux variables
sum(is.na(base_2022$nb_adultes_dernier_sejour) & is.na(base_2022$nb_adultes_premier_sejour))

base_2022 <- base_2022[!is.na(base_2022$nb_adultes_premier_sejour),] # premier sejour
base_2022 <- base_2022[!is.na(base_2022$nb_adultes_dernier_sejour),] # dernier sejour

base_2022 <- base_2022[!is.na(base_2022$mt_extras_sejours_2021),] # mt_extras_sejours_2021


# Application des traitements d?finis -----

## Nb_enfants_premiers_sejour : 34 132 valeurs manquantes
## => Imputation par la valeur 0

## Nb_enfants_dernier_sejour : 34 137 valeurs manquantes
## => Imputation par la valeur 0

## Liste_extras : 23 151 valeurs manquantes
## => Imputation par la valeur "Aucun"

## Duree_sejour_moyen : 2532 valeurs manquantes
## => Imputation par le calcul de la moyenne

## Canal_reservation_premier_sejour : 1540 valeurs manquantes
## => Imputation par la valeur du canal_reservation_dernier_sejour de la personne

## Nb_adultes_premier_sejour : 526 valeurs manquantes
## => Suppression des observations

## Nb_adultes_dernier_sejour : 542 valeurs manquantes
## => Suppression des observations

## mt_extras_sejours_2021 : 1100 valeurs manquantes
## => Suppression des observations

## Au total, suppression de 1656 (3.25 % de la base initial).
## Verification des valeurs manquantes dans la base

skim(base_2022)