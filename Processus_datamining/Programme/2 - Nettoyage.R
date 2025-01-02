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

pct_miss(base_2022) ## 5 % de valeurs manquantes total

## Valeurs manquantes par variables

skim(base_2022) ### Audit

pct_miss_var(base_2022) # 20 % des variables contiennent des valeurs manquantes

gg_miss_var(base_2022, show_pct = TRUE) # valeurs manquantes en % par variables (graphique)

vis_miss(base_2022, warn_large_data = FALSE) # valeurs manquantes en % par variables (graphique)

miss_var_summary(base_2022) # resulé des valeurs manquantes par variables

# Traitement variable par variable -----

## nb_enfants_premier sejour et nb_enfants_dernier_sejours         
## Ici, les NA ne sont pas des anomalies

## liste_extras 
## Les NA ne sont pas des anomalies

## duree_sejour_moyen
## Les NA sont des anomalies car les observations ont des valeurs renseignés
## dans les variables duree_sejours


# Suppression de la variable nationalite car inutile -----
base_2022 <- subset(base_2022, select = -nationalite)
base_2023 <- subset(base_2023, select = -nationalite)

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




















