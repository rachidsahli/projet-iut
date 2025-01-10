#_______________________________________________________________________________
# COURS     : SCORING
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
# DONNEES   : Agence de voyage
#
# PROGRAMME : 10 - Nettoyage sur base 2023
#_______________________________________________________________________________


# Chargement de la base 2023 -----

base_2023 <- read.table("~/Documents/Iut/projets but stid/2024-2025/Processus_datamining/Processus_datamining/Data/base_tourisme_2023_12.txt", 
                        encoding = "UTF-8",
                        sep = ";",
                        header = TRUE,
                        na.strings = "")  # Année 2023

# 1 - Nettoyage  -----

var_dates_2023 <- base_2023 %>% 
  select(starts_with("date")) %>% 
  colnames()

var_ql_2023 <- base_2023 %>%
  select(where(is.character),
         starts_with("flag"),
         - starts_with("date"), - id_client) %>% 
  colnames()

var_qt_2023 <- base_2023 %>%
  select(where(is.numeric),
         - all_of(var_ql_2023), - all_of(var_dates_2023), - id_client) %>% 
  colnames()

# Conversion des variables pour homogénéiser le type informatique des variables en fonction de leur type statistique :
# - variables dates importés en chanes de caractires converties en dates
# - variables qualitatives importées en chaines de caractères, entiers ou numériques converties en facteurs
# - variables importées en entiers converties en num?riques

base_2023 <- base_2023 %>% 
  mutate(across(all_of(var_dates_2023), ~ as.Date(.x, format = "%d/%m/%Y")),
         across(c(all_of(var_ql_2023)), as.factor),
         across(where(is.integer), as.numeric))

# Nettoyage de l'envrionnement

rm(var_dates_2023,var_ql_2023,var_qt_2023)

# Renommage des variables -----

base_2023 <- base_2023 %>% 
  rename(nb_sejours_2020 = "nb_sejours_2021",
         nb_sejours_2021 = "nb_sejours_2022",
         nb_sejours_2022 = "nb_sejours_2023",
         mt_sejours_2020 = "mt_sejours_2021",
         mt_sejours_2021 = "mt_sejours_2022",
         mt_sejours_2022 = "mt_sejours_2023",
         duree_sejours_2020 = "duree_sejours_2021",
         duree_sejours_2021 = "duree_sejours_2022",
         duree_sejours_2022 = "duree_sejours_2023",
         mt_extras_sejours_2020 = "mt_extras_sejours_2021",
         mt_extras_sejours_2021 = "mt_extras_sejours_2022",
         mt_extras_sejours_2022 = "mt_extras_sejours_2023")





# 2 - Nettoyage (le même que sur 2022) -----

## Imputation par la valeur 0 -----
base_2023$nb_enfants_premier_sejour[is.na(base_2023$nb_enfants_premier_sejour)] <- 0
base_2023$nb_enfants_dernier_sejour[is.na(base_2023$nb_enfants_dernier_sejour)] <- 0 # 2022

base_2023$mt_extras_sejours_2021[is.na(base_2023$mt_extras_sejours_2021)] <- 0

base_2023$nb_adultes_premier_sejour[is.na(base_2023$nb_adultes_premier_sejour)] <- 0
base_2023$nb_adultes_dernier_sejour[is.na(base_2023$nb_adultes_dernier_sejour)] <- 0

## Imputation par la valeur Aucun -----
base_2023$liste_extras <- as.character(base_2023$liste_extras) # conversion en character
base_2023$liste_extras[is.na(base_2023$liste_extras)] <- "Aucun" # imputation
base_2023$liste_extras <- as.factor(base_2023$liste_extras) # conversion en factor

## Imputation par le calcul de la moyenne -----
base_2023 <- base_2023 %>% 
  mutate(duree_sejour_moyen = if_else(
    is.na(duree_sejour_moyen),
    round((duree_sejours_2020 + duree_sejours_2021 + duree_sejours_2022) / 
            rowSums(select(., duree_sejours_2020, duree_sejours_2021, duree_sejours_2022) > 0), 2),
    duree_sejour_moyen
  ))

## Imputation par la valeur de canal_reservation_dernier_sejour -----
base_2023 <- base_2023 %>%
  mutate(canal_reservation_premier_sejour = if_else(is.na(canal_reservation_premier_sejour), 
                                                    canal_reservation_dernier_sejour, 
                                                    canal_reservation_premier_sejour))

# Transformation du type de variables
base_2023$gamme_premier_sejour <- as.factor(base_2023$gamme_premier_sejour)
base_2023$gamme_dernier_sejour <- as.factor(base_2023$gamme_dernier_sejour)
