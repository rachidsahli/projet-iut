#_______________________________________________________________________________
# COURS     : SCORING
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
# DONNEES   : Agence de voyage
#
# PROGRAMME : 1 - NETTOYAGE
#_______________________________________________________________________________


# Identification des types statistiques des variables :
# - dates
# - qualitatives : variables caractères, variables numériques binaires (hors dates, identifiant et code postal)
# - quantitatives : variables numériques continues ou discrètes (hors dates, qualitatives, identifiant et code postal)

# 2022

var_dates_2022 <- base_2022 %>% 
  select(starts_with("date")) %>% 
  colnames()

var_ql_2022 <- base_2022 %>%
  select(where(is.character),
         starts_with("flag"),
         - starts_with("date"), - id_client) %>% 
  colnames()

var_qt_2022 <- base_2022 %>%
  select(where(is.numeric),
         - all_of(var_ql_2022), - all_of(var_dates_2022), - id_client) %>% 
  colnames()

# 2023

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
# - variables dates importées en chaînes de caractères converties en dates
# - variables qualitatives importées en chaînes de caractères, entiers ou numériques converties en facteurs
# - variables importées en entiers converties en numériques

base_2022 <- base_2022 %>% 
  mutate(across(all_of(var_dates_2022), ~ as.Date(.x, format = "%d/%m/%Y")),
         across(c(all_of(var_ql_2022), "flag_reachat"), as.factor),
         across(where(is.integer), as.numeric))

base_2023 <- base_2023 %>% 
  mutate(across(all_of(var_dates_2023), ~ as.Date(.x, format = "%d/%m/%Y")),
         across(all_of(var_ql_2023), as.factor),
         across(where(is.integer), as.numeric))

# Nettoyage de l'envrionnement

rm(var_dates_2022,var_ql_2022,var_qt_2022,var_dates_2023,var_ql_2023,var_qt_2023)

