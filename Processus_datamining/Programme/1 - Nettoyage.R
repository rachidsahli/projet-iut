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
# - qualitatives : variables caract?res, variables num?riques binaires (hors dates, identifiant et code postal)
# - quantitatives : variables num?riques continues ou discr?tes (hors dates, qualitatives, identifiant et code postal)

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

# Conversion des variables pour homogénéiser le type informatique des variables en fonction de leur type statistique :
# - variables dates importés en chanes de caractires converties en dates
# - variables qualitatives importées en chaines de caractères, entiers ou numériques converties en facteurs
# - variables importées en entiers converties en num?riques

base_2022 <- base_2022 %>% 
  mutate(across(all_of(var_dates_2022), ~ as.Date(.x, format = "%d/%m/%Y")),
         across(c(all_of(var_ql_2022), "flag_reachat"), as.factor),
         across(where(is.integer), as.numeric))

# Nettoyage de l'envrionnement

rm(var_dates_2022,var_ql_2022,var_qt_2022)

