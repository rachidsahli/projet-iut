#_______________________________________________________________________________
# COURS     : SCORING
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
# DONNEES   : Agence de voyage
#
# PROGRAMME : 4 - Création de nouvelles variables
#_______________________________________________________________________________


# Séparation de la variable liste_extras -----

liste_extras <- base_2022 %>% # Liste unique des extras 
  pull(liste_extras) %>%           
  as.character() %>%               
  strsplit(split = "\\|") %>%      
  unlist() %>%                     
  unique()

# Création de variables binaires pour chaque extras

for (extra in liste_extras) {
  base_2022 <- base_2022 %>%
    mutate(
      !!extra := ifelse(str_detect(liste_extras, fixed(extra)), 1, 0),
      !!extra := as.factor(!!sym(extra))  # Convertir la nouvelle variable en facteur
    )
}

rm(extra,liste_extras) # Nettoyage de l'environnement

# Création de quelques variables -----

base_2022 <- base_2022 %>% 
  mutate(
    parrain_filleul = as.factor(ifelse(flag_est_parrain == 1 & flag_est_filleul == 1, 1, 0)),
    meme_saison = as.factor(ifelse(saison_premier_sejour == saison_dernier_sejour, 1, 0)),
    meme_gamme = as.factor(ifelse(gamme_premier_sejour == gamme_dernier_sejour, 1, 0)),
    meme_pays = as.factor(ifelse(pays_premier_sejour == pays_dernier_sejour, 1, 0)),
    meme_type_destination = as.factor(ifelse(type_destination_premier_sejour == type_destination_dernier_sejour, 1, 0)),
    duree_jours_premier_sejour = as.numeric(difftime(date_fin_premier_sejour, date_debut_premier_sejour, units = "days")),
    duree_jours_dernier_sejour = as.numeric(difftime(date_fin_dernier_sejour, date_debut_dernier_sejour, units = "days"))
  ) %>% 
  mutate(
    mt_sejours_moyen = (mt_sejours_2020 + mt_sejours_2021 + mt_sejours_2022) / (as.numeric(nb_sejours_2020) + as.numeric(nb_sejours_2021) + as.numeric(nb_sejours_2022)),
    ratio_extras = (mt_extras_sejours_2020 + mt_extras_sejours_2021 + mt_extras_sejours_2022) / (mt_sejours_2020 + mt_sejours_2021 + mt_sejours_2022),
    meme_caract_sejours = as.factor(ifelse(saison_premier_sejour == saison_dernier_sejour &
                                             type_destination_premier_sejour == type_destination_dernier_sejour &
                                             gamme_premier_sejour == gamme_dernier_sejour &
                                             nb_adultes_premier_sejour == nb_adultes_dernier_sejour &
                                             nb_enfants_premier_sejour == nb_enfants_dernier_sejour &
                                             pays_premier_sejour == pays_dernier_sejour &
                                             canal_reservation_premier_sejour == canal_reservation_dernier_sejour, 1, 0
                                             ))
  )

base_2022 <- base_2022 %>% 
  mutate(
    duree_sejour_moyen_class = case_when(
      duree_sejour_moyen == 7 ~ "1 semaine",
      duree_sejour_moyen > 7 & duree_sejour_moyen <= 14 ~ "1 à 2 semaines",
      duree_sejour_moyen > 14 & duree_sejour_moyen <= 21 ~ "2 à 3 semaines",
      duree_sejour_moyen > 21 ~ "Plus de 3 semaines",
      TRUE ~ "Autre"),
    
    duree_sejours_2020_class = case_when(
      duree_sejours_2020 == 7 ~ "1 semaine",
      duree_sejours_2020 > 7 & duree_sejours_2020 <= 14 ~ "1 à 2 semaines",
      duree_sejours_2020 > 14 & duree_sejours_2020 <= 21 ~ "2 à 3 semaines",
      duree_sejours_2020 > 21 ~ "Plus de 3 semaines",
      TRUE ~ "Autre"),
    
    duree_sejours_2021_class = case_when(
      duree_sejours_2021 == 7 ~ "1 semaine",
      duree_sejours_2021 > 7 & duree_sejours_2021 <= 14 ~ "1 à 2 semaines",
      duree_sejours_2021 > 14 & duree_sejours_2021 <= 21 ~ "2 à 3 semaines",
      duree_sejours_2021 > 21 ~ "Plus de 3 semaines",
      TRUE ~ "Autre"),
    
    duree_sejours_2022_class = case_when(
      duree_sejours_2022 == 7 ~ "1 semaine",
      duree_sejours_2022 > 7 & duree_sejours_2022 <= 14 ~ "1 à 2 semaines",
      duree_sejours_2022 > 14 & duree_sejours_2022 <= 21 ~ "2 à 3 semaines",
      duree_sejours_2022 > 21 ~ "Plus de 3 semaines",
      TRUE ~ "Autre")
  )

# Renommage de la variable Bien-être
base_2022 <- base_2022 %>% 
  rename( "Bien_etre" = `Bien-être`)


# Suppression de variables inutiles
base_2022 <- subset(base_2022, select = -c(pays_premier_sejour, pays_dernier_sejour, nationalite, 
                                           date_debut_premier_sejour, date_fin_premier_sejour, date_debut_dernier_sejour,
                                           date_fin_dernier_sejour, liste_extras, mt_sejours_moyen))

skim(base_2022) ### Audit
