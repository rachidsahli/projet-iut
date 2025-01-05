#_______________________________________________________________________________
# COURS     : SCORING
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
# DONNEES   : Agence de voyage
#
# PROGRAMME : 4 - Création de nouvelles variables
#_______________________________________________________________________________


# Séparation de la variable liste_extras

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

# Création de quelques variables

base_2022 <- base_2022 %>% 
  mutate(
    parrain_filleul = as.factor(ifelse(flag_est_parrain == 1 & flag_est_filleul == 1, 1, 0)),
    total_extras = mt_extras_sejours_2020 + mt_extras_sejours_2021 + mt_extras_sejours_2022,
    meme_saison = as.factor(ifelse(saison_premier_sejour == saison_dernier_sejour, 1, 0)),
    meme_gamme = as.factor(ifelse(gamme_premier_sejour == gamme_dernier_sejour, 1, 0)),
    meme_pays = as.factor(ifelse(pays_premier_sejour == pays_dernier_sejour, 1, 0)),
    meme_type_destination = as.factor(ifelse(type_destination_premier_sejour == type_destination_dernier_sejour, 1, 0)),
    nb_sejours_total = nb_sejours_2020 + nb_sejours_2021 + nb_sejours_2022,
    mt_sejours_total = mt_sejours_2020 + mt_sejours_2021 + mt_sejours_2022,
    nb_participants_sejours_total = nb_participants_sejours_2020 + nb_participants_sejours_2021 + nb_participants_sejours_2022,
    nb_participants_premier_sejours = nb_adultes_premier_sejour + nb_enfants_premier_sejour,
    nb_participants_dernier_sejours = nb_adultes_dernier_sejour + nb_enfants_dernier_sejour,
    delai_mois = interval(date_fin_premier_sejour, date_debut_dernier_sejour) %/% months(1),
    nb_extras = ifelse(Aucun != 1, 
                       as.numeric(as.character(Boutique)) + 
                         as.numeric(as.character(Excursion)) + 
                         as.numeric(as.character(`Bien-être`)) + 
                         as.numeric(as.character(Services)) + 
                         as.numeric(as.character(Restauration)) + 
                         as.numeric(as.character(Sports)) + 
                         as.numeric(as.character(Enfants)), 
                       0)
  ) %>% 
  mutate(
    mt_sejours_moyen = ifelse(nb_sejours_total > 0, mt_sejours_total / nb_sejours_total, 0)
  )

skim(base_2022) ### Audit