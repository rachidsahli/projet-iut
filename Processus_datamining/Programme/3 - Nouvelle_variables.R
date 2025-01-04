#_______________________________________________________________________________
# COURS     : SCORING
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
# DONNEES   : Agence de voyage
#
# PROGRAMME : 4 - Création de nouvelles variables
#_______________________________________________________________________________


# Création de quelques variables

base_2022 <- base_2022 %>% 
  mutate(
    parrain_filleul = ifelse(flag_est_parrain == 1 & flag_est_filleul == 1, 1, 0),
    total_extras = mt_extras_sejours_2020 + mt_extras_sejours_2021 + mt_extras_sejours_2022,
    meme_saison = ifelse(saison_premier_sejour == saison_dernier_sejour, 1, 0),
    meme_gamme = ifelse(gamme_premier_sejour == gamme_dernier_sejour, 1, 0),
    meme_pays = ifelse(pays_premier_sejour == pays_dernier_sejour, 1, 0),
    meme_type_destination = ifelse(type_destination_premier_sejour == type_destination_dernier_sejour, 1, 0),
    nb_sejours_total = nb_sejours_2020 + nb_sejours_2021 + nb_sejours_2022,
    mt_sejours_total = mt_sejours_2020 + mt_sejours_2021 + mt_sejours_2022,
    nb_participants_sejours_total = nb_participants_sejours_2020 + nb_participants_sejours_2021 + nb_participants_sejours_2022,
    nb_participants_premier_sejours = nb_adultes_premier_sejour + nb_enfants_premier_sejour,
    nb_participants_dernier_sejours = nb_adultes_dernier_sejour + nb_enfants_dernier_sejour,
    delai_mois = interval(date_fin_premier_sejour, date_debut_dernier_sejour) %/% months(1)
  ) %>% 
  mutate(
    mt_sejours_moyen = ifelse(nb_sejours_total > 0, mt_sejours_total / nb_sejours_total, 0)
  )
