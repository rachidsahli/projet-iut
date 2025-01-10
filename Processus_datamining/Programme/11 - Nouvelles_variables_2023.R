#_______________________________________________________________________________
# COURS     : SCORING
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
# DONNEES   : Agence de voyage
#
# PROGRAMME : 10 - Nouvelles variables 2023
#_______________________________________________________________________________


# Séparation de la variable liste_extras -----

liste_extras <- base_2023 %>% # Liste unique des extras 
  pull(liste_extras) %>%           
  as.character() %>%               
  strsplit(split = "\\|") %>%      
  unlist() %>%                     
  unique()

# Création de variables binaires pour chaque extras

for (extra in liste_extras) {
  base_2023 <- base_2023 %>%
    mutate(
      !!extra := ifelse(str_detect(liste_extras, fixed(extra)), 1, 0),
      !!extra := as.factor(!!sym(extra))  # Convertir la nouvelle variable en facteur
    )
}

rm(extra,liste_extras) # Nettoyage de l'environnement

# Création de quelques variables -----

base_2023 <- base_2023 %>% 
  mutate(
    parrain_filleul = as.factor(ifelse(flag_est_parrain == 1 & flag_est_filleul == 1, 1, 0)),
    meme_saison = as.factor(ifelse(saison_premier_sejour == saison_dernier_sejour, 1, 0)),
    meme_gamme = as.factor(ifelse(gamme_premier_sejour == gamme_dernier_sejour, 1, 0)),
    meme_pays = as.factor(ifelse(pays_premier_sejour == pays_dernier_sejour, 1, 0)),
    meme_type_destination = as.factor(ifelse(type_destination_premier_sejour == type_destination_dernier_sejour, 1, 0)),
    duree_jours_premier_sejour = as.numeric(difftime(date_fin_premier_sejour, date_debut_premier_sejour, units = "days")),
    duree_jours_dernier_sejour = as.numeric(difftime(date_fin_dernier_sejour, date_debut_dernier_sejour, units = "days")),
    statut_famille_premier_sejour = as.factor(ifelse(!is.na(nb_adultes_premier_sejour) & !is.na(nb_enfants_premier_sejour) & nb_adultes_premier_sejour > 0 & nb_enfants_premier_sejour > 0, 1, 0)),
    statut_famille_dernier_sejour = as.factor(ifelse(!is.na(nb_adultes_dernier_sejour) & !is.na(nb_enfants_dernier_sejour) & nb_adultes_dernier_sejour > 0 & nb_enfants_dernier_sejour > 0, 1, 0)),
    meme_statut_famille = as.factor(ifelse(statut_famille_premier_sejour == statut_famille_dernier_sejour, 1, 0)),
    statut_duree_sejour = factor(ifelse(!is.na(duree_jours_premier_sejour) & duree_jours_premier_sejour > 7, "Long séjour", "Court séjour"))
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
                                             canal_reservation_premier_sejour == canal_reservation_dernier_sejour, 1, 0))
  )


# Renommage de la variable Bien-être

base_2023 <- base_2023 %>% 
  rename( "Bien_etre" = `Bien-être`)



# Application des traitements de l'étude univarié -----

## Modif sur la variable nb_residences_distinctes

base_2023$nb_residences_distinctes <- ifelse(base_2023$nb_residences_distinctes == 1, 
                                             "Residence unique", 
                                             "Residence multiples")
base_2023$nb_residences_distinctes <- as.factor(base_2023$nb_residences_distinctes)

## Modif sur la variable duree_sejour_moyen

base_2023$duree_sejour_moyen <- cut(base_2023$duree_sejour_moyen, 
                                    breaks = c(0, 6, 7, Inf), 
                                    labels = c("Court", "Moyen", "Long"), 
                                    include.lowest = TRUE)

## Les deux variables pays ont trop de modalités, on crée des variables continents

base_2023 <- base_2023 %>% 
  mutate(
    continent_premier_sejour = as.factor(case_when(
      pays_premier_sejour %in% c("France", "Portugal", "Italy", "Switzerland", "Greece", "Turkey") ~ "Europe",
      pays_premier_sejour %in% c("Morocco", "Senegal", "Tunisia", "Egypt", "Mauritius Islands") ~ "Afrique",
      pays_premier_sejour %in% c("Bahamas", "Dominican Republic", "Guadeloupe", "Martinique", "Turks And Caicos", "Brazil", "Usa", "Mexico") ~ "Amériques",
      pays_premier_sejour %in% c("China", "Japan", "Malaysia", "Thailand", "Indonesia", "Maldivian Islands Republic") ~ "Asie",
      TRUE ~ "Autre")),
    continent_dernier_sejour = as.factor(case_when(
      pays_dernier_sejour %in% c("France", "Portugal", "Italy", "Switzerland", "Greece", "Turkey") ~ "Europe",
      pays_dernier_sejour %in% c("Morocco", "Senegal", "Tunisia", "Egypt", "Mauritius Islands") ~ "Afrique",
      pays_dernier_sejour %in% c("Bahamas", "Dominican Republic", "Guadeloupe", "Martinique", "Turks And Caicos", "Brazil", "Usa", "Mexico") ~ "Amériques",
      pays_dernier_sejour %in% c("China", "Japan", "Malaysia", "Thailand", "Indonesia", "Maldivian Islands Republic") ~ "Asie",
      TRUE ~ "Autre"
    ))
  )

base_2023 <- subset(base_2023, select = -c(nationalite,pays_premier_sejour,pays_dernier_sejour,
                                           date_debut_premier_sejour, date_fin_premier_sejour, date_debut_dernier_sejour,
                                           date_fin_dernier_sejour, liste_extras))

base_2023 <- base_2023 %>%
  mutate(
    duree_totale_sejours = duree_sejours_2020 + duree_sejours_2021 + duree_sejours_2022
  )


base_2023$duree_sejour_libelle <- ifelse(base_2023$duree_totale_sejours<7,"<1week",
                                         ifelse(base_2023$duree_totale_sejours==7,"1week",">1week"))


skim(base_2023)






