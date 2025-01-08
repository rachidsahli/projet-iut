#_______________________________________________________________________________
# COURS     : SCORING
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
# DONNEES   : Agence de voyage
#
# PROGRAMME : 5 - Etude_bivariee
#_______________________________________________________________________________


# Matrice de corrélation des variables quantitatives -----

## Séléction des variables quantitatives
var_quanti <- base_2022 %>%
  select_if(is.numeric)

## Calcule de la matrice 
matrice <- cor(var_quanti, use = "complete.obs")

## Filtre des variables ayant une assez forte corrélation
matrice[abs(matrice) < 0.4 & matrice > -0.4] <- NA

## Visualisation graphique
corrplot(matrice, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "red")

## Interpétation de la matrice -----

### Corrélation positive -----
#### nb_enfants_premier_sejour et nb_adultes_dernier_sejour : 0.97
#### nb_enfants_premier_sejour et nb_enfants_dernier_sejour : 0.98
#### nb_sejours_2020 et mt_sejours_2020 : 0.76
#### nb_sejours_2021 et mt_sejours_2021 : 0.73
#### nb_sejours_2022 et mt_sejours_2022 : 0.76
#### nb_sejours_2020 et duree_sejours_2020 : 0.92
#### nb_sejours_2021 et duree_sejours_2021 : 0.9
#### nb_sejours_2022 et duree_sejours_2022 : 0.91
#### nb_sejours_2020 et nb_participants_sejours_2020 : 0.84
#### nb_sejours_2021 et nb_participants_sejours_2021 : 0.82
#### nb_sejours_2022 et nb_participants_sejours_2022 : 0.83
#### mt_sejours_2020 et duree_sejours_2020 : 0.82
#### mt_sejours_2020 et nb_participants_sejours_2020 : 0.83
#### mt_sejours_2020 et mt_extras_sejours_2020
#### nb_participants_sejours_2022 et mt_extras_sejours_2022 : 0.54

### Corrélation négative -----
#### nb_sejours_2021 et duree_sejours_2022 : -0.45 
#### duree_sejours_2021 et duree_sejours_2022 : -0.41
####
####







