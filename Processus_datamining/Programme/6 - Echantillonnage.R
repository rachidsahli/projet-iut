#_______________________________________________________________________________
# COURS     : SCORING
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
# DONNEES   : Agence de voyage
#
# PROGRAMME : 5 - Echantillonnage
#_______________________________________________________________________________


# Echantillonnage stratifié sur la variable cible
# - apprentissage : 70%
# - test : 30%

set.seed(123)

index_apprentissage <- as.vector(createDataPartition(base_2022$flag_reachat,
                                                     p = 0.7,
                                                     list = FALSE))

apprentissage <- base_2022 %>%
  slice(index_apprentissage)

test <- base_2022 %>%
  slice(- index_apprentissage)

# Vérification que la proportion de réchateurs est la même dans chaque base

freq(apprentissage$flag_reachat)
freq(test$flag_reachat)
