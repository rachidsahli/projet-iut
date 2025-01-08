#_______________________________________________________________________________
# COURS     : SCORING
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
# DONNEES   : Agence de voyage
#
# PROGRAMME : 7 - Regression logistique
#_______________________________________________________________________________


# Modèle manuel -----

liste_var <- setdiff(names(base_2022), c("id_client", "flag_reachat"))

formule <- as.formula(paste("flag_reachat ~ ", paste(liste_var, collapse = " + ")))

rl <- glm(formula = formule,
          data = apprentissage,
          family = "binomial")

# Diagnostics du modèle ----
