#_______________________________________________________________________________
# COURS     : Risque banque
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
#
# PROGRAMME : FONCTIONS
#_______________________________________________________________________________


# Check valeurs vides dans variables

check_vide <- function(data) {
  sapply(data, function(col) {
    if (is.factor(col)) {
      sum(is.na(col) | col == "")
    } else {
      sum(is.na(col))
    }
  })
}




