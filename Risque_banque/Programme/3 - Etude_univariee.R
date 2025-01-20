#_______________________________________________________________________________
# COURS     : Risque banque
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
#
# PROGRAMME : 3 - ETUDE_UNIVARIEE
#_______________________________________________________________________________

# Variables quantitatives 

var_quanti <- sapply(periode_2023, is.numeric)
lapply(names(periode_2023)[var_quanti], function(var) plot_quanti(periode_2023, var))

# Variables qualitatives

var_quali <- sapply(periode_2023, function(x) is.factor(x) || is.character(x))
lapply(names(periode_2023)[var_quali], function(var) plot_quali(periode_2023, var))
