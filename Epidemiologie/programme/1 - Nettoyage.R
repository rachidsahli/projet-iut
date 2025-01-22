#_______________________________________________________________________________
# COURS     : Epidemiologie
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
# DONNEES   : Etude ESMAA
#
# PROGRAMME : 1 - NETTOYAGE
#_______________________________________________________________________________

# Filtre des sujets algeriens -----

analysis <- analysis %>% 
  filter(Country == "Algeria")


# Jointure des deux fichiers -----
