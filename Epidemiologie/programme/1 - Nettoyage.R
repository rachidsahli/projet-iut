#_______________________________________________________________________________
# COURS     : Epidemiologie
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
# DONNEES   : Etude ESMAA
#
# PROGRAMME : 1 - NETTOYAGE
#_______________________________________________________________________________

# Filtre les sujets algeriens -----

analysis <- analysis %>% 
  filter(Country == "Algeria")

# Jointure des deux fichiers -----
analysis_patient <- analysis %>% 
  left_join(patient, by = "Patient")

# Audit du fichier -----
skim(analysis_patient)







