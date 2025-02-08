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

analysis_data <- analysis_data %>% 
  filter(Country == "Algeria")

# Merge des deux fichiers -----

data <- analysis_data %>% 
  left_join(patient_data, by = "Patient")

rm(analysis_data, patient_data)

# Remplacement des "." par NA -----

data <- data %>% mutate(across(everything(), ~ replace(., . == ".", NA)))

# Variables à convertir en numéric

var_to_numeric <- c("Age", "Weight (in kg)", "Height (in cm)",
                    "BMI (in kg/m²)", "History of disease", "ACT global",
                    "SF-8 Physical...24", "SF-8 Role...22", "SF-8 Bodily",
                    "SF-8 General", "SF-8 Vitality", "SF-8 Social",
                    "SF-8 Mental...23", "SF-8 Role...17",
                    "SF-8 Mental...25", "SF-8 Physical...16")

data <- data %>% mutate_at(var_to_numeric, as.numeric)

rm(var_to_numeric)

# Convertion des autres variables en facteur

data <- data %>% mutate_at(setdiff(names(data), c("Patient", "Site..", var_to_numeric)), as.factor)

# Suppression des variables à un seul facteur

data <- data %>% select_if(function(x) length(unique(x)) > 1)

# Uniformisation des facteurs

data <- data %>% mutate_if(is.factor, fct_explicit_na) # dans les variables qualitatives, on remplace les NA par "(Missing)"
data <- data %>% mutate_if(is.factor, ~ if ("None" %in% levels(.)) fct_recode(., "No" = "None") else .) # on remplace "None" par "No"
data <- data %>% mutate_if(is.factor, ~ if ("YES" %in% levels(.)) fct_recode(., "Yes" = "YES") else .) # on remplace "YES" par "Yes"
data <- data %>% mutate_if(is.factor, ~ if ("NO" %in% levels(.)) fct_recode(., "No" = "NO") else .) # on remplace "NO" par "No"

# Audit de la base -----

skim(data)