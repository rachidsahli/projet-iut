#_______________________________________________________________________________
# COURS     : SCORING
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
# DONNEES   : Agence de voyage
#
# PROGRAMME : 4 - Etude_univariée
#_______________________________________________________________________________

# Résumé statistique univariée
skim(base_2022)

# Graphique variable quantitative -----

plot_quanti <- function(data, variable) {
  var <- data[[variable]]
  par(mfrow = c(1, 2))
  
  # Graphique
  hist(var, main = paste("Histogramme de", variable),
       xlab = variable, col = "lightblue", border = "black")
  boxplot(var, main = paste("Boxplot de", variable),
          ylab = variable, col = "pink")
  
  par(mfrow = c(1, 1))
}

var_quanti <- sapply(base_2022, is.numeric)
lapply(names(base_2022)[var_quanti], function(var) plot_quanti(base_2022, var))

# Interpretation des graphiques

# Nb_adultes_premier_sejour : Valeur extrêmes au dessus de 3 adultes
# Nb_enfants_premier_sejour : Valeur extrêmes au dessus de 2 enfants
# Nb_adultes_dernier_sejour : Valeur extrêmes au dessus de 3 adultes
# Nb_enfants_dernier_sejour : Valeur extrêmes au dessus de 2 enfants
# Nb_sejours_2020 : Valeur extrêmes au dessus de 2
# Nb_sejours_2021 : Valeur extrêmes au dessus de 2
# Nb_sejours_2022 : Valeur extrêmes au dessus de 2
# Mt_sejours_2020 : Forte dispersion - 71,6 % des valeurs ont O
# Mt_sejours_2021 : Forte dispersion - 58 % des valeurs ont O
# Mt_sejours_2022 : Forte dispersion - 65 % des valeurs ont O
# Duree_sejours_2020 : Forte dispersion - 71 % des valeurs ont 0
# Duree_sejours_2020 : Forte dispersion - 58 % des valeurs ont 0
# Duree_sejours_2022 : Forte dispersion - 65 % des valeurs ont 0
# Nb_participants_sejours_2020 : Forte dispersion, valeurs extremes - 71,6 % des valeurs ont O
# Nb_participants_sejours_2021 : Forte dispersion, valeurs extremes - 58 % des valeurs ont O
# Nb_participants_sejours_2022 : Forte dispersion, valeurs extremes - 65 % des valeurs ont O
# Mt_extras_sejours_2020 : Forte dispersion, valeurs extremes - 86 % des valeurs ont O
# Mt_extras_sejours_2021 : Forte dispersion, valeurs extremes - 79,4 % des valeurs ont O
# Mt_extras_sejours_2022 : Forte dispersion, valeurs extremes - 79,4 des valeurs ont O



# Nettoyage de l'environnement 

rm(var_quanti,plot_quanti,essai)

sum(base_2022$mt_extras_sejours_2020 == 0) / nrow(base_2022)


# Modif sur la variable nb_residences_distinctes

base_2022$nb_residences_classes <- ifelse(base_2022$nb_residences_distinctes == 1, 
                                          "Residence unique", 
                                          "Residence multiples")

# Modif sur la variable duree_sejour_moyen

base_2022$duree_sejour_classes <- cut(base_2022$duree_sejour_moyen, 
                                      breaks = c(0, 6, 7, Inf), 
                                      labels = c("Court", "Moyen", "Long"), 
                                      include.lowest = TRUE)





# Graphique variable qualitative -----

plot_quali <- function(data, variable) {
  var <- as.factor(data[[variable]])
  
    if (length(unique(var)) > 30) {
    warning(paste("La variable", variable, "a trop de catégories pour être affichée correctement."))
    return(NULL)
    }
  
  barplot(table(var), 
          main = paste("Barplot de", variable), 
          xlab = variable, 
          ylab = "Fréquence", 
          col = "lightgreen", 
          border = "black", 
          las = 0.5)
}

var_quali <- sapply(base_2022, function(x) is.factor(x) || is.character(x))
lapply(names(base_2022)[var_quali], function(var) plot_quali(base_2022, var))

# Interpretation des graphiques

# Les deux variables pays ont trop de modalités, on crée des variables continents

base_2022 <- base_2022 %>% 
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
