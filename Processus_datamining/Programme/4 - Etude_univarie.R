#_______________________________________________________________________________
# COURS     : SCORING
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
# DONNEES   : Agence de voyage
#
# PROGRAMME : 4 - Etude_univariée
#_______________________________________________________________________________

# R?sum? statistique univariée
skim(base_2022)

# Graphique variable quantitative
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


# Graphique variable qualitative
plot_quali <- function(data, variable) {
  var <- as.factor(data[[variable]])
  
    if (length(unique(var)) > 30) {
    warning(paste("La variable", variable, "a trop de cat?gories pour ?tre affich?e correctement."))
    return(NULL)
    }
  
  barplot(table(var), 
          main = paste("Barplot de", variable), 
          xlab = variable, 
          ylab = "Fr?quence", 
          col = "lightgreen", 
          border = "black", 
          las = 0.5)
}

var_quali <- sapply(base_2022, function(x) is.factor(x) || is.character(x))
lapply(names(base_2022)[var_quali], function(var) plot_quali(base_2022, var))
