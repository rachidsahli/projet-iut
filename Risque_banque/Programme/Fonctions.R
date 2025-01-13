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

# Graphique variable qualitative -----

plot_quali <- function(data, variable) {
  var <- as.factor(data[[variable]])
  
  if (length(unique(var)) > 30) {
    warning(paste("La variable", variable, "a trop de catégories pour être affichée correctement."))
    return(NULL)
  }
  
  barplot(table(var), 
          main = paste("Barplot de", variable), xlab = variable, ylab = "Fréquence", col = "lightgreen", 
          border = "black", las = 0.5)
}
  


