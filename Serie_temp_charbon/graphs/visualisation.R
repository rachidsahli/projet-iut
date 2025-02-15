# Run Environnement.R pour récupérer les données

source('/Users/rs777/Documents/Projet-datascience/Serie_temp_charbon/scripts/0_environnement.R')


# Graphique pour application -----

# MM
calculateMOVA <- function(charbon, order_MOVA) {
  return(stats::filter(charbon, filter = rep(1/order_MOVA, order_MOVA), sides = 2))
}

# MMC
calculateMOVAC <- function(charbon, order_MOVAC) {
  return(stats::filter(charbon, filter = c(1/(2*order_MOVAC), rep(1/order_MOVAC, 11), 1/(2*order_MOVAC)))) # MMC 12
}

# Courbes de régression des moyennes annuels

xclass <- cut(as.vector(time(charbon)), 22) # 22 intervalles égaux
meananual <- tapply(as.vector(charbon), xclass, mean) # Moyenne de y dans chaque intervalles
t1 <- seq(2001, 2022, length = 22)

# Decomposition pour afficher la tendance

dec <- decompose(charbon, type = "additive")

MOVA_MOVAC <- function(charbon, input) {
  
  charbonMOVA <- calculateMOVA(charbon, input$order_MOVA)
  charbonMOVAC <- calculateMOVAC(charbon, input$order_MOVAC)
  
  dates <- time(charbon)
  
  plot_ly() %>%
    add_lines(x = dates, y = charbon, mode = 'lines', 
              name = 'Données', 
              line = list(color = 'black', width = 1.2)) %>%
    add_lines(x = dates, y = charbonMOVA, mode = 'lines', 
              name = paste0('MM(', input$order_MOVA, ')'), 
              line = list(color = 'cyan', width = 2)) %>%
    add_lines(x = dates, y = charbonMOVAC, mode = 'lines', 
              name = paste0('MMC(', input$order_MOVAC, ')'), 
              line = list(color = 'purple', width = 2)) %>%
    add_lines(x = t1, y = meananual, mode = 'lines', 
              name = 'Régression des moyennes annuels', 
              line = list(color = 'orange', width = 2)) %>%
    add_lines(x = dates, y = dec$trend, mode = 'lines', 
              name = 'Tendance', 
              line = list(color = 'red', width = 2)) %>%
    layout(title = "Production de charbon aux États-Unis",
           xaxis = list(title = "Année"),
           yaxis = list(title = "Production de charbon (mWh)"),
           legend = list(x = 0.8, y = 0.9,
                         bgcolor = "rgba(255, 255, 255, 0.5)",
                         font = list(size = 15)))
}