# Run Environnement.R pour récupérer les données

source('scripts/0_environnement.R')


# Graphique pour application -----

# Décomposition de la série

Decomposition <- function(charbon) {
  
  dec <- decompose(charbon, type = "additive")
  
  time_series <- time(charbon)
  
  subplot(
    plot_ly(x = time_series, y = charbon, type = "scatter", 
            mode = "lines", name = "Données", 
            line = list(color = "black")) %>%
      layout(yaxis = list(title = "Données", zeroline = FALSE)),
    
    plot_ly(x = time_series, y = dec$trend, type = "scatter", 
            mode = "lines", name = "Trend") %>%
      layout(yaxis = list(title = "Trend", zeroline = FALSE)),
    
    plot_ly(x = time_series, y = dec$seasonal, type = "scatter", 
            mode = "lines", name = "Saisonnalité") %>%
      layout(yaxis = list(title = "Saisonnalité", zeroline = FALSE)),
    
    plot_ly(x = time_series, y = dec$random, type = "scatter", 
            mode = "lines", name = "Résidus") %>%
      layout(yaxis = list(title = "Résidus", zeroline = FALSE)),
    
    nrows = 4, shareX = TRUE, heights = c(0.3, 0.25, 0.25, 0.2), margin = 0.05
  ) %>%
    layout(title = "Composantes de la production de charbon aux États-Unis (2001-2022)",
           xaxis = list(title = "Année"),
           legend = list(font = list(color = 'black', size = 15)))
}


Lissage <- function(charbon) {
  
  dec <- decompose(charbon, type = "additive")
  
  time_series <- time(charbon)
  
  plot_ly() %>%
    add_lines(x = time_series, y = charbon, name = "Données", 
              line = list(color = "black")) %>%
    add_lines(x = time_series, y = dec$trend + dec$seasonal, 
              name = "Série lissé des prédictions", 
              line = list(color = "red")) %>%
    add_lines(x = time_series, y = dec$trend + dec$random, 
              name = "Série corrigée des variations saisonnières", 
              line = list(color = "green")) %>% 
    layout(
      title = "Comparaison entre tendance, lissage des prédictions et corrections des variations saisonnières",
      xaxis = list(title = "Année"),
      yaxis = list(title = "Production de charbon (mWh)"),
      legend = list(
        x = 0.8, y = 0.9,
        font = list(color = 'black', size = 15)
      )
    )
}
