# Run Environnement.R pour récupérer les données

source('/Users/rs777/Documents/Projet-datascience/Serie_temp_charbon/scripts/0_environnement.R')

# Graphique pour application -----

# Prévision de la série

# 2023

# Trend + saison

dec = decompose(charbon)
CVS <- charbon-dec$seasonal
CVSend <- tail(CVS,n=36)
y = as.vector(CVSend)
x = as.vector(time(CVSend))
reg <- lm(y~x)
t2 <- seq(2023, 2024, by = 1/12)[1:12]
season <- as.vector(head(dec$seasonal, n = 12))
z <- (reg$coefficients[1] + reg$coefficients[2] * t2) + season

# Holt-Winters

charbon.hw <- HoltWinters(charbon)
predict_hw <- predict(charbon.hw, n.ahead = 12)


# ARMA

zbon <- arima(charbon, order = c(1,1,1), seasonal = list(order = c(1,1,2), period = 12))
# zbon
# tsdiag(zbon, gof.lag=25)
predict_arma <- predict(zbon, n.ahead = 12)



Prevision_2023 <- function(charbon) {
  
  dates = time(charbon)
  dates_pred <- seq(max(dates), max(dates) + 12/12, by = 1/12)[1:12]
  
  plot_ly() %>% 
    add_lines(x = dates, y = charbon, mode = 'lines', 
              name = 'Données',
              line = list(color = 'black', width = 1.2)) %>% 
    add_lines(x = dates_pred, y = z, mode = 'lines', 
              name = 'Trend + saison',
              line = list(color = 'orange', width = 2)) %>% 
    add_lines(x = dates_pred, y = predict_hw, mode = 'lines',
              name = 'Holt-Winters',
              line = list(color = 'red', width = 2)) %>% 
    add_lines(x = dates_pred, y = predict_arma$pred, mode = 'lines',
              name = 'ARMA',
              line = list(color = 'purple', width = 2)) %>% 
    layout(
      title = "Prévision de la production d'éléctricité par combustion de charbon en 2023",
      xaxis = list(title = "Année", range = c(2010, 2024)),
      yaxis = list(title = "Charbon (mWh)")
    )
  
}

