
# Run Environnement.R pour récupérer les données

source('/Users/rs777/Documents/Projet-datascience/Serie_temp_charbon/scripts/0_environnement.R')

charbon_2022 <- read.table("/Users/rs777/Documents/Projet-datascience/Serie_temp_charbon/charbon_usa.txt", col.names = "Charbon mWh")
charbon_2022 <- ts(charbon_2022, frequency = 12, start = c(2001, 1), end = c(2022,1))

# TREND + SAISON

dec_2022 = decompose(charbon_2022, type = "additive")
CVS_2022 <- charbon_2022 - dec_2022$seasonal
CVSend_2022 <- tail(CVS_2022, n = 36)
y_2022 = as.vector(CVSend_2022)
x_2022 = as.vector(time(CVSend_2022))
reg_2022 <- lm(y_2022~x_2022)
t_2022 <- seq(2022, 2022 + 11/12, by = 1/12)
season_2022 <- as.vector(head(dec_2022$seasonal, n = 12))
z_2022 <- (reg_2022$coefficients[1] + reg_2022$coefficients[2])+season_2022 # Problème avec le t2

# HOLT-WINTERS

charbon.hw.2022 <- HoltWinters(charbon_2022)
predict_hw_2022 <- predict(charbon.hw.2022, n.ahead = 12)

# ARMA

library(tseries)

zbon_2022 <- suppressWarnings(arima(charbon_2022, order = c(6, 1, 5), seasonal = list(order = c(2, 1, 2), period = 12)))
prev_2022 <- predict(zbon_2022, n.ahead = 12)

# Graphique pour application -----

Prevision_2022 <- function(charbon_2022) {
  
  dates = time(charbon_2022)
  dates_pred <- seq(max(dates), max(dates) + 12/12, by = 1/12)[1:12]
  
  plot_ly() %>% 
    add_lines(x = time(charbon), y = charbon, mode = 'lines', 
              name = 'Données',
              line = list(color = 'black', width = 1.2)) %>% 
    add_lines(x = dates_pred, y = z_2022, mode = 'lines', 
              name = 'Trend + saison',
              line = list(color = 'orange', width = 2)) %>% 
    add_lines(x = dates_pred, y = predict_hw_2022, mode = 'lines',
              name = 'Holt-Winters',
              line = list(color = 'red', width = 2)) %>% 
    add_lines(x = dates_pred, y = prev_2022$pred, mode = 'lines',
              name = 'ARMA',
              line = list(color = 'purple', width = 2)) %>% 
    layout(
      title = "Prévision de la production d'éléctricité par combustion de charbon en 2022",
      xaxis = list(title = "Année", range = c(2021, 2023)),
      yaxis = list(title = "Charbon (mWh)")
    )
  
}



