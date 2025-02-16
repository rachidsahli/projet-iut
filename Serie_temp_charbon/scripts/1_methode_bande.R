# Run environnement pour récupérer les données 

source('scripts/0_environnement.R')

# Méthode de la bande ----
# Calcul sur 12 mois

minima <- sapply(1:(length(charbon)-11), function(i) min(charbon[i:(i+11)]))  # Minima sur 12 mois
maxima <- sapply(1:(length(charbon)-11), function(i) max(charbon[i:(i+11)]))  # Maxima sur 12 mois
minima <- c(rep(NA, 11), minima)  # Ajouter NA pour le début de la série
maxima <- c(rep(NA, 11), maxima)  # Ajouter NA pour le début de la série
minima_ts <- ts(minima, start = start(charbon), frequency = frequency(charbon))
maxima_ts <- ts(maxima, start = start(charbon), frequency = frequency(charbon))
plot(charbon, type = "l", col = "black", main = "Méthode de la bande : Série additive vs Multiplicative", 
     ylab = "Charbon (mWh)", xlab = "Année")
lines(minima_ts, col = "red", lwd = 2)
lines(maxima_ts, col = "red", lwd = 2)

# Avec le package zoo ----

library(zoo)

minima <- rollapply(charbon, width = 12, FUN = min, align = "center", fill = NA)
maxima <- rollapply(charbon, width = 12, FUN = max, align = "center", fill = NA)

# Tracer les courbes
plot(charbon, type = "l", col = "black", main = "Méthode de la bande : Série additive vs Multiplicative", 
     ylab = "Charbon (mWh)", xlab = "Année")
lines(minima, col = "red", lwd = 2)
lines(maxima, col = "red", lwd = 2)