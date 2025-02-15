# Run Environnement.R pour récupérer les données

source('/Users/rs777/Documents/Projet-datascience/Serie_temp_charbon/scripts/0_environnement.R')


# Graphique pour application -----

# 1 - Prévision Trend + saison

dec = decompose(charbon)
CVS <- charbon-dec$seasonal
plot(CVS)
CVSend <- tail(CVS,n=12)
plot(CVSend)

y = as.vector(CVSend)
x = as.vector(time(CVSend))

reg <- lm(y~x)
summary(reg)
abline(reg$coef)

t <- seq(2023,2024, by = 1/12)[1:12]

season <- as.vector(head(dec$seasonal, n = 12))
z <- (reg$coefficients[1]+reg$coefficients[2]*t)+season

# Tracer les données et la prévision pour 2023
plot(charbon, xlim = c(2001, 2024), main = "", cex.main = 1.2,
     xlab = "Année", ylab = "", lwd = 2)
lines(t,z, col="purple", lwd=2)
legend("topright", legend = c("Données", "Prévision Trend + Saisonnalité"),
       text.col = c("black", "purple"), cex = 1.1)


# 2 - Prévision Holt-Winters (2023)

charbon.hw <- HoltWinters(charbon, alpha = 0.2, beta = 0.2, gamma = 0.2)

predict(charbon.hw, n.ahead = 12)

plot(charbon, xlim = c(2001,2024))
lines(predict(charbon.hw, n.ahead = 13), col = "red", lwd = 2)

# 3 - Prévision Arma

y = diff(charbon,lag=12,differences=1)
x = diff(y,lag=1,differences=1)
plot(y)
plot(x)
mean(x)

library(tseries)
adf.test(charbon, alternative=c("stationary"),12)
adf.test(x, alternative=c("stationary"),12) 

acf(x, lag.max = 24)# q = 1 Q = 1
pacf(x,lag.max = 24)# p soit 1  P = 1 ou 2

z1 <- arima(charbon, order = c(1,1,1), seasonal = list(order = c(1,1,1), period = 12))
z1
tsdiag(z1, gof.lag=25)

zbon <- arima(charbon, order = c(1,1,1), seasonal = list(order = c(1,1,2), period = 12))
zbon
tsdiag(zbon, gof.lag=25)

prev <- predict(zbon, n.ahead = 12)

plot(charbon, main="", cex.main=1.2,
     xlab="année", ylab="", xlim = c(2001, 2024),lwd=2)
lines(prev$pred, col="blue",lwd=2)


