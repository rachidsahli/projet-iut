# Import data ----

charbon_2022 <- read.table("/Users/rs777/Documents/Projet-datascience/Serie_temp_charbon/charbon_usa.txt", col.names = "Charbon mWh")

# Transfomation en série temp ----

charbon_2022 <- ts(charbon_2022, frequency = 12, start = c(2001, 1), end = c(2022,0))

# TREND + SAISON

dec_2022 = decompose(charbon_2022, type = "additive")

## CVS 

CVS_2022 <- charbon_2022 - dec_2022$seasonal
plot(CVS_2022)

CVSend_2022 <- tail(CVS_2022, n = 36)

y_2022 = as.vector(CVSend_2022)
x_2022 = as.vector(time(CVSend_2022))

## Régression

reg_2022 <- lm(y_2022~x_2022)
summary(reg_2022)

t_2022 <- seq(2022, 2022 + 11/12, by = 1/12)

season_2022 <- as.vector(head(dec_2022$seasonal, n = 12))

z_2022 <- (reg_2022$coefficients[1] + reg_2022$coefficients[2] * t_2022) + season_2022

plot(charbon_2022, xlim = c(2020, 2023))
lines(t_2022, z_2022, col="purple", lwd=2)
lines(charbon, col="cyan",lwd=3)


# HOLT-WINTERS

charbon.hw.2022 <- HoltWinters(charbon_2022)
predict_hw_2022 <- predict(charbon.hw.2022, n.ahead = 12)
lines(predict(charbon.hw.2022, n.ahead = 13), col = "red", lwd = 2)

# ARMA

library(tseries)

y_2022_arma = diff(charbon_2022, lag=12, differences = 1)
x_2022_arma = diff(y_2022_arma, lag=1, differences = 1)
plot(x_2022_arma, xlab="Année", ylab = "MGW/H", main  = "Représentation de la série stationnaire")

adf.test(charbon_2022, alternative = c("stationary"), 12)
adf.test(x_2022_arma, alternative = c("stationary"), 12)

zbon_2022 <- arima(charbon_2022, order = c(6,1,5), seasonal = list(order = c(2,1,2), period = 12))

prev_2022 <- predict(zbon_2022, n.ahead=13)

lines(prev_2022$pred, col="blue", lwd = 2)