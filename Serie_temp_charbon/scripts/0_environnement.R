# Import library ----

library(dplyr)

# Import data ----

charbon <- read.table("/Users/rs777/Documents/Projet-datascience/Serie_temp_charbon/charbon_usa.txt", col.names = "Charbon mWh")

# Transfomation en série temp ----

charbon <- ts(charbon, frequency = 12, start = c(2001, 1))

# plot(charbon) # Visualisation