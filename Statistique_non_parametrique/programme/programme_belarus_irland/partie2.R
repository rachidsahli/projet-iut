# Environnement ----
setwd("/Users/rs777/Documents/Statistique-non-parametrique")

source("programme/programme_belarus_irland/preliminaire.R")

# Partie 2 : Estimation de la fonction de régression de la taille en fonction de l'Age

# 1 -----

### Belarus

plot(Belarus$Age, Belarus$Taille,
     xlab = "Âge",
     ylab = "Taille",   
     main = "Taille en fonction de l'âge - Belarus",
     cex.main = 0.9,
     col = "blue")

newt_belarus <- cut(Belarus$Age, breaks = 25) 
meany <- tapply(Belarus$Taille, newt_belarus, mean) 
xseq1 <- seq(min(Belarus$Age), max(Belarus$Age), length.out = 26) 
for (i in 1:25) {
  lines(c(xseq1[i], xseq1[i+1]), c(meany[i], meany[i]), lwd = 2, col = "black")
}
for (i in 2:25) {
  lines(c(xseq1[i], xseq1[i]), c(meany[i-1], meany[i]), lwd = 2, col = "black")
}
kernel_estimate <- ksmooth(Belarus$Age, Belarus$Taille, kernel = "normal", bandwidth = 1)
lines(kernel_estimate$x, kernel_estimate$y, col = "red", lwd = 2)
legend("topleft", legend = c("Courbe de régression", "Régressogramme"), 
       text.col = c("red", "black"),
       col = c("red", "black"),
       lwd = 2)

### Irland

plot(Ireland$Age, Ireland$Taille,
     xlab = "Âge",
     ylab = "Taille",   
     main = "Taille en fonction de l'âge - Irland",
     cex.main = 0.9,
     col = "darkgreen")

newt_ireland <- cut(Ireland$Age, breaks = 25)
meany_ireland <- tapply(Ireland$Taille, newt_ireland, mean) 
xseq1_ireland <- seq(min(Ireland$Age), max(Ireland$Age), length.out = 26)
for (i in 1:25) {
  lines(c(xseq1_ireland[i], xseq1_ireland[i+1]), c(meany_ireland[i], meany_ireland[i]), lwd = 2, col = "black")
}
for (i in 2:25) {
  lines(c(xseq1_ireland[i], xseq1_ireland[i]), c(meany_ireland[i-1], meany_ireland[i]), lwd = 2, col = "black")
}
kernel_estimate_ireland <- ksmooth(Ireland$Age, Ireland$Taille, kernel = "normal", bandwidth = 1)
lines(kernel_estimate_ireland$x, kernel_estimate_ireland$y, col = "red", lwd = 2)
legend("topleft", legend = c("Courbe de régression", "Régressogramme"), 
       text.col = c("red", "black"),
       col = c("red", "black"),
       lwd = 2)

# 2 -----

plot(Belarus$Age, Belarus$Taille, 
     pch = ".", 
     xlab = "Âge", 
     ylab = "Taille",   
     main = "Courbes de régression - Belarus and Irland", 
     xlim = c(min(Belarus$Age, Ireland$Age), max(Belarus$Age, Ireland$Age)), 
     ylim = c(min(Belarus$Taille, Ireland$Taille), max(Belarus$Taille, Ireland$Taille)),
     col = "white")
mtext("Estimation par noyau", side = 3, line = 0.5, cex = 0.9)
lines(kernel_estimate$x, kernel_estimate$y, col = "blue", lwd = 2)
lines(kernel_estimate_ireland$x, kernel_estimate_ireland$y, col = "red", lwd = 2)
legend("topleft", legend = c("Belarus", "Irland"), 
       text.col = c("blue", "red"),
       col = c("blue", "red"),
       lwd = 2)



# 3 -----

## Belarus

kernel_estimate_bel_f <- ksmooth(Belarus_f$Age, Belarus_f$Taille, 
                                 kernel = "normal", bandwidth = 1) # Fille
kernel_estimate_bel_g <- ksmooth(Belarus_h$Age, Belarus_h$Taille, 
                                 kernel = "normal", bandwidth = 1) # Garçons

plot(Belarus_f$Age, Belarus_f$Taille, 
     pch = ".", 
     xlab = "Âge", 
     ylab = "Taille",   
     main = "Comparaison des courbes de régression - Belarus", 
     xlim = c(min(Belarus_f$Age, Belarus_h$Age), max(Belarus_f$Age, Belarus_h$Age)), 
     ylim = c(min(Belarus_f$Taille, Belarus_h$Taille), max(Belarus_f$Taille, Belarus_h$Taille)),
     col = "white")
mtext("Estimation par noyau", side = 3, line = 0.5, cex = 0.9)
lines(kernel_estimate_bel_f$x, kernel_estimate_bel_f$y, col = "magenta", lwd = 2)
lines(kernel_estimate_bel_g$x, kernel_estimate_bel_g$y, col = "orange", lwd = 2)
legend("topleft", 
       legend = c("Garçon", "Fille"),
       col = c("orange", "magenta"), 
       lwd = 2)

## Irland

kernel_estimate_ireland_f <- ksmooth(Ireland_f$Age, Ireland_f$Taille, kernel = "normal", bandwidth = 1)
kernel_estimate_ireland_g <- ksmooth(Ireland_h$Age, Ireland_h$Taille, kernel = "normal", bandwidth = 1)

plot(Ireland_f$Age, Ireland_f$Taille, 
     pch = ".", 
     xlab = "Âge", 
     ylab = "Taille",   
     main = "Comparaison des courbes de régression - Irland", 
     xlim = c(min(Ireland_f$Age, Ireland_h$Age), max(Ireland_f$Age, Ireland_h$Age)), 
     ylim = c(min(Ireland_f$Taille, Ireland_h$Taille), max(Ireland_f$Taille, Ireland_h$Taille)),
     col = "white")
mtext("Estimation par noyau", side = 3, line = 0.5, cex = 0.9)
lines(kernel_estimate_ireland_f$x, kernel_estimate_ireland_f$y, col = "violet", lwd = 2)
lines(kernel_estimate_ireland_g$x, kernel_estimate_ireland_g$y, col = "black", lwd = 2)
legend("topleft", 
       legend = c("Garçon", "Fille"),
       col = c("black", "violet"), 
       lwd = 2)



# 4 -----

## Belarus

### Ajustement polynomial de degré 6

#### Fille

poly6_belarus_f <- lm(Belarus_f$Taille ~ Belarus_f$Age + 
                        I(Belarus_f$Age^2) + I(Belarus_f$Age^3) + 
                        I(Belarus_f$Age^4) + I(Belarus_f$Age^5) + 
                        I(Belarus_f$Age^6))

grille_belarus_f <- seq(min(Belarus_f$Age), max(Belarus_f$Age), by = 0.001)

pred6_belarus_f <- poly6_belarus_f$coefficients[1] + 
  poly6_belarus_f$coefficients[2]*grille_belarus_f +
  poly6_belarus_f$coefficients[3]*(grille_belarus_f)^2 + 
  poly6_belarus_f$coefficients[4]*(grille_belarus_f)^3 +
  poly6_belarus_f$coefficients[5]*(grille_belarus_f)^4 + 
  poly6_belarus_f$coefficients[6]*(grille_belarus_f)^5 + 
  poly6_belarus_f$coefficients[7]*(grille_belarus_f)^6

#### Garçons

poly6_belarus_g <- lm(Belarus_h$Taille ~ Belarus_h$Age + 
                        I(Belarus_h$Age^2) + I(Belarus_h$Age^3) + 
                        I(Belarus_h$Age^4) + I(Belarus_h$Age^5) + 
                        I(Belarus_h$Age^6))

grille_belarus_g <- seq(min(Belarus_h$Age), max(Belarus_h$Age), by = 0.001)

pred6_belarus_g <- poly6_belarus_g$coefficients[1] + 
  poly6_belarus_g$coefficients[2]*grille_belarus_g +
  poly6_belarus_g$coefficients[3]*(grille_belarus_g)^2 + 
  poly6_belarus_g$coefficients[4]*(grille_belarus_g)^3 +
  poly6_belarus_g$coefficients[5]*(grille_belarus_g)^4 + 
  poly6_belarus_g$coefficients[6]*(grille_belarus_g)^5 + 
  poly6_belarus_g$coefficients[7]*(grille_belarus_g)^6

plot(Belarus_f$Age, Belarus_f$Taille, 
     pch = ".", 
     xlab = "Âge", 
     ylab = "Taille",   
     main = "Comparaison des courbes de régression - Belarus", 
     xlim = c(min(Belarus_f$Age, Belarus_h$Age), max(Belarus_f$Age, Belarus_h$Age)), 
     ylim = c(min(Belarus_f$Taille, Belarus_h$Taille), max(Belarus_f$Taille, Belarus_h$Taille)),
     col = "white")
mtext("Estimation par noyau", side = 3, line = 0.5, cex = 0.9)
lines(kernel_estimate_bel_f$x, kernel_estimate_bel_f$y, col = "magenta", lwd = 2)
lines(kernel_estimate_bel_g$x, kernel_estimate_bel_g$y, col = "orange", lwd = 2)
lines(grille_belarus_f, pred6_belarus_f, col = "red", lwd = 1)
lines(grille_belarus_g, pred6_belarus_g, col = "cyan", lwd = 1)
legend("topleft", 
       legend = c("Garçon régression", "Fille régression", 
                  "Fille poly 6", "Garçon poly 6"),
       col = c("orange", "magenta", "red", "cyan"), 
       lwd = 2)

## Irland

### Ajustement polynomial de degré 6

#### Fille

poly6_irland_f <- lm(Ireland_f$Taille ~ Ireland_f$Age + 
                       I(Ireland_f$Age^2) + I(Ireland_f$Age^3) + 
                       I(Ireland_f$Age^4) + I(Ireland_f$Age^5) + 
                       I(Ireland_f$Age^6))

grille_irland_f <- seq(min(Ireland_f$Age), max(Ireland_f$Age), by = 0.001)

pred6_irland_f <- poly6_irland_f$coefficients[1] + 
  poly6_irland_f$coefficients[2]*grille_irland_f +
  poly6_irland_f$coefficients[3]*(grille_irland_f)^2 + 
  poly6_irland_f$coefficients[4]*(grille_irland_f)^3 +
  poly6_irland_f$coefficients[5]*(grille_irland_f)^4 + 
  poly6_irland_f$coefficients[6]*(grille_irland_f)^5 + 
  poly6_irland_f$coefficients[7]*(grille_irland_f)^6

#### Garçons

poly6_irland_g <- lm(Ireland_h$Taille ~ Ireland_h$Age + 
                       I(Ireland_h$Age^2) + I(Ireland_h$Age^3) + 
                       I(Ireland_h$Age^4) + I(Ireland_h$Age^5) + 
                       I(Ireland_h$Age^6))

grille_irland_g <- seq(min(Ireland_h$Age), max(Ireland_h$Age), by = 0.001)

pred6_irland_g <- poly6_irland_g$coefficients[1] + 
  poly6_irland_g$coefficients[2]*grille_irland_g +
  poly6_irland_g$coefficients[3]*(grille_irland_g)^2 + 
  poly6_irland_g$coefficients[4]*(grille_irland_g)^3 +
  poly6_irland_g$coefficients[5]*(grille_irland_g)^4 + 
  poly6_irland_g$coefficients[6]*(grille_irland_g)^5 + 
  poly6_irland_g$coefficients[7]*(grille_irland_g)^6

plot(Ireland_f$Age, Ireland_f$Taille, 
     pch = ".", 
     xlab = "Âge", 
     ylab = "Taille",   
     main = "Comparaison des courbes de régression - Irland", 
     xlim = c(min(Ireland_f$Age, Ireland_h$Age), max(Ireland_f$Age, Ireland_h$Age)), 
     ylim = c(min(Ireland_f$Taille, Ireland_h$Taille), max(Ireland_f$Taille, Ireland_h$Taille)),
     col = "white")
mtext("Estimation par noyau", side = 3, line = 0.5, cex = 0.9)
lines(kernel_estimate_ireland_f$x, kernel_estimate_ireland_f$y, col = "violet", lwd = 2)
lines(kernel_estimate_ireland_g$x, kernel_estimate_ireland_g$y, col = "black", lwd = 2)
lines(grille_irland_f, pred6_irland_f, col = "red", lwd = 1)
lines(grille_irland_g, pred6_irland_g, col = "cyan", lwd = 1)
legend("topleft", 
       legend = c("Garçon régression", "Fille régression", 
                  "Fille poly 6", "Garçon poly 6"),
       col = c("black", "violet", "red", "cyan"), 
       lwd = 2)



# 5 -----

## Belarus

ks.test(Belarus$Age,Belarus$Taille)
cor.test(Belarus$Age,Belarus$Taille)

## Irland

ks.test(Ireland$Age,Ireland$Taille)
cor.test(Ireland$Age,Ireland$Taille)



# 6 -----







