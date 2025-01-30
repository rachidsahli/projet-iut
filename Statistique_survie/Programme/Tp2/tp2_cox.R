# TP2 - Exercice

# Rappel : la fonction Surv() attend deux arguments
# 1 : le temps et 2 : l'indicateur de censure          


# Import library

library(survival)

# Import data

churn <- read.csv("Z:/rapide/Churn.csv")

## 1

churn$Churn <- ifelse(churn$Churn == "No", 0, 1) # Ne pas la mettre en facteur

## 2

KM <- survfit(Surv(churn$tenure, churn$Churn) ~ 1) # Estimation de la survie globale

summary(KM, conf.type = "Plain") # Résultat de l'estimation

plot(KM, 
     main = "Estimation de la courbe de survie globale de la durée d'abonnement",
     cex.main = 0.9,
     xlab = "Durée d'abonnement (mois)",
     ylab = "Probabilité de survie",
     conf.int = TRUE,
     mark.time = TRUE)

quantile(KM, probs = 0.25) # Estimation du Q1
# 36 mois -- IC = [32 ; 39]

summary(KM, conf.type = "Plain", times = 48) # Estimation de la survie a 4 ans
# S(t) = 70.9 % -- IC = [69.7 ; 72.1]

## 3

### gender  ----

KM_gender <- survfit(Surv(churn$tenure, churn$Churn) ~ churn$gender) # Estimation de la survie selon le sexe
summary(KM_gender, conf.type = "Plain") # Résultat de l'estimation
plot(KM_gender,
     col = c(1,2),
     main = "Estimation de la courbe de survie de la durée d'abonnement selon le sexe",
     cex.main = 0.9,
     xlab = "Durée d'abonnement (mois)",
     ylab = "Probabilité de survie",
     mark.time = TRUE) # Représentation graphique
legend("bottomleft", legend = levels(as.factor(churn$gender)), lty=1, col=c(1,2))
survdiff(Surv(churn$tenure, churn$Churn) ~ churn$gender) # Test du log rank
quantile(KM_gender) # Estimation des mesures de position

# Interpretation



### partner  ----

KM_partner <- survfit(Surv(churn$tenure, churn$Churn) ~ churn$Partner) # Estimation de la survie selon partner
summary(KM_partner, conf.type = "Plain") # Résultat de l'estimation
plot(KM_partner,
     col = c(1,2),
     main = "Estimation de la courbe de survie de la durée d'abonnement selon le partenariat",
     cex.main = 0.9,
     xlab = "Durée d'abonnement (mois)",
     ylab = "Probabilité de survie",
     mark.time = TRUE) # Représentation graphique
legend("bottomleft", legend = levels(as.factor(churn$Partner)), lty=1, col=c(1,2))
survdiff(Surv(churn$tenure, churn$Churn) ~ churn$Partner) # Test du log rank
quantile(KM_partner) # Estimation des mesures de position

# Interpretation




### dependents  ----

KM_Dependents <- survfit(Surv(churn$tenure, churn$Churn) ~ churn$Dependents) # Estimation de la survie selon dependents
summary(KM_Dependents, conf.type = "Plain") # Résultat de l'estimation
plot(KM_Dependents,
     col = c(1,2),
     main = "Estimation de la courbe de survie de la durée d'abonnement en fonction de la présence de personnes à charge",
     cex.main = 0.9,
     xlab = "Durée d'abonnement (mois)",
     ylab = "Probabilité de survie",
     mark.time = TRUE) # Représentation graphique
legend("bottomleft", legend = levels(as.factor(churn$Dependents)), lty=1, col=c(1,2))
survdiff(Surv(churn$tenure, churn$Churn) ~ churn$Dependents) # Test du log rank
quantile(KM_Dependents) # Estimation des mesures de position

# Interpretation



### phoneservice  ----

KM_PhoneService <- survfit(Surv(churn$tenure, churn$Churn) ~ churn$PhoneService) # Estimation de la survie selon phoneservice
summary(KM_PhoneService, conf.type = "Plain") # Résultat de l'estimation
plot(KM_PhoneService,
     col = c(1,2),
     main = "Estimation de la courbe de survie de la durée d'abonnement selon ",
     cex.main = 0.9,
     xlab = "Durée d'abonnement (mois)",
     ylab = "Probabilité de survie",
     mark.time = TRUE) # Représentation graphique
legend("bottomleft", legend = levels(as.factor(churn$PhoneService)), lty=1, col=c(1,2))
survdiff(Surv(churn$tenure, churn$Churn) ~ churn$PhoneService) # Test du log rank
quantile(KM_PhoneService) # Estimation des mesures de position

# Interpretation




### multiplelines  ----

KM_MultipleLines <- survfit(Surv(churn$tenure, churn$Churn) ~ churn$MultipleLines) # Estimation de la survie selon 
summary(KM_MultipleLines, conf.type = "Plain") # Résultat de l'estimation
plot(KM_MultipleLines,
     col = c(1,2,3),
     main = "Estimation de la courbe de survie de la durée d'abonnement selon ",
     cex.main = 0.9,
     xlab = "Durée d'abonnement (mois)",
     ylab = "Probabilité de survie",
     mark.time = TRUE) # Représentation graphique
legend("bottomleft", legend = levels(as.factor(churn$MultipleLines)), lty=1, col=c(1,2,3))
survdiff(Surv(churn$tenure, churn$Churn) ~ churn$MultipleLines) # Test du log rank
quantile(KM_MultipleLines) # Estimation des mesures de position

# Interpretation




### internetservice  ----

KM_InternetService <- survfit(Surv(churn$tenure, churn$Churn) ~ churn$InternetService) # Estimation de la survie selon 
summary(KM_InternetService, conf.type = "Plain") # Résultat de l'estimation
plot(KM_InternetService,
     col = c(1,2,3),
     main = "Estimation de la courbe de survie de la durée d'abonnement selon ",
     cex.main = 0.9,
     xlab = "Durée d'abonnement (mois)",
     ylab = "Probabilité de survie",
     mark.time = TRUE) # Représentation graphique
legend("bottomleft", legend = levels(as.factor(churn$InternetService)), lty=1, col=c(1,2,3))
survdiff(Surv(churn$tenure, churn$Churn) ~ churn$InternetService) # Test du log rank
quantile(KM_MultipleLines) # Estimation des mesures de position

# Interpretation



### OnlineSecurity  ----

KM_OnlineSecurity <- survfit(Surv(churn$tenure, churn$Churn) ~ churn$OnlineSecurity) # Estimation de la survie selon 
summary(KM_OnlineSecurity, conf.type = "Plain") # Résultat de l'estimation
plot(KM_OnlineSecurity,
     col = c(1,2,3),
     main = "Estimation de la courbe de survie de la durée d'abonnement selon ",
     cex.main = 0.9,
     xlab = "Durée d'abonnement (mois)",
     ylab = "Probabilité de survie",
     mark.time = TRUE) # Représentation graphique
legend("bottomleft", legend = levels(as.factor(churn$OnlineSecurity)), lty=1, col=c(1,2,3))
survdiff(Surv(churn$tenure, churn$Churn) ~ churn$OnlineSecurity) # Test du log rank
quantile(KM_OnlineSecurity) # Estimation des mesures de position

# Interpretation



### OnlineBackup  ----

KM_OnlineBackup <- survfit(Surv(churn$tenure, churn$Churn) ~ churn$OnlineBackup) # Estimation de la survie selon 
summary(KM_OnlineBackup, conf.type = "Plain") # Résultat de l'estimation
plot(KM_OnlineBackup,
     col = c(1,2,3),
     main = "Estimation de la courbe de survie de la durée d'abonnement selon ",
     cex.main = 0.9,
     xlab = "Durée d'abonnement (mois)",
     ylab = "Probabilité de survie",
     mark.time = TRUE) # Représentation graphique
legend("bottomleft", legend = levels(as.factor(churn$OnlineBackup)), lty=1, col=c(1,2,3))
survdiff(Surv(churn$tenure, churn$Churn) ~ churn$OnlineBackup) # Test du log rank
quantile(KM_OnlineBackup) # Estimation des mesures de position

# Interpretation



### DeviceProtection  ----

KM_DeviceProtection <- survfit(Surv(churn$tenure, churn$Churn) ~ churn$DeviceProtection) # Estimation de la survie selon 
summary(KM_DeviceProtection, conf.type = "Plain") # Résultat de l'estimation
plot(KM_DeviceProtection,
     col = c(1,2,3),
     main = "Estimation de la courbe de survie de la durée d'abonnement selon ",
     cex.main = 0.9,
     xlab = "Durée d'abonnement (mois)",
     ylab = "Probabilité de survie",
     mark.time = TRUE) # Représentation graphique
legend("bottomleft", legend = levels(as.factor(churn$DeviceProtection)), lty=1, col=c(1,2,3))
survdiff(Surv(churn$tenure, churn$Churn) ~ churn$DeviceProtection) # Test du log rank
quantile(KM_DeviceProtection) # Estimation des mesures de position

# Interpretation



### TechSupport  ----

KM_TechSupport <- survfit(Surv(churn$tenure, churn$Churn) ~ churn$TechSupport) # Estimation de la survie selon 
summary(KM_TechSupport, conf.type = "Plain") # Résultat de l'estimation
plot(KM_TechSupport,
     col = c(1,2,3),
     main = "Estimation de la courbe de survie de la durée d'abonnement selon ",
     cex.main = 0.9,
     xlab = "Durée d'abonnement (mois)",
     ylab = "Probabilité de survie",
     mark.time = TRUE) # Représentation graphique
legend("bottomleft", legend = levels(as.factor(churn$TechSupport)), lty=1, col=c(1,2,3))
survdiff(Surv(churn$tenure, churn$Churn) ~ churn$TechSupport) # Test du log rank
quantile(KM_TechSupport) # Estimation des mesures de position

# Interpretation


### StreamingTV  ----

KM_StreamingTV <- survfit(Surv(churn$tenure, churn$Churn) ~ churn$StreamingTV) # Estimation de la survie selon 
summary(KM_StreamingTV, conf.type = "Plain") # Résultat de l'estimation
plot(KM_StreamingTV,
     col = c(1,2,3),
     main = "Estimation de la courbe de survie de la durée d'abonnement selon ",
     cex.main = 0.9,
     xlab = "Durée d'abonnement (mois)",
     ylab = "Probabilité de survie",
     mark.time = TRUE) # Représentation graphique
legend("bottomleft", legend = levels(as.factor(churn$StreamingTV)), lty=1, col=c(1,2,3))
survdiff(Surv(churn$tenure, churn$Churn) ~ churn$StreamingTV) # Test du log rank
quantile(KM_StreamingTV) # Estimation des mesures de position

# Interpretation




### StreamingMovies  ----

KM_StreamingMovies <- survfit(Surv(churn$tenure, churn$Churn) ~ churn$StreamingMovies) # Estimation de la survie selon 
summary(KM_StreamingMovies, conf.type = "Plain") # Résultat de l'estimation
plot(KM_StreamingMovies,
     col = c(1,2,3),
     main = "Estimation de la courbe de survie de la durée d'abonnement selon ",
     cex.main = 0.9,
     xlab = "Durée d'abonnement (mois)",
     ylab = "Probabilité de survie",
     mark.time = TRUE) # Représentation graphique
legend("bottomleft", legend = levels(as.factor(churn$StreamingMovies)), lty=1, col=c(1,2,3))
survdiff(Surv(churn$tenure, churn$Churn) ~ churn$StreamingMovies) # Test du log rank
quantile(KM_StreamingMovies) # Estimation des mesures de position

# Interpretation




### Contract  ----

KM_Contracts <- survfit(Surv(churn$tenure, churn$Churn) ~ churn$Contract) # Estimation de la survie selon 
summary(KM_Contracts, conf.type = "Plain") # Résultat de l'estimation
plot(KM_Contracts,
     col = c(1,2,3),
     main = "Estimation de la courbe de survie de la durée d'abonnement selon ",
     cex.main = 0.9,
     xlab = "Durée d'abonnement (mois)",
     ylab = "Probabilité de survie",
     mark.time = TRUE) # Représentation graphique
legend("bottomleft", legend = levels(as.factor(churn$Contract)), lty=1, col=c(1,2,3))
survdiff(Surv(churn$tenure, churn$Churn) ~ churn$Contract) # Test du log rank
quantile(KM_Contracts) # Estimation des mesures de position

# Interpretation


### PaperlessBilling  ----

KM_PaperlessBilling <- survfit(Surv(churn$tenure, churn$Churn) ~ churn$PaperlessBilling) # Estimation de la survie selon 
summary(KM_PaperlessBilling, conf.type = "Plain") # Résultat de l'estimation
plot(KM_PaperlessBilling,
     col = c(1,2,3),
     main = "Estimation de la courbe de survie de la durée d'abonnement selon ",
     cex.main = 0.9,
     xlab = "Durée d'abonnement (mois)",
     ylab = "Probabilité de survie",
     mark.time = TRUE) # Représentation graphique
legend("bottomleft", legend = levels(as.factor(churn$PaperlessBilling)), lty=1, col=c(1,2,3))
survdiff(Surv(churn$tenure, churn$Churn) ~ churn$PaperlessBilling) # Test du log rank
quantile(KM_PaperlessBilling) # Estimation des mesures de position

# Interpretation




### PaymentMethod  ----

KM_PaymentMethod <- survfit(Surv(churn$tenure, churn$Churn) ~ churn$PaymentMethod) # Estimation de la survie selon 
summary(KM_PaymentMethod, conf.type = "Plain") # Résultat de l'estimation
plot(KM_PaymentMethod,
     col = c(1,2,3),
     main = "Estimation de la courbe de survie de la durée d'abonnement selon ",
     cex.main = 0.9,
     xlab = "Durée d'abonnement (mois)",
     ylab = "Probabilité de survie",
     mark.time = TRUE) # Représentation graphique
legend("bottomleft", legend = levels(as.factor(churn$PaymentMethod)), lty=1, col=c(1,2,3))
survdiff(Surv(churn$tenure, churn$Churn) ~ churn$PaymentMethod) # Test du log rank
quantile(KM_PaymentMethod) # Estimation des mesures de position

# Interpretation




## 4 ----

churn$InternetService <- relevel(factor(churn$InternetService), ref = "No") # 1ere modalite par defaut

fit = coxph(Surv(churn$tenure, churn$Churn) ~ factor(churn$InternetService))
summary(fit)

# Le rapport de risque (Hazard ratio (HR)), pour DSL par rapport a pas d'abonnement : exp(coef) = 2.44
# L'abonnement DSL présente un risque relatif (HR) de 2.44 par rapport à l'absence d'abonnement.
# Interpretation : Une personne ayant un abonnement DSL a un risque d'annulation d'abonnement d'environ
# 2.44 fois plus élevé que quelqu'un qui n'a pas d'abonnement

# Le rapport de risque (Hazard ratio (HR)), pour Fiber optic par rapport a pas d'abonnement : exp(coef) = 5.4
# L'abonnement Fiber optic présente un risque relatif (HR) de 5.4 par rapport à l'absence d'abonnement.
# Interpretation : Une personne ayant un abonnement Fiber optic a un risque d'annulation d'abonnement d'environ
# 5.4 fois plus élevé que quelqu'un qui n'a pas d'abonnement

# La variable InternetService un effet significatif sur la durée de souscription.
# Cela car les p-valeurs sont hautement significatives

## 5

fit2 = coxph(Surv(churn$tenure, churn$Churn) ~ factor(churn$InternetService) + factor(churn$Contract))
summary(fit2)

addmargins(table(churn$InternetService, churn$Contract)) # Realtion entre les 2 variables

# Les p-valeurs de DSL et fiber optic sont toujours significatives
# Leur HR : 1.33 (33.76 %) et 1.80 (80 %) par rapport à l'absence d'abonnement internet

# Les p-valeurs de One year et Two year de la variable contract sont aussi significatives
# Leur coefficients sont négatifs. Leur impact sur la résiliation d'abonnement va être négatif
# Un contrat one year réduit le risque de résiliation d'environ 97.76 % par rapport à un contrat Mone-to-month
# un contrat two year réduit le risque de résil d'environ 98.2 %  par rapport à un contrat Mone-to-Month

# Comparaison de fit et fit2 : Avant l'ajout de la var contrat les HR de DSL et Fiber optic
# étaient plus élevé.
# Donc l'ajout de cette variable diminue l'impact des abonnements internet sur la durée d'abonnement.

# Realtion entre les 2 var :Les abonnements a la fibre optique sont majoritairement associé à des contrats mensuels
# La relation entre ces deux variables peut être assez forte
# Le fait que le type de contrat ait un impact sur la résiliation peut rendre l'impact de InternetService
# moins significatif

## 6 
