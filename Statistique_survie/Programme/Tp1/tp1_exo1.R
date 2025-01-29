# TP1 - Exercice 1

# Importation library

library(ggplot2) # Visualisation graphique
library(dplyr) # Manipulation de données

## Création dataframe

data <- data.frame(
  Time = c(3,5,7,9,18,18,20,12,33,19,20,20,25,26),
  status = c(rep(1,8),0,1,1,0,0,1),  # 1 = événement, 0 = censuré
  grp = c(rep(0,7),rep(1,7)))

## 1

KM_calc <- function(data) {
  # Trie des données selon le temps
  data <- data[order(data$Time), ]
  
  # Initialiser les colonnes pour Kaplan-Meier
  n <- nrow(data)
  data$at_risk <- rev(seq(n))                # Nombre de sujets à risque
  data$event <- data$status                  # Nombre d'événements
  data$survival <- 1 - data$event / data$at_risk  # Probabilité de survie conditionnelle
  data$cum_survival <- cumprod(data$survival)    # Probabilité cumulative
  
  # Retourner les colonnes utiles
  return(data[, c("Time", "at_risk", "event", "cum_survival")])
}

groupe0_data <- data[data$grp == 0, ] # Calcul Kaplan-Meier par groupe
groupe1_data <- data[data$grp == 1, ]

km_groupeA <- KM_calc(groupe0_data)
km_groupeB <- KM_calc(groupe1_data)

km_groupeA$group <- "Groupe A" # Ajout d'une colonne pour identifier le groupe
km_groupeB$group <- "Groupe B"


km_resultats <- rbind(km_groupeA, km_groupeB) # Fusion des résultats
print(km_resultats)

## 2

ggplot(km_resultats, aes(x = Time, y = cum_survival, color = group)) +
  geom_step() +
  geom_point(data = km_resultats[km_resultats$event == 0, ],
             aes(x = Time, y = cum_survival),
             shape = 3, size = 4, color = "black"
  ) + 
  labs(
    title = "Courbes de survie Kaplan-Meier par groupe",
    x = "Temps",
    y = "Probabilité de survie",
    color = ""
  ) +
  theme_classic()

## 3

estimation_mediane <- km_resultats %>%
  group_by(group) %>% 
  filter(cum_survival < 0.5) %>% 
  slice_min(Time) %>% 
  ungroup()

estimation_mediane

estimation_quartile_1 <- km_resultats %>% 
  group_by(group) %>% 
  filter(cum_survival <= 0.25) %>% 
  slice_min(Time) %>% 
  ungroup()

estimation_quartile_1

## 4 

library(survival) # Chargement de la librairie

### 4.1

KM0 <- survfit(Surv(data$Time, data$status)~1) # sans distinction de groupe
summary(KM0,conf.type="Plain")

KM <- survfit(Surv(data$Time, data$status)~data$grp) # avec distinction de groupe
summary(KM,conf.type="Plain")

### 4.2

plot(KM, mark.time=TRUE, col=c(1,2), 
     main = "Courbes de survie de Kaplan-Meier par groupe", 
     cex.main = 0.9,
     xlab = "Temps", 
     ylab = "Porbabilité de survie", 
     lty = 1)
legend("topright", legend = c("Groupe 0", "Groupe 1"), col = c(1, 2), lty = 1)

## 4.3

quantile(KM, probs = 0.25) # Estimation du 1er quartile

quantile(KM, probs = 0.5) # Estimation de la médiane

## 5

### Test du log-rang à la main

#### Étape 1 : Regrouper les données par temps

data <- data[order(data$Time), ]  # Trier par temps
unique_times <- unique(data$Time)  # Temps uniques où les événements se produisent

results <- data.frame(
  time = unique_times,
  events = numeric(length(unique_times)),   # Total des événements à chaque temps
  at_risk_0 = numeric(length(unique_times)),  # Nombre à risque dans le Groupe 0
  events_0 = numeric(length(unique_times)),   # Événements observés dans le Groupe 0
  at_risk_1 = numeric(length(unique_times)),  # Nombre à risque dans le Groupe 1
  events_1 = numeric(length(unique_times))    # Événements observés dans le Groupe 1
) # Initialiser les résultats

#### Étape 2 : Calculer les nombres à risque et événements observés

for (i in seq_along(unique_times)) {
  t <- unique_times[i]
  at_risk <- data[data$Time >= t, ]  # Sujets toujours à risque au temps `t`
  events <- data[data$Time == t & data$status == 1, ]  # Événements au temps `t`
  
  results$events[i] <- nrow(events)  # Total des événements
  results$at_risk_0[i] <- sum(at_risk$grp == 0)  # À risque dans Groupe 0
  results$events_0[i] <- sum(events$grp == 0)    # Événements dans Groupe 0
  results$at_risk_1[i] <- sum(at_risk$grp == 1)  # À risque dans Groupe 1
  results$events_1[i] <- sum(events$grp == 1)    # Événements dans Groupe 1
}

#### Étape 3 : Calculer les contributions attendues et les résidus

results$expected_0 <- results$events * results$at_risk_0 / (results$at_risk_0 + results$at_risk_1)  # Événements attendus Groupe 0
results$expected_1 <- results$events * results$at_risk_1 / (results$at_risk_0 + results$at_risk_1)  # Événements attendus Groupe 1

# Résidus observés - attendus

results$residual_0 <- results$events_0 - results$expected_0
results$residual_1 <- results$events_1 - results$expected_1

# Variance

results$variance_1 <- results$expected_1 * (1 - results$expected_1 / results$events)

#### Étape 4 : Calcul de la statistique du test

O_E <- sum(results$residual_1)  # Somme des (O - E) pour le Groupe 1
V <- sum(results$variance_1, na.rm = TRUE)  # Variance totale

O_E
V

chi_square <- (O_E^2) / V  # Statistique chi-carré

#### Étape 5 : Calcul de la valeur-p
p_value <- 1 - pchisq(chi_square, df = 1)  # Degré de liberté = 1


# Résultats

cat("Statistique du test log-rank : ", chi_square, "\n")
cat("Valeur-p : ", p_value, "\n")

# Afficher les résultats

print(results)