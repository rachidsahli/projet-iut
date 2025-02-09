#_______________________________________________________________________________
# COURS     : Epidemiologie
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
# DONNEES   : Etude ESMAA
#
# PROGRAMME : 3 - ANALYSES MULTIVARIEES
#_______________________________________________________________________________

# Séléction des variables d'intérêts

var_interet <- c("Age", "Sex", "Weight (in kg)", "Height (in cm)",
                 "BMI (in kg/m²)", "Level of education", 
                 "Professional situation", "Smoking status",
                 "Physical", "Medical", "History of disease",
                 "GINA asthma control", "Good observance",
                 "Asthma diagnosis", "SF-8 Mental...25", "SF-8 Physical...24",
                 "Hypertension", "Diabetes", "ACT global", "Exposure")

# Séléction dans le data frame

data_interet <- data %>%
  select(all_of(var_interet))

# Suppression des valeurs manquantes : 628 patients restants

data_interet <- na.omit(data_interet)

hist(data_interet$`ACT global`, 
     main="Distribution de ACT global", 
     xlab="ACT global", 
     col="skyblue", 
     border="black") # Visu rapide

# Modèle de régression complet

fit_complet <- lm(`ACT global` ~ ., data = data_interet)

summary(fit_complet) # Résultat du modèle

# Variables significatives : 
# Age, Level of education, Smoking status, Good observance, SF-8, Diabetes


plot(fit_complet$residuals)
shapiro.test(fit_complet$residuals) # Pas de distribution normale


# Modèle de régression avec interaction

fit_interaction <- lm(`ACT global` ~ Age * `BMI (in kg/m²)` 
                      + `Sex` * `Smoking status`, 
                      data = data_interet)
summary(fit_interaction)

# Test de modèle

anova(fit_complet, fit_interaction)

# L'ajout de l'interaction entre le sex et le statut de fumeur n'a
# pas améliorer le modèle. Il augmente même l'erreur résiduelle.




