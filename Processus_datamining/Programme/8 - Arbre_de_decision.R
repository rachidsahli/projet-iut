#_______________________________________________________________________________
# COURS     : SCORING
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
# DONNEES   : Agence de voyage
#
# PROGRAMME : 8 - Arbre de décision
#_______________________________________________________________________________

# Arbre maximal

## Calcul du modèle

formule_modele_complet <- as.formula(paste("flag_reachat ~ ",
                                           paste(apprentissage %>%
                                                   select(- id_client, - flag_reachat) %>% 
                                                   colnames(),
                                                 collapse = " + ")))

formule <- as.formula(paste("flag_reachat ~ ", paste(liste_var, collapse = " + ")))

arbre_max <- rpart(formula = formule,
                   data = apprentissage,
                   method = "class",
                   cp = -Inf)


## Représentations de l'arbre

arbre_max <- rpart(flag_reachat ~ ., data = base_2022, 
                   method = "class", 
                   control = rpart.control(maxdepth = 5, minsplit = 20))


rpart.plot(arbre_max)
prp(arbre_max, maxdepth = 3) 

# Élagage de l'arbre

## Représentation du taux de mal classés

plotcp(arbre_max)




## Sélection de la constante avec le taux d'erreur minimum

printcp(arbre_max)


## Construction du sous-arbre optimal en utilisant la constante avec le taux d'erreur minimum

cp_min <- arbre_max$cptable %>% 
  as.data.frame() %>% 
  arrange(xerror) %>% 
  slice(1) %>% 
  select(CP) %>% 
  pull()

arbre <- rpart(formula = formule,
               data = apprentissage,
               method = "class",
               cp = cp_min)

## Visualisation de l'arbre

rpart.plot(x = arbre, cex = 0.7)

## Importance des variables

importance <- data.frame(arbre$variable.importance) %>% 
  rownames_to_column(var = "variable") %>% 
  rename(importance = arbre.variable.importance)

ggplot(data = importance) +
  aes(x = importance, y = fct_reorder(variable, importance)) +
  geom_col(fill = "steelblue4") +
  labs(title = "Importance des variables", x = NULL, y = NULL) +
  theme_minimal()

## Calcul des probabilités estimées

test_arbre <- test %>% 
  mutate(p_reachat = predict(object = arbre,
                                 newdata = test,
                                 type = "prob")[, 2]) %>% 
  select(id_client, flag_reachat, p_reachat)



## Distribution des probabilités estimées

ggplot(test_arbre) +
  aes(x = p_reachat) +
  geom_density()




## Performance du modèle (0.6632)

print(roc(test_arbre$flag_reachat, test_arbre$p_reachat)$auc)
