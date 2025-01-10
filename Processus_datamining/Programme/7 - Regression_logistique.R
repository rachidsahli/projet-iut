#_______________________________________________________________________________
# COURS     : SCORING
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
# DONNEES   : Agence de voyage
#
# PROGRAMME : 7 - Regression logistique
#_______________________________________________________________________________

liste_var <- c("flag_est_parrain","flag_est_filleul","saison_premier_sejour",
               "type_destination_dernier_sejour","nb_adultes_dernier_sejour",
               "nb_sejours_2020","nb_sejours_2021","nb_sejours_2022","mt_sejours_2020","mt_sejours_2021",
               "mt_sejours_2022","duree_sejours_2022",
               "nb_participants_sejours_2021","mt_extras_sejours_2020","mt_extras_sejours_2021","mt_extras_sejours_2022",
               "nb_residences_distinctes","duree_sejour_moyen","Boutique","Bien_etre","Sports","meme_saison","meme_pays",
               "meme_type_destination","mt_sejours_moyen","duree_sejour_libelle","duree_totale_sejours",
               "parrain_filleul", "Excursion",
               "nb_participants_sejours_2022", "nb_participants_sejours_2020")

formule <- as.formula(paste("flag_reachat ~ ", paste(liste_var, collapse = " + ")))

rl_2 <- glm(formula = formule,
          data = apprentissage,
          family = "binomial")

# Diagnostics du modèle ----

## Test de significativité globale du modèle (test du rapport de vraisemblance)

pchisq(q = rl_2$null.deviance - rl_2$deviance,
       df = rl_2$df.null - rl_2$df.residual,
       lower.tail = FALSE)

## R² de McFadden (~ R² en régression linéaire)

1 - (rl_2$deviance/(-2)) / (rl_2$null.deviance/(-2)) # 0.06449481

summary(rl_2)
Anova(rl_2)








## Calcul des probabilités estimées

test_rl <- test %>% 
  mutate(p_reachat = predict(object = rl_2,
                                 newdata = test,
                                 type = "response")) %>% 
  select(id_client, flag_reachat, p_reachat)


## Distribution des probabilités estimées

ggplot(test_rl) +
  aes(x = p_reachat) +
  geom_density() +
  theme_minimal()

## Affectation de chaque client à une classe prédite
## Quel seuil choisir ?

### Distribution des probabilités estimées pour chaque catégorie

ggplot(test_rl) +
  aes(x = p_reachat, fill = flag_reachat) +
  geom_density(alpha = 0.7) +
  theme_minimal()


### Distributions cumulées

rl_2 %>%
  blr_gains_table() %>%
  blr_ks_chart()


### Courbe ROC

rl_2 %>%
  blr_gains_table() %>%
  blr_roc_curve()


### Choix du seuil : taux de Y=1

test_rl <- test_rl %>% 
  mutate(flag_reachat_predit = factor(if_else(p_reachat >= 0.2, 1, 0)))

table(test_rl$flag_reachat_predit, test_rl$flag_reachat)
prop(table(test_rl$flag_reachat_predit, test_rl$flag_reachat))   # Taux de bien classés = 80,7%
rprop(table(test_rl$flag_reachat_predit, test_rl$flag_reachat))  # Précision = 35,9%
cprop(table(test_rl$flag_reachat_predit, test_rl$flag_reachat))  # Sensibilité = 82,4%

confusionMatrix(data = test_rl$flag_reachat_predit,
                reference = test_rl$flag_reachat,
                positive = "1")


# Courbe ROC (AUC = ) ----

roc <- data.frame(tx_faux_positifs = 1 - roc(test_rl$flag_reachat, test_rl$p_reachat)$specificities,
                  tx_vrais_positifs = roc(test_rl$flag_reachat, test_rl$p_reachat)$sensitivities) %>% 
  arrange(tx_faux_positifs, tx_vrais_positifs)

auc <- roc(test_rl$flag_reachat, test_rl$p_reachat)$auc
auc

ggplot() +
  geom_line(data = roc,
            aes(x = tx_faux_positifs, y = tx_vrais_positifs),
            color = "steelblue4",
            linewidth = 1) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),
               color = "indianred",
               size = 1) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1),
                     labels = scales::percent) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                     labels = scales::percent) +
  labs(title = paste("Courbe ROC   -   AUC =", round(auc, 3)),
       x = "% Faux positifs\n(1 - spécificité)",
       y = "% Vrais positifs\n(sensibilité)") +
  theme_minimal()


## Courbe de lift ----

lift <- test_rl %>% 
  mutate(decile = ntile(p_reachat, 10)) %>% 
  group_by(decile) %>% 
  summarise(nb_clients = n(),
            nb_resilies = sum(as.numeric(as.character(flag_reachat)))) %>% 
  arrange(- decile)

lift <- lift %>% 
  mutate(nb_clients_cum = cumsum(nb_clients),
         nb_resilies_cum = cumsum(nb_resilies),
         tx_resilies = nb_resilies / nb_clients,
         p_clients_cum = nb_clients_cum / nrow(test_rl),
         p_resilies_cum = nb_resilies_cum / sum(nb_resilies))

lift_parfait <- apprentissage %>% 
  summarise(p = mean(flag_reachat == "1"))

ggplot() +
  geom_path(data = rbind(c(0, 0, 0), lift %>% select(decile, p_clients_cum, p_resilies_cum)),
            aes(x = p_clients_cum, y = p_resilies_cum),
            color = "steelblue4",
            size = 1
  ) +
  geom_line(data = lift,
            aes(x = p_clients_cum, y = p_resilies_cum),
            color = "steelblue4",
            linewidth = 1) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),
               color = "indianred",
               size = 1) +
  geom_segment(data = lift_parfait,
               aes(x = 0, y = 0, xend = p, yend = 1),
               linetype = "dashed") +
  geom_segment(data = lift_parfait,
               aes(x = p, y = 1, xend = 1, yend = 1),
               linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1),
                     labels = scales::percent) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                     labels = scales::percent) +
  labs(title = "Courbe de lift",
       x = "% Population",
       y = "% cumulé Y=1") +
  theme_minimal()

ggplot(data = lift) +
  aes(x = decile, y = tx_resilies) +
  geom_col(fill = "steelblue4") +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                     labels = scales::percent) +
  labs(x = "Décile",
       y = "% Y=1") +
  theme_minimal()
