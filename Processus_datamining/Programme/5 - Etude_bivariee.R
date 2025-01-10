#_______________________________________________________________________________
# COURS     : SCORING
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
# DONNEES   : Agence de voyage
#
# PROGRAMME : 5 - Etude_bivariee
#_______________________________________________________________________________


# Matrice de corrélation des variables quantitatives -----

var_quanti <- base_2022 %>% 
  select_if(is.numeric)

# Matrice de corrélation
matrice <- cor(var_quanti, use = "complete.obs")

# Paires de variables avec une corrélation > 0.7
correlations_fortes <- which(abs(matrice) > 0.7, arr.ind = TRUE)
correlations_fortes <- correlations_fortes[correlations_fortes[,1] < correlations_fortes[,2], ]

pairs_corr <- data.frame(
  Variable_1 = colnames(matrice)[correlations_fortes[,1]],
  Variable_2 = colnames(matrice)[correlations_fortes[,2]],
  Correlation = matrice[correlations_fortes]
)

# Affichage des résultats
kable(pairs_corr, col.names = c("Variable 1", "Variable 2", "Correlation"))

# Selection des variables quantitatives
var_qt <- base_2022 %>%
  select(where(is.numeric)) %>% 
  colnames()


# Mesures d'intensité du lien de chaque variable avec la variable à expliquer
## - Statistique du test de Student
## - Statistique du test de Wilcoxon
## - Distance de Kolmogorov-Smirnov
## - Rapport de corrélation


student <- sapply(base_2022[var_qt],
                  function(x) { abs(t.test(x ~ base_2022$flag_reachat)$statistic) })

wilcoxon <- sapply(base_2022[var_qt],
                   function(x) { wilcox.test(x ~ base_2022$flag_reachat)$statistic })

ks <- sapply(base_2022[var_qt],
             function(x) { ks.test(x ~ base_2022$flag_reachat)$statistic })

eta2 <- sapply(base_2022[var_qt],
               function(x) { eta2(x, base_2022$flag_reachat) })


## Regroupement des mesures issues de chaque test

liens_var_qt <- data.frame(eta2, ks, wilcoxon, student) %>% 
  rownames_to_column(var = "variable") %>% 
  arrange(desc(ks))



## Représentation graphique de la distance de Kolmogorov-Smirnov

ggplot(liens_var_qt) +
  aes(x = ks, y = reorder(variable, ks)) +
  geom_col(fill = "steelblue4") +
  labs(x = "Distance de Kolmogorov-Smirnov", y = NULL) +
  theme_minimal()


# Calcul des mesures de liaison pour toutes les variables qualitatives ----

## Liste des variables à analyser

var_ql <- base_2022 %>%
  select(where(is.factor), - flag_reachat) %>% 
  colnames()

## Caractérisation fine des réacheteurs

profil_resilies_ql <- as.data.frame(round(catdes(base_2022[, c("flag_reachat", var_ql)],
                                                 1)$category$"1",
                                          1)) %>% 
  rownames_to_column(var = "indicateurs")

ggplot(profil_resilies_ql) +
  aes(y = reorder(indicateurs, v.test), x = v.test) +
  geom_col(fill = "steelblue4") +
  labs(x = "Valeur test", y = NULL) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))


# Analyse simultanée des variables quantitatives et qualitatives ----

## Découpage des variables quantitatives en déciles (avec les NA mis dans une modalité à part)

base_2022_2 <- base_2022 %>% 
  mutate(across(all_of(var_qt), ~ ntile(.x, n = 10)),
         across(all_of(var_qt), as.factor))

var_ql_all <- base_2022_2 %>%
  select(where(is.factor), - flag_reachat) %>% 
  colnames()



# Etude de la correlation entre les variables avec la variable flag_reachat avec t.test -----

## Création du dataframe ----
results <- data.frame(
  Variable = character(),
  t_statistic = numeric(),
  p_value = numeric(),
  Mean_Group_0 = numeric(),
  Mean_Group_1 = numeric(),
  stringsAsFactors = FALSE
)

## Boucle t-test
for (var_name in colnames(var_quanti)) {
  
  t_test_result <- t.test(var_quanti[[var_name]] ~ base_2022$flag_reachat, na.rm = TRUE)
  
  # Moyenne
  mean_group_0 <- mean(var_quanti[[var_name]][base_2022$flag_reachat == 0], na.rm = TRUE)
  mean_group_1 <- mean(var_quanti[[var_name]][base_2022$flag_reachat == 1], na.rm = TRUE)
  
  results <- rbind(results, data.frame(
    Variable = var_name,
    t_statistic = t_test_result$statistic,
    p_value = t_test_result$p.value,
    Mean_Group_0 = mean_group_0,
    Mean_Group_1 = mean_group_1,
    stringsAsFactors = FALSE
  ))

  # Affichage
  cat("\nVariable:", var_name, "\n")
  cat("  Moyenne groupe 0:", mean_group_0, "\n")
  cat("  Moyenne groupe 1:", mean_group_1, "\n")
  cat("  p-valeur:", t_test_result$p.value, "\n")
  
  data_plot <- data.frame(
    Value = var_quanti[[var_name]],
    Flag = factor(base_2022$flag_reachat)
  )
  
  # Boîte à moustaches
  boxplot_plot <- ggplot(data_plot, aes(x = Flag, y = Value, fill = Flag)) +
    geom_boxplot(outlier.colour = "black", alpha = 0.7) +
    scale_fill_manual(values = c("red", "green")) +
    labs(
      title = paste("Boîte à moustaches de", var_name),
      x = "Flag_reachat",
      y = var_name
    ) +
    theme_classic()
  
  # Densité
  density_plot <- ggplot(data_plot, aes(x = Value, fill = Flag)) +
    geom_density(alpha = 0.4) +
    scale_fill_manual(values = c("red", "green")) +
    labs(
      title = paste("Densité de", var_name),
      x = var_name,
      y = "Densité"
    ) +
    theme_classic()
  
  grid.arrange(boxplot_plot, density_plot, ncol = 2)
}

print(results)


















