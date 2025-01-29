#_______________________________________________________________________________
# COURS     : Risque banque
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
#
# PROGRAMME : 7 - INDICE DE GINI
#_______________________________________________________________________________

roc_ref <- roc(periode_reference$defaut_12M, periode_reference$SCORE, quiet = TRUE)
roc_2023 <- roc(periode_2023$defaut_12M, periode_2023$SCORE, quiet = TRUE)
auc_ref <- auc(roc_ref)
auc_2023 <- auc(roc_2023)
gini_ref <- 2 * auc_ref - 1
gini_2023 <- 2 * auc_2023 - 1
resultats_performance <- data.frame(
  Période = c("Période de référence", "Période 2023"),
  AUC = c(auc_ref, auc_2023),
  Gini = c(gini_ref, gini_2023)
)
kable(resultats_performance, caption = "Comparaison indice de performance")

pred_prob_ref <- periode_reference$SCORE
pred_ref <- prediction(pred_prob_ref, periode_reference$defaut_12M)
perf_ref <- performance(pred_ref, "tpr", "fpr")
auc_ref <- performance(pred_ref, "auc")@y.values[[1]]
gini_ref <- 2 * auc_ref - 1

pred_prob_2023 <- periode_2023$SCORE
pred_2023 <- prediction(pred_prob_2023, periode_2023$defaut_12M)
perf_2023 <- performance(pred_2023, "tpr", "fpr")
auc_2023 <- performance(pred_2023, "auc")@y.values[[1]]
gini_2023 <- 2 * auc_2023 - 1

plot(perf_ref, main = paste("Courbe ROC\nIndice de Gini =", round(gini_ref, 2)),
     col = "red", lwd = 2, xlim = c(0, 1), ylim = c(0, 1))
abline(a = 0, b = 1, col = "black", lty = 2)
plot(perf_2023, col = "blue", lwd = 2, add = TRUE)
legend("bottomright", legend = c("Période de Référence", "Période 2023"), 
       col = c("red", "blue"), lwd = 2)
