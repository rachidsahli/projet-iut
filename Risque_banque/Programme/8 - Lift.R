#_______________________________________________________________________________
# COURS     : Risque banque
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
#
# PROGRAMME : 8 - LIFT
#_______________________________________________________________________________

lift_curve <- function(data, score_col, target_col) {
  data <- data %>% arrange(desc(!!sym(score_col)))
  data <- data %>%
    mutate(cum_rank = row_number(),
           cum_total = n(),
           cum_defauts = cumsum(!!sym(target_col)),
           total_defauts = sum(!!sym(target_col), na.rm = TRUE),
           lift = cum_defauts / total_defauts)
  data <- data %>%
    mutate(cum_prop_pop = cum_rank / cum_total)
  return(data)
}

periode_2023$defaut_12M <- as.numeric(periode_2023$defaut_12M)
periode_reference$defaut_12M <- as.numeric(periode_reference$defaut_12M)

lift_ref <- lift_curve(periode_reference, score_col = "SCORE", target_col = "defaut_12M")
lift_2023 <- lift_curve(periode_2023, score_col = "SCORE", target_col = "defaut_12M")
plot(lift_ref$cum_prop_pop, lift_ref$lift, type = "l", col = "red", 
     xlab = "Proportion cumulative de la population", 
     ylab = "Lift cumulatif", 
     main = "Courbe de Lift", 
     lwd = 2, xlim = c(0, 1), ylim = c(0, 1))
lines(lift_2023$cum_prop_pop, lift_2023$lift, col = "blue", lwd = 2)
lines(c(0, 1), c(0, 1), col = "black", lty = 2, lwd = 2)
legend("bottomright", legend = c("Période de Référence", "Période 2023"), 
       col = c("red", "blue"), lwd = 2)

periode_2023$defaut_12M <- as.factor(periode_2023$defaut_12M)
periode_reference$defaut_12M <- as.factor(periode_reference$defaut_12M)
