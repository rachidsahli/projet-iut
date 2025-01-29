#_______________________________________________________________________________
# COURS     : Risque banque
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
#
# PROGRAMME : 9 - COMPARAISON DEFAUTS
#_______________________________________________________________________________


periode_2023$defaut_12M <- as.factor(periode_2023$defaut_12M)
periode_reference$defaut_12M <- as.factor(periode_reference$defaut_12M)

repartition_ref <- prop.table(table(periode_reference$defaut_12M)) * 100
repartition_2023 <- prop.table(table(periode_2023$defaut_12M)) * 100

repartition_ref <- round(repartition_ref, 2)
repartition_2023 <- round(repartition_2023, 2)

table_comparative <- data.frame(
  "Statut du défaut" = c("Non défaillant", "Défaillant"),
  "Période de référence" = c(repartition_ref["0"], repartition_ref["1"]),
  "Période 2023" = c(repartition_2023["0"], repartition_2023["1"])
)

kable(table_comparative, caption = "Comparaison des défauts (en %)")

x1 <- sum(periode_reference$defaut_12M == "1")
n1 <- length(periode_reference$defaut_12M)

x2 <- sum(periode_2023$defaut_12M == "1")
n2 <- length(periode_2023$defaut_12M)

test_proportion <- prop.test(c(x1, x2), c(n1, n2))
test_proportion

