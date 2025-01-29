#_______________________________________________________________________________
# COURS     : Risque banque
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
#
# PROGRAMME : 11 - IV sur classe de risque
#_______________________________________________________________________________

calc_iv <- function(data) {
  df <- data %>%
    group_by(PD_ID_NDD) %>%
    summarise(
      effectifs = n(),
      bons = sum(defaut_12M == 0),
      mauvais = sum(defaut_12M == 1)
    ) %>%
    mutate(
      prop_bons = bons / effectifs,
      prop_mauvais = mauvais / effectifs,
      woe = log(prop_bons / prop_mauvais),
      iv = (prop_bons - prop_mauvais) * woe
    ) %>%
    select(PD_ID_NDD, iv) %>%
    arrange(desc(iv))
  
  return(df)
}

iv_2023 <- calc_iv(periode_2023)
iv_reference <- calc_iv(periode_reference)
iv_comparatif <- merge(iv_2023, iv_reference, by = "PD_ID_NDD", suffixes = c("_2023", "_reference"))
kable(iv_comparatif, caption = "Valeur d'information par classe de risque (2023 vs Référence)")