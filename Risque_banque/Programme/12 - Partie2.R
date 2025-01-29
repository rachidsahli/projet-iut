#_______________________________________________________________________________
# COURS     : Risque banque
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
#
# PROGRAMME : 12 - Partie2
#_______________________________________________________________________________


periode_2023$defaut_12M <- as.numeric(as.character(periode_2023$defaut_12M))
taux_defaut_par_classe_2023 <- periode_2023 %>% 
  group_by(PD_ID_NDD) %>%
  summarise(
    Effectif_defaut = sum(defaut_12M == 1, na.rm = TRUE),
    Effectif_total = n(),
    Taux_de_defaut_en_pourcentage = round((Effectif_defaut / Effectif_total) * 100, 3)
  ) %>%
  rename(Classe_de_risque = PD_ID_NDD) %>%
  mutate(Periode = "2023")
periode_reference$defaut_12M <- as.numeric(as.character(periode_reference$defaut_12M))
taux_defaut_par_classe_reference <- periode_reference %>% 
  group_by(PD_ID_NDD) %>%
  summarise(
    Effectif_defaut = sum(defaut_12M == 1, na.rm = TRUE),
    Effectif_total = n(),
    Taux_de_defaut_en_pourcentage = round((Effectif_defaut / Effectif_total) * 100, 3)
  ) %>%
  rename(Classe_de_risque = PD_ID_NDD) %>%
  mutate(Periode = "Reference")
taux_defaut_combine <- bind_rows(taux_defaut_par_classe_2023, taux_defaut_par_classe_reference)
taux_defaut_combine <- taux_defaut_combine %>%
  arrange(Classe_de_risque)
kable(taux_defaut_combine, 
      col.names = c("Classe de risque", "Effectif de defaut", "Effectif total", 
                    "Taux de defaut (%)", "Periode"),
      caption = "Taux de defaut observes par classe de risque pour 2023 et la periode de reference")




taux_defaut_combine <- taux_defaut_combine %>% 
  mutate(PD = Taux_de_defaut_en_pourcentage / 100)
taux_defaut_combine_select <- taux_defaut_combine %>% 
  select(Classe_de_risque, PD, Periode)
kable(taux_defaut_combine_select, 
      col.names = c("Classe de risque", "Probabilité de défaut (PD)", "Période"), 
      caption = "Probabilité de défaut observée par classe de risque pour 2023 et la période de référence")


