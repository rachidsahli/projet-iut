#_______________________________________________________________________________
# COURS     : Risque banque
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
#
# PROGRAMME : 10 - PORTEFEUILLE
#_______________________________________________________________________________

frequence_ref <- periode_reference %>%
  count(`PD_ID_NDD`) %>%
  mutate(Frequence = n / sum(n) * 100) %>%
  select(`PD_ID_NDD`, Frequence)

frequence_2023 <- periode_2023 %>%
  count(`PD_ID_NDD`) %>%
  mutate(Frequence = n / sum(n) * 100) %>%
  select(`PD_ID_NDD`, Frequence)

table_comparative <- merge(
  frequence_ref, 
  frequence_2023, 
  by = "PD_ID_NDD", 
  suffixes = c("_reference", "_2023")
)

colnames(table_comparative) <- c("Classe de risque", "Fréquence référence (%)", "Fréquence 2023 (%)")
table_comparative <- table_comparative %>%
  mutate(across(-`Classe de risque`, round, 2))
kable(table_comparative, caption = "Répartition des fréquences par classe de risque")


encours_ref <- periode_reference %>%
  group_by(`PD_ID_NDD`) %>%
  summarise(Encours = sum(`XE_EXP_B2`, na.rm = TRUE)) %>%
  mutate(Frequence = Encours / sum(Encours) * 100)

encours_2023 <- periode_2023 %>%
  group_by(`PD_ID_NDD`) %>%
  summarise(Encours = sum(`XE_EXP_B2`, na.rm = TRUE)) %>%
  mutate(Frequence = Encours / sum(Encours) * 100)

table_encours <- merge(
  encours_ref %>% select(`PD_ID_NDD`, Frequence),
  encours_2023 %>% select(`PD_ID_NDD`, Frequence),
  by = "PD_ID_NDD",
  suffixes = c("_reference", "_2023")
)
colnames(table_encours) <- c("Classe de risque", "Encours référence (%)", "Encours 2023 (%)")
table_encours <- table_encours %>%
  mutate(across(-`Classe de risque`, round, 2))
kable(table_encours, caption = "Répartition des encours par classe de risque (en %)")















