#_______________________________________________________________________________
# COURS     : Epidemiologie
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
# DONNEES   : Etude ESMAA
#
# PROGRAMME : 2 - SELECTION PATIENTS
#_______________________________________________________________________________

# Variables "pertinentes"

# Démographie & Anthropométrie : Ces variables définissent le profil des patients.
# Age  (quantitative)   - Âge des patients (médiane 44 ans)
# Sexe (qualitative)    - Sexe des patients (femme 60%)
# BMI  (quantitative)   - Indice de masse corporelle (médiane 26)

# Les patients ont un âge médian de 44 ans et sont en majorité des femmes (60%). L'IMC médian est de 26 kg/m2, indiquant une tendance au surpoids, un facteur pouvant influancer la gestion des maladies chroniques.



# Pathologies et Historique Médical: Elles permettent d’évaluer les maladies chroniques associées.
# Hypertension (factor) - Présence d'hypertension (5% des patients)
# Diabetes     (factor) - Présence de diabète (2% des patients)

# Parmi les pathologies recensées, 5% des patients ont une hypertension et 2% un dibète. Même si peu fréquentes, ces pathologies nécessitent une attention particulière en cas de comorbidité avec l'asthme



# Contrôle de l’asthme & Qualité de vie: Ces scores sont essentiels pour évaluer la gestion de la maladie.
# ACT.global.score   (numeric) - Score de contrôle de l’asthme (médiane 20/25)
# SF.8.Mental.health (numeric) - Score de santé mentale (moyenne 46.8)

# Le score ACT global médian est de 20/25, indiquant un contrôle modéré de l'asthme. Le score de santé mentale (SF-8) moyen est de 46.8, ce qui peut montrer l'impact psychologique de la maladie, ce qui peut nécessité une prise en charge plus globale des patients.



# Mode de Vie & Facteurs de Risque: Ils influencent directement l’état de santé des patients.
# Regular.physical.exercise   (factor) - Activité physique régulière (80% "Yes")
# Smoking.status              (factor) - Statut tabagique (81% non-fumeurs)
# Exposure.to.cigarette.smoke (factor) - Exposition au tabagisme passif (20% "Yes")

# 80% des patients pratiquent une activité physique régulière, ce qui peut être bénéfique pour leur santé. 81% sont non-fumeurs, mais 20% sont exposés au tabagisme passif, ce qui peut aggraver leur état de santé et donc être un facteur aggravant de l'asthme.
