#_______________________________________________________________________________
# COURS     : Risque banque
# ANNEE     : 2025
# AUTEUR    : Rachid SAHLI
#
#
# PROGRAMME : 6 - COMPARAISON DES MDOALITES DE DEFAUT
#_______________________________________________________________________________

calculate_iv_modalities <- function(data, variable, target) { 
  data %>% 
    group_by(!!sym(variable)) %>% 
    summarise( 
      prop_bons = sum(!!sym(target) == 0) / sum(data[[target]] == 0), 
      prop_mauvais = sum(!!sym(target) == 1) / sum(data[[target]] == 1), 
      .groups = "drop" 
    ) %>% 
    mutate( 
      prop_bons = ifelse(prop_bons == 0, 1e-10, prop_bons), 
      prop_mauvais = ifelse(prop_mauvais == 0, 1e-10, prop_mauvais), 
      modalite = as.character(!!sym(variable)),  
      woe = log(prop_bons / prop_mauvais),      
      iv = (prop_bons - prop_mauvais) * woe,    
      variable = variable                       
    ) %>% 
    mutate(
      prop_bons = round(prop_bons, 2),
      prop_mauvais = round(prop_mauvais, 2),
      woe = round(woe, 2),
      iv = round(iv, 2)
    ) %>% 
    select(variable, modalite, prop_bons, prop_mauvais, iv) 
}

calculate_iv_for_all_variables <- function(data, target) {
  vars <- names(data)[sapply(data, is.factor) | sapply(data, is.character)]
  
  iv_results <- lapply(vars, function(var) calculate_iv_modalities(data, var, target))
  
  iv_df <- bind_rows(iv_results)
  
  return(iv_df)
}

iv_results <- calculate_iv_for_all_variables(periode_reference, "defaut_12M")
iv_results <- iv_results %>% arrange(desc(iv))
print(iv_results)
