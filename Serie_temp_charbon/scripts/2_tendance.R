# Objectif : montrer la tendance de la série

# Run environnement pour récupérer les données 

source('scripts/0_environnement.R')

## Moyennes mobiles

charbonMova = stats::filter(charbon, filter = rep(1/12, 12), sides = 2) # MM 12
plot(charbonMova)

charbonMOVAC = stats::filter(charbon, filter = c(1/24, rep(1/12,11), 1/24)) # MMC 12
plot(charbonMOVAC)

## Courbe de régression des moyennes annuelles

y = as.vector(charbon) # Yi
x = as.vector(time(charbon)) # ti (12 mois)

length(x)/12 # 264 ti = 22 années d'observation

xclass <- cut(x, 22) # 22 intervalles égaux

meananual <- tapply(y, xclass, mean) # Moyenne de y dans chaque intervalles

t <- seq(2001, 2022, length = 22)

plot(x, y, main = "", ylab = "Charbon (mWh)", xlab = "Année")
lines(x, y, lwd = 1.2)
lines(t, meananual, col = "red", lwd = 2)
lines(charbonMOVAC, col = "yellow", lwd = 2)

### Graphique plotly

plot_ly() %>%
  add_lines(x = x, y = y, mode = 'lines', 
            name = 'Données mensuelles', 
            line = list(color = 'black', width = 1.2)) %>%
  add_lines(x = t, y = meananual, mode = 'lines', 
            name = 'Régression des moyennes annuelles', 
            line = list(color = 'red', width = 2)) %>%
  add_lines(x = time(charbonMOVAC), y = charbonMOVAC, mode = 'lines', 
            name = 'MMC(12)', 
            line = list(color = 'orange', width = 2)) %>% 
  layout(title = "Production de charbon aux États-Unis (2021-2022)",
         xaxis = list(title = "Année"),
         yaxis = list(title = "Charbon (mWh)"),
         legend = list(x = 0.8, y = 0.9,
                       bgcolor = "rgba(255, 255, 255, 0.5)",
                       font = list(size = 15)),
         template = "plotly_dark")