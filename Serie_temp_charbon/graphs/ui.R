descriptionUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    h3("📌 Contexte"),
    p("Ce projet a pour objectif d'étudier la production d'électricité 
       par combustion aux États-Unis, de 2001 à 2022. 
       Le jeu de données provient du site web de l'EIA 
       (Energy Information Administration)."),
    
    p("L'Administration américaine de l'information sur l'énergie est une agence clé du système statistique fédéral des États-Unis. 
         Elle est responsable de la collecte, de l'analyse et de la diffusion des informations sur l'énergie, afin de promouvoir une prise de décision éclairée, 
         de favoriser des marchés efficaces et d'améliorer la compréhension publique de l'énergie ainsi que de son interaction avec l'économie et l'environnement."),
    p("Les programmes de l'EIA couvrent des données sur divers secteurs, le charbon, le pétrole, le gaz naturel, l'éléctricité, lénergie nucléaire et les énergies renouvelables."),
    
    h3("📊 Méthodologie d'analyse"),
    p("Nous analysons l'évolution mensuelle de la production 
       d'électricité par combustion de 2001 à 2022 à l'aide des 
       méthodes suivantes :"),
    
    tags$ul(
      tags$li("Décomposition de la série, en appliquant un filtrage de moyennes mobiles et en ajustant les séries pour prendre en compte les effets saisonniers."),
      tags$li("Prévision de la production d'électricité pour 2023 en utilisant la tendance + saisonnalité, Holt-Winters, ARMA."),
      tags$li("Évaluation de la prévision à l'aide de l'erreur quadratique moyenne.")
    ),
    
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.2/css/all.min.css")
    ),
    
    tags$div(
      style = "margin-top: 10px; display: flex; gap: 10px;",
      
      tags$a(
        href = "https://www.eia.gov/",
        target = "_blank",
        tags$button(
          "EIA",
          style = "background-color: #007bff; color: white; border: none; 
               padding: 10px 15px; border-radius: 5px; font-size: 16px; 
               cursor: pointer; transition: background-color 0.3s ease;",
          onmouseover = "this.style.backgroundColor='#0056b3'",
          onmouseout = "this.style.backgroundColor='#007bff'"
        )
      ),
      
      tags$a(
        href = "https://github.com/rachidsahli",
        target = "_blank",
        style = "text-decoration: none;", 
        tags$button(
          HTML('<i class="fab fa-github"></i> GitHub'),
          style = "background-color: black; color: white; border: none; 
               padding: 10px 15px; border-radius: 5px; font-size: 16px; 
               cursor: pointer; transition: background-color 0.3s ease; 
               display: flex; align-items: center; gap: 5px;",
          onmouseover = "this.style.backgroundColor='#333'",
          onmouseout = "this.style.backgroundColor='black'"
        )
      ),
      
      tags$a(
        href = "mailto:rachidsahli144pro@outlook.com",
        target = "_blank",
        style = "text-decoration: none;", 
        tags$button(
          HTML('<i class="fas fa-envelope"></i> Mail'),
          style = "background-color: #008f7a; color: white; border: none; 
               padding: 10px 15px; border-radius: 5px; font-size: 16px; 
               cursor: pointer; transition: background-color 0.3s ease; 
               display: flex; align-items: center; gap: 5px;",
          onmouseover = "this.style.backgroundColor='#333'",
          onmouseout = "this.style.backgroundColor='#008f7a'"
        )
      )
    )
  )
}
