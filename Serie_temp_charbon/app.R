library(shiny)
library(bslib)
library(plotly)

# Appel des fichiers externes  -----

source('/Users/rs777/Documents/Projet-datascience/Serie_temp_charbon/scripts/0_environnement.R') # Données
source("/Users/rs777/Documents/Projet-datascience/Serie_temp_charbon/graphs/ui.R") # Page description
source("/Users/rs777/Documents/Projet-datascience/Serie_temp_charbon/graphs/visualisation.R") # Page visualisation
source("/Users/rs777/Documents/Projet-datascience/Serie_temp_charbon/graphs/decomposition.R") # Page decomposisition
source("/Users/rs777/Documents/Projet-datascience/Serie_temp_charbon/graphs/prevision.R") # Page prevision

# UI -----

ui <- page_fillable(
  
  theme = bs_theme(bootswatch = "lumen"),
  
  titlePanel("Séries Temporelles : Analyse de la production de charbon aux États-Unis entre 2001 et 2022. ⛏🏭"),
  
  navset_card_tab(
    
    nav_panel("Visualisation",
              layout_sidebar(
                fillable = TRUE,
                sidebar = sidebar(
                  title = "Paramètres",
                  h5("Moyennes mobiles"),
                  numericInput("order_MOVA", "Ordre de MM( ) :",
                               min = 2, max = 24, value = 12, step = 1),
                  numericInput("order_MOVAC", "Ordre de MMC( ) :",
                               min = 2, max = 24, value = 12, step = 1),
                  h5("Prédiction")
                ),
                plotlyOutput("plot_visualisation")
              )),
    
    
    nav_panel("Décomposition",
              layout_sidebar(
                fillable = TRUE,
                sidebar = sidebar(),
                card(
                  full_screen = TRUE,
                  plotlyOutput("plot_decomposition"),
                ),
                card(
                  full_screen = TRUE,
                  plotlyOutput("plot_lissage")
                )
              )),
    
    
    nav_panel("Prévision",
              layout_sidebar(
                fillable = TRUE,
                sidebar = sidebar(),
                card(
                  full_screen = TRUE,
                  plotlyOutput(""),
                ),
                card(
                  full_screen = TRUE,
                  plotlyOutput("plot_prevision_2023"),
                )
              )),
    
    
    nav_panel("À propos",
              descriptionUI("desc")
    )
  )
)

# Server
server <- function(input, output) {
  
  output$plot_visualisation <- renderPlotly({
    MOVA_MOVAC(charbon, input)
  })
  
  output$plot_decomposition <- renderPlotly({
    Decomposition(charbon)
  })
  
  output$plot_lissage <- renderPlotly({
    Lissage(charbon)
  })
  
  output$plot_prevision_2023 <- renderPlotly({
    Prevision_2023(charbon)
  })
  
  
  Prevision_2023
}


shinyApp(ui = ui, server = server) # Run App -----
