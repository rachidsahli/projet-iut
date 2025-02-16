# Import library ----

library(shiny)
library(bslib)
library(plotly)
library(rsconnect)
library(forecast)
library(tseries)

# Appel des fichiers externes  -----

source('graphs/environnement.R') # Données
source("graphs/ui.R") # Page description
source("graphs/visualisation.R") # Page visualisation
source("graphs/decomposition.R") # Page decomposisition
source("graphs/prevision.R") # Page prevision
source("graphs/prevision_2022.R") # Page prevision


# UI -----

ui <- page_fillable(
  
  theme = bs_theme(bootswatch = "lumen"),
  
  titlePanel("Séries Temporelle : Analyse de la production de charbon aux États-Unis entre 2001 et 2022 💡🔨"),
  
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
                  nav_spacer(),
                  actionButton("quit_button", "Quitter", icon = icon("sign-out-alt"), class = "btn-danger")
                ),
                plotlyOutput("plot_visualisation")
              )),
    
    
    nav_panel("Décomposition",
              card(
                full_screen = TRUE,
                plotlyOutput("plot_decomposition"),
              ),
              card(
                full_screen = TRUE,
                plotlyOutput("plot_lissage")
              )
    ),
    
    
    nav_panel("Prévision",
              card(
                full_screen = TRUE,
                plotlyOutput("plot_prevision_2022"),
              ),
              card(
                full_screen = TRUE,
                plotlyOutput("plot_prevision_2023"),
              )
    ),
    
    
    nav_panel("À propos",
              card(
                height = 300,
                descriptionUI("desc")
              )
    )
  )
)

# Server
server <- function(input, output) {
  
  observeEvent(input$quit_button, {
    stopApp()  
  })
  
  output$plot_visualisation <- renderPlotly({
    MOVA_MOVAC(charbon, input)
  })
  
  output$plot_decomposition <- renderPlotly({
    Decomposition(charbon)
  })
  
  output$plot_lissage <- renderPlotly({
    Lissage(charbon)
  })
  
  output$plot_prevision_2022 <- renderPlotly({
    Prevision_2022(charbon_2022)
  })
  
  output$plot_prevision_2023 <- renderPlotly({
    Prevision_2023(charbon)
  })
  
}


shinyApp(ui = ui, server = server) # Run App -----
