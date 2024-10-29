# Packages ----

library(shiny)
library(shinythemes)
library(shinyalert)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(dplyr)
library(stringr)
library(htmlwidgets)
library(webshot)
# if (!webshot::is_phantomjs_installed()) {
#   webshot::install_phantomjs()
# }
# library(ggplot2)


# ui ----

ui <- dashboardPage(
  dashboardHeader(title = "TDR PROFILE ANALYZER",
                  disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(
      # On mets ici le lien avec le fichier CSS, qui determine le style de l'application
      # Mais ce fichier s'appliquera à l'ensemble de l'app, pas uniquement au dashboardBody
      tags$link(rel = "stylesheet", type = "text/css", href = "script_style.css")
    ),
    
    useShinyjs(),
    fluidRow(
      column(8,
             h1("TDR PROFILE ANALYZER", style = "padding-left: 20px;font-family: Helvetica, Arial, sans-serif; margin-top: 5px; margin-bottom: 5px; color:#26355b;"),
             h3("version 1.0", style = "padding-left: 60px;font-family: Helvetica, Arial, sans-serif; margin-top: 5px; margin-bottom: 15px; color:#26355b;")),
      column(width = 4,
             tags$img(src = "logo_ob7_ird.png", height = "70px", style = "float: right; padding-right: 30px;")),
    ),
    fluidRow(
      box(
        title = "1. Chargement du fichier .csv", 
        width = 12,
        status = "primary", solidHeader = TRUE,
        
        column(12,
               # Input : le fichier .csv d'entrée
               column(8,
                      h5("Sélectionnez un fichier .csv original provenant d'un NKE WiSens TD1000 v2 (avec ou sans profondeur) :"),
                      fileInput("file1", label = NULL,
                                buttonLabel = "Parcourir",
                                placeholder = "Pas de fichier sélectionné",
                                multiple = FALSE,
                                accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv"))),
               
               # Numéro de série, dates de marée
               column(4,
                      h5("Metadata :"),
                      textOutput("tdrmodel"),
                      textOutput("numerodeserie"))),
        
        # Partie de l'UI a afficher uniquement si un fichier
        # d'entrée a bien été sélectionné
        uiOutput("fichier_entree")
      )
    ),
    fluidRow(
      box(
        title = "2. Profil TDR et données clés", 
        width = 12,
        status = "primary", solidHeader = TRUE,
        
        column(6,
               h5("Sur le profil suivant, cliquez pour placer dans l'ordre :"),
               h5("- (A) Le début du déploiement."),
               h5("- (B) Le début de pêche."),
               h5("- (C) La fin de pêche (début de la remontée ou capture d'un individu)."),
               h5("- (D) La fin du déploiement (la fin de la remontée)."),
               h5("Veillez à bien cliquer sur les points dans l'ordre demandé."),
               h5("Les points que vous avez sélectionnés s'affichent en rouge sur le graphique, et leurs coordonnées dans le tableau ci-dessous.")),
        
        column(6,
               h5("Si vous vous êtes trompés, cliquez sur le bouton Recommencer."),
               actionButton("clear_points", "Recommencer"),
               h5("Une fois que vous avez sélectionné les 4 points demandés, cliquez sur le bouton Calculer pour obtenir les métriques qui nous intéressent."),
               actionButton("calculate", "Calculer")),
        
        # Partie de l'UI a afficher uniquement si un fichier
        # d'entrée a bien été sélectionné
        uiOutput("graphe_et_tables")
      )
    ),
    fluidRow(
      box(
        title = "3. Téléchargements", 
        width = 12,
        status = "primary", solidHeader = TRUE,
        
        fluidRow(
          column(
            width = 6,
            align = "center",
            br(),
            downloadButton("download_plot", "Télécharger figure en .png")
          ),
          
          column(
            width = 6,
            align = "center",
            br(),
            downloadButton("download_data", "Télécharger données en .txt")
          )
        )
      )
    )
  )
)
