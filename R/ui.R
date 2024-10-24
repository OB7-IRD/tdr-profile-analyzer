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


# ui ----

ui <- dashboardPage(
  dashboardHeader(title = "TDR PROFILE ANALYZER - v0.1",
                  titleWidth = 500),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    useShinyjs(),
    fluidRow(
      box(
        title = "1. Sélection du fichier .csv (NKE WiSens TD1000 v2)", 
        width = 12,
        status = "primary", solidHeader = TRUE,
        
        column(12,
               # Input : le fichier .csv d'entrée
               column(8,
                      fileInput("file1", label = NULL,
                                buttonLabel = "Parcourir",
                                placeholder = "Pas de fichier sélectionné",
                                multiple = FALSE,
                                accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv"))),
               
               # Numéro de série, dates de marée
               column(4,
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
               h5("Sur le profile suivant, cliquez pour placer dans l'ordre :"),
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
        
        # h5("S'il y a eu une capture, entrez le code FAO de l'espèce :"),
        # textInput("capture_FAO",
        #           label = NULL,
        #           value = "",
        #           width = '100px'),
        # downloadButton("download_plot", "Télécharger figure en .png"),
        # downloadButton("download_data", "Télécharger données en .txt")
        
        fluidRow(
          column(
            width = 4,
            align = "center",
            h5("S'il y a eu une capture, entrez le code FAO de l'espèce :"),
            textInput("capture_FAO", label = NULL, value = "", width = '100px')
          ),
          
          column(
            width = 4,
            align = "center",
            br(),
            downloadButton("download_plot", "Télécharger figure en .png")
          ),
          
          column(
            width = 4,
            align = "center",
            br(),
            downloadButton("download_data", "Télécharger données en .txt")
          )
        )
      )
    )
  )
)
