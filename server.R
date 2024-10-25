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
if (!webshot::is_phantomjs_installed()) {
  webshot::install_phantomjs()
}


# server ----

server <- function(input, output, session) {
  
  ## Les parties d'UI cachées ----
  
  # La partie de l'UI graphe et tables, a afficher uniquement
  # si un fichier d'entrée a bien été sélectionné
  output$graphe_et_tables <- renderUI({
    if (is.null(input$file1)) {
      # Si aucun fichier n'est sélectionné, ne rien afficher
      return(NULL)
    } else {
      # Contenu à afficher si le fichier est sélectionné
      list(
        column(12,
               hr(),
               plotlyOutput("plot"),
               hr()),
        
        column(6,
               h4("Horodatage :"),
               tableOutput("selected_points")),
        
        column(6,
               h4("Données clés :"),
               tableOutput("calculated_table"))
      )
    }
  })
  
  # La partie de l'UI sur le fichier d'entrée, a afficher uniquement
  # si un fichier d'entrée a bien été sélectionné
  output$fichier_entree <- renderUI({
    if (is.null(input$file1)) {
      # Si aucun fichier n'est sélectionné, ne rien afficher
      return(NULL)
    } else {
      # Contenu à afficher si le fichier est sélectionné
      list(
        column(12,
               # Output: le .csv chargé
               column(8, tableOutput("contents")),
               # Input : ce qu'il faut afficher
               column(4,
                      radioButtons("disp", "Affichage",
                                   choices = c("Premières lignes" = "head",
                                               "Toutes les lignes" = "all"),
                                   selected = "head")))
      )
    }
  })
  
  
  ## Objets réactifs ----
  
  # Valeur réactive qui stocke les points cliqués sur le graphique
  selected_points <- reactiveVal(data.frame(Etape = character(0),
                                            Nom = character(0),
                                            TimeStamp = character(0),
                                            Depth = numeric(0)))
  
  # Valeur réactive pour le tableau des métriques à calculer
  calculated_data <- reactiveVal(data.frame(Metrique = character(0),
                                            Profondeur = numeric(0)))
  
  # Quand on change de fichier d'entrée, on ré-initialise les valeurs réactive
  observeEvent(input$file1, {
    selected_points(data.frame(Etape = character(0),
                               Nom = character(0),
                               TimeStamp = character(0),
                               Depth = numeric(0)))
    calculated_data(data.frame(Metrique = character(0),
                               Profondeur = numeric(0)))
  })
  
  # Evenement réactif pour l'enregistrement des clics sur les points
  observeEvent(event_data("plotly_click"), {
    click_data <- event_data("plotly_click")
    
    # Les points actuellement sélectionnés, issus de l'objet réactif
    current_points <- selected_points()
    
    # Récupération des coordonnées x et y du point cliqué
    time <- click_data$x
    depth <- click_data$y
    
    # Vérification de si le point cliqué est déjà dans la table
    point_exists <- any(current_points$TimeStamp == time & current_points$Depth == depth)
    if (point_exists) {
      
      # Cas 1 : l'utilisateur clique sur un point déjà cliqué, il ne se passe rien
      
    } else {
      
      # Cas 2 : l'utilisateur clique sur un point pas encore sélectionné, il veut l'ajouter
      
      if (nrow(current_points) >= 4) {
        # Cas 2.1 : l'utilisateur a déjà sélectionné 4 points
        
        shinyalert::shinyalert(title = "Vous avez déjà placé les 4 points demandés",
                               text = "Si vous souhaitez changer les points sélectionnés, cliquez sur le bouton Recommencer.",
                               type = "warning",
                               confirmButtonText = "Ok")
        
      } else {
        # Cas 2.2 : l'utilisateur a sélectionné moins de 4 points
        
        if (nrow(current_points) == 0 | all(current_points$TimeStamp < time)) {
          # Cas 2.2.1 : premier point sélectionné,
          # ou bien le point sélectionné est bien APRES le point précédent
          
          # Création de l'étape du point
          etape <- LETTERS[nrow(current_points) + 1]
          # Nom de l'étape
          nom_etape <- "Erreur, veuillez supprimer les points et recommencer."
          if (etape == "A") {
            nom_etape <- "Début du déploiement"
          } else if (etape == "B") {
            nom_etape <- "Début de pêche"
          } else if (etape == "C") {
            nom_etape <- "Fin de pêche"
          } else if (etape == "D") {
            nom_etape <- "Fin du déploiement"
          }
          # Ajout du point à l'objet new_points
          new_points <- rbind(current_points, data.frame(Etape = etape,
                                                         Nom = nom_etape,
                                                         TimeStamp = time,
                                                         Depth = depth))
          # Mettre à jour le réactif avec le nouveau tableau
          selected_points(new_points)
        } else {
          # Cas 2.2.2 : l'utilisateur a cliqué sur un point avant un point déjà sélectionné
          
          shinyalert::shinyalert(title = "Il faut sélectionner les points dans l'ordre demandé.",
                                 text = "Si vous vous êtes trompés, cliquez sur le bouton Recommencer.",
                                 type = "warning",
                                 confirmButtonText = "Ok")
        }
      }
    }
  })
  
  # Bouton qui permet de réinitialiser les points sélectionnés
  observeEvent(input$clear_points, {
    selected_points(data.frame(Etape = character(0),
                               Nom = character(0),
                               TimeStamp = character(0),
                               Depth = numeric(0)))
    calculated_data(data.frame(Metrique = character(0),
                               Profondeur = numeric(0)))
  })
  
  ## Fonctions ----
  
  # Function stage1 : chargement du fichier d'entrée
  stage1 <- function(input){
    # Lire le fichier CSV téléchargé
    df0 <- read.csv(input$file1$datapath,
                    header = T)
    
    # Isoler les metadata
    metdata <- df0[substr(df0[,1], 1, 1) == "#", "Timestamp.Standard."] 
    # Supprimer ces metadata
    df1 <- df0[substr(df0[,1], 1, 1) != "#",]
    
    # On vérifie les colonnes présentes
    # Vérifier si une colonne contient le mot "Depth" (insensible à la casse)
    has_depth_col <- any(grepl("Depth", colnames(df0), ignore.case = TRUE))
    # Vérifier si une colonne contient le mot "Pressure" (insensible à la casse)
    has_pressure_col <- any(grepl("Pressure", colnames(df0), ignore.case = TRUE))
   
    if (has_pressure_col && has_depth_col) { # Cas 1 : on a Depth en CH2 et Pressure en CH0
      df1 <- as_tibble(df1) %>%
        mutate(Timestamp.Standard. = as.character(Timestamp.Standard.),
               CH0.Pressure.bar. = as.numeric(CH0.Pressure.bar.),
               CH1.Temperature.degC. = as.numeric(CH1.Temperature.degC.),
               CH2.Depth.m. = as.numeric(CH2.Depth.m.))
      return(df1)
    } else if (has_pressure_col && !has_depth_col) { # Cas 2 : on a Pressure en CH0 mais pas Dep
      df1 <- as_tibble(df1) %>%
        mutate(Timestamp.Standard. = as.character(Timestamp.Standard.),
               CH0.Pressure.bar. = as.numeric(CH0.Pressure.bar.),
               CH1.Temperature.degC. = as.numeric(CH1.Temperature.degC.))
      df1$CH2.Depth.m. <- df1$CH0.Pressure.bar. * 10
      return(df1)
    }
  }
  
  # Function stage2 : transformation du fichier d'entrée en un truc un peu + lisible
  stage2 <- function(df1){
    # Modify columns
    df2 <- df1 %>%
      rename(TimeStamp = Timestamp.Standard.) %>%
      rename(Pressure = CH0.Pressure.bar.) %>%
      rename(Temperature = CH1.Temperature.degC.) %>%
      rename(Depth = CH2.Depth.m.) %>%
      # mutate(TimeStamp = as.POSIXct(TimeStamp, format = "%d/%m/%Y %H:%M")) %>%
      mutate(TimeStamp = as.POSIXct(TimeStamp, format = "%Y-%m-%d %H:%M:%S")) %>%
      mutate(Date = as.Date(TimeStamp)) %>%
      mutate(Time = substr(TimeStamp, 12, 20))
    # Return tibble
    return(df2)
  }
  
  
  ## Outputs ----
  
  output$tdrmodel <- renderText({
    req(input$file1)
    tdrmodel <- "NKE WiSens TD1000"
    paste("Modèle du TDR :", tdrmodel)
  })
  
  output$numerodeserie <- renderText({
    req(input$file1)
    file_name <- input$file1$name
    # Extraire les caractères avant le premier "_"
    numerodeserie <- sub("_.*", "", file_name)
    # Remplace tout après le premier "_" par une chaîne vide
    paste("Numéro de série :", numerodeserie)
  })
  
  output$contents <- renderTable({
    req(input$file1)
    df1 <- stage1(input)
    
    # On affiche seulement les premieres lignes ou toute la table
    if(input$disp == "head") {
      return(head(df1, n = 3))
    }
    else {
      return(df1)
    }
  })
  
  # Le graphique
  output$plot <- renderPlotly({
    
    req(input$file1)
    
    # Run stage1 and stage2
    df1 <- stage1(input)
    df2 <- stage2(df1)
    
    # Le graphique
    p <- plot_ly(data = df2, x = ~TimeStamp, y = ~Depth * -1,
                 type = 'scatter', mode = 'lines') %>%
      layout(
        title = list(text = gsub(".csv", "", input$file1$name)),
        xaxis = list(title = "Jour et heure", tickformat = "%y-%m-%d %H:%M"),
        yaxis = list(title = "Profondeur (m)", range = c(max(df2$Depth, na.rm = TRUE) * -1, 0)),
        showlegend = FALSE
      )
    
    # Ajout des points sélectionnés sur le graphique
    current_points <- selected_points()
    if (nrow(current_points) > 0) {
      p <- p %>%
        add_trace(data = current_points, x = ~TimeStamp, y = ~Depth,
                  type = 'scatter', mode = 'markers+text',
                  text = ~Etape, textposition = 'top right',
                  textfont = list(color = 'red'),
                  marker = list(color = 'red', size = 10), showlegend = FALSE)
    }
    
    # Ajout des barres verticales sur le graphique
    click_data <- event_data("plotly_click")
    if (nrow(current_points) > 0 & !is.null(click_data)) {
      p <- p %>%
        add_segments(x = current_points$TimeStamp, xend = current_points$TimeStamp,
                     y = max(df2$Depth, na.rm = TRUE) * -1, yend = 0,
                     line = list(color = 'red', width = 2), showlegend = FALSE)
    }
    
    p
  })
  
  # Tableau qui affiche les points sélectionnés
  output$selected_points <- renderTable({
    # On recupere le tableau reactif
    selected_points <- selected_points()
    # On modifie les noms des colonnes
    colnames(selected_points) <- c("Etape", "Nom", "Jour et heure", "Profondeur (m)")
    # On retourne le tableau
    selected_points
  })
  
  # Tableau qui calcule les métriques qui nous intéressent
  output$calculated_table <- renderTable({
    calculated_data <- calculated_data()
    colnames(calculated_data) <- c("Métrique", "Profondeur (m)")
    calculated_data
  })
  
  ## Boutons ----
  
  # Activation des boutons de téléchargement sous condition
  # Au démarrage, les deux boutons de téléchargement sont désactivés
  shinyjs::disable("download_data")
  shinyjs::disable("download_plot")
  
  # Si fichier d'entrée, on active les deux boutons
  observe({
    if (!is.null(input$file1)) {
      shinyjs::enable("download_data")
      shinyjs::enable("download_plot")
    } else {
      shinyjs::disable("download_data")
      shinyjs::disable("download_plot")
    }
  })
  
  # Téléchargement des données au format .txt
  output$download_data <- downloadHandler(
    
    filename = function() {
      paste0(gsub(".csv", "", input$file1$name),
             "_tdr-profile-analyzer.txt")
    },
    
    content = function(file) {
      # Annule le téléchargement si aucun fichier n'est sélectionné
      if (is.null(input$file1)) {
        return(NULL)
      }
      
      # # On définit le chemin ou on veut enregistrer le document final
      # save_path <- file.path("output", paste0(gsub(".csv", "", input$file1$name), "_tdr-profile-analyzer.txt"))
      
      # Ouvrir le fichier pour écrire
      # con <- file(save_path, open = "wt")
      con <- file(file, open = "wt")
      
      # Récupérer les données des tables
      selected <- selected_points()
      calculated <- calculated_data()
      
      # On modifie les tables pour avoir un bel alignement sur le fichier .txt de sortie
      # selected
      colnames(selected)[2] <- paste0(colnames(selected)[2], ".................")
      colnames(selected)[3] <- paste0(colnames(selected)[3], ".......")
      selected[1, 1] <- paste0(selected[1, 1], "....")
      selected[2, 1] <- paste0(selected[2, 1], "....")
      selected[2, 2] <- paste0(selected[2, 2], "......")
      selected[3, 1] <- paste0(selected[3, 1], "....")
      selected[3, 2] <- paste0(selected[3, 2], "........")
      selected[4, 1] <- paste0(selected[4, 1], "....")
      selected[4, 2] <- paste0(selected[4, 2], "..")
      # calculated
      colnames(calculated)[1] <- paste0(colnames(calculated)[1], "................................................")
      calculated[1, 1] <- paste0(calculated[1, 1], "........................")
      calculated[2, 1] <- paste0(calculated[2, 1], "..........................")
      calculated[3, 1] <- paste0(calculated[3, 1], ".........")
      calculated[4, 1] <- paste0(calculated[4, 1], ".........")
      calculated[5, 1] <- paste0(calculated[5, 1], "......")
      calculated[6, 1] <- paste0(calculated[6, 1], "......")
      
      # On arrondit les valeurs des données clé à 1 décimale
      calculated[,2] = round(calculated[,2], 1)
      
      # Récupérer le code FAO du potentiel individu capturé
      capture_FAO <- input$capture_FAO
      
      # Récupérer le modèle du TDR
      tdrmodel <- "NKE WiSens TD1000"
      
      # Récupérer le numéro de série
      req(input$file1)
      file_name <- input$file1$name
      numerodeserie <- sub("_.*", "", file_name)
      
      # Ecrire dans le fichier
      writeLines("Metadata :\n", con)
      writeLines(paste("Modèle du TDR :", tdrmodel), con)
      writeLines(paste("Numéro de série :", numerodeserie), con)
      writeLines("\n\n", con)
      writeLines("Horodatage :\n", con)
      write.table(selected, con, sep = "\t", row.names = FALSE, col.names = TRUE)
      writeLines("\n\n", con)
      writeLines("Données clés :\n", con)
      write.table(calculated, con, sep = "\t", row.names = FALSE, col.names = TRUE)
      writeLines("\n\n", con)
      writeLines(paste("Espèce(s) capturée(s) :", capture_FAO), con)
      
      # Fermer la connexion
      close(con)
    }
  )
  
  # Téléchargement de la figure au format .png
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0(gsub(".csv", "", input$file1$name),
             "_tdr-profile-analyzer.png")
    },
    
    content = function(file) {
      # # On définit le chemin ou on veut enregistrer le graphique final
      # save_path <- file.path("output", paste0(gsub(".csv", "", input$file1$name), "_tdr-profile-analyzer.png"))
      
      # On sauvegarde en passant par un fichier HTML temporaire
      temp_html <- tempfile(fileext = ".html")
      
      # Générer le graphique
      req(input$file1)
      df1 <- stage1(input)
      df2 <- stage2(df1)
      p <- plot_ly(data = df2, x = ~TimeStamp, y = ~Depth * -1,
                   type = 'scatter', mode = 'lines') %>%
        layout(
          title = list(text = gsub(".csv", "", input$file1$name)),
          xaxis = list(title = "Jour et heure", tickformat = "%y-%m-%d %H:%M"),
          yaxis = list(title = "Profondeur (m)", range = c(max(df2$Depth, na.rm = TRUE) * -1, 0)),
          showlegend = FALSE
        )
      current_points <- selected_points()
      if (nrow(current_points) > 0) {
        p <- p %>%
          add_trace(data = current_points, x = ~TimeStamp, y = ~Depth,
                    type = 'scatter', mode = 'markers+text',
                    text = ~Etape, textposition = 'top right',
                    textfont = list(color = 'red'),
                    marker = list(color = 'red', size = 10), showlegend = FALSE)
      }
      click_data <- event_data("plotly_click")
      if (nrow(current_points) > 0 & !is.null(click_data)) {
        p <- p %>%
          add_segments(x = current_points$TimeStamp, xend = current_points$TimeStamp,
                       y = max(df2$Depth, na.rm = TRUE) * -1, yend = 0,
                       line = list(color = 'red', width = 2), showlegend = FALSE)
      }
      
      # Sauvegarder le graphique dans un fichier HTML temporaire
      htmlwidgets::saveWidget(p, temp_html, selfcontained = TRUE)
      # Utiliser webshot pour créer une image PNG à partir du fichier HTML
      # webshot::webshot(temp_html, file = save_path, vwidth = 1200, vheight = 600)
      webshot::webshot(temp_html, file, vwidth = 1200, vheight = 600)
    }
  )
  
  # Bouton pour calculer les données clés
  observeEvent(input$calculate, {
    current_points <- selected_points()
    
    if (nrow(current_points) == 4) {
      # L'utilisateur a bien sélectionné les 4 points requis
      
      # - Profondeur en début de pêche : B
      fishing_beginning_depth <- abs(current_points[current_points$Etape == "B", "Depth"])
      
      # - Profondeur en fin de pêche : C
      fishing_end_depth <- abs(current_points[current_points$Etape == "C", "Depth"])
      
      # - Profondeur minimale de peche : min entre B et C
      # On extrait les heures des points B et C
      time_B <- current_points$TimeStamp[current_points$Etape == "B"]
      time_C <- current_points$TimeStamp[current_points$Etape == "C"]
      # On filtre les lignes de df2 entre les heures de B et C
      df1 <- stage1(input)
      df2 <- stage2(df1)
      filtered_data_BC <- df2 %>%
        filter(TimeStamp >= time_B & TimeStamp <= time_C)
      # On calcule le min de Depth pour ces lignes
      # (ce sont des valeurs absolues donc > 0)
      fishing_min_depth <- min(filtered_data_BC$Depth, na.rm = TRUE)
      
      # - Profondeur maximale de peche : max entre B et C
      fishing_max_depth <- max(filtered_data_BC$Depth, na.rm = TRUE)
      
      # - Profondeur moyenne de peche : mean entre B et C
      fishing_mean_depth <- mean(filtered_data_BC$Depth, na.rm = TRUE)
      
      # - Profondeur mediane de peche : med entre B et C
      fishing_med_depth <- median(filtered_data_BC$Depth, na.rm = TRUE)
      
      # - Profondeur moyenne de deploiement : mean entre A et D
      # On extrait les heures des points A et D
      time_A <- current_points$TimeStamp[current_points$Etape == "A"]
      time_D <- current_points$TimeStamp[current_points$Etape == "D"]
      # On filtre les lignes de df2 entre les heures de A et D
      filtered_data_AD <- df2 %>%
        filter(TimeStamp >= time_A & TimeStamp <= time_D)
      # On calcule la mean de Depth pour ces lignes
      deployment_mean_depth <- mean(filtered_data_AD$Depth, na.rm = TRUE)
      
      # - Profondeur mediane de deploiement : med entre A et D
      deployment_med_depth <- median(filtered_data_AD$Depth, na.rm = TRUE)
      
      # On crée un tableau avec les résultats des calculs
      result <- data.frame(
        Metrique = c("Profondeur en début de pêche [B]",
                     "Profondeur en fin de pêche [C]",
                     "Profondeur minimale de pêche [min entre B et C]",
                     "Profondeur maximale de pêche [max entre B et C]",
                     "Profondeur moyenne de pêche [moyenne entre B et C]",
                     "Profondeur médiane de pêche [mediane entre B et C]",
                     "Profondeur moyenne de déploiement [moyenne entre A et D]",
                     "Profondeur médiane de déploiement [mediane entre A et D]"),
        Profondeur = c(fishing_beginning_depth,
                       fishing_end_depth,
                       fishing_min_depth,
                       fishing_max_depth,
                       fishing_mean_depth,
                       fishing_med_depth,
                       deployment_mean_depth,
                       deployment_med_depth)
      )
      
      # On met à jour le tableau calculé
      calculated_data(result)
      
    } else {
      # L'utilisateur n'a pas sélectionné les 4 points requis
      
      # Alerte
      shinyalert::shinyalert(
        title = "Points nécessaires non sélectionnés",
        text = "Veuillez sélectionner les points demandés avant de calculer les données.",
        type = "warning",
        confirmButtonText = "Ok"
      )
      # On réinitialise le tableau calculé
      calculated_data(data.frame(Metrique = character(0),
                                 Profondeur = numeric(0)))
      
    }
  })
}
