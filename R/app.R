## 1. Packages ----

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


## 2. Lancer l'app Shiny ----

# Charger les fichiers ui et server
source("R/ui.R")
source("R/server.R")

# Lancer l'application
shinyApp(ui = ui, server = server)


## 3. Reste à faire ----

# Bonnes pratiques : où mettre les librairies ? Suppr app.R

# Download : me demander le chemin du dossier ou enregistrer

# Axe des abscisses sur la figure : Date Heure et pas seulement Heure

# Mettre les logos : IRD et Ob7
# Mettre les mentions de copyright IRD Ob7


# Big test de toutes les manips possibles pour détecter les bugs

# Récuperer le modele du TDR dans les metadata du fichier

# A et D : pas besoin de les placer à la main, ça peut être min(timeStamp) et max(timeStamp)

# Rendre l'app autonome, standalone
