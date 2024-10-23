## 1. Packages ----

library(shiny)
library(shinythemes)
library(plotly)
library(dplyr)
library(shinyalert)
library(shinydashboard)
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

# .txt de sortie
# Ajouter le modele, le numéro de série

# Apparence .txt sortie
# Aligner les colonnes (avec des ... : dans une table qu'on a dupliqué juste avant de save)
# Idem pour les colnames
# Arrondir les métriques à 1 décimale

# Cacher les boutons de téléchargement si pas de fichier d'entrée

# Condition à la lecture du fichier :
# Si le champ Depth existe ok
# S'il n'existe pas on le crée : Pression * 10, arrondir à 1 chiffre après la virgule

# Download : me demander le chemin du dossier ou enregistrer

# Mettre les logos : IRD et Ob7
# Mettre les mentions de copyright IRD Ob7

# Axe des abscisses sur la figure : Date Heure et pas seulement Heure

# Big test de toutes les manips possibles pour détecter les bugs

# Récuperer le modele du TDR dans les metadata du fichier

# Vérifier : si je change le fichier d'entrée est-ce que ça clean bien les tables / adapte bien la figure ?
