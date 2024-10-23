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

# Mettre les logos : IRD et Ob7
# Mettre les mentions de copyright IRD Ob7

# Download figure : me demander le chemin du dossier ou enregistrer

# Mettre un petit champ :
# "S'il y a eu une capture, inscrire le code FAO de l'espèce"
# et bien mettre cette info dans le .txt de sortie

# Bouton download, fichier de sortie :
# .txt qui a toutes ces infos, celles des deux tables, assez lisible
# Premiere section : Horodatage
# Deuxieme section : Données clés
# Nom : le meme que le nom d'entree avec derriere _tdr-profile-analyzer

# Forcer les positions des lettres par rapport aux points

# Axe des abscisses sur la figure : Date Heure et pas seulement Heure

# Design : bloc 1 et 3 à gauche en largeur 4 et bloc 2 à droite en largeur 8 ?

# Big test de toutes les configs possibles

# Récuperer le modele du TDR (metadata du file)

# Vérifier : si je change le fichier d'entrée est-ce que ça clean bien les tables / adapte bien la figure
