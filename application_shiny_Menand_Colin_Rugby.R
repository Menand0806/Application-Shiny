setwd("~/Colin/Incription DU data/Cours/Oral final/Application Shiny evaluation")
library(shiny)
library(tidyverse)
library(readr)
library(readxl)
library(DT)
library(plotly)
library(ggimage)


### Partie 1 : Chargement des données du championnat 

fichier <-"Rapport_Competition_ProD2_2024-2025.xlsx"

# Récupérer les noms des feuilles du fichier Excel en cours d'extraction
noms_feuilles <- excel_sheets(fichier)

# Boucle pour récupérer toutes les feuilles 
for (nom in noms_feuilles) {
  # Enregistrer le nom de l'équipe dans la variable nom_feuille 
  nom_feuille <- nom  
  
  # Supprimer les espaces début/fin avec la fct str_trim pour enlever des erreurs
  nom <- str_trim(nom_feuille)
  
  #Importer les données de l'équipe dans la variable donnees
  donnees <- read_excel(fichier, sheet = nom_feuille)
  
  
  # Condition qui permet si on lance deux fois la boucle de ne pas dupliquer les données
  if (exists(nom)) { # Si la variable nom existe dans l’environnement R
    donnees_existantes <- get(nom) #On récupère dans donnees_existantes ses donnees
    donnees <- bind_rows(donnees_existantes, donnees) |> distinct() #distinct() supprime les lignes identiques en double pour garder seulement les données uniques
  }
  
  # Créer une variable avec le nom de la feuille = le nom de la variable
  assign(nom, donnees) 
  
}




###

###Partie 2 : Visualisation de la page principale
# Définir l'interface utilisateur UI (User Interface) #C'est le visuel 
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    /* Paramètres de la colonne contenant le logo */
      .logo-col {
        display: flex;
        justify-content: center;
        align-items: center;
        background-color: white;
        height: 100vh;
      }
      /* Dimension du logo respectant la taille de la colonne */
      .logo-img {
        max-width: 100%;
        max-height: 90vh;
      }
      .content-col {
        padding: 20px;
        color: black;
      }
      /* Paramètres du titre principal (h1) */
      h1 {
        font-size: 3.5em;
        margin-top: 10px;
        text-align: center;
        font-weight: bold;
      }
       /* Paramètres des sous-titres (h2)*/
      h2 {
        text-align: center;
      }
    "))
  ),
  # Mise en page en ligne avec deux colonnes
  fluidRow(
    column(
      width = 8, #taille de la première colonne
      class = "content-col",
      # Titre principal
      h1("Saison 2024-2025 ProD2"),
      
      # Texte d'aide au-dessus des onglets
      helpText("Menu principal"),
      
      # Sous-titre dynamique affichant la sélection
      h2(textOutput("selection catégorie")),
      
      # Panneau à onglets avec différents tableaux et graphiques
      tabsetPanel(type = "tabs",
                  tabPanel("Classement", 
                           DT::dataTableOutput("table_classement")),
                  tabPanel("Points marqués", plotlyOutput("graphique_ptsM_vs_ptsE"),
                           plotOutput("graphique_bars_empilees_marqués_temps"),
                           plotOutput("graphique_pts_domicile_vs_exterieur"),
                           plotOutput("graphique_bars_empilees_marqués_origine")),
                  tabPanel("Points encaissés",
                           plotOutput("graphique_bars_empilees_encaissés_temps"),
                           plotOutput("graphique_pts_encaissés_domicile_vs_exterieur")),
                  tabPanel("Possession",
                           plotOutput("graphique_bars_empilees_possession_temps"),
                           plotOutput("graphique_bars_empilees_possession_zone")),
                  tabPanel("Essais",
                           
                           h3("Essais marqués"),
                           plotOutput("graphique_bars_empilees_essais_marques_type",   height = "350px"),
                           plotOutput("graphique_bars_empilees_essais_marques_temps",  height = "350px"),
                           plotOutput("graphique_barres_moy_phases_essai",             height = "350px"),
                           
                           tags$hr(style = "margin-top:30px; margin-bottom:30px;"),
                           
                           h3("Essais encaissés"),
                           plotOutput("graphique_bars_empilees_essais_encaisses_type", height = "350px"),
                           plotOutput("graphique_bars_empilees_essais_encaisses_temps",height = "350px")
                  ),
                  tabPanel("Touches",     
                           plotOutput("graphique_touches_gagnees_vs_positives"),
                           plotOutput("graphique_barres_touches_jouees_vites")),
                  tabPanel("Mêlées",     
                           plotOutput("graphique_barres_melees_gagnees"),
                           plotOutput("graphique_barres_melees_refaire"),
                           plotOutput("graphique_barres_melees_penalites_gagnees")),
                  tabPanel("Jeux au pied",
                           plotOutput("graphique_barres_distance_pied"),
                           plotOutput("graphique_barres_distance_pied_div_nbpied"),
                           plotOutput("graphique_barres_50_22"),
                           plotOutput("graphique_touches_positives_neutre_negatif")),
                  tabPanel("Duels aériens",
                           plotOutput("graphique_barres_duels_aeriens_offensifs_gagnes"),
                           plotOutput("graphique_barres_duels_aeriens_defensifs_gagnes")),
                  tabPanel("Tirs au but",
                           plotOutput("graphique_barres_tir_au_but"),
                           plotOutput("graphique_barres_tir_au_but_penalites"),
                           plotOutput("graphique_barres_tir_au_but_transformations"),
                           plotOutput("graphique_barres_tir_au_but_drops")),
                  tabPanel("Rucks",
                           plotOutput("graphique_barres_rucks")),
                  tabPanel("Attaque",
                           fluidRow(
                             column(5, 
                                    h3("Classement Passes"),
                                    DTOutput("table_classement_passes")
                             ),
                             column(5, 
                                    h3("Classement Offloads"),
                                    DTOutput("table_classement_offloads")
                             )
                           ),
                           fluidRow(
                             column(5, 
                                    h3("Classement Franchissements"),
                                    DTOutput("table_classement_franchissements")
                             ),
                             column(5, 
                                    h3("Classement Défenseurs battus"),
                                    DTOutput("table_classement_defenseurs_battus")
                             )
                           ),
                           fluidRow(
                             column(5, 
                                    h3("Classement Mètres Parcourus"),
                                    DTOutput("table_classement_metres_parcourus")
                             ),
                             column(5,
                                     h3("Classement Ballons Perdus"),
                                     DTOutput("table_classement_ballons_perdus")
                             )
                           ),
                           fluidRow(
                             column(5, 
                                    h3("Classement En avant"),
                                    DTOutput("table_classement_en_avant")
                             ),
                             column(5,
                                    h3("Classement Contacts"),
                                    DTOutput("table_classement_contacts")
                  )
      )),
      tabPanel("Défense",
               fluidRow(
                 column(5, 
                        h3("Classement Plaquages réussis %"),
                        DTOutput("table_classement_plaquages_reussis")
                 ),
                 column(5, 
                        h3("Classement Plaquages avancés"),
                        DTOutput("table_classement_plaquages_avances")
                 )
               ),
               fluidRow(
                 column(5, 
                        h3("Classement Constests positifs %"),
                        DTOutput("table_classement_contests_positifs")
                 ),
                 column(5, 
                        h3("Classement Contre-rucks positifs"),
                        DTOutput("table_classement_contrerucks_positifs")
                 )
               ),
               fluidRow(
                 column(5, 
                        h3("Classement Turnovers"),
                        DTOutput("table_classement_turnovers")
                 ),
                 column(5,
                        h3("Classement Franchissements subis"),
                        DTOutput("table_classement_franchissements_subis")
                 )
               )))
    ),
    #Colonne secondaire contenant le logo
    column(
      width = 4,
      class = "logo-col",
      #Image logo chargée depuis le dossier www ou le même répertoire
      img(src = "US Montauban.png", class = "logo-img")
    )
  )
)

###

### Partie 3 : Créer les visuels graphique 
server <- function(input, output, session) {
  ##Classement
  #Table Classement ProD2
  output$table_classement <- DT::renderDataTable({
    req(Classement)   # vérifie que ta variable Classement existe
    DT::datatable(Classement, options = list(pageLength = 16, scrollX = TRUE),
                  ,width = '200px')
  })
  
  ##
  ##Points Marqués ##
  
  #Graphique Point encaissés vs points marqués
  output$graphique_ptsM_vs_ptsE <- renderPlotly({
    req(Classement)
    
    moyenne_ptsM <- mean(Classement$`Points Marqués`, na.rm = TRUE)
    moyenne_ptsE <- mean(Classement$`Points Encaissés`, na.rm = TRUE)
    
    p <- ggplot(Classement, aes(
      x = `Points Marqués`,
      y = `Points Encaissés`
    )) +
      geom_point(size = 4, color = "steelblue") +
      geom_text(
        aes(label = `Nom de l'équipe`),
        vjust = -0.7,
        size = 3
      ) +  # étiquette au-dessus
      scale_y_reverse() +
      geom_vline(xintercept = moyenne_ptsM, linetype = "dashed", color = "red") +
      geom_hline(yintercept = moyenne_ptsE, linetype = "dashed", color = "red") +
      labs(
        title = "Points marqués vs encaissés (Pro D2)",
        subtitle = paste0("Moyennes : marqués = ",
                          round(moyenne_ptsM, 1),
                          " | encaissés = ",
                          round(moyenne_ptsE, 1)),
        x = "Points marqués",
        y = "Points encaissés (axe inversé)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5)
      )
    
    ggplotly(p)
  })
  
  #Graphique points marqués en fonction du temps 
  output$graphique_bars_empilees_marqués_temps <- renderPlot({
    req(`Points Marqués_inverse`) 
    
    ggplot(`Points Marqués_inverse`,
           aes(x = Equipes,
               y = valeur,
               fill = `Points Marqués %`)) +
      
      geom_bar(stat = "identity", position = "stack") +
      
      geom_text(aes(label = paste0(round(valeur, 1), "%")),
                position = position_stack(vjust = 0.5),
                color = "white", size = 3) +
      
      scale_y_continuous(limits = c(0, 101), expand = c(0, 0)) +
      
      labs(title = "Répartition des points marqués par période (100%)",
           x = "Équipe",
           y = "Répartition des points marqués (%)") +
      
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank()
      )
  })
  
  #points domicile vs extérieur 
  output$graphique_pts_domicile_vs_exterieur <- renderPlot({
    req(`Points Marqués`)
    
    ggplot(`Points Marqués`, aes(x = Equipes)) +
      geom_col(aes(y = `Points Marqués - Domicile / Match`, fill = "Domicile"),
               position = position_nudge(x = -0.15), width = 0.3) +
      geom_col(aes(y = `Points Marqués - Extérieur / Match`, fill = "Extérieur"),
               position = position_nudge(x = 0.15), width = 0.3) +
      
      geom_text(aes(y = `Points Marqués - Domicile / Match`, label = round(`Points Marqués - Domicile / Match`,1)),
                position = position_nudge(x = -0.15), vjust = -0.5, size = 3) +
      geom_text(aes(y = `Points Marqués - Extérieur / Match`, label = round(`Points Marqués - Extérieur / Match`,1)),
                position = position_nudge(x = 0.15), vjust = -0.5, size = 3) +
      
      scale_fill_manual(name = "Lieu du match", values = c("Domicile" = "steelblue", "Extérieur" = "orange")) +
      labs(title = "Points marqués - Domicile vs Extérieur",
           x = "Équipe",
           y = "Points par match") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  #Graphique points marqués en fonction de l'origines 
  output$graphique_bars_empilees_marqués_origine <- renderPlot({
    req(`Origines points_inverse`) 
    
    ggplot(`Origines points_inverse`,
           aes(x = Equipes,
               y = valeur,
               fill = Origine)) +
      
      geom_bar(stat = "identity", position = "stack") +
      
      geom_text(aes(label = paste0(round(valeur, 1), "%")),
                position = position_stack(vjust = 0.5),
                color = "white", size = 3) +
      
      scale_y_continuous(limits = c(0, 101), expand = c(0, 0)) +
      
      labs(title = "Répartition des points marqués par origines (100%)",
           x = "Équipe",
           y = "Répartition des points marqués (%)") +
      
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank()
      )
  })
  
  ##
  
  ##Points encaissés ##
  #Graphique points marqués en fonction du temps 
  output$graphique_bars_empilees_encaissés_temps <- renderPlot({
    req(`Points Encaissés_inverse`) 
    
    ggplot(`Points Encaissés_inverse`,
           aes(x = Equipes,
               y = valeur,
               fill = colonne)) +
      
      geom_bar(stat = "identity", position = "stack") +
      
      geom_text(aes(label = paste0(round(valeur, 1), "%")),
                position = position_stack(vjust = 0.5),
                color = "white", size = 3) +
      
      scale_y_continuous(limits = c(0, 101), expand = c(0, 0)) +
      
      labs(title = "Répartition des points encaissés par période (100%)",
           x = "Équipe",
           y = "Répartition des points encaissés (%)") +
      
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank()
      )
  })
  
  #points encaissés domicile vs extérieur 
  output$graphique_pts_encaissés_domicile_vs_exterieur <- renderPlot({
    req(`Points Encaissés`)
    
    ggplot(`Points Encaissés`, aes(x = Equipes)) +
      geom_col(aes(y = `Points Encaissés - Domicile / Match`, fill = "Domicile"),
               position = position_nudge(x = -0.15), width = 0.3) +
      geom_col(aes(y = `Points Encaissés - Extérieur / Match`, fill = "Extérieur"),
               position = position_nudge(x = 0.15), width = 0.3) +
      
      geom_text(aes(y = `Points Encaissés - Domicile / Match`, label = round(`Points Encaissés - Domicile / Match`,1)),
                position = position_nudge(x = -0.15), vjust = -0.5, size = 3) +
      geom_text(aes(y = `Points Encaissés - Extérieur / Match`, label = round(`Points Encaissés - Extérieur / Match`,1)),
                position = position_nudge(x = 0.15), vjust = -0.5, size = 3) +
      
      scale_fill_manual(name = "Lieu du match", values = c("Domicile" = "steelblue", "Extérieur" = "orange")) +
      labs(title = "Points Encaissés - Domicile vs Extérieur",
           x = "Équipe",
           y = "Points encaissés par match") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  ###
  
  ###Possession ###
  #Possession en fonction de la zone 
  output$graphique_bars_empilees_possession_zone <- renderPlot({
    req(Possession_inverse) 
    
    ggplot(Possession_inverse,
           aes(x = Equipes,
               y = valeur,
               fill = `%Possession`)) +
      
      geom_bar(stat = "identity", position = "stack") +
      
      geom_text(aes(label = paste0(round(valeur, 1), "%")),
                position = position_stack(vjust = 0.5),
                color = "white", size = 3) +
      
      scale_y_continuous(limits = c(0, 101), expand = c(0, 0)) +
      
      labs(title = "Répartition des possessions par zone terrain (100%)",
           x = "Équipe",
           y = "Répartition des possessions (%)") +
      
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank()
      )
  })
  
  #Possession par temps 
  output$graphique_bars_empilees_possession_temps <- renderPlot({
    req(Possession)   
    
    #Préparation des données
    donnees <- Possession |>
      select(
        Equipes,
        `% Possession - 0-30s`,
        `% Possession - 30-60s`,
        `% Possession - >60s`
      ) |>
      tidyr::pivot_longer(
        cols      = starts_with("% Possession"),
        names_to  = "%Possession",
        values_to = "valeur"
      ) |>
      mutate(
        `%Possession` = factor(
          `%Possession`,
          levels = c(
            "% Possession - 0-30s",
            "% Possession - 30-60s",
            "% Possession - >60s"
          ),
          labels = c("0‑30 s", "30‑60 s", "> 60 s") #renommer les variables
        )
      )
  
    #faire le graphique
    ggplot(donnees,
           aes(x = Equipes,
               y = valeur,
               fill = `%Possession`)) +
      
      geom_bar(stat = "identity", position = "stack") +
      
      geom_text(aes(label = paste0(round(valeur, 1), "%")),
                position = position_stack(vjust = 0.5),
                colour = "white", size = 3) +
      
      scale_y_continuous(limits = c(0, 101), expand = c(0, 0)) +
      
      labs(title = "Répartition des possessions par durée (100 %)",
           x = "Équipe",
           y = "Pourcentage de possession") +
      
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1),
        plot.title   = element_text(size = 18, face = "bold", hjust = .5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank()
      )
  })
  
  ##Essais ##
  #Essais marqués en fct du type 
  output$graphique_bars_empilees_essais_marques_type <- renderPlot({
    req(`Essais Marques_inverse`) 
    
    ggplot(`Essais Marques_inverse`,
           aes(x = Equipes,
               y = valeur,
               fill = `Essais Marques %`)) +
      
      geom_bar(stat = "identity", position = "stack") +
      
      geom_text(aes(label = paste0(round(valeur, 1), "%")),
                position = position_stack(vjust = 0.5),
                color = "white", size = 3) +
      
      scale_y_continuous(limits = c(0, 101), expand = c(0, 0)) +
      
      labs(title = "Répartition des essais marqués par origine (100%)",
           x = "Équipe",
           y = "Répartition des points essais (%)") +
      
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank()
      )
  })
  
  #Essais par temps
  output$graphique_bars_empilees_essais_marques_temps <- renderPlot({
    req(`Essais Marques`)  
    
    # 1. préparation
    donnees <- `Essais Marques` |>
      select(
        Equipes,
        `Essais Marques - 0-20 min %`,
        `Essais Marques - 20-40 min %`,
        `Essais Marques - 40-60 min %`,
        `Essais Marques - 60-80 min %`
      ) |>
      pivot_longer(
        cols      = starts_with("Essais Marques"),
        names_to  = "Temps",                        
        values_to = "valeur"
      ) |>
      mutate(
        Temps = factor(
          Temps,
          levels = c(
            "Essais Marques - 0-20 min %",
            "Essais Marques - 20-40 min %",
            "Essais Marques - 40-60 min %",
            "Essais Marques - 60-80 min %"
          ),
          labels = c("0‑20 min", "20‑40 min", "40‑60 min", "60‑80 min")
        )
      )
    
    # 2. graphique
    ggplot(donnees,
           aes(x = Equipes,
               y = valeur,
               fill = Temps)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = paste0(round(valeur, 1), "%")),
                position = position_stack(vjust = 0.5),
                colour = "white", size = 3) +
      scale_y_continuous(limits = c(0, 101), expand = c(0, 0)) +
      labs(title = "Répartition des essais marqués par tranche de temps (100 %)",
           x = "Équipe",
           y = "Pourcentage d'essais marqués") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1),
        plot.title   = element_text(size = 18, face = "bold", hjust = .5),
        legend.title = element_blank()
      )
  })
  
  #Essais marques en fonction de moyenne phase
  output$graphique_barres_moy_phases_essai <- renderPlot({
    req(`Essais Marques`)  
    
    ggplot(`Essais Marques`,
           aes(x = reorder(Equipes, `Essais - Moyenne de phases`),#tri croissant
               y = `Essais - Moyenne de phases`,
               fill = Equipes)) +#couleur unique par équipe
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(`Essais - Moyenne de phases`, 1)),
                vjust = -0.5,
                size = 3.5) +#valeur au-dessus
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +   
      labs(title = "Nombre moyen de phases par essai",
           x = "Équipe",
           y = "Moyenne de phases") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1),
        plot.title   = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.position = "none"  # inutile car Equipes = x
      )
  })
  
  #Essais encaisses par type
  output$graphique_bars_empilees_essais_encaisses_type <- renderPlot({
    req(`Essais Encaissés`)  
    
    # 1. préparation
    donnees <- `Essais Encaissés` |>
      select(
        Equipes,
        `Essais Encaissés - Maul %`,
        `Essais Encaissés - Bras cassé %`,
        `Essais Encaissés - Pénalité %`,
        `Essais Encaissés - Dans le jeu %`,
        `Essais Encaissés - Contre attaque %`,
        `Essais Encaissés - Turnovers %`,
        `Essais Encaissés - Essais de pénalité %`,
        `Essais Encaissés - Renvois Ligne de But %`,
        `Essais Encaissés - Renvois 22m %`,
        `Essais Encaissés - Coups d'envoi %`,
        `Essais Encaissés - Touche %`,
        `Essais Encaissés - Mêlée %`
      ) |>
      pivot_longer(
        cols      = starts_with("Essais Encaissés"),
        names_to  = "Type",                        
        values_to = "valeur"
      ) |>
      mutate(
        Temps = factor(
          Type,
          levels = c(
            "Essais Encaissés - Maul %",
            "Essais Encaissés - Bras cassé %",
            "Essais Encaissés - Pénalité %",
            "Essais Encaissés - Dans le jeu %",
            "Essais Encaissés - Contre attaque %",
            "Essais Encaissés - Turnovers %",
            "Essais Encaissés - Essais de pénalité %",
            "Essais Encaissés - Renvois Ligne de But %",
            "Essais Encaissés - Renvois 22m %",
            "Essais Encaissés - Coups d'envoi %",
            "Essais Encaissés - Touche %",
            "Essais Encaissés - Mêlée %"
          )
        )
      )
    
    # 2. graphique
    ggplot(donnees,
           aes(x = Equipes,
               y = valeur,
               fill = Type)) +  
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = paste0(round(valeur, 1), "%")),
                position = position_stack(vjust = 0.5),
                colour = "white", size = 3) +
      scale_y_continuous(limits = c(0, 101), expand = c(0, 0)) +
      labs(title = "Répartition des essais encaissés par type (100 %)",
           x = "Équipe",
           y = "Pourcentage d'essais encaissés") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1),
        plot.title   = element_text(size = 18, face = "bold", hjust = .5),
        legend.title = element_blank()
      )
  })
  
  #Essais encaissés par temps
  output$graphique_bars_empilees_essais_encaisses_temps <- renderPlot({
    req(`Essais Encaissés`)  
    
    # 1. préparation
    donnees <- `Essais Encaissés` |>
      select(
        Equipes,
        `Essais Encaissés - 0-20 min %`,
        `Essais Encaissés - 20-40 min %`,
        `Essais Encaissés - 40-60 min %`,
        `Essais Encaissés - 60-80 min %`
      ) |>
      pivot_longer(
        cols      = starts_with("Essais Encaissés"),
        names_to  = "Temps",                        
        values_to = "valeur"
      ) |>
      mutate(
        Temps = factor(
          Temps,
          levels = c(
            "Essais Encaissés - 0-20 min %",
            "Essais Encaissés - 20-40 min %",
            "Essais Encaissés - 40-60 min %",
            "Essais Encaissés - 60-80 min %"
          ),
          labels = c("0‑20 min", "20‑40 min", "40‑60 min", "60‑80 min")
        )
      )
    
    # 2. graphique
    ggplot(donnees,
           aes(x = Equipes,
               y = valeur,
               fill = Temps)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = paste0(round(valeur, 1), "%")),
                position = position_stack(vjust = 0.5),
                colour = "white", size = 3) +
      scale_y_continuous(limits = c(0, 101), expand = c(0, 0)) +
      labs(title = "Répartition des essais encaissés par tranche de temps (100 %)",
           x = "Équipe",
           y = "Pourcentage d'essais encaissés") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1),
        plot.title   = element_text(size = 18, face = "bold", hjust = .5),
        legend.title = element_blank()
      )
  })
  
  ### touches ###
  output$graphique_touches_gagnees_vs_positives <- renderPlot({
    req(Touches)
    
    ggplot(Touches, aes(x = Equipes)) +
      geom_col(aes(y = `Touches gagnées %`, fill = "Touches gagnées %"),
               position = position_nudge(x = -0.15), width = 0.3) +
      geom_col(aes(y = `Touches positives %`, fill = "Touches positives %"),
               position = position_nudge(x = 0.15), width = 0.3) +
      
      geom_text(aes(y = `Touches gagnées %`, label = round(`Touches gagnées %`,1)),
                position = position_nudge(x = -0.15), vjust = -0.5, size = 3) +
      geom_text(aes(y = `Touches positives %`, label = round(`Touches positives %`,1)),
                position = position_nudge(x = 0.15), vjust = -0.5, size = 3) +
      
      scale_fill_manual(name = "Resultats touches", values = c("Touches gagnées %" = "yellow", "Touches positives %" = "orange")) +
      labs(title = "Resultats touches : Touches gagnées % et Touches positives %",
           x = "Équipe",
           y = "Touches %") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  #Touches jouees vites
  output$graphique_barres_touches_jouees_vites <- renderPlot({
    req(`Touches`)
    
    ggplot(`Touches`,
           aes(x = reorder(Equipes, `Touches jouées vite`),#tri croissant
               y = `Touches jouées vite`,
               fill = Equipes)) + #couleur unique par équipe
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(`Touches jouées vite`, 1)),
                vjust = -0.5,
                size = 3.5) + # valeur au-dessus
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +   
      labs(title = "Nombre de touches jouées vite",
           x = "Équipe",
           y = "Nombres de touches") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1),
        plot.title   = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.position = "none"  # inutile car Equipes = x
      )
  })
  
  #### Mêlées ####
  #Mêlées gagnées %
  output$graphique_barres_melees_gagnees <- renderPlot({
    req(`Mêlées`)
    
    ggplot(`Mêlées`,
           aes(x = reorder(Equipes, `Mêlées gagnées %`),#tri croissant
               y = `Mêlées gagnées %`,
               fill = Equipes)) + #couleur unique par équipe
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(`Mêlées gagnées %`, 1)),
                vjust = -0.5,
                size = 3.5) + # valeur au-dessus
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
      coord_cartesian(ylim = c(75, 93)) +  
      labs(title = "Pourcentage de mêlées gagnées",
           x = "Équipe",
           y = "Mêlées gagnées %") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1),
        plot.title   = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.position = "none"  # inutile car Equipes = x
      )
  })
  
  #Mêlées à refaire
  output$graphique_barres_melees_refaire <- renderPlot({
    req(`Mêlées`)
    
    ggplot(`Mêlées`,
           aes(x = reorder(Equipes, `Mêlées à refaire`),#tri croissant
               y = `Mêlées à refaire`,
               fill = Equipes)) + #couleur unique par équipe
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(`Mêlées à refaire`, 1)),
                vjust = -0.5,
                size = 3.5) + # valeur au-dessus
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
      coord_cartesian(ylim = c(20, 55))  +
      labs(title = "Nombre de mêlées à refaire",
           x = "Équipe",
           y = "Nombre mêlées") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1),
        plot.title   = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.position = "none"  # inutile car Equipes = x
      )
  })
  
  #Mêlées avec pénalités
  output$graphique_barres_melees_penalites_gagnees <- renderPlot({
    req(`Mêlées`)
    
    ggplot(`Mêlées`,
           aes(x = reorder(Equipes, `Pénalités gagnées`),#tri croissant
               y = `Pénalités gagnées`,
               fill = Equipes)) + #couleur unique par équipe
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(`Pénalités gagnées`, 1)),
                vjust = -0.5,
                size = 3.5) + # valeur au-dessus
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +   
      labs(title = "Pénalités gagnées grâce à la mêlée",
           x = "Équipe",
           y = "Nombre pénalités") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1),
        plot.title   = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.position = "none"  # inutile car Equipes = x
      )
  })
  
  #####Jeux au pied #####
  #Distance jeux au pied
  output$graphique_barres_distance_pied <- renderPlot({
    req(`Jeux au pied`)
    
    ggplot(`Jeux au pied`,
           aes(x = reorder(Equipes, `Distance jeux au pied`),#tri croissant
               y = `Distance jeux au pied`,
               fill = Equipes)) + #couleur unique par équipe
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(`Distance jeux au pied`, 1)),
                vjust = -0.5,
                size = 3.5) + # valeur au-dessus
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
      coord_cartesian(ylim = c(20000, 33000))  +
      labs(title = "Distance jeux au pied (en m)",
           x = "Équipe",
           y = "Distance jeux au pied (en m)") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1),
        plot.title   = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.position = "none"  # inutile car Equipes = x
      )
  })
  
  #Distance jeux au pied / nb pied
  output$graphique_barres_distance_pied_div_nbpied <- renderPlot({
    req(`Jeux au pied`)
    
    ggplot(`Jeux au pied`,
           aes(x = reorder(Equipes, `Distance jeux au pied /Nb Jeux au pied`),#tri croissant
               y = `Distance jeux au pied /Nb Jeux au pied`,
               fill = Equipes)) + #couleur unique par équipe
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(`Distance jeux au pied /Nb Jeux au pied`, 1)),
                vjust = -0.5,
                size = 3.5) + # valeur au-dessus
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
      coord_cartesian(ylim = c(28, 40))  +
      labs(title = "Distance jeux au pied /Nb Jeux au pied (en m)",
           x = "Équipe",
           y = "Distance jeux au pied (en m)") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1),
        plot.title   = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.position = "none"  # inutile car Equipes = x
      )
  })
  
  #nombre de 50/22
  output$graphique_barres_50_22<- renderPlot({
    req(`Jeux au pied`)
    
    ggplot(`Jeux au pied`,
           aes(x = reorder(Equipes, `50/22`),#tri croissant
               y = `50/22`,
               fill = Equipes)) + #couleur unique par équipe
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(`50/22`, 1)),
                vjust = -0.5,
                size = 3.5) + # valeur au-dessus
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
      #coord_cartesian(ylim = c(20, 55))  +
      labs(title = "Nombre de 50/22",
           x = "Équipe",
           y = "Nombre de 50/22") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1),
        plot.title   = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.position = "none"  # inutile car Equipes = x
      )
  })
  
  ###jeux au pied positif vs neutre v negatif
  output$graphique_touches_positives_neutre_negatif <- renderPlot({
    req(`Jeux au pied`)
    
    ggplot(`Jeux au pied`, aes(x = Equipes)) +
      geom_col(aes(y = `Jeux au pied positifs`, fill = "Jeux au pied positifs"),
               position = position_nudge(x = -0.15), width = 0.3) +
      geom_col(aes(y = `Jeux au pied neutres`, fill = "Jeux au pied neutres"),
               position = position_nudge(x = 0.15), width = 0.3) +
      geom_col(aes(y = `Jeux au pied négatifs`, fill = "Jeux au pied négatifs"),
               position = position_nudge(x = 0.15), width = 0.3) +
      
      geom_text(aes(y = `Jeux au pied positifs`, label = round(`Jeux au pied positifs`,1)),
                position = position_nudge(x = -0.15), vjust = -0.5, size = 3) +
      geom_text(aes(y = `Jeux au pied neutres`, label = round(`Jeux au pied neutres`,1)),
                position = position_nudge(x = 0.15), vjust = -0.5, size = 3) +
      geom_text(aes(y = `Jeux au pied négatifs`, label = round(`Jeux au pied négatifs`,1)),
                position = position_nudge(x = 0.15), vjust = -0.5, size = 3) +
      
      scale_fill_manual(name = "Resultats touches", values = c("Jeux au pied positifs" = "green", "Jeux au pied neutres" = "orange", "Jeux au pied négatifs" = "red")) +
      labs(title = "Resultats jeux au pied : Jeux au pied positifs vs Jeux au pied neutres vs Jeux au pied négatifs",
           x = "Équipe",
           y = "Nombre de jeux au pied") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  ###Duels aériens ###
  #Duels aériens positifs offensifs
  output$graphique_barres_duels_aeriens_offensifs_gagnes <-renderPlot({
    req(`Duels aériens`)
    
    ggplot(`Duels aériens`,
           aes(x = reorder(Equipes, `Duels aériens offensifs gagnés %`),#tri croissant
               y = `Duels aériens offensifs gagnés %`,
               fill = Equipes)) + #couleur unique par équipe
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(`Duels aériens offensifs gagnés %`, 1)),
                vjust = -0.5,
                size = 3.5) + # valeur au-dessus
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
      coord_cartesian(ylim = c(20, 48))  +
      labs(title = "Duels aériens offensifs gagnés %",
           x = "Équipe",
           y = "Duels aériens offensifs gagnés %") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1),
        plot.title   = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.position = "none"  # inutile car Equipes = x
      )
  })
  
  #duels aériens défensif
  output$graphique_barres_duels_aeriens_defensifs_gagnes <-renderPlot({
    req(`Duels aériens`)
    
    ggplot(`Duels aériens`,
           aes(x = reorder(Equipes, `Duels aériens défensifs gagnés %`),#tri croissant
               y = `Duels aériens défensifs gagnés %`,
               fill = Equipes)) + #couleur unique par équipe
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(`Duels aériens défensifs gagnés %`, 1)),
                vjust = -0.5,
                size = 3.5) + # valeur au-dessus
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
      coord_cartesian(ylim = c(20, 57))  +
      labs(title = "Duels aériens défensifs gagnés %",
           x = "Équipe",
           y = "Duels aériens défensifs gagnés %") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1),
        plot.title   = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.position = "none"  # inutile car Equipes = x
      )
  })
  
  ####Tirs au but ####
  #Tirs aux buts (drops+transfromations+pénalités)
  output$graphique_barres_tir_au_but <-renderPlot({
    req(`Tirs au but`)
    
    ggplot(`Tirs au but`,
           aes(x = reorder(Equipes, `Tirs au but %`),#tri croissant
               y = `Tirs au but %`,
               fill = Equipes)) + #couleur unique par équipe
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(`Tirs au but %`, 1)),
                vjust = -0.5,
                size = 3.5) + # valeur au-dessus
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
      coord_cartesian(ylim = c(60, 82))  +
      labs(title = "Tirs au but % (Pénalités, Transformations, Drops)",
           x = "Équipe",
           y = "Tirs au but %") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1),
        plot.title   = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.position = "none"  # inutile car Equipes = x
      )
  })
  
  #Tirs aux buts pénalités
  output$graphique_barres_tir_au_but_penalites <-renderPlot({
    req(`Tirs au but`)
    
    ggplot(`Tirs au but`,
           aes(x = reorder(Equipes, `Pénalités %`),#tri croissant
               y = `Pénalités %`,
               fill = Equipes)) + #couleur unique par équipe
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(`Pénalités %`, 1)),
                vjust = -0.5,
                size = 3.5) + # valeur au-dessus
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
      coord_cartesian(ylim = c(67, 90))  +
      labs(title = "Pénalités réussies %",
           x = "Équipe",
           y = "Pénalités réussies %") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1),
        plot.title   = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.position = "none"  # inutile car Equipes = x
      )
  })
  
  #Tirs aux buts transformation
  output$graphique_barres_tir_au_but_transformations <-renderPlot({
    req(`Tirs au but`)
    
    ggplot(`Tirs au but`,
           aes(x = reorder(Equipes, `Transformations %`),#tri croissant
               y = `Transformations %`,
               fill = Equipes)) + #couleur unique par équipe
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(`Transformations %`, 1)),
                vjust = -0.5,
                size = 3.5) + # valeur au-dessus
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
      coord_cartesian(ylim = c(57, 87))  +
      labs(title = "Transformations réussies %",
           x = "Équipe",
           y = "Transformations réussies %") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1),
        plot.title   = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.position = "none"  # inutile car Equipes = x
      )
  })
  
  
  #Tirs aux buts drops
  output$graphique_barres_tir_au_but_drops <-renderPlot({
    req(`Tirs au but`)
    
    ggplot(`Tirs au but`, aes(x = Equipes)) +
      geom_col(aes(y = `Drops`, fill = "Drops"),
               position = position_nudge(x = -0.15), width = 0.3) +
      geom_col(aes(y = `Drops %`, fill = "Drops %"),
               position = position_nudge(x = 0.15), width = 0.3) +
      
      geom_text(aes(y = `Drops`, label = round(`Drops`,1)),
                position = position_nudge(x = -0.15), vjust = -0.5, size = 3) +
      geom_text(aes(y = `Drops %`, label = round(`Drops %`,1)),
                position = position_nudge(x = 0.15), vjust = -0.5, size = 3) +
      
      scale_fill_manual(name = "Resultats touches", values = c("Drops" = "yellow", "Drops %" = "orange")) +
      labs(title = "Nombre de drops vs et Drops %",
           x = "Équipe",
           y = "Drops") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  #Rucks
  output$graphique_barres_rucks <- renderPlot({
    req(Rucks)
    
    ggplot(Rucks, aes(x = Equipes)) +
      geom_col(aes(y = `Rucks 22m gagnés %`, fill = "Rucks 22m gagnés %"),
               position = position_nudge(x = -0.3), width = 0.2) +
      geom_col(aes(y = `Rucks 22m - 50m gagnés %`, fill = "Rucks 22m - 50m gagnés %"),
               position = position_nudge(x = -0.1), width = 0.2) +
      geom_col(aes(y = `Rucks 22m - 50m Adv. gagnés %`, fill = "Rucks 22m - 50m Adv. gagnés %"),
               position = position_nudge(x = 0.1), width = 0.2) +
      geom_col(aes(y = `Rucks 22m Adv. gagnés %`, fill = "Rucks 22m Adv. gagnés %"),
               position = position_nudge(x = 0.3), width = 0.2) +
      
      geom_text(aes(y = `Rucks 22m gagnés %`, label = round(`Rucks 22m gagnés %`,1)),
                position = position_nudge(x = -0.3), vjust = -0.5, size = 3) +
      geom_text(aes(y = `Rucks 22m - 50m gagnés %`, label = round(`Rucks 22m - 50m gagnés %`,1)),
                position = position_nudge(x = -0.1), vjust = -0.5, size = 3) +
      geom_text(aes(y = `Rucks 22m - 50m Adv. gagnés %`, label = round(`Rucks 22m - 50m Adv. gagnés %`,1)),
                position = position_nudge(x = 0.1), vjust = -0.5, size = 3) +
      geom_text(aes(y = `Rucks 22m Adv. gagnés %`, label = round(`Rucks 22m Adv. gagnés %`,1)),
                position = position_nudge(x = 0.3), vjust = -0.5, size = 3) +
      
      scale_fill_manual(
        name = "Résultats Rucks",
        values = c(
          "Rucks 22m gagnés %" = "blue",
          "Rucks 22m - 50m gagnés %" = "green",
          "Rucks 22m - 50m Adv. gagnés %" = "yellow",
          "Rucks 22m Adv. gagnés %" = "orange"
        )
      ) +coord_cartesian(ylim = c(90, 100))  +
      labs(title = "Rucks gagnés en fonction des zones",
           x = "Équipe",
           y = "Rucks %") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "bottom")
  })
  
  
  ####Attaques ####
  #Passes
  output$table_classement_passes <- DT::renderDataTable({
    req(Attaque)
    
    Classement_mod <- Attaque %>%
      dplyr::arrange(desc(Passes)) %>% #tri par Points décroissants
      dplyr::mutate(Classement = row_number()) %>%#colonne Rang 1 à 16
      dplyr::select(Classement, Equipes, Passes)#sélectionner colonnes Classement, Equipes + valeur
    
    DT::datatable(
      Classement_mod,
      options = list(pageLength = 17, scrollX = TRUE,searching = FALSE,lengthChange = FALSE,dom = "t"),
      rownames = FALSE
    )
  })
  
  
  #Offloads
  output$table_classement_offloads <- DT::renderDataTable({
    req(Attaque)
    
    Classement_mod <- Attaque %>%
      dplyr::arrange(desc(Offloads)) %>% #tri par Points décroissants
      dplyr::mutate(Classement = row_number()) %>%#colonne Rang 1 à 16
      dplyr::select(Classement, Equipes, Offloads)#sélectionner colonnes Classement, Equipes + valeur
    
    DT::datatable(
      Classement_mod,
      options = list(pageLength = 17, scrollX = TRUE,searching = FALSE,lengthChange = FALSE,dom = "t"),
      rownames = FALSE
    )
  })
  
  #Franchissements
  output$table_classement_franchissements <- DT::renderDataTable({
    req(Attaque)
    
    Classement_mod <- Attaque %>%
      dplyr::arrange(desc(Franchissements)) %>% #tri par Points décroissants
      dplyr::mutate(Classement = row_number()) %>%#colonne Rang 1 à 16
      dplyr::select(Classement, Equipes, Franchissements)#sélectionner colonnes Classement, Equipes + valeur
    
    DT::datatable(
      Classement_mod,
      options = list(pageLength = 17, scrollX = TRUE,searching = FALSE,lengthChange = FALSE,dom = "t"),
      rownames = FALSE
    )
  })
  
  #Défenseurs battus
  output$table_classement_defenseurs_battus <- DT::renderDataTable({
    req(Attaque)
    
    Classement_mod <- Attaque %>%
      dplyr::arrange(desc(`Défenseurs battus`)) %>% #tri par Points décroissants
      dplyr::mutate(Classement = row_number()) %>%#colonne Rang 1 à 16
      dplyr::select(Classement, Equipes, `Défenseurs battus`)#sélectionner colonnes Classement, Equipes + valeur
    
    DT::datatable(
      Classement_mod,
      options = list(pageLength = 17, scrollX = TRUE,searching = FALSE,lengthChange = FALSE,dom = "t"),
      rownames = FALSE
    )
  })
  
  #Mètres parcourus
  output$table_classement_metres_parcourus <- DT::renderDataTable({
    req(Attaque)
    
    Classement_mod <- Attaque %>%
      dplyr::arrange(desc(`Mètres parcourus`)) %>% #tri par Points décroissants
      dplyr::mutate(Classement = row_number()) %>%#colonne Rang 1 à 16
      dplyr::select(Classement, Equipes, `Mètres parcourus`)#sélectionner colonnes Classement, Equipes + valeur
    
    DT::datatable(
      Classement_mod,
      options = list(pageLength = 17, scrollX = TRUE,searching = FALSE,lengthChange = FALSE,dom = "t"),
      rownames = FALSE
    )
  })
  
  #Ballons perdus
  output$table_classement_ballons_perdus <- DT::renderDataTable({
    req(Attaque)
    
    Classement_mod <- Attaque %>%
      dplyr::arrange(desc(`Ballons perdus`)) %>% #tri par Points décroissants
      dplyr::mutate(Classement = row_number()) %>%#colonne Rang 1 à 16
      dplyr::select(Classement, Equipes, `Ballons perdus`)#sélectionner colonnes Classement, Equipes + valeur
    
    DT::datatable(
      Classement_mod,
      options = list(pageLength = 17, scrollX = TRUE,searching = FALSE,lengthChange = FALSE,dom = "t"),
      rownames = FALSE
    )
  })
  
  #En avant
  output$table_classement_en_avant <- DT::renderDataTable({
    req(Attaque)
    
    Classement_mod <- Attaque %>%
      dplyr::arrange(desc(`En avants`)) %>% #tri par Points décroissants
      dplyr::mutate(Classement = row_number()) %>%#colonne Rang 1 à 16
      dplyr::select(Classement, Equipes, `En avants`)#sélectionner colonnes Classement, Equipes + valeur
    
    DT::datatable(
      Classement_mod,
      options = list(pageLength = 17, scrollX = TRUE,searching = FALSE,lengthChange = FALSE,dom = "t"),
      rownames = FALSE
    )
  })
  
  #Contacts
  output$table_classement_contacts <- DT::renderDataTable({
    req(Attaque)
    
    Classement_mod <- Attaque %>%
      dplyr::arrange(desc(`Contacts`)) %>% #tri par Points décroissants
      dplyr::mutate(Classement = row_number()) %>%#colonne Rang 1 à 16
      dplyr::select(Classement, Equipes, `Contacts`)#sélectionner colonnes Classement, Equipes + valeur
    
    DT::datatable(
      Classement_mod,
      options = list(pageLength = 17, scrollX = TRUE,searching = FALSE,lengthChange = FALSE,dom = "t"),
      rownames = FALSE
    )
  })
  
  #####Défense
  
  #plaquages réussis
  output$table_classement_plaquages_reussis <- DT::renderDataTable({
    req(Défense)
    
    Classement_mod <- Défense %>%
      dplyr::arrange(desc(`Plaquages réussis %`)) %>% #tri par Points décroissants
      dplyr::mutate(Classement = row_number()) %>%#colonne Rang 1 à 16
      dplyr::select(Classement, Equipes, `Plaquages réussis %`)#sélectionner colonnes Classement, Equipes + valeur
    
    DT::datatable(
      Classement_mod,
      options = list(pageLength = 17, scrollX = TRUE,searching = FALSE,lengthChange = FALSE,dom = "t"),
      rownames = FALSE
    )
  })
  
  #plaquages avancés
  output$table_classement_plaquages_avances <- DT::renderDataTable({
    req(Défense)
    
    Classement_mod <- Défense %>%
      dplyr::arrange(desc(`Plaquages avancés`)) %>% #tri par Points décroissants
      dplyr::mutate(Classement = row_number()) %>%#colonne Rang 1 à 16
      dplyr::select(Classement, Equipes, `Plaquages avancés`)#sélectionner colonnes Classement, Equipes + valeur
    
    DT::datatable(
      Classement_mod,
      options = list(pageLength = 17, scrollX = TRUE,searching = FALSE,lengthChange = FALSE,dom = "t"),
      rownames = FALSE
    )
  })
  
  #contests positif
  output$table_classement_contests_positifs <- DT::renderDataTable({
    req(Défense)
    
    Classement_mod <- Défense %>%
      dplyr::arrange(desc(`Contests positifs %`)) %>% #tri par Points décroissants
      dplyr::mutate(Classement = row_number()) %>%#colonne Rang 1 à 16
      dplyr::select(Classement, Equipes, `Contests positifs %`)#sélectionner colonnes Classement, Equipes + valeur
    
    DT::datatable(
      Classement_mod,
      options = list(pageLength = 17, scrollX = TRUE,searching = FALSE,lengthChange = FALSE,dom = "t"),
      rownames = FALSE
    )
  })
  
  #contre rucks positif
  output$table_classement_contrerucks_positifs <- DT::renderDataTable({
    req(Défense)
    
    Classement_mod <- Défense %>%
      dplyr::arrange(desc(`Contre-rucks positifs %`)) %>% #tri par Points décroissants
      dplyr::mutate(Classement = row_number()) %>%#colonne Rang 1 à 16
      dplyr::select(Classement, Equipes, `Contre-rucks positifs %`)#sélectionner colonnes Classement, Equipes + valeur
    
    DT::datatable(
      Classement_mod,
      options = list(pageLength = 17, scrollX = TRUE,searching = FALSE,lengthChange = FALSE,dom = "t"),
      rownames = FALSE
    )
  })
  
  #turnovers
  output$table_classement_turnovers <- DT::renderDataTable({
    req(Défense)
    
    Classement_mod <- Défense %>%
      dplyr::arrange(desc(`Turnovers`)) %>% #tri par Points décroissants
      dplyr::mutate(Classement = row_number()) %>%#colonne Rang 1 à 16
      dplyr::select(Classement, Equipes, `Turnovers`)#sélectionner colonnes Classement, Equipes + valeur
    
    DT::datatable(
      Classement_mod,
      options = list(pageLength = 17, scrollX = TRUE,searching = FALSE,lengthChange = FALSE,dom = "t"),
      rownames = FALSE
    )
  })
  
  #franchissement subis
  output$table_classement_franchissements_subis <- DT::renderDataTable({
    req(Défense)
    
    Classement_mod <- Défense %>%
      dplyr::arrange(desc(`Franchissements subis`)) %>% #tri par Points décroissants
      dplyr::mutate(Classement = row_number()) %>%#colonne Rang 1 à 16
      dplyr::select(Classement, Equipes, `Franchissements subis`)#sélectionner colonnes Classement, Equipes + valeur
    
    DT::datatable(
      Classement_mod,
      options = list(pageLength = 17, scrollX = TRUE,searching = FALSE,lengthChange = FALSE,dom = "t"),
      rownames = FALSE
    )
  })
  
}




# Lance l'application ----
shinyApp(ui = ui, server = server)


# Pour aller plus loin dans la visualisation de tableaux de données :
# - gt https://gt.rstudio.com/index.html
# - gtExtra https://jthomasmock.github.io/gtExtras/index.html 