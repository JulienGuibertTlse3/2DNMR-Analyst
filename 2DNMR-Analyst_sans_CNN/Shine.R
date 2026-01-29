# ============================================================================

# 2D NMR Spectra Analysis - Application Shiny

# ============================================================================

# Auteur : Julien Guibert

# GitHub : https://github.com/JulienGuibertTlse3/2DNMR-Analyst

# ============================================================================



# --- Chargement des packages ---

library(shiny)

library(shinyFiles)

library(plotly)

library(ggplot2)

library(DT)

library(shinycssloaders)

library(shinydashboard)

library(shinydashboardPlus)

library(shinyBS) 

library(shinyjs)

library(dplyr)

library(sp)

library(data.table)

library(dbscan)

# library(tensorflow)

# library(keras)

# library(reticulate)

library(zoo)

library(matrixStats)

library(minpack.lm)

library(imager)

library(viridis)

library(reshape2)

library(abind)

library(readr)



# --- Chargement des fichiers sources (chemins relatifs) ---

# Ces fichiers doivent être dans le sous-dossier Function_test/



source("Function/Read_2DNMR_spectrum.R")

source("Function/Vizualisation.R")

source("Function/Pping.R")

source("Function/Peak_fitting.R")

# source("Function/CNN_shiny.R")



# Fichier C++ (si disponible)

if (file.exists("Function_test/petit_test.cpp")) {
  
  Rcpp::sourceCpp("Function_test/petit_test.cpp")
  
}



# UI SIMPLIFIÉE ET INTUITIVE - ACCORDÉON ----


ui <- fluidPage(
  
  
  # Activer shinyjs
  
  useShinyjs(),
  
  # CSS personnalisé
  
  tags$head(
    
    tags$style(HTML("

      /* Style général */

      body, label, input, button, select, .form-control {

        font-size: 13px !important;

      }


      /* Plot interactif pleine largeur */

      #interactivePlot {

        width: 100% !important;

        height: 100vh !important;

      }


      /* Style de l'accordéon */

      .panel-group {

        margin-bottom: 0 !important;

      }

      .panel-group .panel {

        border-radius: 8px !important;

        margin-bottom: 8px !important;

        border: none !important;

        box-shadow: 0 2px 4px rgba(0,0,0,0.1) !important;

        overflow: hidden;

      }

      .panel-group .panel-heading {

        border-radius: 0 !important;

        padding: 12px 15px !important;

        cursor: pointer;

        transition: all 0.2s ease;

      }

      .panel-group .panel-heading:hover {

        filter: brightness(95%);

      }

      .panel-group .panel-title {

        font-size: 14px !important;

        font-weight: 600 !important;

      }

      .panel-group .panel-title a {

        color: white !important;

        text-decoration: none !important;

        display: block;

      }

      .panel-group .panel-title a:hover,

      .panel-group .panel-title a:focus {

        text-decoration: none !important;

      }


      .panel-group .panel-body {

        padding: 15px !important;

        background-color: #fafafa !important;

      }

      /* Couleurs par section */

      .panel-group .panel:nth-child(1) .panel-heading {

        background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%) !important;

        border: none !important;

      }

      .panel-group .panel:nth-child(2) .panel-heading {

        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%) !important;

        border: none !important;

      }

      .panel-group .panel:nth-child(3) .panel-heading {

        background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%) !important;

        border: none !important;

      }

      .panel-group .panel:nth-child(4) .panel-heading {

        background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%) !important;

        border: none !important;

      }

      .panel-group .panel:nth-child(5) .panel-heading {

        background: linear-gradient(135deg, #fa709a 0%, #fee140 100%) !important;

        border: none !important;

      }
      

      /* Info box */

      .info-box-custom {

        background-color: #e7f3ff;

        border-left: 4px solid #007bff;

        padding: 10px;

        margin: 10px 0;

        border-radius: 4px;

      }

      /* Warning box */

      .warning-box {

        background-color: #fff3cd;

        border-left: 4px solid #ffc107;

        padding: 10px;

        margin: 10px 0;

        border-radius: 4px;

      }

      /* Coordonnées cliquées */

      .click-coords {

        background-color: #e9ecef;

        padding: 8px 12px;

        border-radius: 4px;

        font-family: monospace;

        margin: 5px 0;

        font-size: 12px;

      }

      /* Tables plus compactes */

      .dataTables_wrapper {

        overflow-x: auto;

        font-size: 12px;

      }

      /* Checkbox list scrollable */

      .spectra-list {

        max-height: 200px;

        overflow-y: auto;

        border: 1px solid #ddd;

        border-radius: 5px;

        padding: 10px;

        background-color: #fff;

      }

      /* Details/Summary styling */

      details {

        margin: 10px 0;

      }

      details summary {

        padding: 8px 10px;

        background: #e9ecef;

        border-radius: 4px;

        cursor: pointer;

        font-weight: 500;

        font-size: 12px;

        color: #495057;

      }

      details summary:hover {

        background: #dee2e6;

      }

      details[open] summary {

        border-radius: 4px 4px 0 0;

        margin-bottom: 0;

        background: #007bff;

        color: white;

      }

      details > div {

        border: 1px solid #dee2e6;

        border-top: none;

        border-radius: 0 0 4px 4px;

        padding: 10px;

        background: #f8f9fa;

      }

      /* Boutons compacts */

      .btn-xs {

        padding: 2px 8px;

        font-size: 11px;

      }

      /* Move buttons layout */

      .move-btn-grid {

        display: inline-grid;

        grid-template-columns: repeat(3, 35px);

        grid-template-rows: repeat(3, 30px);

        gap: 2px;

        justify-items: center;

        align-items: center;

      }

      .move-btn-grid .btn {

        width: 32px;

        height: 28px;

        padding: 0;

        font-size: 14px;

      }

      /* Fix overflow issues in sidebar and panels */

.panel-body {

  overflow: visible !important;

}

.bsCollapsePanel .panel-body {

  overflow: visible !important;

}


/* Fix sidebar overflow */

.main-sidebar, .sidebar, .sidebar-menu {

  overflow: visible !important;

}

.content-wrapper, .main-content {

  overflow: visible !important;

}

/* Fix pour les boutons Peak Picking qui sont coupés */

.panel-group .panel:nth-child(3) .panel-body {

  overflow: visible !important;

}

/* Boutons Local Max et CNN plus compacts */

.btn-peak-picking {

  font-size: 11px !important;

  padding: 5px 8px !important;

  white-space: nowrap !important;

}

/* Fix pour le Step input qui est coupé */

#move_box_step {

  width: 100% !important;

}


/* Labels plus compacts */

.form-group label {

  margin-bottom: 2px !important;

  font-size: 12px !important;

}

/* NumericInput plus compact dans Edit box */

.edit-box-inputs .form-group {

  margin-bottom: 5px !important;

}

.edit-box-inputs input[type='number'] {

  padding: 4px 6px !important;

  font-size: 12px !important;

}

/* Step input compact */

.step-input-compact .form-group {

  margin-bottom: 0 !important;

}

.step-input-compact label {

  font-size: 11px !important;

}

.step-input-compact input {

  font-size: 11px !important;

  padding: 3px 5px !important;

  height: 28px !important;

}

/* Move buttons grid fix */

.move-btn-grid {

  display: inline-grid;

  grid-template-columns: repeat(3, 28px);

  grid-template-rows: repeat(3, 26px);

  gap: 1px;

  justify-items: center;

  align-items: center;

}

.move-btn-grid .btn {

  width: 26px !important;

  height: 24px !important;

  padding: 0 !important;

  font-size: 12px !important;

  line-height: 24px !important;

}

/* Shrink/Expand buttons */

.move-btn-grid .btn-warning,

.move-btn-grid .btn-success {

  width: 12px !important;

  font-size: 14px !important;

  font-weight: bold !important;

}

    ")),
    
    
    
    # JavaScript pour mise à jour synchrone des ticks lors du zoom
    
    tags$script(HTML("

      // Fonction pour générer des ticks 'propres' pour une plage donnée

      function generateNiceTicks(min, max, targetCount) {

        var range = max - min;

        if (range <= 0) return { tickvals: [], ticktext: [] };

        

        // Pas 'propres' possibles

        var niceSteps = [0.01, 0.02, 0.05, 0.1, 0.2, 0.25, 0.5, 1, 2, 2.5, 5, 10, 20, 25, 50, 100];

        var roughStep = range / targetCount;

        

        // Trouver le pas le plus proche

        var step = niceSteps[0];

        for (var i = 0; i < niceSteps.length; i++) {

          if (niceSteps[i] >= roughStep) {

            step = niceSteps[i];

            break;

          }

          step = niceSteps[i];

        }

        

        // Calculer les ticks

        var startTick = Math.ceil(min / step) * step;

        var tickvals = [];

        var ticktext = [];

        

        // Déterminer le nombre de décimales

        var decimals = 0;

        if (step < 1) decimals = step < 0.1 ? 2 : 1;

        

        for (var t = startTick; t <= max; t += step) {

          tickvals.push(t);

          // Inverser le signe pour l'affichage (convention NMR)

          ticktext.push((-t).toFixed(decimals));

        }

        

        return { tickvals: tickvals, ticktext: ticktext };

      }

      

      // Fonction principale pour mettre à jour les ticks

      function updateTicksOnZoom(gd) {

        if (!gd || !gd.layout) return;

        

        var xaxis = gd.layout.xaxis || {};

        var yaxis = gd.layout.yaxis || {};

        

        var xRange = xaxis.range;

        var yRange = yaxis.range;

        

        if (!xRange || !yRange) return;

        

        var xTicks = generateNiceTicks(Math.min(xRange[0], xRange[1]), Math.max(xRange[0], xRange[1]), 10);

        var yTicks = generateNiceTicks(Math.min(yRange[0], yRange[1]), Math.max(yRange[0], yRange[1]), 10);

        

        // Mise à jour synchrone sans déclencher d'événement relayout

        Plotly.relayout(gd, {

          'xaxis.tickmode': 'array',

          'xaxis.tickvals': xTicks.tickvals,

          'xaxis.ticktext': xTicks.ticktext,

          'xaxis.showticklabels': true,

          'xaxis.ticks': 'outside',

          'yaxis.tickmode': 'array',

          'yaxis.tickvals': yTicks.tickvals,

          'yaxis.ticktext': yTicks.ticktext,

          'yaxis.showticklabels': true,

          'yaxis.ticks': 'outside'

        });

      }

      

      // Fonction pour masquer les ticks temporairement (utilisée lors du dézoom)

      function hideTicksTemporarily(gd) {

        Plotly.relayout(gd, {

          'xaxis.showticklabels': false,

          'xaxis.ticks': '',

          'yaxis.showticklabels': false,

          'yaxis.ticks': ''

        });

      }

      

      // Observer pour détecter quand le plot est créé/mis à jour

      $(document).on('shiny:value', function(event) {

        if (event.name === 'interactivePlot') {

          setTimeout(function() {

            var gd = document.getElementById('interactivePlot');

            if (gd && gd.on) {

              // Écouter les événements de zoom

              gd.on('plotly_relayout', function(eventData) {

                if (!eventData) return;

                

                // Ignorer nos propres mises à jour de ticks

                if (eventData['xaxis.tickvals'] !== undefined) return;

                if (eventData['xaxis.showticklabels'] !== undefined && 

                    eventData['xaxis.range[0]'] === undefined &&

                    eventData['xaxis.autorange'] === undefined) return;

                

                // Détecter si c'est un autoscale (double-clic, reset)

                var isAutoscale = eventData['xaxis.autorange'] !== undefined || 

                                  eventData['yaxis.autorange'] !== undefined ||

                                  eventData['autosize'] !== undefined;

                

                if (isAutoscale) {

                  // Masquer immédiatement les ticks pendant l'autoscale

                  hideTicksTemporarily(gd);

                  // Attendre que plotly ait calculé les nouveaux ranges puis mettre à jour

                  setTimeout(function() { updateTicksOnZoom(gd); }, 80);

                } else if (eventData['xaxis.range[0]'] !== undefined || 

                           eventData['yaxis.range[0]'] !== undefined) {

                  // Zoom manuel - comportement inchangé

                  setTimeout(function() { updateTicksOnZoom(gd); }, 10);

                }

              });

            }

          }, 500);

        }

      });

    "))
    
  ),
  
  
  
  
  
  ## DASHBOARD ----
  
  dashboardPage(
    
    # Header
    
    dashboardHeader(title = "NMR 2D Analysis"),

    
    # Sidebar
    
    dashboardSidebar(
      
      width = 280,
      
      sidebarMenu(
        
        id = "main_menu",
        
        menuItem("📖 Guide", tabName = "guide", icon = icon("book")),
        
        menuItem("📊 Analysis", tabName = "analysis", icon = icon("chart-area"))
        
      )
      
    ),
    
    # Body
    
    dashboardBody(

      
      tabItems(
 
        
        ### TAB: GUIDE ----
       
        
        tabItem(
          
          tabName = "guide",
          
          fluidRow(
            
            column(12,
                   
                   div(
                     
                     class = "guide-container",
                     
                     style = "padding: 20px;",
                     
                     
                     
                     # En-tête stylisé
                     
                     div(
                       
                       style = "display: flex; align-items: center; margin-bottom: 20px; padding-bottom: 15px; border-bottom: 2px solid #3c8dbc;",
                       
                       icon("book-open", class = "fa-2x", style = "color: #3c8dbc; margin-right: 15px;"),
                       
                       h2("Guide d'utilisation", style = "margin: 0; color: #333; font-weight: 600;")
                       
                     ),
                     
                     
                     
                     # Contenu avec style amélioré
                     
                     div(
                       
                       class = "guide-content",
                       
                       style = "background: #fff; border-radius: 8px; padding: 25px; box-shadow: 0 2px 10px rgba(0,0,0,0.08);",
                       
                       uiOutput("toolDescription")
                       
                     )
                     
                   )
                   
            )
            
          )
          
        ),
        
        ### TAB: ANALYSIS ----
        
        tabItem(
          
          tabName = "analysis",
          
          fluidRow(

            
            #### LEFT PANEL - Controls (width = 3) ----
       
            column(3,

                   
                   # ACCORDÉON - Une seule section ouverte

                   bsCollapse(
                     
                     id = "main_accordion",
                     
                     open = "panel_load",
                     
                     multiple = FALSE,
                     
                     
                     
                     ##### ===== SECTION 1: LOAD DATA =====
                     
                     bsCollapsePanel(
                       
                       title = "📂 1. Load Data",
                       
                       value = "panel_load",
                       
                       style = "primary",
                       
                       
                       
                       shinyDirButton("directory", "Select Folder", "Choose directory", 
                                      
                                      class = "btn-primary btn-sm btn-block"),
                       
                       
                       
                       verbatimTextOutput("selected_dir"),
                       
                       
                       
                       uiOutput("available_spectra_ui")
                       
                     ),
                     
                     
                     
                     ##### ===== SECTION 2: PLOT SETTINGS =====
                     
                     bsCollapsePanel(
                       
                       title = "📈 2. Plot Settings",
                       
                       value = "panel_plot",
                       
                       style = "primary",
                       
                       
                       
                       uiOutput("subfolder_selector"),
                       
                       
                       
                       selectInput("spectrum_type", "Type:", 
                                   
                                   choices = c("TOCSY", "HSQC", "COSY", "UFCOSY"),
                                   
                                   selected = "TOCSY"),
                       
                       
                       
                       fluidRow(
                         
                         column(8, 
                                
                                numericInput("contour_start", "Threshold:", value = 80000, min = 0, step = 1000)
                                
                         ),
                         
                         column(4,
                                
                                div(style = "padding-top: 25px;",
                                    
                                    actionButton("calculate_contour", "Auto", class = "btn-info btn-sm")
                                    
                                )
                                
                         )
                         
                       ),
                       
                       
                       
                       tags$details(
                         
                         tags$summary("⚙️ Advanced"),
                         
                         div(
                           
                           selectInput("seuil_method", NULL, 
                                       
                                       choices = c("% of max" = "max_pct", "Noise ×" = "bruit_mult")),
                           
                           conditionalPanel("input.seuil_method == 'max_pct'",
                                            
                                            numericInput("pct_val", "Percentage:", value = 0.0001, min = 0.001, max = 1, step = 0.001)
                                            
                           ),
                           
                           conditionalPanel("input.seuil_method == 'bruit_mult'",
                                            
                                            numericInput("bruit_mult", "Multiplier:", value = 1, min = 0.5, max = 10, step = 0.5)
                                            
                           )
                           
                         )
                         
                       ),
                       
                       
                       
                       br(),
                       
                       actionButton("generate_plot", "📊 Generate Plot", class = "btn-primary btn-block")
                       
                     ),
                     
                     
                     
                     ##### ===== SECTION 3: PEAK PICKING =====
                     
                     bsCollapsePanel(
                       
                       title = "🎯 3. Peak Picking",
                       
                       value = "panel_peaks",
                       
                       style = "primary",
                       
                       
                       
                       # Boutons sur une seule ligne avec texte plus court
                       
                       div(style = "display: flex; gap: 5px; margin-bottom: 10px;",
                           
                           actionButton("generate_centroids", "Local Max", 
                                        
                                        class = "btn-success btn-sm", 
                                        
                                        style = "flex: 1; font-size: 11px; padding: 5px 2px;")
                           
                           # actionButton("generate_centroids_cnn", "CNN", 
                           
                           #              class = "btn-info btn-sm", 
                           
                           #              style = "flex: 1; font-size: 11px; padding: 5px 2px;")
                           
                       ),
                       
                       
                       
                       tags$details(
                         
                         tags$summary("⚙️ Options"),
                         
                         div(
                           
                           checkboxInput("disable_clustering", "No clustering", value = FALSE),
                           
                           numericInput("eps_value", "Epsilon:", value = 0.01, min = 0, step = 0.001),
                           
                           textAreaInput("keep_peak_ranges_text", "Delete ranges:", 
                                         
                                         value = "0.5,-0.5; 1,0.8; 1.55,1.45; 5.1,4.5,", rows = 2)
                           
                         )
                         
                       )
                       
                     ),
                     
                     
                     
                     ##### ===== SECTION 4: MANUAL EDITING =====
                     
                     bsCollapsePanel(
                       
                       title = "✏️ 4. Manual Editing",
                       
                       value = "panel_edit",
                       
                       style = "primary",
                       
                       
                       
                       # Click mode - repliable
                       
                       tags$details(
                         
                         tags$summary("🖱️ Click mode"),
                         
                         div(
                           
                           radioButtons("box_click_mode", NULL,
                                        
                                        choices = c("Off" = "disabled", 
                                                    
                                                    "Add box (2 clicks)" = "two_clicks",
                                                    
                                                    "Delete box on click" = "delete_click"),
                                        
                                        selected = "disabled", inline = FALSE),
                           
                           
                           
                           conditionalPanel("input.box_click_mode == 'two_clicks'",
                                            
                                            uiOutput("two_click_indicator"),
                                            
                                            actionButton("cancel_first_click", "Cancel", class = "btn-warning btn-xs")
                                            
                           ),
                           
                           
                           
                           conditionalPanel("input.box_click_mode == 'delete_click'",
                                            
                                            div(class = "warning-box", style = "padding: 8px; margin: 5px 0;",
                                                
                                                icon("exclamation-triangle"), 
                                                
                                                tags$b(" Delete mode active"), br(),
                                                
                                                tags$small("Click inside a box to mark it for deletion")
                                                
                                            )
                                            
                           ),
                           
                           
                           
                           div(class = "click-coords", textOutput("click_coords_display"))
                           
                         )
                         
                       ),
                       
                       
                       
                       # Fusing Peaks and Boxes - repliable
                       
                       tags$details(
                         
                         tags$summary("🔗 Fusing Peaks and Boxes"),
                         
                         div(
                           
                           actionButton("fuse_btn", "🔗 Fuse Selected", class = "btn-warning btn-sm btn-block")
                           
                         )
                         
                       ),
                       
                       
                       
                       # Edit box - version compacte
                       
                       tags$details(
                         
                         tags$summary("📦 Edit selected box"),
                         
                         div(
                           
                           verbatimTextOutput("selected_box_info"),
                           
                           
                           
                           # Coordonnées en 2 lignes compactes
                           
                           div(class = "edit-box-inputs",
                               
                               fluidRow(
                                 
                                 column(6, numericInput("edit_box_xmin", "xmin:", value = NA, step = 0.01)),
                                 
                                 column(6, numericInput("edit_box_xmax", "xmax:", value = NA, step = 0.01))
                                 
                               ),
                               
                               fluidRow(
                                 
                                 column(6, numericInput("edit_box_ymin", "ymin:", value = NA, step = 0.01)),
                                 
                                 column(6, numericInput("edit_box_ymax", "ymax:", value = NA, step = 0.01))
                                 
                               )
                               
                           ),
                           
                           
                           
                           # Step et Move buttons sur la même ligne
                           
                           div(style = "display: flex; align-items: flex-end; gap: 10px; margin-top: 10px;",
                               
                               
                               
                               # Step input compact
                               
                               div(class = "step-input-compact", style = "width: 70px;",
                                   
                                   numericInput("move_box_step", "Step:", value = 0.01, min = 0.001, step = 0.005)
                                   
                               ),
                               
                               
                               
                               # Move buttons grid
                               
                               div(class = "move-btn-grid",
                                   
                                   # Ligne 1
                                   
                                   div(),
                                   
                                   actionButton("move_box_up", "↑", class = "btn-default btn-xs"),
                                   
                                   div(),
                                   
                                   # Ligne 2
                                   
                                   actionButton("move_box_left", "←", class = "btn-default btn-xs"),
                                   
                                   div(style = "display: flex; gap: 1px;",
                                       
                                       actionButton("shrink_box", "−", class = "btn-warning btn-xs"),
                                       
                                       actionButton("expand_box", "+", class = "btn-success btn-xs")
                                       
                                   ),
                                   
                                   actionButton("move_box_right", "→", class = "btn-default btn-xs"),
                                   
                                   # Ligne 3
                                   
                                   div(),
                                   
                                   actionButton("move_box_down", "↓", class = "btn-default btn-xs"),
                                   
                                   div()
                                   
                               )
                               
                           ),
                           
                           
                           
                           br(),
                           
                           actionButton("apply_box_edit", "Apply Edit", class = "btn-primary btn-sm btn-block")
                           
                         )
                         
                       ),
                       
                       # Add manually
                       
                       tags$details(
                         
                         tags$summary("➕ Add manually"),
                         
                         div(
                           
                           tags$b("Peak:"),
                           
                           fluidRow(
                             
                             column(5, numericInput("manual_f2", "F2:", value = 4.0, step = 0.01)),
                             
                             column(5, numericInput("manual_f1", "F1:", value = 3.5, step = 0.01)),
                             
                           ),
                           
                           actionButton("add_manual_centroid", "Add Peak", class = "btn-info btn-sm btn-block"),
                           
                           hr(),
                           
                           tags$b("Box:"),
                           
                           fluidRow(
                             
                             column(6, numericInput("manual_xmin", "xmin:", value = 3.5, step = 0.01)),
                             
                             column(6, numericInput("manual_xmax", "xmax:", value = 4.0, step = 0.01))
                             
                           ),
                           
                           fluidRow(
                             
                             column(6, numericInput("manual_ymin", "ymin:", value = 2.0, step = 0.01)),
                             
                             column(6, numericInput("manual_ymax", "ymax:", value = 3.0, step = 0.01))
                             
                           ),
                           
                           actionButton("add_manual_bbox", "Add Box", class = "btn-info btn-sm btn-block")
                           
                         )
                         
                       ),

                       
                       fluidRow(
                         
                         column(6, actionButton("apply_changes", "✅ Apply", class = "btn-success btn-sm btn-block")),
                         
                         column(6, actionButton("discard_changes", "❌ Discard", class = "btn-secondary btn-sm btn-block"))
                         
                       )
                       
                     ),

                     ##### ===== SECTION 5: INTEGRATION =====
                     
                     bsCollapsePanel(
                       
                       title = "📐 5. Integration",
                       
                       value = "panel_integration",
                       
                       style = "primary",
                       
                       
                       
                       # Méthode d'intégration avec groupes visuels
                       
                       h5(tags$b("Select method:")),
                       
                       
                       
                       # Groupe AUC
                       
                       div(style = "background: #e8f5e9; border-radius: 8px; padding: 10px; margin-bottom: 10px; border-left: 4px solid #4caf50;",
                           
                           tags$b("Area Under Curve (AUC)", style = "color: #2e7d32;"),
                           
                           radioButtons("integration_method", NULL,
                                        
                                        choices = c("Sum (AUC)" = "sum"),
                                        
                                        selected = "sum",
                                        
                                        inline = TRUE),
                           
                           tags$small("Direct integration of intensity values in the box", style = "color: #666;")
                           
                       ),
                       

                       # Groupe Peak Fitting
                       
                       div(style = "background: #e3f2fd; border-radius: 8px; padding: 10px; border-left: 4px solid #2196f3;",
                           
                           tags$b("Peak Fitting", style = "color: #1565c0;"),
                           
                           radioButtons("integration_method_fit", NULL,
                                        
                                        choices = c("Gaussian" = "gaussian",
                                                    
                                                    "Voigt (Gaussian + Lorentzian)" = "voigt"),
                                        
                                        selected = character(0),
                                        
                                        inline = TRUE),
                           
                           tags$small("Fits a mathematical model to the peak shape", style = "color: #666;")
                           
                       ),
                       

                       conditionalPanel(
                         
                         "input.integration_method_fit !== undefined && input.integration_method_fit !== null && input.integration_method_fit.length > 0",
                         
                         div(style = "margin-top: 10px; padding: 10px; background: #fff8e1; border-radius: 8px; border-left: 4px solid #ff9800;",
                             
                             tags$b("⚙️ Fitting options", style = "color: #e65100;"),
                             
                             checkboxInput("show_fit_quality", "Include R² in export", value = TRUE),
                             
                             sliderInput("min_r_squared", "Min R² threshold:", 
                                         
                                         min = 0, max = 1, value = 0.7, step = 0.05),
                             
                             tags$small("Peaks with R² below threshold will use sum fallback", style = "color: #666;")
                             
                         )
                         
                       ),

                       hr(),

                       # Bouton Calculate
                       
                       actionButton("run_integration", "▶️ Run Integration", class = "btn-success btn-block"),
                       

                       br(),

                       # Résultat de l'intégration
                       
                       conditionalPanel(
                         
                         "output.integration_done",
                         
                         div(style = "margin-top: 10px; padding: 10px; background: #e8f5e9; border-radius: 8px; border: 1px solid #4caf50;",
                             
                             h5(tags$b("✅ Integration Results"), style = "color: #2e7d32;"),
                             
                             verbatimTextOutput("integration_summary"),
                             
                             br(),
                             
                             downloadButton("export_integration_results", "📥 Download Results (CSV)", class = "btn-primary btn-block")
                             
                         )
                         
                       )
                       
                     ),
                     

                     ##### ===== SECTION 6: SAVE & EXPORT =====
                     
                     bsCollapsePanel(
                       
                       title = "💾 6. Save & Export",
                       
                       value = "panel_export",
                       
                       style = "primary",
                       
                       
                       
                       # Session save/load - repliable
                       
                       tags$details(
                         
                         tags$summary("💼 Session"),
                         
                         div(
                           
                           fluidRow(
                             
                             column(6, 
                                    div(style = "margin-top: 8px;",
                                        downloadButton("save_session", "💾 Save", 
                                                       class = "btn-success btn-sm btn-block")
                                    )
                             ),
                             
                             column(6, 
                                    fileInput("load_session_file", NULL, accept = ".rds", 
                                              buttonLabel = "📂 Load", width = "100%")
                             )
                             
                           ),
                           
                           tags$small("Save/load your complete work (peaks, boxes, parameters)", 
                                      style = "color: #666;")
                           
                         )
                         
                       ),
                       
                       
                       
                       # Import - repliable
                       
                       tags$details(
                         
                         tags$summary("📥 Import Peaks & Boxes"),
                         
                         div(
                           
                           fileInput("import_centroids_file", "Peaks CSV:", accept = ".csv"),
                           
                           fileInput("import_boxes_file", "Boxes CSV:", accept = ".csv")
                           
                         )
                         
                       ),
                       
                       
                       
                       # Export - repliable
                       
                       tags$details(
                         
                         tags$summary("📤 Export Data"),
                         
                         div(
                           
                           fluidRow(
                             
                             column(6, downloadButton("export_centroids", "Peaks", class = "btn-sm btn-block")),
                             
                             column(6, downloadButton("export_boxes", "Boxes", class = "btn-sm btn-block"))
                             
                           ),
                           
                           br(),
                           
                           downloadButton("export_batch_box_intensities", "📤 Batch Export (all spectra)", class = "btn-primary btn-sm btn-block")
                           
                         )
                         
                       ),
                       

                       hr(),
                       
                       actionButton("reset_all", "🔄 Reset All", class = "btn-outline-danger btn-sm btn-block")
                       
                     )
                     
  
                   ) # End bsCollapse
                   
            ),

            
            #### RIGHT PANEL - Visualization (width = 9) ----
            
            
            
            column(9,
                   
                   
                   
                   # Status message
                   
                   div(class = "info-box-custom",
                       
                       textOutput("status_message")
                       
                   ),
                   
                   
                   
                   # Main tabs
                   
                   tabBox(
                     
                     width = 12,
                     
                     id = "main_tabs",
                     
                     
                     
                     ##### Tab 1: Spectrum plot ----
                     
                     tabPanel(
                       
                       title = tagList(icon("chart-area"), "Spectrum"),
                       
                       value = "spectrum_tab",
                       
                       
                       
                       plotlyOutput("interactivePlot", height = "550px", width = "100%"),
                       
                       
                       
                       conditionalPanel(
                         
                         "output.has_pending_changes",
                         
                         div(class = "warning-box",
                             
                             tags$b("⏳ Pending: "),
                             
                             textOutput("pending_summary", inline = TRUE)
                             
                         )
                         
                       )
                       
                     ),
                     
                     
                     
                     ##### Tab 2: Data tables ----
                     
                     tabPanel(
                       
                       title = tagList(icon("table"), "Data"),
                       
                       value = "data_tab",
                       
                       
                       
                       fluidRow(
                         
                         column(6,
                                
                                h4("🔴 Peaks"),
                                
                                DTOutput("centroid_table"),
                                
                                div(style = "margin-top: 8px;",
                                    
                                    actionButton("delete_selected_peaks", "🗑️ Delete Selected", 
                                                 
                                                 class = "btn-sm btn-danger"),
                                    
                                    tags$small(" (Ctrl+Click for multiple)", style = "color: #666; margin-left: 10px;")
                                    
                                )
                                
                         ),
                         
                         column(6,
                                
                                h4("🟦 Boxes"),
                                
                                DTOutput("bbox_table"),
                                
                                div(style = "margin-top: 8px;",
                                    
                                    actionButton("delete_selected_boxes", "🗑️ Delete Selected", 
                                                 
                                                 class = "btn-sm btn-danger"),
                                    
                                    tags$small(" (Ctrl+Click for multiple)", style = "color: #666; margin-left: 10px;")
                                    
                                )
                                
                         )
                         
                       )
                       
                     ),
                     
                     
                     
                     ##### Tab 3: Pending changes ----
                     
                     tabPanel(
                       
                       title = tagList(icon("clock"), "Pending"),
                       
                       value = "pending_tab",
                       
                       
                       
                       # Peaks - première ligne
                       
                       div(style = "margin-bottom: 20px;",
                           
                           h4("🔴 Pending Peaks"),
                           
                           DTOutput("pending_centroids_table"),
                           
                           actionButton("discard_selected_centroid", "Remove Selected", class = "btn-sm btn-danger", style = "margin-top: 5px;")
                           
                       ),
                       
                       
                       
                       hr(),
                       
                       
                       
                       # Boxes - deuxième ligne
                       
                       div(style = "margin-bottom: 20px;",
                           
                           h4("🟦 Pending Boxes"),
                           
                           DTOutput("pending_boxes_table"),
                           
                           actionButton("discard_selected_box", "Remove Selected", class = "btn-sm btn-danger", style = "margin-top: 5px;")
                           
                       ),
                       
                       
                       
                       hr(),
                       
                       
                       
                       # Fusions - troisième ligne
                       
                       div(style = "margin-bottom: 20px;",
                           
                           h4("🔗 Pending Fusions"),
                           
                           DTOutput("pending_fusions_table"),
                           
                           actionButton("discard_selected_fusion", "Remove Selected", class = "btn-sm btn-danger", style = "margin-top: 5px;")
                           
                       )
                       
                     ),
                     
                     # Dans tabBox, ajouter un nouvel onglet
                     
                     tabPanel(
                       
                       title = tagList(icon("chart-line"), "Fit Quality"),
                       
                       value = "fit_quality_tab",
                       
                       
                       
                       # Info box en haut
                       
                       div(class = "info-box-custom",
                           
                           style = "margin-bottom: 20px;",
                           
                           icon("info-circle"),
                           
                           " This tab displays fit quality metrics when using Gaussian or Voigt integration methods. ",
                           
                           "Select a box in the ", tags$b("Data"), " tab to see detailed fit visualization."
                           
                       ),
                       
                       
                       
                       # Résumé global
                       
                       fluidRow(
                         
                         column(6,
                                
                                h4("📊 Fit Summary by Method"),
                                
                                DTOutput("fit_summary_table")
                                
                         ),
                         
                         column(6,
                                
                                h4("📋 Fitted Boxes Details"),
                                
                                DTOutput("fit_boxes_detail_table")
                                
                         )
                         
                       ),
                       
                       
                       
                       br(),
                       
                       
                       
                       # Distribution des R²
                       
                       fluidRow(
                         
                         column(12,
                                
                                h4("📈 R² Distribution"),
                                
                                plotlyOutput("fit_quality_plot", height = "400px")
                                
                         )
                         
                       ),
                       
                       
                       
                       br(),
                       
                       
                       
                       # Visualisation détaillée d'une box sélectionnée
                       
                       fluidRow(
                         
                         column(6,
                                
                                h4("🔍 Selected Box - 2D Fit"),
                                
                                div(style = "border: 1px solid #ddd; border-radius: 8px; padding: 10px; background: #fafafa;",
                                    
                                    plotOutput("example_fit_2d", height = "400px")
                                    
                                )
                                
                         ),
                         
                         column(6,
                                
                                h4("📉 Residuals Distribution"),
                                
                                div(style = "border: 1px solid #ddd; border-radius: 8px; padding: 10px; background: #fafafa;",
                                    
                                    plotOutput("residuals_plot", height = "400px")
                                    
                                )
                                
                         )
                         
                       ),

                       br(),
 
                       # Tips
                       
                       div(style = "background: #fff3e0; padding: 15px; border-radius: 8px; border-left: 4px solid #ff9800;",
                           
                           h5("💡 Interpretation Tips"),
                           
                           tags$ul(
                             
                             tags$li(tags$b("R² > 0.9:"), " Excellent fit - peak is well-defined"),
                             
                             tags$li(tags$b("R² 0.7-0.9:"), " Good fit - acceptable quantification"),
                             
                             tags$li(tags$b("R² < 0.7:"), " Poor fit - consider manual inspection or sum method"),
                             
                             tags$li(tags$b("Residuals:"), " Should be randomly distributed around zero")
                             
                           )
                           
                       )
                       
                     )
                     
                   )
                   
            )
            
          )
          
        )
        
      )
      
    )
    
  )
  
)


# Functions ----

server <- function(input, output, session) {

  
  # SECTION 1: CONFIGURATION ET OPTIONS ----
  

  options(future.globals.maxSize = 10000 * 1024^2)  # allow up to 10 GB
  
  
  
  # Enable future/promises if available
  
  future_available <- requireNamespace("future", quietly = TRUE) && 
    
    requireNamespace("promises", quietly = TRUE)
  
  
  
  # Opérateur null-coalesce
  
  
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  

  
  # SECTION 2: FONCTIONS UTILITAIRES ----
  

  ## 2.1 Cache pour lecture Bruker ----
  
  spectrum_cache <- new.env(parent = emptyenv())
  
  
  
  read_bruker_cached <- function(path, dim = "2D") {
    
    key <- normalizePath(path, mustWork = FALSE)
    
    if (exists(key, envir = spectrum_cache, inherits = FALSE)) {
      
      return(get(key, envir = spectrum_cache, inherits = FALSE))
      
    }
    
    data <- read_bruker(path, dim = dim)
    
    assign(key, data, envir = spectrum_cache)
    
    data
    
  }
  
  
  
  ## 2.2 Calcul d'intensité des boxes (vectorisé avec option fitting) ----
  
  get_box_intensity <- function(mat, ppm_x, ppm_y, boxes, method = "sum", model = "gaussian") {
    
    if (nrow(boxes) == 0) return(numeric(0))
    
    
    
    if (method == "sum") {
      
      # Méthode existante (rapide)
      
      xmin_v <- as.numeric(boxes$xmin)
      
      xmax_v <- as.numeric(boxes$xmax)
      
      ymin_v <- as.numeric(boxes$ymin)
      
      ymax_v <- as.numeric(boxes$ymax)
      
      
      
      vapply(seq_along(xmin_v), FUN.VALUE = 0.0, FUN = function(i) {
        
        xi <- which(ppm_x >= xmin_v[i] & ppm_x <= xmax_v[i])
        
        yi <- which(ppm_y >= ymin_v[i] & ppm_y <= ymax_v[i])
        
        if (length(xi) == 0 || length(yi) == 0) return(NA_real_)
        
        sum(mat[yi, xi], na.rm = TRUE)
        
      })
      
      
      
    } else {
      
      # Méthode par fitting (plus lente mais plus précise)
      
      fit_results <- calculate_fitted_volumes(mat, ppm_x, ppm_y, boxes, model = model)
      
      fit_results$volume_fitted
      
    }
    
  }
  
  
  
  ## 2.3 Parse keep_peak_ranges ----
  
  parse_keep_peak_ranges <- function(input_string) {
    
    if (is.null(input_string) || input_string == "") return(NULL)
    
    pairs <- strsplit(input_string, ";")[[1]]
    
    parsed <- lapply(pairs, function(p) {
      
      nums <- as.numeric(trimws(unlist(strsplit(p, ","))))
      
      if (length(nums) == 2 && all(!is.na(nums))) nums else NULL
      
    })
    
    parsed[!sapply(parsed, is.null)]
    
  }
  
  
  
  ## 2.4 Nettoyage des centroids importés ----
  
  clean_centroids_df <- function(df) {
    
    df$F2_ppm <- as.numeric(gsub(",", ".", trimws(df$F2_ppm)))
    
    df$F1_ppm <- as.numeric(gsub(",", ".", trimws(df$F1_ppm)))
    
    df$Volume <- as.numeric(gsub(",", ".", trimws(df$Volume)))
    
    df
    
  }
  
  
  
  ## 2.5 Calcul batch des intensités ----
  
  calculate_batch_box_intensities <- function(reference_boxes, 
                                              
                                              spectra_list, 
                                              
                                              apply_shift = FALSE, 
                                              
                                              method = "sum",      # NOUVEAU paramètre
                                              
                                              model = "gaussian",  # NOUVEAU paramètre
                                              
                                              progress = NULL) {
    
    
    
    # ========== VALIDATIONS ==========
    
    if (is.null(reference_boxes) || nrow(reference_boxes) == 0) {
      
      stop("reference_boxes est vide ou NULL")
      
    }
    
    
    
    # Copie pour ne pas modifier l'original
    
    ref_boxes <- as.data.frame(reference_boxes)
    
    
    
    # Vérifier les colonnes requises
    
    required_cols <- c("xmin", "xmax", "ymin", "ymax")
    
    missing_cols <- setdiff(required_cols, names(ref_boxes))
    
    if (length(missing_cols) > 0) {
      
      stop(paste("Colonnes manquantes:", paste(missing_cols, collapse = ", ")))
      
    }
    
    
    
    # ========== NETTOYAGE DES BOXES ==========
    
    
    
    # Ajouter stain_id si manquant
    
    if (!"stain_id" %in% names(ref_boxes)) {
      
      ref_boxes$stain_id <- paste0("box_", seq_len(nrow(ref_boxes)))
      
    }
    
    
    
    # Supprimer les lignes avec des coordonnées NA
    
    ref_boxes <- ref_boxes[
      
      !is.na(ref_boxes$xmin) & !is.na(ref_boxes$xmax) & 
        
        !is.na(ref_boxes$ymin) & !is.na(ref_boxes$ymax), , drop = FALSE
      
    ]
    
    
    
    if (nrow(ref_boxes) == 0) {
      
      stop("Toutes les boxes ont des coordonnées NA")
      
    }
    
    
    
    # Corriger les boxes inversées (xmin > xmax ou ymin > ymax)
    
    for (i in seq_len(nrow(ref_boxes))) {
      
      if (ref_boxes$xmin[i] > ref_boxes$xmax[i]) {
        
        tmp <- ref_boxes$xmin[i]
        
        ref_boxes$xmin[i] <- ref_boxes$xmax[i]
        
        ref_boxes$xmax[i] <- tmp
        
      }
      
      if (ref_boxes$ymin[i] > ref_boxes$ymax[i]) {
        
        tmp <- ref_boxes$ymin[i]
        
        ref_boxes$ymin[i] <- ref_boxes$ymax[i]
        
        ref_boxes$ymax[i] <- tmp
        
      }
      
    }
    
    
    
    # ========== GESTION DES DUPLICATS ==========
    
    
    
    # Vérifier les duplicats de stain_id
    
    if (any(duplicated(ref_boxes$stain_id))) {
      
      warning("Duplicats de stain_id détectés - renommage automatique")
      
      # Renommer les duplicats
      
      dup_ids <- ref_boxes$stain_id[duplicated(ref_boxes$stain_id)]
      
      for (dup_id in unique(dup_ids)) {
        
        idx <- which(ref_boxes$stain_id == dup_id)
        
        if (length(idx) > 1) {
          
          ref_boxes$stain_id[idx[-1]] <- paste0(dup_id, "_dup", seq_along(idx[-1]))
          
        }
        
      }
      
    }
    
    
    
    # Vérifier les boxes avec coordonnées identiques
    
    coord_signature <- paste(ref_boxes$xmin, ref_boxes$xmax, ref_boxes$ymin, ref_boxes$ymax, sep = "_")
    
    if (any(duplicated(coord_signature))) {
      
      warning("Boxes avec coordonnées identiques détectées - suppression des doublons")
      
      ref_boxes <- ref_boxes[!duplicated(coord_signature), , drop = FALSE]
      
    }
    
    
    
    # ========== CALCUL DES CENTRES ==========
    
    
    
    # Calculer F2_ppm et F1_ppm (centres des boxes)
    
    ref_boxes$F2_ppm <- (ref_boxes$xmin + ref_boxes$xmax) / 2
    
    ref_boxes$F1_ppm <- (ref_boxes$ymin + ref_boxes$ymax) / 2
    
    
    
    # ========== CONSTRUCTION DU DATAFRAME RÉSULTAT ==========
    
    
    
    # Nombre de boxes final
    
    n_boxes <- nrow(ref_boxes)
    
    message(sprintf("Processing %d boxes across %d spectra", n_boxes, length(spectra_list)))
    
    
    
    # Créer le dataframe résultat avec les colonnes de base
    
    result_df <- data.frame(
      
      stain_id = ref_boxes$stain_id,
      
      F2_ppm = ref_boxes$F2_ppm,
      
      F1_ppm = ref_boxes$F1_ppm,
      
      xmin = ref_boxes$xmin,
      
      xmax = ref_boxes$xmax,
      
      ymin = ref_boxes$ymin,
      
      ymax = ref_boxes$ymax,
      
      stringsAsFactors = FALSE
      
    )
    
    
    
    # ========== CALCUL DES INTENSITÉS PAR SPECTRE ==========
    
    
    
    for (spectrum_name in names(spectra_list)) {
      
      spectrum_data <- spectra_list[[spectrum_name]]
      
      
      
      if (is.null(spectrum_data) || is.null(spectrum_data$spectrumData)) {
        
        warning(paste("Spectre", spectrum_name, "invalide - colonne remplie de NA"))
        
        col_name <- paste0("Intensity_", make.names(basename(spectrum_name)))
        
        result_df[[col_name]] <- rep(NA_real_, n_boxes)
        
        next
        
      }
      
      
      
      mat <- spectrum_data$spectrumData
      
      ppm_x <- suppressWarnings(as.numeric(colnames(mat)))
      
      ppm_y <- suppressWarnings(as.numeric(rownames(mat)))
      
      
      
      # Vérifier que les ppm sont valides
      
      if (any(is.na(ppm_x)) || any(is.na(ppm_y))) {
        
        warning(paste("Spectre", spectrum_name, "a des ppm invalides"))
        
        col_name <- paste0("Intensity_", make.names(basename(spectrum_name)))
        
        result_df[[col_name]] <- rep(NA_real_, n_boxes)
        
        next
        
      }
      
      
      
      # Calcul du shift si demandé
      
      shift_f2 <- 0
      
      shift_f1 <- 0
      
      
      
      if (apply_shift && n_boxes > 0) {
        
        max_idx <- which(mat == max(mat, na.rm = TRUE), arr.ind = TRUE)
        
        if (length(max_idx) > 0 && nrow(max_idx) > 0) {
          
          max_f2 <- ppm_x[max_idx[1, 2]]
          
          max_f1 <- ppm_y[max_idx[1, 1]]
          
          shift_f2 <- max_f2 - ref_boxes$F2_ppm[1]
          
          shift_f1 <- max_f1 - ref_boxes$F1_ppm[1]
          
          # Limiter le shift à 0.5 ppm max
          
          if (abs(shift_f2) > 0.5) shift_f2 <- 0
          
          if (abs(shift_f1) > 0.5) shift_f1 <- 0
          
        }
        
      }
      
      
      
      if (method == "sum") {
        
        # Calculer les intensités pour chaque box
        
        intensities <- numeric(n_boxes)
        
        
        
        for (i in seq_len(n_boxes)) {
          
          xmin_shifted <- ref_boxes$xmin[i] + shift_f2
          
          xmax_shifted <- ref_boxes$xmax[i] + shift_f2
          
          ymin_shifted <- ref_boxes$ymin[i] + shift_f1
          
          ymax_shifted <- ref_boxes$ymax[i] + shift_f1
          
          
          
          x_idx <- which(ppm_x >= xmin_shifted & ppm_x <= xmax_shifted)
          
          y_idx <- which(ppm_y >= ymin_shifted & ppm_y <= ymax_shifted)
          
          
          
          if (length(x_idx) == 0 || length(y_idx) == 0) {
            
            intensities[i] <- NA_real_
            
          } else {
            
            intensities[i] <- sum(mat[y_idx, x_idx, drop = FALSE], na.rm = TRUE)
            
          }
          
        }
        
        
        
      } else {
        
        # Méthode par fitting
        
        fit_results <- calculate_fitted_volumes(
          
          mat, ppm_x, ppm_y, 
          
          ref_boxes[, c("xmin", "xmax", "ymin", "ymax", "stain_id")],
          
          model = model
          
        )
        
        
        
        intensities <- fit_results$volume_fitted
        
        
        
        # Note: On ne stocke plus R², CenterX, CenterY dans le batch export
        
        # pour garder un CSV compact et facile à comparer entre spectres
        
        # Ces infos sont disponibles dans l'onglet "Fit Quality" et via "Run Integration"
        
      }
      
      
      
      col_name <- paste0("Intensity_", make.names(basename(spectrum_name)))
      
      result_df[[col_name]] <- intensities
      
    }
    
    
    
    # ========== VÉRIFICATION FINALE ==========
    
    
    
    # Vérifier que le nombre de lignes est correct
    
    if (nrow(result_df) != n_boxes) {
      
      warning(sprintf("Anomalie: %d lignes attendues, %d obtenues", n_boxes, nrow(result_df)))
      
    }
    
    
    
    # Log final
    
    message(sprintf("Export: %d boxes, %d colonnes d'intensité", 
                    
                    nrow(result_df), 
                    
                    sum(grepl("^Intensity_", names(result_df)))))
    
    
    
    return(result_df)
    
  }
  
  
  
  # SECTION 3: VALEURS RÉACTIVES ----
  
  
  
  
  
  ## 3.1 État général ----
  
  status_msg <- reactiveVal("")
  
  bruker_data <- reactiveVal(NULL)
  
  spectra_list <- reactiveVal(list())
  
  spectra_plots <- reactiveVal(list())
  
  
  
  ## 3.2 Plots et cache ----
  
  plot_cache <- reactiveVal(list())
  
  contour_cache <- reactiveVal(list())
  
  box_intensity_cache <- reactiveVal(list())
  
  nmr_plot <- reactiveVal(NULL)
  
  contour_plot_base <- reactiveVal(NULL)
  
  result_data <- reactiveVal(NULL)
  
  result_data_list <- reactiveVal(list())
  
  
  
  ## 3.3 Centroids ----
  
  centroids <- reactiveVal(NULL)
  
  centroids_data <- reactiveVal(NULL)
  
  imported_centroids <- reactiveVal(NULL)
  
  
  
  ## 3.4 Boxes ----
  
  fixed_boxes <- reactiveVal(data.frame(xmin = numeric(), xmax = numeric(),
                                        
                                        ymin = numeric(), ymax = numeric()))
  
  modifiable_boxes <- reactiveVal(data.frame())
  
  reference_boxes <- reactiveVal()
  
  
  
  ## 3.5 Pending changes ----
  
  pending_centroids <- reactiveVal(data.frame(
    
    F2_ppm = numeric(0), F1_ppm = numeric(0),
    
    Volume = numeric(0), stain_id = character(0),
    
    stringsAsFactors = FALSE
    
  ))
  
  
  
  pending_boxes <- reactiveVal(data.frame(
    
    xmin = numeric(0), xmax = numeric(0),
    
    ymin = numeric(0), ymax = numeric(0)
    
  ))
  
  
  
  pending_fusions <- reactiveVal(data.frame(
    
    stain_id = character(), F2_ppm = numeric(),
    
    F1_ppm = numeric(), Volume = numeric(),
    
    stringsAsFactors = FALSE
    
  ))
  
  
  
  ## 3.6 Clics ----
  
  last_click_coords <- reactiveVal(NULL)
  
  first_click_for_box <- reactiveVal(NULL)
  
  
  
  ## 3.7 Autres ----
  
  calculated_contour_value <- reactiveVal(NULL)
  
  progress_bar <- reactiveVal(NULL)
  
  data_cc <- reactiveVal(NULL)
  
  plot_list <- reactiveVal(list())
  
  
  
  ## 3.8 Fit results ----
  
  fit_results_data <- reactiveVal(NULL)
  
  last_fit_method <- reactiveVal("sum")
  
  
  
  ## 3.9 Integration method management ----
  
  # Reactive pour obtenir la méthode d'intégration effective
  
  effective_integration_method <- reactive({
    
    auc_method <- input$integration_method
    
    fit_method <- input$integration_method_fit
    
    
    
    # Si une méthode de fit est sélectionnée, l'utiliser
    
    if (!is.null(fit_method) && fit_method != "") {
      
      return(fit_method)
      
    }
    
    # Sinon utiliser AUC (sum)
    
    return("sum")
    
  })
  
  
  
  # Observer: quand on sélectionne AUC, désélectionner Peak Fitting
  
  observeEvent(input$integration_method, {
    
    if (!is.null(input$integration_method) && input$integration_method == "sum") {
      
      updateRadioButtons(session, "integration_method_fit", selected = character(0))
      
    }
    
  }, ignoreInit = TRUE)
  
  
  
  # Observer: quand on sélectionne Peak Fitting, désélectionner AUC
  
  observeEvent(input$integration_method_fit, {
    
    if (!is.null(input$integration_method_fit) && input$integration_method_fit != "") {
      
      updateRadioButtons(session, "integration_method", selected = character(0))
      
    }
    
  }, ignoreInit = TRUE)
  
  
  
  ## 3.10 Integration results storage ----
  
  integration_results <- reactiveVal(NULL)
  
  integration_done <- reactiveVal(FALSE)
  
  
  
  # Output pour conditionalPanel
  
  output$integration_done <- reactive({
    
    !is.null(integration_results())
    
  })
  
  outputOptions(output, "integration_done", suspendWhenHidden = FALSE)
  
  
  
  # Observer: Run Integration button
  
  observeEvent(input$run_integration, {
    
    req(bruker_data(), modifiable_boxes())
    
    
    
    boxes <- modifiable_boxes()
    
    if (is.null(boxes) || nrow(boxes) == 0) {
      
      showNotification("⚠️ No boxes to integrate", type = "warning")
      
      return()
      
    }
    
    
    
    method <- effective_integration_method()
    
    model <- if (method %in% c("gaussian", "voigt")) method else "gaussian"
    
    
    
    status_msg(paste0("🔄 Running integration (", method, " method)..."))
    
    
    
    # Progress bar
    
    progress <- shiny::Progress$new()
    
    on.exit(progress$close())
    
    progress$set(message = "Calculating intensities", value = 0)
    
    
    
    tryCatch({
      
      mat <- bruker_data()$spectrumData
      
      ppm_x <- suppressWarnings(as.numeric(colnames(mat)))
      
      ppm_y <- suppressWarnings(as.numeric(rownames(mat)))
      
      
      
      if (method == "sum") {
        
        # Méthode AUC simple
        
        intensities <- sapply(seq_len(nrow(boxes)), function(i) {
          
          progress$set(value = i / nrow(boxes), detail = paste("Box", i, "/", nrow(boxes)))
          
          box <- boxes[i, ]
          
          x_idx <- which(ppm_x >= box$xmin & ppm_x <= box$xmax)
          
          y_idx <- which(ppm_y >= box$ymin & ppm_y <= box$ymax)
          
          if (length(x_idx) == 0 || length(y_idx) == 0) return(NA_real_)
          
          sum(mat[y_idx, x_idx], na.rm = TRUE)
          
        })
        
        
        
        results <- data.frame(
          
          stain_id = boxes$stain_id,
          
          F2_ppm = (boxes$xmin + boxes$xmax) / 2,
          
          F1_ppm = (boxes$ymin + boxes$ymax) / 2,
          
          intensity = intensities,
          
          method = "sum",
          
          r_squared = NA_real_,
          
          n_peaks = 1L,
          
          stringsAsFactors = FALSE
          
        )
        
        
        
      } else {
        
        # Méthode Peak Fitting
        
        fit_results <- calculate_fitted_volumes(
          
          mat, ppm_x, ppm_y,
          
          boxes[, c("xmin", "xmax", "ymin", "ymax", "stain_id")],
          
          model = model,
          
          progress_callback = function(value, detail) {
            
            progress$set(value = value, detail = detail)
            
          }
          
        )
        
        
        
        results <- data.frame(
          
          stain_id = fit_results$stain_id,
          
          F2_ppm = (boxes$xmin + boxes$xmax) / 2,
          
          F1_ppm = (boxes$ymin + boxes$ymax) / 2,
          
          intensity = fit_results$volume_fitted,
          
          method = fit_results$fit_method,
          
          r_squared = fit_results$r_squared,
          
          n_peaks = fit_results$n_peaks,
          
          stringsAsFactors = FALSE
          
        )
        
        
        
        # Stocker aussi pour l'onglet Fit Quality
        
        fit_results_data(fit_results %>% select(stain_id, r_squared, center_x, center_y, fit_method, n_peaks, is_multiplet))
        
      }
      
      
      
      integration_results(results)
      
      integration_done(TRUE)
      
      last_fit_method(method)
      
      
      
      status_msg(paste0("✅ Integration complete! ", nrow(results), " boxes processed."))
      
      showNotification("✅ Integration complete!", type = "message")
      
      
      
    }, error = function(e) {
      
      status_msg(paste0("❌ Error: ", e$message))
      
      showNotification(paste0("❌ Error: ", e$message), type = "error")
      
    })
    
  })
  
  
  
  # Output: Integration summary
  
  output$integration_summary <- renderText({
    
    results <- integration_results()
    
    if (is.null(results)) return("No results yet.")
    
    
    
    method <- last_fit_method()
    
    n_total <- nrow(results)
    
    
    
    if (method == "sum") {
      
      paste0(
        
        "Method: Sum (AUC)\n",
        
        "Boxes processed: ", n_total, "\n",
        
        "Total intensity: ", format(sum(results$intensity, na.rm = TRUE), big.mark = ",", scientific = FALSE)
        
      )
      
    } else {
      
      n_fitted <- sum(results$method %in% c("gaussian", "voigt", "multiplet_fit"), na.rm = TRUE)
      
      n_fallback <- sum(results$method == "sum_fallback", na.rm = TRUE)
      
      n_multiplets <- sum(results$n_peaks > 1, na.rm = TRUE)
      
      mean_r2 <- mean(results$r_squared, na.rm = TRUE)
      
      
      
      paste0(
        
        "Method: ", method, " (Peak Fitting)\n",
        
        "Boxes processed: ", n_total, "\n",
        
        "  - Successfully fitted: ", n_fitted, "\n",
        
        "  - Multiplets: ", n_multiplets, "\n", 
        
        "  - Fallback to sum: ", n_fallback, "\n",
        
        "Mean R²: ", round(mean_r2, 3), "\n",
        
        "Total intensity: ", format(sum(results$intensity, na.rm = TRUE), big.mark = ",", scientific = FALSE)
        
      )
      
    }
    
  })
  
  
  
  # Download handler for integration results
  
  output$export_integration_results <- downloadHandler(
    
    filename = function() {
      
      method <- last_fit_method()
      
      paste0("integration_results_", method, "_", Sys.Date(), ".csv")
      
    },
    
    content = function(file) {
      
      results <- integration_results()
      
      if (!is.null(results)) {
        
        # Utiliser write.csv2 pour séparateur ";" (compatible Excel français)
        
        write.csv2(results, file, row.names = FALSE)
        
      }
      
    }
    
  )
  
  
  
  
  
  # SECTION 4: PARAMÈTRES PAR TYPE DE SPECTRE ----
  
  
  
  
  
  spectrum_params <- reactive({
    
    switch(input$spectrum_type,
           
           "TOCSY" = list(intensity_threshold = 80000, contour_num = 40, contour_factor = 1.5, 
                          
                          eps_value = 0.0068, neighborhood_size = 3),
           
           "HSQC"  = list(intensity_threshold = 20000, contour_num = 30, contour_factor = 1.3, 
                          
                          eps_value = 0.002, neighborhood_size = 3),
           
           "COSY"  = list(intensity_threshold = 60000, contour_num = 30, contour_factor = 1.3, 
                          
                          eps_value = 0.014, neighborhood_size = 9),
           
           "UFCOSY" = list(intensity_threshold = 50000, contour_num = 70, contour_factor = 1.3, 
                           
                           eps_value = 0.014, neighborhood_size = 2)
           
    )
    
  })
  
  
  
  spectrum_params_CNN <- reactive({
    
    switch(input$spectrum_type,
           
           "TOCSY" = list(int_thres = 0.01, int_prop = 0.001, eps_value = 0.0068,
                          
                          pred_class_thres = 0.00001, batch_size = 64, step = 4),
           
           "UFCOSY" = list(int_thres = 0.001, int_prop = 0.5, eps_value = 0.02,
                           
                           pred_class_thres = 0.001, batch_size = 64, step = 4),
           
           "HSQC" = list(int_thres = 0.001, int_prop = 0.5, eps_value = 0.014,
                         
                         pred_class_thres = 0.001, batch_size = 64, step = 4),
           
           stop("Type de spectre inconnu pour CNN")
           
    )
    
  })
  
  
  
  
  
  # SECTION 5: CHARGEMENT DES DONNÉES ----
  
  
  
  ## 5.1 Directory selection ----
  
  roots <- c(Home = normalizePath("~"), Root = "/")
  
  shinyDirChoose(input, "directory", roots = roots, session = session)
  
  
  
  main_directory <- reactive({
    
    req(input$directory)
    
    
    
    # Parser le chemin sélectionné
    
    dir_path <- tryCatch({
      
      
      
      # Extraire les infos de la sélection
      
      selection <- input$directory
      
      
      
      # Vérifier que la sélection est valide
      
      if (is.null(selection) || length(selection) == 0) {
        
        return(NULL)
        
      }
      
      
      
      # Récupérer la racine sélectionnée
      
      selected_root <- selection$root
      
      
      
      if (is.null(selected_root) || !selected_root %in% names(roots)) {
        
        return(NULL)
        
      }
      
      
      
      # Récupérer le chemin de base de la racine
      
      base_path <- roots[[selected_root]]
      
      
      
      # Récupérer le chemin relatif (liste de dossiers)
      
      path_parts <- selection$path
      
      
      
      if (is.null(path_parts) || length(path_parts) == 0) {
        
        # Seulement la racine sélectionnée
        
        final_path <- base_path
        
      } else {
        
        # Filtrer les parties vides
        
        path_parts <- unlist(path_parts)
        
        path_parts <- path_parts[path_parts != ""]
        
        
        
        if (length(path_parts) == 0) {
          
          final_path <- base_path
          
        } else {
          
          # Construire le chemin complet
          
          if (selected_root == "Root") {
            
            # Pour Root (/), construire le chemin absolu directement
            
            final_path <- paste0("/", paste(path_parts, collapse = "/"))
            
          } else {
            
            # Pour les autres racines, joindre avec le base_path
            
            final_path <- file.path(base_path, paste(path_parts, collapse = "/"))
            
          }
          
        }
        
      }
      
      
      
      # Normaliser le chemin
      
      norm_path <- normalizePath(final_path, mustWork = FALSE)
      
      
      
      # Vérifier que le dossier existe
      
      if (!dir.exists(norm_path)) {
        
        warning(paste("Directory does not exist:", norm_path))
        
        return(NULL)
        
      }
      
      
      
      norm_path
      
    }, error = function(e) {
      
      warning(paste("Error parsing directory:", e$message))
      
      NULL
      
    })
    
    
    
    dir_path
    
  })
  
  
  
  output$selected_dir <- renderPrint({ main_directory() })
  
  
  
  ## 5.2 Subfolders detection ----
  
  subfolders <- reactive({
    
    req(main_directory())
    
    all_subfolders <- list.dirs(main_directory(), recursive = TRUE, full.names = TRUE)
    
    all_subfolders[sapply(all_subfolders, function(folder) {
      
      file.exists(file.path(folder, "acqus")) &&
        
        (file.exists(file.path(folder, "ser")) || file.exists(file.path(folder, "fid")))
      
    })]
    
  })
  
  
  
  ## 5.3 Display available spectra with checkboxes ----
  
  output$available_spectra_ui <- renderUI({
    
    
    
    # Vérifier si un dossier principal est sélectionné
    
    if (is.null(input$directory) || length(input$directory) == 0) {
      
      return(
        
        div(style = "color: #6c757d; font-style: italic; padding: 10px 0;",
            
            "Select a folder to see available spectra.")
        
      )
      
    }
    
    
    
    folders <- tryCatch({
      
      subfolders()
      
    }, error = function(e) {
      
      return(character(0))
      
    })
    
    
    
    if (is.null(folders) || length(folders) == 0) {
      
      return(tags$p(style = "color: #6c757d; font-style: italic;", 
                    
                    "No Bruker spectra found in selected directory"))
      
    }
    
    
    
    # Créer des noms d'affichage courts
    
    display_names <- basename(folders)
    
    
    
    # Si plusieurs ont le même nom, ajouter le dossier parent
    
    if (any(duplicated(display_names))) {
      
      display_names <- sapply(folders, function(f) {
        
        paste0(basename(dirname(f)), "/", basename(f))
        
      })
      
    }
    
    
    
    tagList(
      
      tags$div(
        
        style = "margin-bottom: 10px;",
        
        tags$strong(paste(length(folders), "spectra found"))
        
      ),
      
      
      
      # Boutons Select All / Deselect All
      
      fluidRow(
        
        column(6, actionButton("select_all_spectra", "✅ All", class = "btn-sm btn-outline-success")),
        
        column(6, actionButton("deselect_all_spectra", "❌ None", class = "btn-sm btn-outline-warning"))
        
      ),
      
      
      
      tags$hr(),
      
      
      
      # Liste scrollable avec checkboxes
      
      tags$div(
        
        style = "max-height: 200px; overflow-y: auto; border: 1px solid #ddd; border-radius: 5px; padding: 10px; background-color: #fff;",
        
        checkboxGroupInput(
          
          "spectra_to_load",
          
          label = NULL,
          
          choices = setNames(folders, display_names),
          
          selected = folders  # Tout sélectionné par défaut
          
        )
        
      ),
      
      
      
      tags$hr(),
      
      
      
      # Bouton de chargement
      
      actionButton("load_selected_spectra", "📥 Load Selected", class = "btn-primary btn-block")
      
    )
    
  })
  
  
  
  # Select All
  
  observeEvent(input$select_all_spectra, {
    
    folders <- subfolders()
    
    updateCheckboxGroupInput(session, "spectra_to_load", selected = folders)
    
  })
  
  
  
  # Deselect All
  
  observeEvent(input$deselect_all_spectra, {
    
    updateCheckboxGroupInput(session, "spectra_to_load", selected = character(0))
    
  })
  
  
  
  ## 5.4 Load selected spectra ----
  
  observeEvent(input$load_selected_spectra, {
    
    req(input$spectra_to_load)
    
    
    
    folders <- input$spectra_to_load
    
    
    
    if (length(folders) == 0) {
      
      showNotification("⚠️ No spectra selected", type = "warning")
      
      return()
      
    }
    
    
    
    status_msg(paste("🔄 Loading", length(folders), "spectra..."))
    
    progress <- shiny::Progress$new()
    
    progress$set(message = "Loading spectra", value = 0)
    
    on.exit(progress$close(), add = TRUE)
    
    
    
    all_data <- list()
    
    for (i in seq_along(folders)) {
      
      sub <- folders[[i]]
      
      data_path <- file.path(sub, "pdata", "1")
      
      progress$inc(1 / length(folders), detail = paste0(basename(sub), " (", i, "/", length(folders), ")"))
      
      
      
      if (!dir.exists(data_path)) next
      
      
      
      data <- tryCatch({
        
        read_bruker_cached(data_path, dim = "2D")
        
      }, error = function(e) {
        
        showNotification(paste("❌ Error reading", basename(sub)), type = "error")
        
        NULL
        
      })
      
      
      
      if (!is.null(data)) all_data[[sub]] <- data
      
    }
    
    
    
    spectra_list(all_data)
    
    updateSelectInput(session, "selected_subfolder", 
                      
                      choices = setNames(names(all_data), basename(names(all_data))))
    
    
    
    if (length(all_data) > 0) {
      
      bruker_data(all_data[[1]])
      
      showNotification(paste("✅", length(all_data), "spectra loaded"), type = "message")
      
      status_msg(paste("✅", length(all_data), "spectra loaded"))
      
    } else {
      
      status_msg("⚠️ No valid spectra found")
      
    }
    
  })
  
  
  
  ## 5.5 Subfolder selector ----
  
  output$subfolder_selector <- renderUI({
    
    spectra <- spectra_list()
    
    
    
    # Si pas de spectres chargés, afficher un message
    
    if (is.null(spectra) || length(spectra) == 0) {
      
      return(
        
        div(style = "color: #6c757d; font-style: italic; padding: 10px 0;",
            
            "No spectra loaded yet. Load data first.")
        
      )
      
    }
    
    
    
    subfolder_names <- names(spectra)
    
    
    
    # Vérifier que les noms sont valides
    
    if (is.null(subfolder_names) || length(subfolder_names) == 0) {
      
      return(
        
        div(style = "color: #dc3545;",
            
            "Error: No valid spectra names found.")
        
      )
      
    }
    
    
    
    selectInput("selected_subfolder", "Chosen spectrum:", 
                
                choices = setNames(subfolder_names, basename(subfolder_names)))
    
  })
  
  
  
  ## 5.6 Subfolder change ----
  
  observeEvent(input$selected_subfolder, {
    
    req(spectra_list(), spectra_plots())
    
    selected <- input$selected_subfolder
    
    
    
    if (is.null(selected) || !selected %in% names(spectra_list())) return()
    
    
    
    cached_plots <- plot_cache()
    
    cache_key <- paste0(selected, "_boxes:0_centroids:0")
    
    
    
    if (!is.null(cached_plots[[cache_key]])) {
      
      bruker_data(spectra_list()[[selected]])
      
      contour_plot_base(spectra_plots()[[selected]])
      
      nmr_plot(cached_plots[[cache_key]])
      
      status_msg(paste0("✅ Spectrum loaded: ", basename(selected)))
      
      return(invisible(NULL))
      
    }
    
    
    
    progress <- shiny::Progress$new()
    
    on.exit(progress$close(), add = TRUE)
    
    progress$set(message = paste0("Loading: ", basename(selected)), value = 0)
    
    
    
    bruker_data(spectra_list()[[selected]])
    
    progress$inc(0.3)
    
    
    
    if (!is.null(reference_boxes())) {
      
      fixed_boxes(reference_boxes())
      
    } else {
      
      fixed_boxes(data.frame(xmin = numeric(), xmax = numeric(), 
                             
                             ymin = numeric(), ymax = numeric()))
      
    }
    
    
    
    progress$inc(0.3)
    
    contour_plot_base(spectra_plots()[[selected]])
    
    refresh_nmr_plot(force_recalc = FALSE)
    
    progress$inc(0.4)
    
    
    
    status_msg(paste0("✅ Spectrum selected: ", basename(selected)))
    
  })
  
  
  
  # SECTION 6: GÉNÉRATION DES PLOTS ----
  
  
  
  ## 6.1 Bounding boxes data (reactive) ----
  
  bounding_boxes_data <- reactive({
    
    req(modifiable_boxes(), bruker_data())
    
    
    
    boxes <- modifiable_boxes()
    
    if (is.null(boxes) || nrow(boxes) == 0) {
      
      return(data.frame(xmin = numeric(0), xmax = numeric(0),
                        
                        ymin = numeric(0), ymax = numeric(0),
                        
                        stain_id = character(0), Volume = numeric(0)))
      
    }
    
    
    
    cache_key <- paste0(input$selected_subfolder, "_", 
                        
                        digest::digest(boxes[, c("xmin", "xmax", "ymin", "ymax")]))
    
    
    
    cached <- box_intensity_cache()
    
    if (!is.null(cached[[cache_key]])) return(cached[[cache_key]])
    
    
    
    mat <- bruker_data()$spectrumData
    
    if (is.null(mat)) return(boxes)
    
    
    
    ppm_x <- suppressWarnings(as.numeric(colnames(mat)))
    
    ppm_y <- suppressWarnings(as.numeric(rownames(mat)))
    
    
    
    if (!"stain_id" %in% names(boxes)) {
      
      boxes$stain_id <- paste0("box_", seq_len(nrow(boxes)))
      
    }
    
    
    
    boxes$Volume <- get_box_intensity(mat, ppm_x, ppm_y, boxes)
    
    boxes$Volume[is.na(boxes$Volume)] <- 0
    
    
    
    cached[[cache_key]] <- boxes
    
    box_intensity_cache(cached)
    
    
    
    boxes
    
  })
  
  
  
  ## 6.2 Refresh NMR plot ----
  
  refresh_nmr_plot <- function(force_recalc = FALSE) {
    
    req(contour_plot_base(), bruker_data())
    
    
    
    cache_key <- paste0(input$selected_subfolder,
                        
                        "_boxes:", nrow(modifiable_boxes() %||% data.frame()),
                        
                        "_centroids:", nrow(centroids_data() %||% data.frame()))
    
    
    
    cached_plots <- plot_cache()
    
    
    
    if (!force_recalc && !is.null(cached_plots[[cache_key]])) {
      
      nmr_plot(cached_plots[[cache_key]])
      
      return(invisible(NULL))
      
    }
    
    
    
    plot_base <- contour_plot_base()
    
    
    
    # Ajouter bounding boxes
    
    boxes <- tryCatch(bounding_boxes_data(), error = function(e) NULL)
    
    
    
    if (!is.null(boxes) && nrow(boxes) > 0) {
      
      bbox_path_df <- tryCatch(make_bbox_outline(boxes), error = function(e) NULL)
      
      
      
      if (!is.null(bbox_path_df) && nrow(bbox_path_df) > 0) {
        
        plot_base <- plot_base +
          
          geom_path(data = bbox_path_df, 
                    
                    aes(x = x, y = y, group = group), 
                    
                    color = "red", linewidth = 0.5, inherit.aes = FALSE)
        
      }
      
    }
    
    
    
    # Ajouter centroids
    
    centrs <- imported_centroids() %||% centroids_data()
    
    
    
    if (!is.null(centrs) && nrow(centrs) > 0) {
      
      centrs_clean <- centrs
      
      centrs_clean$F2_ppm <- as.numeric(centrs_clean$F2_ppm)
      
      centrs_clean$F1_ppm <- as.numeric(centrs_clean$F1_ppm)
      
      
      
      if ("Volume" %in% names(centrs_clean)) {
        
        centrs_clean$intensity_plot <- as.numeric(centrs_clean$Volume)
        
        centrs_clean$intensity_plot[is.na(centrs_clean$intensity_plot)] <- 0
        
        centrs_clean$intensity_plot[is.infinite(centrs_clean$intensity_plot)] <- 0
        
        centrs_clean <- centrs_clean[!is.na(centrs_clean$F2_ppm) & !is.na(centrs_clean$F1_ppm), ]
        
        
        
        if (nrow(centrs_clean) > 0) {
          
          plot_base <- plot_base +
            
            geom_point(data = centrs_clean, 
                       
                       aes(x = F2_ppm, y = F1_ppm, color = intensity_plot),
                       
                       size = 1.2, inherit.aes = FALSE) +
            
            scale_color_gradient(low = "blue", high = "red", name = "Intensity")
          
        }
        
      } else {
        
        centrs_clean <- centrs_clean[!is.na(centrs_clean$F2_ppm) & !is.na(centrs_clean$F1_ppm), ]
        
        if (nrow(centrs_clean) > 0) {
          
          plot_base <- plot_base +
            
            geom_point(data = centrs_clean, aes(x = F2_ppm, y = F1_ppm),
                       
                       color = "red", size = 1.2, inherit.aes = FALSE)
          
        }
        
      }
      
    }
    
    
    
    cached_plots[[cache_key]] <- plot_base
    
    plot_cache(cached_plots)
    
    nmr_plot(plot_base)
    
  }
  
  
  
  ## 6.3 Generate plots button ----
  
  observeEvent(input$generate_plot, {
    
    # Vérifications initiales
    
    spectra <- spectra_list()
    
    
    
    if (is.null(spectra) || length(spectra) == 0) {
      
      showNotification("⚠️ No spectra loaded. Please load data first.", type = "warning")
      
      return()
      
    }
    
    
    
    spectra_names <- names(spectra)
    
    if (is.null(spectra_names) || length(spectra_names) == 0) {
      
      showNotification("⚠️ Spectra have no names.", type = "warning")
      
      # Créer des noms par défaut
      
      spectra_names <- paste0("spectrum_", seq_along(spectra))
      
      names(spectra) <- spectra_names
      
      spectra_list(spectra)
      
    }
    
    
    
    status_msg("🔄 Generating plots...")
    
    params <- spectrum_params()
    
    
    
    n <- length(spectra)
    
    if (n == 0) {
      
      showNotification("⚠️ No spectra to process.", type = "warning")
      
      return()
      
    }
    
    
    
    progress <- shiny::Progress$new()
    
    on.exit(progress$close())
    
    progress$set(message = "Processing spectra", value = 0)
    
    
    
    start_time <- Sys.time()
    
    all_results <- vector("list", n)  # Pré-allouer la liste avec la bonne taille
    
    
    
    for (i in seq_len(n)) {
      
      data <- spectra[[i]]
      
      spectrum_name <- spectra_names[i]
      
      
      
      # Vérifier que les données sont valides
      
      if (is.null(data) || is.null(data$spectrumData)) {
        
        showNotification(paste("⚠️ Skipping invalid spectrum:", spectrum_name), type = "warning")
        
        all_results[[i]] <- NULL
        
        next
        
      }
      
      
      
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      
      avg_time <- if (i > 1) elapsed / (i - 1) else 0
      
      remaining <- round(avg_time * (n - i))
      
      time_msg <- if (i > 1) paste("⏱️ ~", remaining, "sec remaining") else ""
      
      
      
      progress$inc(1/n, detail = paste("Processing", basename(spectrum_name), time_msg))
      
      
      
      result <- tryCatch({
        
        find_nmr_peak_centroids_optimized(
          
          data$spectrumData,
          
          spectrum_type = input$spectrum_type,
          
          intensity_threshold = modulate_threshold(input$contour_start) %||% 
            
            modulate_threshold(calculated_contour_value()),
          
          contour_start = input$contour_start %||% calculated_contour_value(),
          
          contour_num = params$contour_num,
          
          contour_factor = params$contour_factor,
          
          f2_exclude_range = c(4.7, 5.0)
          
        )
        
      }, error = function(e) {
        
        showNotification(paste("❌ Error:", basename(spectrum_name), "-", e$message), type = "error")
        
        NULL
        
      })
      
      
      
      all_results[[i]] <- result
      
    }
    
    
    
    # Extraire les plots (garder NULL pour les échecs)
    
    all_plots <- lapply(all_results, function(res) {
      
      if (!is.null(res) && !is.null(res$plot)) res$plot else NULL
      
    })
    
    
    
    # Assigner les noms seulement si les longueurs correspondent
    
    if (length(all_plots) == length(spectra_names)) {
      
      names(all_plots) <- spectra_names
      
    } else {
      
      showNotification("⚠️ Mismatch in plots/names length", type = "warning")
      
    }
    
    spectra_plots(all_plots)
    
    
    
    # Trouver le premier résultat valide
    
    first_valid_idx <- which(sapply(all_results, function(x) !is.null(x) && !is.null(x$plot)))[1]
    
    
    
    if (!is.na(first_valid_idx) && length(all_results) > 0) {
      
      if (length(all_results) == length(spectra_names)) {
        
        names(all_results) <- spectra_names
        
      }
      
      result_data_list(all_results)
      
      result_data(all_results[[first_valid_idx]])
      
      
      
      # Vérifier que le plot existe avant de l'utiliser
      
      if (!is.null(all_results[[first_valid_idx]]$plot)) {
        
        contour_plot_base(all_results[[first_valid_idx]]$plot + labs(title = ""))
        
        refresh_nmr_plot()
        
      }
      
      
      
      n_success <- sum(sapply(all_results, function(x) !is.null(x)))
      
      showNotification(paste("✅", n_success, "/", n, "plots generated"), type = "message")
      
      status_msg(paste("✅", n_success, "plots generated"))
      
    } else {
      
      showNotification("❌ No valid plots could be generated", type = "error")
      
      status_msg("❌ Plot generation failed")
      
    }
    
    
    
    shinyjs::hide("loading_message")
    
  })
  
  
  
  ## 6.4 Calculate contour threshold ----
  
  observeEvent(input$calculate_contour, {
    
    req(bruker_data())
    
    mat <- bruker_data()$spectrumData
    
    seuil <- switch(input$seuil_method,
                    
                    "max_pct" = seuil_max_pourcentage(mat, pourcentage = input$pct_val),
                    
                    "bruit_mult" = seuil_bruit_multiplicatif(mat, facteur = input$bruit_mult),
                    
                    { showNotification("❌ Unknown method", type = "error"); return(NULL) }
                    
    )
    
    calculated_contour_value(seuil)
    
    showNotification(paste0("✅ Threshold: ", round(seuil, 2)), type = "message")
    
  })
  
  
  
  # SECTION 7: PEAK PICKING ----
  
  
  
  ## 7.1 Peak picking (Max method) ----
  
  observeEvent(input$generate_centroids, {
    
    req(input$selected_subfolder, result_data_list(), bruker_data())
    
    params <- spectrum_params()
    
    
    
    all_results <- result_data_list()
    
    selected_result <- all_results[[input$selected_subfolder]]
    
    
    
    if (is.null(selected_result)) {
      
      showNotification("⚠️ No result found", type = "error")
      
      return()
      
    }
    
    
    
    # Message de progression - Étape 1
    
    status_msg("🔄 [1/4] Preparing data...")
    
    
    
    selected_spectrum <- bruker_data()$spectrumData
    
    if (is.null(selected_spectrum)) {
      
      showNotification("⚠️ Spectrum not found", type = "error")
      
      return()
      
    }
    
    
    
    keep_ranges <- parse_keep_peak_ranges(input$keep_peak_ranges_text)
    
    
    
    if (input$disable_clustering) {
      
      # Mode sans clustering
      
      status_msg("🔄 [2/4] Detecting local maxima (no clustering)...")
      
      
      
      result_peaks <- tryCatch({
        
        peak_pick_2d_nt2(
          
          bruker_data = selected_spectrum,
          
          threshold_value = input$contour_start,
          
          neighborhood_size = params$neighborhood_size,
          
          f2_exclude_range = c(4.7, 5.0),
          
          keep_peak_ranges = keep_ranges,
          
          spectrum_type = "TOCSY",
          
          diagnose_zones = c(0.9, 1.6),
          
          diagnose_radius = 0.1
          
        )
        
      }, error = function(e) {
        
        showNotification(paste("❌ Error:", e$message), type = "error")
        
        return(NULL)
        
      })
      
      
      
      if (is.null(result_peaks)) return()
      
      
      
      status_msg("🔄 [3/4] Processing peaks...")
      
      centroids_data(result_peaks$peaks)
      
      
      
      # Vérifier que bounding_boxes existe et a les bonnes colonnes
      
      if (!is.null(result_peaks$bounding_boxes) && nrow(result_peaks$bounding_boxes) > 0) {
        
        required_cols <- c("xmin", "xmax", "ymin", "ymax", "stain_id")
        
        if (all(required_cols %in% names(result_peaks$bounding_boxes))) {
          
          box_coords_only <- result_peaks$bounding_boxes[, required_cols, drop = FALSE]
          
        } else {
          
          box_coords_only <- data.frame(xmin = numeric(0), xmax = numeric(0), 
                                        
                                        ymin = numeric(0), ymax = numeric(0), 
                                        
                                        stain_id = character(0))
          
        }
        
      } else {
        
        box_coords_only <- data.frame(xmin = numeric(0), xmax = numeric(0), 
                                      
                                      ymin = numeric(0), ymax = numeric(0), 
                                      
                                      stain_id = character(0))
        
      }
      
      
      
    } else {
      
      # Mode avec clustering
      
      status_msg("🔄 [2/4] Detecting peaks + DBSCAN clustering...")
      
      
      
      result1 <- tryCatch({
        
        process_nmr_centroids(
          
          rr_data = selected_spectrum,
          
          contour_data = selected_result$contour_data,
          
          intensity_threshold = modulate_threshold(input$contour_start) %||% 
            
            modulate_threshold(calculated_contour_value()),
          
          contour_num = params$contour_num,
          
          contour_factor = params$contour_factor,
          
          eps_value = input$eps_value,
          
          keep_peak_ranges = keep_ranges,
          
          spectrum_type = input$spectrum_type
          
        )
        
      }, error = function(e) {
        
        showNotification(paste("❌ Error:", e$message), type = "error")
        
        NULL
        
      })
      
      
      
      if (is.null(result1)) {
        
        return()
        
      }
      
      
      
      status_msg("🔄 [3/4] Processing centroids...")
      
      centroids_data(result1$centroids)
      
      
      
      # Vérifier que bounding_boxes existe et a les bonnes colonnes
      
      if (!is.null(result1$bounding_boxes) && nrow(result1$bounding_boxes) > 0) {
        
        required_cols <- c("xmin", "xmax", "ymin", "ymax", "stain_id")
        
        if (all(required_cols %in% names(result1$bounding_boxes))) {
          
          box_coords_only <- result1$bounding_boxes[, required_cols, drop = FALSE]
          
        } else {
          
          box_coords_only <- data.frame(xmin = numeric(0), xmax = numeric(0), 
                                        
                                        ymin = numeric(0), ymax = numeric(0), 
                                        
                                        stain_id = character(0))
          
        }
        
      } else {
        
        box_coords_only <- data.frame(xmin = numeric(0), xmax = numeric(0), 
                                      
                                      ymin = numeric(0), ymax = numeric(0), 
                                      
                                      stain_id = character(0))
        
      }
      
    }
    
    
    
    status_msg("🔄 [4/4] Updating plot...")
    
    
    
    fixed_boxes(box_coords_only)
    
    modifiable_boxes(fixed_boxes())
    
    reference_boxes(fixed_boxes())
    
    contour_plot_base(selected_result$plot + labs(title = ""))
    
    refresh_nmr_plot()
    
    
    
    # Résumé final
    
    n_peaks <- nrow(centroids_data() %||% data.frame())
    
    n_boxes <- nrow(box_coords_only)
    
    status_msg(paste0("✅ Peak picking complete: ", n_peaks, " peaks, ", n_boxes, " boxes"))
    
    showNotification(paste0("✅ Found ", n_peaks, " peaks and ", n_boxes, " boxes"), type = "message", duration = 4)
    
  })
  
  
  
  ## 7.2 Peak picking (CNN method) ----
  
  # observeEvent(input$generate_centroids_cnn, {
  
  #   req(bruker_data(), input$selected_subfolder)
  
  #   
  
  #   status_msg("🔄 [1/5] Preparing CNN input...")
  
  #   
  
  #   selected_spectrum <- bruker_data()$spectrumData
  
  #   if (is.null(selected_spectrum)) {
  
  #     showNotification("⚠️ Spectrum not found", type = "error")
  
  #     return()
  
  #   }
  
  #   
  
  #   status_msg("🔄 [2/5] Normalizing spectrum...")
  
  #   rr_abs <- abs(selected_spectrum)
  
  #   rr_norm <- (rr_abs - min(rr_abs)) / (max(rr_abs) - min(rr_abs))
  
  #   
  
  #   # DEBUG: Vérifier les inputs
  
  #   cat("=== DEBUG CNN ===\n")
  
  #   cat("rr_norm dim:", dim(rr_norm), "\n")
  
  #   cat("rr_norm class:", class(rr_norm), "\n")
  
  #   cat("model class:", class(new_model)[1], "\n")
  
  #   cat("params:", names(spectrum_params_CNN()), "\n")
  
  #   cat("threshold_class:", spectrum_params_CNN()$pred_class_thres, "\n")
  
  #   cat("batch_size:", spectrum_params_CNN()$batch_size, "\n")
  
  #   
  
  #   status_msg("🔄 [3/5] Running CNN detection...")
  
  #   
  
  #   result_peaks <- tryCatch({
  
  #     run_cnn_peak_picking(
  
  #       rr_norm = rr_norm,
  
  #       method = "batch",
  
  #       model = new_model,
  
  #       params = spectrum_params_CNN(),
  
  #       threshold_class = spectrum_params_CNN()$pred_class_thres,
  
  #       batch_size = spectrum_params_CNN()$batch_size,
  
  #       step = 4
  
  #     )
  
  #   }, error = function(e) {
  
  #     cat("❌ ERROR:", e$message, "\n")
  
  #     cat("Call:", deparse(e$call), "\n")
  
  #     showNotification(paste("❌ CNN error:", e$message), type = "error")
  
  #     return(NULL)
  
  #   })
  
  #   
  
  #   if (is.null(result_peaks)) return()
  
  #   
  
  #   status_msg("🔄 [4/5] Processing CNN results...")
  
  #   
  
  #   if (!is.null(result_peaks$boxes) && nrow(result_peaks$boxes) > 0) {
  
  #     # Créer les peaks (centroïdes) à partir des centres des bounding boxes
  
  #     result_peaks$peaks <- result_peaks$boxes %>%
  
  #       dplyr::transmute(
  
  #         F1 = as.integer(round(cy_ppm)), F2 = as.integer(round(cx_ppm)),
  
  #         F1_ppm = cy_ppm, F2_ppm = cx_ppm,
  
  #         Volume = intensity, cluster_db = cluster_db
  
  #       )
  
  #     
  
  #     # IMPORTANT: Assigner les centroïdes pour qu'ils s'affichent sur le plot
  
  #     centroids_data(result_peaks$peaks)
  
  #     
  
  #     # Préparer les bounding boxes
  
  #     result_peaks$boxes$stain_id <- seq_len(nrow(result_peaks$boxes))
  
  #     result_peaks$boxes$Volume <- result_peaks$boxes$intensity
  
  #     result_peaks$boxes$xmin <- result_peaks$boxes$xmin_ppm
  
  #     result_peaks$boxes$xmax <- result_peaks$boxes$xmax_ppm
  
  #     result_peaks$boxes$ymin <- result_peaks$boxes$ymin_ppm
  
  #     result_peaks$boxes$ymax <- result_peaks$boxes$ymax_ppm
  
  #     
  
  #     box_coords_only <- result_peaks$boxes[, c("xmin", "xmax", "ymin", "ymax", "stain_id")]
  
  #     fixed_boxes(box_coords_only)
  
  #     modifiable_boxes(fixed_boxes())
  
  #     reference_boxes(fixed_boxes())
  
  #   }
  
  #   
  
  #   status_msg("🔄 [5/5] Updating plot...")
  
  #   
  
  #   contour_plot_base(result_data_list()[[input$selected_subfolder]]$plot + labs(title = ""))
  
  #   refresh_nmr_plot()
  
  #   
  
  #   # Résumé final
  
  #   n_boxes <- nrow(result_peaks$boxes %||% data.frame())
  
  #   status_msg(paste0("✅ CNN complete: ", n_boxes, " peaks detected"))
  
  #   showNotification(paste0("✅ CNN found ", n_boxes, " peaks"), type = "message", duration = 4)
  
  # })
  
  
  
  
  
  
  
  # SECTION 8: GESTION MANUELLE (CENTROIDS, BOXES, CLICS) ----
  
  ## 8.1 Add manual centroid ----
  
  observeEvent(input$add_manual_centroid, {
    
    req(input$manual_f2, input$manual_f1)
    
    
    
    current <- centroids_data()
    
    if (is.null(current)) {
      
      current <- data.frame(F2_ppm = numeric(0), F1_ppm = numeric(0), 
                            
                            Volume = numeric(0), stain_id = character(0))
      
    }
    
    
    
    existing_ids <- current$stain_id[grepl("^man", current$stain_id)]
    
    man_number <- if (length(existing_ids) == 0) 1 else 
      
      max(as.integer(sub("man", "", existing_ids)), na.rm = TRUE) + 1
    
    
    
    contour_dat <- result_data()$contour_data
    
    eps <- input$eps_value
    
    estimated_intensity <- 0
    
    
    
    if (!is.null(contour_dat) && nrow(contour_dat) > 0) {
      
      local_points <- contour_dat[
        
        abs(contour_dat$x + input$manual_f2) <= eps*16 &
          
          abs(contour_dat$y + input$manual_f1) <= eps*16, , drop = FALSE]
      
      estimated_intensity <- sum(local_points$level, na.rm = TRUE)
      
    }
    
    
    
    new_point <- data.frame(
      
      F2_ppm = as.numeric(input$manual_f2),
      
      F1_ppm = as.numeric(input$manual_f1),
      
      Volume = as.numeric(estimated_intensity),
      
      stain_id = paste0("man", man_number),
      
      status = "add", stringsAsFactors = FALSE
      
    )
    
    
    
    missing_cols <- setdiff(colnames(current), colnames(new_point))
    
    for (mc in missing_cols) new_point[[mc]] <- NA
    
    
    
    pending_centroids(dplyr::bind_rows(pending_centroids(), new_point))
    
    showNotification(paste("✅ Peak added:", new_point$stain_id), type = "message")
    
  })
  
  
  
  ## 8.2 Add manual box ----
  
  observeEvent(input$add_manual_bbox, {
    
    req(input$manual_xmin, input$manual_xmax, input$manual_ymin, input$manual_ymax)
    
    
    
    current_boxes <- modifiable_boxes()
    
    existing_manual_ids <- if (!is.null(current_boxes) && nrow(current_boxes) > 0 && 
                               
                               "stain_id" %in% names(current_boxes)) {
      
      current_boxes$stain_id[grepl("^manual_box", current_boxes$stain_id)]
      
    } else character(0)
    
    
    
    manual_number <- if (length(existing_manual_ids) == 0) 1 else 
      
      max(as.integer(sub("manual_box", "", existing_manual_ids)), na.rm = TRUE) + 1
    
    
    
    new_box <- data.frame(
      
      xmin = input$manual_xmin, xmax = input$manual_xmax,
      
      ymin = input$manual_ymin, ymax = input$manual_ymax,
      
      stain_id = paste0("manual_box", manual_number),
      
      Volume = NA_real_, status = "add", stringsAsFactors = FALSE
      
    )
    
    
    
    pending_boxes(dplyr::bind_rows(pending_boxes(), new_box))
    
    showNotification(paste("🟦 Box added:", new_box$stain_id), type = "message")
    
  })
  
  
  
  ## 8.3 Delete centroid ----
  
  observeEvent(input$delete_centroid, {
    
    selected <- input$centroid_table_rows_selected
    
    if (length(selected) > 0) {
      
      current <- centroids_data()
      
      to_delete <- current[selected, , drop = FALSE]
      
      to_delete$status <- "delete"
      
      pending_centroids(bind_rows(pending_centroids(), to_delete))
      
      centroids_data(current[-selected, , drop = FALSE])
      
      showNotification("🗑️ Centroid marked for deletion", type = "message")
      
    } else {
      
      showNotification("⚠️ Select a centroid first", type = "warning")
      
    }
    
  })
  
  
  
  ## 8.4 Delete box ----
  
  observeEvent(input$delete_bbox, {
    
    selected <- input$bbox_table_rows_selected
    
    if (length(selected) > 0) {
      
      current <- modifiable_boxes()
      
      to_delete <- current[selected, , drop = FALSE]
      
      to_delete$status <- "delete"
      
      
      
      # Ajouter aux pending au lieu de supprimer directement
      
      pending_boxes(dplyr::bind_rows(pending_boxes(), to_delete))
      
      
      
      showNotification(paste("🗑️ Box marked for deletion:", to_delete$stain_id[1]), type = "message")
      
    } else {
      
      showNotification("⚠️ Select a box first", type = "warning")
      
    }
    
  })
  
  
  
  ## 8.4b Edit/Move box ----
  
  # Variables pour stocker l'état de l'édition
  
  selected_box_for_edit <- reactiveVal(NULL)
  
  selected_box_index <- reactiveVal(NULL)
  
  original_box_coords <- reactiveVal(NULL)  # Stocker les coordonnées originales
  
  box_has_been_modified <- reactiveVal(FALSE)  # Flag pour savoir si modification
  
  preview_trace_added <- reactiveVal(FALSE)  # Flag pour savoir si une trace preview existe
  
  
  
  # Fonction pour mettre à jour la preview de la box
  
  update_box_preview <- function() {
    
    box_idx <- selected_box_index()
    
    if (is.null(box_idx)) return()
    
    
    
    # Marquer comme modifié si les coordonnées ont changé
    
    original <- original_box_coords()
    
    if (!is.null(original)) {
      
      if (input$edit_box_xmin != original$xmin ||
          
          input$edit_box_xmax != original$xmax ||
          
          input$edit_box_ymin != original$ymin ||
          
          input$edit_box_ymax != original$ymax) {
        
        box_has_been_modified(TRUE)
        
      }
      
    }
    
    
    
    # Coordonnées de preview (inversées pour le plot)
    
    x0 <- -input$edit_box_xmin
    
    x1 <- -input$edit_box_xmax
    
    y0 <- -input$edit_box_ymin
    
    y1 <- -input$edit_box_ymax
    
    
    
    # S'assurer que x0 < x1 et y0 < y1
    
    if (x0 > x1) { tmp <- x0; x0 <- x1; x1 <- tmp }
    
    if (y0 > y1) { tmp <- y0; y0 <- y1; y1 <- tmp }
    
    
    
    # Mettre à jour la trace preview via plotlyProxy
    
    if (preview_trace_added()) {
      
      plotlyProxy("interactivePlot", session) %>%
        
        plotlyProxyInvoke(
          
          "restyle",
          
          list(
            
            x = list(c(x0, x1, x1, x0, x0)),
            
            y = list(c(y0, y0, y1, y1, y0))
            
          ),
          
          list(as.integer(preview_trace_index()))  
          
        )
      
    }
    
  }
  
  
  
  # Index de la trace preview dans le plot
  
  preview_trace_index <- reactiveVal(NULL)
  
  
  
  # Quand une box est sélectionnée dans la table, charger ses valeurs
  
  # Note: Si plusieurs boxes sont sélectionnées, on n'édite que la première
  
  observeEvent(input$bbox_table_rows_selected, {
    
    selected <- input$bbox_table_rows_selected
    
    
    
    # Si plusieurs lignes sélectionnées, on prend seulement la première pour l'édition
    
    # (les suppressions multiples se font via le bouton "Delete Selected")
    
    if (length(selected) > 0 && !is.null(selected)) {
      
      # Prendre uniquement la première sélection pour l'édition
      
      first_selected <- selected[1]
      
      
      
      boxes <- bounding_boxes_data()
      
      if (!is.null(boxes) && nrow(boxes) >= first_selected) {
        
        box <- boxes[first_selected, , drop = FALSE]
        
        selected_box_for_edit(box)
        
        selected_box_index(first_selected)
        
        box_has_been_modified(FALSE)  # Reset le flag de modification
        
        
        
        # Stocker les coordonnées originales
        
        original_box_coords(list(
          
          xmin = box$xmin,
          
          xmax = box$xmax,
          
          ymin = box$ymin,
          
          ymax = box$ymax
          
        ))
        
        
        
        # Mettre à jour les inputs d'édition
        
        updateNumericInput(session, "edit_box_xmin", value = round(box$xmin, 4))
        
        updateNumericInput(session, "edit_box_xmax", value = round(box$xmax, 4))
        
        updateNumericInput(session, "edit_box_ymin", value = round(box$ymin, 4))
        
        updateNumericInput(session, "edit_box_ymax", value = round(box$ymax, 4))
        
        
        
        # Coordonnées pour la preview (en vert pour différencier)
        
        x0 <- -box$xmin
        
        x1 <- -box$xmax
        
        y0 <- -box$ymin
        
        y1 <- -box$ymax
        
        if (x0 > x1) { tmp <- x0; x0 <- x1; x1 <- tmp }
        
        if (y0 > y1) { tmp <- y0; y0 <- y1; y1 <- tmp }
        
        
        
        # Ajouter la trace preview sur le plot EXISTANT
        
        plotlyProxy("interactivePlot", session) %>%
          
          plotlyProxyInvoke(
            
            "addTraces",
            
            list(
              
              x = c(x0, x1, x1, x0, x0),
              
              y = c(y0, y0, y1, y1, y0),
              
              type = "scatter",
              
              mode = "lines",
              
              line = list(color = "lime", width = 3, dash = "dash"),
              
              hoverinfo = "text",
              
              text = paste("Preview:", box$stain_id),
              
              showlegend = FALSE,
              
              name = "preview_box"
              
            )
            
          )
        
        
        
        preview_trace_added(TRUE)
        
        
        
        # Calculer l'index de la trace (dernière trace ajoutée)
        
        # On stocke qu'on a ajouté une trace, on utilisera -1 pour la supprimer
        
      }
      
    } else {
      
      # Désélection - supprimer la preview SANS modifier le plot de base
      
      if (isTRUE(preview_trace_added())) {
        
        # Utiliser un délai pour s'assurer que la suppression s'exécute
        
        plotlyProxy("interactivePlot", session) %>%
          
          plotlyProxyInvoke("deleteTraces", -1L)  # -1L pour supprimer la dernière trace
        
        preview_trace_added(FALSE)
        
      }
      
      
      
      # Reset les variables d'état
      
      selected_box_for_edit(NULL)
      
      selected_box_index(NULL)
      
      original_box_coords(NULL)
      
      box_has_been_modified(FALSE)
      
    }
    
  })
  
  
  
  # Ajouter la modification aux pending
  
  observeEvent(input$apply_box_edit, {
    
    box_to_edit <- selected_box_for_edit()
    
    if (is.null(box_to_edit)) {
      
      showNotification("⚠️ Select a box first", type = "warning")
      
      return()
      
    }
    
    
    
    # Récupérer les nouvelles coordonnées
    
    new_xmin <- input$edit_box_xmin
    
    new_xmax <- input$edit_box_xmax
    
    new_ymin <- input$edit_box_ymin
    
    new_ymax <- input$edit_box_ymax
    
    
    
    # Validation des coordonnées
    
    coords_valid <- TRUE
    
    error_msg <- ""
    
    
    
    # Vérifier que les valeurs ne sont pas NA
    
    if (is.na(new_xmin) || is.na(new_xmax) || is.na(new_ymin) || is.na(new_ymax)) {
      
      coords_valid <- FALSE
      
      error_msg <- "Coordinates cannot be empty (NA)"
      
    }
    
    # Vérifier que les valeurs ne sont pas toutes à 0
    
    else if (new_xmin == 0 && new_xmax == 0 && new_ymin == 0 && new_ymax == 0) {
      
      coords_valid <- FALSE
      
      error_msg <- "All coordinates cannot be zero. Use 'Delete' to remove a box."
      
    }
    
    # Vérifier que xmin < xmax
    
    else if (new_xmin >= new_xmax) {
      
      coords_valid <- FALSE
      
      error_msg <- "xmin must be less than xmax"
      
    }
    
    # Vérifier que ymin < ymax
    
    else if (new_ymin >= new_ymax) {
      
      coords_valid <- FALSE
      
      error_msg <- "ymin must be less than ymax"
      
    }
    
    # Vérifier que la box a une taille minimale raisonnable
    
    else if ((new_xmax - new_xmin) < 0.001 || (new_ymax - new_ymin) < 0.001) {
      
      coords_valid <- FALSE
      
      error_msg <- "Box is too small (min size: 0.001 ppm)"
      
    }
    
    
    
    if (!coords_valid) {
      
      showNotification(paste("❌ Invalid coordinates:", error_msg), type = "error", duration = 5)
      
      return()
      
    }
    
    
    
    # Vérifier si des modifications ont été faites
    
    if (!box_has_been_modified()) {
      
      showNotification("ℹ️ No changes to apply", type = "message")
      
      # Supprimer la preview
      
      if (preview_trace_added()) {
        
        plotlyProxy("interactivePlot", session) %>%
          
          plotlyProxyInvoke("deleteTraces", list(-1))
        
        preview_trace_added(FALSE)
        
      }
      
      selected_box_for_edit(NULL)
      
      selected_box_index(NULL)
      
      original_box_coords(NULL)
      
      box_has_been_modified(FALSE)
      
      return()
      
    }
    
    
    
    # Créer l'entrée pour la modification
    
    edited_box <- data.frame(
      
      xmin = new_xmin,
      
      xmax = new_xmax,
      
      ymin = new_ymin,
      
      ymax = new_ymax,
      
      stain_id = box_to_edit$stain_id,
      
      Volume = NA_real_,
      
      status = "edit",
      
      original_stain_id = box_to_edit$stain_id,
      
      stringsAsFactors = FALSE
      
    )
    
    
    
    # Ajouter aux pending
    
    pending_boxes(dplyr::bind_rows(pending_boxes(), edited_box))
    
    
    
    # Supprimer la preview
    
    if (isTRUE(preview_trace_added())) {
      
      plotlyProxy("interactivePlot", session) %>%
        
        plotlyProxyInvoke("deleteTraces", -1L)
      
      preview_trace_added(FALSE)
      
    }
    
    
    
    # Reset
    
    selected_box_for_edit(NULL)
    
    selected_box_index(NULL)
    
    original_box_coords(NULL)
    
    box_has_been_modified(FALSE)
    
    
    
    showNotification(paste("✏️ Box edit pending:", box_to_edit$stain_id), type = "message")
    
  })
  
  
  
  # Bouton pour annuler l'édition en cours (sans appliquer)
  
  observeEvent(input$cancel_box_edit, {
    
    if (isTRUE(preview_trace_added())) {
      
      plotlyProxy("interactivePlot", session) %>%
        
        plotlyProxyInvoke("deleteTraces", -1L)
      
      preview_trace_added(FALSE)
      
    }
    
    
    
    selected_box_for_edit(NULL)
    
    selected_box_index(NULL)
    
    original_box_coords(NULL)
    
    box_has_been_modified(FALSE)
    
    
    
    showNotification("❌ Edit cancelled", type = "warning", duration = 2)
    
  })
  
  
  
  # Déplacer la box (shift par delta) avec preview
  
  observeEvent(input$move_box_up, {
    
    req(selected_box_for_edit())
    
    delta <- input$move_box_step %||% 0.01
    
    updateNumericInput(session, "edit_box_ymin", value = input$edit_box_ymin - delta)
    
    updateNumericInput(session, "edit_box_ymax", value = input$edit_box_ymax - delta)
    
    box_has_been_modified(TRUE)
    
  })
  
  
  
  observeEvent(input$move_box_down, {
    
    req(selected_box_for_edit())
    
    delta <- input$move_box_step %||% 0.01
    
    updateNumericInput(session, "edit_box_ymin", value = input$edit_box_ymin + delta)
    
    updateNumericInput(session, "edit_box_ymax", value = input$edit_box_ymax + delta)
    
    box_has_been_modified(TRUE)
    
  })
  
  
  
  observeEvent(input$move_box_left, {
    
    req(selected_box_for_edit())
    
    delta <- input$move_box_step %||% 0.01
    
    updateNumericInput(session, "edit_box_xmin", value = input$edit_box_xmin + delta)
    
    updateNumericInput(session, "edit_box_xmax", value = input$edit_box_xmax + delta)
    
    box_has_been_modified(TRUE)
    
  })
  
  
  
  observeEvent(input$move_box_right, {
    
    req(selected_box_for_edit())
    
    delta <- input$move_box_step %||% 0.01
    
    updateNumericInput(session, "edit_box_xmin", value = input$edit_box_xmin - delta)
    
    updateNumericInput(session, "edit_box_xmax", value = input$edit_box_xmax - delta)
    
    box_has_been_modified(TRUE)
    
  })
  
  
  
  # Observer les changements des inputs pour mettre à jour la preview
  
  observeEvent(c(input$edit_box_xmin, input$edit_box_xmax, input$edit_box_ymin, input$edit_box_ymax), {
    
    req(selected_box_index())
    
    req(isTRUE(preview_trace_added()))
    
    
    
    # Coordonnées de preview
    
    x0 <- -input$edit_box_xmin
    
    x1 <- -input$edit_box_xmax
    
    y0 <- -input$edit_box_ymin
    
    y1 <- -input$edit_box_ymax
    
    if (x0 > x1) { tmp <- x0; x0 <- x1; x1 <- tmp }
    
    if (y0 > y1) { tmp <- y0; y0 <- y1; y1 <- tmp }
    
    
    
    # Supprimer et recréer la trace preview
    
    plotlyProxy("interactivePlot", session) %>%
      
      plotlyProxyInvoke("deleteTraces", -1L) %>%
      
      plotlyProxyInvoke(
        
        "addTraces",
        
        list(
          
          x = c(x0, x1, x1, x0, x0),
          
          y = c(y0, y0, y1, y1, y0),
          
          type = "scatter",
          
          mode = "lines",
          
          line = list(color = "lime", width = 3, dash = "dash"),
          
          hoverinfo = "text",
          
          text = "Preview (modified)",
          
          showlegend = FALSE,
          
          name = "preview_box"
          
        )
        
      )
    
    
    
    # Vérifier si modifié par rapport à l'original
    
    original <- original_box_coords()
    
    if (!is.null(original)) {
      
      if (abs(input$edit_box_xmin - original$xmin) > 1e-6 ||
          
          abs(input$edit_box_xmax - original$xmax) > 1e-6 ||
          
          abs(input$edit_box_ymin - original$ymin) > 1e-6 ||
          
          abs(input$edit_box_ymax - original$ymax) > 1e-6) {
        
        box_has_been_modified(TRUE)
        
      }
      
    }
    
  }, ignoreInit = TRUE)
  
  
  
  # Redimensionner la box
  
  observeEvent(input$expand_box, {
    
    req(selected_box_for_edit())
    
    delta <- input$move_box_step %||% 0.01
    
    updateNumericInput(session, "edit_box_xmin", value = input$edit_box_xmin - delta)
    
    updateNumericInput(session, "edit_box_xmax", value = input$edit_box_xmax + delta)
    
    updateNumericInput(session, "edit_box_ymin", value = input$edit_box_ymin - delta)
    
    updateNumericInput(session, "edit_box_ymax", value = input$edit_box_ymax + delta)
    
    box_has_been_modified(TRUE)
    
  })
  
  
  
  observeEvent(input$shrink_box, {
    
    req(selected_box_for_edit())
    
    delta <- input$move_box_step %||% 0.01
    
    updateNumericInput(session, "edit_box_xmin", value = input$edit_box_xmin + delta)
    
    updateNumericInput(session, "edit_box_xmax", value = input$edit_box_xmax - delta)
    
    updateNumericInput(session, "edit_box_ymin", value = input$edit_box_ymin + delta)
    
    updateNumericInput(session, "edit_box_ymax", value = input$edit_box_ymax - delta)
    
    box_has_been_modified(TRUE)
    
  })
  
  
  
  # Output pour afficher quelle box est sélectionnée
  
  output$selected_box_info <- renderText({
    
    box <- selected_box_for_edit()
    
    modified <- box_has_been_modified()
    
    if (is.null(box)) return("No box selected")
    
    status <- if (modified) " (modified)" else ""
    
    sprintf("Editing: %s%s", box$stain_id, status)
    
  })
  
  
  
  ## 8.5 Fuse points ----
  
  observeEvent(input$fuse_btn, {
    
    req(centroids_data())
    
    sel <- event_data("plotly_selected", source = "nmr_plot")
    
    
    
    if (is.null(sel) || nrow(sel) < 2) {
      
      showNotification("⚠️ Select at least 2 points", type = "error")
      
      return()
      
    }
    
    
    
    sel$x <- -sel$x; sel$y <- -sel$y
    
    brushed <- dplyr::semi_join(centroids_data(), sel, by = c("F2_ppm" = "x", "F1_ppm" = "y"))
    
    
    
    if (nrow(brushed) < 2) {
      
      showNotification("⚠️ Selection did not match enough points", type = "error")
      
      return()
      
    }
    
    
    
    first_peak_id <- brushed$stain_id[1]
    
    peak_number <- gsub("[^0-9]", "", first_peak_id)
    
    
    
    fused_point <- data.frame(
      
      stain_id = paste0("fused_point", peak_number),
      
      F2_ppm = mean(brushed$F2_ppm),
      
      F1_ppm = mean(brushed$F1_ppm),
      
      Volume = sum(as.numeric(brushed$Volume), na.rm = TRUE),
      
      stringsAsFactors = FALSE
      
    )
    
    
    
    remaining <- dplyr::anti_join(centroids_data(), brushed, by = c("F2_ppm", "F1_ppm"))
    
    missing_cols <- setdiff(names(remaining), names(fused_point))
    
    for (mc in missing_cols) fused_point[[mc]] <- NA
    
    fused_point <- fused_point[, names(remaining), drop = FALSE]
    
    
    
    centroids_data(rbind(remaining, fused_point))
    
    
    
    if (!is.null(modifiable_boxes()) && nrow(modifiable_boxes()) > 0) {
      
      boxes <- modifiable_boxes()
      
      selected_boxes <- which(boxes$xmin <= max(brushed$F2_ppm) & boxes$xmax >= min(brushed$F2_ppm) & 
                                
                                boxes$ymin <= max(brushed$F1_ppm) & boxes$ymax >= min(brushed$F1_ppm))
      
      removed_boxes <- if (length(selected_boxes) > 0) boxes[selected_boxes, , drop = FALSE] else boxes[0, ]
      
      boxes <- if (length(selected_boxes) > 0) boxes[-selected_boxes, , drop = FALSE] else boxes
      
      
      
      if (nrow(removed_boxes) > 0) {
        
        new_box <- data.frame(xmin = min(removed_boxes$xmin), xmax = max(removed_boxes$xmax),
                              
                              ymin = min(removed_boxes$ymin), ymax = max(removed_boxes$ymax),
                              
                              stain_id = paste0("bbox_fused_point", peak_number))
        
        
        
        # Ajouter les colonnes manquantes à new_box pour matcher boxes
        
        missing_box_cols <- setdiff(names(boxes), names(new_box))
        
        for (mc in missing_box_cols) new_box[[mc]] <- NA
        
        new_box <- new_box[, names(boxes), drop = FALSE]
        
        
        
        boxes <- rbind(boxes, new_box)
        
      }
      
      modifiable_boxes(boxes)
      
      fixed_boxes(boxes)
      
    }
    
    
    
    pending_fusions(bind_rows(pending_fusions(), fused_point))
    
    showNotification("✅ Points fused", type = "message")
    
  })
  
  
  
  ## 8.6 Click handling for two-click box creation ----
  
  observeEvent(event_data("plotly_click", source = "nmr_plot"), {
    
    click_data <- event_data("plotly_click", source = "nmr_plot")
    
    
    
    # Stocker les coordonnées
    
    if (!is.null(click_data) && !is.null(click_data$x) && !is.null(click_data$y) && 
        
        !is.na(click_data$x) && !is.na(click_data$y)) {
      
      last_click_coords(list(F2_ppm = click_data$x, F1_ppm = click_data$y))
      
    }
    
    
    
    # Vérifier les coordonnées
    
    if (is.null(click_data$x) || is.null(click_data$y)) return()
    
    if (is.na(click_data$x) || is.na(click_data$y)) return()
    
    
    
    f2_ppm <- -click_data$x
    
    f1_ppm <- -click_data$y
    
    
    
    click_mode <- input$box_click_mode
    
    
    
    # Mode DELETE: supprimer la box sous le clic
    
    if (!is.null(click_mode) && click_mode == "delete_click") {
      
      boxes <- bounding_boxes_data()
      
      
      
      if (is.null(boxes) || nrow(boxes) == 0) {
        
        showNotification("⚠️ No boxes to delete", type = "warning")
        
        return()
        
      }
      
      
      
      # Trouver la box qui contient le point cliqué
      
      clicked_box_idx <- which(
        
        boxes$xmin <= f2_ppm & boxes$xmax >= f2_ppm &
          
          boxes$ymin <= f1_ppm & boxes$ymax >= f1_ppm
        
      )
      
      
      
      if (length(clicked_box_idx) == 0) {
        
        showNotification("⚠️ No box at this location", type = "warning")
        
        return()
        
      }
      
      
      
      # Si plusieurs boxes se chevauchent, prendre la première (ou la plus petite)
      
      if (length(clicked_box_idx) > 1) {
        
        # Prendre la plus petite box (la plus spécifique)
        
        box_areas <- (boxes$xmax[clicked_box_idx] - boxes$xmin[clicked_box_idx]) * 
          
          (boxes$ymax[clicked_box_idx] - boxes$ymin[clicked_box_idx])
        
        clicked_box_idx <- clicked_box_idx[which.min(box_areas)]
        
      }
      
      
      
      # Marquer la box pour suppression (via pending)
      
      box_to_delete <- boxes[clicked_box_idx, , drop = FALSE]
      
      box_to_delete$status <- "delete"
      
      
      
      pending_boxes(dplyr::bind_rows(pending_boxes(), box_to_delete))
      
      
      
      showNotification(
        
        paste("🗑️ Box", box_to_delete$stain_id, "marked for deletion. Click 'Apply' to confirm."), 
        
        type = "message"
        
      )
      
      return()
      
    }
    
    
    
    # Mode TWO_CLICKS: créer une box avec deux clics
    
    if (!is.null(click_mode) && click_mode == "two_clicks") {
      
      first_click <- first_click_for_box()
      
      
      
      if (is.null(first_click)) {
        
        # Premier clic
        
        first_click_for_box(list(f2 = f2_ppm, f1 = f1_ppm))
        
        showNotification(sprintf("📍 Coin 1: F2=%.3f, F1=%.3f", f2_ppm, f1_ppm), duration = 4)
        
      } else {
        
        # Deuxième clic - créer la boîte
        
        new_box <- data.frame(
          
          xmin = min(first_click$f2, f2_ppm),
          
          xmax = max(first_click$f2, f2_ppm),
          
          ymin = min(first_click$f1, f1_ppm),
          
          ymax = max(first_click$f1, f1_ppm),
          
          stain_id = paste0("click_box_", format(Sys.time(), "%H%M%S")),
          
          Volume = NA_real_,
          
          status = "add",
          
          stringsAsFactors = FALSE
          
        )
        
        
        
        pending_boxes(dplyr::bind_rows(pending_boxes(), new_box))
        
        first_click_for_box(NULL)
        
        
        
        showNotification(sprintf("🟦 Box: F2=[%.3f,%.3f], F1=[%.3f,%.3f]",
                                 
                                 new_box$xmin, new_box$xmax, new_box$ymin, new_box$ymax), 
                         
                         duration = 3)
        
      }
      
    }
    
  }, priority = 10)
  
  
  
  ## 8.7 Cancel first click ----
  
  observeEvent(input$cancel_first_click, {
    
    first_click_for_box(NULL)
    
    showNotification("❌ First click cancelled", type = "warning", duration = 2)
    
  })
  
  
  
  ## 8.8 Apply/Discard changes ----
  
  observeEvent(input$apply_changes, {
    
    current_centroids <- centroids_data()
    
    pending_cents <- pending_centroids()
    
    
    
    # Traiter les pending centroids avec gestion du statut delete
    
    if (!is.null(pending_cents) && nrow(pending_cents) > 0) {
      
      if (is.null(current_centroids)) current_centroids <- data.frame()
      
      
      
      # Vérifier si la colonne status existe
      
      if ("status" %in% names(pending_cents)) {
        
        # Séparer les ajouts et les suppressions
        
        cents_to_add <- pending_cents[is.na(pending_cents$status) | pending_cents$status != "delete", , drop = FALSE]
        
        cents_to_delete <- pending_cents[!is.na(pending_cents$status) & pending_cents$status == "delete", , drop = FALSE]
        
        
        
        # 1. Supprimer les centroids marqués pour suppression
        
        if (nrow(cents_to_delete) > 0 && nrow(current_centroids) > 0) {
          
          # Supprimer par stain_id si disponible, sinon par coordonnées
          
          if ("stain_id" %in% names(cents_to_delete) && "stain_id" %in% names(current_centroids)) {
            
            ids_to_delete <- cents_to_delete$stain_id
            
            current_centroids <- current_centroids[!current_centroids$stain_id %in% ids_to_delete, , drop = FALSE]
            
          }
          
        }
        
        
        
        # 2. Ajouter les nouveaux centroids
        
        if (nrow(cents_to_add) > 0) {
          
          # Retirer la colonne status avant d'ajouter
          
          cents_to_add$status <- NULL
          
          current_centroids <- dplyr::bind_rows(current_centroids, cents_to_add)
          
        }
        
      } else {
        
        # Pas de colonne status, tout ajouter
        
        current_centroids <- dplyr::bind_rows(current_centroids, pending_cents)
        
      }
      
      
      
      centroids_data(current_centroids)
      
    }
    
    
    
    current_boxes <- modifiable_boxes()
    
    pending_bxs <- pending_boxes()
    
    
    
    # DEBUG
    
    message("=== APPLY CHANGES DEBUG ===")
    
    message("Current boxes count: ", if(is.null(current_boxes)) 0 else nrow(current_boxes))
    
    message("Pending boxes count: ", if(is.null(pending_bxs)) 0 else nrow(pending_bxs))
    
    if (!is.null(pending_bxs) && nrow(pending_bxs) > 0) {
      
      message("Pending statuses: ", paste(pending_bxs$status, collapse = ", "))
      
    }
    
    
    
    if (!is.null(pending_bxs) && nrow(pending_bxs) > 0) {
      
      
      
      # S'assurer que la colonne status existe
      
      if (!"status" %in% names(pending_bxs)) {
        
        pending_bxs$status <- "add"
        
      }
      
      
      
      # Remplacer NA par "add" dans status
      
      pending_bxs$status[is.na(pending_bxs$status)] <- "add"
      
      
      
      # Séparer les différents types d'opérations
      
      boxes_to_add <- pending_bxs[pending_bxs$status == "add", , drop = FALSE]
      
      boxes_to_delete <- pending_bxs[pending_bxs$status == "delete", , drop = FALSE]
      
      boxes_to_edit <- pending_bxs[pending_bxs$status == "edit", , drop = FALSE]
      
      
      
      message("To add: ", nrow(boxes_to_add))
      
      message("To delete: ", nrow(boxes_to_delete))
      
      message("To edit: ", nrow(boxes_to_edit))
      
      
      
      # Initialiser current_boxes si NULL
      
      if (is.null(current_boxes)) {
        
        current_boxes <- data.frame(
          
          xmin = numeric(0), xmax = numeric(0),
          
          ymin = numeric(0), ymax = numeric(0),
          
          stain_id = character(0), Volume = numeric(0),
          
          stringsAsFactors = FALSE
          
        )
        
      }
      
      
      
      # 1. Traiter les suppressions
      
      if (nrow(boxes_to_delete) > 0 && nrow(current_boxes) > 0) {
        
        ids_to_delete <- boxes_to_delete$stain_id
        
        message("Deleting IDs: ", paste(ids_to_delete, collapse = ", "))
        
        current_boxes <- current_boxes[!current_boxes$stain_id %in% ids_to_delete, , drop = FALSE]
        
        message("After delete, boxes count: ", nrow(current_boxes))
        
      }
      
      
      
      # 2. Traiter les éditions (modifier les boxes existantes)
      
      if (nrow(boxes_to_edit) > 0 && nrow(current_boxes) > 0) {
        
        for (i in seq_len(nrow(boxes_to_edit))) {
          
          edit_row <- boxes_to_edit[i, ]
          
          original_id <- if ("original_stain_id" %in% names(edit_row) && !is.na(edit_row$original_stain_id)) {
            
            edit_row$original_stain_id
            
          } else {
            
            edit_row$stain_id
            
          }
          
          
          
          message("Editing box: ", original_id)
          
          
          
          # Trouver l'index de la box originale
          
          box_idx <- which(current_boxes$stain_id == original_id)
          
          
          
          if (length(box_idx) > 0) {
            
            message("Found at index: ", box_idx)
            
            # Mettre à jour les coordonnées
            
            current_boxes[box_idx, "xmin"] <- edit_row$xmin
            
            current_boxes[box_idx, "xmax"] <- edit_row$xmax
            
            current_boxes[box_idx, "ymin"] <- edit_row$ymin
            
            current_boxes[box_idx, "ymax"] <- edit_row$ymax
            
            
            
            # Recalculer l'intensité
            
            mat <- bruker_data()$spectrumData
            
            if (!is.null(mat)) {
              
              ppm_x <- suppressWarnings(as.numeric(colnames(mat)))
              
              ppm_y <- suppressWarnings(as.numeric(rownames(mat)))
              
              current_boxes[box_idx, "Volume"] <- get_box_intensity(
                
                mat, ppm_x, ppm_y, current_boxes[box_idx, , drop = FALSE]
                
              )
              
            }
            
          } else {
            
            message("WARNING: Box not found for editing: ", original_id)
            
          }
          
        }
        
        message("After edit, boxes count: ", nrow(current_boxes))
        
      }
      
      
      
      # 3. Traiter les ajouts
      
      if (nrow(boxes_to_add) > 0) {
        
        message("Adding ", nrow(boxes_to_add), " boxes")
        
        
        
        if (!"stain_id" %in% names(boxes_to_add) || any(is.na(boxes_to_add$stain_id))) {
          
          boxes_to_add$stain_id <- paste0("box_", seq_len(nrow(boxes_to_add)))
          
        }
        
        
        
        # Calculer les intensités
        
        mat <- bruker_data()$spectrumData
        
        if (!is.null(mat)) {
          
          ppm_x <- suppressWarnings(as.numeric(colnames(mat)))
          
          ppm_y <- suppressWarnings(as.numeric(rownames(mat)))
          
          boxes_to_add$Volume <- get_box_intensity(mat, ppm_x, ppm_y, boxes_to_add)
          
        }
        
        
        
        # Nettoyer les colonnes de status avant merge
        
        cols_to_remove <- c("status", "original_stain_id")
        
        boxes_to_add <- boxes_to_add[, !names(boxes_to_add) %in% cols_to_remove, drop = FALSE]
        
        
        
        # Ajouter les colonnes manquantes
        
        all_cols <- unique(c(names(current_boxes), names(boxes_to_add)))
        
        for (col in all_cols) {
          
          if (!col %in% names(current_boxes)) current_boxes[[col]] <- NA
          
          if (!col %in% names(boxes_to_add)) boxes_to_add[[col]] <- NA
          
        }
        
        
        
        # S'assurer que les colonnes sont dans le même ordre
        
        boxes_to_add <- boxes_to_add[, names(current_boxes), drop = FALSE]
        
        
        
        current_boxes <- rbind(current_boxes, boxes_to_add)
        
        message("After add, boxes count: ", nrow(current_boxes))
        
      }
      
      
      
      # Nettoyer les colonnes de status dans current_boxes
      
      cols_to_clean <- c("status", "original_stain_id")
      
      for (col in cols_to_clean) {
        
        if (col %in% names(current_boxes)) {
          
          current_boxes[[col]] <- NULL
          
        }
        
      }
      
      
      
      message("Final boxes count: ", nrow(current_boxes))
      
      message("Final box IDs: ", paste(current_boxes$stain_id, collapse = ", "))
      
      
      
      # Mettre à jour toutes les variables réactives
      
      modifiable_boxes(current_boxes)
      
      fixed_boxes(current_boxes)
      
      reference_boxes(current_boxes)
      
    }
    
    
    
    # Reset pending
    
    pending_centroids(data.frame(
      
      F2_ppm = numeric(0), F1_ppm = numeric(0),
      
      Volume = numeric(0), stain_id = character(0),
      
      stringsAsFactors = FALSE
      
    ))
    
    pending_boxes(data.frame(
      
      xmin = numeric(0), xmax = numeric(0),
      
      ymin = numeric(0), ymax = numeric(0),
      
      stain_id = character(0), Volume = numeric(0),
      
      status = character(0),
      
      stringsAsFactors = FALSE
      
    ))
    
    pending_fusions(data.frame(
      
      stain_id = character(0), F2_ppm = numeric(0),
      
      F1_ppm = numeric(0), Volume = numeric(0),
      
      stringsAsFactors = FALSE
      
    ))
    
    
    
    box_intensity_cache(list())
    
    refresh_nmr_plot(force_recalc = TRUE)
    
    
    
    showNotification("✅ Changes applied", type = "message")
    
  })
  
  
  
  ## 8.9 Reset all ----
  
  observeEvent(input$reset_all, {
    
    # Reset des plots
    
    nmr_plot(NULL)
    
    contour_plot_base(NULL)
    
    
    
    # Reset des centroids/peaks
    
    imported_centroids(NULL)
    
    centroids_data(NULL)
    
    centroids(NULL)
    
    
    
    # Reset des boxes - TOUTES les variables liées aux boxes
    
    fixed_boxes(data.frame(xmin = numeric(), xmax = numeric(), 
                           
                           ymin = numeric(), ymax = numeric()))
    
    modifiable_boxes(data.frame())
    
    reference_boxes(NULL)
    
    
    
    # Reset des pending changes
    
    pending_centroids(data.frame(
      
      F2_ppm = numeric(0), F1_ppm = numeric(0),
      
      Volume = numeric(0), stain_id = character(0),
      
      stringsAsFactors = FALSE
      
    ))
    
    pending_boxes(data.frame(
      
      xmin = numeric(0), xmax = numeric(0),
      
      ymin = numeric(0), ymax = numeric(0)
      
    ))
    
    pending_fusions(data.frame(
      
      stain_id = character(), F2_ppm = numeric(),
      
      F1_ppm = numeric(), Volume = numeric(),
      
      stringsAsFactors = FALSE
      
    ))
    
    
    
    # Reset des clics et sélections
    
    first_click_for_box(NULL)
    
    last_click_coords(NULL)
    
    selected_box_for_edit(NULL)
    
    selected_box_index(NULL)
    
    original_box_coords(NULL)
    
    box_has_been_modified(FALSE)
    
    
    
    # Reset des résultats de fit
    
    fit_results_data(NULL)
    
    last_fit_method("sum")
    
    
    
    # Reset du cache
    
    plot_cache(list())
    
    contour_cache(list())
    
    box_intensity_cache(list())
    
    
    
    # Reset de l'UI
    
    updateSelectInput(session, "selected_subfolder", selected = "")
    
    
    
    # Supprimer la preview si présente
    
    if (isTRUE(preview_trace_added())) {
      
      tryCatch({
        
        plotlyProxy("interactivePlot", session) %>%
          
          plotlyProxyInvoke("deleteTraces", -1L)
        
      }, error = function(e) NULL)
      
      preview_trace_added(FALSE)
      
    }
    
    
    
    status_msg("🔁 All data reset")
    
    showNotification("🔁 All data has been reset", type = "message")
    
  })
  
  
  
  ## 8.9 Delete selected peaks (from Data tab) - via pending ----
  
  observeEvent(input$delete_selected_peaks, {
    
    selected_rows <- input$centroid_table_rows_selected
    
    
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      
      showNotification("⚠️ No peaks selected. Use Ctrl+Click to select multiple.", type = "warning")
      
      return()
      
    }
    
    
    
    df <- centroids_data()
    
    if (is.null(df) || nrow(df) == 0) return()
    
    
    
    # Marquer les lignes sélectionnées pour suppression (via pending)
    
    n_to_delete <- length(selected_rows)
    
    to_delete <- df[selected_rows, , drop = FALSE]
    
    to_delete$status <- "delete"
    
    
    
    # Ajouter aux pending
    
    pending_centroids(dplyr::bind_rows(pending_centroids(), to_delete))
    
    
    
    showNotification(paste("🗑️", n_to_delete, "peak(s) marked for deletion. Click 'Apply' to confirm."), type = "message")
    
  })
  
  
  
  ## 8.10 Delete selected boxes (from Data tab) - via pending ----
  
  observeEvent(input$delete_selected_boxes, {
    
    selected_rows <- input$bbox_table_rows_selected
    
    
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      
      showNotification("⚠️ No boxes selected. Use Ctrl+Click to select multiple.", type = "warning")
      
      return()
      
    }
    
    
    
    df <- bounding_boxes_data()
    
    if (is.null(df) || nrow(df) == 0) return()
    
    
    
    # Marquer les lignes sélectionnées pour suppression (via pending)
    
    n_to_delete <- length(selected_rows)
    
    to_delete <- df[selected_rows, , drop = FALSE]
    
    to_delete$status <- "delete"
    
    
    
    # Ajouter aux pending
    
    pending_boxes(dplyr::bind_rows(pending_boxes(), to_delete))
    
    
    
    showNotification(paste("🗑️", n_to_delete, "box(es) marked for deletion. Click 'Apply' to confirm."), type = "message")
    
  })
  
  
  
  ## 8.11 Discard selected pending centroids ----
  
  observeEvent(input$discard_selected_centroid, {
    
    selected_rows <- input$pending_centroids_table_rows_selected
    
    
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      
      showNotification("⚠️ No pending peaks selected", type = "warning")
      
      return()
      
    }
    
    
    
    df <- pending_centroids()
    
    if (is.null(df) || nrow(df) == 0) return()
    
    
    
    n_to_delete <- length(selected_rows)
    
    
    
    # Récupérer les peaks qu'on va retirer de pending
    
    peaks_to_discard <- df[selected_rows, , drop = FALSE]
    
    
    
    # Retirer de pending
    
    df_remaining <- df[-selected_rows, , drop = FALSE]
    
    pending_centroids(df_remaining)
    
    
    
    # Message informatif selon le type
    
    if ("status" %in% names(peaks_to_discard)) {
      
      n_delete <- sum(peaks_to_discard$status == "delete", na.rm = TRUE)
      
      n_add <- sum(is.na(peaks_to_discard$status) | peaks_to_discard$status != "delete")
      
      
      
      msg_parts <- c()
      
      if (n_delete > 0) msg_parts <- c(msg_parts, paste(n_delete, "deletion(s) cancelled"))
      
      if (n_add > 0) msg_parts <- c(msg_parts, paste(n_add, "addition(s) cancelled"))
      
      
      
      showNotification(paste("↩️", paste(msg_parts, collapse = ", ")), type = "message")
      
    } else {
      
      showNotification(paste("🗑️ Removed", n_to_delete, "pending peak(s)"), type = "message")
      
    }
    
  })
  
  
  
  ## 8.12 Discard selected pending boxes ----
  
  observeEvent(input$discard_selected_box, {
    
    selected_rows <- input$pending_boxes_table_rows_selected
    
    
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      
      showNotification("⚠️ No pending boxes selected", type = "warning")
      
      return()
      
    }
    
    
    
    df <- pending_boxes()
    
    if (is.null(df) || nrow(df) == 0) return()
    
    
    
    n_to_delete <- length(selected_rows)
    
    
    
    # Récupérer les boxes qu'on va retirer de pending
    
    boxes_to_discard <- df[selected_rows, , drop = FALSE]
    
    
    
    # Pour les boxes avec status "delete" qui sont retirées de pending,
    
    # elles restent dans Data (c'est le comportement voulu : annuler la suppression)
    
    # Pour les boxes avec status "add", elles disparaissent simplement
    
    # Pour les boxes avec status "edit", on annule l'édition
    
    
    
    # Retirer de pending
    
    df_remaining <- df[-selected_rows, , drop = FALSE]
    
    pending_boxes(df_remaining)
    
    
    
    # Nettoyer la preview trace si présente
    
    if (isTRUE(preview_trace_added())) {
      
      tryCatch({
        
        plotlyProxy("interactivePlot", session) %>%
          
          plotlyProxyInvoke("deleteTraces", -1L)
        
      }, error = function(e) NULL)
      
      preview_trace_added(FALSE)
      
    }
    
    
    
    # Reset la sélection d'édition
    
    selected_box_for_edit(NULL)
    
    selected_box_index(NULL)
    
    original_box_coords(NULL)
    
    box_has_been_modified(FALSE)
    
    
    
    # Message informatif selon le type
    
    if ("status" %in% names(boxes_to_discard)) {
      
      n_delete <- sum(boxes_to_discard$status == "delete", na.rm = TRUE)
      
      n_add <- sum(boxes_to_discard$status == "add", na.rm = TRUE)
      
      n_edit <- sum(boxes_to_discard$status == "edit", na.rm = TRUE)
      
      
      
      msg_parts <- c()
      
      if (n_delete > 0) msg_parts <- c(msg_parts, paste(n_delete, "deletion(s) cancelled"))
      
      if (n_add > 0) msg_parts <- c(msg_parts, paste(n_add, "addition(s) cancelled"))
      
      if (n_edit > 0) msg_parts <- c(msg_parts, paste(n_edit, "edit(s) cancelled"))
      
      
      
      showNotification(paste("↩️", paste(msg_parts, collapse = ", ")), type = "message")
      
    } else {
      
      showNotification(paste("🗑️ Removed", n_to_delete, "pending box(es)"), type = "message")
      
    }
    
  })
  
  
  
  ## 8.13 Discard selected pending fusions ----
  
  observeEvent(input$discard_selected_fusion, {
    
    selected_rows <- input$pending_fusions_table_rows_selected
    
    
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      
      showNotification("⚠️ No pending fusions selected", type = "warning")
      
      return()
      
    }
    
    
    
    df <- pending_fusions()
    
    if (is.null(df) || nrow(df) == 0) return()
    
    
    
    n_to_delete <- length(selected_rows)
    
    df_remaining <- df[-selected_rows, , drop = FALSE]
    
    pending_fusions(df_remaining)
    
    
    
    showNotification(paste("🗑️ Removed", n_to_delete, "pending fusion(s)"), type = "message")
    
  })
  
  
  
  # SECTION 9: IMPORT/EXPORT ----
  
  
  
  
  
  ## 9.0 Save/Load Session ----
  
  
  
  ### Save session ----
  
  output$save_session <- downloadHandler(
    
    filename = function() {
      
      paste0("nmr_session_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      
    },
    
    content = function(file) {
      
      # Collecter toutes les données de la session
      
      session_data <- list(
        
        # Version pour compatibilité future
        
        version = "1.0",
        
        timestamp = Sys.time(),
        
        
        
        # Données principales
        
        centroids = centroids_data(),
        
        boxes = modifiable_boxes(),
        
        fixed_boxes = fixed_boxes(),
        
        
        
        # Pending changes
        
        pending_centroids = pending_centroids(),
        
        pending_boxes = pending_boxes(),
        
        pending_fusions = pending_fusions(),
        
        
        
        # Résultats de fit
        
        fit_results = fit_results_data(),
        
        last_fit_method = last_fit_method(),
        
        
        
        # Paramètres UI
        
        params = list(
          
          spectrum_type = input$spectrum_type,
          
          contour_start = input$contour_start,
          
          eps_value = input$eps_value,
          
          neighborhood_size = input$neighborhood_size,
          
          exclusion_zones = input$exclusion_zones,
          
          filter_artifacts = input$filter_artifacts,
          
          filter_diagonal = input$filter_diagonal,
          
          diagonal_tolerance = input$diagonal_tolerance,
          
          integration_method = input$integration_method,
          
          integration_method_fit = input$integration_method_fit
          
        ),
        
        
        
        # Chemin du dossier (pour référence)
        
        data_path = if (!is.null(input$directory)) {
          
          tryCatch(parseDirPath(c(Home = normalizePath("~"), getwd = getwd()), input$directory),
                   
                   error = function(e) NULL)
          
        } else NULL
        
      )
      
      
      
      # Sauvegarder en RDS
      
      saveRDS(session_data, file)
      
      
      
      showNotification("💾 Session saved successfully!", type = "message")
      
    }
    
  )
  
  
  
  ### Load session ----
  
  observeEvent(input$load_session_file, {
    
    req(input$load_session_file)
    
    
    
    tryCatch({
      
      # Charger le fichier RDS
      
      session_data <- readRDS(input$load_session_file$datapath)
      
      
      
      # Vérifier la version
      
      if (is.null(session_data$version)) {
        
        showNotification("⚠️ Old session format, some data may not load correctly", type = "warning")
        
      }
      
      
      
      # Vider les caches pour forcer le recalcul
      
      plot_cache(list())
      
      contour_cache(list())
      
      box_intensity_cache(list())
      
      
      
      # Restaurer les données principales
      
      if (!is.null(session_data$centroids) && nrow(session_data$centroids) > 0) {
        
        centroids_data(session_data$centroids)
        
      }
      
      
      
      if (!is.null(session_data$boxes) && nrow(session_data$boxes) > 0) {
        
        modifiable_boxes(session_data$boxes)
        
        # Aussi mettre à jour fixed_boxes pour la cohérence
        
        fixed_boxes(session_data$boxes)
        
      }
      
      
      
      if (!is.null(session_data$fixed_boxes) && nrow(session_data$fixed_boxes) > 0) {
        
        fixed_boxes(session_data$fixed_boxes)
        
      }
      
      
      
      # Restaurer les pending changes
      
      if (!is.null(session_data$pending_centroids) && nrow(session_data$pending_centroids) > 0) {
        
        pending_centroids(session_data$pending_centroids)
        
      }
      
      
      
      if (!is.null(session_data$pending_boxes) && nrow(session_data$pending_boxes) > 0) {
        
        pending_boxes(session_data$pending_boxes)
        
      }
      
      
      
      if (!is.null(session_data$pending_fusions) && nrow(session_data$pending_fusions) > 0) {
        
        pending_fusions(session_data$pending_fusions)
        
      }
      
      
      
      # Restaurer les résultats de fit
      
      if (!is.null(session_data$fit_results)) {
        
        fit_results_data(session_data$fit_results)
        
      }
      
      
      
      if (!is.null(session_data$last_fit_method)) {
        
        last_fit_method(session_data$last_fit_method)
        
      }
      
      
      
      # Restaurer les paramètres UI
      
      params <- session_data$params
      
      if (!is.null(params)) {
        
        if (!is.null(params$spectrum_type)) {
          
          updateSelectInput(session, "spectrum_type", selected = params$spectrum_type)
          
        }
        
        if (!is.null(params$contour_start)) {
          
          updateNumericInput(session, "contour_start", value = params$contour_start)
          
        }
        
        if (!is.null(params$eps_value)) {
          
          updateNumericInput(session, "eps_value", value = params$eps_value)
          
        }
        
        if (!is.null(params$neighborhood_size)) {
          
          updateNumericInput(session, "neighborhood_size", value = params$neighborhood_size)
          
        }
        
        if (!is.null(params$exclusion_zones)) {
          
          updateTextInput(session, "exclusion_zones", value = params$exclusion_zones)
          
        }
        
        if (!is.null(params$filter_artifacts)) {
          
          updateCheckboxInput(session, "filter_artifacts", value = params$filter_artifacts)
          
        }
        
        if (!is.null(params$filter_diagonal)) {
          
          updateCheckboxInput(session, "filter_diagonal", value = params$filter_diagonal)
          
        }
        
        if (!is.null(params$diagonal_tolerance)) {
          
          updateNumericInput(session, "diagonal_tolerance", value = params$diagonal_tolerance)
          
        }
        
      }
      
      
      
      # Message de succès avec résumé
      
      n_peaks <- if (!is.null(session_data$centroids)) nrow(session_data$centroids) else 0
      
      n_boxes <- if (!is.null(session_data$boxes)) nrow(session_data$boxes) else 0
      
      
      
      # Rafraîchir le plot si des données spectrales sont chargées
      
      if (!is.null(bruker_data()) && !is.null(contour_plot_base())) {
        
        # Forcer la régénération du plot
        
        refresh_nmr_plot(force_recalc = TRUE)
        
        
        
        showNotification(
          
          paste0("✅ Session loaded! ", n_peaks, " peaks, ", n_boxes, " boxes"),
          
          type = "message",
          
          duration = 5
          
        )
        
      } else {
        
        # Données spectrales pas encore chargées
        
        showNotification(
          
          paste0("✅ Session data loaded! ", n_peaks, " peaks, ", n_boxes, " boxes. ",
                 
                 "Load spectrum data to see them on the plot."),
          
          type = "message",
          
          duration = 8
          
        )
        
      }
      
      
      
      status_msg(paste0("📂 Session loaded: ", n_peaks, " peaks, ", n_boxes, " boxes"))
      
      
      
      # Afficher le chemin original si disponible
      
      if (!is.null(session_data$data_path)) {
        
        showNotification(
          
          paste0("ℹ️ Original data path: ", session_data$data_path),
          
          type = "message",
          
          duration = 8
          
        )
        
      }
      
      
      
    }, error = function(e) {
      
      showNotification(paste("❌ Error loading session:", e$message), type = "error")
      
    })
    
  })
  
  
  
  
  
  ## 9.1 Import centroids and boxes ----
  
  
  
  ### Import bounding centroids ----
  
  observeEvent(input$import_centroids_file, {
    
    req(input$import_centroids_file)
    
    imported <- tryCatch(
      
      read.csv(input$import_centroids_file$datapath, sep = ";", stringsAsFactors = FALSE),
      
      error = function(e) { showNotification(paste("Import error:", e$message), type = "error"); NULL }
      
    )
    
    
    
    if (!is.null(imported) && all(c("stain_id", "Volume", "F2_ppm", "F1_ppm") %in% colnames(imported))) {
      
      centroids_data(clean_centroids_df(imported))
      
      refresh_nmr_plot()
      
      showNotification("✅ Centroids imported", type = "message")
      
    } else {
      
      showNotification("❌ File must contain: stain_id, Volume, F2_ppm, F1_ppm", type = "error")
      
    }
    
  })
  
  
  
  ### Import bounding boxes ----
  
  observeEvent(input$import_boxes_file, {
    
    req(input$import_boxes_file)
    
    
    
    # Essayer d'abord avec point-virgule, puis avec virgule
    
    imported <- tryCatch({
      
      df <- read.csv(input$import_boxes_file$datapath, sep = ";", stringsAsFactors = FALSE)
      
      # Si on n'a qu'une colonne, essayer avec virgule
      
      if (ncol(df) == 1) {
        
        df <- read.csv(input$import_boxes_file$datapath, sep = ",", stringsAsFactors = FALSE)
        
      }
      
      df
      
    }, error = function(e) {
      
      showNotification(paste("Import error:", e$message), type = "error")
      
      return(NULL)
      
    })
    
    
    
    if (is.null(imported)) return()
    
    
    
    # Debug
    
    cat("=== DEBUG Import boxes ===\n")
    
    cat("Columns found:", paste(colnames(imported), collapse = ", "), "\n")
    
    cat("Nrow:", nrow(imported), "\n")
    
    if (nrow(imported) > 0) {
      
      cat("First row:\n")
      
      print(imported[1, ])
      
    }
    
    
    
    required_cols <- c("stain_id", "xmin", "xmax", "ymin", "ymax")
    
    
    
    # Validate imported structure
    
    if (!all(required_cols %in% colnames(imported))) {
      
      showNotification(
        
        paste(
          
          "❌ File must contain columns:",
          
          paste(required_cols, collapse = ", "),
          
          "\nFound:", paste(colnames(imported), collapse = ", ")
          
        ),
        
        type = "error",
        
        duration = 10
        
      )
      
      return()
      
    }
    
    
    
    # Garder seulement les colonnes requises (+ Volume si présent)
    
    cols_to_keep <- intersect(c(required_cols, "Volume"), colnames(imported))
    
    imported <- imported[, cols_to_keep, drop = FALSE]
    
    
    
    # Convertir les types - stain_id reste character !
    
    imported$stain_id <- as.character(imported$stain_id)
    
    imported$xmin <- as.numeric(imported$xmin)
    
    imported$xmax <- as.numeric(imported$xmax)
    
    imported$ymin <- as.numeric(imported$ymin)
    
    imported$ymax <- as.numeric(imported$ymax)
    
    
    
    # Vérifier qu'on a des données valides
    
    valid_rows <- !is.na(imported$xmin) & !is.na(imported$xmax) & 
      
      !is.na(imported$ymin) & !is.na(imported$ymax)
    
    
    
    if (sum(valid_rows) == 0) {
      
      showNotification("❌ No valid box coordinates found", type = "error")
      
      return()
      
    }
    
    
    
    imported <- imported[valid_rows, , drop = FALSE]
    
    
    
    cat("After cleaning - Nrow:", nrow(imported), "\n")
    
    cat("Coords range - xmin:", range(imported$xmin), "xmax:", range(imported$xmax), "\n")
    
    
    
    # Update reactive values for your app
    
    fixed_boxes(imported)
    
    modifiable_boxes(imported)
    
    reference_boxes(imported)
    
    
    
    # Forcer le rafraîchissement du plot
    
    box_intensity_cache(list())
    
    refresh_nmr_plot(force_recalc = TRUE)
    
    
    
    showNotification(paste("✅", nrow(imported), "bounding boxes imported"), type = "message")
    
  })
  
  
  
  
  
  ## 9.2 Export centroids ----
  
  output$export_centroids <- downloadHandler(
    
    filename = function() paste0("centroids_", Sys.Date(), ".csv"),
    
    content = function(file) {
      
      df <- centroids_data()
      
      # Utiliser write.csv2 pour séparateur ";" (compatible Excel français)
      
      if (!is.null(df) && nrow(df) > 0) write.csv2(df, file, row.names = FALSE) 
      
      else write.csv2(data.frame(), file)
      
    }
    
  )
  
  
  
  ## 9.3 Export boxes ----
  
  output$export_boxes <- downloadHandler(
    
    filename = function() paste0("combined_box_intensities_", Sys.Date(), ".csv"),
    
    content = function(file) {
      
      req(result_data_list(), bounding_boxes_data())
      
      
      
      boxes_ref <- bounding_boxes_data() %>%
        
        dplyr::mutate(stain_id = dplyr::row_number(), 
                      
                      F2_ppm = (xmin + xmax)/2, F1_ppm = (ymin + ymax)/2)
      
      
      
      intensity_list <- list()
      
      for (name in names(result_data_list())) {
        
        result <- result_data_list()[[name]]
        
        if (is.null(result$contour_data)) next
        
        contour_data <- result$contour_data
        
        
        
        Intensity <- vapply(seq_len(nrow(boxes_ref)), FUN.VALUE = 0.0, FUN = function(i) {
          
          rowb <- boxes_ref[i, ]
          
          sum(contour_data$level[
            
            contour_data$x >= -rowb$xmax & contour_data$x <= -rowb$xmin &
              
              contour_data$y >= -rowb$ymax & contour_data$y <= -rowb$ymin
            
          ], na.rm = TRUE)
          
        })
        
        
        
        tmp_df <- data.frame(stain_id = boxes_ref$stain_id, Intensity = Intensity)
        
        col_name <- paste0("Intensity_", make.names(basename(name)))
        
        colnames(tmp_df)[2] <- col_name
        
        intensity_list[[name]] <- tmp_df
        
      }
      
      
      
      if (length(intensity_list) == 0) {
        
        write.csv2(data.frame(message = "No contour_data found."), file, row.names = FALSE)
        
        return(invisible(NULL))
        
      }
      
      
      
      merged_data <- Reduce(function(x, y) dplyr::full_join(x, y, by = "stain_id"), intensity_list)
      
      final_data <- dplyr::left_join(boxes_ref, merged_data, by = "stain_id") %>%
        
        dplyr::select(stain_id, F2_ppm, F1_ppm, xmin, xmax, ymin, ymax, dplyr::starts_with("Intensity_"))
      
      
      
      # Utiliser write.csv2 pour séparateur ";" (compatible Excel français)
      
      write.csv2(final_data, file, row.names = FALSE)
      
    }
    
  )
  
  
  
  ## 9.4 Export batch box intensities ----
  
  output$export_batch_box_intensities <- downloadHandler(
    
    filename = function() {
      
      method <- effective_integration_method()
      
      method_suffix <- if (method == "sum") "" else paste0("_", method)
      
      paste0("batch_box_intensities", method_suffix, "_", Sys.Date(), ".csv")
      
    },
    
    content = function(file) {
      
      req(reference_boxes(), spectra_list())
      
      
      
      # Récupérer la méthode choisie
      
      method <- effective_integration_method()
      
      model <- if (method %in% c("gaussian", "voigt")) method else "gaussian"
      
      
      
      status_msg(paste0("🔄 Calculating batch intensities (", method, " method)..."))
      
      
      
      # Progress bar
      
      progress <- shiny::Progress$new()
      
      on.exit(progress$close())
      
      progress$set(message = "Processing spectra", value = 0)
      
      
      
      tryCatch({
        
        ref_boxes <- reference_boxes()
        
        
        
        if (is.null(ref_boxes) || nrow(ref_boxes) == 0) {
          
          showNotification("⚠️ No reference boxes found", type = "warning")
          
          return()
          
        }
        
        
        
        # Stocker la méthode utilisée
        
        last_fit_method(method)
        
        
        
        # APPEL AVEC LES NOUVEAUX PARAMÈTRES
        
        batch_intensities <- calculate_batch_box_intensities(
          
          reference_boxes = ref_boxes,
          
          spectra_list = spectra_list(),
          
          apply_shift = FALSE,
          
          method = method,
          
          model = model,
          
          progress = function(value, detail) {
            
            progress$set(value = value, detail = detail)
            
          }
          
        )
        
        
        
        # Note: Les infos de fit (R², centers) ne sont plus dans le batch export
        
        # Elles sont disponibles via "Run Integration" dans la section Integration
        
        # Cela garde le CSV compact pour les comparaisons entre spectres
        
        
        
        # Remplacer valeurs négatives par 0
        
        intensity_cols <- grep("^Intensity_", names(batch_intensities), value = TRUE)
        
        for (col in intensity_cols) {
          
          batch_intensities[[col]] <- pmax(batch_intensities[[col]], 0, na.rm = TRUE)
          
        }
        
        
        
        # Utiliser write.csv2 pour séparateur ";" (compatible Excel français)
        
        write.csv2(batch_intensities, file, row.names = FALSE)
        
        
        
        status_msg("✅ Batch export complete")
        
        showNotification(paste("✅ Exported", nrow(ref_boxes), "boxes,", 
                               
                               length(spectra_list()), "spectra"), type = "message")
        
        
        
      }, error = function(e) {
        
        showNotification(paste("❌ Export error:", e$message), type = "error")
        
        status_msg(paste("❌ Error:", e$message))
        
      })
      
    }
    
  )
  
  
  
  ## 9.5 Export projected centroids ----
  
  output$download_projected_centroids <- downloadHandler(
    
    filename = function() paste0("projected_centroids_", Sys.Date(), ".zip"),
    
    content = function(zipfile) {
      
      req(centroids_data(), result_data_list())
      
      
      
      tmp_dir <- tempdir()
      
      csv_files <- character(0)
      
      eps_val <- input$eps_value %||% 0.04
      
      reference_centroids <- centroids_data()
      
      volumes_list <- list()
      
      eps_factors <- list(HSQC = 10, TOCSY = 8, COSY = 10, UFCOSY = 4)
      
      
      
      for (name in names(result_data_list())) {
        
        result <- result_data_list()[[name]]
        
        if (is.null(result$contour_data)) next
        
        
        
        contour_data <- result$contour_data
        
        spec_type <- result$spectrum_type %||% "COSY"
        
        eps_factor <- eps_factors[[spec_type]] %||% 1
        
        delta_F2 <- 0; delta_F1 <- 0
        
        
        
        try({
          
          ref_hist <- with(reference_centroids, MASS::kde2d(F2_ppm, F1_ppm, n = 200))
          
          spec_hist <- with(contour_data, MASS::kde2d(-x, -y, n = 200))
          
          corr <- stats::convolve2d(ref_hist$z, spec_hist$z, type = "open")
          
          max_idx <- which(corr == max(corr, na.rm = TRUE), arr.ind = TRUE)
          
          delta_F2 <- (max_idx[1] - nrow(ref_hist$z)) * mean(diff(ref_hist$x))
          
          delta_F1 <- (max_idx[2] - ncol(ref_hist$z)) * mean(diff(ref_hist$y))
          
        }, silent = TRUE)
        
        
        
        shifted_centroids <- reference_centroids %>% 
          
          dplyr::mutate(F2_ppm = F2_ppm + delta_F2, F1_ppm = F1_ppm + delta_F1)
        
        
        
        projected_centroids <- shifted_centroids %>% 
          
          dplyr::rowwise() %>% 
          
          dplyr::mutate(Volume = {
            
            local_points <- contour_data %>% 
              
              dplyr::filter(sqrt((-x - F2_ppm)^2 + (-y - F1_ppm)^2) <= eps_val * eps_factor)
            
            sum(local_points$level, na.rm = TRUE)
            
          }) %>% 
          
          dplyr::ungroup()
        
        
        
        safe_name <- make.names(basename(name))
        
        output_csv <- file.path(tmp_dir, paste0(safe_name, "_projected_centroids.csv"))
        
        # Utiliser write.csv2 pour séparateur ";" (compatible Excel français)
        
        write.csv2(projected_centroids, output_csv, row.names = FALSE)
        
        csv_files <- c(csv_files, output_csv)
        
        
        
        vol_df <- projected_centroids %>% 
          
          dplyr::select(stain_id, F2_ppm, F1_ppm, Volume) %>% 
          
          dplyr::rename(!!paste0("Volume_", safe_name) := Volume)
        
        volumes_list[[safe_name]] <- vol_df
        
      }
      
      
      
      if (length(volumes_list) > 0) {
        
        merged_data <- Reduce(function(x, y) dplyr::full_join(x, y, by = c("stain_id", "F2_ppm", "F1_ppm")), volumes_list)
        
        volume_cols <- grep("^Volume_", names(merged_data), value = TRUE)
        
        merged_data <- merged_data %>% dplyr::select(stain_id, F2_ppm, F1_ppm, dplyr::all_of(volume_cols))
        
        merged_data$status <- ifelse(rowSums(is.na(merged_data[volume_cols])) == 0, "ok", "missing")
        
        summary_csv <- file.path(tmp_dir, "summary_volumes.csv")
        
        # Utiliser write.csv2 pour séparateur ";" (compatible Excel français)
        
        write.csv2(merged_data, summary_csv, row.names = FALSE)
        
        csv_files <- c(csv_files, summary_csv)
        
      }
      
      
      
      zip(zipfile, files = csv_files, flags = "-j")
      
    }
    
  )
  
  
  
  ## 9.6 Save directory ----
  
  save_roots <- c(Home = normalizePath("~"), Root = "/")
  
  shinyDirChoose(input, "save_directory", roots = save_roots, session = session)
  
  save_directory <- reactive({ 
    
    req(input$save_directory)
    
    
    
    tryCatch({
      
      selection <- input$save_directory
      
      
      
      if (is.null(selection) || length(selection) == 0) {
        
        return(NULL)
        
      }
      
      
      
      selected_root <- selection$root
      
      if (is.null(selected_root) || !selected_root %in% names(save_roots)) {
        
        return(NULL)
        
      }
      
      
      
      base_path <- save_roots[[selected_root]]
      
      path_parts <- selection$path
      
      
      
      if (is.null(path_parts) || length(path_parts) == 0) {
        
        final_path <- base_path
        
      } else {
        
        path_parts <- unlist(path_parts)
        
        path_parts <- path_parts[path_parts != ""]
        
        
        
        if (length(path_parts) == 0) {
          
          final_path <- base_path
          
        } else {
          
          if (selected_root == "Root") {
            
            final_path <- paste0("/", paste(path_parts, collapse = "/"))
            
          } else {
            
            final_path <- file.path(base_path, paste(path_parts, collapse = "/"))
            
          }
          
        }
        
      }
      
      
      
      norm_path <- normalizePath(final_path, mustWork = FALSE)
      
      if (!dir.exists(norm_path)) return(NULL)
      
      norm_path
      
    }, error = function(e) {
      
      NULL
      
    })
    
  })
  
  output$save_dir_display <- renderPrint({ save_directory() })
  
  
  
  
  
  # SECTION 10: UI OUTPUTS ----
  
  
  
  
  
  ## 10.1 Description ----
  
  output$toolDescription <- renderUI({
    
    tags$div(
      
      #'       
      
      #'       # Import de la font Orbitron
      
      #'       tags$style(HTML("
      
      #'   @import url('https://fonts.googleapis.com/css2?family=Orbitron:wght@400;700;900&display=swap');
      
      #' ")),
      
      #'       
      
      #'       div(
      
      #'         style = "
      
      #'     background: rgba(8, 8, 16, 1);
      
      #'     border-radius: 20px;
      
      #'     padding: 25px 35px;
      
      #'     display: flex;
      
      #'     align-items: center;
      
      #'     gap: 30px;
      
      #'     margin-bottom: 20px;
      
      #'     box-shadow: 0 0 40px rgba(196,77,255,0.15), inset 0 0 60px rgba(196,77,255,0.05);
      
      #'     border: 1px solid rgba(196,77,255,0.15);
      
      #'   ",
      
      #'         
      
      #'         img(src = "spin.png", height = "130px", 
      
      #'             style = "filter: drop-shadow(0 0 25px rgba(255,107,107,0.4));"),
      
      #'         
      
      #'         div(
      
      #'           style = "flex: 1;",
      
      #'           h2(
      
      #'             style = "
      
      #'         font-family: 'Orbitron', sans-serif;
      
      #'         font-size: 36px;
      
      #'         font-weight: 900;
      
      #'         letter-spacing: 10px;
      
      #'         margin: 0;
      
      #'         background: linear-gradient(135deg, #ff6b6b 0%, #ffd93d 25%, #6bcb77 50%, #4d96ff 75%, #9b59b6 100%);
      
      #'         -webkit-background-clip: text;
      
      #'         -webkit-text-fill-color: transparent;
      
      #'       ",
      
      #'             "SPIN"
      
      #'           ),
      
      #'           p(
      
      #'             style = "color: rgba(255,255,255,0.6); font-size: 13px; margin-top: 8px;",
      
      #'             HTML("<span style='color: #c44dff; font-weight: 600;'>S</span>harp <span style='color: #c44dff; font-weight: 600;'>P</span>eak <span style='color: #c44dff; font-weight: 600;'>I</span>dentification for 2D <span style='color: #c44dff; font-weight: 600;'>N</span>MR")
      
      #'           )
      
      #'         )
      
      #'       ),
      
      
      
      # Quick Start
      
      div(style = "background: #e8f5e9; padding: 20px; border-radius: 8px; margin-bottom: 20px; border-left: 4px solid #4caf50;",
          
          h4(style = "color: #2e7d32; margin-top: 0;", "🚀 Quick Start"),
          
          tags$ol(style = "margin-bottom: 0;",
                  
                  tags$li(tags$b("Load"), " → Select your Bruker data folder"),
                  
                  tags$li(tags$b("Plot"), " → Generate contour plots"),
                  
                  tags$li(tags$b("Pick"), " → Detect peaks automatically"),
                  
                  tags$li(tags$b("Edit"), " → Refine boxes manually if needed"),
                  
                  tags$li(tags$b("Export"), " → Save results to CSV")
                  
          )
          
      ),
      
      
      
      # Tips
      
      h4("💡 Tips"),
      
      
      
      div(style = "background: #fff3e0; padding: 15px; border-radius: 8px; border-left: 4px solid #ff9800; margin-bottom: 20px;",
          
          tags$ul(style = "margin-bottom: 0; font-size: 13px;",
                  
                  tags$li("Start with a QC sample or the most intense spectrum to optimize parameters"),
                  
                  tags$li("Use 'No clustering' option if you do not want to group multiplets"),
                  
                  tags$li("Increase epsilon value to decrease size of cluster and get more solo peaks (e.g., TOCSY)"),
                  
                  tags$li("Click 'Apply' to confirm changes before exporting"),
                  
                  tags$li("Use the 'Data' tab to review and select boxes for editing"),
                  
                  tags$li("If you want to process a batch, you might want to select the folder, only select a QC or the most intense spectrum. Process it (Step 1), then reload every spectra and use the 'batch export' with the peaks selected on the first spectrum."),
                  
                  tags$li("For batch treatment, limit the number of sprectrum per batch to 25 for TOCSY, 50 for COSY and HSQC.")
                  
                  
                  
          )
          
      ),
      
      
      
      # Main Features
      
      h4("📋 Features"),
      
      
      
      fluidRow(
        
        column(6,
               
               div(style = "background: #f5f5f5; padding: 15px; border-radius: 8px; margin-bottom: 15px; height: 180px;",
                   
                   h5(style = "color: #1976d2;", "📂 Data Loading"),
                   
                   tags$ul(style = "font-size: 13px;",
                           
                           tags$li("Load Bruker 2D NMR data (ser/fid files)"),
                           
                           tags$li("Batch processing of multiple spectra"),
                           
                           tags$li("Select specific spectra to analyze"),
                           
                           tags$li("Automatic detection of valid datasets")
                           
                   )
                   
               )
               
        ),
        
        column(6,
               
               div(style = "background: #f5f5f5; padding: 15px; border-radius: 8px; margin-bottom: 15px; height: 180px;",
                   
                   h5(style = "color: #1976d2;", "📈 Visualization"),
                   
                   tags$ul(style = "font-size: 13px;",
                           
                           tags$li("Interactive contour plots (zoom, pan)"),
                           
                           tags$li("Adjustable intensity threshold"),
                           
                           tags$li("Click to get coordinates"),
                           
                           tags$li("Real-time display of peaks and boxes")
                           
                   )
                   
               )
               
        )
        
      ),
      
      
      
      fluidRow(
        
        column(6,
               
               div(style = "background: #f5f5f5; padding: 15px; border-radius: 8px; margin-bottom: 15px; height: 180px;",
                   
                   h5(style = "color: #1976d2;", "🎯 Peak Detection"),
                   
                   tags$ul(style = "font-size: 13px;",
                           
                           tags$li(tags$b("Local Max method:"), " Local maxima + DBSCAN clustering"),
                           
                           tags$li(tags$b("CNN method:"), " Deep learning detection"),
                           
                           tags$li("Automatic bounding box generation"),
                           
                           tags$li("Configurable clustering parameters")
                           
                   )
                   
               )
               
        ),
        
        column(6,
               
               div(style = "background: #f5f5f5; padding: 15px; border-radius: 8px; margin-bottom: 15px; height: 180px;",
                   
                   h5(style = "color: #1976d2;", "✏️ Manual Editing"),
                   
                   tags$ul(style = "font-size: 13px;",
                           
                           tags$li("Add boxes by clicking (two-click mode)"),
                           
                           tags$li("Move and resize existing boxes"),
                           
                           tags$li("Delete unwanted peaks/boxes"),
                           
                           tags$li("Fuse multiple peaks into one")
                           
                   )
                   
               )
               
        )
        
      ),
      
      
      
      # Supported Spectrum Types
      
      h4("🧪 Supported Spectrum Types"),
      
      
      
      div(style = "display: flex; flex-wrap: wrap; gap: 10px; margin-bottom: 20px;",
          
          div(style = "background: linear-gradient(135deg, #11998e, #38ef7d); color: white; padding: 10px 20px; border-radius: 20px;",
              
              tags$b("TOCSY"), " - Total Correlation Spectroscopy"
              
          ),
          
          div(style = "background: linear-gradient(135deg, #667eea, #764ba2); color: white; padding: 10px 20px; border-radius: 20px;",
              
              tags$b("HSQC"), " - Heteronuclear Single Quantum Coherence"
              
          ),
          
          div(style = "background: linear-gradient(135deg, #f093fb, #f5576c); color: white; padding: 10px 20px; border-radius: 20px;",
              
              tags$b("COSY"), " - Correlation Spectroscopy"
              
          ),
          
          div(style = "background: linear-gradient(135deg, #4facfe, #00f2fe); color: white; padding: 10px 20px; border-radius: 20px;",
              
              tags$b("UFCOSY"), " - Ultra-Fast COSY"
              
          )
          
      ),
      
      
      
      # Workflow Details
      
      h4("📖 Detailed Workflow"),
      
      
      
      # Step 1
      
      div(style = "background: #fff; border: 1px solid #ddd; border-radius: 8px; padding: 15px; margin-bottom: 10px;",
          
          fluidRow(
            
            column(1, div(style = "background: #11998e; color: white; width: 30px; height: 30px; border-radius: 50%; text-align: center; line-height: 30px; font-weight: bold;", "1")),
            
            column(11,
                   
                   h5(style = "margin-top: 5px;", "Load Data"),
                   
                   p(style = "margin-bottom: 0; font-size: 13px;", 
                     
                     "Select a folder containing Bruker NMR data. The tool will automatically detect valid 2D spectra ",
                     
                     "(folders containing 'acqus' and 'ser' or 'fid' files). You can select which spectra to load using checkboxes.")
                   
            )
            
          )
          
      ),
      
      
      
      # Step 2
      
      div(style = "background: #fff; border: 1px solid #ddd; border-radius: 8px; padding: 15px; margin-bottom: 10px;",
          
          fluidRow(
            
            column(1, div(style = "background: #667eea; color: white; width: 30px; height: 30px; border-radius: 50%; text-align: center; line-height: 30px; font-weight: bold;", "2")),
            
            column(11,
                   
                   h5(style = "margin-top: 5px;", "Generate Plot"),
                   
                   p(style = "margin-bottom: 0; font-size: 13px;", 
                     
                     "Choose the spectrum type and adjust the intensity threshold. Click 'Auto' to calculate an optimal threshold ",
                     
                     "based on noise level or maximum intensity. Then click 'Generate Plot' to create the contour visualization.")
                   
            )
            
          )
          
      ),
      
      
      
      # Step 3
      
      div(style = "background: #fff; border: 1px solid #ddd; border-radius: 8px; padding: 15px; margin-bottom: 10px;",
          
          fluidRow(
            
            column(1, div(style = "background: #f5576c; color: white; width: 30px; height: 30px; border-radius: 50%; text-align: center; line-height: 30px; font-weight: bold;", "3")),
            
            column(11,
                   
                   h5(style = "margin-top: 5px;", "Peak Picking"),
                   
                   p(style = "font-size: 13px;", 
                     
                     tags$b("Local Max method:"), " Uses local maxima detection followed by DBSCAN clustering to group nearby points. ",
                     
                     "Adjust 'epsilon' to control cluster size."),
                   
                   p(style = "margin-bottom: 0; font-size: 13px;", 
                     
                     tags$b("CNN method:"), " Uses a trained convolutional neural network for peak detection. ",
                     
                     "Better for complex or overlapping peaks.")
                   
            )
            
          )
          
      ),
      
      
      
      # Step 4
      
      div(style = "background: #fff; border: 1px solid #ddd; border-radius: 8px; padding: 15px; margin-bottom: 10px;",
          
          fluidRow(
            
            column(1, div(style = "background: #4facfe; color: white; width: 30px; height: 30px; border-radius: 50%; text-align: center; line-height: 30px; font-weight: bold;", "4")),
            
            column(11,
                   
                   h5(style = "margin-top: 5px;", "Manual Editing"),
                   
                   p(style = "font-size: 13px;", 
                     
                     tags$b("Add boxes:"), " Enable 'Two clicks' mode, then click two opposite corners on the spectrum."),
                   
                   p(style = "font-size: 13px;", 
                     
                     tags$b("Edit boxes:"), " Select a box in the Data tab, then use arrow buttons to move or +/- to resize."),
                   
                   p(style = "margin-bottom: 0; font-size: 13px;", 
                     
                     tags$b("Fuse peaks:"), " Use the lasso tool to select multiple peaks, then click 'Fuse'.")
                   
            )
            
          )
          
      ),
      
      
      
      # Step 5
      
      div(style = "background: #fff; border: 1px solid #ddd; border-radius: 8px; padding: 15px; margin-bottom: 10px;",
          
          fluidRow(
            
            column(1, div(style = "background: #fa709a; color: white; width: 30px; height: 30px; border-radius: 50%; text-align: center; line-height: 30px; font-weight: bold;", "5")),
            
            column(11,
                   
                   h5(style = "margin-top: 5px;", "Export Results"),
                   
                   p(style = "font-size: 13px;", 
                     
                     tags$b("Peaks:"), " Export peak positions (F1, F2 coordinates) and intensities."),
                   
                   p(style = "font-size: 13px;", 
                     
                     tags$b("Boxes:"), " Export bounding box coordinates and integrated intensities."),
                   
                   p(style = "margin-bottom: 0; font-size: 13px;", 
                     
                     tags$b("Batch Export:"), " Apply the same boxes to all loaded spectra and export intensities for each.")
                   
            )
            
          )
          
      ),
      
      
      
      # Output Format
      
      h4("📄 Output Format"),
      
      
      
      fluidRow(
        
        column(6,
               
               div(style = "background: #f5f5f5; padding: 15px; border-radius: 8px;",
                   
                   h5("Peaks CSV"),
                   
                   tags$code(style = "font-size: 11px;",
                             
                             "stain_id, F2_ppm, F1_ppm, Volume"
                             
                   )
                   
               )
               
        ),
        
        column(6,
               
               div(style = "background: #f5f5f5; padding: 15px; border-radius: 8px;",
                   
                   h5("Boxes CSV"),
                   
                   tags$code(style = "font-size: 11px;",
                             
                             "stain_id, xmin, xmax, ymin, ymax, Volume"
                             
                   )
                   
               )
               
        )
        
      ),
      
      
      
      br(),
      
      
      
      # Footer
      
      div(style = "text-align: center; color: #9e9e9e; font-size: 12px; margin-top: 20px;",
          
          p("Developed for metabolomics research"),
          
          p("For questions or bug reports, please contact the development team")
          
      )
      
    )
    
  })
  
  
  
  ## 10.2 Status messages ----
  
  output$status_message <- renderText({ status_msg() })
  
  output$matrix_dim <- renderPrint({ req(bruker_data()); dim(bruker_data()$spectrumData) })
  
  output$seuil_text <- renderText({
    
    val <- calculated_contour_value()
    
    if (is.null(val)) return("No threshold calculated.")
    
    round(val, 5)
    
  })
  
  
  
  ## 10.3 Interactive plot ----
  
  output$interactivePlot <- renderPlotly({
    
    plot_obj <- nmr_plot()
    
    
    
    if (is.null(plot_obj)) {
      
      return(ggplotly(ggplot() + theme_void() + ggtitle("No spectrum displayed")))
      
    }
    
    
    
    tryCatch({
      
      plot_obj <- plot_obj + 
        
        theme(legend.text = element_text(size = 8), 
              
              legend.title = element_text(size = 9), 
              
              legend.key.size = unit(0.4, "cm"))
      
      
      
      # Ticks fixes simples : tous les 1 ppm pour F2
      
      # Pour F1 : 1 ppm pour TOCSY/COSY/UFCOSY (homonucléaire 1H), 10 ppm pour HSQC (13C)
      
      # Les valeurs sur l'axe sont négatives, on les inverse pour l'affichage (ex: -3 devient 3)
      
      # SAUF pour les vraies valeurs négatives ppm (ex: -0.5 ppm reste -0.5)
      
      x_tickvals <- seq(-14, 2, by = 1)  # Couvre de -2 à 14 ppm en affichage
      
      x_ticktext <- sprintf("%.0f", -x_tickvals)  # Inverse le signe : -7 -> "7", 1 -> "-1"
      
      
      
      # Adapter F1 selon le type de spectre
      
      is_hsqc <- !is.null(input$spectrum_type) && input$spectrum_type == "HSQC"
      
      if (is_hsqc) {
        
        # HSQC : F1 = 13C, ticks tous les 10 ppm (-10 à 230 ppm en affichage)
        
        y_tickvals <- seq(-240, 20, by = 10)
        
        y_ticktext <- sprintf("%.0f", -y_tickvals)
        
      } else {
        
        # TOCSY/COSY/UFCOSY : F1 = 1H, ticks tous les 1 ppm (-2 à 14 ppm en affichage)
        
        y_tickvals <- seq(-14, 2, by = 1)
        
        y_ticktext <- sprintf("%.0f", -y_tickvals)
        
      }
      
      
      
      p <- suppressWarnings({
        
        ggplotly(plot_obj, source = "nmr_plot") %>%
          
          layout(dragmode = "zoom",
                 
                 xaxis = list(
                   
                   showticklabels = TRUE, 
                   
                   ticks = "outside",
                   
                   tickmode = "array",
                   
                   tickvals = x_tickvals,
                   
                   ticktext = x_ticktext,
                   
                   title = list(text = "F2 (ppm)", standoff = 10),
                   
                   gridcolor = "rgba(200,200,200,0.3)"
                   
                 ), 
                 
                 yaxis = list(
                   
                   showticklabels = TRUE, 
                   
                   ticks = "outside",
                   
                   tickmode = "array",
                   
                   tickvals = y_tickvals,
                   
                   ticktext = y_ticktext,
                   
                   title = list(text = "F1 (ppm)", standoff = 10),
                   
                   gridcolor = "rgba(200,200,200,0.3)"
                   
                 )) %>%
          
          config(modeBarButtonsToAdd = list("select2d", "lasso2d"), displayModeBar = TRUE) %>%
          
          event_register("plotly_click") %>%
          
          event_register("plotly_selected") %>%
          
          event_register("plotly_relayout")
        
      })
      
      
      
      # Grille invisible pour capturer les clics
      
      # Plus la grille est dense, plus les clics sont précis
      
      x_range <- layer_scales(plot_obj)$x$range$range
      
      y_range <- layer_scales(plot_obj)$y$range$range
      
      
      
      if (!is.null(x_range) && !is.null(y_range)) {
        
        # Grille très dense: 100x100 = 10000 points pour une précision maximale
        
        n_points <- 150
        
        grid <- expand.grid(x = seq(x_range[1], x_range[2], length.out = n_points),
                            
                            y = seq(y_range[1], y_range[2], length.out = n_points))
        
        
        
        p <- p %>%
          
          add_trace(x = grid$x, y = grid$y, type = "scatter", mode = "markers",
                    
                    marker = list(size = 6, opacity = 0),
                    
                    hoverinfo = "none", showlegend = FALSE, name = "click_capture")
        
      }
      
      
      
      p
      
      
      
    }, error = function(e) {
      
      showNotification(paste("Plot error:", e$message), type = "error")
      
      ggplotly(ggplot() + theme_void() + ggtitle("Error"))
      
    })
    
  })
  
  
  
  ## 10.4 Click indicators ----
  
  output$clickedCoords <- renderPrint({
    
    coords <- last_click_coords()
    
    if (is.null(coords)) "Click on the spectrum" 
    
    else paste0("F2=", round(-coords$F2_ppm, 4), ", F1=", round(-coords$F1_ppm, 4))
    
  })
  
  
  
  output$click_coords_display <- renderText({
    
    click_data <- event_data("plotly_click", source = "nmr_plot")
    
    if (is.null(click_data)) return("Click on the spectrum")
    
    sprintf("F2 = %.4f ppm, F1 = %.4f ppm", -click_data$x, -click_data$y)
    
  })
  
  
  
  output$two_click_indicator <- renderUI({
    
    first_click <- first_click_for_box()
    
    if (is.null(first_click)) {
      
      div(style = "background-color: #e3f2fd; padding: 10px;",
          
          icon("mouse-pointer"), " Click for first corner")
      
    } else {
      
      div(style = "background-color: #fff3e0; padding: 10px;",
          
          icon("check"), sprintf(" Corner 1: F2=%.3f, F1=%.3f", first_click$f2, first_click$f1),
          
          br(), icon("mouse-pointer"), " Click for opposite corner")
      
    }
    
  })
  
  
  
  output$last_click_for_box <- renderText({
    
    first_click <- first_click_for_box()
    
    if (!is.null(first_click)) {
      
      return(sprintf("Corner 1: F2=%.4f, F1=%.4f", first_click$f2, first_click$f1))
      
    }
    
    click_data <- event_data("plotly_click", source = "nmr_plot")
    
    if (is.null(click_data)) return("No click recorded")
    
    sprintf("F2 = %.4f ppm, F1 = %.4f ppm", -click_data$x, -click_data$y)
    
  })
  
  
  
  ## 10.5 Tables ----
  
  output$centroid_table <- renderDT({
    
    df <- centroids_data()
    
    if (is.null(df) || nrow(df) == 0) return(datatable(data.frame()))
    
    datatable(df[, seq_len(min(4, ncol(df))), drop = FALSE], 
              
              selection = "multiple",  # Sélection multiple activée
              
              options = list(pageLength = 10))  # Plus d'entrées par page
    
  })
  
  
  
  output$full_centroid_table <- renderDT({
    
    df <- centroids_data() %||% data.frame()
    
    datatable(df, selection = "multiple", options = list(pageLength = 10))
    
  })
  
  
  
  output$bbox_table <- renderDT({
    
    df <- bounding_boxes_data() %||% data.frame()
    
    datatable(df, 
              
              selection = "multiple",  # Sélection multiple activée
              
              options = list(pageLength = 10))  # Plus d'entrées par page
    
  })
  
  
  
  output$pending_centroids_table <- renderDT({ 
    
    datatable(pending_centroids(), selection = "multiple", options = list(pageLength = 10)) 
    
  })
  
  
  
  output$pending_boxes_table <- renderDT({ 
    
    datatable(pending_boxes(), selection = "multiple", options = list(pageLength = 10)) 
    
  })
  
  
  
  output$pending_fusions_table <- renderDT({ 
    
    req(pending_fusions())
    
    datatable(pending_fusions(), selection = "multiple", options = list(scrollX = TRUE, pageLength = 10)) 
    
  })
  
  
  
  ## 10.6 Spectrum type update ----
  
  observeEvent(input$spectrum_type, {
    
    params <- switch(input$spectrum_type,
                     
                     "TOCSY" = list(contour_start = 80000, eps_value = 0.0068),
                     
                     "HSQC" = list(contour_start = 20000, eps_value = 0.0068),
                     
                     "COSY" = list(contour_start = 80000, eps_value = 0.0068),
                     
                     "UFCOSY" = list(contour_start = 30000, eps_value = 0.014))
    
    updateNumericInput(session, "contour_start", value = params$contour_start)
    
    updateNumericInput(session, "eps_value", value = params$eps_value)
    
  })
  
  
  
  ## 10.7 Dragmode update ----
  
  observeEvent(input$plotly_dragmode, {
    
    plotlyProxy("interactivePlot", session) %>%
      
      plotlyProxyInvoke("relayout", list(dragmode = input$plotly_dragmode))
    
  })
  
  
  
  ## 10.7b Dynamic axis ticks on zoom ----
  
  # Stocker les ranges complets du plot pour l'autoscale
  
  full_plot_ranges <- reactiveVal(list(x = c(-10, 0), y = c(-200, 0)))
  
  
  
  # Observer pour capturer les ranges initiaux quand le plot est créé
  
  observe({
    
    plot_obj <- nmr_plot()
    
    if (!is.null(plot_obj)) {
      
      x_range <- tryCatch({
        
        layer_scales(plot_obj)$x$range$range
        
      }, error = function(e) c(-10, 0))
      
      y_range <- tryCatch({
        
        layer_scales(plot_obj)$y$range$range
        
      }, error = function(e) c(-200, 0))
      
      
      
      if (!is.null(x_range) && !is.null(y_range)) {
        
        full_plot_ranges(list(x = x_range, y = y_range))
        
      }
      
    }
    
  })
  
  
  
  # Fonction pour générer des ticks avec les bonnes valeurs ppm affichées
  
  # Les données sur l'axe sont l'opposé des valeurs ppm réelles (ex: ppm=3 -> axe=-3)
  
  generate_positive_ticks <- function(range_vals, target_nticks = 10, decimals = 2) {
    
    if (any(is.null(range_vals)) || any(is.na(range_vals)) || length(range_vals) < 2) return(NULL)
    
    
    
    # Range sur l'axe (valeurs négatives des ppm, ex: -7.5 à 0.5 si ppm va de 7.5 à -0.5)
    
    axis_min <- min(range_vals)
    
    axis_max <- max(range_vals)
    
    
    
    # Valeurs ppm correspondantes (on inverse le signe)
    
    ppm_max <- -axis_min  # ex: 7.5
    
    ppm_min <- -axis_max  # ex: -0.5
    
    
    
    span <- ppm_max - ppm_min
    
    
    
    if (span <= 0 || !is.finite(span)) return(NULL)
    
    
    
    # Calculer un pas "joli"
    
    rough_step <- span / target_nticks
    
    if (rough_step <= 0 || !is.finite(rough_step)) return(NULL)
    
    
    
    magnitude <- 10^floor(log10(rough_step))
    
    nice_steps <- c(0.05, 0.1, 0.2, 0.25, 0.5, 1, 2, 2.5, 5, 10)
    
    step <- magnitude * nice_steps[which.min(abs(nice_steps - rough_step/magnitude))]
    
    
    
    if (step <= 0 || !is.finite(step)) return(NULL)
    
    
    
    # Générer les valeurs de tick en ppm
    
    tick_start <- floor(ppm_min / step) * step
    
    tick_end <- ceiling(ppm_max / step) * step
    
    
    
    tick_values_ppm <- seq(tick_start, tick_end, by = step)
    
    
    
    # Filtrer pour ne garder que les ticks dans la plage visible (avec petite marge)
    
    margin <- step * 0.1
    
    tick_values_ppm <- tick_values_ppm[
      
      tick_values_ppm >= (ppm_min - margin) & 
        
        tick_values_ppm <= (ppm_max + margin)
      
    ]
    
    
    
    if (length(tick_values_ppm) == 0) return(NULL)
    
    
    
    # Les valeurs sur l'axe sont l'opposé des ppm
    
    tick_values_axis <- -tick_values_ppm
    
    
    
    # Format selon le nombre de décimales
    
    fmt <- paste0("%.", decimals, "f")
    
    
    
    list(
      
      tickvals = tick_values_axis,
      
      ticktext = sprintf(fmt, tick_values_ppm)  # Affiche les vraies valeurs ppm
      
    )
    
  }
  
  
  
  # Variable pour éviter les boucles de relayout (gardé pour compatibilité)
  
  last_tick_update <- reactiveVal(0)
  
  
  
  # Note: La mise à jour des ticks lors du zoom est gérée côté client (JavaScript)
  
  # pour une meilleure fluidité. Cet observer est gardé en backup pour les cas
  
  # où le JavaScript ne se déclenche pas correctement.
  
  observeEvent(event_data("plotly_relayout", source = "nmr_plot"), {
    
    # Les ticks sont maintenant gérés par le JavaScript côté client
    
    # pour une mise à jour synchrone avec le zoom
    
    NULL
    
  }, ignoreNULL = FALSE)
  
  
  
  ## 10.8 Pending changes indicators ----
  
  output$has_pending_changes <- reactive({
    
    n_centroids <- nrow(pending_centroids() %||% data.frame())
    
    n_boxes <- nrow(pending_boxes() %||% data.frame())
    
    n_fusions <- nrow(pending_fusions() %||% data.frame())
    
    (n_centroids + n_boxes + n_fusions) > 0
    
  })
  
  outputOptions(output, "has_pending_changes", suspendWhenHidden = FALSE)
  
  
  
  output$pending_summary <- renderText({
    
    n_centroids <- nrow(pending_centroids() %||% data.frame())
    
    n_boxes <- nrow(pending_boxes() %||% data.frame())
    
    n_fusions <- nrow(pending_fusions() %||% data.frame())
    
    
    
    parts <- c()
    
    if (n_centroids > 0) parts <- c(parts, paste(n_centroids, "peaks"))
    
    if (n_boxes > 0) parts <- c(parts, paste(n_boxes, "boxes"))
    
    if (n_fusions > 0) parts <- c(parts, paste(n_fusions, "fusions"))
    
    
    
    if (length(parts) == 0) return("")
    
    paste(parts, collapse = ", ")
    
  })
  
  
  
  observeEvent(spectra_list(), { centroids(NULL) })
  
  
  
  
  
  ## 10.9 Fit Quality Visualizations ----
  
  
  
  # Données combinées : boxes + fit results
  
  boxes_with_fit <- reactive({
    
    boxes <- modifiable_boxes()
    
    fit_data <- fit_results_data()
    
    
    
    if (is.null(boxes) || nrow(boxes) == 0) return(NULL)
    
    if (is.null(fit_data)) return(boxes)  # Retourner boxes sans fit info
    
    
    
    # Joindre les données de fit avec les boxes
    
    boxes_merged <- boxes %>%
      
      left_join(fit_data, by = "stain_id")
    
    
    
    boxes_merged
    
  })
  
  
  
  # Table résumée des résultats de fitting
  
  fit_summary_data <- reactive({
    
    boxes <- boxes_with_fit()
    
    
    
    if (is.null(boxes) || !"fit_method" %in% names(boxes)) {
      
      return(NULL)
      
    }
    
    
    
    # Résumé par méthode
    
    summary_df <- boxes %>%
      
      filter(!is.na(fit_method)) %>%
      
      group_by(fit_method) %>%
      
      summarise(
        
        n_boxes = n(),
        
        mean_r2 = mean(r_squared, na.rm = TRUE),
        
        median_r2 = median(r_squared, na.rm = TRUE),
        
        min_r2 = min(r_squared, na.rm = TRUE),
        
        max_r2 = max(r_squared, na.rm = TRUE),
        
        .groups = "drop"
        
      )
    
    
    
    summary_df
    
  })
  
  
  
  # Plot de distribution des R²
  
  output$fit_quality_plot <- renderPlotly({
    
    boxes <- boxes_with_fit()
    
    
    
    # Vérifier si on a des données de fit
    
    if (is.null(boxes) || !"r_squared" %in% names(boxes) || all(is.na(boxes$r_squared))) {
      
      p <- ggplot() +
        
        annotate("text", x = 0.5, y = 0.5, 
                 
                 label = "No fit quality data available.\n\nSteps to generate fit data:\n1. Select 'Gaussian fit' or 'Voigt fit' in Export section\n2. Click 'Batch Export' button\n3. Return to this tab to see results",
                 
                 size = 4, color = "gray50", hjust = 0.5, vjust = 0.5) +
        
        theme_void() +
        
        ggtitle("Fit Quality Distribution")
      
      
      
      return(ggplotly(p))
      
    }
    
    
    
    # Filtrer les NA
    
    boxes_with_r2 <- boxes %>% filter(!is.na(r_squared))
    
    
    
    if (nrow(boxes_with_r2) == 0) {
      
      p <- ggplot() +
        
        annotate("text", x = 0.5, y = 0.5, 
                 
                 label = "No R² values computed yet.\nRun 'Batch Export' with fitting method.",
                 
                 size = 5, color = "gray50") +
        
        theme_void()
      
      return(ggplotly(p))
      
    }
    
    
    
    # Histogramme des R² avec couleurs selon la méthode
    
    p <- ggplot(boxes_with_r2, aes(x = r_squared, fill = fit_method)) +
      
      geom_histogram(bins = 30, color = "white", alpha = 0.8, position = "stack") +
      
      geom_vline(aes(xintercept = median(r_squared, na.rm = TRUE)), 
                 
                 color = "red", linetype = "dashed", size = 1) +
      
      scale_fill_manual(
        
        values = c("gaussian" = "#667eea", "voigt" = "#f5576c", "multiplet_fit" = "#38ef7d", "sum_fallback" = "#ffd93d"),
        
        name = "Fit Method"
        
      ) +
      
      labs(
        
        title = "Distribution of Fit Quality (R²)",
        
        subtitle = paste0("Median R² = ", round(median(boxes_with_r2$r_squared, na.rm = TRUE), 3),
                          
                          " | Total boxes = ", nrow(boxes_with_r2)),
        
        x = "R² (Coefficient of Determination)",
        
        y = "Number of Peaks"
        
      ) +
      
      theme_minimal() +
      
      theme(
        
        plot.title = element_text(face = "bold", size = 14),
        
        plot.subtitle = element_text(color = "gray30")
        
      )
    
    
    
    ggplotly(p) %>%
      
      layout(hovermode = "x unified")
    
  })
  
  
  
  # Reactive pour récupérer la box sélectionnée (depuis fit_boxes_detail_table)
  
  selected_fit_box <- reactive({
    
    boxes <- boxes_with_fit()
    
    if (is.null(boxes) || !"fit_method" %in% names(boxes)) return(NULL)
    
    
    
    # Récupérer la sélection dans fit_boxes_detail_table
    
    selected_row <- input$fit_boxes_detail_table_rows_selected
    
    if (is.null(selected_row) || length(selected_row) == 0) return(NULL)
    
    
    
    # Reconstruire le même ordre que dans la table (trié par R² décroissant)
    
    detail_df <- boxes %>%
      
      filter(!is.na(fit_method)) %>%
      
      arrange(desc(r_squared))
    
    
    
    if (selected_row > nrow(detail_df)) return(NULL)
    
    
    
    # Récupérer le stain_id sélectionné
    
    selected_stain_id <- detail_df$stain_id[selected_row]
    
    
    
    # Retourner la box correspondante depuis boxes_with_fit (avec toutes les colonnes)
    
    boxes %>% filter(stain_id == selected_stain_id)
    
  })
  
  
  
  # Exemple de fit 2D pour une box sélectionnée
  
  output$example_fit_2d <- renderPlot({
    
    req(bruker_data())
    
    
    
    box <- selected_fit_box()
    
    
    
    if (is.null(box) || nrow(box) == 0) {
      
      plot.new()
      
      text(0.5, 0.5, "Select a box in the\n'Fitted Boxes Details' table\nto visualize its fit", cex = 1.3, col = "gray50")
      
      return()
      
    }
    
    
    
    box <- box[1, ]  # Prendre la première ligne si plusieurs
    
    
    
    # Vérifier que le fit existe
    
    if (!"fit_method" %in% names(box) || is.na(box$fit_method) || box$fit_method == "sum_fallback") {
      
      plot.new()
      
      text(0.5, 0.5, paste0("Box '", box$stain_id, "'\nwas not fitted successfully\n(used sum method or fit failed)"), 
           
           cex = 1.2, col = "orange")
      
      return()
      
    }
    
    
    
    # Extraire la région
    
    mat <- bruker_data()$spectrumData
    
    ppm_x <- suppressWarnings(as.numeric(colnames(mat)))
    
    ppm_y <- suppressWarnings(as.numeric(rownames(mat)))
    
    
    
    x_idx <- which(ppm_x >= box$xmin & ppm_x <= box$xmax)
    
    y_idx <- which(ppm_y >= box$ymin & ppm_y <= box$ymax)
    
    
    
    if (length(x_idx) == 0 || length(y_idx) == 0) {
      
      plot.new()
      
      text(0.5, 0.5, "Region out of bounds", cex = 1.5, col = "red")
      
      return()
      
    }
    
    
    
    region <- mat[y_idx, x_idx, drop = FALSE]
    
    x_sub <- ppm_x[x_idx]
    
    y_sub <- ppm_y[y_idx]
    
    
    
    # CORRECTION: S'assurer que x et y sont en ordre croissant pour image()
    
    if (is.unsorted(x_sub)) {
      
      x_order <- order(x_sub)
      
      x_sub <- x_sub[x_order]
      
      region <- region[, x_order, drop = FALSE]
      
    }
    
    if (is.unsorted(y_sub)) {
      
      y_order <- order(y_sub)
      
      y_sub <- y_sub[y_order]
      
      region <- region[y_order, , drop = FALSE]
      
    }
    
    
    
    # Créer le plot avec image() pour visualiser la 2D
    
    par(mfrow = c(1, 1), mar = c(4, 4, 3, 2))
    
    
    
    image(x_sub, y_sub, t(region), 
          
          col = viridis::viridis(100),
          
          xlab = "F2 (ppm)", ylab = "F1 (ppm)",
          
          main = paste0("Fitted Region: ", box$stain_id, 
                        
                        "\nMethod: ", box$fit_method,
                        
                        " | R² = ", round(box$r_squared, 3)))
    
    
    
    # Ajouter le centre fitté si disponible
    
    if (!is.na(box$center_x) && !is.na(box$center_y)) {
      
      points(box$center_x, box$center_y, pch = 3, col = "red", cex = 2, lwd = 2)
      
      
      
      # Ajouter aussi le centre de la box (pour comparaison)
      
      box_center_x <- (box$xmin + box$xmax) / 2
      
      box_center_y <- (box$ymin + box$ymax) / 2
      
      points(box_center_x, box_center_y, pch = 1, col = "cyan", cex = 2, lwd = 2)
      
      
      
      legend("topright", 
             
             legend = c("Fitted center", "Box center"), 
             
             pch = c(3, 1), 
             
             col = c("red", "cyan"), 
             
             bg = "white")
      
    }
    
    
    
    # Ajouter les contours
    
    contour(x_sub, y_sub, t(region), add = TRUE, col = "white", lwd = 0.5, nlevels = 10)
    
  })
  
  
  
  # Plot des résidus
  
  output$residuals_plot <- renderPlot({
    
    req(bruker_data())
    
    
    
    box <- selected_fit_box()
    
    
    
    if (is.null(box) || nrow(box) == 0) {
      
      plot.new()
      
      text(0.5, 0.5, "Select a box in the\n'Fitted Boxes Details' table\nto see residuals", cex = 1.3, col = "gray50")
      
      return()
      
    }
    
    
    
    box <- box[1, ]  # Prendre la première ligne si plusieurs
    
    
    
    if (!"fit_method" %in% names(box) || is.na(box$fit_method) || box$fit_method == "sum_fallback") {
      
      plot.new()
      
      text(0.5, 0.5, "No fit residuals available\n(sum method used or fit failed)", cex = 1.2, col = "orange")
      
      return()
      
    }
    
    
    
    # Re-fitter pour obtenir les résidus
    
    mat <- bruker_data()$spectrumData
    
    ppm_x <- suppressWarnings(as.numeric(colnames(mat)))
    
    ppm_y <- suppressWarnings(as.numeric(rownames(mat)))
    
    
    
    fit_result <- fit_2d_peak(mat, ppm_x, ppm_y, box, model = box$fit_method)
    
    
    
    if (is.null(fit_result$residuals) || length(fit_result$residuals) == 0) {
      
      plot.new()
      
      text(0.5, 0.5, "Could not compute residuals", cex = 1.5, col = "red")
      
      return()
      
    }
    
    
    
    # Histogramme des résidus
    
    par(mfrow = c(1, 1), mar = c(4, 4, 3, 2))
    
    hist(fit_result$residuals, breaks = 30, 
         
         col = "#f5576c", border = "white",
         
         main = paste0("Fit Residuals: ", box$stain_id),
         
         xlab = "Residual (Observed - Fitted)",
         
         ylab = "Frequency")
    
    abline(v = 0, col = "blue", lwd = 2, lty = 2)
    
    abline(v = mean(fit_result$residuals), col = "red", lwd = 2, lty = 2)
    
    
    
    # Ajouter stats
    
    legend("topright", 
           
           legend = c(
             
             paste("Mean:", round(mean(fit_result$residuals), 2)),
             
             paste("SD:", round(sd(fit_result$residuals), 2)),
             
             paste("R²:", round(box$r_squared, 3)),
             
             "Blue = 0",
             
             "Red = Mean"
             
           ),
           
           bg = "white", cex = 0.9)
    
  })
  
  
  
  # Table de résumé
  
  output$fit_summary_table <- renderDT({
    
    summary <- fit_summary_data()
    
    
    
    if (is.null(summary) || nrow(summary) == 0) {
      
      return(datatable(data.frame(Message = "No fit data available. Run 'Batch Export' with Gaussian or Voigt method.")))
      
    }
    
    
    
    datatable(summary, 
              
              options = list(pageLength = 10, dom = 't'),
              
              rownames = FALSE) %>%
      
      formatRound(columns = c('mean_r2', 'median_r2', 'min_r2', 'max_r2'), digits = 3)
    
  })
  
  
  
  # Table détaillée des boxes fittées
  
  output$fit_boxes_detail_table <- renderDT({
    
    boxes <- boxes_with_fit()
    
    
    
    if (is.null(boxes) || !"fit_method" %in% names(boxes)) {
      
      return(datatable(data.frame(Message = "No fit data available.")))
      
    }
    
    
    
    # Sélectionner les colonnes pertinentes
    
    detail_cols <- c("stain_id", "r_squared", "fit_method", "center_x", "center_y")
    
    available_cols <- intersect(detail_cols, names(boxes))
    
    
    
    if (length(available_cols) == 0) {
      
      return(datatable(data.frame(Message = "No fit columns available.")))
      
    }
    
    
    
    detail_df <- boxes %>%
      
      select(all_of(available_cols)) %>%
      
      filter(!is.na(fit_method)) %>%
      
      arrange(desc(r_squared))
    
    
    
    # Renommer pour affichage
    
    names(detail_df) <- gsub("stain_id", "Box Name", names(detail_df))
    
    names(detail_df) <- gsub("r_squared", "R²", names(detail_df))
    
    names(detail_df) <- gsub("fit_method", "Method", names(detail_df))
    
    names(detail_df) <- gsub("center_x", "Center F2", names(detail_df))
    
    names(detail_df) <- gsub("center_y", "Center F1", names(detail_df))
    
    
    
    datatable(detail_df, 
              
              options = list(
                
                pageLength = 10, 
                
                scrollY = "300px",
                
                scrollCollapse = TRUE,
                
                dom = 'ftp'
                
              ),
              
              rownames = FALSE,
              
              selection = 'single') %>%
      
      formatRound(columns = c('R²', 'Center F2', 'Center F1'), digits = 3) %>%
      
      formatStyle('R²',
                  
                  backgroundColor = styleInterval(
                    
                    c(0.7, 0.9),
                    
                    c('#ffcccc', '#ffffcc', '#ccffcc')
                    
                  ))
    
  })
  
  
  
} # end server



shinyApp(ui = ui, server = server)