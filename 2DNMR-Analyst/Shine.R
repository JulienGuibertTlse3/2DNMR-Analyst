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
library(tensorflow)
library(keras)

# --- Chargement des fichiers sources (chemins relatifs) ---
# Ces fichiers doivent être dans le sous-dossier Function_test/

source("Function/Read_2DNMR_spectrum.R")
source("Function/Vizualisation.R")
source("Function/Integration.R")
source("Function/Pping.R")
source("Function/CNN_shiny.R")

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
                                            numericInput("pct_val", "Percentage:", value = 0.001, min = 0.001, max = 1, step = 0.001)
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
                                        style = "flex: 1; font-size: 11px; padding: 5px 2px;"),
                           actionButton("generate_centroids_cnn", "CNN", 
                                        class = "btn-info btn-sm", 
                                        style = "flex: 1; font-size: 11px; padding: 5px 2px;")
                       ),
                       
                       tags$details(
                         tags$summary("⚙️ Options"),
                         div(
                           checkboxInput("disable_clustering", "No clustering", value = FALSE),
                           numericInput("eps_value", "Epsilon:", value = 0.01, min = 0, step = 0.001),
                           textAreaInput("keep_peak_ranges_text", "Delete ranges:", 
                                         value = "0.5,-0.5; 1,0.8; 1.55,1.45", rows = 2)
                         )
                       )
                     ),
                     
                     ##### ===== SECTION 4: MANUAL EDITING =====
                     bsCollapsePanel(
                       title = "✏️ 4. Manual Editing",
                       value = "panel_edit",
                       style = "primary",
                       
                       # Click mode
                       radioButtons("box_click_mode", "Add box by click:",
                                    choices = c("Off" = "disabled", "Two clicks" = "two_clicks"),
                                    selected = "disabled", inline = TRUE),
                       
                       conditionalPanel("input.box_click_mode == 'two_clicks'",
                                        uiOutput("two_click_indicator"),
                                        actionButton("cancel_first_click", "Cancel", class = "btn-warning btn-xs")
                       ),
                       
                       div(class = "click-coords", textOutput("click_coords_display")),
                       
                       hr(),
                       
                       # Quick actions
                       fluidRow(
                         column(6, actionButton("delete_bbox", "🗑️ Delete", class = "btn-danger btn-sm btn-block")),
                         column(6, actionButton("fuse_btn", "🔗 Fuse", class = "btn-warning btn-sm btn-block"))
                       ),
                       
                       br(),
                       
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
                             column(2, div(style = "padding-top: 25px;",
                                           actionButton("add_manual_centroid", "+", class = "btn-success btn-xs")
                             ))
                           ),
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
                     
                     ##### ===== SECTION 5: SAVE & EXPORT =====
                     bsCollapsePanel(
                       title = "💾 5. Save & Export",
                       value = "panel_export",
                       style = "primary",
                       
                       hr(),
                       
                       downloadButton("export_centroids", "📤 Peaks", class = "btn-sm"),
                       downloadButton("export_boxes", "📤 Boxes", class = "btn-sm"),
                       br(), br(),
                       downloadButton("export_batch_box_intensities", "📤 Batch Export", class = "btn-primary btn-sm btn-block"),
                       
                       hr(),
                       
                       tags$details(
                         tags$summary("📥 Import"),
                         div(
                           fileInput("import_centroids_file", "Peaks CSV:", accept = ".csv"),
                           fileInput("import_boxes_file", "Boxes CSV:", accept = ".csv")
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
                                DTOutput("centroid_table")
                         ),
                         column(6,
                                h4("🟦 Boxes"),
                                DTOutput("bbox_table")
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
  
  ## 2.2 Calcul d'intensité des boxes (vectorisé) ----
  get_box_intensity <- function(mat, ppm_x, ppm_y, boxes) {
    if (nrow(boxes) == 0) return(numeric(0))
    
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
  calculate_batch_box_intensities <- function(reference_boxes, spectra_list, apply_shift = FALSE) {
    
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
  
  
  # SECTION 4: PARAMÈTRES PAR TYPE DE SPECTRE ----
  
  
  spectrum_params <- reactive({
    switch(input$spectrum_type,
           "TOCSY" = list(intensity_threshold = 80000, contour_num = 40, contour_factor = 1.5, 
                          eps_value = 0.0068, neighborhood_size = 3),
           "HSQC"  = list(intensity_threshold = 30000, contour_num = 30, contour_factor = 1.3, 
                          eps_value = 0.002, neighborhood_size = 3),
           "COSY"  = list(intensity_threshold = 60000, contour_num = 30, contour_factor = 1.3, 
                          eps_value = 0.0068, neighborhood_size = 9),
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
    normalizePath(parseDirPath(roots, input$directory))
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
          keep_peak_ranges = keep_ranges
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
  observeEvent(input$generate_centroids_cnn, {
    req(bruker_data(), input$selected_subfolder)
    
    status_msg("🔄 [1/5] Preparing CNN input...")
    
    selected_spectrum <- bruker_data()$spectrumData
    if (is.null(selected_spectrum)) {
      showNotification("⚠️ Spectrum not found", type = "error")
      return()
    }
    
    status_msg("🔄 [2/5] Normalizing spectrum...")
    rr_abs <- abs(selected_spectrum)
    rr_norm <- (rr_abs - min(rr_abs)) / (max(rr_abs) - min(rr_abs))
    
    # DEBUG: Vérifier les inputs
    cat("=== DEBUG CNN ===\n")
    cat("rr_norm dim:", dim(rr_norm), "\n")
    cat("rr_norm class:", class(rr_norm), "\n")
    cat("model class:", class(new_model)[1], "\n")
    cat("params:", names(spectrum_params_CNN()), "\n")
    cat("threshold_class:", spectrum_params_CNN()$pred_class_thres, "\n")
    cat("batch_size:", spectrum_params_CNN()$batch_size, "\n")
    
    status_msg("🔄 [3/5] Running CNN detection...")
    
    result_peaks <- tryCatch({
      run_cnn_peak_picking(
        rr_norm = rr_norm,
        method = "batch",
        model = new_model,
        params = spectrum_params_CNN(),
        threshold_class = spectrum_params_CNN()$pred_class_thres,
        batch_size = spectrum_params_CNN()$batch_size,
        step = 4
      )
    }, error = function(e) {
      cat("❌ ERROR:", e$message, "\n")
      cat("Call:", deparse(e$call), "\n")
      showNotification(paste("❌ CNN error:", e$message), type = "error")
      return(NULL)
    })
    
    if (is.null(result_peaks)) return()
    
    status_msg("🔄 [4/5] Processing CNN results...")
    
    if (!is.null(result_peaks$boxes) && nrow(result_peaks$boxes) > 0) {
      # Créer les peaks (centroïdes) à partir des centres des bounding boxes
      result_peaks$peaks <- result_peaks$boxes %>%
        dplyr::transmute(
          F1 = as.integer(round(cy_ppm)), F2 = as.integer(round(cx_ppm)),
          F1_ppm = cy_ppm, F2_ppm = cx_ppm,
          Volume = intensity, cluster_db = cluster_db
        )
      
      # IMPORTANT: Assigner les centroïdes pour qu'ils s'affichent sur le plot
      centroids_data(result_peaks$peaks)
      
      # Préparer les bounding boxes
      result_peaks$boxes$stain_id <- seq_len(nrow(result_peaks$boxes))
      result_peaks$boxes$Volume <- result_peaks$boxes$intensity
      result_peaks$boxes$xmin <- result_peaks$boxes$xmin_ppm
      result_peaks$boxes$xmax <- result_peaks$boxes$xmax_ppm
      result_peaks$boxes$ymin <- result_peaks$boxes$ymin_ppm
      result_peaks$boxes$ymax <- result_peaks$boxes$ymax_ppm
      
      box_coords_only <- result_peaks$boxes[, c("xmin", "xmax", "ymin", "ymax", "stain_id")]
      fixed_boxes(box_coords_only)
      modifiable_boxes(fixed_boxes())
      reference_boxes(fixed_boxes())
    }
    
    status_msg("🔄 [5/5] Updating plot...")
    
    contour_plot_base(result_data_list()[[input$selected_subfolder]]$plot + labs(title = ""))
    refresh_nmr_plot()
    
    # Résumé final
    n_boxes <- nrow(result_peaks$boxes %||% data.frame())
    status_msg(paste0("✅ CNN complete: ", n_boxes, " peaks detected"))
    showNotification(paste0("✅ CNN found ", n_boxes, " peaks"), type = "message", duration = 4)
  })
  
  
  
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
  observeEvent(input$bbox_table_rows_selected, {
    selected <- input$bbox_table_rows_selected
    
    if (length(selected) > 0 && !is.null(selected)) {
      boxes <- bounding_boxes_data()
      if (!is.null(boxes) && nrow(boxes) >= selected) {
        box <- boxes[selected, ]
        selected_box_for_edit(box)
        selected_box_index(selected)
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
      xmin = input$edit_box_xmin,
      xmax = input$edit_box_xmax,
      ymin = input$edit_box_ymin,
      ymax = input$edit_box_ymax,
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
    
    # Gestion du mode deux clics
    if (is.null(input$box_click_mode) || input$box_click_mode != "two_clicks") return()
    if (is.null(click_data$x) || is.null(click_data$y)) return()
    if (is.na(click_data$x) || is.na(click_data$y)) return()
    
    f2_ppm <- -click_data$x
    f1_ppm <- -click_data$y
    
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
    
    if (!is.null(pending_cents) && nrow(pending_cents) > 0) {
      if (is.null(current_centroids)) current_centroids <- data.frame()
      centroids_data(dplyr::bind_rows(current_centroids, pending_cents))
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
    nmr_plot(NULL)
    contour_plot_base(NULL)
    imported_centroids(NULL)
    centroids_data(NULL)
    fixed_boxes(NULL)
    reference_boxes(NULL)
    first_click_for_box(NULL)
    updateSelectInput(session, "selected_subfolder", selected = "")
    status_msg("🔁 Interface reset")
  })
  
  # SECTION 9: IMPORT/EXPORT ----
  
  
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
      if (!is.null(df) && nrow(df) > 0) write.csv(df, file, row.names = FALSE) 
      else write.csv(data.frame(), file)
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
        readr::write_csv(tibble::tibble(message = "No contour_data found."), file)
        return(invisible(NULL))
      }
      
      merged_data <- Reduce(function(x, y) dplyr::full_join(x, y, by = "stain_id"), intensity_list)
      final_data <- dplyr::left_join(boxes_ref, merged_data, by = "stain_id") %>%
        dplyr::select(stain_id, F2_ppm, F1_ppm, xmin, xmax, ymin, ymax, dplyr::starts_with("Intensity_"))
      
      readr::write_csv(final_data, file)
    }
  )
  
  ## 9.4 Export batch box intensities ----
  output$export_batch_box_intensities <- downloadHandler(
    filename = function() paste0("batch_box_intensities_", Sys.Date(), ".csv"),
    content = function(file) {
      req(reference_boxes(), spectra_list())
      
      status_msg("🔄 Calculating batch intensities...")
      
      tryCatch({
        ref_boxes <- reference_boxes()
        
        if (is.null(ref_boxes) || nrow(ref_boxes) == 0) {
          showNotification("⚠️ No reference boxes found", type = "warning")
          return()
        }
        
        batch_intensities <- calculate_batch_box_intensities(
          reference_boxes = ref_boxes,
          spectra_list = spectra_list(),
          apply_shift = FALSE
        )
        
        # Remplacer valeurs négatives par 0
        intensity_cols <- grep("^Intensity_", names(batch_intensities), value = TRUE)
        for (col in intensity_cols) {
          batch_intensities[[col]] <- pmax(batch_intensities[[col]], 0)
        }
        
        readr::write_csv(batch_intensities, file)
        
        status_msg("✅ Batch export complete")
        showNotification(paste("✅ Exported", nrow(ref_boxes), "boxes,", 
                               length(spectra_list()), "spectra"), type = "message")
        
      }, error = function(e) {
        showNotification(paste("❌ Export error:", e$message), type = "error")
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
        readr::write_csv(projected_centroids, output_csv)
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
        readr::write_csv(merged_data, summary_csv)
        csv_files <- c(csv_files, summary_csv)
      }
      
      zip(zipfile, files = csv_files, flags = "-j")
    }
  )
  
  ## 9.6 Save directory ----
  save_roots <- c(Home = normalizePath("~"), Root = "/")
  shinyDirChoose(input, "save_directory", roots = save_roots, session = session)
  save_directory <- reactive({ req(input$save_directory); parseDirPath(save_roots, input$save_directory) })
  output$save_dir_display <- renderPrint({ save_directory() })
  
  
  # SECTION 10: UI OUTPUTS ----
  
  
  ## 10.1 Description ----
  output$toolDescription <- renderUI({
    tags$div(
      
      # Import de la font Orbitron
      tags$style(HTML("
  @import url('https://fonts.googleapis.com/css2?family=Orbitron:wght@400;700;900&display=swap');
")),
      
      div(
        style = "
    background: rgba(8, 8, 16, 1);
    border-radius: 20px;
    padding: 25px 35px;
    display: flex;
    align-items: center;
    gap: 30px;
    margin-bottom: 20px;
    box-shadow: 0 0 40px rgba(196,77,255,0.15), inset 0 0 60px rgba(196,77,255,0.05);
    border: 1px solid rgba(196,77,255,0.15);
  ",
        
        img(src = "spin.png", height = "130px", 
            style = "filter: drop-shadow(0 0 25px rgba(255,107,107,0.4));"),
        
        div(
          style = "flex: 1;",
          h2(
            style = "
        font-family: 'Orbitron', sans-serif;
        font-size: 36px;
        font-weight: 900;
        letter-spacing: 10px;
        margin: 0;
        background: linear-gradient(135deg, #ff6b6b 0%, #ffd93d 25%, #6bcb77 50%, #4d96ff 75%, #9b59b6 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
      ",
            "SPIN"
          ),
          p(
            style = "color: rgba(255,255,255,0.6); font-size: 13px; margin-top: 8px;",
            HTML("<span style='color: #c44dff; font-weight: 600;'>S</span>harp <span style='color: #c44dff; font-weight: 600;'>P</span>eak <span style='color: #c44dff; font-weight: 600;'>I</span>dentification for 2D <span style='color: #c44dff; font-weight: 600;'>N</span>MR")
          )
        )
      ),
      
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
                  tags$li("If you want to process a batch, you might want to select the folder, only select a QC or the most intense spectrum. Process it (Step 1), then reload every spectra and use the 'batch export' with the peaks selected on the first spectrum.")
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
      
      p <- suppressWarnings({
        ggplotly(plot_obj, source = "nmr_plot") %>%
          layout(dragmode = "zoom",
                 xaxis = list(showticklabels = TRUE, ticks = "outside"), 
                 yaxis = list(showticklabels = TRUE, ticks = "outside")) %>%
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
    datatable(df[, seq_len(min(4, ncol(df))), drop = FALSE], selection = "single", options = list(pageLength = 5))
  })
  
  output$full_centroid_table <- renderDT({
    df <- centroids_data() %||% data.frame()
    datatable(df, selection = "single", options = list(pageLength = 5))
  })
  
  output$bbox_table <- renderDT({
    df <- bounding_boxes_data() %||% data.frame()
    datatable(df, selection = "single", options = list(pageLength = 5))
  })
  
  output$pending_centroids_table <- renderDT({ 
    datatable(pending_centroids(), selection = "single", options = list(pageLength = 5)) 
  })
  
  output$pending_boxes_table <- renderDT({ 
    datatable(pending_boxes(), selection = "single", options = list(pageLength = 5)) 
  })
  
  output$pending_fusions_table <- renderDT({ 
    req(pending_fusions())
    datatable(pending_fusions(), selection = "single", options = list(scrollX = TRUE, pageLength = 5)) 
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
  
} # end server

shinyApp(ui = ui, server = server)