library(shiny)
library(shinyFiles)
library(plotly)
library(ggplot2)
library(DT)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(dplyr)

source("C://Users//juguibert//Documents//Function_test//Read_2DNMR_spectrum.R")
source("C://Users//juguibert//Documents//Function_test//Vizualisation.R")
source("C://Users//juguibert//Documents//Function_test//Integration.R")

# Interface ----

ui <- fluidPage(
  # Custom CSS to allow full-screen resizing of the plot and README iframe
  tags$head(
    tags$style(HTML("
      #interactivePlot {
        width: 100% !important;
        height: 100vh !important;
      }

      .readme-frame {
        width: 100vw;
        height: 90vh;
        border: none;
      }

      html, body, .content-wrapper {
        overflow-x: hidden;
      }


 /* Limit the width of the selectInput and ensure text does not overflow */
      .selectize-input {
        max-width: 100%;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }

      /* Style for the dropdown list to ensure it doesn't overflow */
      .selectize-dropdown {
        max-width: 100%;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }

      /* Optional: Add a container for the subfolder path with a max width */
      #subfolder-path {
        max-width: 100%;
        overflow-x: auto;
        white-space: nowrap;
      }
    
      /* Make DT tables responsive with horizontal scroll */
  .dataTables_wrapper {
    overflow-x: auto;
  }

  .tab-content .dataTables_wrapper {
    padding-top: 10px;
  }

  /* Ensure DT tables don't overflow box */
  table.dataTable {
    width: 100% !important;
  }
        
                    "))
    
    
  ),
  
  ## DashBoard ----
  
  dashboardPage(
    dashboardHeader(title = "NMR 2D Spectra Analysis"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("README", tabName = "readme", icon = icon("book")),
        menuItem("Visualisation", tabName = "visualisation", icon = icon("chart-area")),
        menuItem("Centroids", tabName = "centroids", icon = icon("table"))
      )
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "readme",
                fluidRow(
                  column(
                    width = 12,
                    # Container that holds the description content
                    div(
                      class = "tool-description",
                      uiOutput("toolDescription")  # Dynamically rendered content
                    )
                  )
                )
        ),
        
        tabItem(tabName = "visualisation",
                fluidRow(
                  column(3,
                         tabBox(
                           width = 12,
                           title = "Paramètres",
                           id = "param_tabs",
                           
                           tabPanel("📂 Chargement",
                                    div(tags$h4("Browse Data Folder"), style = "font-size: 18px; font-weight: 700;"),
                                    shinyDirButton("directory", "Select Main Directory", "Select Directory"),
                                    verbatimTextOutput("selected_dir"),
                                    uiOutput("subfolder_selector"),
                                    actionButton("load_data", "📂 Load Data"),
                                    withSpinner(verbatimTextOutput("matrix_dim"), type = 4, color = "#007bff"),
                                    
                                    actionButton("reset_all", "🔁 Réinitialiser l'interface", icon = icon("redo")),
                                    
                                    
                                    selectInput("spectrum_type", "Type de spectre :",
                                                choices = c("TOCSY", "HSQC", "COSY"),
                                                selected = "TOCSY"),
                                    
                                    actionButton("calculate_contour", "📈 Calculer valeur contour"),
                                    verbatimTextOutput("calculated_contour_text"),
                                    
                                    # Valeur modifiable par l'utilisateur
                                    numericInput("contour_start", "Valeur de départ des contours :", value = NULL, min = 0, step = 100),
                                    numericInput("intensity_threshold", "Valeur d'intensité :", value = 50000, min = 0, step = 100),
                                    numericInput("eps_value", "Valeur pour clustering :", value = 0.01, min = 0, step = 0.001),
                                    
                                    
                                    actionButton("generate_plot", "📊 Generate Plot"),
                                    actionButton("generate_centroids", "Generate Centroids / Bounding Boxes"),
                                    textOutput("status_message"),
                                    actionButton("export_projected_centroids", "📤 Export intensity across all spectra")
                                    

                           ),
                           
                           tabPanel("🔵 Centroïdes",
                                    actionButton("toggle_centroid_section", "Ajouter/Supprimer un centroïde"),
                                    hidden(div(
                                      id = "centroid_section",
                                      tags$h4("➕ Ajouter un centroïde manuellement"),
                                      numericInput("manual_f2", "F2 ppm (x) :", value = 4.0, step = 0.01),
                                      numericInput("manual_f1", "F1 ppm (y) :", value = 3.5, step = 0.01),
                                      actionButton("add_manual_centroid", "Ajouter le centroïde 🔵"),
                                      downloadButton("download_centroids", "Sauvegarder les centroïdes")
                                    )),
                                    hidden(div(
                                      id = "delete_centroid_section",
                                      tags$h4("❌ Supprimer un centroïde"),
                                      actionButton("delete_centroid", "Supprimer le centroïde sélectionné 🗑️")
                                    ))
                           ),
                           
                           tabPanel("🟦 Boîtes",
                                    hidden(div(
                                      id = "box_section",
                                      tags$h4("➕ Ajouter une boîte englobante manuellement"),
                                      numericInput("manual_xmin", "xmin (F2 ppm) :", value = 3.5, step = 0.01),
                                      numericInput("manual_xmax", "xmax (F2 ppm) :", value = 4.0, step = 0.01),
                                      numericInput("manual_ymin", "ymin (F1 ppm) :", value = 2.0, step = 0.01),
                                      numericInput("manual_ymax", "ymax (F1 ppm) :", value = 3.0, step = 0.01),
                                      actionButton("add_manual_bbox", "Ajouter la boîte 🟦")
                                    )),
                                    hidden(div(
                                      id = "delete_box_section",
                                      tags$h4("❌ Supprimer une boîte englobante"),
                                      actionButton("delete_bbox", "Supprimer la boîte sélectionnée 🗑️")
                                    ))
                           ),
                           
                           tabPanel("🔁 Import/Export",
                                    actionButton("export_centroids", "Exporter les centroïdes"),
                                    verbatimTextOutput("centroids_output"),
                                    tags$h4("📥 Importer des centroïdes"),
                                    fileInput("import_centroids_file", "Importer un fichier CSV :", accept = ".csv"),
                                    downloadButton("export_boxes", "Exporter les boîtes englobantes")
                                    
                           ),
                           
                           tabPanel("Batch processing",
                                    shinyDirButton("save_directory", "Choose output folder", "Please select a folder"),
                                    verbatimTextOutput("save_dir_display"),
                                    actionButton("run_batch", "Calculer intensités (batch)")
                                    )
                         )
                  ),
                  
                  column(9, 
                         # Messages de chargement
                         div(id = "loading_message", 
                             "Generating plot, please wait...", 
                             style = "font-size: 18px; color: blue; font-weight: bold; display: none;"),
                         div(id = "export_loading_message", 
                             "Exporting centroids, please wait...", 
                             style = "font-size: 18px; color: blue; font-weight: bold; display: none;"),
                         
                         # Graphe interactif pleine largeur
                         withSpinner(
                           div(id = "interactivePlot", 
                               plotlyOutput("interactivePlot", height = "600px", width = "100%"))
                         ),
                         
                         br(), tags$hr(), br(),
                         
                         # Onglets pour les dataframes
                         box(
                           title = "Données associées",
                           width = 12,
                           solidHeader = TRUE,
                           status = "primary",
                           tabBox(
                             width = 12,
                             id = "data_tabs",
                             tabPanel("🔵 Centroïdes", 
                                      tags$h4("Table des centroïdes détectés ou ajoutés"),
                                      DTOutput("centroid_table")
                             ),
                             tabPanel("🟦 Boîtes englobantes", 
                                      tags$h4("Table des boîtes englobantes"),
                                      DTOutput("bbox_table")
                             )
                           )
                         )
                  )
                  
                )
        ),
        
        tabItem(tabName = "centroids",
                DTOutput("full_centroid_table")
        )
      )
    )
   
  )
)


# Functions ----

server <- function(input, output, session) {
  
  ## Description ----
  
  # Render the tool description directly
  output$toolDescription <- renderUI({
    # You can replace this with the actual description of your tool
    description_text <- "
      <h3>2D NMR Spectra Analysis Tool</h3>
      <p>This tool is designed to help analyze 2D NMR spectra. It allows users to:</p>
      <ul>
        <li>Visualize 2D NMR spectra from Bruker data files</li>
        <li>Automatically detect and integrate peaks</li>
        <li>Identify and analyze centroids in the spectra</li>
        <li>Generate bounding boxes for region-of-interest (ROI) analysis</li>
        <li>Export results in CSV format</li>
      </ul>
      <p>The tool uses advanced algorithms for peak selection, centroid detection, and integration, optimized for handling complex NMR data.</p>
      <p>Instructions:</p>
      <ul>
        <li>Load your NMR dataset</li>
        <li>Generate the spectra visualization</li>
        <li>Use the available tools to manipulate and analyze the data</li>
      </ul>
      <p>For more detailed information, please refer to the documentation available on GitHub.</p>
    "
    
    # Return the HTML description
    tags$div(HTML(description_text))
  })
  
  
  ## Reactive Values ----
  
  
  result_data_list <- reactiveVal(list())
  reference_boxes <- reactiveVal()
  plot_list <- reactiveVal(list())
  bruker_data <- reactiveVal(NULL)
  imported_centroids <- reactiveVal(NULL)
  data_cc <- reactiveVal(NULL)
  calculated_contour_value <- reactiveVal(NULL)
  result_data <- reactiveVal(NULL)
  centroids_data <- reactiveVal(NULL)
  fixed_boxes <- reactiveVal(data.frame(xmin = numeric(), xmax = numeric(),
                                        ymin = numeric(), ymax = numeric()))  
  contour_plot_base <- reactiveVal(NULL)
  nmr_plot <- reactiveVal(NULL)
  status_msg <- reactiveVal("")
  centroids <- reactiveVal(NULL)
  # bounding_boxes_data <- reactiveVal(data.frame(xmin = numeric(0), xmax = numeric(0), ymin = numeric(0), ymax = numeric(0)))
  spectrum_params <- reactive({
    switch(input$spectrum_type,
           "TOCSY" = list(intensity_threshold = 50000, contour_num = 30, contour_factor = 1.3, eps_value = 0.0068),
           "HSQC"  = list(intensity_threshold = 30000,  contour_num = 40,  contour_factor = 1.3, eps_value = 0.0068),
           "COSY"  = list(intensity_threshold = 50000,  contour_num = 80,  contour_factor = 1.3, eps_value = 0.014)
    )
  })
  output$matrix_dim <- renderPrint({ req(bruker_data()); dim(bruker_data()$spectrumData) })
  output$status_message <- renderText({ status_msg() })
  
  # Affichage dans l’interface
  output$calculated_contour_text <- renderText({
    req(calculated_contour_value())
    paste0("Valeur de départ des contours calculée : ", round(calculated_contour_value(), 2))
  })
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  spectra_list <- reactiveVal(list())
  
  observeEvent(subfolders(), {
    all_spectra <- lapply(subfolders(), function(p) {
      tryCatch({
        data_path <- file.path(p, "pdata", "1")
        read_bruker(data_path, dim = "2D")
      }, error = function(e) {
        NULL
      })
    })
    names(all_spectra) <- basename(subfolders())
    spectra_list(all_spectra)
    
    # Mise à jour du choix dans le menu déroulant
    updateSelectInput(session, "selected_spectrum", choices = names(all_spectra), selected = names(all_spectra)[1])
    
    # Affiche immédiatement le premier spectre
    bruker_data(all_spectra[[1]])
  })
  
  
  bounding_boxes_data <- reactive({
    req(fixed_boxes(), bruker_data())
    
    # Récupérer la matrice de données
    mat <- bruker_data()$spectrumData
    if (is.null(mat)) {
      warning("La matrice du spectre est NULL")
      return(NULL)
    }
    
    ppm_x <- suppressWarnings(as.numeric(colnames(mat)))
    ppm_y <- suppressWarnings(as.numeric(rownames(mat)))
    
    # Vérifier les ppm
    if (any(is.na(ppm_x)) || any(is.na(ppm_y))) {
      warning("ppm_x ou ppm_y contient des NA (conversion ratée ?)")
      return(NULL)
    }
    
    boxes <- fixed_boxes()
    if (nrow(boxes) == 0) {
      return(boxes)
    }
    
    # Ajouter l'intensité
    boxes$stain_intensity <- NA_real_
    for (i in seq_len(nrow(boxes))) {
      xmin <- as.numeric(boxes$xmin[i])
      xmax <- as.numeric(boxes$xmax[i])
      ymin <- as.numeric(boxes$ymin[i])
      ymax <- as.numeric(boxes$ymax[i])
      
      x_idx <- which(ppm_x >= xmin & ppm_x <= xmax)
      y_idx <- which(ppm_y >= ymin & ppm_y <= ymax)
      
      if (length(x_idx) > 0 && length(y_idx) > 0) {
        boxes$stain_intensity[i] <- sum(mat[y_idx, x_idx], na.rm = TRUE)
      }
    }
    
    boxes
  })
  
  reference_boxes <- reactiveVal()
  reference_centroids <- reactiveVal()
  
  results_dir <- reactive({
    dir.create(file.path(main_directory(), "results_batch"), showWarnings = FALSE)
    file.path(main_directory(), "results_batch")
  })
  
  ## File Loading ----
  # Initialisation
  roots <- c(Home = normalizePath("~"), Root = "/")
  shinyDirChoose(input, "directory", roots = roots, session = session)
  
  main_directory <- reactive({
    req(input$directory)
    selected_dir <- parseDirPath(roots, input$directory)
    normalizePath(selected_dir)
  })
  
  output$selected_dir <- renderPrint({ main_directory() })
  
  # Détection des sous-dossiers valides
  subfolders <- reactive({
    req(main_directory())
    all_subfolders <- list.dirs(main_directory(), recursive = TRUE, full.names = TRUE)
    valid_subfolders <- all_subfolders[sapply(all_subfolders, function(folder) {
      file.exists(file.path(folder, "acqus")) &&
        (file.exists(file.path(folder, "ser")) || file.exists(file.path(folder, "fid")))
    })]
    valid_subfolders
  })
  
  # Conteneurs réactifs
  spectra_list <- reactiveVal(list())       # Contient les données Bruker
  spectra_plots <- reactiveVal(list())      # Contient les plots générés automatiquement
  
  # Chargement automatique + génération des plots
  observeEvent(subfolders(), {
    folders <- subfolders()
    if (length(folders) == 0) return(NULL)
    
    status_msg("🔄 Chargement et génération des spectres...")
    
    all_data <- list()
    all_plots <- list()
    
    for (sub in folders) {
      data_path <- file.path(sub, "pdata", "1")
      if (!dir.exists(data_path)) next
      
      data <- tryCatch({
        read_bruker(data_path, dim = "2D")
      }, error = function(e) {
        showNotification(paste("Erreur lecture Bruker dans", sub), type = "error")
        return(NULL)
      })
      
      if (!is.null(data)) {
        all_data[[sub]] <- data
      }
    }
    
    spectra_list(all_data)

    
    # Sélection du 1er spectre automatiquement
    updateSelectInput(session, "selected_subfolder", choices = setNames(names(all_data), basename(names(all_data))))
    if (length(all_data) > 0) {
      bruker_data(all_data[[1]])
      status_msg("✅ Spectres chargés et plots générés")
    }
  })

  # UI pour sélectionner le spectre
  output$subfolder_selector <- renderUI({
    req(spectra_list())
    # Utilisation de basename pour afficher seulement les noms des dossiers
    # Vérification si les noms sont bien un vecteur de caractères
    subfolder_names <- names(spectra_list())
    subfolder_names <- if (is.character(subfolder_names)) subfolder_names else as.character(subfolder_names)
    selectInput("selected_subfolder", "Spectre à afficher :", choices = setNames(subfolder_names, basename(subfolder_names)))
  })
  
  
  observeEvent(input$selected_subfolder, {
    req(spectra_list(), spectra_plots())
    selected <- input$selected_subfolder
    
    if (!is.null(selected) && selected %in% names(spectra_list())) {
      selected_data <- spectra_list()[[selected]]
      
      # Si des boîtes fixes ont été définies dans un spectre précédent
      if (!is.null(reference_boxes())) {
        fixed_boxes(reference_boxes())  # Appliquer les boîtes du spectre précédent
      } else {
        fixed_boxes(data.frame(xmin = numeric(), xmax = numeric(), ymin = numeric(), ymax = numeric()))  # Aucune boîte
      }
      
      # Générer et afficher le spectre
      selected_plot <- spectra_plots()[[selected]]
      contour_plot_base(selected_plot)
      bounding_boxes_data()  # force recalcul lors du changement de spectre
      refresh_nmr_plot()  # Mettre à jour le plot avec les boîtes réutilisées
      
      status_msg(paste0("🔄 Spectre sélectionné : ", selected))
    }
  })
  
  
  ## Refresh ----
  
  refresh_nmr_plot <- function() {
    req(contour_plot_base(), bruker_data())
    
    # ⚠️ Forcer l’évaluation de la réactive ici pour mettre à jour les intensités
    boxes <- bounding_boxes_data()
    
    plot <- contour_plot_base()
    
    if (!is.null(boxes) && nrow(boxes) > 0) {
      plot <- plot +
        geom_rect(data = boxes, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  color = "red", fill = NA, linetype = "dashed", inherit.aes = FALSE)
    }
    
    # Ajout des centroïdes
    centroids <- NULL
    if (!is.null(imported_centroids()) && nrow(imported_centroids()) > 0) {
      centroids <- imported_centroids()
    } else if (!is.null(centroids_data()) && nrow(centroids_data()) > 0) {
      centroids <- centroids_data()
    }
    
    if (!is.null(centroids)) {
      plot <- plot +
        geom_point(data = centroids, aes(x = F2_ppm, y = F1_ppm, color = as.numeric(stain_intensity)),
                   size = 0.5, inherit.aes = FALSE) +
        scale_color_gradient(low = "blue", high = "green")
    }
    
    nmr_plot(plot)
  }
  
  
  
  output$main_plot <- renderPlot({
    req(nmr_plot())  # Le graphe mis à jour avec centroïdes et BB
    nmr_plot()
  })
  
  
  ## Calcul Threshold ----
  
  
  observeEvent(input$calculate_contour, {
    req(bruker_data())
    data_cc <- as.data.frame(bruker_data()$spectrumData)
    
    # Choix du quantile selon le type de spectre
    selected_quantile <- switch(input$spectrum_type,
                                "TOCSY" = 0.9995,
                                "HSQC" = 0.998,
                                "COSY" = 0.998,
                                0.999)  # par défaut
    
    # Calcul
    inten <- quantile(data_cc, selected_quantile, na.rm = TRUE)
    calculated_contour_value(inten)
    
    showNotification(
      paste0("✅ Seuil de départ des contours calculé : ", round(inten, 2)),
      type = "message"
    )
  })
  
  
  
  ## Generate Plot ----
  
  observeEvent(input$generate_plot, {
    req(spectra_list())
    status_msg("🔄 Génération des graphiques...")
    params <- spectrum_params()
    
    all_results <- lapply(spectra_list(), function(data) {
      tryCatch({
        find_nmr_peak_centroids(
          data$spectrumData,
          spectrum_type = input$spectrum_type,
          intensity_threshold = input$intensity_threshold,
          contour_start = input$contour_start %||% calculated_contour_value(),
          contour_num = params$contour_num,
          contour_factor = params$contour_factor,
          f2_exclude_range = c(4.7, 5.0)
        )
      }, error = function(e) {
        showNotification(paste("Erreur dans le traitement d'un spectre :", e$message), type = "error")
        return(NULL)
      })
    })
    
    # Séparation des plots et des données de résultat
    all_plots <- lapply(all_results, function(res) if (!is.null(res)) res$plot else NULL)
    
    spectra_plots(all_plots)
    
    if (length(all_results) > 0 && !is.null(all_results[[1]])) {

      # Donne des noms aux résultats pour les retrouver plus tard
      names(all_results) <- names(spectra_list())
      
      # Stocke tous les résultats
      result_data_list(all_results)      
      
      # Stocke le premier résultat dans result_data()
      result_data(all_results[[1]])
      
      contour_plot_base(all_results[[1]]$plot + labs(title = ""))
      refresh_nmr_plot()
      showNotification("✅ Graphiques générés avec succès", type = "message")
      status_msg("✅ Analyse terminée")
    }
    
    shinyjs::hide("loading_message")
  })
  
  ## Generate Centroids & Bb ----
  
  observeEvent(input$generate_centroids, {
  
  req(input$selected_subfolder)
  req(result_data_list())
  req(bruker_data())
  params <- spectrum_params()
  
  # Récupérer le bon résultat basé sur le spectre sélectionné
  all_results <- result_data_list()
  selected_result <- all_results[[input$selected_subfolder]]

  
  if (is.null(selected_result)) {
    showNotification(paste("⚠️ Aucun résultat pour", input$selected_subfolder), type = "error")
    print("❌ selected_result est NULL")
    return()
  }

  shinyjs::show("loading_message")
  status_msg("🔄 Génération des centroïdes et BB...")

  # Récupération du spectre associé
  selected_spectrum <- bruker_data()$spectrumData
  if (is.null(selected_spectrum)) {
    showNotification(paste("⚠️ Spectre introuvable pour", input$selected_subfolder), type = "error")
    print("❌ selected_spectrum est NULL")
    shinyjs::hide("loading_message")
    return()
  }
  
  result1 <- tryCatch({
    process_nmr_centroids(
      rr_data = selected_spectrum,
      contour_data = selected_result$contour_data,
      intensity_threshold = input$intensity_threshold,
      contour_num = params$contour_num,
      contour_factor = params$contour_factor,
      eps_value = input$eps_value,
      keep_peak_ranges = list(c(0.5, -0.5), c(1, 0.8), c(1.55,1.45))
    )
  }, error = function(e) {
    showNotification(paste("❌ Erreur de traitement :", e$message), type = "error")
    print(paste("Erreur capturée :", e$message))
    shinyjs::hide("loading_message")
    return(NULL)
  })

  if (!is.null(result1)) {
    centroids_data(result1$centroids)

    box_coords_only <- result1$bounding_boxes[, c("xmin", "xmax", "ymin", "ymax", "stain_id")]
    fixed_boxes(box_coords_only)
    reference_boxes(fixed_boxes())
    
    # Mise à jour du graphe affiché
    contour_plot_base(selected_result$plot + labs(title =""))
    refresh_nmr_plot()

    showNotification("✅ Centroïdes et BBs calculés", type = "message")
    status_msg("✅ Analyse terminée")
  } else {
    print("⚠️ Aucun résultat à afficher après process_nmr_centroids.")
  }

  shinyjs::hide("loading_message")
})

  
  
  
  ## Manually Add Centroids / BBs ----
  
  observeEvent(input$add_manual_centroid, {
    req(input$manual_f2, input$manual_f1)
    current <- centroids_data()
    if (is.null(current)) current <- data.frame(F2_ppm = numeric(0), F1_ppm = numeric(0), stain_intensity = numeric(0), stain_id = character(0))
    
    existing_ids <- current$stain_id[grepl("^man", current$stain_id)]
    man_number <- if (length(existing_ids) == 0) 1 else max(as.integer(sub("man", "", existing_ids)), na.rm = TRUE) + 1
    
    # === Estimate intensity from contour_data
    contour_data <- result_data()$contour_data
    eps <- input$eps_value/2 # or any value you used for clustering
    
    local_points <- contour_data %>%
      dplyr::filter(
        abs(-x - input$manual_f2) <= eps,
        abs(-y - input$manual_f1) <= eps
      )
    
    print(paste("Manual F2:", input$manual_f2, "Manual F1:", input$manual_f1))
    
    print(head(
      contour_data %>%
        dplyr::filter(
          abs(x - input$manual_f2) <= eps,
          abs(y - input$manual_f1) <= eps
        )
    ))
    
    estimated_intensity <- sum(local_points$level, na.rm = TRUE)
    
    new_point <- data.frame(
      F2_ppm = input$manual_f2,
      F1_ppm = input$manual_f1,
      stain_intensity = estimated_intensity,
      stain_id = paste0("man", man_number)
    )
    
    centroids_data(rbind(current, new_point))
    refresh_nmr_plot()
    showNotification(paste("Centroïde ajouté :", new_point$stain_id, "- Intensité =", round(estimated_intensity)), type = "message")
  })
  
  
  
  observeEvent(input$add_manual_bbox, {
    req(input$manual_xmin, input$manual_xmax, input$manual_ymin, input$manual_ymax)
    
    new_box <- data.frame(
      xmin = input$manual_xmin,
      xmax = input$manual_xmax,
      ymin = input$manual_ymin,
      ymax = input$manual_ymax
    )
    
    current_boxes <- bounding_boxes_data()
    
    # Vérifie que le dataframe a les bonnes colonnes
    if (nrow(current_boxes) > 0 && !all(c("xmin", "xmax", "ymin", "ymax") %in% names(current_boxes))) {
      showNotification("Erreur : le format des boîtes existantes est incorrect.", type = "error")
      return()
    }
    
    updated_boxes <- bind_rows(current_boxes, new_box)
    bounding_boxes_data(updated_boxes)
    
    refresh_nmr_plot()
    showNotification("🟦 Boîte ajoutée manuellement", type = "message")
  })
  
  
  ## Manually Delete centroids / BBs ----
  
  observeEvent(input$delete_centroid, {
    selected <- input$centroid_table_rows_selected
    if (length(selected) > 0) {
      current <- centroids_data()
      centroids_data(current[-selected, ])
      refresh_nmr_plot()
      showNotification("Centroïde supprimé 🗑️", type = "message")
    } else {
      showNotification("Veuillez sélectionner un centroïde à supprimer", type = "warning")
    }
  })
  
  
  observeEvent(input$delete_bbox, {
    selected <- input$bbox_table_rows_selected
    if (length(selected) > 0) {
      current <- bounding_boxes_data()
      bounding_boxes_data(current[-selected, ])
      refresh_nmr_plot()
      showNotification("Boîte englobante supprimée 🗑️", type = "message")
    } else {
      showNotification("Veuillez sélectionner une boîte englobante à supprimer", type = "warning")
    }
  })

  
  ## Dataframes ----
  
  # Dans le server
  output$centroid_table <- renderDT({
    centroids_filtered <- centroids_data()[, 1:4]  # Afficher seulement les 3 premières colonnes
    datatable(centroids_filtered, selection = "single", options = list(pageLength = 5))
  })
  
  output$full_centroid_table <- renderDT({
    # Afficher toutes les colonnes
    datatable(centroids_data(), selection = "single", options = list(pageLength = 5))
  })
  
  output$bbox_table <- renderDT({
    datatable(bounding_boxes_data(), selection = "single", options = list(pageLength = 5))
  })
  
  ## Interactive Plot ----
  
  output$interactivePlot <- renderPlotly({
    plot_obj <- nmr_plot()
    if (is.null(plot_obj)) {
      # Affiche un message vide ou un plot vide
      ggplotly(ggplot() + theme_void() + ggtitle("Aucun spectre affiché"))
    } else {
      ggplotly(plot_obj, source = "nmr_plot")
    }
  })
  
  
  observeEvent(input$spectrum_type, {
    params <- switch(input$spectrum_type,
                     "TOCSY" = list(intensity_threshold = 30000, contour_start = 100000, contour_num = 30, contour_factor = 1.3),
                     "HSQC"  = list(intensity_threshold = 5000,  contour_start = 20000,  contour_num = 30,  contour_factor = 1.3),
                     "COSY"  = list(intensity_threshold = 10000, contour_start = 50000,  contour_num = 30,  contour_factor = 1.3)
    )
    
    updateNumericInput(session, "contour_start", value = params$contour_start)
    
    # Si plus tard tu veux que les autres valeurs soient modifiables aussi :
    updateNumericInput(session, "intensity_threshold", value = params$intensity_threshold)
    # updateNumericInput(session, "contour_num", value = params$contour_num)
    # updateNumericInput(session, "contour_factor", value = params$contour_factor)
  })

  
  ## Export Centroids & BB ----
  
  observeEvent(input$export_centroids, {
    shinyjs::show("loading_message")  # Show loading message during export
    
    req(main_directory())
    
    print(main_directory())
    
    export_path <- file.path(main_directory(), "exported_centroids.csv")
    
    tryCatch({
      if (nrow(centroids_data()) > 0) {
        write.csv(centroids_data(), export_path, row.names = FALSE)
        showNotification(paste("Centroïdes exportés dans", export_path), type = "message")
      } else {
        showNotification("Aucun centroïde à exporter", type = "warning")
      }
    }, error = function(e) {
      showNotification(paste("Erreur lors de l'exportation:", e$message), type = "error")
    })
    
    shinyjs::hide("loading_message")  # Hide loading message after export
  })
  
  
  output$export_boxes <- downloadHandler(
    filename = function() {
      paste0("bounding_boxes_", Sys.Date(), ".csv")
    },
    content = function(file) {
      boxes <- bounding_boxes_data()
      if (!is.null(boxes) && nrow(boxes) > 0) {
        write.csv(boxes, file, row.names = FALSE)
      } else {
        write.csv(data.frame(xmin = NA, xmax = NA, ymin = NA, ymax = NA), file, row.names = FALSE)
      }
    }
  )
  
  
  ## Import centroid list ----
  
  observeEvent(input$import_centroids_file, {
    req(input$import_centroids_file)
    
    imported <- tryCatch({
      read.csv(input$import_centroids_file$datapath, sep = ";")
    }, error = function(e) {
      showNotification(paste("Erreur d'importation :", e$message), type = "error")
      return(NULL)
    })
    
    if (!is.null(imported)) {
      # Vérifie qu'on a bien les colonnes attendues
      if (all(c("stain_id", "stain_intensity","F2_ppm", "F1_ppm") %in% colnames(imported))) {
        imported_centroids(imported)
        refresh_nmr_plot()
        showNotification("✅ Centroïdes importés et ajoutés au graphique", type = "message")
      } else {
        showNotification("❌ Le fichier doit contenir les colonnes 'F2_ppm' et 'F1_ppm'", type = "error")
      }
    }
  })
  
  
  ## Centroid  ----
  
  # Initialisation après chargement du premier spectre
  observeEvent(spectra_list(), {
    centroids(NULL) # reset
  })
  
  # Mise à jour si ajout manuel par l'utilisateur
  observeEvent(input$add_centroid, {
    current <- centroids()
    new_row <- data.frame( # Adapté selon ta structure
      stain_id = paste0("man", nrow(current) + 1),
      F2_ppm = input$manual_F2_ppm,
      F1_ppm = input$manual_F1_ppm,
      stain_intensity = 0  # ou une estimation
    )
    centroids(rbind(current, new_row))
  })
  
  
  output$download_centroids <- downloadHandler(
    filename = function() {
      paste0("centroids_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(centroids(), file, row.names = FALSE)
    }
  )
  
  ## Batch ( Pas du tout fini ) ----

  observeEvent(input$run_batch, {
    req(save_directory())
    
    if (is.null(bounding_boxes_data()  ) || is.null(centroids_data()) || is.null(spectra_list())) {
      showNotification("❌ Données manquantes pour le traitement batch.", type = "error")
      return(NULL)
    }    
    
    box_ref <- bounding_boxes_data()  
    print("box_ref:")
    print(box_ref)
    centroids_ref <- centroids_data()

    spectra <- spectra_list()
    n <- length(spectra)
    
    if (n == 0) {
      showNotification("❌ Aucun spectre chargé.", type = "error")
      return(NULL)
    }
    
    dir.create("results_batch", showWarnings = FALSE)
    
    withProgress(message = "Traitement batch en cours...", value = 0, {
      lapply(seq_along(spectra), function(i) {
        incProgress(1 / n, detail = paste("Spectre", names(spectra)[i]))
        spectre_name <- names(spectra)[i]
        data <- spectra[[i]]
        mat <- data$spectrumData
        ppm_x <- as.numeric(colnames(mat))
        # print("ppm_x :")
        # print(ppm_x)
        ppm_y <- as.numeric(rownames(mat))
        # print("ppm_y :")
        # print(ppm_y)
        
        boxes <- box_ref
        boxes$stain_intensity <- apply(boxes, 1, function(box) {
          x_idx <- which(ppm_y <= box["xmax"] & ppm_y >= box["xmin"])
          print("ppm_x_idx :")
          print(x_idx)
          y_idx <- which(ppm_x <= box["ymax"] & ppm_x >= box["ymin"])
          # print("ppm_y_idx :")
          # print(y_idx)
          sum(mat[x_idx, y_idx], na.rm = TRUE)
        })
        
        # Ajouter un identifiant de boîte si nécessaire
        if (is.null(boxes$box_id)) {
          boxes$box_id <- paste0("box", seq_len(nrow(boxes)))
        }
        
        # Réorganiser les colonnes pour le CSV
        result_data <- boxes[, c("box_id", "xmin", "xmax", "ymin", "ymax", "stain_intensity")]
        
        
        write.csv(result_data, file.path(save_directory(), paste0(basename(spectre_name), "_results.csv")), row.names = FALSE)
        
      })
    })
    
    showNotification("✅ Traitement batch terminé. Résultats sauvegardés dans le dossier 'results_batch/'.", type = "message")
  })
  
  ## Test ----
  
  
  project_centroids_with_intensity <- function(reference_centroids, result_list, output_dir = "centroid_exports", eps = input$eps_value/2) {
    if (!dir.exists(output_dir)) dir.create(output_dir)
    
    for (name in names(result_list)) {
      result <- result_list[[name]]
      if (is.null(result$contour_data)) next
      
      contour_data <- result$contour_data
      
      projected_centroids <- reference_centroids %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          stain_intensity = {
            local_points <- contour_data %>%
              dplyr::filter(
                abs(-x - F2_ppm) <= eps,
                abs(-y - F1_ppm) <= eps
              )
            sum(local_points$level, na.rm = TRUE)
          }
        ) %>%
        dplyr::ungroup()
      
      # Export en CSV
      # Extraire le nom de base (dernier segment du chemin)
      subfolder_name <- basename(name)
      output_file <- file.path(output_dir, paste0(subfolder_name, "_projected_centroids.csv"))
      readr::write_csv(projected_centroids, output_file)
    }
    
    showNotification("✅ Centroïdes projetés et exportés avec intensité.", type = "message")
  }
  
  
  observeEvent(input$export_projected_centroids, {
    req(centroids_data())  # vos centroïdes de référence
    req(result_data_list())
    
    project_centroids_with_intensity(
      reference_centroids = centroids_data(),
      result_list = result_data_list(),
      output_dir = "centroid_exports",  # ou n'importe quel dossier
      eps = input$eps_value %||% 0.04
    )
  })
  
  
  # Allow user to select output directory
  save_roots <- c(Home = normalizePath("~"), Root = "/")
  shinyDirChoose(input, "save_directory", roots = save_roots, session = session)
  
  save_directory <- reactive({
    req(input$save_directory)
    parseDirPath(save_roots, input$save_directory)
  })
  
  output$save_dir_display <- renderPrint({
    save_directory()
  })
  
  
  ## Reset all ----
  
  observeEvent(input$reset_all, {
    nmr_plot(NULL)
    contour_plot_base(NULL)
    imported_centroids(NULL)
    centroids_data(NULL)
    fixed_boxes(NULL)
    status_msg("🔁 Interface réinitialisée.")
  })
  
  
  
}

shinyApp(ui = ui, server = server)
