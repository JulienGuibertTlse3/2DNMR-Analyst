library(shiny)
library(shinyFiles)
library(plotly)
library(ggplot2)
library(DT)
library(shinycssloaders)
library(shinydashboard)
library(shinyBS) 
library(shinyjs)
library(dplyr)
library(sp)

source("C://Users//juguibert//Documents//Function_test//Read_2DNMR_spectrum.R")
source("C://Users//juguibert//Documents//Function_test//Vizualisation.R")
source("C://Users//juguibert//Documents//Function_test//Integration.R")
source("C://Users//juguibert//Documents//Function_test//Pping.R")
source("C://Users//juguibert//Documents//Function_test//CNN_shiny.R")
Rcpp::sourceCpp("C://Users//juguibert//Documents//Function_test//petit_test.cpp")


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
  
  /* Réduction de la taille de la police pour les éléments de l'interface */
body, label, input, button, select, .form-control, .box, .tabBox, .shiny-input-container {
  font-size: 13px !important;
}

/* Titres h4 et h5 un peu plus petits */
h4 {
  font-size: 13px !important;
}

h5 {
  font-size: 13px !important;
}

/* Réduction taille police globale dans les boxes shinydashboardPlus::box */
.box {
  font-size: 13px !important;
}

/* Pour réduire aussi la taille des titres dans ces boxes (ex: les titres de box) */
.box .box-title {
  font-size: 13px !important;
  font-weight: 500;
}

.nav-tabs-custom > .box-header > .box-title {
      font-size: 20px !important;
      font-weight: bold;
}
    
    #apply_changes, #discard_changes {
  pointer-events: auto !important;
  opacity: 1 !important;
}
        
                    "))
    
    
  ),
  
  ## DashBoard ----
  
  dashboardPage(
    dashboardHeader(title = "NMR 2D Spectra Analysis"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("README", tabName = "Readme", icon = icon("book")),
        menuItem("Visualization", tabName = "Visualization", icon = icon("chart-area"))
      )
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "Readme",
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
        
        tabItem(tabName = "Visualization",
                fluidRow(
                  column(3,
                         tabBox(
                           width = 12,
                           title = "Parameters",
                           id = "param_tabs",
                           
                           # First panel : Loading file and if needed reset interface
                           
                           tabPanel("📂 Load",
                                    div(tags$h4("Browse Data Folder"), style = "font-size: 18px; font-weight: 700;"),
                                    shinyDirButton("directory", "Select Main Directory", "Select Directory"),
                                    verbatimTextOutput("selected_dir"),
                                    # withSpinner(verbatimTextOutput("matrix_dim"), type = 4, color = "#007bff"),
                                    
                                    actionButton("reset_all", "🔁 Reset interface", icon = icon("redo"))),
                           
                           
                           # Second Panel : Used to plot and pick peaks
                           
                           tabPanel("📈 Plot",   
                                    
                                    uiOutput("subfolder_selector"),
                                    textOutput("status_message"),
                                    br(),
                                    div(
                                      style = "background-color: #eaf4fc; padding: 10px; border-left: 5px solid #007bff; margin-bottom: 10px;",
                                      tags$strong("💡 Tip:"),
                                      " For optimal batch processing, start by tuning the parameters on a QC sample or the most intense spectrum."
                                    ),
                                    
                                    selectInput("spectrum_type", "Spectrum type:",
                                                choices = c("TOCSY", "HSQC", "COSY", "UFCOSY"),
                                                selected = "TOCSY"),
                                    
                                    shinydashboardPlus::box(
                                      title = "Threshold Parameters",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      collapsed = TRUE,  # facultatif : pour que la boîte soit repliée par défaut
                                      width = 12,
                                      
                                      selectInput("seuil_method", "Thresholding method:", 
                                                  choices = c("Percentage of max" = "max_pct", 
                                                              "Noise multiplier" = "bruit_mult")),
                                      
                                      conditionalPanel(
                                        condition = "input.seuil_method == 'max_pct'",
                                        numericInput("pct_val", "Percentage of max:", value = 0.01, min = 0.001, max = 1, step = 0.001)
                                      ),
                                      
                                      conditionalPanel(
                                        condition = "input.seuil_method == 'bruit_mult'",
                                        numericInput("bruit_mult", "Noise multiplier:", value = 3, min = 0.5, max = 10, step = 0.5)
                                      ),
                                      
                                      actionButton("calculate_contour", "Compute threshold"),
                                      
                                      br(),
                                      strong("Computed threshold:"),
                                      verbatimTextOutput("seuil_text"),
                                      br()
                                    ),
                                    
                                    # User-editable value
                                    numericInput("contour_start", "Intensity value:", value = NULL, min = 0, step = 100),
                                    
                                    actionButton("generate_plot", "📊 Generate Plot"),
                                    br(),
                                    br(),
                                    shinydashboardPlus::box(
                                      title = "Peak picking",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      collapsed = TRUE,  # facultatif : pour que la boîte soit repliée par défaut
                                      width = 12,
                                      fluidRow(
                                        column(
                                          width = 2,
                                          actionButton("generate_centroids", "🔴 Max method"),
                                          br(),  # saut de ligne
                                          actionButton("generate_centroids_cnn", "🟢 CNN method")
                                        )
                                      ),
                                      checkboxInput("disable_clustering", "🔍 No clustering", value = FALSE),
                                      numericInput("eps_value", "Clustering epsilon:", value = 0.01, min = 0, step = 0.001),
                                      textAreaInput("keep_peak_ranges_text", 
                                                    "Plages de pics à conserver (ex: 0.5,-0.5; 1,0.8; 3.27,3.26)", 
                                                    value = "0.5,-0.5; 1,0.8; 1.55,1.45; 3.397,3.38; 3.27,3.26; 1.28,1.26; 5.367,5.353; 4.47,4.45; 4.385,4.375",
                                                    rows = 5)
                                    ),
                                    
                                    div(
                                      style = "background-color: #eaf4fc; padding: 10px; border-left: 5px solid #007bff; margin-bottom: 10px;",
                                      tags$strong("💡 Tip:"),
                                      " Using clustering will help detect multiplets as such."
                                    ),
                                    
                                    ),
                           
                           
                           
                           # Third panel : Used to manually modify automated peak picking result
                           
                           tabPanel("✋ Manual Editing",
                                    
                                    hidden(div(
                                      id = "centroid_section",
                                      tags$h4("➕ Manually add a Peak"),
                                      numericInput("manual_f2", "F2 ppm (x):", value = 4.0, step = 0.01),
                                      numericInput("manual_f1", "F1 ppm (y):", value = 3.5, step = 0.01),
                                      actionButton("add_manual_centroid", "Add peak 🔵")
                                    )),
                                    hidden(div(
                                      id = "delete_centroid_section",
                                      tags$h4("❌ Remove a Peak"),
                                      actionButton("delete_centroid", "Delete selected peak 🗑️")
                                    )),
                                    
                                    br(),
                                    br(),
                           
                                    hidden(div(
                                      id = "box_section",
                                      tags$h4("➕ Manually add a bounding box"),
                                      numericInput("manual_xmin", "xmin (F2 ppm):", value = 3.5, step = 0.01),
                                      numericInput("manual_xmax", "xmax (F2 ppm):", value = 4.0, step = 0.01),
                                      numericInput("manual_ymin", "ymin (F1 ppm):", value = 2.0, step = 0.01),
                                      numericInput("manual_ymax", "ymax (F1 ppm):", value = 3.0, step = 0.01),
                                      actionButton("add_manual_bbox", "Add box 🟦")
                                    )),
                                    hidden(div(
                                      id = "delete_box_section",
                                      tags$h4("❌ Remove a bounding box"),
                                      actionButton("delete_bbox", "Delete selected box 🗑️")
                                    )),
                                    
                                    br(),
                                    br(),
                                    
                                    # ✅ Bouton toujours visible pour fusion
                                    tags$h4("🔗 Fusion peaks & boxes"),
                                    actionButton("fuse_btn", "Fusionner sélection ✅", class = "btn btn-success"),                           
                                    br(),
                                    br(),
                                    actionButton("apply_changes", "✅ Appliquer"),
                                    actionButton("discard_changes", "❌ Annuler"),
                                    br(),
                                    br(),
                                    actionButton("discard_selected_centroid", "Discard selected centroid"),
                                    actionButton("discard_selected_box", "Discard selected box"),
                                    actionButton("discard_selected_fusion", "Discard selected fusion")
                                    
                                    ),
                           
                           
                           # Fifth panel : Used to manually import or export result
                           
                           tabPanel("🔁 Import/Export",
                                    verbatimTextOutput("centroids_output"),
                                    tags$h4("📥 Import peaks"),
                                    fileInput("import_centroids_file", "Import CSV file:", accept = ".csv"),
                                    downloadButton("export_centroids", "Export peaks"),
                                    br(),
                                    br(),
                                    downloadButton("export_boxes", "Export bounding boxes"),
                                    br(),
                                    br(),
                                    downloadButton("download_projected_centroids", "Export Projected Peaks"),
                                    downloadButton("export_batch_box_intensities", "Export Projected Boxes")
                                    
                           )
                         )
                  ),
                  
                  column(9,
                         tabBox(
                           width = 12,
                           id = "main_plot_tabs",
                           
                           # Onglet 1 : Spectre interactif
                           tabPanel("📊 Spectrum",
                                    div(id = "loading_message", 
                                        "Generating plot, please wait...", 
                                        style = "font-size: 18px; color: blue; font-weight: bold; display: none;"),
                                    div(id = "export_loading_message", 
                                        "Exporting centroids, please wait...", 
                                        style = "font-size: 18px; color: blue; font-weight: bold; display: none;"),
                                    
                                    withSpinner(
                                      div(id = "interactivePlot", 
                                          plotlyOutput("interactivePlot", height = "600px", width = "100%"))
                                    ),
                                    verbatimTextOutput("clickedCoords"),
                                    
                                    h3("🔄 Modifications en attente"),
                                    h4("🔴 Pending Peaks"),
                                    DTOutput("pending_centroids_table"),
                                    h4("🟦 Pending Bounding Boxes"),
                                    DTOutput("pending_boxes_table"),
                                    h4("🔗 Pending Fusions"),
                                    DTOutput("pending_fusions_table"),
                                    
                                    
                                    
                           ),
                           
                           # Onglet 2 : Tableaux de données
                           tabPanel("🧾 Associated Data",
                                    tabBox(
                                      width = 12,
                                      id = "data_tabs",
                                      tabPanel("🔴 Peaks", 
                                               tags$h4("Table of detected or manually added peaks"),
                                               DTOutput("centroid_table")
                                      ),
                                      tabPanel("🟦 Bounding boxes", 
                                               tags$h4("Table of bounding boxes"),
                                               DTOutput("bbox_table")
                                      )
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

  options(future.globals.maxSize = 10000 * 1024^2)  # allow up to 10 GB
  
  rv <- reactiveValues(
    start_time = NULL,
    progress_msg = ""
  )
  
  
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

  
  # Optional: enable future/promises if available for non-blocking heavy work
  if (requireNamespace("future", quietly = TRUE) && requireNamespace("promises", quietly = TRUE)) {
    # Only set plan here if not already set elsewhere (global.R)
    # future::plan(future::multisession) # uncomment if you want parallel workers
    future_available <- TRUE
  } else {
    future_available <- FALSE
  }
  
  ## Helper caches and functions (performance-focused) ----
  spectrum_cache <- new.env(parent = emptyenv())   # in-memory cache for read_bruker
  contour_cache <- reactiveVal(list())             # cache for contour/plot results
  
  read_bruker_cached <- function(path, dim = "2D") {
    key <- normalizePath(path, mustWork = FALSE)
    if (exists(key, envir = spectrum_cache, inherits = FALSE)) {
      return(get(key, envir = spectrum_cache, inherits = FALSE))
    }
    # read_bruker expected to be defined elsewhere
    data <- read_bruker(path, dim = dim)
    assign(key, data, envir = spectrum_cache)
    data
  }
  
  # Faster (reasonable) box intensity calculation: uses vectorized apply
  get_box_intensity <- function(mat, ppm_x, ppm_y, boxes) {
    if (nrow(boxes) == 0) return(numeric(0))
    # convert names to numeric to be robust
    xmin_v <- as.numeric(boxes$xmin)
    xmax_v <- as.numeric(boxes$xmax)
    ymin_v <- as.numeric(boxes$ymin)
    ymax_v <- as.numeric(boxes$ymax)
    
    # Precompute: avoid repeated which by using findInterval is not straightforward,
    # so use sapply which is still faster than an R-level for loop for typical sizes.
    res <- vapply(seq_along(xmin_v), FUN.VALUE = 0.0, FUN = function(i) {
      xi <- which(ppm_x >= xmin_v[i] & ppm_x <= xmax_v[i])
      yi <- which(ppm_y >= ymin_v[i] & ppm_y <= ymax_v[i])
      if (length(xi) == 0 || length(yi) == 0) return(NA_real_)
      # matrix subset sum
      sum(mat[yi, xi], na.rm = TRUE)
    })
    res
  }
  
  # generate or reuse expensive contour/peak result (find_nmr_peak_centroids) ----
  generate_or_reuse_contour <- function(name, spectrum_matrix, params_list) {
    cache <- contour_cache()
    if (!is.null(cache[[name]])) return(cache[[name]])
    # compute synchronously here (caller may run in a future)
    res <- find_nmr_peak_centroids(
      spectrum_matrix,
      spectrum_type = params_list$spectrum_type %||% input$spectrum_type,
      intensity_threshold = params_list$intensity_threshold %||% (input$contour_start %||% calculated_contour_value()),
      contour_start = params_list$contour_start %||% calculated_contour_value(),
      contour_num = params_list$contour_num,
      contour_factor = params_list$contour_factor,
      f2_exclude_range = params_list$f2_exclude_range %||% c(4.7, 5.0)
    )
    cache[[name]] <- res
    contour_cache(cache)
    res
  }
  
  # helper wrapper to run either future or synchronous ----
  run_maybe_future <- function(expr) {
    if (future_available) {
      future::future(expr)
    } else {
      # mimic future::future by returning a promise-like object using promises::promise_resolve if possible
      if (requireNamespace("promises", quietly = TRUE)) {
        promises::promise(function(resolve, reject) {
          tryCatch({
            val <- eval(expr)
            resolve(val)
          }, error = function(e) reject(e))
        })
      } else {
        # synchronous fallback
        list(value = eval(expr))
      }
    }
  }
  
  ## Reactive values (kept names from your original app) ----
  
  plot_cache <- reactiveVal(list())
  last_click_coords <- reactiveVal(NULL)
  modifiable_boxes <- reactiveVal(data.frame())
  progress_bar <- reactiveVal(NULL)
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
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  spectrum_params <- reactive({
    switch(input$spectrum_type,
           "TOCSY" = list(intensity_threshold = 80000, contour_num = 20, contour_factor = 1.3, eps_value = 0.0068, neighborhood_size = 3),
           "HSQC"  = list(intensity_threshold = 30000,  contour_num = 20,  contour_factor = 1.3, eps_value = 0.002, neighborhood_size = 3),
           "COSY"  = list(intensity_threshold = 60000,  contour_num = 30,  contour_factor = 1.3, eps_value = 0.0068, neighborhood_size = 9),
           "UFCOSY"  = list(intensity_threshold = 50000,  contour_num = 70,  contour_factor = 1.3, eps_value = 0.014, neighborhood_size = 2)
    )
  })
  
  spectrum_params_CNN <- reactive({
    switch(input$spectrum_type,
           "TOCSY" = list(
             int_thres = 0.01,
             int_prop = 0.001,
             eps_value = 0.0068,
             pred_class_thres = 0.00001,
             batch_size = 64,
             step = 4
           ),
           "UFCOSY" = list(
             int_thres = 0.001,
             int_prop = 0.5,
             eps_value = 0.02,
             pred_class_thres = 0.001,
             batch_size = 64,
             step = 4
           ),
           "HSQC" = list(
             int_thres = 0.001,
             int_prop = 0.5,
             eps_value = 0.014,
             pred_class_thres = 0.001,
             batch_size = 64,
             step = 4
           ),
           stop("Type de spectre inconnu pour CNN")
    )
  })
  
  
  pending_centroids <- reactiveVal(data.frame(
    F2_ppm = numeric(0),
    F1_ppm = numeric(0),
    stain_intensity = numeric(0),
    stain_id = character(0),
    stringsAsFactors = FALSE
  ))
  
  pending_boxes <- reactiveVal(data.frame(
    xmin = numeric(0),
    xmax = numeric(0),
    ymin = numeric(0),
    ymax = numeric(0)
  ))  
  
  
  pending_fusions <- reactiveVal(data.frame(
    stain_id = character(),
    F2_ppm = numeric(),
    F1_ppm = numeric(),
    stain_intensity = numeric(),
    stringsAsFactors = FALSE
  ))
  
  output$matrix_dim <- renderPrint({ req(bruker_data()); dim(bruker_data()$spectrumData) })
  output$status_message <- renderText({ status_msg() })
  
  output$calculated_contour_text <- renderText({
    val <- calculated_contour_value()
    if (is.null(val)) return("")
    paste0("Adviced value of the threshold : ", round(val, 2))
  
  })
  
  box_intensity_cache <- reactiveVal(list())
  
  bounding_boxes_data <- reactive({
    req(modifiable_boxes(), bruker_data())
    
    boxes <- modifiable_boxes()
    if (nrow(boxes) == 0) return(boxes)
    
    # Clé de cache basée sur le spectre actuel et les boxes
    cache_key <- paste0(
      input$selected_subfolder,
      "_", digest::digest(boxes)
    )
    
    cached <- box_intensity_cache()
    if (!is.null(cached[[cache_key]])) {
      return(cached[[cache_key]])
    }
    
    # Calculer les intensités
    mat <- bruker_data()$spectrumData
    ppm_x <- suppressWarnings(as.numeric(colnames(mat)))
    ppm_y <- suppressWarnings(as.numeric(rownames(mat)))
    
    boxes$stain_intensity <- get_box_intensity(mat, ppm_x, ppm_y, boxes)
    
    # Mettre en cache
    cached[[cache_key]] <- boxes
    box_intensity_cache(cached)
    
    boxes
  })
  
  # ensure helper functions from earlier sections exist in the environment (they are defined above)
  
  ## Spectra loading & caching (uses read_bruker_cached) ----
  
  roots <- c(Home = normalizePath("~"), Root = "/")
  shinyDirChoose(input, "directory", roots = roots, session = session)
  
  main_directory <- reactive({
    req(input$directory)
    selected_dir <- parseDirPath(roots, input$directory)
    normalizePath(selected_dir)
  })
  
  output$selected_dir <- renderPrint({ main_directory() })
  
  # Detection of sub folders
  subfolders <- reactive({
    req(main_directory())
    all_subfolders <- list.dirs(main_directory(), recursive = TRUE, full.names = TRUE)
    valid_subfolders <- all_subfolders[sapply(all_subfolders, function(folder) {
      file.exists(file.path(folder, "acqus")) &&
        (file.exists(file.path(folder, "ser")) || file.exists(file.path(folder, "fid")))
    })]
    valid_subfolders
  })
  
  
  spectra_list <- reactiveVal(list())
  spectra_plots <- reactiveVal(list()) 
  
  observeEvent(subfolders(), {
    folders <- subfolders()
    if (length(folders) == 0) {
      spectra_list(list())
      return(NULL)
    }
    
    status_msg("🔄 Loading and creation of the spectrum...")
    progress <- shiny::Progress$new()
    progress$set(message = "Loading spectra", value = 0)
    on.exit(progress$close(), add = TRUE)
    
    all_data <- list()
    for (i in seq_along(folders)) {
      sub <- folders[[i]]
      data_path <- file.path(sub, "pdata", "1")
      progress$inc(1 / length(folders), detail = paste0("Processing ", basename(sub)))
      
      if (!dir.exists(data_path)) next
      
      data <- tryCatch({
        read_bruker_cached(data_path, dim = "2D")
      }, error = function(e) {
        showNotification(paste("❌ Error reading Bruker in", sub), type = "error")
        NULL
      })
      
      if (!is.null(data)) all_data[[sub]] <- data
    }
    
    spectra_list(all_data)
    updateSelectInput(session, "selected_subfolder", choices = setNames(names(all_data), basename(names(all_data))))
    if (length(all_data) > 0) {
      bruker_data(all_data[[1]])
      status_msg("✅ Spectra loaded, generating plot...")
    } else {
      status_msg("⚠️ No valid spectra found")
    }
  }, ignoreNULL = TRUE)
  
  # bounding boxes calculation optimized to vectorized helper
  bounding_boxes_data <- reactive({
    req(modifiable_boxes(), bruker_data())
    mat <- bruker_data()$spectrumData
    if (is.null(mat)) return(NULL)
    ppm_x <- suppressWarnings(as.numeric(colnames(mat)))
    ppm_y <- suppressWarnings(as.numeric(rownames(mat)))
    boxes <- modifiable_boxes()
    if (nrow(boxes) == 0) return(boxes)
    boxes$stain_intensity <- get_box_intensity(mat, ppm_x, ppm_y, boxes)
    boxes
  })
  
  ## UI: subfolder selector (unchanged) ----
  output$subfolder_selector <- renderUI({
    req(spectra_list())
    subfolder_names <- names(spectra_list())
    selectInput("selected_subfolder", "Chosen spectrum :", choices = setNames(subfolder_names, basename(subfolder_names)))
  })
  
  observeEvent(input$selected_subfolder, {
    req(spectra_list(), spectra_plots())
    selected <- input$selected_subfolder
    
    if (is.null(selected) || !selected %in% names(spectra_list())) return()
    
    # ✅ Vérifier si on a déjà ce plot en cache
    cached_plots <- plot_cache()
    cache_key <- paste0(selected, "_boxes:0_centroids:0")
    
    # Si le plot de base est en cache, utilisation ultra-rapide
    if (!is.null(cached_plots[[cache_key]])) {
      selected_data <- spectra_list()[[selected]]
      bruker_data(selected_data)
      
      selected_plot <- spectra_plots()[[selected]]
      contour_plot_base(selected_plot)
      nmr_plot(cached_plots[[cache_key]])
      
      status_msg(paste0("✅ Spectrum loaded: ", basename(selected)))
      return(invisible(NULL))
    }
    
    # Sinon, calcul normal avec progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close(), add = TRUE)
    progress$set(message = paste0("Loading spectrum: ", basename(selected)), value = 0)
    
    selected_data <- spectra_list()[[selected]]
    bruker_data(selected_data)  # ✅ Mettre à jour bruker_data AVANT
    
    progress$inc(0.3, detail = "Applying parameters...")
    
    if (!is.null(reference_boxes())) {
      fixed_boxes(reference_boxes())
    } else {
      fixed_boxes(data.frame(xmin = numeric(), xmax = numeric(), 
                             ymin = numeric(), ymax = numeric()))
    }
    
    progress$inc(0.3, detail = "Rendering plot...")
    
    selected_plot <- spectra_plots()[[selected]]
    contour_plot_base(selected_plot)
    
    # ✅ refresh_nmr_plot va maintenant utiliser le cache
    refresh_nmr_plot(force_recalc = FALSE)
    
    progress$inc(0.4, detail = "Done")
    status_msg(paste0("✅ Spectrum selected: ", basename(selected)))
  })
  
  
  ## refresh plot function (kept but minimized heavy recompute) ----
  refresh_nmr_plot <- function(force_recalc = FALSE) {
    req(contour_plot_base(), bruker_data())
    
    # Utiliser un cache basé sur l'état actuel
    cache_key <- paste0(
      input$selected_subfolder,
      "_boxes:", nrow(modifiable_boxes()),
      "_centroids:", nrow(centroids_data() %||% data.frame())
    )
    
    cached_plots <- plot_cache()
    
    # Si le plot est en cache et pas de forçage, le retourner directement
    if (!force_recalc && !is.null(cached_plots[[cache_key]])) {
      nmr_plot(cached_plots[[cache_key]])
      return(invisible(NULL))
    }
    
    # Sinon, construire le plot
    boxes <- bounding_boxes_data()
    bbox_path_df <- NULL
    
    if (!is.null(boxes) && nrow(boxes) > 0) {
      bbox_path_df <- make_bbox_outline(boxes)
    }
    
    plot_base <- contour_plot_base()
    
    if (!is.null(bbox_path_df) && nrow(bbox_path_df) > 0) {
      plot_base <- plot_base +
        geom_path(data = bbox_path_df, aes(x = x, y = y, group = group), 
                  color = "red", linewidth = 0.5)
    }
    
    # Prioritize imported centroids
    centrs <- NULL
    if (!is.null(imported_centroids()) && nrow(imported_centroids()) > 0) {
      centrs <- imported_centroids()
    } else if (!is.null(centroids_data()) && nrow(centroids_data()) > 0) {
      centrs <- centroids_data()
    }
    
    if (!is.null(centrs) && nrow(centrs) > 0) {
      plot_base <- plot_base +
        geom_point(data = centrs, 
                   aes(x = F2_ppm, y = F1_ppm, color = as.numeric(stain_intensity)),
                   size = 1.2, inherit.aes = FALSE) +
        scale_color_gradient(low = "blue", high = "red", name = "Intensity")
    }
    
    # Mettre en cache
    cached_plots[[cache_key]] <- plot_base
    plot_cache(cached_plots)
    
    nmr_plot(plot_base)
  }
  
  
  
  output$main_plot <- renderCachedPlot({
    req(nmr_plot())
    nmr_plot()
  }, cacheKeyExpr = {
    list(
      input$selected_subfolder,
      nrow(modifiable_boxes()),
      nrow(centroids_data() %||% data.frame()),
      input$spectrum_type
    )
  }, sizePolicy = sizeGrowthRatio(width = 400, height = 400, growthRate = 1.2))
  
  close_progress <- function() {
    isolate({
      if (!is.null(progress_bar())) {
        progress_bar()$close()
        progress_bar(NULL)
      }
    })
  }
  
  ## Calculate contour threshold (unchanged logic) ----
  observeEvent(input$calculate_contour, {
    req(bruker_data())
    mat <- bruker_data()$spectrumData
    seuil <- switch(input$seuil_method,
                    "max_pct" = seuil_max_pourcentage(mat, pourcentage = input$pct_val),
                    "bruit_mult" = seuil_bruit_multiplicatif(mat, facteur = input$bruit_mult),
                    { showNotification("❌ Unrecognized threshold method", type = "error"); return(NULL) }
    )
    calculated_contour_value(seuil)
    showNotification(paste0("✅ Contour threshold successfully calculated: ", round(seuil, 2)), type = "message")
  })
  
  output$seuil_text <- renderText({
    val <- calculated_contour_value()
    if (is.null(val)) return("No threshold calculated.")
    round(val, 5)
  })
  
  ## Generate plot ----
  
  observeEvent(input$generate_plot, {
    req(spectra_list())
    status_msg("🔄 Generating plots...")
    params <- spectrum_params()
    
    # Create progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    n <- length(spectra_list())
    progress$set(message = "Processing spectra", value = 0)
    
    start_time <- Sys.time()
    all_results <- list()
    
    for (i in seq_along(spectra_list())) {
      data <- spectra_list()[[i]]
      
      # Estimate time
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      avg_time <- if (i > 1) elapsed / (i - 1) else 0
      remaining <- round(avg_time * (n - i))
      
      time_msg <- if (i > 1) paste("⏱️ ~", remaining, "sec remaining") else ""
      
      progress$inc(1/n, detail = paste("Processing", names(spectra_list())[i], time_msg))
      
      result <- tryCatch({
        find_nmr_peak_centroids_optimized(
          data$spectrumData,
          spectrum_type = input$spectrum_type,
          intensity_threshold = modulate_threshold(input$contour_start) %||% modulate_threshold(calculated_contour_value()),
          contour_start = input$contour_start %||% calculated_contour_value(),
          contour_num = params$contour_num,
          contour_factor = params$contour_factor,
          f2_exclude_range = c(4.7, 5.0)
        )
      }, error = function(e) {
        showNotification(paste("❌ Error while processing", names(spectra_list())[i], ":", e$message), type = "error")
        NULL
      })
      
      all_results[[i]] <- result
    }
    
    all_plots <- lapply(all_results, function(res) if (!is.null(res)) res$plot else NULL)
    names(all_plots) <- names(spectra_list())  # <-- AJOUT ESSENTIEL
    spectra_plots(all_plots)
    
    if (length(all_results) > 0 && !is.null(all_results[[1]])) {
      names(all_results) <- names(spectra_list())
      result_data_list(all_results)
      result_data(all_results[[1]])
      contour_plot_base(all_results[[1]]$plot + labs(title = ""))
      refresh_nmr_plot()
      showNotification("✅ Plots successfully generated", type = "message")
      status_msg("✅ Analysis complete")
    }
    
    shinyjs::hide("loading_message")
  })
  
  
  
  
  ## Generate Centroids & Bounding Boxes ----
  observeEvent(input$generate_centroids, {
    
    req(input$selected_subfolder)
    req(result_data_list())
    req(bruker_data())
    params <- spectrum_params()
    
    all_results <- result_data_list()
    selected_result <- all_results[[input$selected_subfolder]]
    
    if (is.null(selected_result)) {
      showNotification(paste("⚠️ No result found for", input$selected_subfolder), type = "error")
      print("❌ selected_result is NULL")
      return()
    }
    
    shinyjs::show("loading_message")
    status_msg("🔄 Generating centroids and bounding boxes...")
    
    selected_spectrum <- bruker_data()$spectrumData
    if (is.null(selected_spectrum)) {
      showNotification(paste("⚠️ Spectrum not found for", input$selected_subfolder), type = "error")
      print("❌ selected_spectrum is NULL")
      shinyjs::hide("loading_message")
      return()
    }
    
    if (input$disable_clustering) {
      
      keep_ranges <- parse_keep_peak_ranges(input$keep_peak_ranges_text)
      
      ### 🔹 UTILISATION de `peak_pick_2d_nt2` (pas de clustering) ----
      result_peaks <- tryCatch({
        peak_pick_2d_nt2(
          bruker_data = selected_spectrum,
          threshold_value = input$contour_start,
          neighborhood_size = params$neighborhood_size,
          f2_exclude_range = c(4.7, 5.0),
          keep_peak_ranges = keep_ranges
        )
      }, error = function(e) {
        showNotification(paste("❌ peak_pick_2d_nt2 error:", e$message), type = "error")
        print(paste("Caught error in peak_pick_2d_nt2:", e$message))
        shinyjs::hide("loading_message")
        return(NULL)
      })
      
      
      
      centroids_data(result_peaks$peaks)  # <- ici on accède bien au data.frame
      box_coords_only <- result_peaks$bounding_boxes[, c("xmin", "xmax", "ymin", "ymax", "stain_id")]
      fixed_boxes(box_coords_only)
      modifiable_boxes(fixed_boxes())
      reference_boxes(fixed_boxes())
      contour_plot_base(selected_result$plot + labs(title = ""))
      
      refresh_nmr_plot()
      
      showNotification("✅ Peaks (sans clustering) générés", type = "message")
      
      status_msg("✅ Peak picking alternatif terminé")
      
      
    } else {
      
      ### 🔹 UTILISATION de `process_nmr_centroids` (clustering) ----
      
      
      keep_ranges <- parse_keep_peak_ranges(input$keep_peak_ranges_text)
      
      
      result1 <- tryCatch({
        process_nmr_centroids(
          rr_data = selected_spectrum,
          contour_data = selected_result$contour_data,
          intensity_threshold = modulate_threshold(input$contour_start) %||% modulate_threshold(calculated_contour_value()),
          contour_num = params$contour_num,
          contour_factor = params$contour_factor,
          eps_value = input$eps_value,
          keep_peak_ranges = keep_ranges,
          spectrum_type = input$spectrum_type
        )
      }, error = function(e) {
        # Gestion d’erreur
        showNotification(paste("Erreur :", e$message), type = "error")
        NULL
      })
      
      if (!is.null(result1)) {
        centroids_data(result1$centroids)
        box_coords_only <- result1$bounding_boxes[, c("xmin", "xmax", "ymin", "ymax", "stain_id")]
        fixed_boxes(box_coords_only)
        modifiable_boxes(fixed_boxes())
        reference_boxes(fixed_boxes())
        contour_plot_base(selected_result$plot + labs(title = ""))
        refresh_nmr_plot()
        showNotification("✅ Peaks and bounding boxes generated", type = "message")
        status_msg("✅ Analysis complete")
      }
    }
    
    shinyjs::hide("loading_message")
  })
  
  
  ## Generate Centroids & Bounding Boxes AVEC UN CNN ----
  
  observeEvent(input$generate_centroids_cnn, {
    
    req(bruker_data())
    req(input$selected_subfolder)
    
    shinyjs::show("loading_message")
    status_msg("🔄 Détection des pics avec CNN en cours...")
    
    # --- 1. Récupérer et normaliser le spectre ---
    selected_spectrum <- bruker_data()$spectrumData
    if (is.null(selected_spectrum)) {
      showNotification("⚠️ Spectrum not found", type = "error")
      shinyjs::hide("loading_message")
      return()
    }
    
    rr_abs <- abs(selected_spectrum)
    rr_norm <- (rr_abs - min(rr_abs)) / (max(rr_abs) - min(rr_abs))
    
    # --- 2. Lancer la détection CNN ---
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
      showNotification(paste("❌ CNN peak picking error:", e$message), type = "error")
      shinyjs::hide("loading_message")
      return(NULL)
    })
    
    if (is.null(result_peaks)) return()
    
    # --- 3. Vérifier et préparer les centres des bounding boxes ---
    if (!is.null(result_peaks$boxes) && nrow(result_peaks$boxes) > 0) {
      
      # Ajouter colonne pour compatibilité avec pipeline original
      result_peaks$peaks <- result_peaks$boxes %>%
        dplyr::transmute(
          F1 = as.integer(round(cx_ppm)),
          F2 = as.integer(round(cy_ppm)),
          F1_ppm = cx_ppm,
          F2_ppm = cy_ppm,
          stain_intensity = intensity,
          cluster_db = cluster_db
        )
      
      # Créer un identifiant pour chaque box
      result_peaks$boxes$stain_id <- seq_len(nrow(result_peaks$boxes))
      result_peaks$boxes$stain_intensity <- result_peaks$boxes$intensity
      result_peaks$boxes$xmin <- result_peaks$boxes$xmin_ppm
      result_peaks$boxes$xmax <- result_peaks$boxes$xmax_ppm
      result_peaks$boxes$ymin <- result_peaks$boxes$ymin_ppm
      result_peaks$boxes$ymax <- result_peaks$boxes$ymax_ppm
      result_peaks$boxes$cx <- result_peaks$boxes$cx_ppm
      result_peaks$boxes$cy <- result_peaks$boxes$cy_ppm
      
      # Garder seulement les coordonnées utiles pour manipulation/affichage
      result_peaks$boxes$stain_id <- seq_len(nrow(result_peaks$boxes))
      
      # Garder seulement les coordonnées utiles pour manipulation/affichage
      box_coords_only <- result_peaks$boxes[, c("xmin", "xmax", "ymin", "ymax", "stain_id")]
      fixed_boxes(box_coords_only)
      modifiable_boxes(fixed_boxes())
      reference_boxes(fixed_boxes())
    }
    
    # --- 4. Préparer les shapes Plotly ---
    if (!is.null(result_peaks$shapes) && length(result_peaks$shapes) > 0) {
      shapes_list <- result_peaks$shapes
    }
    
    # --- 5. Rafraîchir le plot avec contours et shapes ---
    contour_plot_base(
      result_data_list()[[input$selected_subfolder]]$plot + labs(title = "")
    )
    refresh_nmr_plot()
    
    showNotification("✅ Peak picking CNN terminé", type = "message")
    status_msg("✅ CNN peak picking terminé")
    
    shinyjs::hide("loading_message")
  })
  
  ## Manual centroid addition: small bugfix (local -> local_points) and faster filtering ----
  observeEvent(input$add_manual_centroid, {
    req(input$manual_f2, input$manual_f1)
    current <- centroids_data()
    if (is.null(current)) {
      current <- data.frame(F2_ppm = numeric(0), F1_ppm = numeric(0), stain_intensity = numeric(0), stain_id = character(0), stringsAsFactors = FALSE)
    }
    existing_ids <- current$stain_id[grepl("^man", current$stain_id)]
    man_number <- if (length(existing_ids) == 0) 1 else max(as.integer(sub("man", "", existing_ids)), na.rm = TRUE) + 1
    
    contour_dat <- result_data()$contour_data
    eps <- input$eps_value
    if (!is.null(contour_dat) && nrow(contour_dat) > 0) {
      local_points <- contour_dat[
        abs(contour_dat$x + input$manual_f2) <= eps*16 &
          abs(contour_dat$y + input$manual_f1) <= eps*16, , drop = FALSE]
      estimated_intensity <- sum(local_points$level, na.rm = TRUE)
    } else {
      estimated_intensity <- 0
    }
    
    new_point <- data.frame(
      F2_ppm = as.numeric(input$manual_f2),
      F1_ppm = as.numeric(input$manual_f1),
      stain_intensity = as.numeric(estimated_intensity),
      stain_id = paste0("man", man_number),
      status = "add", stringsAsFactors = FALSE
    )
    
    missing_cols <- setdiff(colnames(current), colnames(new_point))
    for (mc in missing_cols) new_point[[mc]] <- NA
    new_point <- new_point[, unique(c(colnames(current), "status")), drop = FALSE]
    
    pending_centroids(dplyr::bind_rows(pending_centroids(), new_point))
    showNotification(paste("✅ Manual peak added:", new_point$stain_id, "- Intensity =", round(estimated_intensity)), type = "message")
  })
  
  ## Remaining event handlers & exports left mostly unchanged, with small performance improvements ----
  
  observeEvent(input$add_manual_bbox, {
    req(input$manual_xmin, input$manual_xmax, input$manual_ymin, input$manual_ymax)
    new_box <- data.frame(xmin = input$manual_xmin, xmax = input$manual_xmax, ymin = input$manual_ymin, ymax = input$manual_ymax, status = "add")
    current_boxes <- modifiable_boxes()
    if (nrow(current_boxes) > 0 && !all(c("xmin", "xmax", "ymin", "ymax") %in% names(current_boxes))) {
      showNotification("❌ Format invalide des boîtes existantes.", type = "error"); return()
    }
    pending_boxes(dplyr::bind_rows(pending_boxes(), new_box))
    showNotification("🟦 Boîte ajoutée manuellement (pending).", type = "message")
  })
  
  observeEvent(input$delete_centroid, {
    selected <- input$centroid_table_rows_selected
    if (length(selected) > 0) {
      current <- centroids_data()
      to_delete <- current[selected, , drop = FALSE]
      to_delete$status <- "delete"
      pending_centroids(bind_rows(pending_centroids(), to_delete))
      centroids_data(current[-selected, , drop = FALSE])
      showNotification("🗑️ Centroid marked for deletion (pending)", type = "message")
    } else showNotification("⚠️ Please select a centroid to delete", type = "warning")
  })
  
  observeEvent(input$delete_bbox, {
    selected <- input$bbox_table_rows_selected
    if (length(selected) > 0) {
      current <- modifiable_boxes()
      to_delete <- current[selected, , drop = FALSE]
      to_delete$status <- "delete"
      pending_boxes(bind_rows(pending_boxes(), to_delete))
      modifiable_boxes(current[-selected, , drop = FALSE])
      showNotification("🗑️ Box marked for deletion (pending)", type = "message")
    } else showNotification("⚠️ Select a box to delete", type = "warning")
  })
  
  output$centroid_table <- renderDT({
    df <- centroids_data()
    if (is.null(df) || nrow(df) == 0) return(datatable(data.frame()))
    centroids_filtered <- df[, seq_len(min(4, ncol(df))), drop = FALSE]
    datatable(centroids_filtered, selection = "single", options = list(pageLength = 5))
  })
  
  output$full_centroid_table <- renderDT({
    df <- centroids_data()
    if (is.null(df)) df <- data.frame()
    datatable(df, selection = "single", options = list(pageLength = 5))
  })
  
  output$bbox_table <- renderDT({
    df <- bounding_boxes_data()
    if (is.null(df)) df <- data.frame()
    datatable(df, selection = "single", options = list(pageLength = 5))
  })
  
  output$interactivePlot <- renderPlotly({
    plot_obj <- nmr_plot()
    if (is.null(plot_obj)) {
      ggplotly(ggplot() + theme_void() + ggtitle("No spectrum displayed"))
    } else {
      plot_obj <- plot_obj + theme(legend.text = element_text(size = 8), legend.title = element_text(size = 9), legend.key.size = unit(0.4, "cm"))
      ggplotly(plot_obj, source = "nmr_plot") %>%
        layout(dragmode = "select") %>%
        config(modeBarButtonsToAdd = list("select2d", "lasso2d")) %>%
        event_register("plotly_selected") %>%
        layout(xaxis = list(showticklabels = TRUE, ticks = "outside"), yaxis = list(showticklabels = TRUE, ticks = "outside"))
    }
  })
  
  observeEvent(event_data("plotly_click", source = "nmr_plot"), {
    click_data <- event_data("plotly_click", source = "nmr_plot")
    if (!is.null(click_data) && !is.null(click_data$x) && !is.null(click_data$y) && !is.na(click_data$x) && !is.na(click_data$y)) {
      coords <- list(F2_ppm = click_data$x, F1_ppm = click_data$y)
      last_click_coords(coords)
      message("Clic détecté : x=", -click_data$x, " y=", -click_data$y)
    } else {
      message("Clic ignoré : pas de coordonnées valides.")
    }
  })
  
  output$clickedCoords <- renderPrint({
    coords <- last_click_coords()
    if (is.null(coords)) "Cliquez sur un point du spectre pour afficher les coordonnées." else paste0("F2_ppm = ", round(-coords$F2_ppm, 9), ", F1_ppm = ", round(-coords$F1_ppm, 9))
  })
  
  observeEvent(input$spectrum_type, {
    params <- switch(input$spectrum_type,
                     "TOCSY" = list(contour_start = 100000, contour_num = 30, contour_factor = 1.3, eps_value = 0.0068),
                     "HSQC"  = list(contour_start = 20000,  contour_num = 30,  contour_factor = 1.3, eps_value = 0.0068),
                     "COSY"  = list(contour_start = 80000,  contour_num = 40,  contour_factor = 1.3, eps_value = 0.0068),
                     "UFCOSY"  = list(contour_start = 30000,  contour_num = 60,  contour_factor = 1.3, eps_value = 0.014)
    )
    updateNumericInput(session, "contour_start", value = params$contour_start)
    updateNumericInput(session, "eps_value", value = params$eps_value)
  })
  
  output$export_centroids <- downloadHandler(
    filename = function() paste0("centroids_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- centroids_data()
      if (!is.null(df) && nrow(df) > 0) write.csv(df, file, row.names = FALSE) else write.csv(data.frame(), file)
    }
  )
  
  output$export_boxes <- downloadHandler(
    filename = function() paste0("combined_box_intensities_", Sys.Date(), ".csv"),
    content = function(file) {
      req(result_data_list())
      req(bounding_boxes_data())
      boxes_ref <- bounding_boxes_data() %>%
        dplyr::mutate(stain_id = dplyr::row_number(), F2_ppm = (xmin + xmax)/2, F1_ppm = (ymin + ymax)/2)
      intensity_list <- list()
      for (name in names(result_data_list())) {
        result <- result_data_list()[[name]]
        if (is.null(result$contour_data)) next
        contour_data <- result$contour_data
        # vectorized per-box compute using vapply
        Intensity <- vapply(seq_len(nrow(boxes_ref)), FUN.VALUE = 0.0, FUN = function(i) {
          rowb <- boxes_ref[i, ]
          sum(contour_data$level[
            contour_data$x >= -rowb$xmax &
              contour_data$x <= -rowb$xmin &
              contour_data$y >= -rowb$ymax &
              contour_data$y <= -rowb$ymin
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
  
  clean_centroids_df <- function(df) {
    df$F2_ppm <- as.numeric(gsub(",", ".", trimws(df$F2_ppm)))
    df$F1_ppm <- as.numeric(gsub(",", ".", trimws(df$F1_ppm)))
    df$stain_intensity <- as.numeric(gsub(",", ".", trimws(df$stain_intensity)))
    df
  }
  
  observeEvent(input$import_centroids_file, {
    req(input$import_centroids_file)
    imported <- tryCatch(read.csv(input$import_centroids_file$datapath, sep = ";", stringsAsFactors = FALSE), error = function(e) { showNotification(paste("Import error:", e$message), type = "error"); return(NULL) })
    if (!is.null(imported) && all(c("stain_id", "stain_intensity", "F2_ppm", "F1_ppm") %in% colnames(imported))) {
      imported2 <- clean_centroids_df(imported)
      centroids_data(imported2)
      refresh_nmr_plot()
      showNotification("✅ Imported centroids replaced the current set", type = "message")
    } else showNotification("❌ The file must contain columns 'stain_id','stain_intensity','F2_ppm','F1_ppm'", type = "error")
  })
  
  observeEvent(spectra_list(), { centroids(NULL) })
  
  observeEvent(input$add_centroid, {
    current <- centroids()
    nr <- if (is.null(current) || nrow(current) == 0) 0 else nrow(current)
    new_row <- data.frame(stain_id = paste0("man", nr + 1), F2_ppm = input$manual_F2_ppm, F1_ppm = input$manual_F1_ppm, stain_intensity = 0, status = "add", stringsAsFactors = FALSE)
    pending_centroids(bind_rows(pending_centroids(), new_row))
    showNotification("➕ Centroid added (pending)", type = "message")
  })
  
  observeEvent(input$fuse_btn, {
    req(centroids_data())
    sel <- event_data("plotly_selected", source = "nmr_plot")
    if (is.null(sel) || nrow(sel) < 2) { showNotification("⚠️ Need at least 2 points selected.", type = "error"); return() }
    sel$x <- -sel$x; sel$y <- -sel$y
    brushed <- dplyr::semi_join(centroids_data(), sel, by = c("F2_ppm" = "x", "F1_ppm" = "y"))
    if (nrow(brushed) < 2) { showNotification("⚠️ Selection did not match enough points.", type = "error"); return() }
    first_peak_id <- brushed$stain_id[1]
    peak_number <- gsub("[^0-9]", "", first_peak_id)
    fused_point <- data.frame(stain_id = paste0("fused_point", peak_number), F2_ppm = mean(brushed$F2_ppm), F1_ppm = mean(brushed$F1_ppm), stain_intensity = sum(as.numeric(brushed$stain_intensity), na.rm = TRUE), stringsAsFactors = FALSE)
    remaining <- dplyr::anti_join(centroids_data(), brushed, by = c("F2_ppm", "F1_ppm"))
    missing_cols <- setdiff(names(remaining), names(fused_point))
    for (mc in missing_cols) fused_point[[mc]] <- NA
    fused_point <- fused_point[, names(remaining), drop = FALSE]
    centroids_data(rbind(remaining, fused_point))
    if (!is.null(modifiable_boxes()) && nrow(modifiable_boxes()) > 0) {
      boxes <- modifiable_boxes()
      selected_boxes <- which(boxes$xmin <= max(brushed$F2_ppm) & boxes$xmax >= min(brushed$F2_ppm) & boxes$ymin <= max(brushed$F1_ppm) & boxes$ymax >= min(brushed$F1_ppm))
      removed_boxes <- if (length(selected_boxes) > 0) boxes[selected_boxes, , drop = FALSE] else boxes[0, , drop = FALSE]
      boxes <- if (length(selected_boxes) > 0) boxes[-selected_boxes, , drop = FALSE] else boxes
      if (nrow(removed_boxes) > 0) {
        new_box <- data.frame(xmin = min(removed_boxes$xmin), xmax = max(removed_boxes$xmax), ymin = min(removed_boxes$ymin), ymax = max(removed_boxes$ymax), stain_id = paste0("bbox_fused_point", peak_number))
        boxes <- rbind(boxes, new_box)
      }
      modifiable_boxes(boxes); fixed_boxes(boxes)
    }
    pending_fusions(bind_rows(pending_fusions(), fused_point))
    showNotification("✅ Points fused successfully", type = "message")
  })
  
  output$pending_centroids_table <- renderDT({ datatable(pending_centroids(), selection = "single", options = list(pageLength = 5)) })
  output$pending_boxes_table <- renderDT({ datatable(pending_boxes(), selection = "single", options = list(pageLength = 5)) })
  output$pending_fusions_table <- renderDT({ req(pending_fusions()); datatable(pending_fusions(), selection = "single", options = list(scrollX = TRUE, pageLength = 5)) })
  
  observeEvent(input$discard_selected_centroid, { selected <- input$pending_centroids_table_rows_selected; if (length(selected) > 0) { current <- pending_centroids(); pending_centroids(current[-selected, , drop = FALSE]); showNotification("🗑️ Pending centroid discarded", type = "message") } else showNotification("⚠️ Select a pending centroid to discard", type = "warning") })
  observeEvent(input$discard_selected_box, { selected <- input$pending_boxes_table_rows_selected; if (length(selected) > 0) { current <- pending_boxes(); pending_boxes(current[-selected, , drop = FALSE]); showNotification("🗑️ Pending box discarded", type = "message") } else showNotification("⚠️ Select a pending box to discard", type = "warning") })
  observeEvent(input$discard_selected_fusion, { selected <- input$pending_fusions_table_rows_selected; if (length(selected) > 0) { current <- pending_fusions(); pending_fusions(current[-selected, , drop = FALSE]); showNotification("🗑️ Pending fusion discarded", type = "message") } else showNotification("⚠️ Select a pending fusion to discard", type = "warning") })
  
  observe({ print(event_data("plotly_selected")) })
  
  ## output batch ----
  
  output$download_projected_centroids <- downloadHandler(
    filename = function() paste0("projected_centroids_", Sys.Date(), ".zip"),
    content = function(zipfile) {
      req(centroids_data(), result_data_list())
      tmp_dir <- tempdir(); csv_files <- character(0)
      eps_val <- input$eps_value %||% 0.04
      reference_centroids <- centroids_data(); volumes_list <- list()
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
        shifted_centroids <- reference_centroids %>% dplyr::mutate(F2_ppm = F2_ppm + delta_F2, F1_ppm = F1_ppm + delta_F1)
        projected_centroids <- shifted_centroids %>% dplyr::rowwise() %>% dplyr::mutate(Volume = { local_points <- contour_data %>% dplyr::filter(sqrt((-x - F2_ppm)^2 + (-y - F1_ppm)^2) <= eps_val * eps_factor); sum(local_points$level, na.rm = TRUE) }) %>% dplyr::ungroup()
        safe_name <- make.names(basename(name)); output_csv <- file.path(tmp_dir, paste0(safe_name, "_projected_centroids.csv")); readr::write_csv(projected_centroids, output_csv); csv_files <- c(csv_files, output_csv)
        vol_df <- projected_centroids %>% dplyr::select(stain_id, F2_ppm, F1_ppm, Volume) %>% dplyr::rename(!!paste0("Volume_", safe_name) := Volume)
        volumes_list[[safe_name]] <- vol_df
      }
      if (length(volumes_list) == 0) {
        err_df <- tibble::tibble(message = "Aucun contour_data trouvé dans result_data_list(); aucun CSV individuel produit.")
        summary_csv <- file.path(tmp_dir, "summary_volumes.csv"); readr::write_csv(err_df, summary_csv); csv_files <- c(csv_files, summary_csv); zip(zipfile, files = csv_files, flags = "-j"); return(invisible(NULL))
      }
      merged_data <- Reduce(function(x, y) dplyr::full_join(x, y, by = c("stain_id", "F2_ppm", "F1_ppm")), volumes_list)
      volume_cols <- grep("^Volume_", names(merged_data), value = TRUE)
      merged_data <- merged_data %>% dplyr::select(stain_id, F2_ppm, F1_ppm, dplyr::all_of(volume_cols))
      merged_data$status <- ifelse(rowSums(is.na(merged_data[volume_cols])) == 0, "ok", "missing")
      summary_csv <- file.path(tmp_dir, "summary_volumes.csv"); readr::write_csv(merged_data, summary_csv); csv_files <- c(csv_files, summary_csv)
      zip(zipfile, files = csv_files, flags = "-j")
    }
  )
  
  save_roots <- c(Home = normalizePath("~"), Root = "/")
  shinyDirChoose(input, "save_directory", roots = save_roots, session = session)
  save_directory <- reactive({ req(input$save_directory); parseDirPath(save_roots, input$save_directory) })
  output$save_dir_display <- renderPrint({ save_directory() })
  
  observeEvent(input$apply_changes, {
    centroids_data(bind_rows(centroids_data(), pending_centroids()))
    modifiable_boxes(bind_rows(modifiable_boxes(), pending_boxes()))
    pending_centroids(pending_centroids()[0, ])
    pending_boxes(pending_boxes()[0, ])
    pending_fusions(data.frame(stain_id = character(), F2_ppm = numeric(), F1_ppm = numeric(), stain_intensity = numeric()))
    refresh_nmr_plot()
    showNotification("✅ Tous les changements ont été appliqués", type = "message")
  })
  
  observeEvent(input$discard_changes, {
    pending_centroids(pending_centroids()[0, ])
    pending_boxes(pending_boxes()[0, ])
    pending_fusions(data.frame(stain_id = character(), F2_ppm = numeric(), F1_ppm = numeric(), stain_intensity = numeric()))
    showNotification("❌ Modifications en attente annulées", type = "warning")
  })
  
  observeEvent(input$reset_all, {
    nmr_plot(NULL); contour_plot_base(NULL); imported_centroids(NULL); centroids_data(NULL); fixed_boxes(NULL); reference_boxes(NULL)
    updateSelectInput(session, "selected_subfolder", selected = "")
    status_msg("🔁 Interface reset. Please select a new spectrum.")
  })
  
  output$status_msg <- renderUI({
    req(input$selected_subfolder)
    HTML(paste0("<span style='font-size:16px; font-weight:bold; color:green;'>✅ Spectrum selected: <code>", basename(input$selected_subfolder), "</code></span>"))
  })
  
  parse_keep_peak_ranges <- function(input_string) {
    if (is.null(input_string) || input_string == "") return(NULL)
    pairs <- strsplit(input_string, ";")[[1]]
    parsed <- lapply(pairs, function(p) {
      nums <- as.numeric(trimws(unlist(strsplit(p, ","))))
      if (length(nums) == 2 && all(!is.na(nums))) nums else NULL
    })
    parsed[!sapply(parsed, is.null)]
  }
  
  status_msg <- reactiveVal("")
  
  output$status_message <- renderText({
    status_msg()
  })
  
  
  ## Test ----
  
  # ===== FONCTION OPTIMISÉE POUR CALCULER LES INTENSITÉS DE BOXES SUR UN BATCH =====
  
  #' Calcule les intensités des bounding boxes de référence sur tous les spectres du batch
  #' 
  #' @param reference_boxes data.frame avec colonnes: xmin, xmax, ymin, ymax, stain_id
  #' @param spectra_list Liste nommée des spectres (résultat de read_bruker)
  #' @param apply_shift Booléen - appliquer un décalage chimique automatique ?
  #' @return data.frame avec stain_id, F2_ppm, F1_ppm, et une colonne Intensity_* par spectre
  calculate_batch_box_intensities <- function(reference_boxes, 
                                              spectra_list,
                                              apply_shift = FALSE) {
    
    if (is.null(reference_boxes) || nrow(reference_boxes) == 0) {
      stop("reference_boxes est vide ou NULL")
    }
    
    # Ajouter les coordonnées centrales si elles n'existent pas
    if (!all(c("F2_ppm", "F1_ppm") %in% names(reference_boxes))) {
      reference_boxes <- reference_boxes %>%
        dplyr::mutate(
          F2_ppm = (xmin + xmax) / 2,
          F1_ppm = (ymin + ymax) / 2
        )
    }
    
    # Initialiser le résultat avec les infos des boxes
    result_df <- reference_boxes %>%
      dplyr::select(stain_id, F2_ppm, F1_ppm, xmin, xmax, ymin, ymax)
    
    # Boucle sur chaque spectre
    for (spectrum_name in names(spectra_list)) {
      
      spectrum_data <- spectra_list[[spectrum_name]]
      
      if (is.null(spectrum_data) || is.null(spectrum_data$spectrumData)) {
        warning(paste("Spectre", spectrum_name, "est NULL ou invalide"))
        next
      }
      
      mat <- spectrum_data$spectrumData
      ppm_x <- suppressWarnings(as.numeric(colnames(mat)))  # F2
      ppm_y <- suppressWarnings(as.numeric(rownames(mat)))  # F1
      
      # ✅ DÉCALAGE CHIMIQUE OPTIONNEL (si apply_shift = TRUE)
      shift_f2 <- 0
      shift_f1 <- 0
      
      if (apply_shift) {
        # Estimation simple du décalage en trouvant le pic le plus intense
        # et en le comparant avec le premier pic de référence
        max_idx <- which(mat == max(mat, na.rm = TRUE), arr.ind = TRUE)
        
        if (length(max_idx) > 0) {
          max_f2 <- ppm_x[max_idx[2]]
          max_f1 <- ppm_y[max_idx[1]]
          
          ref_f2 <- reference_boxes$F2_ppm[1]
          ref_f1 <- reference_boxes$F1_ppm[1]
          
          shift_f2 <- max_f2 - ref_f2
          shift_f1 <- max_f1 - ref_f1
          
          # Limiter le shift à des valeurs raisonnables
          if (abs(shift_f2) > 0.5) shift_f2 <- 0
          if (abs(shift_f1) > 0.5) shift_f1 <- 0
        }
      }
      
      # Appliquer les shifts aux boxes
      shifted_boxes <- reference_boxes %>%
        dplyr::mutate(
          xmin_shifted = xmin + shift_f2,
          xmax_shifted = xmax + shift_f2,
          ymin_shifted = ymin + shift_f1,
          ymax_shifted = ymax + shift_f1
        )
      
      # ✅ CALCUL VECTORISÉ DES INTENSITÉS
      intensities <- vapply(seq_len(nrow(shifted_boxes)), FUN.VALUE = 0.0, FUN = function(i) {
        box <- shifted_boxes[i, ]
        
        # Trouver les indices correspondant à la box
        x_idx <- which(ppm_x >= box$xmin_shifted & ppm_x <= box$xmax_shifted)
        y_idx <- which(ppm_y >= box$ymin_shifted & ppm_y <= box$ymax_shifted)
        
        if (length(x_idx) == 0 || length(y_idx) == 0) {
          return(NA_real_)
        }
        
        # Somme des intensités dans la région
        sum(mat[y_idx, x_idx], na.rm = TRUE)
      })
      
      # Ajouter la colonne au résultat
      col_name <- paste0("Intensity_", make.names(basename(spectrum_name)))
      result_df[[col_name]] <- intensities
    }
    
    return(result_df)
  }
  
  
  # ===== INTÉGRATION DANS SHINY: NOUVEAU BOUTON D'EXPORT =====
  
  # Ajouter dans l'UI, dans le tabPanel "📁 Import/Export":
  # downloadButton("export_batch_box_intensities", "Export Batch Box Intensities")
  
  # Dans le server, ajouter ce handler:
  output$export_batch_box_intensities <- downloadHandler(
    filename = function() {
      paste0("batch_box_intensities_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(reference_boxes())
      req(spectra_list())
      
      # Afficher un message de progression
      shinyjs::show("export_loading_message")
      status_msg("🔄 Calculating box intensities across all spectra...")
      
      tryCatch({
        # ✅ Utiliser les boxes de référence (fixées lors du 1er spectre)
        ref_boxes <- reference_boxes()
        
        if (is.null(ref_boxes) || nrow(ref_boxes) == 0) {
          showNotification("⚠️ No reference boxes found. Generate centroids first.", 
                           type = "warning")
          return()
        }
        
        # ✅ Calculer les intensités sur tous les spectres
        batch_intensities <- calculate_batch_box_intensities(
          reference_boxes = ref_boxes,
          spectra_list = spectra_list(),
          apply_shift = FALSE  # Mettre TRUE si vous voulez la correction automatique
        )
        
        # Exporter
        readr::write_csv(batch_intensities, file)
        
        status_msg("✅ Batch export complete")
        showNotification(paste("✅ Exported intensities for", 
                               nrow(ref_boxes), "boxes across", 
                               length(spectra_list()), "spectra"), 
                         type = "message")
        
      }, error = function(e) {
        showNotification(paste("❌ Export error:", e$message), type = "error")
      })
      
      shinyjs::hide("export_loading_message")
    }
  )
  
  
  # ===== VERSION AMÉLIORÉE AVEC CORRECTION DE DÉCALAGE CHIMIQUE =====
  
  #' Version avancée avec estimation du décalage par corrélation croisée
  calculate_batch_box_intensities_advanced <- function(reference_boxes,
                                                       spectra_list,
                                                       reference_spectrum_name = NULL) {
    
    if (is.null(reference_boxes) || nrow(reference_boxes) == 0) {
      stop("reference_boxes est vide")
    }
    
    # Ajouter coordonnées centrales
    if (!all(c("F2_ppm", "F1_ppm") %in% names(reference_boxes))) {
      reference_boxes <- reference_boxes %>%
        dplyr::mutate(
          F2_ppm = (xmin + xmax) / 2,
          F1_ppm = (ymin + ymax) / 2
        )
    }
    
    result_df <- reference_boxes %>%
      dplyr::select(stain_id, F2_ppm, F1_ppm, xmin, xmax, ymin, ymax)
    
    # Spectre de référence pour calculer les décalages
    ref_spectrum <- NULL
    if (!is.null(reference_spectrum_name) && 
        reference_spectrum_name %in% names(spectra_list)) {
      ref_spectrum <- spectra_list[[reference_spectrum_name]]$spectrumData
    } else if (length(spectra_list) > 0) {
      ref_spectrum <- spectra_list[[1]]$spectrumData
    }
    
    for (spectrum_name in names(spectra_list)) {
      
      spectrum_data <- spectra_list[[spectrum_name]]
      
      if (is.null(spectrum_data) || is.null(spectrum_data$spectrumData)) {
        warning(paste("Spectre", spectrum_name, "invalide"))
        next
      }
      
      mat <- spectrum_data$spectrumData
      ppm_x <- suppressWarnings(as.numeric(colnames(mat)))
      ppm_y <- suppressWarnings(as.numeric(rownames(mat)))
      
      # ✅ ESTIMATION ROBUSTE DU DÉCALAGE PAR CORRÉLATION
      shift_f2 <- 0
      shift_f1 <- 0
      
      if (!is.null(ref_spectrum) && !identical(mat, ref_spectrum)) {
        tryCatch({
          # Créer des projections 1D pour estimer les shifts
          proj_ref_f2 <- colSums(ref_spectrum, na.rm = TRUE)
          proj_cur_f2 <- colSums(mat, na.rm = TRUE)
          
          # Corrélation croisée pour trouver le décalage optimal
          ccf_result <- ccf(proj_ref_f2, proj_cur_f2, 
                            lag.max = min(50, length(proj_ref_f2)/4), 
                            plot = FALSE)
          
          optimal_lag_f2 <- ccf_result$lag[which.max(ccf_result$acf)]
          
          # Convertir le lag en ppm
          if (length(ppm_x) > 1) {
            ppm_step <- mean(diff(ppm_x))
            shift_f2 <- optimal_lag_f2 * ppm_step
          }
          
          # Même chose pour F1
          proj_ref_f1 <- rowSums(ref_spectrum, na.rm = TRUE)
          proj_cur_f1 <- rowSums(mat, na.rm = TRUE)
          
          ccf_result_f1 <- ccf(proj_ref_f1, proj_cur_f1, 
                               lag.max = min(50, length(proj_ref_f1)/4), 
                               plot = FALSE)
          
          optimal_lag_f1 <- ccf_result_f1$lag[which.max(ccf_result_f1$acf)]
          
          if (length(ppm_y) > 1) {
            ppm_step <- mean(diff(ppm_y))
            shift_f1 <- optimal_lag_f1 * ppm_step
          }
          
        }, error = function(e) {
          # Si l'estimation échoue, pas de shift
          shift_f2 <<- 0
          shift_f1 <<- 0
        })
      }
      
      # Appliquer les shifts
      shifted_boxes <- reference_boxes %>%
        dplyr::mutate(
          xmin_shifted = xmin + shift_f2,
          xmax_shifted = xmax + shift_f2,
          ymin_shifted = ymin + shift_f1,
          ymax_shifted = ymax + shift_f1
        )
      
      # Calcul des intensités
      intensities <- vapply(seq_len(nrow(shifted_boxes)), FUN.VALUE = 0.0, 
                            FUN = function(i) {
                              box <- shifted_boxes[i, ]
                              
                              x_idx <- which(ppm_x >= box$xmin_shifted & ppm_x <= box$xmax_shifted)
                              y_idx <- which(ppm_y >= box$ymin_shifted & ppm_y <= box$ymax_shifted)
                              
                              if (length(x_idx) == 0 || length(y_idx) == 0) return(NA_real_)
                              
                              sum(mat[y_idx, x_idx], na.rm = TRUE)
                            })
      
      col_name <- paste0("Intensity_", make.names(basename(spectrum_name)))
      result_df[[col_name]] <- intensities
      
      # Ajouter les shifts détectés comme colonnes supplémentaires
      result_df[[paste0("Shift_F2_", make.names(basename(spectrum_name)))]] <- shift_f2
      result_df[[paste0("Shift_F1_", make.names(basename(spectrum_name)))]] <- shift_f1
    }
    
    return(result_df)
  }
  
  
} # end server

shinyApp(ui = ui, server = server)
