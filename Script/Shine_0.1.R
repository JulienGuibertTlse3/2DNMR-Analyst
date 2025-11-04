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
                                    downloadButton("download_projected_centroids", "Export Projected Peaks")
                                    
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
  # bounding_boxes_data <- reactiveVal(data.frame(xmin = numeric(0), xmax = numeric(0), ymin = numeric(0), ymax = numeric(0)))
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
  
  output$matrix_dim <- renderPrint({ req(bruker_data()); dim(bruker_data()$spectrumData) })
  output$status_message <- renderText({ status_msg() })
  
  # Displayed in the interface
  output$calculated_contour_text <- renderText({
    req(calculated_contour_value())
    paste0("Adviced value of the threshold : ", round(calculated_contour_value(), 2))
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
    
    # Choice update in the menu
    updateSelectInput(session, "selected_spectrum", choices = names(all_spectra), selected = names(all_spectra)[1])
    
    # Directly display the first spectrum
    bruker_data(all_spectra[[1]])
  })
  
  
  bounding_boxes_data <- reactive({
    req(modifiable_boxes(), bruker_data())
    
    mat <- bruker_data()$spectrumData
    if (is.null(mat)) {
      warning("La matrice du spectre est NULL")
      return(NULL)
    }
    
    ppm_x <- suppressWarnings(as.numeric(colnames(mat)))
    ppm_y <- suppressWarnings(as.numeric(rownames(mat)))
    
    if (any(is.na(ppm_x)) || any(is.na(ppm_y))) {
      warning("ppm_x ou ppm_y contient des NA")
      return(NULL)
    }
    
    boxes <- modifiable_boxes()
    if (nrow(boxes) == 0) return(boxes)
    
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
  
  # Reactive containers
  spectra_list <- reactiveVal(list())       # Contient les données Bruker
  spectra_plots <- reactiveVal(list())      # Contient les plots générés automatiquement
  
  observeEvent(subfolders(), {
    folders <- subfolders()
    if (length(folders) == 0) return(NULL)
    
    # Initialise la barre
    progress <- shiny::Progress$new()
    progress$set(message = "Loading spectra", value = 0)
    
    status_msg("🔄 Loading and creation of the spectrum...")
    
    all_data <- list()
    for (i in seq_along(folders)) {
      sub <- folders[[i]]
      data_path <- file.path(sub, "pdata", "1")
      progress$inc(1 / length(folders), detail = paste0("Processing ", basename(sub)))
      
      if (!dir.exists(data_path)) next
      
      data <- tryCatch({
        read_bruker(data_path, dim = "2D")
      }, error = function(e) {
        showNotification(paste("❌ Error reading Bruker in", sub), type = "error")
        return(NULL)
      })
      
      if (!is.null(data)) {
        all_data[[sub]] <- data
      }
    }
    
    spectra_list(all_data)
    
    updateSelectInput(session, "selected_subfolder",
                      choices = setNames(names(all_data), basename(names(all_data))))
    
    # ✅ Fermeture de la barre dans tous les cas
    progress$close()
    
    if (length(all_data) > 0) {
      bruker_data(all_data[[1]])
      status_msg("✅ Spectra loaded, generating plot...")
    } else {
      status_msg("⚠️ No valid spectra found")
    }
  })
  


  # UI to select the spectrum
  output$subfolder_selector <- renderUI({
    req(spectra_list())
    # Use of basename to display only the name of the folder
    subfolder_names <- names(spectra_list())
    subfolder_names <- if (is.character(subfolder_names)) subfolder_names else as.character(subfolder_names)
    selectInput("selected_subfolder", "Chosen spectrum :", choices = setNames(subfolder_names, basename(subfolder_names)))
  })
  
  
  observeEvent(input$selected_subfolder, {
    req(spectra_list(), spectra_plots())
    selected <- input$selected_subfolder
    
    if (!is.null(selected) && selected %in% names(spectra_list())) {
      
      # 🔄 Barre de progression
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = paste0("Loading spectrum: ", basename(selected)), value = 0)
      
      # Mise à jour des données
      selected_data <- spectra_list()[[selected]]
      progress$inc(0.3, detail = "Applying previous parameters...")
      
      if (!is.null(reference_boxes())) {
        fixed_boxes(reference_boxes())
      } else {
        fixed_boxes(data.frame(xmin = numeric(), xmax = numeric(), ymin = numeric(), ymax = numeric()))
      }
      
      progress$inc(0.3, detail = "Refreshing plot...")
      selected_plot <- spectra_plots()[[selected]]
      contour_plot_base(selected_plot)
      bounding_boxes_data()
      refresh_nmr_plot()
      
      progress$inc(0.4, detail = "Done")
      status_msg(paste0("✅ Spectrum selected: ", basename(selected)))
      
      close_progress()  # ⬅️ Fermeture ici, une fois le plot affiché
      }
  })
  
  
  ## Refresh ----
  
  # This function updates the NMR contour plot with the current bounding boxes and centroids.
  refresh_nmr_plot <- function() {
    req(contour_plot_base(), bruker_data())  # Ensure required reactive inputs are available before proceeding

    # ⚠️ Force evaluation of the reactive bounding_boxes_data to update intensity values
    boxes <- bounding_boxes_data()
    bbox_path_df <- make_bbox_outline(boxes)
    
    # Start with the base contour plot
    plot <- contour_plot_base()

    
    # If bounding boxes exist, overlay them on the plot as red dashed rectangles
    if (!is.null(boxes) && nrow(boxes) > 0) {
      plot <- plot +
        geom_path(data = bbox_path_df, aes(x = x, y = y, group = group), color = "red",
                  linewidth = 0.5)
    }

    
    # Initialize centroids to NULL
    centroids <- NULL
    
    # Prioritize imported centroids if available, otherwise use reactive centroid data
    if (!is.null(imported_centroids()) && nrow(imported_centroids()) > 0) {
      centroids <- imported_centroids()
    } else if (!is.null(centroids_data()) && nrow(centroids_data()) > 0) {
      centroids <- centroids_data()
    }
    
    # If centroids are available, plot them as colored points using their intensity
    if (!is.null(centroids)) {
      plot <- plot +
        geom_point(data = centroids, aes(x = F2_ppm, y = F1_ppm, color = as.numeric(stain_intensity)),
                   size = 1.2, inherit.aes = FALSE) +
        scale_color_gradient(low = "blue", high = "red", name = "Intensity")  # Color gradient based on intensity
    }
    
    # Store the final plot in the reactive value nmr_plot()
    nmr_plot(plot)
  }
  
  # Render the main NMR plot in the UI
  output$main_plot <- renderPlot({
    req(nmr_plot())  # Make sure the plot has been generated
    nmr_plot()       # Display the updated plot with centroids and bounding boxes
  })
  
  close_progress <- function() {
    isolate({
      if (!is.null(progress_bar())) {
        progress_bar()$close()
        progress_bar(NULL)
      }
    })
  }
  
  
  ## Calcul Threshold ----

  # This observer reacts when the "calculate_contour" button is clicked
  observeEvent(input$calculate_contour, {
    req(bruker_data())  # Ensure that Bruker data is loaded before proceeding
    
    # Extract the spectrum matrix from the reactive Bruker data
    mat <- bruker_data()$spectrumData
    
    # Select the threshold method based on user input
    seuil <- switch(input$seuil_method,
                    
                    # If "max_pct" is selected: threshold = max intensity * percentage
                    "max_pct" = seuil_max_pourcentage(mat, pourcentage = input$pct_val),
                    
                    # If "bruit_mult" is selected: threshold = noise level * multiplier
                    "bruit_mult" = seuil_bruit_multiplicatif(mat, facteur = input$bruit_mult),
                    
                    # Default/fallback: show an error if the method is not recognized
                    {
                      showNotification("❌ Unrecognized threshold method", type = "error")
                      return(NULL)  # Stop execution
                    }
    )
    
    # Save the calculated threshold in the corresponding reactive value
    calculated_contour_value(seuil)
    
    # Display a success notification with the calculated threshold value
    showNotification(
      paste0("✅ Contour threshold successfully calculated: ", round(seuil, 2)),
      type = "message"
    )
  })
  
  # Display the calculated threshold value in the UI
  output$seuil_text <- renderText({
    val <- calculated_contour_value()  # Retrieve the current value
    if (is.null(val)) return("No threshold calculated.")  # Fallback if no value
    round(val, 5)  # Round the value and render it as text
  })
  
  
  ## Generate Plot ----
  
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
        find_nmr_peak_centroids(
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
  
  ## Manually Add Centroids / BBs ----
  
  observeEvent(input$add_manual_centroid, {
    req(input$manual_f2, input$manual_f1)
    current <- centroids_data()
    if (is.null(current)) {
      current <- data.frame(
        F2_ppm = numeric(0),
        F1_ppm = numeric(0),
        stain_intensity = numeric(0),
        stain_id = character(0),
        stringsAsFactors = FALSE
      )
    }
    
    existing_ids <- current$stain_id[grepl("^man", current$stain_id)]
    man_number <- if (length(existing_ids) == 0) 1 else max(as.integer(sub("man", "", existing_ids)), na.rm = TRUE) + 1
    
    # Estimate intensity
    contour_data <- result_data()$contour_data
    eps <- input$eps_value
    local_points <- contour_data %>%
      dplyr::filter(
        abs(-x - input$manual_f2) <= eps*16,
        abs(-y - input$manual_f1) <= eps*16
      )
    estimated_intensity <- sum(local_points$level, na.rm = TRUE)
    
    # Build new_point with *same columns as current*
    new_point <- data.frame(
      F2_ppm = as.numeric(input$manual_f2),
      F1_ppm = as.numeric(input$manual_f1),
      stain_intensity = as.numeric(estimated_intensity),
      stain_id = paste0("man", man_number),
      status = "add",   # ✅ add status column
      stringsAsFactors = FALSE
    )
    
    # Add any missing columns from current
    missing_cols <- setdiff(colnames(current), colnames(new_point))
    for (mc in missing_cols) {
      new_point[[mc]] <- NA
    }
    
    # Reorder columns to match current (status will stay at end if not in current)
    new_point <- new_point[, unique(c(colnames(current), "status")), drop = FALSE]
    
    pending_centroids(dplyr::bind_rows(pending_centroids(), new_point))
    showNotification(
      paste("✅ Manual peak added:", new_point$stain_id, "- Intensity =", round(estimated_intensity)),
      type = "message"
    )
  })
  
  
  
  observeEvent(input$add_manual_bbox, {
    req(input$manual_xmin, input$manual_xmax, input$manual_ymin, input$manual_ymax)
    
    new_box <- data.frame(
      xmin = input$manual_xmin,
      xmax = input$manual_xmax,
      ymin = input$manual_ymin,
      ymax = input$manual_ymax,
      status = "add"   # ✅ add status column
    )
    
    current_boxes <- modifiable_boxes()
    
    if (nrow(current_boxes) > 0 && !all(c("xmin", "xmax", "ymin", "ymax") %in% names(current_boxes))) {
      showNotification("❌ Format invalide des boîtes existantes.", type = "error")
      return()
    }
    
    pending_boxes(dplyr::bind_rows(pending_boxes(), new_box))
    showNotification("🟦 Boîte ajoutée manuellement (pending).", type = "message")
  })
  
  
  ## Manually Delete centroids / BBs ----
  
  observeEvent(input$delete_centroid, {
  selected <- input$centroid_table_rows_selected
  if (length(selected) > 0) {
    current <- centroids_data()
    to_delete <- current[selected, ]
    to_delete$status <- "delete"

    pending_centroids(bind_rows(pending_centroids(), to_delete))
    centroids_data(current[-selected, ])

    showNotification("🗑️ Centroid marked for deletion (pending)", type = "message")
  } else {
    showNotification("⚠️ Please select a centroid to delete", type = "warning")
  }
})

observeEvent(input$delete_bbox, {
  selected <- input$bbox_table_rows_selected
  if (length(selected) > 0) {
    current <- modifiable_boxes()
    to_delete <- current[selected, ]
    to_delete$status <- "delete"

    pending_boxes(bind_rows(pending_boxes(), to_delete))
    modifiable_boxes(current[-selected, ])

    showNotification("🗑️ Box marked for deletion (pending)", type = "message")
  } else {
    showNotification("⚠️ Select a box to delete", type = "warning")
  }
})


  
  ## Dataframes ----
  
  # Display a table with only the 4 main columns of centroids
  output$centroid_table <- renderDT({
    centroids_filtered <- centroids_data()[, 1:4]
    datatable(centroids_filtered, selection = "single", options = list(pageLength = 5))
  })
  
  # Display full centroid table with all columns
  output$full_centroid_table <- renderDT({
    datatable(centroids_data(), selection = "single", options = list(pageLength = 5))
  })
  
  # Display bounding box table
  output$bbox_table <- renderDT({
    datatable(bounding_boxes_data(), selection = "single", options = list(pageLength = 5))
  })
  
  
  ## Interactive Plot ----
  
  output$interactivePlot <- renderPlotly({
    plot_obj <- nmr_plot()
    if (is.null(plot_obj)) {
      # Show an empty message or placeholder plot
      ggplotly(ggplot() + theme_void() + ggtitle("No spectrum displayed"))
    } else {
      plot_obj <- plot_obj + 
        theme(
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 9),
          legend.key.size = unit(0.4, "cm")
        )
      ggplotly(plot_obj, source = "nmr_plot") %>% 
        layout(dragmode = "select") %>%
        config(modeBarButtonsToAdd = list("select2d", "lasso2d")) %>%
        event_register("plotly_selected") %>% layout(
          xaxis = list(
            showticklabels = TRUE,
            ticks = "outside"
          ),
          yaxis = list(
            showticklabels = TRUE,
            ticks = "outside"
          )
        )
    }
  })
  
  ## Click event ----
  
  observeEvent(event_data("plotly_click", source = "nmr_plot"), {
    click_data <- event_data("plotly_click", source = "nmr_plot")
    
    # Vérification : x et y doivent exister et ne pas être NA
    if (!is.null(click_data) &&
        !is.null(click_data$x) && !is.null(click_data$y) &&
        !is.na(click_data$x) && !is.na(click_data$y)) {
      
      coords <- list(F2_ppm = click_data$x, F1_ppm = click_data$y)
      last_click_coords(coords)
      message("Clic détecté : x=", -click_data$x, " y=", -click_data$y)
      
    } else {
      message("Clic ignoré : pas de coordonnées valides.")
    }
  })
  
  
  output$clickedCoords <- renderPrint({
    coords <- last_click_coords()
    if (is.null(coords)) {
      "Cliquez sur un point du spectre pour afficher les coordonnées."
    } else {
      paste0("F2_ppm = ", round(-coords$F2_ppm, 9), ", F1_ppm = ", round(-coords$F1_ppm, 9))
    }
  })
  
  
  ## Numeric params ----
  
  observeEvent(input$spectrum_type, {
    params <- switch(input$spectrum_type,
                     "TOCSY" = list(contour_start = 100000, contour_num = 30, contour_factor = 1.3, eps_value = 0.0068),
                     "HSQC"  = list(contour_start = 20000,  contour_num = 30,  contour_factor = 1.3, eps_value = 0.0068),
                     "COSY"  = list(contour_start = 80000,  contour_num = 40,  contour_factor = 1.3, eps_value = 0.0068),
                     "UFCOSY"  = list(contour_start = 30000,  contour_num = 60,  contour_factor = 1.3, eps_value = 0.014)
    )
    
    updateNumericInput(session, "contour_start", value = params$contour_start)
    updateNumericInput(session, "eps_value", value = params$eps_value)
    
    
    # Optional: Uncomment if these inputs are enabled in the UI
    # updateNumericInput(session, "contour_num", value = params$contour_num)
    # updateNumericInput(session, "contour_factor", value = params$contour_factor)
  })
  

  
  ## Export Centroids & BB ----
  
  # --- Centroids Export Handler ---
  
  output$export_centroids <- downloadHandler(
    filename = function() {
      paste0("centroids_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- centroids_data()
      if (!is.null(df) && nrow(df) > 0) {
        write.csv(df, file, row.names = FALSE)
      } else {
        # Create empty file or with NA to avoid error
        write.csv(data.frame(), file)
      }
    }
  )
  
  # --- Bounding Boxes Export Handler ---
  
  output$export_boxes <- downloadHandler(
    filename = function() {
      paste0("bounding_boxes_", Sys.Date(), ".csv")  # Filename includes current date for versioning
    },
    content = function(file) {
      boxes <- bounding_boxes_data()  # Retrieve current bounding box data
      
      if (!is.null(boxes) && nrow(boxes) > 0) {
        write.csv(boxes, file, row.names = FALSE)  # Write bounding box data to CSV if available
      } else {
        # If no bounding boxes exist, write a CSV with NA values to keep file structure consistent
        write.csv(data.frame(xmin = NA, xmax = NA, ymin = NA, ymax = NA), file, row.names = FALSE)
      }
    }
  )
  
  
  ## Import centroid list ----
  
  clean_centroids_df <- function(df) {
    df$F2_ppm <- as.numeric(gsub(",", ".", trimws(df$F2_ppm)))
    df$F1_ppm <- as.numeric(gsub(",", ".", trimws(df$F1_ppm)))
    df$stain_intensity <- as.numeric(gsub(",", ".", trimws(df$stain_intensity)))
    df
  }
  
  
  observeEvent(input$import_centroids_file, {
    req(input$import_centroids_file)
    
    imported <- tryCatch({
      read.csv(input$import_centroids_file$datapath, sep = ";", stringsAsFactors = FALSE)
    }, error = function(e) {
      showNotification(paste("Import error:", e$message), type = "error")
      return(NULL)
    })
    
    if (!is.null(imported)) {
      if (all(c("stain_id", "stain_intensity", "F2_ppm", "F1_ppm") %in% colnames(imported))) {
        
        imported <- clean_centroids_df(imported)
        
        centroids_data(imported)
        refresh_nmr_plot()
        
        showNotification("✅ Imported centroids replaced the current set", type = "message")
      } else {
        showNotification("❌ The file must contain the columns 'stain_id', 'stain_intensity', 'F2_ppm' and 'F1_ppm'", type = "error")
      }
    }
    
  })
  
  
  
  ## Centroid ----
  
  # Initialization after loading the first spectrum
  observeEvent(spectra_list(), {
    centroids(NULL)  # Reset centroids when a new spectrum list is loaded
  })
  
  observeEvent(input$add_centroid, {
    current <- centroids()
    
    new_row <- data.frame(
      stain_id = paste0("man", nrow(current) + 1),
      F2_ppm = input$manual_F2_ppm,
      F1_ppm = input$manual_F1_ppm,
      stain_intensity = 0,
      status = "add"
    )
    
    pending_centroids(bind_rows(pending_centroids(), new_row))
    showNotification("➕ Centroid added (pending)", type = "message")
  })
  
  
  
  observeEvent(input$fuse_btn, {
    req(centroids_data())
    
    sel <- event_data("plotly_selected", source = "nmr_plot")

    
    if (is.null(sel) || nrow(sel) < 2) {
      showNotification("⚠️ Need at least 2 points selected.", type = "error")
      return()
    }
    
    sel$x <- -sel$x
    sel$y <- -sel$y
    
    # Récupère les points sélectionnés
    brushed <- dplyr::semi_join(centroids_data(),
                                sel,
                                by = c("F2_ppm" = "x", "F1_ppm" = "y"))
    
    if (nrow(brushed) < 2) {
      showNotification("⚠️ Selection did not match enough points.", type = "error")
      return()
    }

    # Extraire le numéro du premier pic fusionné
    first_peak_id <- brushed$stain_id[1]  # "peak1"
    peak_number <- gsub("[^0-9]", "", first_peak_id)


    fused_point <- data.frame(
      stain_id = paste0("fused_point", peak_number),
      F2_ppm = mean(brushed$F2_ppm),
      F1_ppm = mean(brushed$F1_ppm),
      stain_intensity = sum(as.numeric(brushed$stain_intensity), na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    

    remaining <- dplyr::anti_join(centroids_data(), brushed, 
                                  by = c("F2_ppm", "F1_ppm"))
    
    # Add NA for any extra columns in `remaining`
    missing_cols <- setdiff(names(remaining), names(fused_point))
    for (mc in missing_cols) {
      fused_point[[mc]] <- NA
    }
    
    # Reorder to match `remaining`
    fused_point <- fused_point[, names(remaining), drop = FALSE]
    
    centroids_data(rbind(remaining, fused_point))
    
    # Suppression des boxes correspondantes
    if (!is.null(modifiable_boxes()) && nrow(modifiable_boxes()) > 0) {
      boxes <- modifiable_boxes()
      
      selected_boxes <- which(
        boxes$xmin <= max(brushed$F2_ppm) &
          boxes$xmax >= min(brushed$F2_ppm) &
          boxes$ymin <= max(brushed$F1_ppm) &
          boxes$ymax >= min(brushed$F1_ppm)
      )
      
      # Récupérer les boîtes supprimées
      removed_boxes <- boxes[selected_boxes, ]
      
      # Supprimer ces boîtes
      boxes <- boxes[-selected_boxes, ]
      
      # Créer une nouvelle boîte englobante basée sur les extremums des boîtes supprimées
      if (nrow(removed_boxes) > 0) {
        new_box <- data.frame(
          xmin = min(removed_boxes$xmin),
          xmax = max(removed_boxes$xmax),
          ymin = min(removed_boxes$ymin),
          ymax = max(removed_boxes$ymax),
          stain_id = paste0("bbox_fused_point", peak_number)
        )
        boxes <- rbind(boxes, new_box)
      }
      
      modifiable_boxes(boxes)
      fixed_boxes(boxes)
    }

    # Append newly fused point to fused_points()
    pending_fusions(rbind(pending_fusions(), fused_point))
    
    showNotification("✅ Points fused successfully", type = "message")
  })
  
  
  output$pending_centroids_table <- renderDT({
    datatable(
      pending_centroids(),
      selection = "single",
      options = list(pageLength = 5)
    )
  })
  
  output$pending_boxes_table <- renderDT({
    datatable(
      pending_boxes(),
      selection = "single",
      options = list(pageLength = 5)
    )
  })
  
  output$pending_fusions_table <- renderDT({
    req(pending_fusions())
    datatable(
      pending_fusions(),
      selection = "single",
      options = list(scrollX = TRUE, pageLength = 5)
    )
  })
  
  
  # Discard one centroid
  observeEvent(input$discard_selected_centroid, {
    selected <- input$pending_centroids_table_rows_selected
    if (length(selected) > 0) {
      current <- pending_centroids()
      pending_centroids(current[-selected, ])
      showNotification("🗑️ Pending centroid discarded", type = "message")
    } else {
      showNotification("⚠️ Select a pending centroid to discard", type = "warning")
    }
  })
  
  # Discard one box
  observeEvent(input$discard_selected_box, {
    selected <- input$pending_boxes_table_rows_selected
    if (length(selected) > 0) {
      current <- pending_boxes()
      pending_boxes(current[-selected, ])
      showNotification("🗑️ Pending box discarded", type = "message")
    } else {
      showNotification("⚠️ Select a pending box to discard", type = "warning")
    }
  })
  
  # Discard one fusion
  observeEvent(input$discard_selected_fusion, {
    selected <- input$pending_fusions_table_rows_selected
    if (length(selected) > 0) {
      current <- pending_fusions()
      pending_fusions(current[-selected, ])
      showNotification("🗑️ Pending fusion discarded", type = "message")
    } else {
      showNotification("⚠️ Select a pending fusion to discard", type = "warning")
    }
  })
  
  
  observe({
    print(event_data("plotly_selected"))
  })
  
  
  # Batch ( Not implemented yet ) ----
  ## Test ----
  output$download_projected_centroids <- downloadHandler(
    filename = function() {
      paste0("projected_centroids_", Sys.Date(), ".zip")
    },
    content = function(zipfile) {
      req(centroids_data())
      req(result_data_list())
      
      tmp_dir <- tempdir()
      csv_files <- c()
      
      eps_val <- input$eps_value %||% 0.03
      
      # Reference centroids from the base spectrum (e.g., first TOCSY)
      reference_centroids <- centroids_data()
      
      for (name in names(result_data_list())) {
        result <- result_data_list()[[name]]
        if (is.null(result$contour_data)) next
        
        contour_data <- result$contour_data
        
        # --- Estimate 2D shift by cross-correlation of contour coordinates ---
        # Default shifts in case cross-correlation fails
        delta_F2 <- 0
        delta_F1 <- 0
        
        # Attempt shift estimation
        try({
          ref_hist <- with(reference_centroids, MASS::kde2d(F2_ppm, F1_ppm, n = 200))
          spec_hist <- with(contour_data, MASS::kde2d(-x, -y, n = 200))
          corr <- stats::convolve2d(ref_hist$z, spec_hist$z, type = "open")
          max_idx <- which(corr == max(corr, na.rm = TRUE), arr.ind = TRUE)
          delta_F2 <- (max_idx[1] - nrow(ref_hist$z)) * mean(diff(ref_hist$x))
          delta_F1 <- (max_idx[2] - ncol(ref_hist$z)) * mean(diff(ref_hist$y))
        }, silent = TRUE)
        
        shifted_centroids <- reference_centroids %>%
          dplyr::mutate(
            F2_ppm = F2_ppm + delta_F2,
            F1_ppm = F1_ppm + delta_F1
          )
        
        if (isTRUE(input$disable_clustering)) {
          # === Cas sans clustering : recalcul des intensités ===
          projected_centroids <- shifted_centroids %>%
            dplyr::rowwise() %>%
            dplyr::mutate(
              stain_intensity = {
                local_points <- contour_data %>%
                  dplyr::filter(
                    sqrt((-x - F2_ppm)^2 + (-y - F1_ppm)^2) <= eps_val * 16
                  )
                
                if (nrow(local_points) > 3) {
                  covmat <- cov(cbind(local_points$x, local_points$y))
                  eigvals <- eigen(covmat)$values
                  adaptive_radius <- sqrt(max(eigvals)) * 2
                  
                  local_points <- contour_data %>%
                    dplyr::filter(
                      sqrt((-x - F2_ppm)^2 + (-y - F1_ppm)^2) <= adaptive_radius
                    )
                }
                sum(local_points$level, na.rm = TRUE)
              }
            ) %>%
            dplyr::ungroup()
        } else {
          # === Cas clustering : garder les valeurs originales ===
          projected_centroids <- shifted_centroids %>%
            dplyr::mutate(
              stain_intensity = reference_centroids$stain_intensity
            )
        }
        
            noise_est <- median(projected_centroids$stain_intensity[projected_centroids$stain_intensity > 0], na.rm = TRUE) / 10
            projected_centroids$VolumeAUC <- pmax(projected_centroids$stain_intensity - noise_est, 0)

            projected_centroids$VolumeEllipsoid <- sapply(1:nrow(projected_centroids), function(i) {
              calculate_ellipsoid_volume(projected_centroids[i, ], contour_data, eps_val *20)
            })
        
        subfolder_name <- basename(name)
        output_csv <- file.path(tmp_dir, paste0(subfolder_name, "_projected_centroids.csv"))
        readr::write_csv(projected_centroids, output_csv)
        csv_files <- c(csv_files, output_csv)
      }
      
      zip(zipfile, files = csv_files, flags = "-j")
    }
  )
  
  
  # Allow user to select output directory via a folder chooser dialog
  # 'save_roots' defines root folders accessible for navigation: user home and root "/"
  save_roots <- c(Home = normalizePath("~"), Root = "/")
  
  # Enable shinyDirChoose to let user pick a directory for saving results
  shinyDirChoose(input, "save_directory", roots = save_roots, session = session)
  
  # Reactive expression to parse the selected directory path from the shinyDirChoose input
  save_directory <- reactive({
    req(input$save_directory)  # Ensure user has selected a directory
    parseDirPath(save_roots, input$save_directory)  # Returns full normalized path
  })
  
  # Render the currently selected directory path in the UI as printed text
  output$save_dir_display <- renderPrint({
    save_directory()
  })
  
  
  observeEvent(input$apply_changes, {
    centroids_data(bind_rows(centroids_data(), pending_centroids()))
    modifiable_boxes(bind_rows(modifiable_boxes(), pending_boxes()))
    
    pending_centroids(pending_centroids()[0, ])
    pending_boxes(pending_boxes()[0, ])
    pending_fusions(data.frame(stain_id=character(), F2_ppm=numeric(),
                            F1_ppm=numeric(), stain_intensity=numeric()))
    
    refresh_nmr_plot()
    showNotification("✅ Tous les changements ont été appliqués", type = "message")
  })
  
  observeEvent(input$discard_changes, {
    pending_centroids(pending_centroids()[0, ])
    pending_boxes(pending_boxes()[0, ])
    pending_fusions(data.frame(stain_id=character(), F2_ppm=numeric(),
                            F1_ppm=numeric(), stain_intensity=numeric()))
    showNotification("❌ Modifications en attente annulées", type = "warning")
  })
  
  ## Reset all ----
  
  # Observe the reset button event to clear all reactive values and reset interface state
  observeEvent(input$reset_all, {
    nmr_plot(NULL)
    contour_plot_base(NULL)
    imported_centroids(NULL)
    centroids_data(NULL)
    fixed_boxes(NULL)
    reference_boxes(NULL)
    
    # Optionally also reset inputs like selected_subfolder
    updateSelectInput(session, "selected_subfolder", selected = "")
    
    # ✅ Update message
    status_msg("🔁 Interface reset. Please select a new spectrum.")
  })
  
  
  
  ## Other small functions ----
  
  ### status msg
  
  output$status_msg <- renderUI({
    req(input$selected_subfolder)
    HTML(paste0("<span style='font-size:16px; font-weight:bold; color:green;'>✅ Spectrum selected: <code>", 
                basename(input$selected_subfolder), "</code></span>"))
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
  
  
}

shinyApp(ui = ui, server = server)
