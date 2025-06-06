library(shiny)
library(shinyFiles)
library(plotly)
library(ggplot2)
library(DT)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(sp)

source("C://Users//juguibert//Documents//Function_test//Read_2DNMR_spectrum.R")
source("C://Users//juguibert//Documents//Function_test//Vizualisation.R")
source("C://Users//juguibert//Documents//Function_test//Integration.R")
source("C://Users//juguibert//Documents//Function_test//Pping.R")


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
        menuItem("README", tabName = "Readme", icon = icon("book")),
        menuItem("Visualization", tabName = "Visualisation", icon = icon("chart-area"))
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
        
        tabItem(tabName = "Visualisation",
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
                                    
                                    selectInput("spectrum_type", "Spectrum type:",
                                                choices = c("TOCSY", "HSQC", "COSY", "UFCOSY"),
                                                selected = "TOCSY"),
                                    
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
                                    
                                    # Button to compute the threshold
                                    actionButton("calculate_contour", "Compute threshold"),
                                    
                                    # Display computed threshold
                                    br(),
                                    strong("Computed threshold:"),
                                    verbatimTextOutput("seuil_text"),
                                    br(),
                                    
                                    # User-editable value
                                    numericInput("contour_start", "Intensity value:", value = NULL, min = 0, step = 100),
                                    
                                    actionButton("generate_plot", "📊 Generate Plot"),
                                    checkboxInput("disable_clustering", "🔍 Ne pas utiliser de clustering (méthode alternative)", value = FALSE),
                                    actionButton("generate_centroids", "🔴 Find Peaks"),
                                    numericInput("eps_value", "Clustering epsilon:", value = 0.01, min = 0, step = 0.001),
                                    actionButton("export_projected_centroids", "📤 Export batch intensity")
                           ),
                           
                           # Third panel : Used to manually modify automated peak picking result
                           
                           tabPanel("🔴 Peaks and Bounding Boxes",
                                    actionButton("toggle_centroid_section", "Add/Remove a peak"),
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
                                    ))
                           ),
                           
                           
                           # Fifth panel : Used to manually import or export result
                           
                           tabPanel("🔁 Import/Export",
                                    actionButton("export_centroids", "Export peaks"),
                                    verbatimTextOutput("centroids_output"),
                                    tags$h4("📥 Import peaks"),
                                    fileInput("import_centroids_file", "Import CSV file:", accept = ".csv"),
                                    downloadButton("export_boxes", "Export bounding boxes")
                           )
                         )
                  ),
                  
                  column(9, 
                         # Loading messages
                         div(id = "loading_message", 
                             "Generating plot, please wait...", 
                             style = "font-size: 18px; color: blue; font-weight: bold; display: none;"),
                         div(id = "export_loading_message", 
                             "Exporting centroids, please wait...", 
                             style = "font-size: 18px; color: blue; font-weight: bold; display: none;"),
                         
                         # Full-width interactive plot
                         withSpinner(
                           div(id = "interactivePlot", 
                               plotlyOutput("interactivePlot", height = "600px", width = "100%"))
                         ),
                         
                         br(), tags$hr(), br(),
                         
                         # Tabs for dataframes
                         box(
                           title = "Associated data",
                           width = 12,
                           solidHeader = TRUE,
                           status = "primary",
                           tabBox(
                             width = 12,
                             id = "data_tabs",
                             tabPanel("🔵 Peaks", 
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
           "TOCSY" = list(intensity_threshold = 80000, contour_num = 20, contour_factor = 1.3, eps_value = 0.0068),
           "HSQC"  = list(intensity_threshold = 30000,  contour_num = 30,  contour_factor = 1.3, eps_value = 0.0068),
           "COSY"  = list(intensity_threshold = 60000,  contour_num = 30,  contour_factor = 1.3, eps_value = 0.0068),
           "UFCOSY"  = list(intensity_threshold = 50000,  contour_num = 70,  contour_factor = 1.3, eps_value = 0.014)
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

    
    # Start with the base contour plot
    plot <- contour_plot_base()

    
    # If bounding boxes exist, overlay them on the plot as red dashed rectangles
    if (!is.null(boxes) && nrow(boxes) > 0) {
      plot <- plot +
        geom_rect(data = boxes, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  color = "red", fill = NA, linetype = "dashed", inherit.aes = FALSE)
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
        scale_color_gradient(low = "blue", high = "red")  # Color gradient based on intensity
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

      ### 🔹 UTILISATION de `peak_pick_2d_nt2` (pas de clustering) ----
      result_peaks <- tryCatch({
        peak_pick_2d_nt2(
          bruker_data = selected_spectrum,
          threshold_value = input$contour_start,
          neighborhood_size = 3,
          f2_exclude_range = c(4.7, 5.0)
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
      ### 🔹 UTILISATION de `process_nmr_centroids` (avec clustering) ----
      result1 <- tryCatch({
        process_nmr_centroids(
          rr_data = selected_spectrum,
          contour_data = selected_result$contour_data,
          intensity_threshold = modulate_threshold(input$contour_start) %||% modulate_threshold(calculated_contour_value()),
          contour_num = params$contour_num,
          contour_factor = params$contour_factor,
          eps_value = input$eps_value,
          keep_peak_ranges = list(
            c(0.5, -0.5), c(1, 0.8), c(1.55, 1.45), c(3.397, 3.38),
            c(1.28, 1.26), c(5.367, 5.353), c(4.47, 4.45), c(4.385, 4.375)
          )
        )
      }, error = function(e) {
        showNotification(paste("❌ Processing error:", e$message), type = "error")
        print(paste("Caught error:", e$message))
        shinyjs::hide("loading_message")
        return(NULL)
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
  
  
  
  
  ## Manually Add Centroids / BBs ----
  
  observeEvent(input$add_manual_centroid, {
    req(input$manual_f2, input$manual_f1)
    current <- centroids_data()
    
    if (is.null(current)) {
      current <- data.frame(
        F2_ppm = numeric(0),
        F1_ppm = numeric(0),
        stain_intensity = numeric(0),
        stain_id = character(0)
      )
    }
    
    existing_ids <- current$stain_id[grepl("^man", current$stain_id)]
    man_number <- if (length(existing_ids) == 0) 1 else max(as.integer(sub("man", "", existing_ids)), na.rm = TRUE) + 1
    
    # === Estimate intensity from contour_data
    contour_data <- result_data()$contour_data
    eps <- input$eps_value
    
    local_points <- contour_data %>%
      dplyr::filter(
        abs(-x - input$manual_f2) <= eps,
        abs(-y - input$manual_f1) <= eps
      )
    
    estimated_intensity <- sum(local_points$level, na.rm = TRUE)
    
    new_point <- data.frame(
      F2_ppm = input$manual_f2,
      F1_ppm = input$manual_f1,
      stain_intensity = estimated_intensity,
      stain_id = paste0("man", man_number)
    )
    
    centroids_data(rbind(current, new_point))
    refresh_nmr_plot()
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
      ymax = input$manual_ymax
    )
    
    current_boxes <- modifiable_boxes()
    
    if (nrow(current_boxes) > 0 && !all(c("xmin", "xmax", "ymin", "ymax") %in% names(current_boxes))) {
      showNotification("❌ Format invalide des boîtes existantes.", type = "error")
      return()
    }
    
    updated_boxes <- bind_rows(current_boxes, new_box)
    modifiable_boxes(updated_boxes)
    refresh_nmr_plot()
    showNotification("🟦 Boîte ajoutée manuellement.", type = "message")
  })
  
  
  ## Manually Delete centroids / BBs ----
  
  observeEvent(input$delete_centroid, {
    selected <- input$centroid_table_rows_selected
    if (length(selected) > 0) {
      current <- centroids_data()
      centroids_data(current[-selected, ])
      refresh_nmr_plot()
      showNotification("🗑️ Centroid deleted", type = "message")
    } else {
      showNotification("⚠️ Please select a centroid to delete", type = "warning")
    }
  })
  
  
  observeEvent(input$delete_bbox, {
    selected <- input$bbox_table_rows_selected
    if (length(selected) > 0) {
      current <- modifiable_boxes()
      modifiable_boxes(current[-selected, ])
      refresh_nmr_plot()
      showNotification("🗑️ Boîte supprimée", type = "message")
    } else {
      showNotification("⚠️ Sélectionnez une boîte à supprimer", type = "warning")
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
      ggplotly(plot_obj, source = "nmr_plot")
    }
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
    
    
    # Optional: Uncomment if these inputs are enabled in the UI
    # updateNumericInput(session, "contour_num", value = params$contour_num)
    # updateNumericInput(session, "contour_factor", value = params$contour_factor)
  })
  

  
  ## Export Centroids & BB ----
  
  # --- Centroids Export Handler ---
  
  observeEvent(input$export_centroids, {
    shinyjs::show("loading_message")  # Show a loading message to inform user the export is in progress
    
    req(main_directory())  # Ensure the main directory is set before proceeding
    
    print(main_directory())  # Print the path to console for debugging
    
    export_path <- file.path(main_directory(), "exported_centroids.csv")  # Define full path for export file
    
    tryCatch({
      # Check if there are any centroids to export
      if (nrow(centroids_data()) > 0) {
        write.csv(centroids_data(), export_path, row.names = FALSE)  # Save centroid data as CSV without row names
        
        # Notify user that export succeeded and show file location
        showNotification(paste("Centroids exported to", export_path), type = "message")
      } else {
        # Warn user if there are no centroids to export
        showNotification("No centroids to export", type = "warning")
      }
    }, error = function(e) {
      # Handle any error during export and notify user with error message
      showNotification(paste("Error during export:", e$message), type = "error")
    })
    
    shinyjs::hide("loading_message")  # Hide the loading message after export completes
  })
  
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
  
  observeEvent(input$import_centroids_file, {
    req(input$import_centroids_file)  # Ensure a file has been uploaded before proceeding
    
    imported <- tryCatch({
      read.csv(input$import_centroids_file$datapath, sep = ";")  # Attempt to read the CSV file with ';' separator
    }, error = function(e) {
      showNotification(paste("Import error:", e$message), type = "error")  # Show error notification if import fails
      return(NULL)
    })
    
    if (!is.null(imported)) {
      # Verify that the imported data contains the expected columns
      if (all(c("stain_id", "stain_intensity", "F2_ppm", "F1_ppm") %in% colnames(imported))) {
        imported_centroids(imported)  # Update reactive value with imported centroids
        refresh_nmr_plot()  # Refresh the plot to display new centroids
        showNotification("✅ Centroids imported and added to the plot", type = "message")
      } else {
        showNotification("❌ The file must contain the columns 'F2_ppm' and 'F1_ppm'", type = "error")
      }
    }
  })
  
  ## Centroid ----
  
  # Initialization after loading the first spectrum
  observeEvent(spectra_list(), {
    centroids(NULL)  # Reset centroids when a new spectrum list is loaded
  })
  
  # Update centroid list if user manually adds a centroid
  observeEvent(input$add_centroid, {
    current <- centroids()
    
    new_row <- data.frame(  # Create a new row with manual input values
      stain_id = paste0("man", nrow(current) + 1),  # Assign an incremental manual ID
      F2_ppm = input$manual_F2_ppm,  # X-coordinate from manual input
      F1_ppm = input$manual_F1_ppm,  # Y-coordinate from manual input
      stain_intensity = 0  # Default intensity or could be estimated later
    )
    
    centroids(rbind(current, new_row))  # Append the new centroid to the existing list
  })
  
  

  
  ## Batch ( Not implemented yet ) ----
  observeEvent(input$run_batch, {
    req(save_directory())  # Ensure output directory is set
    
    # Check if necessary data for batch processing is available
    if (is.null(bounding_boxes_data()) || is.null(centroids_data()) || is.null(spectra_list())) {
      showNotification("❌ Missing data for batch processing.", type = "error")
      return(NULL)
    }
    
    box_ref <- bounding_boxes_data()
    print("box_ref:")
    print(box_ref)
    
    centroids_ref <- centroids_data()
    
    spectra <- spectra_list()
    n <- length(spectra)
    
    if (n == 0) {
      showNotification("❌ No spectra loaded.", type = "error")
      return(NULL)
    }
    
    dir.create("results_batch", showWarnings = FALSE)  # Create output folder if it doesn't exist
    
    withProgress(message = "Running batch processing...", value = 0, {
      lapply(seq_along(spectra), function(i) {
        incProgress(1 / n, detail = paste("Spectrum", names(spectra)[i]))
        spectre_name <- names(spectra)[i]
        data <- spectra[[i]]
        mat <- data$spectrumData  # Matrix of spectral data
        
        ppm_x <- as.numeric(colnames(mat))
        ppm_y <- as.numeric(rownames(mat))
        
        boxes <- box_ref
        
        # Calculate intensity within each bounding box on the spectrum
        boxes$stain_intensity <- apply(boxes, 1, function(box) {
          x_idx <- which(ppm_y <= box["xmax"] & ppm_y >= box["xmin"])  # Indices on x-axis within box limits
          print("ppm_x_idx :")
          print(x_idx)
          y_idx <- which(ppm_x <= box["ymax"] & ppm_x >= box["ymin"])  # Indices on y-axis within box limits
          # print("ppm_y_idx :")
          # print(y_idx)
          
          # Sum intensity values inside the bounding box, ignoring NA
          sum(mat[x_idx, y_idx], na.rm = TRUE)
        })
        
        # Add box ID if it doesn't exist
        if (is.null(boxes$box_id)) {
          boxes$box_id <- paste0("box", seq_len(nrow(boxes)))
        }
        
        # Select columns to export
        result_data <- boxes[, c("box_id", "xmin", "xmax", "ymin", "ymax", "stain_intensity")]
        
        # Save results CSV per spectrum
        write.csv(result_data, file.path(save_directory(), paste0(basename(spectre_name), "_results.csv")), row.names = FALSE)
      })
    })
    
    showNotification("✅ Batch processing complete. Results saved in 'results_batch/' folder.", type = "message")
  })
  
  ## Test ----
  
  # Function to project reference centroids on contour data and calculate intensity around each centroid
  project_centroids_with_intensity <- function(reference_centroids, result_list, output_dir = "centroid_exports", eps = input$eps_value/2) {
    if (!dir.exists(output_dir)) dir.create(output_dir)  # Create output directory if missing
    
    for (name in names(result_list)) {
      result <- result_list[[name]]
      if (is.null(result$contour_data)) next  # Skip if no contour data
      
      contour_data <- result$contour_data
      
      # Calculate intensity near each centroid by summing contour levels within eps distance
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
      
      # Extract folder/spectrum name to use in output filename
      subfolder_name <- basename(name)
      output_file <- file.path(output_dir, paste0(subfolder_name, "_projected_centroids.csv"))
      
      # Export projected centroids with intensity to CSV
      readr::write_csv(projected_centroids, output_file)
    }
    
    showNotification("✅ Centroids projected and exported with intensity.", type = "message")
  }
  
  observeEvent(input$export_projected_centroids, {
    req(centroids_data())  # Reference centroids required
    req(result_data_list())  # List of processed results with contour data
    
    project_centroids_with_intensity(
      reference_centroids = centroids_data(),
      result_list = result_data_list(),
      output_dir = "centroid_exports",  # Output folder for exported files
      eps = input$eps_value %||% 0.04  # Epsilon radius for projection (default fallback)
    )
  })

  
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
  
}

shinyApp(ui = ui, server = server)
