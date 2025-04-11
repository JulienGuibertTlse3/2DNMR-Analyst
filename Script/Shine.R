library(shiny)
library(shinyFiles)
library(plotly)
library(ggplot2)
library(DT)
library(shinycssloaders)
library(shinyjs)
library(dplyr)

source("C://Users//juguibert//Documents//Function_test//Read_2DNMR_spectrum.R")
source("C://Users//juguibert//Documents//Function_test//Vizualisation.R")

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
                         div(tags$h4("Browse Data Folder"), style = "font-size: 18px; font-weight: 700;"),
                         shinyDirButton("directory", "Select Main Directory", "Select Directory"),
                         verbatimTextOutput("selected_dir"),
                         uiOutput("subfolder_selector"),
                         actionButton("load_data", "📂 Load Data"),
                         withSpinner(verbatimTextOutput("matrix_dim"), type = 4, color = "#007bff"),
                         actionButton("generate_plot", "📊 Generate Plot"),
                         actionButton("generate_centroids", "Generate Centroids and Bounding Boxes"),
                         textOutput("status_message"),
                         tags$hr(),
                         
                         
                         # Button to toggle visibility
                         actionButton("toggle_centroid_section", "Ajouter/Supprimer un centroïde"),
                         # Section for adding and deleting centroids (hidden by default)
                         hidden(div(
                           id = "centroid_section",
                           class = "centroid-section",
                           tags$h4("➕ Ajouter un centroïde manuellement"),
                           numericInput("manual_f2", "F2 ppm (x) :", value = 4.0, step = 0.01),
                           numericInput("manual_f1", "F1 ppm (y) :", value = 3.5, step = 0.01),
                           actionButton("add_manual_centroid", "Ajouter le centroïde 🔵")
                         )),
                         hidden(div(
                           id = "delete_centroid_section",
                           class = "centroid-section",
                           tags$h4("❌ Supprimer un centroïde"),
                           DTOutput("centroid_table"),
                           actionButton("delete_centroid", "Supprimer le centroïde sélectionné 🗑️")
                         )),
                         
                         actionButton("export_centroids", "Exporter les centroïdes"),
                         verbatimTextOutput("centroids_output"),
                         tags$hr(),
                         tags$h4("📥 Importer des centroïdes"),
                         fileInput("import_centroids", "Importer un fichier CSV :", accept = ".csv")
                  ),
                  
                  column(9, 
                         div(id = "loading_message", "Generating plot, please wait...", style = "font-size: 18px; color: blue; font-weight: bold; display: none;"),
                         div(id = "export_loading_message", "Exporting centroids, please wait...", style = "font-size: 18px; color: blue; font-weight: bold; display: none;"),
                         withSpinner(div(id = "interactivePlot", plotlyOutput("interactivePlot", height = "100%", width = "100%")))
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
  
  bruker_data <- reactiveVal(NULL)
  result_data <- reactiveVal(NULL)
  centroids_data <- reactiveVal(NULL)
  bounding_boxes_data <- reactiveVal(NULL)
  contour_plot_base <- reactiveVal(NULL)
  nmr_plot <- reactiveVal(NULL)
  status_msg <- reactiveVal("")
  bounding_boxes_data <- reactiveVal(data.frame(xmin = numeric(0), xmax = numeric(0), ymin = numeric(0), ymax = numeric(0)))
  
  output$matrix_dim <- renderPrint({ req(bruker_data()); dim(bruker_data()$spectrumData) })
  output$status_message <- renderText({ status_msg() })
  
  ## File Loading ----
  
  roots <- c(Home = normalizePath("~"), Root = "/")
  shinyDirChoose(input, "directory", roots = roots, session = session)
  
  main_directory <- reactive({
    req(input$directory)
    selected_dir <- parseDirPath(roots, input$directory)
    normalizePath(selected_dir)
  })
  
  output$selected_dir <- renderPrint({ main_directory() })
  
  subfolders <- reactive({
    req(main_directory())
    all_subfolders <- list.dirs(main_directory(), recursive = TRUE, full.names = TRUE)
    all_subfolders[sapply(all_subfolders, function(folder) {
      file.exists(file.path(folder, "acqus")) &&
        (file.exists(file.path(folder, "ser")) || file.exists(file.path(folder, "fid")))
    })]
  })
  
  output$subfolder_selector <- renderUI({
    req(subfolders())
    selectInput("selected_subfolder", "Select a Bruker Subfolder:", choices = subfolders())
  })
  
  output$subfolder_path <- renderUI({
    req(input$selected_subfolder)
    div(id = "subfolder-path", textOutput("selected_subfolder"))
  })
  
  observeEvent(input$load_data, {
    req(input$selected_subfolder)
    data_path <- file.path(input$selected_subfolder, "pdata", "1")
    status_msg("🔄 Chargement des données...")
    
    if (!dir.exists(data_path)) {
      showNotification(paste("Data path does not exist:", data_path), type = "error")
      status_msg("❌ Échec du chargement")
      return(NULL)
    }
    
    data <- tryCatch(read_bruker(data_path, dim = "2D"), error = function(e) {
      showNotification(paste("Erreur chargement:", e$message), type = "error")
      status_msg("❌ Échec du chargement")
      return(NULL)
    })
    
    if (!is.null(data)) {
      bruker_data(data)
      showNotification("✅ Données chargées", type = "message")
      status_msg("✅ Données chargées")
    }
  })
  
  
  ## Refresh plot when modification are made ----
  
  refresh_nmr_plot <- function() {
    req(contour_plot_base())
    plot <- contour_plot_base()
    
    # Add bounding boxes
    boxes <- bounding_boxes_data()
    if (!is.null(boxes) && nrow(boxes) > 0) {
      plot <- plot +
        geom_rect(data = boxes, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  color = "red", fill = NA, linetype = "dashed", inherit.aes = FALSE)
    }
    
    # Add centroids
    centroids <- centroids_data()
    if (!is.null(centroids) && nrow(centroids) > 0) {
      plot <- plot +
        geom_point(data = centroids, aes(x = F2_ppm, y = F1_ppm, color = stain_intensity),
                   size = 0.5, inherit.aes = FALSE) +
        scale_color_gradient(low = "blue", high = "green")
    }
    
    nmr_plot(plot)
  }
  
  ## Generate Plot ----
  
  observeEvent(input$generate_plot, {
    req(bruker_data())
    shinyjs::show("loading_message")
    status_msg("🔄 Génération du graphique...")
    
    result <- tryCatch({
      find_nmr_peak_centroids(
        bruker_data()$spectrumData,
        spectrum_type = "TOCSY",
        intensity_threshold = 30000,
        contour_start = 100000,
        contour_num = 110,
        contour_factor = 1.3,
        f2_exclude_range = c(4.7, 5.0)
      )
    }, error = function(e) {
      showNotification(paste("Erreur de traitement :", e$message), type = "error")
      shinyjs::hide("loading_message")
      return(NULL)
    })
    
    if (!is.null(result)) {
      result_data(result)
      contour_plot_base(result$plot + labs(title = ""))
      refresh_nmr_plot()
      showNotification("✅ Graphique générés", type = "message")
      status_msg("✅ Analyse terminée")
    }
    
    shinyjs::hide("loading_message")
  })
  
  
  ## Generate Centroids ----
  
  observeEvent(input$generate_centroids, {
    req(bruker_data())
    result <- result_data()
    req(result)
    shinyjs::show("loading_message")
    status_msg("🔄 Génération des centroïdes et BB...")
    
    result1 <- tryCatch({
      process_nmr_centroids(
        rr_data = bruker_data()$spectrumData, 
        contour_data = result$contour_data, 
        contour_num = 110,
        contour_factor = 1.3,
        intensity_threshold = 30000
      )
    }, error = function(e) {
      showNotification(paste("Erreur de traitement :", e$message), type = "error")
      shinyjs::hide("loading_message")
      return(NULL)
    })
    
    if (!is.null(result1)) {
      centroids_data(result1$centroids)
      bounding_boxes_data(result1$bounding_boxes)
      refresh_nmr_plot()
      showNotification("✅ Centroïdes et BBs calculés", type = "message")
      status_msg("✅ Analyse terminée")
    }
    
    shinyjs::hide("loading_message")
  })
  
  ## Manually Add Centroids ----
  
  observeEvent(input$add_manual_centroid, {
    req(input$manual_f2, input$manual_f1)
    current <- centroids_data()
    if (is.null(current)) current <- data.frame(F2_ppm = numeric(0), F1_ppm = numeric(0), stain_intensity = numeric(0), stain_id = character(0))
    
    existing_ids <- current$stain_id[grepl("^man", current$stain_id)]
    man_number <- if (length(existing_ids) == 0) 1 else max(as.integer(sub("man", "", existing_ids)), na.rm = TRUE) + 1
    
    # === Estimate intensity from contour_data
    contour_data <- result_data()$contour_data
    eps <- 0.02  # or any value you used for clustering
    
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
  
  ## Manually Delete centroids ----
  
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
  
  ## Dataframes ----
  
  # Dans le server
  output$centroid_table <- renderDT({
    centroids_filtered <- centroids_data()[, 1:3]  # Afficher seulement les 3 premières colonnes
    datatable(centroids_filtered, selection = "single", options = list(pageLength = 5))
  })
  
  output$full_centroid_table <- renderDT({
    # Afficher toutes les colonnes
    datatable(centroids_data(), selection = "single", options = list(pageLength = 5))
  })
  
  ## Interactive Plot ----
  
  output$interactivePlot <- renderPlotly({
    req(nmr_plot())
    ggplotly(nmr_plot(), source = "nmr_plot")
  })
  
  ## Export Centroids ----
  
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
  
  
  ## Import centroid list ----
  
  observeEvent(input$import_centroids, {
    req(input$import_centroids)
    
    tryCatch({
      new_centroids <- read.csv(input$import_centroids$datapath)
      centroids_data(rbind(centroids_data(), new_centroids))
      refresh_nmr_plot()
      showNotification("Centroïdes importés", type = "message")
    }, error = function(e) {
      showNotification(paste("Erreur lors de l'importation:", e$message), type = "error")
    })
  })
}

shinyApp(ui = ui, server = server)


