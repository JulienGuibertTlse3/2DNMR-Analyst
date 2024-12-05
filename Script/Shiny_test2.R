library(shiny)
library(shinyFiles)

# UI
ui <- fluidPage(
  titlePanel("Bruker 2D NMR Spectrum Viewer"),
  sidebarLayout(
    sidebarPanel(
      div(
        tags$span("Browse Data Folder", style = "font-size: 18px;font-weight: 700;"),
        tags$p(
          "Select a directory containing Bruker 2D NMR data folders (with 'acqus' and 'ser/fid' files).",
          style = "font-size: 14px;"
        )
      ),
      shinyDirButton("directory", "Select Main Directory", "Select Directory"),
      verbatimTextOutput("selected_dir"),
      uiOutput("subfolder_selector"),
      tags$hr(),
      sliderInput("threshold", "Intensity Threshold (%):", min = 0, max = 100, value = 50, step = 5),
      checkboxInput("reverse_axes", "Reverse Axes Display", value = TRUE),
      actionButton("plot", "Generate Plot")
    ),
    mainPanel(
      plotOutput("nmr_plot", height = "800px", width = "800px")
    )
  )
)

server <- function(input, output, session) {
  # Allow folder browsing
  roots <- c(Home = normalizePath("~"), Root = "/")
  shinyDirChoose(input, "directory", roots = roots, session = session)
  
  # Reactive value to store the selected directory path
  main_directory <- reactive({
    req(input$directory)
    selected_dir <- parseDirPath(roots, input$directory)
    normalizePath(selected_dir)
  })
  
  # Display the selected directory path
  output$selected_dir <- renderPrint({
    req(main_directory())
    main_directory()
  })
  
  # Dynamically find subfolders within the selected directory
  subfolders <- reactive({
    req(main_directory())
    all_subfolders <- list.dirs(main_directory(), recursive = TRUE, full.names = TRUE)
    valid_subfolders <- all_subfolders[sapply(all_subfolders, function(folder) {
      file.exists(file.path(folder, "acqus")) &&
        (file.exists(file.path(folder, "ser")) || file.exists(file.path(folder, "fid")))
    })]
    if (length(valid_subfolders) == 0) {
      showNotification("No valid Bruker subfolders found!", type = "warning")
    }
    valid_subfolders
  })
  
  # Generate dropdown for selecting subfolders
  output$subfolder_selector <- renderUI({
    req(subfolders())
    selectInput("selected_subfolder", "Select a Bruker Subfolder:", choices = subfolders())
  })
  
  # Reactive value to store the Bruker data
  bruker_data <- reactiveVal()
  
  # Process the selected subfolder and generate the plot
  observeEvent(input$plot, {
    req(input$selected_subfolder)
    folder_path <- input$selected_subfolder
    
    tryCatch({
      # Load the Bruker data using the `read_bruker_file` function
      data <- read_bruker_file(folder_path)
      
      # Extract and process the data
      intensity_matrix <- t(data$rr_data)  # Transpose the matrix
      w1_ppm <- data$ppm_y
      w2_ppm <- data$ppm_x
      
      print(dim(intensity_matrix))  # Rows and columns of the matrix
      print(length(data$ppm_x))         # Length of the x-axis (F2)
      print(length(data$ppm_y))
      
      # Ensure axis lengths match matrix dimensions
      if (length(w1_ppm) != nrow(intensity_matrix)) {
        w1_ppm <- seq(min(w1_ppm), max(w1_ppm), length.out = nrow(intensity_matrix))
      }
      if (length(w2_ppm) != ncol(intensity_matrix)) {
        w2_ppm <- seq(min(w2_ppm), max(w2_ppm), length.out = ncol(intensity_matrix))
      }
      
      # Apply intensity threshold
      threshold_value <- quantile(intensity_matrix, probs = input$threshold / 100, na.rm = TRUE)
      intensity_matrix[intensity_matrix < threshold_value] <- threshold_value  # Replace below-threshold values
      
      # Reverse the matrix if axes need to be flipped
      if (input$reverse_axes) {
        intensity_matrix <- intensity_matrix[nrow(intensity_matrix):1, ncol(intensity_matrix):1]
      }
      
      print(dim(intensity_matrix))
      
      # Save processed data for visualization
      bruker_data(list(ppm_x = w2_ppm, ppm_y = w1_ppm, intensity_matrix = intensity_matrix))
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  output$nmr_plot <- renderPlot({
    req(bruker_data())
    data <- bruker_data()
    
    # Debugging dimensions
    print(dim(data$intensity_matrix))
    print(length(data$ppm_x))
    print(length(data$ppm_y))
    
    # Replace intensities ≤ 0 with a small positive value (e.g., 0.001)
    filtered_matrix <- intensity_matrix
    filtered_matrix[filtered_matrix <= 0] <- 0.001
    
    # Define red gradient for intensities from 0 to 100
    red_palette <- colorRampPalette(c("white", "red"))(100)  # Gradient of red
    
    # Use filled.contour for plotting
    filled.contour(
      x = w2_ppm, y = w1_ppm, z = filtered_matrix,
      color.palette = function(n) red_palette[seq_len(n)],
      plot.title = title(main = "2D NMR Spectrum (Intensities > 0)"),
      plot.axes = {
        axis(1)  # Bottom axis
        axis(2)  # Left axis
      },
      xlim = rev(range(data$ppm_x)),
      ylim = rev(range(data$ppm_y)),
      key.title = title(main = "Intensity")
    )
  })
}


# Run the app
shinyApp(ui = ui, server = server)
