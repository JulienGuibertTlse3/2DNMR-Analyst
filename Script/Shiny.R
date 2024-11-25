#### TEST (NOT WORKING) -----


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
      numericInput("threshold", "Intensity Threshold (%):", value = 50, min = 1, max = 100),
      actionButton("plot", "Generate Plot")
    ),
    mainPanel(
      plotOutput("nmr_plot", height = "800px", width = "800px")
    )
  )
)

# Server
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
  
  print(48)
  
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
  
  print(64)
  
  # Generate dropdown for selecting subfolders
  output$subfolder_selector <- renderUI({
    req(subfolders())
    selectInput("selected_subfolder", "Select a Bruker Subfolder:", choices = subfolders())
  })
  
  # Reactive value to store the Bruker data
  bruker_data <- reactiveVal()
  
  print(74)
  
  # Generate plot when the "plot" button is clicked
  observeEvent(input$plot, {
    req(input$selected_subfolder)
    folder_path <- input$selected_subfolder
    
    print(82)
    
    tryCatch({
      # Load the Bruker data using the `read_bruker_file` function
      data <- read_bruker_file(folder_path)
      
      print(88)
      

      # Create a reactive or local copy of threshold
      threshold_value <- as.numeric(input$threshold)
      if (is.na(threshold_value)) stop("Threshold must be a numeric value.")
      
      threshold_value <- as.double(threshold_value)  
      
      print(class(threshold_value))
      
      # Generate contours for the intensity matrix
      contours <- calculateContours(data$data, threshold_value)
      
      print(93)
      
      # Save processed data for visualization
      bruker_data(list(ppm_x = data$ppm_x, ppm_y = data$ppm_y, contours = contours))
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  print(102)
  
  # Render the 2D NMR plot
  output$nmr_plot <- renderPlot({
    req(bruker_data())
    data <- bruker_data()
    filled.contour(
      x = data$ppm_x, y = data$ppm_y, z = data$contours,
      color.palette = colorRampPalette(c("blue", "green", "yellow", "red")),
      plot.title = title(main = "2D NMR Spectrum"),
      plot.axes = { axis(1); axis(2) },
      key.title = title(main = "Intensity")
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)

