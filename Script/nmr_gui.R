library(tcltk)

# Functions from your scripts
source("C://Users//juguibert//Documents//Function_test//Affichage.R")  # Bruker file loading and spectrum visualization
source("C://Users//juguibert//Documents//Function_test//Peak_picking.R")  # Peak picking
source("C://Users//juguibert//Documents//Function_test//Integration.R")  # Peak integration

# Global Variables to Store Data
bruker_data <- NULL
peaks <- NULL
integrated_peaks <- NULL
plot_canvas <- NULL

draw_spectrum <- function(canvas, bruker_data, peaks = NULL) {
  # Clear previous canvas content
  tkdelete(canvas, "all")
  
  if (is.null(bruker_data) || is.null(bruker_data$rr_data)) {
    tkmessageBox(title = "Error", message = "No spectrum data to display.")
    return()
  }
  
  intensity_matrix <- as.matrix(bruker_data$rr_data)
  ppm_x <- bruker_data$ppm_x  # Correct mapping: ppm_x -> columns
  ppm_y <- bruker_data$ppm_y  # Correct mapping: ppm_y -> rows
  
  # Validate dimensions
  if (ncol(intensity_matrix) != length(ppm_x) || nrow(intensity_matrix) != length(ppm_y)) {
    tkmessageBox(title = "Error", message = "Dimension mismatch between intensity matrix and ppm values.")
    return()
  }
  
  # Canvas dimensions
  canvas_width <- as.numeric(tkcget(canvas, "-width"))
  canvas_height <- as.numeric(tkcget(canvas, "-height"))
  
  x_scale <- canvas_width / (max(ppm_x) - min(ppm_x))
  y_scale <- canvas_height / (max(ppm_y) - min(ppm_y))
  
  # Draw axes
  tkcreate(canvas, "line", 50, canvas_height - 50, canvas_width - 50, canvas_height - 50, fill = "black")  # X-axis
  tkcreate(canvas, "line", 50, canvas_height - 50, 50, 50, fill = "black")  # Y-axis
  
  # Add numerical PPM ticks on the axes (reversed)
  x_ticks <- pretty(ppm_x, n = 10)
  y_ticks <- pretty(ppm_y, n = 10)
  
  for (x in x_ticks) {
    x_pos <- canvas_width - ((x - min(ppm_x)) * x_scale + 50)
    tkcreate(canvas, "line", x_pos, canvas_height - 55, x_pos, canvas_height - 45, fill = "black")
    tkcreate(canvas, "text", x_pos, canvas_height - 40, text = round(x, 1), anchor = "n", font = "Helvetica 10")
  }
  
  for (y in y_ticks) {
    y_pos <- ((y - min(ppm_y)) * y_scale + 50)
    tkcreate(canvas, "line", 45, y_pos, 55, y_pos, fill = "black")
    tkcreate(canvas, "text", 35, y_pos, text = round(y, 1), anchor = "e", font = "Helvetica 10")
  }
  
  # Plot peaks from the intensity matrix
  intensity_threshold <- 0.2  # Adjust the threshold for significant peaks
  for (i in seq_len(nrow(intensity_matrix))) {
    for (j in seq_len(ncol(intensity_matrix))) {
      if (intensity_matrix[i, j] > intensity_threshold) {
        peak_x <- canvas_width - ((ppm_x[j] - min(ppm_x)) * x_scale + 50)
        peak_y <- ((ppm_y[i] - min(ppm_y)) * y_scale + 50)
        tkcreate(canvas, "oval", peak_x - 3, peak_y - 3, peak_x + 3, peak_y + 3, fill = "blue", outline = "blue")
      }
    }
  }
  
  # Add provided peaks if available
  if (!is.null(peaks)) {
    for (k in seq_len(nrow(peaks))) {
      peak_x <- canvas_width - ((peaks$ppm_f2[k] - min(ppm_x)) * x_scale + 50)
      peak_y <- ((peaks$ppm_f1[k] - min(ppm_y)) * y_scale + 50)
      tkcreate(canvas, "oval", peak_x - 5, peak_y - 5, peak_x + 5, peak_y + 5, fill = "red", outline = "black")
    }
  }
}



load_bruker_file <- function(gui) {
  progress <- tkProgressBar(title = "Loading Bruker Data", min = 0, max = 100, width = 300)
  bruker_folder <- tclvalue(tkchooseDirectory(title = "Select Bruker Folder"))
  if (!is.null(bruker_folder) && bruker_folder != "") {
    tryCatch({
      setTkProgressBar(progress, 50, label = "Loading data...")
      bruker_data <<- read_bruker_file(bruker_folder)
      bruker_data$rr_data[!is.finite(bruker_data$rr_data)] <- 0
      finite_max = max(bruker_data$rr_data[is.finite(bruker_data$rr_data)], na.rm = TRUE)
      finite_min = min(bruker_data$rr_data[is.finite(bruker_data$rr_data)], na.rm = TRUE)
      
      # Replace Inf with the maximum finite value
      bruker_data$rr_data[bruker_data$rr_data == Inf] <- finite_max
      
      # Replace -Inf with the minimum finite value
      bruker_data$rr_data[bruker_data$rr_data == -Inf] <- finite_min
      
      # Debugging: Print loaded data structure
      print("Debugging Bruker Data:")
      print(str(bruker_data))
      print(summary(bruker_data))
      
      # Validate critical fields
      if (is.null(bruker_data$rr_data) || any(is.na(bruker_data$rr_data))) {
        close(progress)
        tkmessageBox(title = "Error", message = "Invalid rr_data in Bruker file.")
        return()
      }
      
      if (is.null(bruker_data$ppm_x) || any(is.na(bruker_data$ppm_x))) {
        close(progress)
        tkmessageBox(title = "Error", message = "Invalid ppm_x in Bruker file.")
        return()
      }
      
      if (is.null(bruker_data$ppm_y) || any(is.na(bruker_data$ppm_y))) {
        close(progress)
        tkmessageBox(title = "Error", message = "Invalid ppm_y in Bruker file.")
        return()
      }
      
      setTkProgressBar(progress, 100, label = "Data loaded successfully.")
      close(progress)
      tkmessageBox(title = "Success", message = "Bruker data loaded successfully.")
      draw_spectrum(plot_canvas, bruker_data)
    }, error = function(e) {
      close(progress)
      tkmessageBox(title = "Error", message = paste("Failed to load Bruker data:", e$message))
    })
  } else {
    close(progress)
    tkmessageBox(title = "Error", message = "No folder selected.")
  }
}


main_menu <- function() {
  gui <- tktoplevel()
  tkwm.title(gui, "NMR Analysis GUI")
  menu_bar <- tkmenu(gui)
  
  # File Menu
  file_menu <- tkmenu(menu_bar, tearoff = FALSE)
  tkadd(file_menu, "command", label = "Load Bruker File", command = function() load_bruker_file(gui))
  tkadd(menu_bar, "cascade", label = "File", menu = file_menu)
  
  # Analysis Menu
  analysis_menu <- tkmenu(menu_bar, tearoff = FALSE)
  tkadd(analysis_menu, "command", label = "Pick Peaks", command = pick_peaks)
  tkadd(analysis_menu, "command", label = "Integrate Peaks", command = integrate_peaks)
  tkadd(menu_bar, "cascade", label = "Analysis", menu = analysis_menu)
  
  # View Menu
  view_menu <- tkmenu(menu_bar, tearoff = FALSE)
  tkadd(view_menu, "command", label = "Visualize Spectrum", command = function() draw_spectrum(plot_canvas, bruker_data, peaks))
  tkadd(menu_bar, "cascade", label = "View", menu = view_menu)
  
  # Help Menu
  help_menu <- tkmenu(menu_bar, tearoff = FALSE)
  tkadd(help_menu, "command", label = "About", command = function() tkmessageBox(title = "About", message = "NMR Analysis GUI\nVersion 1.0\nDeveloped with user-specific functions."))
  tkadd(menu_bar, "cascade", label = "Help", menu = help_menu)
  
  tkconfigure(gui, menu = menu_bar)
  
  # Canvas for Visualization
  plot_canvas <<- tkcanvas(gui, width = 800, height = 600, bg = "white")
  tkpack(plot_canvas, side = "top", fill = "both", expand = TRUE)
}

# Launch the GUI
main_menu()
