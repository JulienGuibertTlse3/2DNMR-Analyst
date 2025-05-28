# 2D NMR Peak Picking and Analysis ----


# This script defines two main functions:
# 1. find_nmr_peak_centroids: Generates a contour plot from a 2D NMR matrix and extracts contour data.
# 2. process_nmr_centroids: Applies clustering to the contour lines to define peak centroids and bounding boxes.
# Additionally, it includes functions to estimate signal thresholds and local intensity.


## ---- Required libraries ----
library(ggplot2)    # for contour plotting
library(data.table) # for efficient data manipulation
library(dplyr)      # for data wrangling (group_by, summarise, etc.)
library(dbscan)     # for DBSCAN clustering
library(magrittr)   # for piping (%>%)

# ---- Function: find_nmr_peak_centroids ----
# Purpose: Display 2D NMR contours and extract contour data for clustering
find_nmr_peak_centroids <- function(rr_data, spectrum_type = NULL, 
                                    contour_start = NULL, intensity_threshold = NULL, 
                                    contour_num = NULL, contour_factor = NULL, 
                                    zoom_xlim = NULL, zoom_ylim = NULL, 
                                    f2_exclude_range = NULL) {
  
  # Validate input data: must be a numeric matrix
  if (is.null(rr_data) || !is.matrix(rr_data)) {
    stop("Invalid Bruker data. Ensure rr_data is a matrix with proper intensity values.")
  }
  
  # Define default contour parameters for different spectrum types
  spectrum_defaults <- list(
    HSQC = list(contour_start = 8000, intensity_threshold = 200, contour_num = 8, contour_factor = 1.3),
    TOCSY = list(contour_start = 100000, intensity_threshold = 4000 , contour_num = 110, contour_factor = 1.3, f2_exclude_range = c(4.7, 5.0)),
    UFCOSY = list(contour_start = 1000, intensity_threshold = 20000, contour_num = 60, contour_factor = 1.3),
    COSY   = list(contour_start = 1000, intensity_threshold = 20000, contour_num = 60, contour_factor = 1.3)
  )
  
  # Apply defaults if spectrum type is recognized and user did not override parameters
  if (!is.null(spectrum_type)) {
    if (!spectrum_type %in% names(spectrum_defaults)) {
      stop("Invalid spectrum_type. Choose from 'HSQC', 'TOCSY', 'COSY' or 'UFCOSY'.")
    }
    defaults <- spectrum_defaults[[spectrum_type]]
    contour_start <- ifelse(is.null(contour_start), defaults$contour_start, contour_start)
    intensity_threshold <- ifelse(is.null(intensity_threshold), defaults$intensity_threshold, intensity_threshold)
    contour_num <- ifelse(is.null(contour_num), defaults$contour_num, contour_num)
    contour_factor <- ifelse(is.null(contour_factor), defaults$contour_factor, contour_factor)
  }
  
  # Extract the ppm axes from the matrix row/column names
  ppm_x <- as.numeric(rownames(rr_data))  # F1 dimension
  ppm_y <- as.numeric(colnames(rr_data))  # F2 dimension
  
  # Create a data.frame from matrix for ggplot compatibility
  intensity_df <- expand.grid(ppm_x = ppm_x, ppm_y = ppm_y)
  intensity_df$intensity <- as.vector(rr_data)
  
  # Optional: remove water region or other excluded ppm range in F2
  if (!is.null(f2_exclude_range) && length(f2_exclude_range) == 2) {
    intensity_df <- intensity_df %>%
      filter(!(ppm_y >= f2_exclude_range[1] & ppm_y <= f2_exclude_range[2]))
  }
  
  # Filter out points below the intensity threshold
  intensity_df <- intensity_df[intensity_df$intensity >= intensity_threshold, ]
  
  # Calculate contour levels exponentially
  contour_levels <- contour_start * contour_factor^(0:(contour_num - 1))
  
  # Generate the contour plot using ggplot2
  p <- ggplot(intensity_df, aes(x = ppm_y, y = ppm_x, z = intensity)) +
    geom_contour(color = "black", breaks = contour_levels) + 
    scale_x_reverse() +
    scale_y_reverse() +
    labs(y = "F1_ppm", x = "F2_ppm") +
    theme_minimal()
  
  # Extract contour data from the plot for further processing
  contour_data <- ggplot_build(p)$data[[1]]
  
  # Return both the plot and the extracted contour data
  return(list(plot = p, contour_data = contour_data))
}

## Identify peaks ----

process_nmr_centroids <- function(rr_data, contour_data, contour_num = NULL, contour_factor = NULL, 
                                  intensity_threshold = NULL, keep_peak_ranges = NULL, eps_value = NULL) {
  
  # Check if contour data is available and well-formed
  if (nrow(contour_data) == 0 || !"group" %in% colnames(contour_data)) {
    stop("No contours data available for processing.")
  }
  
  # Normalize contour coordinates for clustering
  contour_data <- contour_data %>%
    mutate(
      x_scaled = (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE),
      y_scaled = (y - mean(y, na.rm = TRUE)) / sd(y, na.rm = TRUE)
    )
  
  # Apply DBSCAN clustering to contour points
  clusters <- dbscan::dbscan(contour_data[, c("x_scaled", "y_scaled")], eps = eps_value, minPts = 0)
  contour_data$stain_id <- as.character(clusters$cluster)
  
  # Calculate centroids for each cluster
  centroids <- contour_data %>%
    group_by(stain_id) %>%
    summarise(
      F2_ppm = mean(x, na.rm = TRUE),
      F1_ppm = mean(y, na.rm = TRUE),
      stain_intensity = sum(level, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Invert ppm axes to match NMR convention
  centroids$F2_ppm <- -centroids$F2_ppm
  centroids$F1_ppm <- -centroids$F1_ppm
  
  # Create bounding boxes for each cluster
  bounding_boxes <- contour_data %>%
    group_by(stain_id) %>%
    summarise(
      xmin = min(-x, na.rm = TRUE),
      xmax = max(-x, na.rm = TRUE),
      ymin = min(-y, na.rm = TRUE),
      ymax = max(-y, na.rm = TRUE),
      intensity = sum(level, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Remove weaker centroids that fall inside bounding boxes of stronger ones
  if (nrow(centroids) > 1) {
    centroids_with_boxes <- centroids %>%
      left_join(bounding_boxes, by = "stain_id")
    
    to_remove <- c()
    
    for (i in seq_len(nrow(centroids_with_boxes))) {
      current <- centroids_with_boxes[i, ]
      
      for (j in seq_len(nrow(centroids_with_boxes))) {
        if (i == j) next
        
        compare <- centroids_with_boxes[j, ]
        
        inside_box <- current$F2_ppm >= compare$xmin & current$F2_ppm <= compare$xmax &
          current$F1_ppm >= compare$ymin & current$F1_ppm <= compare$ymax
        lower_intensity <- current$stain_intensity < compare$intensity
        
        if (inside_box && lower_intensity) {
          to_remove <- c(to_remove, current$stain_id)
          break
        }
      }
    }
    
    centroids <- centroids %>% filter(!stain_id %in% to_remove)
    bounding_boxes <- bounding_boxes %>% filter(!stain_id %in% to_remove)
  }
  
  # Optional: Keep only strongest peaks in user-defined ppm regions
  if (!is.null(keep_peak_ranges) && is.list(keep_peak_ranges)) {
    for (i in seq_along(keep_peak_ranges)) {
      range <- keep_peak_ranges[[i]]
      
      if (length(range) == 2) {
        centroids_in_range <- centroids %>%
          filter(F2_ppm >= range[2] & F2_ppm <= range[1])
        
        num_peaks_to_keep <- if (i <= 3) 2 else 10
        
        top_peaks_in_range <- centroids_in_range %>%
          arrange(desc(stain_intensity)) %>%
          slice_head(n = num_peaks_to_keep)
        
        centroids <- centroids %>%
          filter(!(F2_ppm >= range[2] & F2_ppm <= range[1])) %>%
          bind_rows(top_peaks_in_range)
      }
    }
  }
  
  # Optional: Keep only strongest boxes in defined regions
  if (!is.null(keep_peak_ranges) && is.list(keep_peak_ranges)) {
    for (i in seq_along(keep_peak_ranges)) {
      range <- keep_peak_ranges[[i]]
      
      if (length(range) == 2) {
        boxes_in_range <- bounding_boxes %>%
          filter(xmin >= range[2] & xmax <= range[1])
        
        num_boxes_to_keep <- if (i <= 3) 2 else 10
        
        top_boxes_in_range <- boxes_in_range %>%
          arrange(desc(intensity)) %>%
          slice_head(n = num_boxes_to_keep)
        
        bounding_boxes <- bounding_boxes %>%
          filter(!(xmin >= range[2] & xmax <= range[1])) %>%
          bind_rows(top_boxes_in_range)
      }
    }
  }
  
  # Return centroids and their bounding boxes
  return(list(
    centroids = centroids,
    bounding_boxes = bounding_boxes
  ))
}



# Compute local contour intensity around a point (stain)
get_local_stain_intensity <- function(f2_ppm, f1_ppm, contour_data, eps_ppm = 0.004) {
  local_points <- contour_data %>%
    filter(
      x >= f2_ppm - eps_ppm & x <= f2_ppm + eps_ppm,
      y >= f1_ppm - eps_ppm & y <= f1_ppm + eps_ppm
    )
  
  if (nrow(local_points) == 0) {
    return(NA)
  }
  
  return(sum(local_points$level, na.rm = TRUE))
}



## Seuil Bruit ----

# Threshold based on standard deviation * factor (default = 3)
seuil_bruit_multiplicatif <- function(mat, facteur = 3) {
  bruit_estime <- sd(as.numeric(mat), na.rm = TRUE)
  seuil <- bruit_estime * facteur
  return(seuil)
}

# Threshold based on percentage of the max intensity (default = 5%)
seuil_max_pourcentage <- function(mat, pourcentage = 0.05) {
  max_val <- max(mat, na.rm = TRUE)
  seuil <- max_val * pourcentage
  return(seuil)
}

# Custom threshold modulation based on intensity (VI)
modulate_threshold <- function(VI) {
  a <- 0.0006
  b <- 1.2
  a * VI^b
}
