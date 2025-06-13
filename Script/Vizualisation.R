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

## Identify local maxima ----

library(dplyr)
library(tidyr)

process_nmr_centroids_localmax <- function(contour_data, neighborhood_size = 3, min_intensity = NULL) {
  # contour_data doit contenir au minimum: x, y, level
  
  if (nrow(contour_data) == 0) {
    stop("contour_data est vide")
  }
  
  # Créer une grille complète des points pour rechercher les maxima locaux
  # On pivotera pour une matrice avec colonnes = unique(x), lignes = unique(y)
  grid_df <- contour_data %>%
    select(x, y, level) %>%
    pivot_wider(names_from = x, values_from = level)
  
  # Récupérer les vecteurs x et y uniques (ordonnés)
  unique_x <- sort(unique(contour_data$x))
  unique_y <- sort(unique(contour_data$y))
  
  # Construire une matrice des intensités
  intensity_matrix <- as.matrix(grid_df[,-1])  # enlever colonne y (pivot_wider conserve y en 1ère col)
  rownames(intensity_matrix) <- grid_df$y
  
  # Padding de la matrice pour gérer bords
  pad <- floor(neighborhood_size / 2)
  padded_mat <- matrix(NA, 
                       nrow = nrow(intensity_matrix) + 2 * pad, 
                       ncol = ncol(intensity_matrix) + 2 * pad)
  padded_mat[(pad+1):(pad+nrow(intensity_matrix)), (pad+1):(pad+ncol(intensity_matrix))] <- intensity_matrix
  
  maxima_coords <- list()
  
  # Fonction pour vérifier si un point est un maximum local
  is_local_max <- function(i, j, mat, neighborhood_size) {
    neighborhood <- mat[(i - pad):(i + pad), (j - pad):(j + pad)]
    center_value <- mat[i, j]
    if (is.na(center_value)) return(FALSE)
    all(center_value >= neighborhood, na.rm = TRUE) && any(center_value > neighborhood, na.rm = TRUE)
  }
  
  # Balayage de la matrice sans les bords
  for (i in (pad+1):(nrow(padded_mat)-pad)) {
    for (j in (pad+1):(ncol(padded_mat)-pad)) {
      if (is_local_max(i, j, padded_mat, neighborhood_size)) {
        # Récupérer coords x,y correspondants
        x_val <- unique_x[j - pad]
        y_val <- unique_y[i - pad]
        maxima_coords <- append(maxima_coords, list(c(x_val, y_val, padded_mat[i,j])))
      }
    }
  }
  
  if (length(maxima_coords) == 0) {
    warning("Aucun maximum local trouvé")
    return(list(centroids = NULL, bounding_boxes = NULL))
  }
  
  maxima_df <- do.call(rbind, maxima_coords) %>%
    as.data.frame()
  colnames(maxima_df) <- c("x", "y", "intensity")
  
  # Filtrer par intensity si demandé
  if (!is.null(min_intensity)) {
    maxima_df <- maxima_df %>% filter(intensity >= min_intensity)
  }
  
  # Pour chaque maximum local, définir une boîte englobante autour des points proches
  bounding_boxes <- maxima_df %>% rowwise() %>%
    do({
      cx <- .$x
      cy <- .$y
      # Définir un rayon autour du maximum (ex: ±0.01 ppm ou adapté selon données)
      radius <- 0.01
      
      nearby_points <- contour_data %>%
        filter(abs(x - cx) <= radius, abs(y - cy) <= radius)
      
      if (nrow(nearby_points) == 0) {
        # Au moins une petite boîte autour du centroïde
        tibble(
          xmin = cx - radius, xmax = cx + radius,
          ymin = cy - radius, ymax = cy + radius,
          intensity = .$intensity
        )
      } else {
        tibble(
          xmin = min(nearby_points$x),
          xmax = max(nearby_points$x),
          ymin = min(nearby_points$y),
          ymax = max(nearby_points$y),
          intensity = sum(nearby_points$level)
        )
      }
    }) %>% ungroup()
  
  # Renommer colonnes pour cohérence avec ta fonction
  centroids <- maxima_df %>%
    rename(F2_ppm = x, F1_ppm = y, stain_intensity = intensity)
  
  # Inverser axes pour correspondre à convention NMR (optionnel)
  centroids <- centroids %>%
    mutate(
      F2_ppm = -F2_ppm,
      F1_ppm = -F1_ppm
    )
  
  bounding_boxes <- bounding_boxes %>%
    mutate(
      xmin = -xmax,
      xmax = -xmin,
      ymin = -ymax,
      ymax = -ymin
    )
  
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


make_bbox_outline <- function(bbox_df) {
  do.call(rbind, lapply(1:nrow(bbox_df), function(i) {
    row <- bbox_df[i, ]
    data.frame(
      stain_id = row$stain_id,
      x = c(row$xmin, row$xmax, row$xmax, row$xmin, row$xmin),
      y = c(row$ymin, row$ymin, row$ymax, row$ymax, row$ymin),
      group = row$stain_id,
      intensity = row$stain_intensity,
      stringsAsFactors = FALSE
    )
  }))
}