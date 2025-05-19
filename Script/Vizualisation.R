# Functional function ----

library(Matrix)
library(ggplot2)
library(data.table)
library(dbscan)
library(FNN)
library(zoo)
library(stringr)  # For extracting stain IDs

## Peak pick ----

# Function for displaying contours
find_nmr_peak_centroids <- function(rr_data, spectrum_type = NULL, 
                                    contour_start = NULL, intensity_threshold = NULL, 
                                    contour_num = NULL, contour_factor = NULL, 
                                    zoom_xlim = NULL, zoom_ylim = NULL, 
                                    f2_exclude_range = NULL) {
  if (is.null(rr_data) || !is.matrix(rr_data)) {
    stop("Invalid Bruker data. Ensure rr_data is a matrix with proper intensity values.")
  }
  
  # Default values based on spectrum type
  spectrum_defaults <- list(
    HSQC = list(contour_start = 8000, intensity_threshold = 200, contour_num = 8, contour_factor = 1.3),
    TOCSY = list(contour_start = 100000, intensity_threshold = 4000 , contour_num = 110, contour_factor = 1.3, f2_exclude_range = c(4.7, 5.0)),
    COSY  = list(contour_start = 1000, intensity_threshold = 20000, contour_num = 60, contour_factor = 1.3)
  )
  
  if (!is.null(spectrum_type)) {
    if (!spectrum_type %in% names(spectrum_defaults)) {
      stop("Invalid spectrum_type. Choose from 'HSQC', 'TOCSY', or 'COSY'.")
    }
    defaults <- spectrum_defaults[[spectrum_type]]
    
    contour_start <- ifelse(is.null(contour_start), defaults$contour_start, contour_start)
    intensity_threshold <- ifelse(is.null(intensity_threshold), defaults$intensity_threshold, intensity_threshold)
    contour_num <- ifelse(is.null(contour_num), defaults$contour_num, contour_num)
    contour_factor <- ifelse(is.null(contour_factor), defaults$contour_factor, contour_factor)
  }
  
  ppm_x <- as.numeric(rownames(rr_data))
  ppm_y <- as.numeric(colnames(rr_data))
  
  intensity_df <- expand.grid(ppm_x = ppm_x, ppm_y = ppm_y)
  intensity_df$intensity <- as.vector(rr_data)
  
  if (!is.null(f2_exclude_range) && length(f2_exclude_range) == 2) {
    intensity_df <- intensity_df %>%
      filter(!(ppm_y >= f2_exclude_range[1] & ppm_y <= f2_exclude_range[2]))
  }
  
  intensity_df <- intensity_df[intensity_df$intensity >= intensity_threshold, ]
  
  contour_levels <- contour_start * contour_factor^(0:(contour_num - 1))
  
  p <- ggplot(intensity_df, aes(x = ppm_y, y = ppm_x, z = intensity)) +
    geom_contour(color = "black", breaks = contour_levels) + 
    scale_x_reverse() +
    scale_y_reverse() +
    labs(y = "F1_ppm", x = "F2_ppm") +
    theme_minimal()
  
  contour_data <- ggplot_build(p)$data[[1]]
  
  return(list( plot = p, contour_data = contour_data))
}


process_nmr_centroids <- function(rr_data, contour_data, contour_num = NULL, contour_factor = NULL, 
                                  intensity_threshold = NULL, keep_peak_ranges = NULL, eps_value = NULL) {
  
  # Vérifie que des contours sont disponibles
  if (nrow(contour_data) == 0 || !"group" %in% colnames(contour_data)) {
    stop("No contours data available for processing.")
  }
  
  # Normalisation
  contour_data <- contour_data %>%
    mutate(
      x_scaled = (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE),
      y_scaled = (y - mean(y, na.rm = TRUE)) / sd(y, na.rm = TRUE)
    )
  
  # Clustering DBSCAN
  clusters <- dbscan::dbscan(contour_data[, c("x_scaled", "y_scaled")], eps = eps_value, minPts = 0)
  contour_data$stain_id <- as.character(clusters$cluster)
  
  # Calcul des centroïdes
  centroids <- contour_data %>%
    group_by(stain_id) %>%
    summarise(
      F2_ppm = mean(x, na.rm = TRUE),
      F1_ppm = mean(y, na.rm = TRUE),
      stain_intensity = sum(level, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Inversion des ppm
  centroids$F2_ppm <- -centroids$F2_ppm
  centroids$F1_ppm <- -centroids$F1_ppm
  
  # Calcul des boîtes englobantes
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
  
  # Suppression des centroïdes de faible intensité à l'intérieur des boîtes englobantes d'autres pics
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
  
  # Optionnel : conserver les pics dans des plages définies
  if (!is.null(keep_peak_ranges) && is.list(keep_peak_ranges)) {
    first_range <- TRUE
    for (range in keep_peak_ranges) {
      if (length(range) == 2) {
        centroids_in_range <- centroids %>%
          filter(F2_ppm >= range[2] & F2_ppm <= range[1])
        
        num_peaks_to_keep <- if (first_range) 2 else 2
        first_range <- FALSE
        
        top_peaks_in_range <- centroids_in_range %>%
          arrange(desc(stain_intensity)) %>%
          slice_head(n = num_peaks_to_keep)
        
        centroids <- centroids %>%
          filter(!(F2_ppm >= range[2] & F2_ppm <= range[1])) %>%
          bind_rows(top_peaks_in_range)
      }
    }
  }
  
  return(list(
    centroids = centroids,
    bounding_boxes = bounding_boxes
  ))
}



## Intensité Locale ----

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

seuil_bruit_multiplicatif <- function(mat, facteur = 3) {
  bruit_estime <- sd(as.numeric(mat), na.rm = TRUE)
  seuil <- bruit_estime * facteur
  return(seuil)
}

seuil_max_pourcentage <- function(mat, pourcentage = 0.05) {
  max_val <- max(mat, na.rm = TRUE)
  seuil <- max_val * pourcentage
  return(seuil)
}

modulate_threshold <- function(VI) {
  a <- 0.0006
  b <- 1.2
  a * VI^b
}