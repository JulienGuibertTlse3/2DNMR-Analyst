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


find_nmr_peak_centroids_optimized <- function(rr_data, spectrum_type = NULL, 
                                              contour_start = NULL, intensity_threshold = NULL, 
                                              contour_num = NULL, contour_factor = NULL, 
                                              zoom_xlim = NULL, zoom_ylim = NULL, 
                                              f2_exclude_range = NULL,
                                              downsample_factor = 2) {  # New parameter
  
  if (is.null(rr_data) || !is.matrix(rr_data)) {
    stop("Invalid Bruker data. Ensure rr_data is a matrix with proper intensity values.")
  }
  
  spectrum_defaults <- list(
    HSQC = list(contour_start = 8000, intensity_threshold = 200, contour_num = 8, contour_factor = 1.3),
    TOCSY = list(contour_start = 100000, intensity_threshold = 4000, contour_num = 40, contour_factor = 1.3, f2_exclude_range = c(4.7, 5.0)),
    UFCOSY = list(contour_start = 1000, intensity_threshold = 20000, contour_num = 40, contour_factor = 1.3),
    COSY = list(contour_start = 1000, intensity_threshold = 20000, contour_num = 60, contour_factor = 1.3)
  )
  
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
  
  # OPTIMIZATION 1: Downsampling the matrix for display
  if (downsample_factor > 1) {
    seq_x <- seq(1, nrow(rr_data), by = downsample_factor)
    seq_y <- seq(1, ncol(rr_data), by = downsample_factor)
    rr_data <- rr_data[seq_x, seq_y]
  }
  
  ppm_x <- as.numeric(rownames(rr_data))
  ppm_y <- as.numeric(colnames(rr_data))
  
  # OPTIMIZATION 2: Early filtering before expand.grid
  
  # Keep only indices where the intensity exceeds the threshold
  high_intensity_indices <- which(rr_data >= intensity_threshold, arr.ind = TRUE)
  
  if (nrow(high_intensity_indices) == 0) {
    warning("No data points above intensity threshold")
    return(list(plot = ggplot() + theme_void(), contour_data = data.frame()))
  }
  
  # OPTIMIZATION 3: Direct creation of the data.frame without expand.grid
  intensity_df <- data.table(
    ppm_x = ppm_x[high_intensity_indices[, 1]],
    ppm_y = ppm_y[high_intensity_indices[, 2]],
    intensity = rr_data[high_intensity_indices]
  )
  
  # Exclusion of the water area (if specified)
  if (!is.null(f2_exclude_range) && length(f2_exclude_range) == 2) {
    intensity_df <- intensity_df[!(ppm_y >= f2_exclude_range[1] & ppm_y <= f2_exclude_range[2])]
  }
  
  # OPTIMIZATION 4: Reduce the number of contour levels for TOCSY
  if (!is.null(spectrum_type) && spectrum_type == "TOCSY" && contour_num > 20) {
    contour_num <- min(contour_num, 20)  # Limit to a maximum of 20 outlines
  }
  
  contour_levels <- contour_start * contour_factor^(0:(contour_num - 1))
  
  # OPTIMIZATION 5: Use stat_contour with bins instead of breaks if possible
  p <- ggplot(intensity_df, aes(x = ppm_y, y = ppm_x, z = intensity)) +
    geom_contour(color = "black", breaks = contour_levels, linewidth = 0.3) +  # linewidth reduced
    scale_x_reverse() +
    scale_y_reverse() +
    labs(y = "F1_ppm", x = "F2_ppm") +
    theme_minimal() +
    theme(
      panel.grid = element_blank()  # Remove the grid to speed up rendering
    )
  
  # Zoom if specified (improves performance by reducing the displayed data)
  if (!is.null(zoom_xlim)) p <- p + coord_cartesian(xlim = zoom_xlim)
  if (!is.null(zoom_ylim)) p <- p + coord_cartesian(ylim = zoom_ylim)
  
  contour_data <- ggplot_build(p)$data[[1]]
  
  return(list(plot = p, contour_data = contour_data))
}


# # ===== FONCTION BONUS: Visualiser les clusters rejetés
# 
# plot_rejected_clusters <- function(rr_data, process_result) {
#   
#   # Récupérer les stats
#   stats <- process_result$cluster_stats
#   valid_ids <- process_result$centroids$stain_id
#   
#   # Identifier les rejetés
#   stats <- stats %>%
#     mutate(status = ifelse(stain_id %in% valid_ids, "Valid", "Rejected"))
#   
#   # Plot
#   p <- ggplot(stats, aes(x = elongation, y = intensity_norm, color = status)) +
#     geom_point(size = 3, alpha = 0.7) +
#     geom_vline(xintercept = c(5, 8, 10, 15), linetype = "dashed", alpha = 0.3) +
#     scale_color_manual(values = c("Valid" = "green", "Rejected" = "red")) +
#     scale_x_log10() +
#     labs(
#       title = "Diagnostic: Clusters valides vs rejetés",
#       x = "Élongation (log scale)",
#       y = "Intensité normalisée",
#       color = "Status"
#     ) +
#     theme_minimal()
#   
#   print(p)
#   
#   # Tableau des rejetés
#   rejected <- stats %>% 
#     filter(status == "Rejected") %>%
#     arrange(desc(intensity))
#   
#   cat("\n=== Top 10 clusters rejetés ===\n")
#   print(rejected %>% 
#           select(stain_id, x_center, y_center, intensity, elongation, aspect_ratio, density) %>%
#           head(10))
#   
#   invisible(stats)
# }



# Compute local contour intensity around a point (stain)
get_local_Volume <- function(f2_ppm, f1_ppm, contour_data, eps_ppm = 0.0068) {
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



## Noise threshold ----

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

make_bbox_outline <- function(boxes) {
  if (is.null(boxes) || nrow(boxes) == 0) return(NULL)
  
  # Ensure that stain_id exists
  if (!"stain_id" %in% names(boxes)) {
    boxes$stain_id <- paste0("box_", seq_len(nrow(boxes)))
  }
  
  # ✅ CORRECTION: Create outlines with UNIQUE group and NA between each box
  outline_list <- lapply(seq_len(nrow(boxes)), function(i) {
    box <- boxes[i, ]
    
    # Create a closed rectangle for this box
    rect <- data.frame(
      x = c(box$xmin, box$xmax, box$xmax, box$xmin, box$xmin),
      y = c(box$ymin, box$ymin, box$ymax, box$ymax, box$ymin),
      group = paste0("box_", i),  # ✅ Unique group per box
      stringsAsFactors = FALSE
    )
    
    # ✅ Add a line with NA to separate the boxes
    if (i < nrow(boxes)) {
      rect <- rbind(rect, data.frame(
        x = NA,
        y = NA,
        group = paste0("box_", i),
        stringsAsFactors = FALSE
      ))
    }
    
    rect
  })
  
  do.call(rbind, outline_list)
}
