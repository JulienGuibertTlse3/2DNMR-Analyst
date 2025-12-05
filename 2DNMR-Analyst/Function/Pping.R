library(dplyr)
library(zoo)
library(matrixStats)
library(minpack.lm)  # For Gaussian fitting
library(dplyr)
library(imager)


## R Function ----
peak_pick_2d_nt2 <- function(bruker_data, threshold = 5, neighborhood_size = 5,
                             prominence_factor = 0.01, adaptive_peak_threshold = 0.001,
                             threshold_value = NULL, f2_exclude_range = NULL,
                             keep_peak_ranges = NULL, box_window_size = 11,
                             intensity_fraction = 0.5) {
  
  if (is.null(rownames(bruker_data)) || is.null(colnames(bruker_data))) {
    stop("The input object must have row names (F2 ppm) and column names (F1 ppm).")
  }
  
  ppm_x <- as.numeric(rownames(bruker_data))  # F2 ppm
  ppm_y <- as.numeric(colnames(bruker_data))  # F1 ppm
  
  mask <- bruker_data >= threshold_value
  if (!any(mask)) {
    return(list(peaks = data.frame(), bounding_boxes = data.frame()))
  }
  
  significant_points <- which(mask, arr.ind = TRUE)
  
  is_peak <- apply(significant_points, 1, function(idx) {
    x <- idx[1]
    y <- idx[2]
    
    x_range <- max(1, x - floor(neighborhood_size / 2)):min(nrow(bruker_data), x + floor(neighborhood_size / 2))
    y_range <- max(1, y - floor(neighborhood_size / 2)):min(ncol(bruker_data), y + floor(neighborhood_size / 2))
    
    neighbors <- as.vector(bruker_data[x_range, y_range])
    median_neighbors <- median(neighbors, na.rm = TRUE)
    mad_neighbors <- mad(neighbors, na.rm = TRUE)
    stain_intensity <- bruker_data[x, y]
    
    prominence_threshold <- prominence_factor * (median_neighbors + mad_neighbors)
    
    sorted_neighbors <- sort(neighbors, decreasing = TRUE, na.last = NA)
    top_index <- ceiling(adaptive_peak_threshold * length(sorted_neighbors))
    top_threshold <- ifelse(top_index > 0, sorted_neighbors[top_index], sorted_neighbors[1])
    
    is_local_max <- stain_intensity >= top_threshold
    prominence <- stain_intensity - median_neighbors
    
    is_local_max && prominence > prominence_threshold
  })
  
  peaks <- significant_points[is_peak, , drop = FALSE]
  if (nrow(peaks) == 0) {
    return(list(peaks = data.frame(), bounding_boxes = data.frame()))
  }
  
  peak_list <- data.frame(
    F2_ppm = ppm_y[peaks[, 2]],
    F1_ppm = ppm_x[peaks[, 1]],
    stain_intensity = bruker_data[peaks]
  )
  
  if (!is.null(f2_exclude_range) && length(f2_exclude_range) == 2) {
    peak_list <- peak_list[!(peak_list$F2_ppm >= f2_exclude_range[1] & peak_list$F2_ppm <= f2_exclude_range[2]), ]
  }
  
  if (!is.null(keep_peak_ranges) && is.list(keep_peak_ranges)) {
    filtered_peaks <- data.frame()
    
    for (i in seq_along(keep_peak_ranges)) {
      range <- keep_peak_ranges[[i]]
      
      if (length(range) == 2) {
        lower_bound <- min(range)
        upper_bound <- max(range)
        
        peaks_in_range <- peak_list %>%
          dplyr::filter(F2_ppm >= lower_bound & F2_ppm <= upper_bound)
        
        num_peaks_to_keep <- if (i <= 1) 1 else 4
        
        top_peaks_in_range <- peaks_in_range %>%
          dplyr::arrange(desc(stain_intensity)) %>%
          dplyr::slice_head(n = num_peaks_to_keep)
        
        filtered_peaks <- dplyr::bind_rows(filtered_peaks, top_peaks_in_range)
      }
    }
    
    peaks_outside_ranges <- peak_list %>%
      dplyr::filter(!(
        sapply(1:nrow(peak_list), function(j) {
          any(sapply(keep_peak_ranges, function(range) {
            lower_bound <- min(range)
            upper_bound <- max(range)
            peak_list$F2_ppm[j] >= lower_bound && peak_list$F2_ppm[j] <= upper_bound
          }))
        })
      ))
    
    peak_list <- dplyr::bind_rows(peaks_outside_ranges, filtered_peaks) %>%
      dplyr::distinct()
  }
  peak_list <- peak_list %>%
    filter_noise_peaks(min_neighbors = 0, neighbor_radius = 0.01, min_relative_intensity = 0.02) %>%
    dplyr::mutate(stain_id = paste0("peak", seq_len(n()))) %>%
    dplyr::select(stain_id, F2_ppm, F1_ppm, stain_intensity)
  
  # === Fusionner les pics proches avec DBSCAN ===
  if (nrow(peak_list) > 1) {
    coords <- as.matrix(peak_list[, c("F2_ppm", "F1_ppm")])
    
    # clustering : points à moins de fusion_tolerance appartiennent au même cluster
    clustering <- dbscan(coords, eps = 0.02, minPts = 1)
    
    peak_list <- peak_list %>%
      mutate(cluster = clustering$cluster) %>%
      group_by(cluster) %>%
      summarise(
        F2_ppm = mean(F2_ppm, na.rm = TRUE),
        F1_ppm = mean(F1_ppm, na.rm = TRUE),
        stain_intensity = sum(stain_intensity, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(stain_id = paste0("peak", seq_len(n())))
  }
  
  # === Bounding boxes comme avant ===
  bounding_boxes <- lapply(seq_len(nrow(peak_list)), function(i) {
    peak <- peak_list[i, ]
    cx_ppm <- peak$F1_ppm
    cy_ppm <- peak$F2_ppm
    intensity <- peak$stain_intensity
    
    width  <- 0.02
    height <- 0.002
    
    data.frame(
      stain_id = peak$stain_id,
      xmin = cy_ppm - width / 2,
      xmax = cy_ppm + width / 2,
      ymin = cx_ppm - height / 2,
      ymax = cx_ppm + height / 2
    )
  })
  
  bounding_boxes <- do.call(rbind, bounding_boxes)
  
  return(list(peaks = peak_list, bounding_boxes = bounding_boxes))
}

# ---- Filtrage des pics de bruit résiduel dans les spectres TOCSY ----
filter_noise_peaks <- function(peaks, min_neighbors = 2, neighbor_radius = 0.015, min_relative_intensity = 0.03) {
  if (nrow(peaks) < 2) return(peaks)  # Aucun filtrage nécessaire
  
  peaks <- peaks %>%
    dplyr::mutate(
      keep = sapply(seq_len(n()), function(i) {
        x <- peaks$F2_ppm[i]
        y <- peaks$F1_ppm[i]
        intensity <- peaks$stain_intensity[i]
        max_intensity <- max(peaks$stain_intensity)
        
        # Distance aux autres pics
        dist <- sqrt((peaks$F2_ppm - x)^2 + (peaks$F1_ppm - y)^2)
        neighbors <- sum(dist < neighbor_radius) - 1  # Exclure soi-même
        
        # Critères de rejet : isolement + intensité trop faible
        neighbors >= min_neighbors || (intensity / max_intensity > min_relative_intensity)
      })
    ) %>%
    dplyr::filter(keep) %>%
    dplyr::select(-keep)
  
  return(peaks)
}