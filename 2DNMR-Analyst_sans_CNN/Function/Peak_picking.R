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
                             intensity_fraction = 0.5, 
                             box_padding_f2 = 0.001, box_padding_f1 = 0.005,
                             min_cluster_intensity = 0.03, spectrum_type = "COSY",
                             eps_value = 0.02, verbose = TRUE, show_borderline = TRUE,
                             diagnose_zones = NULL, diagnose_radius = 0.1) {
  
  if (is.null(rownames(bruker_data)) || is.null(colnames(bruker_data))) {
    stop("The input object must have row names (F2 ppm) and column names (F1 ppm).")
  }
  
  ppm_x <- as.numeric(rownames(bruker_data))  # F1 ppm (rows)
  ppm_y <- as.numeric(colnames(bruker_data))  # F2 ppm (cols)
  
  mask <- bruker_data >= threshold_value
  if (!any(mask)) {
    return(list(peaks = data.frame(), bounding_boxes = data.frame(), cluster_stats = data.frame()))
  }
  
  significant_points <- which(mask, arr.ind = TRUE)
  
  # === Tous les points au-dessus du seuil avec leurs coordonnées ppm ===
  all_points <- data.frame(
    row_idx = significant_points[, 1],
    col_idx = significant_points[, 2],
    F1_ppm = ppm_x[significant_points[, 1]],
    F2_ppm = ppm_y[significant_points[, 2]],
    Volume = bruker_data[significant_points]
  )
  
  # === Clustering des points du contour pour trouver les régions connectées ===
  contour_coords <- as.matrix(all_points[, c("row_idx", "col_idx")])
  contour_clustering <- dbscan(contour_coords, eps = 1.5, minPts = 1)
  all_points$contour_id <- contour_clustering$cluster
  
  # === Identifier les pics locaux (maxima) ===
  is_peak <- apply(significant_points, 1, function(idx) {
    x <- idx[1]
    y <- idx[2]
    
    x_range <- max(1, x - floor(neighborhood_size / 2)):min(nrow(bruker_data), x + floor(neighborhood_size / 2))
    y_range <- max(1, y - floor(neighborhood_size / 2)):min(ncol(bruker_data), y + floor(neighborhood_size / 2))
    
    neighbors <- as.vector(bruker_data[x_range, y_range])
    median_neighbors <- median(neighbors, na.rm = TRUE)
    mad_neighbors <- mad(neighbors, na.rm = TRUE)
    Volume <- bruker_data[x, y]
    
    prominence_threshold <- prominence_factor * (median_neighbors + mad_neighbors)
    
    sorted_neighbors <- sort(neighbors, decreasing = TRUE, na.last = NA)
    top_index <- ceiling(adaptive_peak_threshold * length(sorted_neighbors))
    top_threshold <- ifelse(top_index > 0, sorted_neighbors[top_index], sorted_neighbors[1])
    
    is_local_max <- Volume >= top_threshold
    prominence <- Volume - median_neighbors
    
    is_local_max && prominence > prominence_threshold
  })
  
  all_points$is_peak <- is_peak
  
  peaks_df <- all_points[is_peak, ]
  if (nrow(peaks_df) == 0) {
    return(list(peaks = data.frame(), bounding_boxes = data.frame(), cluster_stats = data.frame()))
  }
  
  # === Filtres de base ===
  if (!is.null(f2_exclude_range) && length(f2_exclude_range) == 2) {
    peaks_df <- peaks_df[!(peaks_df$F2_ppm >= f2_exclude_range[1] & peaks_df$F2_ppm <= f2_exclude_range[2]), ]
  }
  
  if (nrow(peaks_df) == 0) {
    return(list(peaks = data.frame(), bounding_boxes = data.frame(), cluster_stats = data.frame()))
  }
  
  # === Calculer les statistiques par contour ===
  valid_contours <- unique(peaks_df$contour_id)
  
  cluster_stats <- all_points %>%
    filter(contour_id %in% valid_contours) %>%
    group_by(contour_id) %>%
    summarise(
      intensity = sum(Volume, na.rm = TRUE),
      x_span = max(F2_ppm, na.rm = TRUE) - min(F2_ppm, na.rm = TRUE),
      y_span = max(F1_ppm, na.rm = TRUE) - min(F1_ppm, na.rm = TRUE),
      x_center = mean(F2_ppm, na.rm = TRUE),
      y_center = mean(F1_ppm, na.rm = TRUE),
      n_points = n(),
      x_sd = sd(F2_ppm, na.rm = TRUE),
      y_sd = sd(F1_ppm, na.rm = TRUE),
      x_var = var(F2_ppm, na.rm = TRUE),
      y_var = var(F1_ppm, na.rm = TRUE),
      F2_min = min(F2_ppm, na.rm = TRUE),
      F2_max = max(F2_ppm, na.rm = TRUE),
      F1_min = min(F1_ppm, na.rm = TRUE),
      F1_max = max(F1_ppm, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      x_var = ifelse(is.na(x_var), 0, x_var),
      y_var = ifelse(is.na(y_var), 0, y_var),
      x_sd = ifelse(is.na(x_sd), 0, x_sd),
      y_sd = ifelse(is.na(y_sd), 0, y_sd),
      
      elongation = pmax(x_span / (y_span + 1e-10), y_span / (x_span + 1e-10)),
      is_diagonal = abs(abs(x_center) - abs(y_center)) < 0.1,
      aspect_ratio_x = x_span / (y_span + 1e-10),
      aspect_ratio_y = y_span / (x_span + 1e-10),
      is_horizontal = aspect_ratio_x > aspect_ratio_y,
      is_vertical = aspect_ratio_y > aspect_ratio_x,
      is_horizontal_line = is_horizontal & (y_var < 0.0001) & (x_span > 0.01) & (n_points < 100),
      is_vertical_line = is_vertical & (x_var < 1e-09) & (y_span > 0.015) & (n_points < 30),
      width_to_height = x_span / (y_span + 1e-10),
      is_thin_vertical = is_vertical & (x_span < 0.00002) & (y_span > 0.01),
      area = x_span * y_span,
      density = intensity / (area + 1e-10),
      cv_x = x_sd / (abs(x_center) + 1e-10),
      cv_y = y_sd / (abs(y_center) + 1e-10),
      linearity_x = 1 - pmin(1, x_var / (x_span^2 / 12 + 1e-10)),
      linearity_y = 1 - pmin(1, y_var / (y_span^2 / 12 + 1e-10))
    )
  
  max_intensity <- max(cluster_stats$intensity, na.rm = TRUE)
  density_q02 <- quantile(cluster_stats$density, 0.02, na.rm = TRUE)
  density_q05 <- quantile(cluster_stats$density, 0.05, na.rm = TRUE)
  density_q10 <- quantile(cluster_stats$density, 0.10, na.rm = TRUE)
  density_q20 <- quantile(cluster_stats$density, 0.20, na.rm = TRUE)
  density_q30 <- quantile(cluster_stats$density, 0.30, na.rm = TRUE)
  
  cluster_stats <- cluster_stats %>%
    mutate(intensity_norm = intensity / max_intensity)
  
  # === Calculer les scores de proximité aux seuils ===
  cluster_stats <- cluster_stats %>%
    mutate(
      intensity_threshold = min_cluster_intensity * 0.3,
      intensity_margin = (intensity - intensity_threshold) / (intensity_threshold + 1e-10),
      
      elongation_max = case_when(
        is_diagonal ~ 25,
        intensity_norm > 0.2 ~ 20,
        intensity_norm > 0.05 ~ 12,
        intensity_norm > 0.01 ~ 10,
        TRUE ~ 100
      ),
      elongation_margin = (elongation_max - elongation) / (elongation_max + 1e-10),
      
      density_threshold = density_q10,
      density_margin = (density - density_threshold) / (density_threshold + 1e-10),
      
      borderline_score = (
        pmin(1, pmax(-1, intensity_margin)) + 
          pmin(1, pmax(-1, elongation_margin)) + 
          pmin(1, pmax(-1, density_margin))
      ) / 3,
      
      is_artifact = is_horizontal_line | is_vertical_line | is_thin_vertical
    )
  
  # === FILTRE selon le type de spectre ===
  
  if (spectrum_type == "HSQC") {
    
    valid_ids <- cluster_stats %>%
      filter(
        intensity >= min_cluster_intensity,
        elongation <= 100
      ) %>%
      pull(contour_id)
    
  } else if (spectrum_type %in% c("TOCSY", "COSY")) {
    
    # Dans la section TOCSY, ajouter cette condition :
    valid_ids <- cluster_stats %>%
      pull(contour_id)
    
    # ══════════════════════════════════════════════════════════════════
    # NOUVEAU: Filtrer les pics faibles sur la même colonne F2 qu'un pic intense
    # ══════════════════════════════════════════════════════════════════
    
    valid_stats <- cluster_stats %>% filter(contour_id %in% valid_ids)
    
    if (nrow(valid_stats) > 1) {
      
      f2_tolerance <- 0.02  # ppm - pics considérés sur la même "colonne"
      intensity_ratio_threshold <- 0.05  # garder seulement si > 5% du pic max de la colonne
      
      # Pour chaque pic, trouver l'intensité max sur sa colonne F2
      valid_stats <- valid_stats %>%
        mutate(
          f2_group = round(x_center / f2_tolerance) * f2_tolerance
        ) %>%
        group_by(f2_group) %>%
        mutate(
          max_intensity_in_column = max(intensity, na.rm = TRUE),
          intensity_ratio = intensity / max_intensity_in_column,
          n_peaks_in_column = n()
        ) %>%
        ungroup()
      
      # Garder si : pic dominant OU ratio suffisant OU colonne avec peu de pics
      valid_ids <- valid_stats %>%
        filter(
          intensity == max_intensity_in_column |  # Toujours garder le plus intense
            intensity_ratio >= intensity_ratio_threshold |  # Assez intense relativement
            n_peaks_in_column <= 2 |  # Pas de problème si peu de pics
            is_diagonal  # Toujours garder les diagonaux
        ) %>%
        pull(contour_id)
    }
    
  } else {  # UFCOSY
    
    valid_ids <- cluster_stats %>%
      filter(
        intensity >= min_cluster_intensity * 0.5,
        (
          (intensity_norm > 0.2 & elongation <= 20) |
            (intensity_norm > 0.05 & elongation <= 12) |
            (elongation <= 20) |
            # NOUVEAU: Multiplets verticaux (fins mais avec bonne densité)
            (is_vertical & density > density_q05 & n_points >= 15 & elongation <= 50)
        ),
        !(is_horizontal & aspect_ratio_x > 20 & y_span < 0.03),
        density > density_q05 | intensity_norm > 0.1,
        # NOUVEAU: Ne pas rejeter les artefacts verticaux si c'est un multiplet probable
        !(is_vertical_line & density <= density_q05),
        !(is_thin_vertical & n_points < 15)
      ) %>%
      pull(contour_id)
  }
  
  # Ajouter le statut de validation
  cluster_stats <- cluster_stats %>%
    mutate(status = case_when(
      contour_id %in% valid_ids ~ "ACCEPTED",
      is_artifact ~ "REJECTED_ARTIFACT",
      TRUE ~ "REJECTED"
    ))
  
  valid_clusters <- cluster_stats %>% filter(status == "ACCEPTED")
  invalid_clusters <- cluster_stats %>% filter(status != "ACCEPTED")
  # === Redistribution des intensités perdues ===
  if (nrow(invalid_clusters) > 0 && nrow(valid_clusters) > 0) {
    for (i in seq_len(nrow(invalid_clusters))) {
      inv <- invalid_clusters[i, ]
      
      valid_clusters <- valid_clusters %>%
        mutate(x_dist = abs(x_center - inv$x_center))
      
      idx_target <- valid_clusters %>%
        arrange(x_dist, desc(intensity)) %>%
        slice(1) %>%
        pull(contour_id)
      
      cluster_stats$intensity[cluster_stats$contour_id == idx_target] <-
        cluster_stats$intensity[cluster_stats$contour_id == idx_target] + inv$intensity
    }
  }
  
  if (length(valid_ids) == 0) {
    return(list(peaks = data.frame(), bounding_boxes = data.frame(), cluster_stats = cluster_stats))
  }
  
  # === Fusionner les pics par contour ===
  peak_list <- peaks_df %>%
    filter(contour_id %in% valid_ids) %>%
    group_by(contour_id) %>%
    summarise(
      F2_ppm = weighted.mean(F2_ppm, Volume, na.rm = TRUE),
      F1_ppm = weighted.mean(F1_ppm, Volume, na.rm = TRUE),
      Volume = sum(Volume, na.rm = TRUE),
      .groups = "drop"
    )
  
  # === Joindre les bounding boxes ===
  bounding_boxes_data <- valid_clusters %>%
    select(contour_id, F2_min, F2_max, F1_min, F1_max)
  
  peak_list <- peak_list %>%
    left_join(bounding_boxes_data, by = "contour_id")
  
  # === Sélection par zone ===
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
          dplyr::arrange(desc(Volume)) %>%
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
  
  # === Suppression centroïdes inclus plus faibles ===
  if (nrow(peak_list) > 1) {
    to_remove <- c()
    for (i in seq_len(nrow(peak_list))) {
      current <- peak_list[i, ]
      for (j in seq_len(nrow(peak_list))) {
        if (i == j) next
        compare <- peak_list[j, ]
        inside_box <- current$F2_ppm >= compare$F2_min & current$F2_ppm <= compare$F2_max &
          current$F1_ppm >= compare$F1_min & current$F1_ppm <= compare$F1_max
        lower_intensity <- current$Volume < compare$Volume
        if (inside_box && lower_intensity) {
          to_remove <- c(to_remove, current$contour_id)
          break
        }
      }
    }
    peak_list <- peak_list %>% filter(!contour_id %in% to_remove)
  }
  
  # === Finalisation ===
  peak_list <- peak_list %>%
    mutate(stain_id = paste0("peak", seq_len(n()))) %>%
    select(stain_id, F2_ppm, F1_ppm, Volume, F2_min, F2_max, F1_min, F1_max)
  
  bounding_boxes <- data.frame(
    stain_id = peak_list$stain_id,
    xmin = peak_list$F2_min - box_padding_f2,
    xmax = peak_list$F2_max + box_padding_f2,
    ymin = peak_list$F1_min - box_padding_f1,
    ymax = peak_list$F1_max + box_padding_f1
  )
  
  peak_list <- peak_list %>%
    select(stain_id, F2_ppm, F1_ppm, Volume)
  
  
  return(list(
    peaks = peak_list, 
    bounding_boxes = bounding_boxes, 
    cluster_stats = cluster_stats
  ))
}

# ---- Filtrage des pics de bruit résiduel dans les spectres TOCSY ----
filter_noise_peaks <- function(peaks, min_neighbors = 4, neighbor_radius = 0.03, min_relative_intensity = 0.03) {
  if (nrow(peaks) < 2) return(peaks)  # Aucun filtrage nécessaire
  
  peaks <- peaks %>%
    dplyr::mutate(
      keep = sapply(seq_len(n()), function(i) {
        x <- peaks$F2_ppm[i]
        y <- peaks$F1_ppm[i]
        intensity <- peaks$Volume[i]
        max_intensity <- max(peaks$Volume)
        
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