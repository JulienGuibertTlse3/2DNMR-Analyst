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
                                              downsample_factor = 2.2) {  # Nouveau paramètre
  
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
  
  # OPTIMISATION 1: Downsampling de la matrice pour l'affichage
  if (downsample_factor > 1) {
    seq_x <- seq(1, nrow(rr_data), by = downsample_factor)
    seq_y <- seq(1, ncol(rr_data), by = downsample_factor)
    rr_data <- rr_data[seq_x, seq_y]
  }
  
  ppm_x <- as.numeric(rownames(rr_data))
  ppm_y <- as.numeric(colnames(rr_data))
  
  # OPTIMISATION 2: Filtrage précoce avant expand.grid
  # Ne garder que les indices où l'intensité dépasse le seuil
  high_intensity_indices <- which(rr_data >= intensity_threshold, arr.ind = TRUE)
  
  if (nrow(high_intensity_indices) == 0) {
    warning("No data points above intensity threshold")
    return(list(plot = ggplot() + theme_void(), contour_data = data.frame()))
  }
  
  # OPTIMISATION 3: Création directe du data.frame sans expand.grid
  intensity_df <- data.table(
    ppm_x = ppm_x[high_intensity_indices[, 1]],
    ppm_y = ppm_y[high_intensity_indices[, 2]],
    intensity = rr_data[high_intensity_indices]
  )
  
  # Exclusion de la région d'eau (si spécifiée)
  if (!is.null(f2_exclude_range) && length(f2_exclude_range) == 2) {
    intensity_df <- intensity_df[!(ppm_y >= f2_exclude_range[1] & ppm_y <= f2_exclude_range[2])]
  }
  
  # OPTIMISATION 4: Réduire le nombre de niveaux de contour pour TOCSY
  if (!is.null(spectrum_type) && spectrum_type == "TOCSY" && contour_num > 20) {
    contour_num <- min(contour_num, 20)  # Limiter à 20 contours max
  }
  
  contour_levels <- contour_start * contour_factor^(0:(contour_num - 1))
  
  # OPTIMISATION 5: Utiliser stat_contour avec bins au lieu de breaks si possible
  p <- ggplot(intensity_df, aes(x = ppm_y, y = ppm_x, z = intensity)) +
    geom_contour(color = "black", breaks = contour_levels, linewidth = 0.3) +  # linewidth réduit
    scale_x_reverse() +
    scale_y_reverse() +
    labs(y = "F1_ppm", x = "F2_ppm") +
    theme_minimal() +
    theme(
      panel.grid = element_blank()  # Supprimer la grille pour accélérer le rendu
    )
  
  # Zoom si spécifié (améliore la performance en réduisant les données affichées)
  if (!is.null(zoom_xlim)) p <- p + coord_cartesian(xlim = zoom_xlim)
  if (!is.null(zoom_ylim)) p <- p + coord_cartesian(ylim = zoom_ylim)
  
  contour_data <- ggplot_build(p)$data[[1]]
  
  return(list(plot = p, contour_data = contour_data))
}




# ---- VERSION ALTERNATIVE: Utiliser geom_raster pour un aperçu rapide ----
find_nmr_peak_centroids_fast_preview <- function(rr_data, spectrum_type = NULL,
                                                 intensity_threshold = NULL,
                                                 zoom_xlim = NULL, zoom_ylim = NULL,
                                                 f2_exclude_range = NULL,
                                                 downsample_factor = 4) {  # Plus agressif
  
  if (is.null(rr_data) || !is.matrix(rr_data)) {
    stop("Invalid Bruker data.")
  }
  
  # Downsampling agressif
  seq_x <- seq(1, nrow(rr_data), by = downsample_factor)
  seq_y <- seq(1, ncol(rr_data), by = downsample_factor)
  rr_data_small <- rr_data[seq_x, seq_y]
  
  ppm_x <- as.numeric(rownames(rr_data_small))
  ppm_y <- as.numeric(colnames(rr_data_small))
  
  # Création data.frame minimal
  intensity_df <- data.table(
    ppm_x = rep(ppm_x, each = length(ppm_y)),
    ppm_y = rep(ppm_y, times = length(ppm_x)),
    intensity = as.vector(rr_data_small)
  )
  
  # Filtrage du seuil
  if (!is.null(intensity_threshold)) {
    intensity_df <- intensity_df[intensity >= intensity_threshold]
  }
  
  # Exclusion région d'eau
  if (!is.null(f2_exclude_range) && length(f2_exclude_range) == 2) {
    intensity_df <- intensity_df[!(ppm_y >= f2_exclude_range[1] & ppm_y <= f2_exclude_range[2])]
  }
  
  # Utiliser geom_raster (beaucoup plus rapide que geom_contour)
  p <- ggplot(intensity_df, aes(x = ppm_y, y = ppm_x, fill = log10(intensity + 1))) +
    geom_raster(interpolate = TRUE) +
    scale_fill_viridis_c(option = "magma") +
    scale_x_reverse() +
    scale_y_reverse() +
    labs(y = "F1_ppm", x = "F2_ppm", fill = "log10(Intensity)") +
    theme_minimal() +
    theme(panel.grid = element_blank())
  
  if (!is.null(zoom_xlim)) p <- p + coord_cartesian(xlim = zoom_xlim)
  if (!is.null(zoom_ylim)) p <- p + coord_cartesian(ylim = zoom_ylim)
  
  return(list(plot = p, contour_data = NULL))
}

# pp ----
process_nmr_centroids <- function(rr_data, contour_data, contour_num = NULL, contour_factor = NULL, 
                                  intensity_threshold = NULL, keep_peak_ranges = NULL, eps_value = NULL,
                                  min_cluster_intensity = 0.03, spectrum_type = "cosy") {
  
  # Check contour validity
  if (nrow(contour_data) == 0 || !"group" %in% colnames(contour_data)) {
    stop("No contours data available for processing.")
  }
  
  # --- 2. Normalisation adaptée ---
  if (spectrum_type == "HSQC") {
    contour_data <- contour_data %>%
      mutate(
        F2_scaled = (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE),
        F1_scaled = (y - mean(y, na.rm = TRUE)) / (sd(y, na.rm = TRUE) * 5)
      )
    min_pts <- 10
  } else {
    contour_data <- contour_data %>%
      mutate(
        F2_scaled = (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE),
        F1_scaled = (y - mean(y, na.rm = TRUE)) / sd(y, na.rm = TRUE)
      )
    min_pts <- 0
  }
  
  # --- 3. DBSCAN ---
  clusters <- dbscan::dbscan(contour_data[, c("F2_scaled", "F1_scaled")], eps = eps_value, minPts = min_pts)
  contour_data$stain_id <- as.character(clusters$cluster)
  contour_data <- contour_data %>% filter(stain_id != "0")
  
  # --- 4. Statistiques des clusters AMÉLIORÉES ---
  cluster_stats <- contour_data %>%
    group_by(stain_id) %>%
    summarise(
      intensity = sum(level, na.rm = TRUE),
      x_span = max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
      y_span = max(y, na.rm = TRUE) - min(y, na.rm = TRUE),
      x_center = mean(x, na.rm = TRUE),
      y_center = mean(y, na.rm = TRUE),
      n_points = n(),
      x_sd = sd(x, na.rm = TRUE),
      y_sd = sd(y, na.rm = TRUE),
      # ✅ NOUVEAU: Variance pour détecter l'alignement
      x_var = var(x, na.rm = TRUE),
      y_var = var(y, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      elongation = pmax(x_span / (y_span + 1e-10), y_span / (x_span + 1e-10)),
      
      # ✅ Calculer si c'est un pic diagonal (TOCSY)
      is_diagonal = abs(abs(x_center) - abs(y_center)) < 0.1,
      
      # ✅ Ratio de compacité avec direction
      aspect_ratio_x = x_span / (y_span + 1e-10),
      aspect_ratio_y = y_span / (x_span + 1e-10),
      
      # ✅ Direction d'élongation
      is_horizontal = aspect_ratio_x > aspect_ratio_y,
      is_vertical = aspect_ratio_y > aspect_ratio_x,
      
      # ✅ NOUVEAU: Détecter les "lignes" (artefacts) vs "formes" (vrais pics)
      # Les artefacts ont une très faible variance sur un axe ET peu de points
      is_horizontal_line = is_horizontal & (y_var < 0.0001) & (x_span > 0.08) & (n_points < 200),
      is_vertical_line = is_vertical & (x_var < 0.00005) & (y_span > 0.15) & (n_points < 200),  # ✅ Seuils ajustés
      
      # ✅ NOUVEAU: Pour les pics verticaux, ratio largeur/hauteur
      # Vrais multiplets: largeur raisonnable même si allongés
      # Artefacts: très fins (x_span très petit) ET longs
      width_to_height = x_span / (y_span + 1e-10),
      is_thin_vertical = is_vertical & (x_span < 0.015) & (y_span > 0.1),  # ✅ Seuil réduit de 0.02 à 0.015
      
      # ✅ Densité
      area = x_span * y_span,
      density = intensity / (area + 1e-10),
      
      # ✅ Coefficient de variation
      cv_x = x_sd / (abs(x_center) + 1e-10),
      cv_y = y_sd / (abs(y_center) + 1e-10),
      
      # ✅ NOUVEAU: Score de "linearité" (0 = compact, 1 = ligne parfaite)
      linearity_x = 1 - pmin(1, x_var / (x_span^2 / 12 + 1e-10)),
      linearity_y = 1 - pmin(1, y_var / (y_span^2 / 12 + 1e-10))
    )
  
  # --- 4.5 NOUVEAU: Normalisation de l'intensité pour seuils adaptatifs ---
  max_intensity <- max(cluster_stats$intensity, na.rm = TRUE)
  cluster_stats <- cluster_stats %>%
    mutate(intensity_norm = intensity / max_intensity)
  
  
  # --- 5. FILTRE AMÉLIORÉ avec critères adaptatifs ---
  
  if (spectrum_type == "HSQC") {
    
    valid_ids <- cluster_stats %>%
      filter(
        intensity >= min_cluster_intensity,
        elongation <= 100
      ) %>%
      pull(stain_id)
    
  } else if (spectrum_type == "TOCSY") {
    
    # ✅ CRITÈRES POUR TOCSY: Distinguer vrais multiplets vs artefacts
    valid_ids <- cluster_stats %>%
      filter(
        # Critère 1: Intensité minimale TRÈS ASSOUPLIE avec exceptions
        (
          intensity >= min_cluster_intensity * 0.3 |
            
            # ✅ EXCEPTION: Pics avec excellentes caractéristiques morphologiques
            (elongation <= 8 & n_points >= 50 & density > quantile(cluster_stats$density, 0.3, na.rm = TRUE)) |
            
            # ✅ NOUVELLE EXCEPTION: Pics verticaux avec beaucoup de points (vrais multiplets)
            (is_vertical & x_span >= 0.02 & n_points >= 100 & density > quantile(cluster_stats$density, 0.2, na.rm = TRUE))
        ),
        
        # Critère 2: Rejeter les LIGNES (artefacts horizontaux et verticaux)
        !is_horizontal_line,
        !is_vertical_line,
        !is_thin_vertical,
        
        # Critère 3: Élongation adaptative SELON LE TYPE DE PIC
        (
          # Cas A: Pics diagonaux (autopeaks) - permissifs
          (is_diagonal & elongation <= 25) |
            
            # Cas B: Pics très intenses - tolérants
            (intensity_norm > 0.2 & elongation <= 20) |
            
            # Cas C: Multiplets verticaux VRAIS (pas trop fins)
            # ✅ ASSOUPLI: linearity_y peut aller jusqu'à 0.9 pour vrais multiplets
            (is_vertical & x_span >= 0.02 & elongation <= 30 & (linearity_y < 0.9 | n_points >= 200)) |
            
            # Cas D: Pics moyens - modérés
            (intensity_norm > 0.05 & elongation <= 12) |
            
            # Cas E: Pics faibles - assouplis
            (intensity_norm > 0.01 & elongation <= 10) |
            
            # Cas F: Pics très faibles mais compacts
            (elongation <= 6) |
            
            # ✅ NOUVEAU Cas G: Pics verticaux bien structurés (beaucoup de points)
            (is_vertical & x_span >= 0.025 & n_points >= 100 & elongation <= 35)
        ),
        
        # Critère 4: Densité TRÈS ASSOUPLIE
        density > quantile(cluster_stats$density, 0.02, na.rm = TRUE) |
          intensity_norm > 0.05 |
          elongation <= 6 |
          n_points >= 100,  # ✅ Les pics bien structurés exemptés
        
        # Critère 5: Nombre de points ASSOUPLI
        n_points >= 1 |
          intensity_norm > 0.05
        
      ) %>%
      pull(stain_id)
    
  } else {  # COSY, UFCOSY
    
    # ✅ CRITÈRES ASSOUPLIS pour COSY/UFCOSY
    valid_ids <- cluster_stats %>%
      filter(
        intensity >= min_cluster_intensity * 0.5,
        
        # Élongation adaptative ASSOUPLIE
        (
          (intensity_norm > 0.2 & elongation <= 20) |
            (intensity_norm > 0.05 & elongation <= 12) |
            (elongation <= 8)
        ),
        
        # Éviter artefacts TRÈS évidents seulement
        !(is_horizontal & aspect_ratio_x > 20 & y_span < 0.03),  # ✅ CORRIGÉ
        density > quantile(cluster_stats$density, 0.05, na.rm = TRUE) | intensity_norm > 0.1
      ) %>%
      pull(stain_id)
  }
  
  
  valid_clusters <- cluster_stats %>% filter(stain_id %in% valid_ids)
  invalid_clusters <- cluster_stats %>% filter(!stain_id %in% valid_ids)
  
  # ✅ DIAGNOSTIC: Afficher les raisons de rejet
  if (nrow(invalid_clusters) > 0) {
    message(sprintf("🗑️  %d clusters rejetés:", nrow(invalid_clusters)))
    
    # Compter par type de rejet
    n_horizontal_lines <- sum(invalid_clusters$is_horizontal_line, na.rm = TRUE)
    n_vertical_lines <- sum(invalid_clusters$is_vertical_line, na.rm = TRUE)
    n_thin_vertical <- sum(invalid_clusters$is_thin_vertical, na.rm = TRUE)
    
    message(sprintf("  📊 Artefacts horizontaux (lignes): %d", n_horizontal_lines))
    message(sprintf("  📊 Artefacts verticaux (lignes): %d", n_vertical_lines))
    message(sprintf("  📊 Pics trop fins (artefacts): %d", n_thin_vertical))
    
    # ✅ Calculer le seuil de densité
    density_threshold <- quantile(cluster_stats$density, 0.05, na.rm = TRUE)
    
    for (i in seq_len(min(10, nrow(invalid_clusters)))) {
      inv <- invalid_clusters[i, ]
      reasons <- c()
      
      if (inv$intensity < min_cluster_intensity * 0.5) {
        reasons <- c(reasons, sprintf("intensité=%.3f", inv$intensity))
      }
      if (inv$is_horizontal_line) {
        reasons <- c(reasons, sprintf("ARTEFACT horizontal (x_span=%.3f)", inv$x_span))
      }
      if (inv$is_vertical_line) {
        reasons <- c(reasons, sprintf("ARTEFACT vertical (y_span=%.3f)", inv$y_span))
      }
      if (inv$is_thin_vertical) {
        reasons <- c(reasons, sprintf("trop fin (x_span=%.4f)", inv$x_span))
      }
      if (inv$elongation > 30 & !inv$is_diagonal) {
        reasons <- c(reasons, sprintf("elong=%.1f", inv$elongation))
      }
      if (inv$is_vertical & inv$x_span >= 0.02 & inv$linearity_y >= 0.8) {
        reasons <- c(reasons, sprintf("⚠️ vertical linéaire (lin_y=%.2f)", inv$linearity_y))
      }
      # ✅ NOUVEAU: Vérifier densité
      if (inv$density <= density_threshold & inv$intensity_norm <= 0.1) {
        reasons <- c(reasons, sprintf("densité=%.2e (seuil=%.2e)", inv$density, density_threshold))
      }
      # ✅ NOUVEAU: Vérifier nombre de points
      if (inv$n_points < 2 & inv$intensity_norm <= 0.15) {
        reasons <- c(reasons, sprintf("n_pts=%d", inv$n_points))
      }
      
      # ✅ Si aucune raison trouvée, afficher les stats clés
      if (length(reasons) == 0) {
        reasons <- c(reasons, sprintf("⚠️ RAISON INCONNUE - elong=%.1f, dens=%.2e, n_pts=%d, int_norm=%.3f",
                                      inv$elongation, inv$density, inv$n_points, inv$intensity_norm))
      }
      
      message(sprintf("  - Cluster %s @ (%.2f, %.2f): %s", 
                      inv$stain_id, -inv$x_center, -inv$y_center, 
                      paste(reasons, collapse = ", ")))
    }
    
    # ✅ Avertir si des multiplets potentiels ont été rejetés
    potential_good_peaks <- invalid_clusters %>%
      filter(
        !is_horizontal_line, 
        !is_vertical_line, 
        !is_thin_vertical,
        elongation <= 10,  # Pics compacts
        n_points >= 50,    # Beaucoup de points
        density > quantile(cluster_stats$density, 0.2, na.rm = TRUE)  # Bonne densité
      )
    
    if (nrow(potential_good_peaks) > 0) {
      message(sprintf("\n⚠️⚠️ ALERTE: %d pics avec BONNES caractéristiques rejetés (probablement intensité trop faible):", 
                      nrow(potential_good_peaks)))
      for (i in seq_len(min(5, nrow(potential_good_peaks)))) {
        pg <- potential_good_peaks[i, ]
        message(sprintf("    @ (%.2f, %.2f): int=%.3f (norm=%.4f), x_span=%.4f, y_span=%.3f, elong=%.1f, dens=%.2e, n_pts=%d",
                        -pg$x_center, -pg$y_center, pg$intensity, pg$intensity_norm,
                        pg$x_span, pg$y_span, pg$elongation, pg$density, pg$n_points))
      }
      message(sprintf("  💡 Suggestion: Réduire min_cluster_intensity de %.3f à %.3f", 
                      min_cluster_intensity, 
                      min(potential_good_peaks$intensity) * 0.8))
    }
    
    # Multiplets verticaux potentiels
    potential_multiplets <- invalid_clusters %>%
      filter(is_vertical, x_span >= 0.02, x_span < 0.05, elongation <= 40, linearity_y < 0.8)
    
    if (nrow(potential_multiplets) > 0) {
      message(sprintf("\n⚠️  WARNING: %d multiplets verticaux POTENTIELS rejetés:", 
                      nrow(potential_multiplets)))
      for (i in seq_len(min(3, nrow(potential_multiplets)))) {
        pm <- potential_multiplets[i, ]
        
        # ✅ DIAGNOSTIC DÉTAILLÉ: Afficher TOUS les flags
        flags <- c()
        if (pm$is_horizontal_line) flags <- c(flags, "HORIZ_LINE")
        if (pm$is_vertical_line) flags <- c(flags, "VERT_LINE")
        if (pm$is_thin_vertical) flags <- c(flags, "THIN_VERT")
        if (pm$is_diagonal) flags <- c(flags, "DIAGONAL")
        
        message(sprintf("    @ (%.2f, %.2f): int=%.3f, x_span=%.4f, y_span=%.3f, elong=%.1f, dens=%.2e, n_pts=%d",
                        -pm$x_center, -pm$y_center, pm$intensity,
                        pm$x_span, pm$y_span, pm$elongation, pm$density, pm$n_points))
        message(sprintf("      x_var=%.6f, y_var=%.6f, linearity_y=%.3f, flags=[%s]",
                        pm$x_var, pm$y_var, pm$linearity_y, 
                        if(length(flags) > 0) paste(flags, collapse=", ") else "NONE"))
        
        # ✅ Tester chaque condition du filtre
        message("      Tests de filtre:")
        message(sprintf("        - intensity >= min*0.3: %s (%.3f >= %.3f)",
                        pm$intensity >= min_cluster_intensity * 0.3, pm$intensity, min_cluster_intensity * 0.3))
        message(sprintf("        - !is_horizontal_line: %s", !pm$is_horizontal_line))
        message(sprintf("        - !is_vertical_line: %s", !pm$is_vertical_line))
        message(sprintf("        - !is_thin_vertical: %s", !pm$is_thin_vertical))
      }
    }
  }
  
  
  # --- 6. Redistribution des intensités perdues ---
  if (nrow(invalid_clusters) > 0 && nrow(valid_clusters) > 0) {
    
    for (i in seq_len(nrow(invalid_clusters))) {
      inv <- invalid_clusters[i, ]
      
      # Distance sur l'axe X
      valid_clusters <- valid_clusters %>%
        mutate(x_dist = abs(x_center - inv$x_center))
      
      # Trouver le plus proche sur X (et le plus intense si égalité)
      idx_target <- valid_clusters %>%
        arrange(x_dist, desc(intensity)) %>%
        slice(1) %>%
        pull(stain_id)
      
      # Ajouter l'intensité
      cluster_stats$intensity[cluster_stats$stain_id == idx_target] <-
        cluster_stats$intensity[cluster_stats$stain_id == idx_target] + inv$intensity
    }
    
    message(sprintf("✅ Redistribution vers %d clusters valides.", nrow(valid_clusters)))
  }
  
  # --- 7. Filtrage final des données de contour ---
  contour_data <- contour_data %>% filter(stain_id %in% valid_ids)
  
  
  # --- 8. Centroïdes ---
  centroids <- contour_data %>%
    group_by(stain_id) %>%
    summarise(
      F2_ppm = mean(x, na.rm = TRUE),
      F1_ppm = mean(y, na.rm = TRUE),
      stain_intensity = sum(level, na.rm = TRUE),
      .groups = "drop"
    )
  
  centroids$F2_ppm <- -centroids$F2_ppm
  centroids$F1_ppm <- -centroids$F1_ppm
  
  # --- 9. Bounding boxes ---
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
  
  # --- 10. Suppression centroïdes inclus plus faibles ---
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
  
  # --- 11. Sélection par zone ---
  if (!is.null(keep_peak_ranges) && is.list(keep_peak_ranges)) {
    for (i in seq_along(keep_peak_ranges)) {
      range <- keep_peak_ranges[[i]]
      if (length(range) == 2) {
        centroids_in_range <- centroids %>% filter(F2_ppm >= range[2] & F2_ppm <= range[1])
        num_peaks_to_keep <- if (i <= 1) 1 else 4
        top_peaks <- centroids_in_range %>% arrange(desc(stain_intensity)) %>% slice_head(n = num_peaks_to_keep)
        centroids <- centroids %>% filter(!(F2_ppm >= range[2] & F2_ppm <= range[1])) %>% bind_rows(top_peaks)
        
        boxes_in_range <- bounding_boxes %>% filter(xmin >= range[2] & xmax <= range[1])
        top_boxes <- boxes_in_range %>% arrange(desc(intensity)) %>% slice_head(n = num_peaks_to_keep)
        bounding_boxes <- bounding_boxes %>% filter(!(xmin >= range[2] & xmax <= range[1])) %>% bind_rows(top_boxes)
      }
    }
  }
  
  # --- 12. Renommage ---
  centroids <- centroids %>%
    mutate(stain_id = paste0("peak", seq_len(n()))) %>%
    select(stain_id, F2_ppm, F1_ppm, stain_intensity)
  
  message(sprintf("✅ %d pics valides détectés après filtrage.", nrow(centroids)))
  
  return(list(
    centroids = centroids,
    bounding_boxes = bounding_boxes,
    cluster_stats = cluster_stats  # ✅ NOUVEAU: Retourner les stats pour diagnostic
  ))
}


# ===== FONCTION BONUS: Visualiser les clusters rejetés =====

plot_rejected_clusters <- function(rr_data, process_result) {
  
  # Récupérer les stats
  stats <- process_result$cluster_stats
  valid_ids <- process_result$centroids$stain_id
  
  # Identifier les rejetés
  stats <- stats %>%
    mutate(status = ifelse(stain_id %in% valid_ids, "Valid", "Rejected"))
  
  # Plot
  p <- ggplot(stats, aes(x = elongation, y = intensity_norm, color = status)) +
    geom_point(size = 3, alpha = 0.7) +
    geom_vline(xintercept = c(5, 8, 10, 15), linetype = "dashed", alpha = 0.3) +
    scale_color_manual(values = c("Valid" = "green", "Rejected" = "red")) +
    scale_x_log10() +
    labs(
      title = "Diagnostic: Clusters valides vs rejetés",
      x = "Élongation (log scale)",
      y = "Intensité normalisée",
      color = "Status"
    ) +
    theme_minimal()
  
  print(p)
  
  # Tableau des rejetés
  rejected <- stats %>% 
    filter(status == "Rejected") %>%
    arrange(desc(intensity))
  
  cat("\n=== Top 10 clusters rejetés ===\n")
  print(rejected %>% 
          select(stain_id, x_center, y_center, intensity, elongation, aspect_ratio, density) %>%
          head(10))
  
  invisible(stats)
}

# 
# ## Identify local maxima ----
# 
# library(dplyr)
# library(tidyr)
# 
# process_nmr_centroids_localmax <- function(contour_data, neighborhood_size = 3, min_intensity = NULL) {
#   # contour_data doit contenir au minimum: x, y, level
#   
#   if (nrow(contour_data) == 0) {
#     stop("contour_data est vide")
#   }
#   
#   # Créer une grille complète des points pour rechercher les maxima locaux
#   # On pivotera pour une matrice avec colonnes = unique(x), lignes = unique(y)
#   grid_df <- contour_data %>%
#     select(x, y, level) %>%
#     pivot_wider(names_from = x, values_from = level)
#   
#   # Récupérer les vecteurs x et y uniques (ordonnés)
#   unique_x <- sort(unique(contour_data$x))
#   unique_y <- sort(unique(contour_data$y))
#   
#   # Construire une matrice des intensités
#   intensity_matrix <- as.matrix(grid_df[,-1])  # enlever colonne y (pivot_wider conserve y en 1ère col)
#   rownames(intensity_matrix) <- grid_df$y
#   
#   # Padding de la matrice pour gérer bords
#   pad <- floor(neighborhood_size / 2)
#   padded_mat <- matrix(NA, 
#                        nrow = nrow(intensity_matrix) + 2 * pad, 
#                        ncol = ncol(intensity_matrix) + 2 * pad)
#   padded_mat[(pad+1):(pad+nrow(intensity_matrix)), (pad+1):(pad+ncol(intensity_matrix))] <- intensity_matrix
#   
#   maxima_coords <- list()
#   
#   # Fonction pour vérifier si un point est un maximum local
#   is_local_max <- function(i, j, mat, neighborhood_size) {
#     neighborhood <- mat[(i - pad):(i + pad), (j - pad):(j + pad)]
#     center_value <- mat[i, j]
#     if (is.na(center_value)) return(FALSE)
#     all(center_value >= neighborhood, na.rm = TRUE) && any(center_value > neighborhood, na.rm = TRUE)
#   }
#   
#   # Balayage de la matrice sans les bords
#   for (i in (pad+1):(nrow(padded_mat)-pad)) {
#     for (j in (pad+1):(ncol(padded_mat)-pad)) {
#       if (is_local_max(i, j, padded_mat, neighborhood_size)) {
#         # Récupérer coords x,y correspondants
#         x_val <- unique_x[j - pad]
#         y_val <- unique_y[i - pad]
#         maxima_coords <- append(maxima_coords, list(c(x_val, y_val, padded_mat[i,j])))
#       }
#     }
#   }
#   
#   if (length(maxima_coords) == 0) {
#     warning("Aucun maximum local trouvé")
#     return(list(centroids = NULL, bounding_boxes = NULL))
#   }
#   
#   maxima_df <- do.call(rbind, maxima_coords) %>%
#     as.data.frame()
#   colnames(maxima_df) <- c("x", "y", "intensity")
#   
#   # Filtrer par intensity si demandé
#   if (!is.null(min_intensity)) {
#     maxima_df <- maxima_df %>% filter(intensity >= min_intensity)
#   }
#   
#   # Pour chaque maximum local, définir une boîte englobante autour des points proches
#   bounding_boxes <- maxima_df %>% rowwise() %>%
#     do({
#       cx <- .$x
#       cy <- .$y
#       # Définir un rayon autour du maximum (ex: ±0.01 ppm ou adapté selon données)
#       radius <- 0.01
#       
#       nearby_points <- contour_data %>%
#         filter(abs(x - cx) <= radius, abs(y - cy) <= radius)
#       
#       if (nrow(nearby_points) == 0) {
#         # Au moins une petite boîte autour du centroïde
#         tibble(
#           xmin = cx - radius, xmax = cx + radius,
#           ymin = cy - radius, ymax = cy + radius,
#           intensity = .$intensity
#         )
#       } else {
#         tibble(
#           xmin = min(nearby_points$x),
#           xmax = max(nearby_points$x),
#           ymin = min(nearby_points$y),
#           ymax = max(nearby_points$y),
#           intensity = sum(nearby_points$level)
#         )
#       }
#     }) %>% ungroup()
#   
#   # Renommer colonnes pour cohérence avec ta fonction
#   centroids <- maxima_df %>%
#     rename(F2_ppm = x, F1_ppm = y, stain_intensity = intensity)
#   
#   # Inverser axes pour correspondre à convention NMR (optionnel)
#   centroids <- centroids %>%
#     mutate(
#       F2_ppm = -F2_ppm,
#       F1_ppm = -F1_ppm
#     )
#   
#   bounding_boxes <- bounding_boxes %>%
#     mutate(
#       xmin = -xmax,
#       xmax = -xmin,
#       ymin = -ymax,
#       ymax = -ymin
#     )
#   
#   return(list(
#     centroids = centroids,
#     bounding_boxes = bounding_boxes
#   ))
# }


# Compute local contour intensity around a point (stain)
get_local_stain_intensity <- function(f2_ppm, f1_ppm, contour_data, eps_ppm = 0.0068) {
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

make_bbox_outline <- function(boxes) {
  if (is.null(boxes) || nrow(boxes) == 0) return(NULL)
  
  # S'assurer que stain_id existe
  if (!"stain_id" %in% names(boxes)) {
    boxes$stain_id <- paste0("box_", seq_len(nrow(boxes)))
  }
  
  # ✅ CORRECTION: Créer les contours avec group UNIQUE et NA entre chaque box
  outline_list <- lapply(seq_len(nrow(boxes)), function(i) {
    box <- boxes[i, ]
    
    # Créer un rectangle fermé pour cette box
    rect <- data.frame(
      x = c(box$xmin, box$xmax, box$xmax, box$xmin, box$xmin),
      y = c(box$ymin, box$ymin, box$ymax, box$ymax, box$ymin),
      group = paste0("box_", i),  # ✅ Group unique par box
      stringsAsFactors = FALSE
    )
    
    # ✅ Ajouter une ligne avec NA pour séparer les boxes
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
