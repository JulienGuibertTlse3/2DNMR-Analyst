library(dplyr)
library(zoo)
library(matrixStats)
library(minpack.lm)  # For Gaussian fitting
library(imager)


#' 2D NMR Peak Picking Without Pre-Clustering
#'
#' Identifies local maxima in a 2D NMR spectrum matrix and groups them into peaks
#' using DBSCAN clustering on thresholded intensity points. Applies spectrum-type-specific
#' filtering to distinguish true peaks from artifacts.
#'
#' @param bruker_data Numeric matrix. 2D NMR intensity data with F1 ppm as rownames and F2 ppm as colnames.
#' @param threshold Numeric. Base threshold multiplier (legacy parameter, see threshold_value).
#' @param neighborhood_size Integer. Size of the local neighborhood for peak detection (default 5).
#'   Defines the window for local maximum search.
#' @param prominence_factor Numeric. Factor for prominence threshold calculation (default 0.01).
#'   A point must exceed median + prominence_factor * MAD to be considered a peak.
#' @param adaptive_peak_threshold Numeric. Fraction of top intensities to consider as potential peaks (default 0.001).
#' @param threshold_value Numeric. Absolute intensity threshold. Points below this are ignored.
#' @param f2_exclude_range Numeric vector of length 2. F2 ppm range to exclude (e.g., water region).
#' @param keep_peak_ranges List of numeric vectors. Specific F2 ranges where only top peaks are kept.
#' @param box_window_size Integer. Window size for bounding box calculation (default 11).
#' @param intensity_fraction Numeric. Fraction of peak intensity for bounding box edges (default 0.5).
#' @param box_padding_f2 Numeric. Padding added to bounding boxes in F2 dimension (default 0.001 ppm).
#' @param box_padding_f1 Numeric. Padding added to bounding boxes in F1 dimension (default 0.005 ppm).
#' @param min_cluster_intensity Numeric. Minimum total intensity for a cluster to be valid (default 0.03).
#' @param spectrum_type Character. Type of NMR experiment: "HSQC", "TOCSY", "COSY", or "UFCOSY" (default "COSY").
#'   Different filtering criteria are applied based on spectrum type.
#' @param eps_value Numeric. DBSCAN epsilon parameter for clustering (default 0.02).
#' @param verbose Logical. Print diagnostic messages (default TRUE).
#' @param show_borderline Logical. Show borderline cases in diagnostics (default TRUE).
#' @param diagnose_zones List. Specific zones to diagnose in detail.
#' @param diagnose_radius Numeric. Radius around diagnostic zones (default 0.1 ppm).
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{peaks} - Data frame of detected peaks with stain_id, F2_ppm, F1_ppm, Volume
#'     \item \code{bounding_boxes} - Data frame with xmin, xmax, ymin, ymax for each peak
#'     \item \code{cluster_stats} - Data frame with detailed statistics for each cluster (for diagnostics)
#'   }
#'
#' @details
#' The algorithm proceeds in several steps:
#' \enumerate{
#'   \item Threshold the intensity matrix to find significant points
#'   \item Cluster adjacent points using DBSCAN to identify connected regions
#'   \item Identify local maxima within each region using neighborhood analysis
#'   \item Calculate cluster statistics (span, elongation, density, etc.)
#'   \item Apply spectrum-type-specific filters to reject artifacts
#'   \item Merge peaks by contour and calculate weighted centroids
#'   \item Remove weaker peaks contained within stronger peak bounding boxes
#' }
#'
#' Artifact detection includes:
#' \itemize{
#'   \item Horizontal lines (t1 noise, baseline artifacts)
#'   \item Vertical lines (bleeding, decoupling sidebands)
#'   \item Thin vertical streaks (truncation artifacts)
#' }
#'
#' @examples
#' \dontrun{
#' result <- peak_pick_2d_nt2(
#'   bruker_data = spectrum$spectrumData,
#'   threshold_value = 5000,
#'   spectrum_type = "TOCSY",
#'   f2_exclude_range = c(4.7, 5.0)
#' )
#'
#' # Access detected peaks
#' peaks <- result$peaks
#' boxes <- result$bounding_boxes
#' }
#'
#' @export
## Peak picking (no clustering) ----

peak_pick_2d_nt2 <- function(bruker_data, threshold = 5, neighborhood_size = 5,
                             prominence_factor = 0.01, adaptive_peak_threshold = 0.001,
                             threshold_value = NULL, f2_exclude_range = NULL,
                             keep_peak_ranges = NULL, box_window_size = 11,
                             intensity_fraction = 0.5, 
                             box_padding_f2 = 0.001, box_padding_f1 = 0.005,
                             min_cluster_intensity = 0.03, spectrum_type = "COSY",
                             eps_value = 0.02, verbose = TRUE, show_borderline = TRUE,
                             diagnose_zones = NULL, diagnose_radius = 0.1) {
  
  # --- Input validation ---
  if (is.null(rownames(bruker_data)) || is.null(colnames(bruker_data))) {
    stop("The input object must have row names (F2 ppm) and column names (F1 ppm).")
  }
  
  # Extract chemical shift axes from matrix names
  ppm_x <- as.numeric(rownames(bruker_data))  # F1 ppm (rows)
  ppm_y <- as.numeric(colnames(bruker_data))  # F2 ppm (cols)
  
  # --- Step 1: Threshold the intensity matrix ---
  # Create a binary mask of points above the threshold
  mask <- bruker_data >= threshold_value
  if (!any(mask)) {
    return(list(peaks = data.frame(), bounding_boxes = data.frame(), cluster_stats = data.frame()))
  }
  
  # Get indices of significant points (above threshold)
  significant_points <- which(mask, arr.ind = TRUE)
  
  # === Build data frame with all points above threshold and their ppm coordinates ===
  all_points <- data.frame(
    row_idx = significant_points[, 1],
    col_idx = significant_points[, 2],
    F1_ppm = ppm_x[significant_points[, 1]],
    F2_ppm = ppm_y[significant_points[, 2]],
    Volume = bruker_data[significant_points]
  )
  
  # === Step 2: Cluster contour points to find connected regions ===
  # DBSCAN with eps=1.5 pixels groups adjacent points into contours
  contour_coords <- as.matrix(all_points[, c("row_idx", "col_idx")])
  contour_clustering <- dbscan(contour_coords, eps = 1.5, minPts = 1)
  all_points$contour_id <- contour_clustering$cluster
  
  # === Step 3: Identify local peaks (maxima) within each region ===
  # A point is a local maximum if it's in the top percentile AND has sufficient prominence
  is_peak <- apply(significant_points, 1, function(idx) {
    x <- idx[1]
    y <- idx[2]
    
    # Define neighborhood bounds (handle matrix edges)
    x_range <- max(1, x - floor(neighborhood_size / 2)):min(nrow(bruker_data), x + floor(neighborhood_size / 2))
    y_range <- max(1, y - floor(neighborhood_size / 2)):min(ncol(bruker_data), y + floor(neighborhood_size / 2))
    
    # Extract neighborhood intensities
    neighbors <- as.vector(bruker_data[x_range, y_range])
    median_neighbors <- median(neighbors, na.rm = TRUE)
    mad_neighbors <- mad(neighbors, na.rm = TRUE)  # Median Absolute Deviation
    Volume <- bruker_data[x, y]
    
    # Prominence threshold based on local statistics
    prominence_threshold <- prominence_factor * (median_neighbors + mad_neighbors)
    
    # Adaptive threshold: must be in top percentile of neighborhood
    sorted_neighbors <- sort(neighbors, decreasing = TRUE, na.last = NA)
    top_index <- ceiling(adaptive_peak_threshold * length(sorted_neighbors))
    top_threshold <- ifelse(top_index > 0, sorted_neighbors[top_index], sorted_neighbors[1])
    
    # Check if point qualifies as a local maximum
    is_local_max <- Volume >= top_threshold
    prominence <- Volume - median_neighbors
    
    # Must be both a local max AND have sufficient prominence
    is_local_max && prominence > prominence_threshold
  })
  
  all_points$is_peak <- is_peak
  
  # Filter to keep only local maxima
  peaks_df <- all_points[is_peak, ]
  if (nrow(peaks_df) == 0) {
    return(list(peaks = data.frame(), bounding_boxes = data.frame(), cluster_stats = data.frame()))
  }
  
  # === Step 4: Apply basic filters (water region exclusion) ===
  if (!is.null(f2_exclude_range) && length(f2_exclude_range) == 2) {
    peaks_df <- peaks_df[!(peaks_df$F2_ppm >= f2_exclude_range[1] & peaks_df$F2_ppm <= f2_exclude_range[2]), ]
  }
  
  if (nrow(peaks_df) == 0) {
    return(list(peaks = data.frame(), bounding_boxes = data.frame(), cluster_stats = data.frame()))
  }
  
  # === Step 5: Calculate comprehensive statistics per contour ===
  # These statistics are used for artifact detection and filtering
  valid_contours <- unique(peaks_df$contour_id)
  
  cluster_stats <- all_points %>%
    filter(contour_id %in% valid_contours) %>%
    group_by(contour_id) %>%
    summarise(
      # Basic statistics
      intensity = sum(Volume, na.rm = TRUE),           # Total intensity (volume)
      x_span = max(F2_ppm, na.rm = TRUE) - min(F2_ppm, na.rm = TRUE),  # Width in F2
      y_span = max(F1_ppm, na.rm = TRUE) - min(F1_ppm, na.rm = TRUE),  # Height in F1
      x_center = mean(F2_ppm, na.rm = TRUE),           # Centroid F2
      y_center = mean(F1_ppm, na.rm = TRUE),           # Centroid F1
      n_points = n(),                                   # Number of points in cluster
      
      # Spread statistics (for shape analysis)
      x_sd = sd(F2_ppm, na.rm = TRUE),
      y_sd = sd(F1_ppm, na.rm = TRUE),
      x_var = var(F2_ppm, na.rm = TRUE),
      y_var = var(F1_ppm, na.rm = TRUE),
      
      # Bounding box coordinates
      F2_min = min(F2_ppm, na.rm = TRUE),
      F2_max = max(F2_ppm, na.rm = TRUE),
      F1_min = min(F1_ppm, na.rm = TRUE),
      F1_max = max(F1_ppm, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # Handle NA values from single-point clusters
      x_var = ifelse(is.na(x_var), 0, x_var),
      y_var = ifelse(is.na(y_var), 0, y_var),
      x_sd = ifelse(is.na(x_sd), 0, x_sd),
      y_sd = ifelse(is.na(y_sd), 0, y_sd),
      
      # === Shape metrics for artifact detection ===
      
      # Elongation: ratio of longer to shorter span (always >= 1)
      elongation = pmax(x_span / (y_span + 1e-10), y_span / (x_span + 1e-10)),
      
      # Diagonal detection: TOCSY autopeaks appear on the diagonal (F1 ≈ F2)
      is_diagonal = abs(abs(x_center) - abs(y_center)) < 0.1,
      
      # Aspect ratios (directional)
      aspect_ratio_x = x_span / (y_span + 1e-10),  # >1 means wider than tall
      aspect_ratio_y = y_span / (x_span + 1e-10),  # >1 means taller than wide
      
      # Elongation direction
      is_horizontal = aspect_ratio_x > aspect_ratio_y,
      is_vertical = aspect_ratio_y > aspect_ratio_x,
      
      # === Artifact detection flags ===
      # Horizontal lines: low y variance, wide span, few points (t1 noise)
      is_horizontal_line = is_horizontal & (y_var < 0.0001) & (x_span > 0.01) & (n_points < 100),
      
      # Vertical lines: low x variance, tall span, few points (bleeding artifacts)
      is_vertical_line = is_vertical & (x_var < 1e-09) & (y_span > 0.015) & (n_points < 30),
      
      # Width to height ratio
      width_to_height = x_span / (y_span + 1e-10),
      
      # Thin vertical streaks: very narrow but tall (truncation artifacts)
      is_thin_vertical = is_vertical & (x_span < 0.00002) & (y_span > 0.01),
      
      # === Quality metrics ===
      # Area and density (intensity per unit area)
      area = x_span * y_span,
      density = intensity / (area + 1e-10),
      
      # Coefficient of variation (relative spread)
      cv_x = x_sd / (abs(x_center) + 1e-10),
      cv_y = y_sd / (abs(y_center) + 1e-10),
      
      # Linearity score: 0 = compact/circular, 1 = perfect line
      # Based on comparison of variance to uniform distribution over span
      linearity_x = 1 - pmin(1, x_var / (x_span^2 / 12 + 1e-10)),
      linearity_y = 1 - pmin(1, y_var / (y_span^2 / 12 + 1e-10))
    )
  
  # Calculate density quantiles for adaptive filtering
  max_intensity <- max(cluster_stats$intensity, na.rm = TRUE)
  density_q02 <- quantile(cluster_stats$density, 0.02, na.rm = TRUE)
  density_q05 <- quantile(cluster_stats$density, 0.05, na.rm = TRUE)
  density_q10 <- quantile(cluster_stats$density, 0.10, na.rm = TRUE)
  density_q20 <- quantile(cluster_stats$density, 0.20, na.rm = TRUE)
  density_q30 <- quantile(cluster_stats$density, 0.30, na.rm = TRUE)
  
  # Normalize intensity for relative comparisons
  cluster_stats <- cluster_stats %>%
    mutate(intensity_norm = intensity / max_intensity)
  
  # === Calculate proximity scores to decision thresholds ===
  # These help identify borderline cases for diagnostic purposes
  cluster_stats <- cluster_stats %>%
    mutate(
      # How far from intensity threshold
      intensity_threshold = min_cluster_intensity * 0.3,
      intensity_margin = (intensity - intensity_threshold) / (intensity_threshold + 1e-10),
      
      # Adaptive elongation limits based on peak properties
      elongation_max = case_when(
        is_diagonal ~ 25,           # Diagonal peaks can be more elongated
        intensity_norm > 0.2 ~ 20,  # Strong peaks: more tolerant
        intensity_norm > 0.05 ~ 12, # Medium peaks: moderate
        intensity_norm > 0.01 ~ 10, # Weak peaks: stricter
        TRUE ~ 100                  # Default: very permissive
      ),
      elongation_margin = (elongation_max - elongation) / (elongation_max + 1e-10),
      
      # How far from density threshold
      density_threshold = density_q10,
      density_margin = (density - density_threshold) / (density_threshold + 1e-10),
      
      # Combined borderline score (-1 to 1, negative = likely reject)
      borderline_score = (
        pmin(1, pmax(-1, intensity_margin)) + 
          pmin(1, pmax(-1, elongation_margin)) + 
          pmin(1, pmax(-1, density_margin))
      ) / 3,
      
      # Flag obvious artifacts
      is_artifact = is_horizontal_line | is_vertical_line | is_thin_vertical
    )
  
  # === Step 6: Apply spectrum-type-specific filtering ===
  # Different NMR experiments have different artifact patterns
  
  if (spectrum_type == "HSQC") {
    # HSQC: Simple filtering, peaks are well-separated
    valid_ids <- cluster_stats %>%
      filter(
        intensity >= min_cluster_intensity,
        elongation <= 100  # Very permissive for HSQC
      ) %>%
      pull(contour_id)
    
  } else if (spectrum_type %in% c("TOCSY", "COSY")) {
    # TOCSY/COSY: Complex filtering due to multiplet patterns and bleeding
    valid_ids <- cluster_stats %>%
      pull(contour_id)
    
    # ══════════════════════════════════════════════════════════════════
    # Additional filter: Remove weak peaks in the same F2 column as strong peaks
    # This removes bleeding artifacts that appear as vertical streaks
    # ══════════════════════════════════════════════════════════════════
    
    valid_stats <- cluster_stats %>% filter(contour_id %in% valid_ids)
    
    if (nrow(valid_stats) > 1) {
      
      f2_tolerance <- 0.02  # ppm - peaks considered on the same "column"
      intensity_ratio_threshold <- 0.05 # Keep only if > 5% of the column's maximum peak
      
      # Group peaks by F2 position and find column maxima
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
      
      # Keep if: dominant peak OR sufficient ratio OR column with few peaks
      valid_ids <- valid_stats %>%
        filter(
          intensity == max_intensity_in_column |  # Always keep the most intense
            intensity_ratio >= intensity_ratio_threshold |  # Quite intense relatively
            n_peaks_in_column <= 2 |  # No problem if there are few peaks
            is_diagonal  # Always keep the diagonals
        ) %>%
        pull(contour_id)
    }
    
  } else {  # UFCOSY
    # UFCOSY: Similar to COSY but with adjusted thresholds
    valid_ids <- cluster_stats %>%
      filter(
        intensity >= min_cluster_intensity * 0.5,
        (
          (intensity_norm > 0.2 & elongation <= 20) |
            (intensity_norm > 0.05 & elongation <= 12) |
            (elongation <= 20) |
            # Vertical multiplets (thin but with good density)
            (is_vertical & density > density_q05 & n_points >= 15 & elongation <= 50)
        ),
        !(is_horizontal & aspect_ratio_x > 20 & y_span < 0.03),
        density > density_q05 | intensity_norm > 0.1,
        # Do not reject vertical artifacts if it is a probable multiplet
        !(is_vertical_line & density <= density_q05),
        !(is_thin_vertical & n_points < 15)
      ) %>%
      pull(contour_id)
  }
  
  # Add validation status to cluster stats for diagnostics
  cluster_stats <- cluster_stats %>%
    mutate(status = case_when(
      contour_id %in% valid_ids ~ "ACCEPTED",
      is_artifact ~ "REJECTED_ARTIFACT",
      TRUE ~ "REJECTED"
    ))
  
  valid_clusters <- cluster_stats %>% filter(status == "ACCEPTED")
  invalid_clusters <- cluster_stats %>% filter(status != "ACCEPTED")
  
  # === Redistribute intensities from rejected clusters to nearest valid cluster ===
  # This preserves total intensity for quantitative analysis
  if (nrow(invalid_clusters) > 0 && nrow(valid_clusters) > 0) {
    for (i in seq_len(nrow(invalid_clusters))) {
      inv <- invalid_clusters[i, ]
      
      # Find distance to each valid cluster (along F2 axis)
      valid_clusters <- valid_clusters %>%
        mutate(x_dist = abs(x_center - inv$x_center))
      
      # Transfer intensity to nearest valid cluster (or most intense if tied)
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
  
  # === Step 7: Merge peaks by contour and calculate weighted centroids ===
  peak_list <- peaks_df %>%
    filter(contour_id %in% valid_ids) %>%
    group_by(contour_id) %>%
    summarise(
      F2_ppm = weighted.mean(F2_ppm, Volume, na.rm = TRUE),  # Intensity-weighted centroid
      F1_ppm = weighted.mean(F1_ppm, Volume, na.rm = TRUE),
      Volume = sum(Volume, na.rm = TRUE),  # Total volume
      .groups = "drop"
    )
  
  # === Join bounding box coordinates ===
  bounding_boxes_data <- valid_clusters %>%
    select(contour_id, F2_min, F2_max, F1_min, F1_max)
  
  peak_list <- peak_list %>%
    left_join(bounding_boxes_data, by = "contour_id")
  
  # === Step 8: Selection by specific F2 ranges (if specified) ===
  # Keeps only top N peaks in each specified range
  if (!is.null(keep_peak_ranges) && is.list(keep_peak_ranges)) {
    filtered_peaks <- data.frame()
    
    for (i in seq_along(keep_peak_ranges)) {
      range <- keep_peak_ranges[[i]]
      
      if (length(range) == 2) {
        lower_bound <- min(range)
        upper_bound <- max(range)
        
        peaks_in_range <- peak_list %>%
          dplyr::filter(F2_ppm >= lower_bound & F2_ppm <= upper_bound)
        
        # Keep fewer peaks in first range (typically reference peak region)
        num_peaks_to_keep <- if (i <= 1) 1 else 4
        
        top_peaks_in_range <- peaks_in_range %>%
          dplyr::arrange(desc(Volume)) %>%
          dplyr::slice_head(n = num_peaks_to_keep)
        
        filtered_peaks <- dplyr::bind_rows(filtered_peaks, top_peaks_in_range)
      }
    }
    
    # Keep peaks outside any specified range
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
  
  # === Step 9: Remove weaker peaks contained within stronger peaks ===
  # If a peak centroid falls inside another peak's bounding box and is weaker, remove it
  if (nrow(peak_list) > 1) {
    to_remove <- c()
    for (i in seq_len(nrow(peak_list))) {
      current <- peak_list[i, ]
      for (j in seq_len(nrow(peak_list))) {
        if (i == j) next
        compare <- peak_list[j, ]
        
        # Check if current peak's centroid is inside compare's bounding box
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
  
  # === Step 10: Finalize output ===
  # Rename peaks with sequential IDs and build bounding boxes with padding
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
  
  # Keep only essential columns in final output
  peak_list <- peak_list %>%
    select(stain_id, F2_ppm, F1_ppm, Volume)
  
  
  return(list(
    peaks = peak_list, 
    bounding_boxes = bounding_boxes, 
    cluster_stats = cluster_stats
  ))
}


#' Filter Residual Noise Peaks in TOCSY Spectra
#'
#' Removes isolated low-intensity peaks that are likely noise artifacts.
#' A peak is kept if it has enough neighbors or sufficient relative intensity.
#'
#' @param peaks Data frame. Must contain F2_ppm, F1_ppm, and Volume columns.
#' @param min_neighbors Integer. Minimum number of neighboring peaks required (default 4).
#' @param neighbor_radius Numeric. Radius in ppm to search for neighbors (default 0.03).
#' @param min_relative_intensity Numeric. Minimum intensity relative to maximum (default 0.03).
#'
#' @return Data frame with isolated noise peaks removed.
#'
#' @details
#' A peak is kept if either:
#' \itemize{
#'   \item It has at least \code{min_neighbors} peaks within \code{neighbor_radius}
#'   \item Its intensity is at least \code{min_relative_intensity} times the maximum
#' }
#' This filter helps remove random noise spikes that appear as isolated points.
#'
#' @examples
#' \dontrun{
#' clean_peaks <- filter_noise_peaks(peaks, min_neighbors = 4, neighbor_radius = 0.03)
#' }
#'
#' @export
# ---- Filtering of residual noise peaks in TOCSY spectra ----

filter_noise_peaks <- function(peaks, min_neighbors = 4, neighbor_radius = 0.03, min_relative_intensity = 0.03) {
  # No filtering needed for very small datasets
  if (nrow(peaks) < 2) return(peaks)
  
  peaks <- peaks %>%
    dplyr::mutate(
      keep = sapply(seq_len(n()), function(i) {
        x <- peaks$F2_ppm[i]
        y <- peaks$F1_ppm[i]
        intensity <- peaks$Volume[i]
        max_intensity <- max(peaks$Volume)
        
        # Calculate Euclidean distance to all other peaks
        dist <- sqrt((peaks$F2_ppm - x)^2 + (peaks$F1_ppm - y)^2)
        neighbors <- sum(dist < neighbor_radius) - 1  # Exclude itself
        
        # Keep if: has enough neighbors OR has sufficient relative intensity
        neighbors >= min_neighbors || (intensity / max_intensity > min_relative_intensity)
      })
    ) %>%
    dplyr::filter(keep) %>%
    dplyr::select(-keep)
  
  return(peaks)
}


#' Process NMR Centroids Using DBSCAN Clustering
#'
#' Applies DBSCAN clustering to contour data to identify and characterize peaks.
#' Uses scaled coordinates and spectrum-type-specific parameters.
#'
#' @param rr_data Numeric matrix. Original 2D NMR intensity data (for reference).
#' @param contour_data Data frame. Contour coordinates from ggplot_build(), must contain x, y, level, group.
#' @param contour_num Integer. Number of contour levels (for reference).
#' @param contour_factor Numeric. Contour level multiplier (for reference).
#' @param intensity_threshold Numeric. Minimum intensity threshold.
#' @param keep_peak_ranges List. Specific F2 ranges for peak selection.
#' @param eps_value Numeric. DBSCAN epsilon parameter.
#' @param min_cluster_intensity Numeric. Minimum total intensity for valid clusters (default 0.03).
#' @param spectrum_type Character. Type of NMR experiment: "HSQC", "TOCSY", "COSY", or "UFCOSY".
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{centroids} - Data frame with stain_id, F2_ppm, F1_ppm, Volume
#'     \item \code{bounding_boxes} - Data frame with bounding box coordinates
#'     \item \code{cluster_stats} - Data frame with cluster statistics
#'   }
#'
#' @details
#' This function clusters contour points using DBSCAN with coordinates scaled
#' differently based on spectrum type:
#' \itemize{
#'   \item HSQC: F1 dimension scaled by factor of 5 (due to different ppm ranges)
#'   \item TOCSY/COSY: Both dimensions scaled equally
#' }
#'
#' Cluster statistics include shape metrics (elongation, aspect ratio, linearity)
#' that are used to distinguish true peaks from artifacts.
#'
#' @examples
#' \dontrun{
#' result <- process_nmr_centroids(
#'   rr_data = spectrum$spectrumData,
#'   contour_data = contour_result$contour_data,
#'   eps_value = 0.3,
#'   spectrum_type = "TOCSY"
#' )
#' }
#'
#' @export
# Peak picking dbscan ----

process_nmr_centroids <- function(rr_data, contour_data, contour_num = NULL, contour_factor = NULL, 
                                  intensity_threshold = NULL, keep_peak_ranges = NULL, eps_value = NULL,
                                  min_cluster_intensity = 0.03, spectrum_type = "cosy") {
  
  # --- Input validation ---
  if (nrow(contour_data) == 0 || !"group" %in% colnames(contour_data)) {
    stop("No contours data available for processing.")
  }
  
  # --- Step 2: Standardize coordinates for clustering ---
  # Different scaling for HSQC due to different F1/F2 ppm ranges (C vs H)
  if (spectrum_type == "HSQC") {
    # HSQC: F1 (13C) has much larger ppm range than F2 (1H)
    # Scale F1 by factor of 5 to make clustering more balanced
    contour_data <- contour_data %>%
      mutate(
        F2_scaled = (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE),
        F1_scaled = (y - mean(y, na.rm = TRUE)) / (sd(y, na.rm = TRUE) * 5)
      )
    min_pts <- 10  # HSQC peaks should have more points
  } else {
    # TOCSY/COSY: Both dimensions are 1H with similar ppm ranges
    contour_data <- contour_data %>%
      mutate(
        F2_scaled = (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE),
        F1_scaled = (y - mean(y, na.rm = TRUE)) / sd(y, na.rm = TRUE)
      )
    min_pts <- 0  # More permissive for homonuclear spectra
  }
  
  # --- Step 3: DBSCAN clustering ---
  # Groups nearby contour points into clusters (potential peaks)
  clusters <- dbscan::dbscan(contour_data[, c("F2_scaled", "F1_scaled")], eps = eps_value, minPts = min_pts)
  contour_data$stain_id <- as.character(clusters$cluster)
  
  # Remove noise points (cluster 0)
  contour_data <- contour_data %>% filter(stain_id != "0")
  
  # --- Step 4: Calculate comprehensive cluster statistics ---
  cluster_stats <- contour_data %>%
    group_by(stain_id) %>%
    summarise(
      # Basic statistics
      intensity = sum(level, na.rm = TRUE),    # Total intensity (sum of contour levels)
      x_span = max(x, na.rm = TRUE) - min(x, na.rm = TRUE),  # Width in F2
      y_span = max(y, na.rm = TRUE) - min(y, na.rm = TRUE),  # Height in F1
      x_center = mean(x, na.rm = TRUE),        # Centroid F2
      y_center = mean(y, na.rm = TRUE),        # Centroid F1
      n_points = n(),                           # Number of contour points
      
      # Spread statistics for shape analysis
      x_sd = sd(x, na.rm = TRUE),
      y_sd = sd(y, na.rm = TRUE),
      x_var = var(x, na.rm = TRUE),            # Variance for detecting linear artifacts
      y_var = var(y, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # === Shape metrics ===
      
      # Elongation: how stretched is the cluster (ratio of max to min span)
      elongation = pmax(x_span / (y_span + 1e-10), y_span / (x_span + 1e-10)),
      
      # Diagonal detection for TOCSY (autopeaks on F1=F2 line)
      is_diagonal = abs(abs(x_center) - abs(y_center)) < 0.1,
      
      # Directional aspect ratios
      aspect_ratio_x = x_span / (y_span + 1e-10),
      aspect_ratio_y = y_span / (x_span + 1e-10),
      
      # Elongation direction
      is_horizontal = aspect_ratio_x > aspect_ratio_y,
      is_vertical = aspect_ratio_y > aspect_ratio_x,
      
      # === Artifact detection flags ===
      
      # Horizontal line artifacts: very low y variance, wide, few points
      is_horizontal_line = is_horizontal & (y_var < 0.0001) & (x_span > 0.08) & (n_points < 200),
      
      # Vertical line artifacts: very low x variance, tall, few points
      is_vertical_line = is_vertical & (x_var < 0.00005) & (y_span > 0.15) & (n_points < 200),
      
      # Width to height ratio for shape analysis
      width_to_height = x_span / (y_span + 1e-10),
      
      # Thin vertical artifacts: very narrow but tall
      is_thin_vertical = is_vertical & (x_span < 0.015) & (y_span > 0.1),
      
      # === Quality metrics ===
      
      # Area and density
      area = x_span * y_span,
      density = intensity / (area + 1e-10),
      
      # Coefficient of variation (relative spread)
      cv_x = x_sd / (abs(x_center) + 1e-10),
      cv_y = y_sd / (abs(y_center) + 1e-10),
      
      # Linearity scores: how much does the cluster resemble a line?
      # 0 = compact/circular, 1 = perfect line
      # Compares variance to theoretical uniform distribution over span
      linearity_x = 1 - pmin(1, x_var / (x_span^2 / 12 + 1e-10)),
      linearity_y = 1 - pmin(1, y_var / (y_span^2 / 12 + 1e-10))
    )
  
  # --- Step 4.5: Normalize intensity for adaptive thresholds ---
  max_intensity <- max(cluster_stats$intensity, na.rm = TRUE)
  cluster_stats <- cluster_stats %>%
    mutate(intensity_norm = intensity / max_intensity)
  
  
  # --- Step 5: Apply spectrum-type-specific filtering ---
  
  if (spectrum_type == "HSQC") {
    # HSQC: Simple filtering, peaks are well-defined
    valid_ids <- cluster_stats %>%
      filter(
        intensity >= min_cluster_intensity,
        elongation <= 100  # Very permissive
      ) %>%
      pull(stain_id)
    
  } else if (spectrum_type %in% c("TOCSY", "COSY")) {
    
    # TOCSY/COSY: Complex multi-criteria filtering
    valid_ids <- cluster_stats %>%
      filter(
        # Criterion 1: Minimum intensity with exceptions for well-structured peaks
        (
          intensity >= min_cluster_intensity * 0.3 |
            # Exception: Peaks with excellent morphological characteristics
            (elongation <= 8 & n_points >= 50 & density > quantile(cluster_stats$density, 0.3, na.rm = TRUE)) |
            # Exception: Vertical multiplets with many points
            (is_vertical & x_span >= 0.02 & n_points >= 100 & density > quantile(cluster_stats$density, 0.2, na.rm = TRUE))
        ),
        
        # Criterion 2: Reject obvious line artifacts
        !is_horizontal_line,
        !is_vertical_line,
        !is_thin_vertical,
        
        # Criterion 3: Adaptive elongation limits based on peak properties
        (
          # Case A: Diagonal peaks (autopeaks) - more tolerant
          (is_diagonal & elongation <= 25) |
            # Case B: Very intense peaks - tolerant
            (intensity_norm > 0.2 & elongation <= 20) |
            # Case C: True vertical multiplets (not too narrow)
            (is_vertical & x_span >= 0.02 & elongation <= 30 & (linearity_y < 0.9 | n_points >= 200)) |
            # Case D: Medium intensity peaks - moderate
            (intensity_norm > 0.05 & elongation <= 12) |
            # Case E: Weak peaks - stricter
            (intensity_norm > 0.01 & elongation <= 10) |
            # Case F: Very weak but compact peaks
            (elongation <= 6) |
            # Case G: Well-structured vertical peaks (many points)
            (is_vertical & x_span >= 0.025 & n_points >= 100 & elongation <= 35)
        ),
        
        # Criterion 4: Minimum density (relaxed)
        density > quantile(cluster_stats$density, 0.02, na.rm = TRUE) |
          intensity_norm > 0.05 |
          elongation <= 6 |
          n_points >= 100,  # Well-structured peaks exempted
        
        # Criterion 5: Minimum number of points (relaxed)
        n_points >= 1 |
          intensity_norm > 0.05
        
      ) %>%
      
      pull(stain_id)
    
  } else {  # UFCOSY
    
    # UFCOSY: Relaxed criteria compared to standard COSY
    valid_ids <- cluster_stats %>%
      
      filter(
        intensity >= min_cluster_intensity * 0.5,
        
        # Relaxed adaptive elongation
        ((intensity_norm > 0.2 & elongation <= 20) |
           (intensity_norm > 0.05 & elongation <= 12) |
           (elongation <= 8)
        ),
        
        # Only reject very obvious artifacts
        !(is_horizontal & aspect_ratio_x > 20 & y_span < 0.03),
        density > quantile(cluster_stats$density, 0.05, na.rm = TRUE) | intensity_norm > 0.1
      ) %>%
      pull(stain_id)
  }
  
  
  valid_clusters <- cluster_stats %>% filter(stain_id %in% valid_ids)
  invalid_clusters <- cluster_stats %>% filter(!stain_id %in% valid_ids)
  
  # --- Step 6: Redistribute intensities from rejected to valid clusters ---
  if (nrow(invalid_clusters) > 0 && nrow(valid_clusters) > 0) {
    
    for (i in seq_len(nrow(invalid_clusters))) {
      inv <- invalid_clusters[i, ]
      
      # Distance along the F2 axis
      valid_clusters <- valid_clusters %>%
        mutate(x_dist = abs(x_center - inv$x_center))
      
      # Find the closest valid cluster (or most intense if tied)
      idx_target <- valid_clusters %>%
        arrange(x_dist, desc(intensity)) %>%
        slice(1) %>%
        pull(stain_id)
      
      # Transfer intensity to target cluster
      cluster_stats$intensity[cluster_stats$stain_id == idx_target] <-
        cluster_stats$intensity[cluster_stats$stain_id == idx_target] + inv$intensity
    }
    
    message(sprintf("✅ Redistribution to %d valid clusters.", nrow(valid_clusters)))
  }
  
  # --- Step 7: Final filtering of contour data ---
  contour_data <- contour_data %>% filter(stain_id %in% valid_ids)
  
  
  # --- Step 8: Calculate centroids (intensity-weighted mean position) ---
  centroids <- contour_data %>%
    group_by(stain_id) %>%
    summarise(
      F2_ppm = mean(x, na.rm = TRUE),
      F1_ppm = mean(y, na.rm = TRUE),
      Volume = sum(level, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Convert to positive ppm values (contour data uses negative)
  centroids$F2_ppm <- -centroids$F2_ppm
  centroids$F1_ppm <- -centroids$F1_ppm
  
  # --- Step 9: Calculate bounding boxes ---
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
  
  # --- Step 10: Remove weaker centroids contained within stronger peaks ---
  if (nrow(centroids) > 1) {
    centroids_with_boxes <- centroids %>%
      left_join(bounding_boxes, by = "stain_id")
    
    to_remove <- c()
    for (i in seq_len(nrow(centroids_with_boxes))) {
      current <- centroids_with_boxes[i, ]
      for (j in seq_len(nrow(centroids_with_boxes))) {
        if (i == j) next
        compare <- centroids_with_boxes[j, ]
        
        # Check if current centroid is inside compare's box
        inside_box <- current$F2_ppm >= compare$xmin & current$F2_ppm <= compare$xmax &
          current$F1_ppm >= compare$ymin & current$F1_ppm <= compare$ymax
        lower_intensity <- current$Volume < compare$intensity
        
        if (inside_box && lower_intensity) {
          to_remove <- c(to_remove, current$stain_id)
          break
        }
      }
    }
    centroids <- centroids %>% filter(!stain_id %in% to_remove)
    bounding_boxes <- bounding_boxes %>% filter(!stain_id %in% to_remove)
  }
  
  # --- Step 11: Apply range-based selection (if specified) ---
  if (!is.null(keep_peak_ranges) && is.list(keep_peak_ranges)) {
    for (i in seq_along(keep_peak_ranges)) {
      range <- keep_peak_ranges[[i]]
      if (length(range) == 2) {
        centroids_in_range <- centroids %>% filter(F2_ppm >= range[2] & F2_ppm <= range[1])
        num_peaks_to_keep <- if (i <= 1) 1 else 4
        top_peaks <- centroids_in_range %>% arrange(desc(Volume)) %>% slice_head(n = num_peaks_to_keep)
        centroids <- centroids %>% filter(!(F2_ppm >= range[2] & F2_ppm <= range[1])) %>% bind_rows(top_peaks)
        
        boxes_in_range <- bounding_boxes %>% filter(xmin >= range[2] & xmax <= range[1])
        top_boxes <- boxes_in_range %>% arrange(desc(intensity)) %>% slice_head(n = num_peaks_to_keep)
        bounding_boxes <- bounding_boxes %>% filter(!(xmin >= range[2] & xmax <= range[1])) %>% bind_rows(top_boxes)
      }
    }
  }
  
  # --- Step 12: Rename peaks with sequential IDs ---
  centroids <- centroids %>%
    mutate(stain_id = paste0("peak", seq_len(n()))) %>%
    select(stain_id, F2_ppm, F1_ppm, Volume)
  
  message(sprintf("✅ %d valid peaks detected after filtering.", nrow(centroids)))
  
  return(list(
    centroids = centroids,
    bounding_boxes = bounding_boxes,
    cluster_stats = cluster_stats  # Return stats for diagnostic purposes
  ))
}