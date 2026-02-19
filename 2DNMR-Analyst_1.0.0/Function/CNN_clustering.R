# CNN_clustering.R - DBSCAN Clustering and Bounding Box Generation ----
#
# Part of the CNN Peak Detection module for 2D NMR Spectra
# Contains DBSCAN clustering and bounding box functions
#
# Author: Julien Guibert
# Institution: INRAe Toxalim / MetaboHUB


#' Process Peaks with DBSCAN and Generate Bounding Boxes
#'
#' Takes filtered peaks and applies DBSCAN clustering to group them,
#' then generates bounding boxes for each cluster suitable for Plotly visualization.
#'
#' @param peaks_clean_filtered Data frame with filtered peaks (F1, F2, Intensity)
#' @param rr_norm 2D spectrum matrix
#' @param params List of parameters including eps_value for DBSCAN
#' @param step Integer, downsampling factor for visualization (default: 4)
#'
#' @return List containing:
#'   - peaks: Data frame with cluster centroids (F1, F2, F1_ppm, F2_ppm, stain_intensity, cluster_db)
#'   - boxes: Data frame with bounding box coordinates in ppm
#'   - shapes: List of Plotly shape objects for overlay
#'
#' @details
#' Processing steps:
#' 1. Z-score normalization of peak coordinates
#' 2. DBSCAN clustering with specified epsilon
#' 3. Bounding box calculation for each cluster
#' 4. Conversion from indices to ppm values
#' 5. Generation of Plotly-compatible shape objects
#'
#' @export
process_peaks_with_dbscan <- function(peaks_clean_filtered, rr_norm, params, step = 4) {
  
  # ═══════════════════════════════════════════════════════════════════════════
  # VERIFICATION: Ensure there are peaks to process
  # ═══════════════════════════════════════════════════════════════════════════
  if (is.null(peaks_clean_filtered) || nrow(peaks_clean_filtered) == 0) {
    warning("process_peaks_with_dbscan: Aucun pic à traiter")
    return(list(
      peaks = data.frame(F1 = numeric(0), F2 = numeric(0), F1_ppm = numeric(0), 
                         F2_ppm = numeric(0), stain_intensity = numeric(0), cluster_db = integer(0)),
      boxes = data.frame(xmin_ppm = numeric(0), xmax_ppm = numeric(0), 
                         ymin_ppm = numeric(0), ymax_ppm = numeric(0), 
                         stain_intensity = numeric(0), cluster_db = integer(0)),
      shapes = list()
    ))
  }
  
  # Helper function to downsample matrix for visualization
  downsample_matrix <- function(mat, step = 4) {
    mat[seq(1, nrow(mat), by = step),
        seq(1, ncol(mat), by = step)]
  }
  
  # Helper function to downsample axis values
  downsample_axis <- function(axis_vals, step = 4) {
    axis_vals[seq(1, length(axis_vals), by = step)]
  }
  
  # --- Extract matrix and axes ---
  z_matrix <- rr_norm
  x_vals <- as.numeric(colnames(rr_norm))  # F1 (ppm)
  y_vals <- as.numeric(rownames(rr_norm))  # F2 (ppm)
  
  # Downsample for faster visualization
  z_small <- downsample_matrix(z_matrix, step = step)
  x_small <- downsample_axis(x_vals, step = step)
  y_small <- downsample_axis(y_vals, step = step)
  
  # --- Step 1: Convert the F1/F2 indices to ppm coordinates ---

  peaks_with_ppm <- peaks_clean_filtered %>%
    mutate(
      F1_idx = pmin(pmax(round(F1), 1), length(x_vals)),  # Clipper entre 1 et max
      F2_idx = pmin(pmax(round(F2), 1), length(y_vals)),  # Clipper entre 1 et max
      F1_ppm = x_vals[F1_idx],  # Convertir index -> ppm
      F2_ppm = y_vals[F2_idx]   # Convertir index -> ppm
    )

  
  # Delete the lines with NA (just in case)
  peaks_with_ppm <- peaks_with_ppm %>%
    dplyr::filter(!is.na(F1_ppm) & !is.na(F2_ppm))
  
  if (nrow(peaks_with_ppm) == 0) {
    warning("process_peaks_with_dbscan: No valid peak after ppm conversion")
    return(list(
      peaks = data.frame(F2_ppm = numeric(0), F1_ppm = numeric(0), 
                         stain_intensity = numeric(0), cluster_db = integer(0),
                         stain_id = character(0)),
      boxes = data.frame(xmin = numeric(0), xmax = numeric(0), 
                         ymin = numeric(0), ymax = numeric(0), 
                         stain_intensity = numeric(0), stain_id = character(0)),
      shapes = list()
    ))
  }
  
  # --- Step 2: DBSCAN clustering on PPM coordinates (like Local Max) ---

  eps_cnn <- params$eps_value * 5  
  
  cat(sprintf("DBSCAN avec eps_cnn = %.4f ppm (base eps = %.4f × 5)\n", 
              eps_cnn, params$eps_value))
  
  db <- dbscan::dbscan(peaks_with_ppm[, c("F1_ppm", "F2_ppm")],
                       eps = eps_cnn, minPts = 1)
  peaks_with_ppm <- peaks_with_ppm %>%
    mutate(cluster_db = db$cluster)
  
  # ═══════════════════════════════════════════════════════════════════════════
  # VERIFICATION: Ensure that there are valid clusters (cluster_db > 0)
  # ═══════════════════════════════════════════════════════════════════════════
  peaks_with_clusters <- peaks_with_ppm %>% dplyr::filter(cluster_db > 0)
  
  if (nrow(peaks_with_clusters) == 0) {
    warning("process_peaks_with_dbscan: No clusters found (all peaks are noise)")
    return(list(
      peaks = peaks_with_ppm %>% 
        dplyr::transmute(F2_ppm, F1_ppm, stain_intensity = Intensity, 
                         cluster_db = 0, stain_id = paste0("cnn_noise_", row_number())),
      boxes = data.frame(xmin = numeric(0), xmax = numeric(0), 
                         ymin = numeric(0), ymax = numeric(0), 
                         stain_intensity = numeric(0), stain_id = character(0)),
      shapes = list()
    ))
  }
  
  # --- Step 3: Calculate bounding boxes for each cluster (en ppm) ---
  # Convention NMR: xmin/xmax = F2 (horizontal), ymin/ymax = F1 (vertical)
  bounding_boxes <- peaks_with_clusters %>%
    group_by(cluster_db) %>%
    summarise(
      xmin = min(F2_ppm, na.rm = TRUE),  # F2 = horizontal 
      xmax = max(F2_ppm, na.rm = TRUE),
      ymin = min(F1_ppm, na.rm = TRUE),  # F1 = vertical 
      ymax = max(F1_ppm, na.rm = TRUE),
      center_F2 = (min(F2_ppm, na.rm = TRUE) + max(F2_ppm, na.rm = TRUE)) / 2,
      center_F1 = (min(F1_ppm, na.rm = TRUE) + max(F1_ppm, na.rm = TRUE)) / 2,
      intensity = sum(Intensity, na.rm = TRUE),
      .groups = "drop"
    )

  # ═══════════════════════════════════════════════════════════════════════════
  # Add padding around each box to encompass the entire area
  # Uses the box_padding parameter if provided, otherwise calculates automatically
  # ═══════════════════════════════════════════════════════════════════════════
  padding_ppm <- if (!is.null(params$box_padding)) params$box_padding else (eps_cnn * 1.5)
  
  cat(sprintf("Box padding = %.4f ppm\n", padding_ppm))
  
  bounding_boxes <- bounding_boxes %>%
    mutate(
      xmin = xmin - padding_ppm,
      xmax = xmax + padding_ppm,
      ymin = ymin - padding_ppm,
      ymax = ymax + padding_ppm
    )
  
  # --- Step 4 ---
  bounding_boxes <- bounding_boxes %>%
    mutate(
      stain_intensity = intensity
    )
  
  # --- Step 5: Generate Plotly rectangle shapes ---
  shapes_list <- lapply(seq_len(nrow(bounding_boxes)), function(i) {
    list(
      type = "rect",
      x0 = bounding_boxes$xmin[i], x1 = bounding_boxes$xmax[i],
      y0 = bounding_boxes$ymin[i], y1 = bounding_boxes$ymax[i],
      line = list(color = "red", dash = "dot", width = 2),
      fillcolor = "rgba(0,0,0,0)",  # Transparent fill
      xref = "x", yref = "y",
      layer = "above"
    )
  })
  
  # --- Step 6: Build peaks based on bounding box centers ---

    peaks_from_boxes <- bounding_boxes %>%
    dplyr::transmute(
      F2_ppm = center_F2,  # horizontal
      F1_ppm = center_F1,  # vertical
      stain_intensity = stain_intensity,
      cluster_db = cluster_db,
      stain_id = paste0("cnn_", cluster_db)
    )
  
    # Reformat the boxes to be compatible with the app
    # The app expects: xmin, xmax, ymin, ymax, stain_id
  boxes_formatted <- bounding_boxes %>%
    dplyr::transmute(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      stain_id = paste0("cnn_", cluster_db),
      stain_intensity = stain_intensity
    )
  
  # --- Step 7: Return results ---
  return(list(
    peaks = peaks_from_boxes,
    boxes = boxes_formatted
    # shapes = shapes_list
  ))
}


