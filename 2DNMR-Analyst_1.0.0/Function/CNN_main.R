# CNN_main.R - Main Entry Point Function ----
#
# Part of the CNN Peak Detection module for 2D NMR Spectra
# Contains the main pipeline function run_cnn_peak_picking()
#
# Author: Julien Guibert
# Institution: INRAe Toxalim / MetaboHUB


#' Run CNN Peak Picking Pipeline
#'
#' Main entry point for CNN-based peak detection. Combines detection,
#' filtering, and clustering into a single function call.
#'
#' @param rr_norm Normalized 2D spectrum matrix
#' @param model Compiled Keras CNN model
#' @param params List of parameters including:
#'   - pred_class_thres: Classification probability threshold
#'   - int_thres: Intensity threshold
#'   - eps_value: DBSCAN epsilon parameter
#' @param method Character, "batch" (faster) or "classique" (sequential)
#' @param target_length Integer, CNN input size (default: 2048)
#' @param threshold_class Numeric, probability threshold for peaks
#' @param batch_size Integer, batch size for batch method (default: 64)
#' @param step Integer, downsampling factor for visualization (default: 4)
#' @param verbose Logical, print progress messages
#'
#' @return List containing:
#'   - peaks: Data frame with detected peak centroids
#'   - boxes: Data frame with bounding boxes
#'   - shapes: List of Plotly shape objects
#'
#' @details
#' Processing pipeline:
#' 1. Peak detection using CNN (batch or sequential method)
#' 2. Filtering by proportion and intensity
#' 3. Removal of noisy columns
#' 4. DBSCAN clustering
#' 5. Bounding box generation
#'
#' @examples
#' result <- run_cnn_peak_picking(
#'   rr_norm = spectrum_matrix,
#'   model = cnn_model,
#'   params = list(pred_class_thres = 0.5, int_thres = 1000, eps_value = 0.01),
#'   method = "batch"
#' )
#' peaks <- result$peaks
#' boxes <- result$boxes
#'
#' @export
run_cnn_peak_picking <- function(rr_norm, model, params,
                                 method = c("batch", "classique"),
                                 target_length = 2048,
                                 threshold_class = params$pred_class_thres,
                                 batch_size = 64, step = 4,
                                 verbose = TRUE) {
  

  # === Input validation ===
  if (is.null(rr_norm) || !is.matrix(rr_norm)) {
    stop("rr_norm must be a normalized matrix")
  }
  if (is.null(model)) {
    stop("CNN model is not loaded")
  }
  
  method <- match.arg(method)  # Validate method argument
  
  n_row <- nrow(rr_norm)
  n_col <- ncol(rr_norm)
  
  # Utility function for padding sequences
  pad_sequence <- function(x, target_length) {
    current_length <- length(x)
    if (current_length < target_length) {
      c(x, rep(0, target_length - current_length))
    } else {
      x
    }
  }
  
  # Safe prediction wrapper
  safe_predict <- function(model, x) {
    x_tensor <- tensorflow::tf$convert_to_tensor(x, dtype = tensorflow::tf$float32)
    preds <- model$`__call__`(x_tensor, training = FALSE)
    list(preds[[1]]$numpy(), preds[[2]]$numpy())
  }
  
  detected_peaks <- data.frame(F2 = numeric(0), F1 = numeric(0), Intensity = numeric(0))
  
  # === Step 1: Peak Detection ===
  if (method == "classique") {
    if (verbose) cat("Using sequential method: row + column detection\n")
    
    # Row-wise detection
    for (i in seq_len(n_row)) {
      spec1D_row <- rr_norm[i, ]
      spec1D_row_padded <- pad_sequence(spec1D_row, target_length)
      input_tensor <- array(spec1D_row_padded, dim = c(1, target_length, 1))
      
      pred_row <- safe_predict(model, input_tensor)
      class_labels <- apply(pred_row[[1]][1, 1:length(spec1D_row), ], 1, which.max) - 1
      reg_pred <- pred_row[[2]][1, 1:length(spec1D_row), ]
      
      idx <- which(class_labels %in% c(1, 2))
      if (length(idx) > 0) {
        peaks <- data.frame(
          F2 = as.numeric(rownames(rr_norm))[i],
          F1 = as.numeric(colnames(rr_norm))[idx],
          Intensity = reg_pred[idx, 2]
        )
        detected_peaks <- rbind(detected_peaks, peaks)
      }
    }
    
    # Column-wise detection
    for (j in seq_len(n_col)) {
      spec1D_col <- rr_norm[, j]
      spec1D_col_padded <- pad_sequence(spec1D_col, target_length)
      input_tensor <- array(spec1D_col_padded, dim = c(1, target_length, 1))
      
      pred_col <- safe_predict(model, input_tensor)
      class_labels <- apply(pred_col[[1]][1, 1:length(spec1D_col), ], 1, which.max) - 1
      reg_pred <- pred_col[[2]][1, 1:length(spec1D_col), ]
      
      idx <- which(class_labels %in% c(1, 2))
      if (length(idx) > 0) {
        peaks <- data.frame(
          F2 = as.numeric(rownames(rr_norm))[idx],
          F1 = as.numeric(colnames(rr_norm))[j],
          Intensity = reg_pred[idx, 2]
        )
        detected_peaks <- rbind(detected_peaks, peaks)
      }
    }
    
  } else if (method == "batch") {
    if (verbose) cat("Using batch method: predict_peaks_1D_batch()\n")
    
    # Batch prediction on rows
    peaks_rows <- predict_peaks_1D_batch(rr_norm, model,
                                         axis = "rows",
                                         threshold_class = threshold_class,
                                         batch_size = batch_size,
                                         verbose = verbose)
    # Batch prediction on columns
    peaks_cols <- predict_peaks_1D_batch(rr_norm, model,
                                         axis = "columns",
                                         threshold_class = threshold_class,
                                         batch_size = batch_size,
                                         verbose = verbose)
    # Combine and deduplicate
    detected_peaks <- rbind(peaks_rows, peaks_cols) %>% unique()
  }
  
  
  # === Step 2: Initial Filtering ===
  if (nrow(detected_peaks) == 0) {
    warning("No peaks detected")
    return(list(peaks = NULL, boxes = NULL, shapes = NULL))
  }
  
  # ═══════════════════════════════════════════════════════════════════════════
  # TOCSY TRACE FILTER: For each F2 line, compare the peaks
  
  # and remove those that are much weaker than the strongest peak
  
  # This eliminates the "trails" (t1 noise) along the intense peaks
  # ═══════════════════════════════════════════════════════════════════════════
  trace_filter_ratio <- if (!is.null(params$trace_filter_ratio)) params$trace_filter_ratio else 0.1  # 10% par défaut
  f2_tolerance_ppm <- 0.02  # Tolerance in ppm for grouping peaks "on the same line F2"
  
  if (trace_filter_ratio > 0) {
    # Retrieve ppm values
    y_vals <- as.numeric(rownames(rr_norm))  # F2 (ppm)
    
    # Retrieve the actual intensity of the spectrum for each peak AND convert F2 to ppm
    detected_peaks$real_intensity <- sapply(seq_len(nrow(detected_peaks)), function(i) {
      f2_idx <- round(detected_peaks$F2[i])
      f1_idx <- round(detected_peaks$F1[i])
      f2_idx <- max(1, min(f2_idx, nrow(rr_norm)))
      f1_idx <- max(1, min(f1_idx, ncol(rr_norm)))
      rr_norm[f2_idx, f1_idx]
    })
    
    # Convert F2 indices to ppm
    detected_peaks$F2_ppm_temp <- sapply(detected_peaks$F2, function(f2_idx) {
      f2_idx <- max(1, min(round(f2_idx), length(y_vals)))
      y_vals[f2_idx]
    })
    
    # Group the peaks by F2 ppm (with a tolerance of 0.02 ppm)
    detected_peaks$f2_group <- round(detected_peaks$F2_ppm_temp / f2_tolerance_ppm) * f2_tolerance_ppm
    
    # For each group F2, calculate the maximum intensity
    group_max <- tapply(detected_peaks$real_intensity, detected_peaks$f2_group, max)
    detected_peaks$group_max_intensity <- group_max[as.character(detected_peaks$f2_group)]
    
    # Calculate the ratio relative to the group's maximum
    detected_peaks$intensity_ratio <- detected_peaks$real_intensity / detected_peaks$group_max_intensity
    
    # Filter: keep only peaks with a ratio >= threshold
    n_before_trace <- nrow(detected_peaks)
    detected_peaks <- detected_peaks[detected_peaks$intensity_ratio >= trace_filter_ratio, ]
    
    # Clean the temporary columns
    detected_peaks$f2_group <- NULL
    detected_peaks$group_max_intensity <- NULL
    detected_peaks$intensity_ratio <- NULL
    detected_peaks$F2_ppm_temp <- NULL
    
    if (nrow(detected_peaks) == 0) {
      warning("No peaks after trace filter")
      return(list(peaks = NULL, boxes = NULL, shapes = NULL))
    }
  }
  
  # ═══════════════════════════════════════════════════════════════════════════
  # FILTERING CONTROLLED BY params$use_filters
  # FALSE by default = no filtering (for debugging)
  # ═══════════════════════════════════════════════════════════════════════════
  use_filters <- if (!is.null(params$use_filters)) params$use_filters else FALSE
  
  if (verbose) {
    cat("Post-filters:", if(use_filters) "ENABLED" else "DISABLED", "\n")
  }
  
  if (use_filters) {
    # Filter by proportion and intensity
    result <- filter_peaks_by_proportion(detected_peaks,
                                         threshold = 0.5,
                                         intensity_threshold = params$int_thres)
    
    if (is.null(result$filtered_peaks) || nrow(result$filtered_peaks) == 0) {
      warning("No peaks after filter_peaks_by_proportion - try reducing int_thres")
      peaks_clean_filtered <- detected_peaks  # Fallback: use the raw peaks
    } else {
      if (verbose) {
        cat("Peaks after filter_peaks_by_proportion:", nrow(result$filtered_peaks), "\n")
      }
      
      # Remove noisy columns 
      peaks_clean_filtered <- filter_noisy_columns(result$filtered_peaks, threshold_ratio = 0.3)
      
      if (is.null(peaks_clean_filtered) || nrow(peaks_clean_filtered) == 0) {
        warning("No peaks after filter_noisy_columns - using unfiltered peaks")
        peaks_clean_filtered <- result$filtered_peaks
      }
    }
  } else {
    # NO FILTERING - use all detected peaks
    if (verbose) {
      cat("Filters disabled - using all raw peaks\n")
    }
    peaks_clean_filtered <- detected_peaks
  }
  
  if (verbose) {
    cat("Peaks before DBSCAN:", nrow(peaks_clean_filtered), "\n")
  }
  
  # ═══════════════════════════════════════════════════════════════════════════
  # MODE "NO CLUSTERING": Return raw peaks without DBSCAN
  # ═══════════════════════════════════════════════════════════════════════════
  disable_clustering <- if (!is.null(params$disable_clustering)) params$disable_clustering else FALSE
  
  if (disable_clustering) {
    
    # Convertir les indices en ppm
    x_vals <- as.numeric(colnames(rr_norm))
    y_vals <- as.numeric(rownames(rr_norm))
    
    peaks_ppm <- peaks_clean_filtered %>%
      mutate(
        F1_idx = pmin(pmax(round(F1), 1), length(x_vals)),
        F2_idx = pmin(pmax(round(F2), 1), length(y_vals)),
        F1_ppm = x_vals[F1_idx],
        F2_ppm = y_vals[F2_idx],
        stain_id = paste0("cnn_", row_number()),
        stain_intensity = Intensity,
        cluster_db = row_number()
      ) %>%
      select(F2_ppm, F1_ppm, stain_intensity, cluster_db, stain_id)
    
    # Create individual boxes for each peak
    box_padding <- params$eps_value * 2
    boxes <- peaks_ppm %>%
      mutate(
        xmin = F2_ppm - box_padding,
        xmax = F2_ppm + box_padding,
        ymin = F1_ppm - box_padding,
        ymax = F1_ppm + box_padding
      ) %>%
      select(xmin, xmax, ymin, ymax, stain_id, stain_intensity)
    
    return(list(
      peaks = peaks_ppm,
      boxes = boxes,
      shapes = list()
    ))
  }
  
  # === Step 3: DBSCAN Clustering and Bounding Boxes ===
  processed <- process_peaks_with_dbscan(peaks_clean_filtered, rr_norm, params, step)
  
  return(processed)
}