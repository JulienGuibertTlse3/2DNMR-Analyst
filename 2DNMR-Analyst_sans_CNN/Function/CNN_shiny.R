
# CNN_shiny.R - CNN-based Peak Detection for 2D NMR Spectra ----

#
# This module provides CNN (Convolutional Neural Network) based peak detection
# for 2D NMR spectra analysis. It includes functions for model building,
# peak detection using different strategies (batch/sequential), filtering,
# and DBSCAN clustering for bounding box generation.
#
# Author: Julien Guibert
# Institution: INRAe Toxalim / MetaboHUB


## DEPENDENCIES ----


library(purrr)
library(ggplot2)
library(keras)
library(viridis)
library(plotly)
library(reshape2)
library(abind)
library(tibble)
library(dplyr)
library(gridExtra)
library(dbscan)
library(stats)
library(readr)


## SECTION 1: MODEL ARCHITECTURE AND LOADING ----


#' Build CNN Peak Predictor Model
#'
#' Constructs a 1D convolutional neural network for peak detection in NMR spectra.
#' The model has two output heads:
#' - Classification head: 3-class softmax (background, peak edge, peak center)
#' - Regression head: 3 values per point (ppm, intensity, FWHH)
#'
#' @return A compiled Keras model ready for training or inference
#'
#' @details
#' Architecture:
#' - Input: 2048 points (1D spectrum slice)
#' - 8 convolutional layers with ReLU activation
#' - Kernel sizes: 11 (feature extraction), 1 (channel mixing), 3 (refinement)
#' - Filter progression: 40 -> 20 -> 10 -> 20 -> 10 -> 30 -> 18 -> 18
#'
#' Loss functions:

#' - Classification: Focal loss (gamma=3.0, alpha=1.5) for class imbalance
#' - Regression: Mean Squared Error (MSE)
#'
#' @examples
#' model <- build_peak_predictor()
#' summary(model)
#'
#' @export
build_peak_predictor <- function() {
  n_points <- 2048
  input <- layer_input(shape = c(n_points, 1), name = "input")
  
  # Feature extraction backbone
  # Uses alternating large kernels (11) for pattern detection
  
  # and 1x1 kernels for channel-wise feature mixing
  x <- input %>%
    layer_conv_1d(filters = 40, kernel_size = 11, padding = "same", activation = "relu") %>%
    layer_conv_1d(filters = 20, kernel_size = 1, padding = "same", activation = "relu") %>%
    layer_conv_1d(filters = 10, kernel_size = 11, padding = "same", activation = "relu") %>%
    layer_conv_1d(filters = 20, kernel_size = 1, padding = "same", activation = "relu") %>%
    layer_conv_1d(filters = 10, kernel_size = 1, padding = "same", activation = "relu") %>%
    layer_conv_1d(filters = 30, kernel_size = 11, padding = "same", activation = "relu") %>%
    layer_conv_1d(filters = 18, kernel_size = 1, padding = "same", activation = "relu") %>%
    layer_conv_1d(filters = 18, kernel_size = 3, padding = "same", activation = "relu")
  
  # Classification head: 3 classes with softmax activation
  # Classes: 0 = background, 1 = peak edge, 2 = peak center
  output_class <- x %>%
    layer_conv_1d(filters = 3, kernel_size = 1, activation = "softmax", name = "class_output")
  
  # Regression head: 3 values per point (ppm position, intensity, FWHH)
  output_reg <- x %>%
    layer_conv_1d(filters = 3, kernel_size = 1, activation = "linear", name = "reg_output")
  
  model <- keras_model(inputs = input, outputs = list(output_class, output_reg))
  
  # Focal loss implementation for handling class imbalance
  # gamma: focusing parameter (higher = more focus on hard examples)
  # alpha: class weight factor
  focal_loss <- function(gamma = 3.0, alpha = 1.5) {
    function(y_true, y_pred) {
      y_true <- k_cast(y_true, "int32")
      
      # Convert y_true to one-hot encoding
      y_true_one_hot <- k_one_hot(y_true, num_classes = 3)
      
      # Clip predictions to avoid log(0)
      epsilon <- k_epsilon()
      y_pred <- k_clip(y_pred, epsilon, 1.0 - epsilon)
      
      # Compute focal loss
      cross_entropy <- -y_true_one_hot * k_log(y_pred)
      weight <- alpha * k_pow(1 - y_pred, gamma)
      loss <- weight * cross_entropy
      
      return(k_sum(loss, axis = -1))
    }
  }
  
  # Compile model with dual loss functions
  model %>% compile(
    loss = list(
      class_output = focal_loss(gamma = 3.0, alpha = 1.5),
      reg_output = "mse"
    ),
    loss_weights = list(
      class_output = 1.0,
      reg_output = 1.0
    ),
    optimizer = optimizer_adam(learning_rate = 1e-4),
    metrics = list(
      class_output = "accuracy"
    )
  )
  
  return(model)
}

# Build and load pre-trained model weights
new_model <- build_peak_predictor()
load_model_weights_tf(new_model, "saved_model/weights")


## SECTION 2: PEAK DETECTION FOR UFCOSY SPECTRA (Sequential Method) ----


# Initialize empty dataframe for detected peaks
detected_peaks <- data.frame(F2 = numeric(0), F1 = numeric(0))

#' Pad Sequence to Target Length
#'
#' Pads a numeric vector with zeros on the right to reach the target length.
#' Used to prepare variable-length spectra for fixed-size CNN input.
#'
#' @param x Numeric vector to pad
#' @param target_length Integer, desired output length
#'
#' @return Numeric vector of length target_length
#'
#' @examples
#' pad_sequence(c(1, 2, 3), 5)
#' # Returns: c(1, 2, 3, 0, 0)
pad_sequence <- function(x, target_length) {
  current_length <- length(x)
  if (current_length < target_length) {
    pad_width <- target_length - current_length
    x_padded <- c(x, rep(0, pad_width))
  } else {
    x_padded <- x
  }
  return(x_padded)
}

#' Detect Peaks with Intensity (Sequential Method)
#'
#' Performs peak detection on a 2D NMR spectrum by scanning each row and column
#' sequentially. This method is slower but may be more accurate for small spectra.
#'
#' @param rr_norm Normalized 2D spectrum matrix with ppm values as row/column names
#' @param model Compiled Keras CNN model
#' @param target_length Integer, CNN input size (default: 2048)
#'
#' @return Data frame with columns: F2 (ppm), F1 (ppm), Intensity
#'
#' @details
#' The function:
#' 1. Iterates over all rows (F2 dimension) and applies CNN prediction
#' 2. Iterates over all columns (F1 dimension) and applies CNN prediction
#' 3. Combines and deduplicates detected peaks
#'
#' Classification labels:
#' - 0: Background (no peak)
#' - 1: Peak edge
#' - 2: Peak center
#'
#' @examples
#' peaks <- get_detected_peaks_with_intensity(spectrum_matrix, model)
#'
#' @export
get_detected_peaks_with_intensity <- function(rr_norm, model, target_length = 2048) {
  n_row <- nrow(rr_norm)
  n_col <- ncol(rr_norm)
  detected_peaks <- data.frame(F2 = numeric(0), F1 = numeric(0), Intensity = numeric(0))
  
  # Safe prediction wrapper using TensorFlow directly
  # Avoids issues with Keras predict() in some environments
  safe_predict <- function(model, x) {
    x_tensor <- tensorflow::tf$convert_to_tensor(x, dtype = tensorflow::tf$float32)
    preds <- model$`__call__`(x_tensor, training = FALSE)
    list(preds[[1]]$numpy(), preds[[2]]$numpy())
  }
  
  # === Row-wise detection (F2 dimension) ===
  pb_row <- txtProgressBar(min = 0, max = n_row, style = 3)
  for (i in 1:n_row) {
    spec1D_row <- rr_norm[i, ]
    
    # Pad to target length for CNN input
    spec1D_row_padded <- pad_sequence(spec1D_row, target_length)
    
    input_tensor <- array(spec1D_row_padded, dim = c(1, target_length, 1))
    pred_row <- safe_predict(model, input_tensor)
    
    # Extract predictions only for original (non-padded) points
    class_labels <- apply(pred_row[[1]][1, 1:length(spec1D_row), ], 1, which.max) - 1
    reg_pred <- pred_row[[2]][1, 1:length(spec1D_row), ]  # [original_length, 3]
    
    # Keep points classified as peak edge (1) or peak center (2)
    idx <- which(class_labels %in% c(1, 2))
    if (length(idx) > 0) {
      peaks <- data.frame(
        F2 = as.numeric(rownames(rr_norm))[i],
        F1 = as.numeric(colnames(rr_norm))[idx],
        Intensity = reg_pred[idx, 2]  # Predicted intensity
      )
      detected_peaks <- rbind(detected_peaks, peaks)
    }
    setTxtProgressBar(pb_row, i)
  }
  close(pb_row)
  
  # === Column-wise detection (F1 dimension) ===
  pb_col <- txtProgressBar(min = 0, max = n_col, style = 3)
  for (j in 1:n_col) {
    spec1D_col <- rr_norm[, j]
    
    # Pad to target length for CNN input
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
    setTxtProgressBar(pb_col, j)
  }
  close(pb_col)
  
  # Remove duplicate peaks (detected from both row and column scans)
  detected_peaks <- unique(detected_peaks)
  return(detected_peaks)
}


## SECTION 3: PEAK DETECTION FOR TOCSY SPECTRA (Batch Method) ----


#' Batch Peak Detection for 1D Slices
#'
#' Performs efficient batch prediction on multiple 1D slices of the spectrum.
#' Uses batch processing for significant speed improvement on large spectra.
#'
#' @param spectrum_mat 2D spectrum matrix
#' @param model Compiled Keras CNN model
#' @param axis Character, "rows" or "columns" - which dimension to scan
#' @param threshold_class Numeric, minimum probability to consider a peak (default: 0.01)
#' @param batch_size Integer, number of slices per batch (default: 64)
#' @param model_input_length Integer, CNN input size (default: 2048)
#' @param verbose Logical, print progress information
#'
#' @return Data frame with columns: F1, F2, Intensity, ppm
#'
#' @details
#' For spectra larger than model_input_length:
#' - Downsamples using evenly spaced indices
#' - Maps predictions back to original indices
#'
#' For smaller spectra:
#' - Zero-pads to model_input_length
#' - Only uses valid (non-padded) predictions
#'
#' @export
predict_peaks_1D_batch <- function(spectrum_mat, model,
                                   axis = c("rows", "columns"),
                                   threshold_class = 0.01, batch_size = 64,
                                   model_input_length = 2048, verbose = TRUE) {
  axis <- match.arg(axis)
  
  # Safe prediction wrapper using TensorFlow directly
  safe_predict <- function(model, x) {
    x_tensor <- tensorflow::tf$convert_to_tensor(x, dtype = tensorflow::tf$float32)
    preds <- model$`__call__`(x_tensor, training = FALSE)
    list(preds[[1]]$numpy(), preds[[2]]$numpy())
  }
  
  # Transpose if scanning columns (work with rows internally)
  mat <- if (axis == "columns") t(spectrum_mat) else spectrum_mat
  n_vectors <- nrow(mat)
  n_points <- ncol(mat)
  
  if (verbose) cat("Number of vectors:", n_vectors, "- Points per vector:", n_points, "\n")
  
  detected_list <- vector("list", length = ceiling(n_vectors / batch_size))
  pb <- txtProgressBar(min = 0, max = n_vectors, style = 3)
  idx_list <- 1
  
  # Process in batches for efficiency
  for (start_idx in seq(1, n_vectors, by = batch_size)) {
    end_idx <- min(start_idx + batch_size - 1, n_vectors)
    batch_raw <- mat[start_idx:end_idx, , drop = FALSE]
    
    # Prepare batch: resize/pad to model input length
    batch_prepared <- matrix(0, nrow = nrow(batch_raw), ncol = model_input_length)
    idx_map_list <- vector("list", nrow(batch_raw))
    
    for (i in seq_len(nrow(batch_raw))) {
      x <- batch_raw[i, ]
      if (length(x) > model_input_length) {
        # Downsample: evenly spaced indices
        idx_map <- round(seq(1, length(x), length.out = model_input_length))
        batch_prepared[i, ] <- x[idx_map]
      } else {
        # Pad with zeros
        idx_map <- seq_len(length(x))
        batch_prepared[i, 1:length(x)] <- x
      }
      idx_map_list[[i]] <- idx_map
    }
    
    input_array <- array(batch_prepared, dim = c(nrow(batch_prepared), model_input_length, 1))
    
    # Run batch prediction
    preds <- safe_predict(model, input_array)
    probs <- preds[[1]]  # Classification probabilities [batch, length, 3]
    regs <- preds[[2]]   # Regression values [batch, length, 3]
    
    # Extract peak probability (class 2 = peak center)
    prob_peak <- probs[, , 2]
    reg_intensity <- regs[, , 2]  # Predicted intensity
    
    # Extract peaks above threshold
    detected_batch <- list()
    for (i in seq_len(nrow(prob_peak))) {
      idxs <- which(prob_peak[i, ] > threshold_class)
      if (length(idxs) > 0) {
        # Map indices back to original spectrum coordinates
        idxs_orig <- idx_map_list[[i]][idxs]
        
        # Filter out padding indices
        valid <- idxs_orig <= n_points
        idxs <- idxs[valid]
        idxs_orig <- idxs_orig[valid]
        
        if (length(idxs_orig) > 0) {
          # Compute normalized ppm-like axis
          ppm_vals <- seq(from = 0, to = 1, length.out = n_points)[idxs_orig]
          
          df <- data.frame(
            F1 = if (axis == "rows") idxs_orig else (start_idx + i - 1),
            F2 = if (axis == "rows") (start_idx + i - 1) else idxs_orig,
            Intensity = reg_intensity[i, idxs],
            ppm = if (axis == "rows") ppm_vals else NA
          )
          detected_batch[[length(detected_batch) + 1]] <- df
        }
      }
    }
    
    detected_list[[idx_list]] <- if (length(detected_batch) > 0) do.call(rbind, detected_batch) else NULL
    setTxtProgressBar(pb, end_idx)
    idx_list <- idx_list + 1
  }
  
  close(pb)
  detected <- do.call(rbind, detected_list)
  if (is.null(detected)) detected <- data.frame(F1 = numeric(), F2 = numeric(), Intensity = numeric(), ppm = numeric())
  
  # Remove duplicates
  detected <- unique(detected)
  
  if (verbose) cat("Total peaks detected:", nrow(detected), "\n")
  return(detected)
}


## SECTION 4: PEAK FILTERING FUNCTIONS ----


#' Filter Peaks by Proportion
#'
#' Removes rows and columns that have too many detected peaks (likely noise/artifacts).
#' A high proportion of peaks in a single row/column often indicates systematic noise.
#'
#' @param peaks_clean Data frame with peaks (F1, F2, Intensity columns)
#' @param threshold Numeric, maximum proportion of peaks allowed in a row/column (default: 0.5)
#' @param intensity_threshold Numeric, minimum intensity to keep (optional)
#'
#' @return List containing:
#'   - filtered_peaks: Cleaned peak data frame
#'   - removed_columns: F1 values of removed columns
#'   - removed_rows: F2 values of removed rows
#'
#' @details
#' The function calculates:
#' - peaks_per_col / total_points_per_col for each F1 value
#' - peaks_per_row / total_points_per_row for each F2 value
#' Rows/columns exceeding the threshold are removed.
#'
#' @export
filter_peaks_by_proportion <- function(peaks_clean, threshold = NULL, intensity_threshold = NULL) {
  # Optional: initial intensity filtering
  if (!is.null(intensity_threshold)) {
    peaks_clean <- peaks_clean[peaks_clean$Intensity > intensity_threshold, ]
  }
  
  # Calculate total points per column (F1)
  total_points_per_col <- tapply(peaks_clean$F2, peaks_clean$F1, max)
  # Count detected peaks per column
  peaks_per_col <- table(peaks_clean$F1)
  # Calculate proportion of peaks per column
  prop_col <- peaks_per_col / total_points_per_col[names(peaks_per_col)]
  # Identify columns to remove (proportion > threshold)
  cols_to_remove <- names(prop_col[prop_col > threshold])
  
  # Calculate total points per row (F2)
  total_points_per_row <- tapply(peaks_clean$F1, peaks_clean$F2, max)
  # Count detected peaks per row
  peaks_per_row <- table(peaks_clean$F2)
  # Calculate proportion of peaks per row
  prop_row <- peaks_per_row / total_points_per_row[names(peaks_per_row)]
  # Identify rows to remove
  rows_to_remove <- names(prop_row[prop_row > threshold])
  
  # Remove peaks in problematic rows/columns
  filtered_peaks <- peaks_clean[!(peaks_clean$F1 %in% cols_to_remove) &
                                  !(peaks_clean$F2 %in% rows_to_remove), ]
  
  return(list(
    filtered_peaks = filtered_peaks,
    removed_columns = cols_to_remove,
    removed_rows = rows_to_remove
  ))
}

#' Filter Noisy Columns by Relative Intensity
#'
#' Keeps only peaks that have intensity close to the maximum intensity
#' in their respective column. Removes low-amplitude noise.
#'
#' @param peaks_df Data frame with peaks (F1, F2, Intensity columns)
#' @param threshold_ratio Numeric, minimum ratio of peak intensity to column max (default: 0.9)
#' @param min_col_max Numeric, minimum column maximum intensity (optional)
#'
#' @return Filtered data frame with columns F1, F2, Intensity
#'
#' @details
#' A peak is kept if: |Intensity| >= threshold_ratio * MaxIntensity_of_column
#'
#' @export
filter_noisy_columns <- function(peaks_df, threshold_ratio = 0.9, min_col_max = NULL) {
  # Calculate maximum absolute intensity per column (F1)
  max_per_col <- aggregate(Intensity ~ F1, data = peaks_df,
                           FUN = function(x) max(abs(x)))
  colnames(max_per_col)[2] <- "MaxIntensity"
  
  # Merge max intensity back to each peak
  merged <- merge(peaks_df, max_per_col, by = "F1")
  
  # Filter by relative amplitude
  filtered <- subset(merged, abs(Intensity) >= threshold_ratio * MaxIntensity)
  
  # Optional: remove columns with weak maximum intensity
  if (!is.null(min_col_max)) {
    filtered <- subset(filtered, MaxIntensity >= min_col_max)
  }
  
  # Return to original format
  filtered <- filtered[, c("F1", "F2", "Intensity")]
  return(filtered)
}

#' Clean Peak Clusters with DBSCAN
#'
#' Applies DBSCAN clustering to group nearby peaks. Useful for identifying
#' peak multiplets and removing isolated noise points.
#'
#' @param peaks_df Data frame with peaks (F1, F2 columns)
#' @param ppm_x Numeric vector, F1 ppm axis
#' @param ppm_y Numeric vector, F2 ppm axis
#' @param eps_ppm Numeric, DBSCAN epsilon parameter (default: from params)
#' @param minPts Integer, minimum points per cluster (default: 2)
#'
#' @return Data frame with added 'cluster' column (0 = noise, 1+ = cluster ID)
#'
#' @export
clean_peak_clusters_dbscan <- function(peaks_df, ppm_x, ppm_y, eps_ppm = params$eps_value, minPts = 2) {
  # Remove NA values
  peaks_df <- peaks_df[!is.na(peaks_df$F1) & !is.na(peaks_df$F2), ]
  
  # Extract normalized coordinates for clustering
  coords <- as.matrix(peaks_df[, c("F1", "F2")])
  
  # Apply DBSCAN clustering
  clustering <- dbscan::dbscan(coords, eps = eps_ppm, minPts = minPts)
  peaks_df$cluster <- clustering$cluster
  
  return(peaks_df)
}

#' Remove Peaks in PPM Range
#'
#' Filters out peaks that fall within a specified ppm range on a given axis.
#' Useful for removing solvent peaks, reference peaks, or known artifacts.
#'
#' @param peaks Data frame with peaks
#' @param rr_norm 2D spectrum matrix (used to get ppm axes)
#' @param axis Character, "F1" or "F2" - which axis to filter
#' @param ppm_min Numeric, lower bound of range to remove
#' @param ppm_max Numeric, upper bound of range to remove
#'
#' @return Filtered peaks data frame with peaks in range removed
#'
#' @examples
#' # Remove water region peaks (4.7-5.0 ppm on F1)
#' peaks_clean <- remove_peaks_ppm_range(peaks, spectrum, "F1", 4.7, 5.0)
#'
#' @export
remove_peaks_ppm_range <- function(peaks, rr_norm, axis = "F1", ppm_min, ppm_max) {
  # Get ppm axes from spectrum matrix
  ppm_y <- as.numeric(rownames(rr_norm))   # F2 axis (rows)
  ppm_x <- as.numeric(colnames(rr_norm))   # F1 axis (columns)
  
  # Helper function to convert indices or ppm values to actual ppm
  map_to_ppm <- function(vals, ppm_axis) {
    vnum <- as.numeric(vals)
    n <- length(ppm_axis)
    axis_min <- min(ppm_axis, na.rm = TRUE)
    axis_max <- max(ppm_axis, na.rm = TRUE)
    
    # Check if values are indices (integers in valid range)
    if (all(!is.na(vnum) & vnum >= 1 & vnum <= n & abs(vnum - round(vnum)) < 1e-6)) {
      return(as.numeric(ppm_axis[round(vnum)]))
    }
    # Check if values are already ppm
    if (all(!is.na(vnum) & vnum >= axis_min & vnum <= axis_max)) {
      return(as.numeric(sapply(vnum, function(v) ppm_axis[which.min(abs(ppm_axis - v))])))
    }
    # Mixed case: handle each value individually
    sapply(vnum, function(v) {
      if (is.na(v)) return(NA_real_)
      if (v >= 1 && v <= n && abs(v - round(v)) < 1e-6) {
        return(ppm_axis[round(v)])
      } else if (v >= axis_min && v <= axis_max) {
        return(ppm_axis[which.min(abs(ppm_axis - v))])
      } else {
        return(ppm_axis[which.min(abs(ppm_axis - v))])
      }
    })
  }
  
  # Convert peak positions to ppm
  ppm_vals <- if (axis == "F1") {
    map_to_ppm(peaks$F1, ppm_x)
  } else if (axis == "F2") {
    map_to_ppm(peaks$F2, ppm_y)
  } else {
    stop("axis must be 'F1' or 'F2'")
  }
  
  # Create mask for peaks in the removal range
  mask <- ppm_vals >= ppm_min & ppm_vals <= ppm_max
  removed <- sum(mask, na.rm = TRUE)
  
  message("Removed ", removed, " peaks between ", ppm_min, " and ", ppm_max, " ppm (", axis, ")")
  
  return(peaks[!mask, , drop = FALSE])
}


## SECTION 5: DBSCAN CLUSTERING AND BOUNDING BOX GENERATION ----


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
  
  # --- Step 1: Z-score normalization for clustering ---
  peaks_norm <- peaks_clean_filtered %>%
    mutate(
      F1_scaled = (F1 - mean(F1, na.rm = TRUE)) / sd(F1, na.rm = TRUE),
      F2_scaled = (F2 - mean(F2, na.rm = TRUE)) / sd(F2, na.rm = TRUE)
    )
  
  # --- Step 2: DBSCAN clustering ---
  db <- dbscan::dbscan(peaks_norm[, c("F1_scaled", "F2_scaled")],
                       eps = params$eps_value, minPts = 2)
  peaks_clean_filtered <- peaks_clean_filtered %>%
    mutate(cluster_db = db$cluster)
  
  # --- Step 3: Calculate bounding boxes for each cluster ---
  bounding_boxes <- peaks_clean_filtered %>%
    dplyr::filter(cluster_db > 0) %>%  # Exclude noise points (cluster 0)
    group_by(cluster_db) %>%
    summarise(
      xmin = min(F1, na.rm = TRUE),
      xmax = max(F1, na.rm = TRUE),
      ymin = min(F2, na.rm = TRUE),
      ymax = max(F2, na.rm = TRUE),
      cx = (min(F1, na.rm = TRUE) + max(F1, na.rm = TRUE)) / 2,  # Center X
      cy = (min(F2, na.rm = TRUE) + max(F2, na.rm = TRUE)) / 2,  # Center Y
      intensity = sum(Intensity, na.rm = TRUE),
      .groups = "drop"
    )
  
  # --- Step 4: Convert indices to ppm values ---
  bounding_boxes <- bounding_boxes %>%
    mutate(
      xmin_ppm = x_vals[xmin],
      xmax_ppm = x_vals[xmax],
      ymin_ppm = y_vals[ymin],
      ymax_ppm = y_vals[ymax],
      cx_ppm = x_vals[as.integer(round(cx))],
      cy_ppm = y_vals[as.integer(round(cy))],
      stain_intensity = intensity
    )
  
  # --- Step 5: Generate Plotly rectangle shapes ---
  shapes_list <- lapply(seq_len(nrow(bounding_boxes)), function(i) {
    list(
      type = "rect",
      x0 = bounding_boxes$xmin_ppm[i], x1 = bounding_boxes$xmax_ppm[i],
      y0 = bounding_boxes$ymin_ppm[i], y1 = bounding_boxes$ymax_ppm[i],
      line = list(color = "red", dash = "dot", width = 2),
      fillcolor = "rgba(0,0,0,0)",  # Transparent fill
      xref = "x", yref = "y",
      layer = "above"
    )
  })
  
  # --- Step 6: Build peaks based on bounding box centers ---
  peaks_from_boxes <- bounding_boxes %>%
    dplyr::transmute(
      F1 = as.integer(round(cx)),
      F2 = as.integer(round(cy)),
      F1_ppm = cx_ppm,
      F2_ppm = cy_ppm,
      stain_intensity = stain_intensity,
      cluster_db = cluster_db
    )
  
  # --- Step 7: Return results ---
  return(list(
    peaks = peaks_from_boxes,
    boxes = bounding_boxes,
    shapes = shapes_list
  ))
}


## SECTION 6: MAIN ENTRY POINT FUNCTION ----


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
  
  if (verbose) {
    cat("Total raw peaks detected:", nrow(detected_peaks), "\n")
  }
  
  # === Step 2: Initial Filtering ===
  if (nrow(detected_peaks) == 0) {
    warning("No peaks detected")
    return(list(peaks = NULL, boxes = NULL, shapes = NULL))
  }
  
  # Filter by proportion and intensity
  result <- filter_peaks_by_proportion(detected_peaks,
                                       threshold = 0.5,
                                       intensity_threshold = params$int_thres)
  # Remove noisy columns
  peaks_clean_filtered <- result$filtered_peaks %>% filter_noisy_columns()
  
  # === Step 3: DBSCAN Clustering and Bounding Boxes ===
  processed <- process_peaks_with_dbscan(peaks_clean_filtered, rr_norm, params, step)
  
  if (verbose) {
    cat("Final structure:\n")
    str(processed)
  }
  
  return(processed)
}