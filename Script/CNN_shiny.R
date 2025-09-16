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

#==============================CHARGEMENT DU MODELE==================================================================

build_peak_predictor <- function() {
  n_points <- 2048
  input <- layer_input(shape = c(n_points, 1), name = "input")
  
  x <- input %>%
    layer_conv_1d(filters = 40, kernel_size = 11, padding = "same", activation = "relu") %>%
    layer_conv_1d(filters = 20, kernel_size = 1, padding = "same", activation = "relu") %>%
    layer_conv_1d(filters = 10, kernel_size = 11, padding = "same", activation = "relu") %>%
    layer_conv_1d(filters = 20, kernel_size = 1, padding = "same", activation = "relu") %>%
    layer_conv_1d(filters = 10, kernel_size = 1, padding = "same", activation = "relu") %>%
    layer_conv_1d(filters = 30, kernel_size = 11, padding = "same", activation = "relu") %>%
    layer_conv_1d(filters = 18, kernel_size = 1, padding = "same", activation = "relu") %>%
    layer_conv_1d(filters = 18, kernel_size = 3, padding = "same", activation = "relu")
  
  # Tête classification par point : 3 classes ==> classification avec softmax
  output_class <- x %>%
    layer_conv_1d(filters = 3, kernel_size = 1, activation = "softmax", name = "class_output")
  
  # Tête régression : 3 valeurs par point (ppm, intensité, FWHH)
  output_reg <- x %>%
    layer_conv_1d(filters = 3, kernel_size = 1, activation = "linear", name = "reg_output")
  
  model <- keras_model(inputs = input, outputs = list(output_class, output_reg))
  
  focal_loss <- function(gamma = 3.0, alpha = 1.5) {
    function(y_true, y_pred) {
      y_true <- k_cast(y_true, "int32")
      
      # Transforme y_true en one-hot
      y_true_one_hot <- k_one_hot(y_true, num_classes = 3)
      
      # Clip pour éviter log(0)
      epsilon <- k_epsilon()
      y_pred <- k_clip(y_pred, epsilon, 1.0 - epsilon)
      
      # Calcule la focal loss
      cross_entropy <- -y_true_one_hot * k_log(y_pred)
      weight <- alpha * k_pow(1 - y_pred, gamma)
      loss <- weight * cross_entropy
      
      return(k_sum(loss, axis = -1))
    }
  }
  
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

new_model <- build_peak_predictor()
load_model_weights_tf(new_model, "saved_model/weights")

#==================================DETECTION DE PICS SUR SPECTRE TYPE UFCOSY=======================================================

detected_peaks <- data.frame(F2 = numeric(0), F1 = numeric(0))

# Fonction de padding, padding à droite avec des 0
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

get_detected_peaks_with_intensity <- function(rr_norm, model, target_length = 2048) {
  n_row <- nrow(rr_norm)
  n_col <- ncol(rr_norm)
  detected_peaks <- data.frame(F2 = numeric(0), F1 = numeric(0), Intensity = numeric(0))
  
  # Détection sur les lignes (F2)
  pb_row <- txtProgressBar(min = 0, max = n_row, style = 3)
  for (i in 1:n_row) {
    spec1D_row <- rr_norm[i, ]
    
    # Padding à la taille target_length
    spec1D_row_padded <- pad_sequence(spec1D_row, target_length)
    
    input_tensor <- array(spec1D_row_padded, dim = c(1, target_length, 1))
    pred_row <- model %>% predict(input_tensor)
    
    # On récupère uniquement les indices correspondant à la vraie longueur (sans padding)
    class_labels <- apply(pred_row[[1]][1, 1:length(spec1D_row), ], 1, which.max) - 1
    reg_pred <- pred_row[[2]][1, 1:length(spec1D_row), ]  # [vraie longueur, 3]
    
    idx <- which(class_labels %in% c(1, 2))
    if (length(idx) > 0) {
      peaks <- data.frame(
        F2 = as.numeric(rownames(rr_norm))[i],
        F1 = as.numeric(colnames(rr_norm))[idx],
        Intensity = reg_pred[idx, 2]  # intensité prédite
      )
      detected_peaks <- rbind(detected_peaks, peaks)
    }
    setTxtProgressBar(pb_row, i)
  }
  close(pb_row)
  
  # Détection sur les colonnes (F1)
  pb_col <- txtProgressBar(min = 0, max = n_col, style = 3)
  for (j in 1:n_col) {
    spec1D_col <- rr_norm[, j]
    
    # Padding à la taille target_length
    spec1D_col_padded <- pad_sequence(spec1D_col, target_length)
    
    input_tensor <- array(spec1D_col_padded, dim = c(1, target_length, 1))
    pred_col <- model %>% predict(input_tensor)
    
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
  
  detected_peaks <- unique(detected_peaks)
  return(detected_peaks)
}

#=================================DETECTION DE PICS SUR SPECTRE TYPE TOCSY=========================================================

predict_peaks_1D_batch <- function(spectrum_mat, model,
                                   axis = c("rows", "columns"),
                                   threshold_class = 0.01, batch_size = 64,
                                   model_input_length = 2048, verbose = TRUE) {
  axis <- match.arg(axis)
  
  mat <- if (axis == "columns") t(spectrum_mat) else spectrum_mat
  n_vectors <- nrow(mat)
  n_points <- ncol(mat)
  
  if (verbose) cat("Nombre de vecteurs :", n_vectors, "- Points par vecteur :", n_points, "\n")
  
  detected_list <- vector("list", length = ceiling(n_vectors / batch_size))
  pb <- txtProgressBar(min = 0, max = n_vectors, style = 3)
  idx_list <- 1
  
  for (start_idx in seq(1, n_vectors, by = batch_size)) {
    end_idx <- min(start_idx + batch_size - 1, n_vectors)
    batch_raw <- mat[start_idx:end_idx,,drop=FALSE]
    
    batch_prepared <- matrix(0, nrow = nrow(batch_raw), ncol = model_input_length)
    idx_map_list <- vector("list", nrow(batch_raw))
    
    for (i in seq_len(nrow(batch_raw))) {
      x <- batch_raw[i, ]
      if (length(x) > model_input_length) {
        idx_map <- round(seq(1, length(x), length.out = model_input_length))
        batch_prepared[i, ] <- x[idx_map]
      } else {
        idx_map <- seq_len(length(x))
        batch_prepared[i, 1:length(x)] <- x
      }
      idx_map_list[[i]] <- idx_map
    }
    
    input_array <- array(batch_prepared, dim = c(nrow(batch_prepared), model_input_length, 1))
    
    preds <- predict(model, input_array, batch_size = batch_size)
    probs <- preds[[1]]
    regs  <- preds[[2]]
    
    prob_peak <- probs[,,2]
    reg_intensity <- regs[,,2]
    
    detected_batch <- list()
    for (i in seq_len(nrow(prob_peak))) {
      idxs <- which(prob_peak[i, ] > threshold_class)
      if (length(idxs) > 0) {
        idxs_orig <- idx_map_list[[i]][idxs]
        
        # Filtrer les indices hors spectre réel (padding)
        valid <- idxs_orig <= n_points
        idxs <- idxs[valid]
        idxs_orig <- idxs_orig[valid]
        
        if (length(idxs_orig) > 0) {
          # Reconstruct ppm-like axis (normalized index)
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
  if (is.null(detected)) detected <- data.frame(F1=numeric(), F2=numeric(), Intensity=numeric(), ppm=numeric())
  
  detected <- unique(detected)
  
  if (verbose) cat("Nombre total de pics détectés :", nrow(detected), "\n")
  return(detected)
}


filter_peaks_by_proportion <- function(peaks_clean, threshold = NULL, intensity_threshold = NULL) {
  # Optionnel : filtrage initial par intensité
  if (!is.null(intensity_threshold)) {
    peaks_clean <- peaks_clean[peaks_clean$Intensity > intensity_threshold, ]
  }
  
  # Calcul total points par colonne (F1)
  total_points_per_col <- tapply(peaks_clean$F2, peaks_clean$F1, max)
  # Nombre de pics détectés par colonne
  peaks_per_col <- table(peaks_clean$F1)
  # Proportion de pics par colonne
  prop_col <- peaks_per_col / total_points_per_col[names(peaks_per_col)]
  # Colonnes à supprimer
  cols_to_remove <- names(prop_col[prop_col > threshold])
  
  # Calcul total points par ligne (F2)
  total_points_per_row <- tapply(peaks_clean$F1, peaks_clean$F2, max)
  # Nombre de pics détectés par ligne
  peaks_per_row <- table(peaks_clean$F2)
  # Proportion de pics par ligne
  prop_row <- peaks_per_row / total_points_per_row[names(peaks_per_row)]
  # Lignes à supprimer
  rows_to_remove <- names(prop_row[prop_row > threshold])
  
  # Suppression des pics dans lignes/colonnes indésirables
  filtered_peaks <- peaks_clean[!(peaks_clean$F1 %in% cols_to_remove) &
                                  !(peaks_clean$F2 %in% rows_to_remove), ]
  
  return(list(
    filtered_peaks = filtered_peaks,
    removed_columns = cols_to_remove,
    removed_rows = rows_to_remove
  ))
}
filter_noisy_columns <- function(peaks_df, threshold_ratio = 0.9, min_col_max = NULL) {
  # max en valeur absolue par colonne F1
  max_per_col <- aggregate(Intensity ~ F1, data = peaks_df,
                           FUN = function(x) max(abs(x)))
  colnames(max_per_col)[2] <- "MaxIntensity"
  
  # associer à chaque point le max de sa colonne
  merged <- merge(peaks_df, max_per_col, by = "F1")
  
  # filtrer selon amplitude relative
  filtered <- subset(merged, abs(Intensity) >= threshold_ratio * MaxIntensity)
  
  # optionnel : enlever carrément les colonnes trop faibles
  if (!is.null(min_col_max)) {
    filtered <- subset(filtered, MaxIntensity >= min_col_max)
  }
  
  # retour format original
  filtered <- filtered[, c("F1", "F2", "Intensity")]
  return(filtered)
}
clean_peak_clusters_dbscan <- function(peaks_df, ppm_x, ppm_y, eps_ppm = params$eps_value, minPts = 2) {
  # Enlever les NA
  peaks_df <- peaks_df[!is.na(peaks_df$F1) & !is.na(peaks_df$F2), ]
  
  # Coordonnées normalisées
  coords <- as.matrix(peaks_df[, c("F1", "F2")])
  
  # Clustering
  clustering <- dbscan::dbscan(coords, eps = eps_ppm, minPts = minPts)
  peaks_df$cluster <- clustering$cluster
  
  return(peaks_df)
}
remove_peaks_ppm_range <- function(peaks, rr_norm, axis = "F1", ppm_min, ppm_max) {
  # --- get ppm axes from rr_norm ---
  ppm_y <- as.numeric(rownames(rr_norm))   # F2 axis (rows)
  ppm_x <- as.numeric(colnames(rr_norm))   # F1 axis (cols)
  
  # --- helper pour convertir indices ou ppm → ppm réel ---
  map_to_ppm <- function(vals, ppm_axis) {
    vnum <- as.numeric(vals)
    n <- length(ppm_axis)
    axis_min <- min(ppm_axis, na.rm = TRUE)
    axis_max <- max(ppm_axis, na.rm = TRUE)
    
    if (all(!is.na(vnum) & vnum >= 1 & vnum <= n & abs(vnum - round(vnum)) < 1e-6)) {
      return(as.numeric(ppm_axis[round(vnum)]))
    }
    if (all(!is.na(vnum) & vnum >= axis_min & vnum <= axis_max)) {
      return(as.numeric(sapply(vnum, function(v) ppm_axis[which.min(abs(ppm_axis - v))])))
    }
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
  
  # --- conversion ---
  ppm_vals <- if (axis == "F1") {
    map_to_ppm(peaks$F1, ppm_x)
  } else if (axis == "F2") {
    map_to_ppm(peaks$F2, ppm_y)
  } else {
    stop("axis doit être 'F1' ou 'F2'")
  }
  
  # --- filtre ---
  mask <- ppm_vals >= ppm_min & ppm_vals <= ppm_max
  removed <- sum(mask, na.rm = TRUE)
  
  message("Retiré ", removed, " pics entre ", ppm_min, " et ", ppm_max, " ppm (", axis, ")")
  
  return(peaks[!mask, , drop = FALSE])
}


#===============================GENERATION DE CLUSTER ET DE BOUNDING BOXES========================================================

process_peaks_with_dbscan <- function(peaks_clean_filtered, rr_norm, params, step = 4) {
  
  downsample_matrix <- function(mat, step = 4) {
    mat[seq(1, nrow(mat), by = step),
        seq(1, ncol(mat), by = step)]
  }
  downsample_axis <- function(axis_vals, step = 4) {
    axis_vals[seq(1, length(axis_vals), by = step)]
  }
  
  # --- Matrice et axes ---
  z_matrix <- rr_norm
  x_vals   <- as.numeric(colnames(rr_norm))  # F1 (ppm)
  y_vals   <- as.numeric(rownames(rr_norm))  # F2 (ppm)
  
  # Downsampling pour affichage
  z_small <- downsample_matrix(z_matrix, step = step)
  x_small <- downsample_axis(x_vals, step = step)
  y_small <- downsample_axis(y_vals, step = step)
  
  # --- 1) Normalisation z-score pour clustering ---
  peaks_norm <- peaks_clean_filtered %>%
    mutate(
      F1_scaled = (F1 - mean(F1, na.rm = TRUE)) / sd(F1, na.rm = TRUE),
      F2_scaled = (F2 - mean(F2, na.rm = TRUE)) / sd(F2, na.rm = TRUE)
    )
  
  # --- 2) DBSCAN ---
  db <- dbscan::dbscan(peaks_norm[, c("F1_scaled", "F2_scaled")],
                       eps = params$eps_value, minPts = 2)
  peaks_clean_filtered <- peaks_clean_filtered %>%
    mutate(cluster_db = db$cluster)
  
  # --- 3) Bounding boxes ---
  bounding_boxes <- peaks_clean_filtered %>%
    dplyr::filter(cluster_db > 0) %>%
    group_by(cluster_db) %>%
    summarise(
      xmin = min(F1, na.rm = TRUE),
      xmax = max(F1, na.rm = TRUE),
      ymin = min(F2, na.rm = TRUE),
      ymax = max(F2, na.rm = TRUE),
      cx   = (min(F1, na.rm = TRUE) + max(F1, na.rm = TRUE)) / 2,
      cy   = (min(F2, na.rm = TRUE) + max(F2, na.rm = TRUE)) / 2,
      intensity = sum(Intensity, na.rm = TRUE),
      .groups = "drop"
    )
  
  # --- 4) Conversion indices → ppm ---
  bounding_boxes <- bounding_boxes %>%
    mutate(
      xmin_ppm = x_vals[xmin],
      xmax_ppm = x_vals[xmax],
      ymin_ppm = y_vals[ymin],
      ymax_ppm = y_vals[ymax],
      cx_ppm   = x_vals[as.integer(round(cx))],
      cy_ppm   = y_vals[as.integer(round(cy))],
      stain_intensity = intensity
    )
  
  # --- 5) Rectangles pour plotly ---
  shapes_list <- lapply(seq_len(nrow(bounding_boxes)), function(i) {
    list(
      type = "rect",
      x0 = bounding_boxes$xmin_ppm[i], x1 = bounding_boxes$xmax_ppm[i],
      y0 = bounding_boxes$ymin_ppm[i], y1 = bounding_boxes$ymax_ppm[i],
      line = list(color = "red", dash = "dot", width = 2),
      fillcolor = "rgba(0,0,0,0)",
      xref = "x", yref = "y",
      layer = "above"
    )
  })
  
  # --- 6) Construire peaks basé sur centres de bounding boxes ---
  peaks_from_boxes <- bounding_boxes %>%
    dplyr::transmute(
      F1 = as.integer(round(cx)),           
      F2 = as.integer(round(cy)),           
      F1_ppm = cx_ppm,
      F2_ppm = cy_ppm,
      stain_intensity = stain_intensity,
      cluster_db = cluster_db
    )
  
  # --- 7) Retour ---
  return(list(
    peaks  = peaks_from_boxes,
    boxes  = bounding_boxes,
    shapes = shapes_list
  ))
}

#===============================FONCTION GENERALE=================================================================================

run_cnn_peak_picking <- function(rr_norm, model, params,
                                 method = c("batch", "classique"),
                                 target_length = 2048,
                                 threshold_class = params$pred_class_thres,
                                 batch_size = 64, step = 4,
                                 verbose = TRUE) {
  # Sécurité
  if (is.null(rr_norm) || !is.matrix(rr_norm)) {
    stop("❌ rr_norm doit être une matrice normalisée")
  }
  if (is.null(model)) {
    stop("❌ Le modèle CNN n’est pas chargé")
  }
  
  method <- match.arg(method)  # sélection de la méthode
  
  n_row <- nrow(rr_norm)
  n_col <- ncol(rr_norm)
  
  # Fonction utilitaire de padding
  pad_sequence <- function(x, target_length) {
    current_length <- length(x)
    if (current_length < target_length) {
      c(x, rep(0, target_length - current_length))
    } else {
      x
    }
  }
  
  detected_peaks <- data.frame(F2 = numeric(0), F1 = numeric(0), Intensity = numeric(0))
  
  # === Étape 1 : Détection des pics ===
  if (method == "classique") {
    if (verbose) cat("➡️ Méthode classique : détection ligne + colonne séquentielle\n")
    
    # Détection lignes
    for (i in seq_len(n_row)) {
      spec1D_row <- rr_norm[i, ]
      spec1D_row_padded <- pad_sequence(spec1D_row, target_length)
      input_tensor <- array(spec1D_row_padded, dim = c(1, target_length, 1))
      
      pred_row <- predict(model, input_tensor, batch_size = 1)
      class_labels <- apply(pred_row[[1]][1, 1:length(spec1D_row), ], 1, which.max) - 1
      reg_pred <- pred_row[[2]][1, 1:length(spec1D_row), ]
      
      idx <- which(class_labels %in% c(1,2))
      if (length(idx) > 0) {
        peaks <- data.frame(
          F2 = as.numeric(rownames(rr_norm))[i],
          F1 = as.numeric(colnames(rr_norm))[idx],
          Intensity = reg_pred[idx, 2]
        )
        detected_peaks <- rbind(detected_peaks, peaks)
      }
    }
    
    # Détection colonnes
    for (j in seq_len(n_col)) {
      spec1D_col <- rr_norm[, j]
      spec1D_col_padded <- pad_sequence(spec1D_col, target_length)
      input_tensor <- array(spec1D_col_padded, dim = c(1, target_length, 1))
      
      pred_col <- predict(model, input_tensor, batch_size = 1)
      class_labels <- apply(pred_col[[1]][1, 1:length(spec1D_col), ], 1, which.max) - 1
      reg_pred <- pred_col[[2]][1, 1:length(spec1D_col), ]
      
      idx <- which(class_labels %in% c(1,2))
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
    if (verbose) cat("➡️ Méthode batch : prédiction via predict_peaks_1D_batch()\n")
    
    peaks_rows <- predict_peaks_1D_batch(rr_norm, model,
                                         axis = "rows",
                                         threshold_class = threshold_class,
                                         batch_size = batch_size,
                                         verbose = verbose)
    peaks_cols <- predict_peaks_1D_batch(rr_norm, model,
                                         axis = "columns",
                                         threshold_class = threshold_class,
                                         batch_size = batch_size,
                                         verbose = verbose)
    detected_peaks <- rbind(peaks_rows, peaks_cols) %>% unique()
  }
  
  if (verbose) {
    cat("✅ Nombre total de pics détectés (bruts) :", nrow(detected_peaks), "\n")
  }
  
  # === Étape 2 : Filtres initiaux ===
  if (nrow(detected_peaks) == 0) {
    warning("⚠️ Aucun pic détecté")
    return(list(peaks = NULL, boxes = NULL, shapes = NULL))
  }
  
  result <- filter_peaks_by_proportion(detected_peaks,
                                       threshold = 0.5,
                                       intensity_threshold = params$int_thres)
  peaks_clean_filtered <- result$filtered_peaks %>% filter_noisy_columns()
  
  # === Étape 3 : DBSCAN + bounding boxes ===
  processed <- process_peaks_with_dbscan(peaks_clean_filtered, rr_norm, params, step)
  
  # Renommage final pour cohérence
  #processed$peaks <- processed$peaks %>% dplyr::rename(stain_intensity = Intensity)
  
  if (verbose) {
    cat("✅ Structure finale :\n")
    str(processed)
  }
  
  return(processed)
}

