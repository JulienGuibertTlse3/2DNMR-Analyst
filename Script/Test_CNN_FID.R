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

source("spectrum_generator_random.R")

#=============================GENERATION DU DATASET DE SPECTRES==============================================

generate_spectrum_labels_full_mixed <- function(n_spectra = 2000,
                                                n_points = 2048,
                                                intensity_threshold = 0.2,
                                                center_margin = 0.2,
                                                seed = 123,
                                                mix_ratio = 0.5) {
  set.seed(seed)
  
  spectra_list <- list()
  y_class_list <- list()
  y_reg_list <- list()
  
  for (i in 1:n_spectra) {
    use_tocsy <- runif(1) < mix_ratio
    spectrum_seed <- sample.int(1e6, 1)
    
    if (use_tocsy) {
      res <- generate_dense_tocsy_1D_cut(n_points = n_points, seed = spectrum_seed, plot = FALSE)
      peaks_df <- res$true_peaks %>% rename(intensity = amplitude)
    } else {
      res <- generate_random_spectrum_with_peaks(
        min_compounds = 20,
        max_compounds = 34,
        seed = spectrum_seed
      )
      peaks_df <- res$true_peaks
    }
    
    # Interpolation à grille standard [14, 0] ppm
    spec_interp <- approx(res$spectrum$ppm, res$spectrum$intensity,
                          xout = seq(14, 0, length.out = n_points),
                          rule = 2)$y
    
    ppm_axis <- seq(14, 0, length.out = n_points)
    
    y_class <- rep(0, n_points)
    y_reg <- matrix(0, nrow = n_points, ncol = 3)
    
    peaks_idx <- sapply(peaks_df$ppm, function(ppm) which.min(abs(ppm_axis - ppm)))
    
    for (j in seq_along(peaks_idx)) {
      idx <- peaks_idx[j]
      
      if (idx >= 1 && idx <= n_points) {
        intensity <- peaks_df$intensity[j]
        ppm_val <- ppm_axis[idx]
        
        fwhh_est <- 0.01  # estimation constante
        
        rel_pos <- idx / n_points
        center_range <- c(0.5 - center_margin, 0.5 + center_margin)
        is_centered <- rel_pos >= center_range[1] && rel_pos <= center_range[2]
        is_strong <- intensity >= intensity_threshold
        
        if (is_centered && is_strong) {
          y_class[idx] <- 1
        } else {
          y_class[idx] <- 2
        }
        
        y_reg[idx, ] <- c(ppm_val, intensity, fwhh_est)
      }
    }
    
    spectra_list[[i]] <- spec_interp
    y_class_list[[i]] <- y_class
    y_reg_list[[i]] <- y_reg
  }
  
  X_array <- do.call(rbind, lapply(spectra_list, function(x) matrix(x, nrow = 1)))
  y_class_array <- do.call(rbind, y_class_list)
  y_reg_array <- array(unlist(y_reg_list), dim = c(n_spectra, n_points, 3))
  
  return(list(
    X = X_array,               # [n_spectra, 2048]
    y_class = y_class_array,   # [n_spectra, 2048]
    y_reg = y_reg_array        # [n_spectra, 2048, 3]
  ))
}
#===============================TEST DU DATASET=============================================================

plot_four_spectra <- function(X, y_class, indices = c(1, 2, 3, 4)) {
  par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))  # 2x2 layout
  
  for (i in indices) {
    spectrum <- X[i, ]
    labels <- y_class[i, ]
    
    plot(spectrum, type = "l", col = "black", lwd = 1,
         main = paste("Spectre", i),
         xlab = "Point index", ylab = "Intensité")
    
    points(which(labels == 1), spectrum[labels == 1], col = "red", pch = 19, cex = 1)
    points(which(labels == 2), spectrum[labels == 2], col = "blue", pch = 19, cex = 1)
    
    legend("topright", legend = c("pic faible (1)", "pic fort (2)"),
           col = c("blue", "red"), pch = 19, cex = 0.8)
  }
}

#data <- generate_spectrum_labels_full()
data <- generate_spectrum_labels_full_mixed(n_spectra = 1000, mix_ratio = 0.5)

set.seed(42)
plot_four_spectra(data$X, data$y_class, indices = c(sample(1:1000,1), sample(1:1000,1), sample(1:1000,1), sample(1:1000,1)))


#======================ENTRAINEMENT DU RESEAU DE NEURONES==================================================

build_peak_predictor <- function() {
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



X_train <- array(data$X, dim = c(n, n_points, 1))  #ajoute une dimension "channel" = 1
y_class_train <- data$y_class
y_reg_train <- data$y_reg


model <- build_peak_predictor()

model %>% fit(
  x = X_train,
  y = list(
    class_output = y_class_train,
    reg_output = y_reg_train
  ),
  epochs = 800,
  batch_size =32,
  validation_split = 0.2
)

#=================================PREPARATION DES VRAIES DONNEES BRUKER, STATS POUR MIEUX ANALYSER==============

resss <- read_bruker(dir = "C:/Users/norouchon/Documents/Test_N/UFCOSY/pdata/1", dim = "2D")
resss <- read_bruker(dir = "C:/Users/norouchon/Documents/2DNMR-Analyst-main/240302193_MTH_interlabo2024_AXIOM_2D/8/pdata/1", dim = "2D")
resss <- read_bruker(dir = "C:/Users/norouchon/Documents/Tocs/MTH_Melange_1.25mM/5/pdata/1", dim="2D")
resss <- read_bruker(dir = "C:/Users/norouchon/Documents/Tocs/MTH_Melange_2.5mM/6/pdata/1", dim="2D")
resss <- read_bruker(dir = "C:/Users/norouchon/Documents/Tocs/MTH_Melange_5mM/7/pdata/1", dim="2D")
resss <- read_bruker(dir = "C:/Users/norouchon/Documents/transfer_10448437_files_523289e5/201/pdata/1", dim="2D")
rr <- resss$spectrumData
str(rr)

get_spectrum_params <- function(spectrum_type) {
  switch(spectrum_type,
         "TOCSY" = list(
           int_thres = 0.001,
           int_prop = 0.001,
           eps_value = 0.0068,
           pred_class_thres = 0.00001
         ),
         "HSQC" = list(
           int_thres = 0.001,
           int_prop = 0.5,
           eps_value = 0.014,
           pred_class_thres = 0.001
         ),
         "COSY" = list(
           int_thres = 0.001,
           int_prop = 0.5,
           eps_value = 0.0068,
           pred_class_thres = 0.001
         ),
         "UFCOSY" = list(
           int_thres = 0.001,
           int_prop = 0.5,
           eps_value = 0.0068,
           pred_class_thres = 0.001
         ),
         stop("Type de spectre inconnu")
  )
}

params <- get_spectrum_params("UFCOSY")

# === Normalisation globale du spectre 2D ===
rr_abs <- abs(rr)
rr_norm <- (rr_abs - min(rr_abs)) / (max(rr_abs) - min(rr_abs))
rr_log <- log1p(rr_norm * 100)  # log(1+x), on peut ajuster le facteur
rr_log <- rr_log / max(rr_log)  # renormaliser entre 0 et 1

rownames(rr) <- as.numeric(rownames(rr))  # F2 (lignes)
colnames(rr) <- as.numeric(colnames(rr))  # F1 (colonnes)

#=================================TENTATIVE DE RECONSTRUCTION DU SPECTRE==================================

# ======= Prédiction complète sur les lignes et colonnes du spectre 2D =========

#===Affichage===
n_row <- nrow(rr_norm)
n_col <- ncol(rr_norm)
ppm_x <- seq(14, 0, length.out = n_col)  # axe x
ppm_y <- seq(14, 0, length.out = n_row)  # axe y

detected_peaks <- data.frame(F2 = numeric(0), F1 = numeric(0))
#=====================================TEST DE PREDICTION UNIQUEMENT APPLIQUABLE A UFCOSY================

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
detected_peaks <- get_detected_peaks_with_intensity(rr_norm, model, target_length = 2048)
#write.csv(detected_peaks, file = "detected_peaks.csv", row.names = FALSE)

p <- ggplot(rr_melt, aes(x = F1, y = F2, fill = Intensity)) +
  geom_raster(interpolate = TRUE) +
  scale_fill_gradient(low = "white", high = "gray20") +
  geom_point(
    data = detected_peaks,
    aes(
      x = F1,
      y = F2,
      color = Intensity,
      text = paste0("F1: ", round(F1, 3),
                    "<br>F2: ", round(F2, 3),
                    "<br>Intensité prédite: ", round(Intensity, 3))
    ),
    size = 1.5,
    inherit.aes = FALSE
  ) +
  scale_color_viridis(option = "plasma", name = "Intensité\nprédite") +
  scale_x_reverse() +
  scale_y_reverse() +
  labs(
    title = "Spectre RMN 2D avec pics détectés",
    x = "ppm F1", y = "ppm F2"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )


ggplotly(p, tooltip = "text")

#==========================================GENERALISATION A TOUT TYPE DE SPECTRES===============================

predict_peaks_1D_batch <- function(spectrum_mat, model, axis = c("rows", "columns"),
                                   threshold_class = NULL, batch_size = NULL, verbose = TRUE) {
  axis <- match.arg(axis)
  
  if (verbose) {
    cat("Dimension spectre input :", dim(spectrum_mat), "\n")
  }
  
  # Si axis = "columns", on transpose pour avoir vecteurs 1D en lignes
  mat <- if(axis == "columns") t(spectrum_mat) else spectrum_mat
  n_vectors <- nrow(mat)  # nombre de vecteurs (lignes ou colonnes)
  n_points <- ncol(mat)
  
  if (verbose) {
    cat("Nombre de vecteurs :", n_vectors, " - Points par vecteur :", n_points, "\n")
  }
  
  # On suppose que le modèle attend un input shape = (n_vectors, 2048, 1)
  # Si la taille diffère, essayer d'ajuster ou avertir
  if (n_points != 2048) {
    warning(sprintf("Attention: nombre de points par vecteur = %d ≠ 2048, on va donxc s'apprêter à formater", n_points))
  }
  
  # Formatage input pour le modèle : [n_vectors, n_points, 1]
  input_array <- array(mat, dim = c(n_vectors, n_points, 1))
  
  detected_list <- vector("list", length = ceiling(n_vectors / batch_size))
  
  pb <- txtProgressBar(min = 0, max = n_vectors, style = 3)
  
  idx_list <- 1
  for (start_idx in seq(1, n_vectors, by = batch_size)) {
    end_idx <- min(start_idx + batch_size - 1, n_vectors)
    batch <- input_array[start_idx:end_idx,,,drop=FALSE]
    
    preds <- predict(model, batch, batch_size = batch_size)
    probs <- preds[[1]]  # shape: [batch_size, n_points, nb_classes]
    regs <- preds[[2]]   # shape: [batch_size, n_points, nb_classes]
    
    # Affichage debug
    if (verbose) {
      max_prob <- max(probs[,,2])
      mean_prob <- mean(probs[,,2])
      cat(sprintf("\nBatch %d-%d : max prob pic = %.3f, mean prob pic = %.3f\n",
                  start_idx, end_idx, max_prob, mean_prob))
    }
    
    prob_peak <- probs[,,2]  # probabilité classe pic (index 2)
    reg_intensity <- regs[,,2]  # intensité associée
    
    detected_batch <- list()
    for (i in seq_len(nrow(prob_peak))) {
      idxs <- which(prob_peak[i, ] > threshold_class)
      if (length(idxs) > 0) {
        df <- data.frame(
          F1 = if (axis == "rows") idxs else (start_idx + i - 1),
          F2 = if (axis == "rows") (start_idx + i - 1) else idxs,
          Intensity = reg_intensity[i, idxs]
        )
        detected_batch[[length(detected_batch) + 1]] <- df
      }
    }
    
    if (length(detected_batch) > 0) {
      detected_list[[idx_list]] <- do.call(rbind, detected_batch)
    } else {
      detected_list[[idx_list]] <- NULL
    }
    
    setTxtProgressBar(pb, end_idx)
    idx_list <- idx_list + 1
  }
  
  close(pb)
  
  detected <- do.call(rbind, detected_list)
  if (is.null(detected)) detected <- data.frame(F1=numeric(), F2=numeric(), Intensity=numeric())
  
  if (verbose) {
    cat("Nombre total de pics détectés :", nrow(detected), "\n")
  }
  
  return(detected)
}

#prédiction classique et nettoyage :
start <- Sys.time()
peaks_rows <- predict_peaks_1D_batch(rr_log, model, axis = "rows", threshold_class = 0.01, batch_size = 64)
peaks_cols <- predict_peaks_1D_batch(rr_log, model, axis = "columns", threshold_class = 0.01, batch_size = 64)
# peaks_rows <- predict_peaks_1D_batch(rr_log, model, axis = "rows", threshold_class = 0.00001, batch_size = 64) #test pour la tocsy cheloue
# peaks_cols <- predict_peaks_1D_batch(rr_log, model, axis = "columns", threshold_class = 0.00001, batch_size = 64)
end <- Sys.time()
elapsed <- end - start
print(elapsed)
peaks_all <- rbind(peaks_rows, peaks_cols)
peaks_clean <- peaks_all

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
filter_noisy_columns <- function(peaks_df, threshold_ratio = 0.999, min_col_max = NULL) {
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
clean_peak_clusters_dbscan <- function(peaks_df, ppm_x, ppm_y, eps_ppm = params$eps_value, minPts = 3) {
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

result <- filter_peaks_by_proportion(peaks_clean, threshold = 0.5, intensity_threshold = 0.2) # intensity_threshold à 0.001 de base
peaks_clean_filtered <- result$filtered_peaks
peaks_clean_filtered <- filter_noisy_columns(peaks_clean_filtered)

# peaks_clean_filtered <- remove_peaks_ppm_range(
#   peaks_clean_filtered,
#   rr_norm,
#   axis = "F1",
#   ppm_min = 4.79,
#   ppm_max = 4.81
# )

plot_peaks_ppm_plotly_clean <- function(peaks,
                                        rr_norm = NULL,
                                        intensity_threshold = NULL,
                                        ratio = NULL) {
  # --- sanity checks ---
  peaks <- as.data.frame(peaks, stringsAsFactors = FALSE)
  if (!all(c("F1", "F2", "Intensity") %in% colnames(peaks))) {
    stop("peaks must be a data.frame with columns: F1, F2, Intensity")
  }
  if (!is.null(intensity_threshold)) {
    peaks <- peaks[peaks$Intensity > intensity_threshold, , drop = FALSE]
  }
  if (nrow(peaks) == 0) {
    warning("No peaks after thresholding -> returning empty plot.")
    return(plot_ly() %>% layout(title = "No peaks"))
  }
  if (is.null(rr_norm)) stop("You must provide rr_norm (matrix/data.frame with rownames and colnames as ppm).")
  
  # --- get ppm axes from rr_norm ---
  ppm_y <- as.numeric(rownames(rr_norm))   # F2 axis (rows)
  ppm_x <- as.numeric(colnames(rr_norm))   # F1 axis (cols)
  if (any(is.na(ppm_x)) || any(is.na(ppm_y))) stop("rownames(rr_norm) and colnames(rr_norm) must be numeric/convertible to numeric ppm values.")
  
  # --- mapping helper (handles indices or ppm values) ---
  map_to_ppm <- function(vals, ppm_axis) {
    vnum <- as.numeric(vals)
    n <- length(ppm_axis)
    axis_min <- min(ppm_axis, na.rm = TRUE)
    axis_max <- max(ppm_axis, na.rm = TRUE)
    
    # CASE 1: all values look like integer indices in [1..n]
    if (all(!is.na(vnum) & vnum >= 1 & vnum <= n & abs(vnum - round(vnum)) < 1e-6)) {
      idx <- pmax(1, pmin(n, round(vnum)))
      return(as.numeric(ppm_axis[idx]))
    }
    # CASE 2: all values fall inside the ppm axis range -> map to nearest ppm point
    if (all(!is.na(vnum) & vnum >= axis_min & vnum <= axis_max)) {
      return(as.numeric(sapply(vnum, function(v) ppm_axis[which.min(abs(ppm_axis - v))])))
    }
    # FALLBACK: handle element-wise
    sapply(vnum, function(v) {
      if (is.na(v)) return(NA_real_)
      if (v >= 1 && v <= n && abs(v - round(v)) < 1e-6) {
        return(ppm_axis[round(v)])
      } else if (v >= axis_min && v <= axis_max) {
        return(ppm_axis[which.min(abs(ppm_axis - v))])
      } else {
        # map to closest ppm anyway
        return(ppm_axis[which.min(abs(ppm_axis - v))])
      }
    })
  }
  
  # --- compute ppm coords ---
  F1_ppm <- map_to_ppm(peaks$F1, ppm_x)
  F2_ppm <- map_to_ppm(peaks$F2, ppm_y)
  
  # attach hover text
  hover_text <- paste0(
    "F1: ", round(F1_ppm, 5), " ppm",
    "<br>F2: ", round(F2_ppm, 5), " ppm",
    "<br>Intensity: ", signif(peaks$Intensity, 5)
  )
  
  # --- build plotly scatter ---
  xaxis <- list(title = "F1 (ppm)", autorange = "reversed")
  if (!is.null(ratio)) {
    yaxis <- list(title = "F2 (ppm)", autorange = "reversed", scaleanchor = "x", scaleratio = ratio)
  } else {
    yaxis <- list(title = "F2 (ppm)", autorange = "reversed")
  }
  
  p <- plot_ly(
    x = ~F1_ppm,
    y = ~F2_ppm,
    type = "scatter",
    mode = "markers",
    marker = list(size = 2, color = peaks$Intensity, colorscale = "Viridis", showscale = TRUE),
    text = hover_text,
    hoverinfo = "text"
  ) %>%
    layout(title = "Detected peaks (ppm scale from rr_norm)",
           xaxis = xaxis, yaxis = yaxis, plot_bgcolor = "white")
  
  return(p)
}

p_filtered <- plot_peaks_ppm_plotly_clean(peaks_clean_filtered, rr_norm = rr_log,
                                           intensity_threshold = params$int_thres, ratio = 1)
p_filtered

# ===================== EXTRACTION ALEATOIRE DE 4 SPECTRES 1D - PLOTS SEPARES ==================================
rr_abs <- abs(rr)
rr_norm <- (rr_abs - min(rr_abs)) / (max(rr_abs) - min(rr_abs))

plot_random_1D_grid <- function(rr_mat, type = c("rows", "columns"), n_extract = 4) {
  type <- match.arg(type)
  
  if (n_extract != 4) stop("Cette version est prévue pour exactement 4 spectres (2x2).")
  
  if (type == "rows") {
    idx <- sample(1:nrow(rr_mat), n_extract)
    ppm_axis <- as.numeric(colnames(rr_mat))
    spectra_list <- lapply(idx, function(i) list(ppm = ppm_axis, intensity = rr_mat[i, ], label = paste("Ligne", i)))
  } else {
    idx <- sample(1:ncol(rr_mat), n_extract)
    ppm_axis <- as.numeric(rownames(rr_mat))
    spectra_list <- lapply(idx, function(i) list(ppm = ppm_axis, intensity = rr_mat[, i], label = paste("Colonne", i)))
  }
  
  # Crée les 4 plots
  plots <- lapply(spectra_list, function(sp) {
    plot_ly(x = sp$ppm, y = sp$intensity, type = 'scatter', mode = 'lines') %>%
      layout(
        title = sp$label,
        xaxis = list(title = "ppm", autorange = "reversed"),
        yaxis = list(title = "Intensité")
      )
  })
  
  # Combine en grille 2x2
  subplot(
    plots[[1]], plots[[2]],
    plots[[3]], plots[[4]],
    nrows = 2, shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE
  )
}

# Exemple pour 4 lignes aléatoires
plot_random_1D_grid(rr_log, type = "rows", n_extract = 4)

# Exemple pour 4 colonnes aléatoires
plot_random_1D_grid(rr_log, type = "columns", n_extract = 4)

#====================================TEST D'AFFICHAGE DES CONTOURS + CLUSTERING==================================
#si on est dans le cas d'une TOCSY complexe type données MTH (pas nécessaire sinon)
rr_norm <- rr_log
# === Fonctions utilitaires ===
downsample_matrix <- function(mat, step = 4) {
  mat[seq(1, nrow(mat), by = step),
      seq(1, ncol(mat), by = step)]
}

downsample_axis <- function(axis_vals, step = 4) {
  axis_vals[seq(1, length(axis_vals), by = step)]
}

# === Matrice du spectre ===
z_matrix <- rr_norm  # matrice F2 x F1


x_vals   <- as.numeric(colnames(rr_norm))  # F1 (ppm)
y_vals   <- as.numeric(rownames(rr_norm))  # F2 (ppm)

# === Seuil pour contours ===
contour_start <- 0.001 * max(z_matrix)

# === Downsampling pour alléger le rendu ===
step <- 4
z_small <- downsample_matrix(z_matrix, step = step)
x_small <- downsample_axis(x_vals, step = step)
y_small <- downsample_axis(y_vals, step = step)

# --- 1) Normalisation z-score pour clustering ---
peaks_norm <- peaks_clean_filtered %>%
  mutate(
    F1_scaled = (F1 - mean(F1, na.rm = TRUE)) / sd(F1, na.rm = TRUE),
    F2_scaled = (F2 - mean(F2, na.rm = TRUE)) / sd(F2, na.rm = TRUE)
  )

# --- 2) DBSCAN (sur coordonnées normalisées) ---
db <- dbscan::dbscan(peaks_norm[, c("F1_scaled", "F2_scaled")],
                     eps = 0.02, minPts = 2)

# Ajouter cluster_db à peaks_clean_filtered (fusion option 2)
peaks_clean_filtered <- peaks_clean_filtered %>%
  mutate(cluster_db = db$cluster)

# --- 3) Bounding boxes + intensité max ---
bounding_boxes <- peaks_clean_filtered %>%
  dplyr::filter(cluster_db > 0) %>%   # <--- bien préciser dplyr::filter
  group_by(cluster_db) %>%
  summarise(
    xmin = min(F1, na.rm = TRUE),
    xmax = max(F1, na.rm = TRUE),
    ymin = min(F2, na.rm = TRUE),
    ymax = max(F2, na.rm = TRUE),
    cx   = (min(F1, na.rm = TRUE) + max(F1, na.rm = TRUE)) / 2,
    cy   = (min(F2, na.rm = TRUE) + max(F2, na.rm = TRUE)) / 2,
    intensity = max(Intensity, na.rm = TRUE),
    .groups = "drop"
  )

# --- 4) Conversion indices → ppm ---
peaks_clean_filtered <- peaks_clean_filtered %>%
  mutate(
    F1_ppm = x_vals[F1],
    F2_ppm = y_vals[F2]
  )

bounding_boxes <- bounding_boxes %>%
  mutate(
    xmin_ppm = x_vals[xmin],
    xmax_ppm = x_vals[xmax],
    ymin_ppm = y_vals[ymin],
    ymax_ppm = y_vals[ymax],
    cx_ppm   = x_vals[cx],
    cy_ppm   = y_vals[cy]
  )

# --- 5) Nettoyage doublons ---
if (nrow(bounding_boxes) > 1) {
  to_remove <- c()
  for (i in seq_len(nrow(bounding_boxes))) {
    current <- bounding_boxes[i, ]
    for (j in seq_len(nrow(bounding_boxes))) {
      if (i == j) next
      compare <- bounding_boxes[j, ]
      
      inside_box <- current$cx_ppm >= compare$xmin_ppm & current$cx_ppm <= compare$xmax_ppm &
        current$cy_ppm >= compare$ymin_ppm & current$cy_ppm <= compare$ymax_ppm
      lower_intensity <- current$intensity < compare$intensity
      
      if (inside_box && lower_intensity) {
        to_remove <- c(to_remove, current$cluster_db)
        break
      }
    }
  }
  bounding_boxes <- bounding_boxes %>%
    dplyr::filter(!cluster_db %in% to_remove)
}

# --- 6) Rectangles pour plotly ---
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

# --- 7) Plot contour + pics + bounding boxes ---
p_plotly <- plot_ly(
  x = x_small, y = y_small, z = z_small,
  type = "contour",
  contours = list(
    start = contour_start,
    end   = max(z_small),
    size  = (max(z_small) - contour_start) / 15
  ),
  colorscale = "Greys",
  reversescale = TRUE
) %>%
  # Pics
  # add_markers(
  #   data = peaks_clean_filtered,
  #   x = ~F1_ppm, y = ~F2_ppm,
  #   color = ~Intensity,
  #   colors = viridis::viridis(100),
  #   marker = list(size = 3),
  #   inherit = FALSE
  # ) %>%
  # Centres clusters
  { if (nrow(bounding_boxes) > 0)
    add_markers(., data = bounding_boxes,
                x = ~cx_ppm, y = ~cy_ppm,
                marker = list(symbol = "x", color = "blue", size = 6),
                showlegend = FALSE, inherit = FALSE) else . } %>%
  layout(
    shapes = shapes_list,
    xaxis = list(title = "F1 (ppm)", autorange = "reversed"),
    yaxis = list(title = "F2 (ppm)", autorange = "reversed")
  )

p_plotly

# --- Sauvegarde des centres ---
centers <- bounding_boxes %>%
  select(cluster_db, cx_ppm, cy_ppm, intensity)

write.csv(centers, "bounding_box_centers.csv", row.names = FALSE)

#==================================FUSIOOONNNNNN===============================================================
library(dplyr)
library(readr)

# Charger les deux CSV
tocs <- read_csv("C:/Users/norouchon/Documents/2DNMR-Analyst-main/Script/centroid_exports/5_projected_centroids.csv")
boxes <- read_csv("bounding_box_centers.csv")

# Harmoniser les colonnes avec labels fixes
tocs_points <- tocs %>%
  transmute(source = "TOCSY_JULIEN",
            F1_ppm = F1_ppm,
            F2_ppm = F2_ppm)

boxes_points <- boxes %>%
  transmute(source = "TOCSY_NOEMIE",
            F1_ppm = cx_ppm,
            F2_ppm = cy_ppm)

# Concaténation "à la suite"
all_points <- bind_rows(tocs_points, boxes_points) %>%
  mutate(point_id = row_number()) %>%
  select(point_id, source, F1_ppm, F2_ppm)

# Sauvegarde
write_csv(all_points, "merged_points_named_5.csv")

#==========================================TEST STATISTIQUES SUR LES RESSEMBLANCES=========================

#Charger les données
points <- read.csv("merged_points_named_5.csv")

#Définir le seuil de proximité (en ppm)
tol <- 0.1

#Calculer la distance euclidienne entre tous les points
dist_matrix <- as.matrix(dist(points[, c("F1_ppm", "F2_ppm")]))

#Créer un vecteur pour assigner un ID de groupe à chaque point
group_id <- rep(NA, nrow(points))
current_group <- 1

for (i in 1:nrow(points)) {
  if (is.na(group_id[i])) {
    # Les points proches de i (y compris i) deviennent le même groupe
    close_points <- which(dist_matrix[i, ] < tol)
    group_id[close_points] <- current_group
    current_group <- current_group + 1
  }
}

#Ajouter le group_id aux données
points$group_id <- group_id

#Résumer chaque groupe (position moyenne)

summary_points <- points %>%
  group_by(group_id) %>%
  summarise(
    F1_mean = mean(F1_ppm),
    F2_mean = mean(F2_ppm),
    n_points = n(),
    points_ids = paste(point_id, collapse = ", ")
  )

#Afficher le résumé
print(summary_points)


# Charger les données
merged_points <- read.csv("merged_points_named_5.csv")

# Trier par F2_ppm croissant
merged_points_sorted <- merged_points[order(merged_points$F2_ppm), ]

# Sauvegarder le fichier trié
write.csv(merged_points_sorted, "merged_points_sorted_5.csv", row.names = FALSE)

df <- read.csv("merged_points_sorted_5.csv", sep = ",")  # lecture classique
write.csv2(df, "merged_points_sorted_clean_5.csv", row.names = FALSE)  

