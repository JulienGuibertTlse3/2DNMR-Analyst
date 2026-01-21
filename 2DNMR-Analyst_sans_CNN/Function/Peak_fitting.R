# Function/Peak_fitting.R

#' Detect local maxima in a 2D region
#' @param mat Matrix of intensities
#' @param threshold Minimum intensity relative to max (0-1)
#' @param min_distance Minimum distance between peaks (in pixels)
#' @return Data frame with local maxima coordinates
detect_local_maxima <- function(mat, threshold = 0.3, min_distance = 2) {
  nr <- nrow(mat)
  nc <- ncol(mat)
  
  if (nr < 3 || nc < 3) return(data.frame(row = integer(), col = integer(), value = numeric()))
  
  max_val <- max(mat, na.rm = TRUE)
  min_val <- min(mat, na.rm = TRUE)
  thresh_abs <- min_val + threshold * (max_val - min_val)
  
  maxima <- data.frame(row = integer(), col = integer(), value = numeric())
  
  for (i in 2:(nr-1)) {
    for (j in 2:(nc-1)) {
      val <- mat[i, j]
      if (is.na(val) || val < thresh_abs) next
      
      # Check if local maximum (8-connectivity)
      neighborhood <- mat[max(1,i-1):min(nr,i+1), max(1,j-1):min(nc,j+1)]
      if (val >= max(neighborhood, na.rm = TRUE)) {
        maxima <- rbind(maxima, data.frame(row = i, col = j, value = val))
      }
    }
  }
  
  if (nrow(maxima) == 0) return(maxima)
  
  # Remove peaks too close to each other (keep the highest)
  maxima <- maxima[order(-maxima$value), ]
  keep <- rep(TRUE, nrow(maxima))
  
  for (i in seq_len(nrow(maxima))) {
    if (!keep[i]) next
    for (j in seq_len(nrow(maxima))) {
      if (i >= j || !keep[j]) next
      dist <- sqrt((maxima$row[i] - maxima$row[j])^2 + (maxima$col[i] - maxima$col[j])^2)
      if (dist < min_distance) {
        keep[j] <- FALSE
      }
    }
  }
  
  maxima[keep, ]
}


#' Pseudo-Voigt 2D profile function
#' Approximation of the Voigt profile as a linear combination of Gaussian and Lorentzian
#' @param x X coordinate
#' @param y Y coordinate
#' @param A Amplitude
#' @param x0 Center X
#' @param y0 Center Y
#' @param sigma_x Gaussian width in X
#' @param sigma_y Gaussian width in Y
#' @param gamma_x Lorentzian width in X
#' @param gamma_y Lorentzian width in Y
#' @param eta Mixing parameter (0 = pure Gaussian, 1 = pure Lorentzian)
#' @return Intensity value
pseudo_voigt_2d <- function(x, y, A, x0, y0, sigma_x, sigma_y, gamma_x, gamma_y, eta) {
  # Gaussian component
  gauss <- exp(-((x - x0)^2 / (2 * sigma_x^2) + (y - y0)^2 / (2 * sigma_y^2)))
  
  # Lorentzian component
  lorentz <- 1 / (1 + ((x - x0) / gamma_x)^2 + ((y - y0) / gamma_y)^2)
  
  # Pseudo-Voigt: linear combination
  A * (eta * lorentz + (1 - eta) * gauss)
}


#' Fit 2D peak to a spectral region (VERSION ROBUSTE avec support MULTIPLET)
#' 
#' @param mat Matrix of spectral intensities
#' @param ppm_x Vector of F2 chemical shifts
#' @param ppm_y Vector of F1 chemical shifts
#' @param box Data frame with xmin, xmax, ymin, ymax
#' @param model Type of peak model: "gaussian", "voigt"
#' @param min_points Minimum number of points required for fitting
#' @return List with fitted parameters and volume
fit_2d_peak <- function(mat, ppm_x, ppm_y, box, model = "gaussian", min_points = 25) {
  
  # Extract region
  x_idx <- which(ppm_x >= box$xmin & ppm_x <= box$xmax)
  y_idx <- which(ppm_y >= box$ymin & ppm_y <= box$ymax)
  
  if (length(x_idx) == 0 || length(y_idx) == 0) {
    return(list(volume = NA, fit_quality = NA, params = NULL, 
                method = "failed", error = "No points in region"))
  }
  
  # Vérifier qu'on a assez de points
  if (length(x_idx) * length(y_idx) < min_points) {
    # Fallback sur la somme
    region <- mat[y_idx, x_idx, drop = FALSE]
    volume_sum <- sum(region, na.rm = TRUE)
    return(list(
      volume = volume_sum,
      fit_quality = NA,
      params = NULL,
      method = "sum_fallback",
      error = "Too few points for fitting"
    ))
  }
  
  region <- mat[y_idx, x_idx, drop = FALSE]
  x_sub <- ppm_x[x_idx]
  y_sub <- ppm_y[y_idx]
  
  # Vérifier que la région n'est pas vide ou constante
  if (all(is.na(region)) || sd(as.vector(region), na.rm = TRUE) < 1e-10) {
    return(list(
      volume = sum(region, na.rm = TRUE),
      fit_quality = NA,
      params = NULL,
      method = "sum_fallback",
      error = "Region is constant or empty"
    ))
  }
  
  # ========== DÉTECTION DE MULTIPLETS ==========
  # Détecter si on a plusieurs pics dans la région
  local_max <- detect_local_maxima(region, threshold = 0.3, min_distance = 2)
  n_peaks <- nrow(local_max)
  is_multiplet <- n_peaks > 1
  
  # Si multiplet détecté, fitter chaque pic séparément
  if (is_multiplet) {
    
    # Résultats pour chaque pic
    peak_volumes <- numeric(n_peaks)
    peak_r_squared <- numeric(n_peaks)
    peak_centers_x <- numeric(n_peaks)
    peak_centers_y <- numeric(n_peaks)
    peak_fitted_values <- vector("list", n_peaks)
    
    total_fitted_vals <- rep(0, length(x_sub) * length(y_sub))
    
    for (p in seq_len(n_peaks)) {
      # Coordonnées du pic
      peak_row <- local_max$row[p]
      peak_col <- local_max$col[p]
      peak_x <- x_sub[peak_col]
      peak_y <- y_sub[peak_row]
      peak_amplitude <- local_max$value[p]
      
      # Créer une sous-région autour de ce pic
      # Estimer la largeur du pic (distance au pic voisin le plus proche / 2)
      if (n_peaks > 1) {
        distances <- sqrt((local_max$col - peak_col)^2 + (local_max$row - peak_row)^2)
        distances[p] <- Inf  # Exclure le pic lui-même
        min_dist <- min(distances)
        half_width_pixels <- max(2, floor(min_dist / 2))
      } else {
        half_width_pixels <- 3
      }
      
      # Indices pour la sous-région
      col_start <- max(1, peak_col - half_width_pixels)
      col_end <- min(ncol(region), peak_col + half_width_pixels)
      row_start <- max(1, peak_row - half_width_pixels)
      row_end <- min(nrow(region), peak_row + half_width_pixels)
      
      sub_region <- region[row_start:row_end, col_start:col_end, drop = FALSE]
      sub_x <- x_sub[col_start:col_end]
      sub_y <- y_sub[row_start:row_end]
      
      # Créer le grid pour ce pic
      sub_grid <- expand.grid(x = sub_x, y = sub_y)
      sub_grid$z <- as.vector(t(sub_region))
      sub_grid <- sub_grid[!is.na(sub_grid$z), ]
      
      if (nrow(sub_grid) < 9) {
        # Pas assez de points, utiliser la somme pour ce pic
        peak_volumes[p] <- sum(sub_region, na.rm = TRUE)
        peak_r_squared[p] <- NA
        peak_centers_x[p] <- peak_x
        peak_centers_y[p] <- peak_y
        next
      }
      
      # Normalisation
      z_scale <- max(abs(sub_grid$z), na.rm = TRUE)
      if (z_scale < 1e-10) z_scale <- 1
      sub_grid$z_norm <- sub_grid$z / z_scale
      
      # Paramètres initiaux pour ce pic
      baseline_init <- quantile(sub_grid$z, 0.1, na.rm = TRUE) / z_scale
      sigma_x_init <- diff(range(sub_x)) / 4
      sigma_y_init <- diff(range(sub_y)) / 4
      
      # Tenter le fit
      fit_single <- tryCatch({
        if (model == "gaussian") {
          fit <- minpack.lm::nlsLM(
            z_norm ~ A * exp(-((x - x0)^2 / (2 * sx^2) + (y - y0)^2 / (2 * sy^2))) + b,
            data = sub_grid,
            start = list(A = peak_amplitude / z_scale, x0 = peak_x, y0 = peak_y,
                         sx = sigma_x_init, sy = sigma_y_init, b = baseline_init),
            lower = c(A = 0, x0 = min(sub_x), y0 = min(sub_y),
                      sx = diff(range(sub_x)) / 20, sy = diff(range(sub_y)) / 20, b = -Inf),
            upper = c(A = Inf, x0 = max(sub_x), y0 = max(sub_y),
                      sx = diff(range(sub_x)) * 2, sy = diff(range(sub_y)) * 2, b = Inf),
            control = list(maxiter = 100, gtol = 0)
          )
        } else {
          # Voigt (pseudo-Voigt)
          fit <- minpack.lm::nlsLM(
            z_norm ~ A * (eta / (1 + ((x - x0) / gx)^2 + ((y - y0) / gy)^2) + 
                            (1 - eta) * exp(-((x - x0)^2 / (2 * sx^2) + (y - y0)^2 / (2 * sy^2)))) + b,
            data = sub_grid,
            start = list(A = peak_amplitude / z_scale, x0 = peak_x, y0 = peak_y,
                         sx = sigma_x_init, sy = sigma_y_init,
                         gx = sigma_x_init, gy = sigma_y_init,
                         eta = 0.5, b = baseline_init),
            lower = c(A = 0, x0 = min(sub_x), y0 = min(sub_y),
                      sx = diff(range(sub_x)) / 20, sy = diff(range(sub_y)) / 20,
                      gx = diff(range(sub_x)) / 20, gy = diff(range(sub_y)) / 20,
                      eta = 0, b = -Inf),
            upper = c(A = Inf, x0 = max(sub_x), y0 = max(sub_y),
                      sx = diff(range(sub_x)) * 2, sy = diff(range(sub_y)) * 2,
                      gx = diff(range(sub_x)) * 2, gy = diff(range(sub_y)) * 2,
                      eta = 1, b = Inf),
            control = list(maxiter = 100, gtol = 0)
          )
        }
        
        params <- coef(fit)
        fitted_vals <- fitted(fit) * z_scale
        
        # Calculer R²
        residuals <- sub_grid$z - fitted_vals
        ss_res <- sum(residuals^2)
        ss_tot <- sum((sub_grid$z - mean(sub_grid$z))^2)
        r2 <- max(0, 1 - (ss_res / ss_tot))
        
        # Volume = somme des valeurs fittées
        vol <- sum(fitted_vals, na.rm = TRUE)
        
        list(volume = vol, r_squared = r2, 
             center_x = params["x0"], center_y = params["y0"],
             fitted_vals = fitted_vals, success = TRUE)
        
      }, error = function(e) {
        # Fallback: somme pour ce pic
        list(volume = sum(sub_region, na.rm = TRUE), r_squared = NA,
             center_x = peak_x, center_y = peak_y,
             fitted_vals = NULL, success = FALSE)
      })
      
      peak_volumes[p] <- fit_single$volume
      peak_r_squared[p] <- fit_single$r_squared
      peak_centers_x[p] <- fit_single$center_x
      peak_centers_y[p] <- fit_single$center_y
      peak_fitted_values[[p]] <- fit_single$fitted_vals
    }
    
    # Agréger les résultats
    total_volume <- sum(peak_volumes, na.rm = TRUE)
    mean_r_squared <- mean(peak_r_squared, na.rm = TRUE)
    
    # Centre pondéré par les volumes
    if (sum(peak_volumes, na.rm = TRUE) > 0) {
      weights <- peak_volumes / sum(peak_volumes, na.rm = TRUE)
      center_x <- sum(peak_centers_x * weights, na.rm = TRUE)
      center_y <- sum(peak_centers_y * weights, na.rm = TRUE)
    } else {
      center_x <- mean(peak_centers_x, na.rm = TRUE)
      center_y <- mean(peak_centers_y, na.rm = TRUE)
    }
    
    return(list(
      volume = total_volume,
      fit_quality = mean_r_squared,
      params = c(x0 = center_x, y0 = center_y),
      fitted_values = NULL,  # Trop complexe à recombiner
      residuals = NULL,
      method = "multiplet_fit",
      n_peaks = n_peaks,
      is_multiplet = TRUE,
      peak_volumes = peak_volumes,
      peak_r_squared = peak_r_squared,
      peak_centers_x = peak_centers_x,
      peak_centers_y = peak_centers_y,
      error = NULL
    ))
  }
  
  # Create grid
  grid <- expand.grid(x = x_sub, y = y_sub)
  grid$z <- as.vector(t(region))
  grid <- grid[!is.na(grid$z), ]  # Enlever les NA
  
  if (nrow(grid) < min_points) {
    return(list(
      volume = sum(region, na.rm = TRUE),
      fit_quality = NA,
      params = NULL,
      method = "sum_fallback",
      error = "Too many NA values"
    ))
  }
  
  # ========== ESTIMATION ROBUSTE DES PARAMÈTRES INITIAUX ==========
  
  # Trouver le maximum global pour centrer
  max_idx <- which.max(grid$z)
  center_x_init <- grid$x[max_idx]
  center_y_init <- grid$y[max_idx]
  amplitude_init <- max(grid$z, na.rm = TRUE)
  
  baseline_init <- quantile(grid$z, 0.1, na.rm = TRUE)  # 10e percentile comme baseline
  
  # Estimations des largeurs basées sur FWHM approximatif
  # Trouver les points > 50% du max
  half_max <- (amplitude_init + baseline_init) / 2
  points_half <- grid[grid$z > half_max, ]
  
  if (nrow(points_half) > 3) {
    sigma_x_init <- sd(points_half$x) * 1.5  # Facteur empirique
    sigma_y_init <- sd(points_half$y) * 1.5
  } else {
    # Fallback sur la taille de la box
    sigma_x_init <- (box$xmax - box$xmin) / 4
    sigma_y_init <- (box$ymax - box$ymin) / 4
  }
  
  # S'assurer que les sigma ne sont pas trop petits
  sigma_x_init <- max(sigma_x_init, diff(range(x_sub)) / 10)
  sigma_y_init <- max(sigma_y_init, diff(range(y_sub)) / 10)
  
  # Normaliser les données pour améliorer la convergence
  z_scale <- max(abs(grid$z), na.rm = TRUE)
  if (z_scale < 1e-10) z_scale <- 1
  grid$z_norm <- grid$z / z_scale
  
  # ========== MODÈLE ET FITTING ==========
  
  if (model == "gaussian") {
    
    fit_formula <- z_norm ~ A * exp(-((x - x0)^2 / (2 * sx^2) + (y - y0)^2 / (2 * sy^2))) + b
    
    start_params <- list(
      A = amplitude_init / z_scale,
      x0 = center_x_init,
      y0 = center_y_init,
      sx = sigma_x_init,
      sy = sigma_y_init,
      b = baseline_init / z_scale
    )
    
    # Contraintes pour éviter les paramètres aberrants
    lower_bounds <- c(
      A = 0,  # Amplitude positive
      x0 = min(x_sub),
      y0 = min(y_sub),
      sx = diff(range(x_sub)) / 20,  # Largeur min
      sy = diff(range(y_sub)) / 20,
      b = -Inf
    )
    
    upper_bounds <- c(
      A = Inf,
      x0 = max(x_sub),
      y0 = max(y_sub),
      sx = diff(range(x_sub)) * 2,  # Largeur max
      sy = diff(range(y_sub)) * 2,
      b = Inf
    )
    
  } else if (model == "voigt") {
    
    # Pseudo-Voigt 2D: combinaison linéaire de Gaussienne et Lorentzienne
    # eta = paramètre de mélange (0 = Gaussien pur, 1 = Lorentzien pur)
    fit_formula <- z_norm ~ A * (eta / (1 + ((x - x0) / gx)^2 + ((y - y0) / gy)^2) + 
                                   (1 - eta) * exp(-((x - x0)^2 / (2 * sx^2) + (y - y0)^2 / (2 * sy^2)))) + b
    
    start_params <- list(
      A = amplitude_init / z_scale,
      x0 = center_x_init,
      y0 = center_y_init,
      sx = sigma_x_init,      # Largeur Gaussienne
      sy = sigma_y_init,
      gx = sigma_x_init,      # Largeur Lorentzienne
      gy = sigma_y_init,
      eta = 0.5,              # Mélange 50-50 par défaut
      b = baseline_init / z_scale
    )
    
    lower_bounds <- c(
      A = 0,
      x0 = min(x_sub),
      y0 = min(y_sub),
      sx = diff(range(x_sub)) / 20,
      sy = diff(range(y_sub)) / 20,
      gx = diff(range(x_sub)) / 20,
      gy = diff(range(y_sub)) / 20,
      eta = 0,                # Pur Gaussien
      b = -Inf
    )
    
    upper_bounds <- c(
      A = Inf,
      x0 = max(x_sub),
      y0 = max(y_sub),
      sx = diff(range(x_sub)) * 2,
      sy = diff(range(y_sub)) * 2,
      gx = diff(range(x_sub)) * 2,
      gy = diff(range(y_sub)) * 2,
      eta = 1,                # Pur Lorentzien
      b = Inf
    )
    
  } else {
    stop("Model not supported. Use 'gaussian' or 'voigt'")
  }
  
  # ========== TENTATIVE DE FITTING ==========
  
  fit_result <- tryCatch({
    
    fit <- minpack.lm::nlsLM(
      fit_formula,
      data = grid,
      start = start_params,
      lower = lower_bounds,
      upper = upper_bounds,
      control = list(
        maxiter = 200,
        ftol = 1e-6,
        ptol = 1e-6,
        gtol = 0  # Désactiver le test de gradient pour éviter "singular gradient"
      )
    )
    
    params <- coef(fit)
    
    # Rescaler les paramètres
    params["A"] <- params["A"] * z_scale
    params["b"] <- params["b"] * z_scale
    
    # Calculer R² sur les données originales
    fitted_vals <- fitted(fit) * z_scale
    residuals <- grid$z - fitted_vals
    ss_res <- sum(residuals^2)
    ss_tot <- sum((grid$z - mean(grid$z))^2)
    r_squared <- max(0, 1 - (ss_res / ss_tot))  # Forcer entre 0 et 1
    
    # ========== CALCUL DU VOLUME ==========
    # Option 1: Somme des valeurs fittées (intensité intégrée dans la box)
    # C'est la méthode la plus fiable et comparable à la méthode "sum"
    volume_fitted_sum <- sum(fitted_vals, na.rm = TRUE)
    
    # Option 2: Volume analytique (pour référence, mais moins utile en pratique)
    if (model == "gaussian") {
      # Volume analytique d'une Gaussienne 2D (intégrale sur tout l'espace)
      volume_analytical <- 2 * pi * params["A"] * abs(params["sx"]) * abs(params["sy"])
    } else if (model == "lorentzian") {
      # Volume analytique d'une Lorentzienne 2D
      volume_analytical <- pi^2 * params["A"] * abs(params["gx"]) * abs(params["gy"])
    }
    
    # Utiliser la somme des valeurs fittées comme volume principal
    # (plus cohérent avec la méthode sum et donne des valeurs comparables)
    volume <- volume_fitted_sum
    
    list(
      volume = volume,
      volume_analytical = volume_analytical,
      fit_quality = r_squared,
      params = params,
      fitted_values = fitted_vals,
      residuals = residuals,
      method = model,
      error = NULL
    )
    
  }, error = function(e) {
    
    # En cas d'échec, fallback sur la somme
    volume_sum <- sum(region, na.rm = TRUE)
    
    list(
      volume = volume_sum,
      fit_quality = NA,
      params = NULL,
      fitted_values = NULL,
      residuals = NULL,
      method = "sum_fallback",
      error = e$message
    )
  })
  
  # Ajouter l'info sur le nombre de pics (pour les non-multiplets, n_peaks = 1)
  fit_result$n_peaks <- 1
  fit_result$is_multiplet <- FALSE
  
  return(fit_result)
}


#' Batch peak fitting for all boxes avec gestion robuste des erreurs
#' 
#' @param mat Spectral matrix
#' @param ppm_x F2 chemical shifts
#' @param ppm_y F1 chemical shifts
#' @param boxes Data frame of bounding boxes
#' @param model Peak model type
#' @param progress_callback Optional function for progress updates
#' @param min_points Minimum points required for fitting
calculate_fitted_volumes <- function(mat, ppm_x, ppm_y, boxes, 
                                     model = "gaussian",
                                     progress_callback = NULL,
                                     min_points = 25) {
  
  n_boxes <- nrow(boxes)
  results <- vector("list", n_boxes)
  
  # Compteurs pour diagnostics
  n_success <- 0
  n_fallback <- 0
  n_failed <- 0
  n_multiplets <- 0
  
  for (i in seq_len(n_boxes)) {
    if (!is.null(progress_callback)) {
      progress_callback(i / n_boxes, detail = paste("Fitting box", i, "/", n_boxes))
    }
    
    box <- boxes[i, ]
    fit_result <- fit_2d_peak(mat, ppm_x, ppm_y, box, 
                              model = model, 
                              min_points = min_points)
    
    # Statistiques
    if (fit_result$method == model) {
      n_success <- n_success + 1
    } else if (fit_result$method == "multiplet_fit") {
      n_success <- n_success + 1
      n_multiplets <- n_multiplets + 1
    } else if (fit_result$method %in% c("sum_fallback", "multiplet_sum")) {
      n_fallback <- n_fallback + 1
    } else {
      n_failed <- n_failed + 1
    }
    
    results[[i]] <- data.frame(
      stain_id = box$stain_id,
      volume_fitted = fit_result$volume,
      r_squared = fit_result$fit_quality,
      center_x = if (!is.null(fit_result$params)) fit_result$params["x0"] else NA,
      center_y = if (!is.null(fit_result$params)) fit_result$params["y0"] else NA,
      fit_method = fit_result$method,
      n_peaks = ifelse(is.null(fit_result$n_peaks), 1, fit_result$n_peaks),
      is_multiplet = ifelse(is.null(fit_result$is_multiplet), FALSE, fit_result$is_multiplet),
      fit_error = ifelse(is.null(fit_result$error), NA, fit_result$error),
      stringsAsFactors = FALSE
    )
  }
  
  # Message de diagnostic
  message(sprintf("Fitting complete: %d successful (%d multiplets fitted), %d fallback to sum, %d failed",
                  n_success, n_multiplets, n_fallback, n_failed))
  
  do.call(rbind, results)
}