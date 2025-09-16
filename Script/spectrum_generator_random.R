library(plotly)
library(signal)
library(tibble)
library(dplyr)

#=============================PETITES FONCTIONS UTILES============================================

ppm <- seq(14, 0, length.out = 2048)

pascal_triangle <- function(n) {
  choose(n, 0:n)
}

voigt_profile <- function(x, center, fwhm_g, fwhm_l) {
  sigma <- fwhm_g / (2 * sqrt(2 * log(2)))
  gamma <- fwhm_l / 2
  lorentz <- gamma^2 / ((x - center)^2 + gamma^2)
  gauss <- exp(-((x - center)^2) / (2 * sigma^2))
  profile <- 0.5 * lorentz + 0.5 * gauss
  return(profile / max(profile))
}

simulate_multiplet <- function(center, n_couplings = 1, J = 7, n_protons = 1,
                               fwhm_g = 0.01, fwhm_l = 0.01) {
  intensities <- pascal_triangle(n_couplings)
  intensities <- intensities / sum(intensities) * n_protons
  delta_ppm <- J / 400  # conversion Hz en ppm pour 400 MHz
  shifts <- seq(-n_couplings / 2, n_couplings / 2, by = 1) * delta_ppm + center
  
  signal <- rep(0, length(ppm))
  for (i in seq_along(shifts)) {
    signal <- signal + intensities[i] * voigt_profile(ppm, shifts[i], fwhm_g, fwhm_l)
  }
  return(signal)
}

#===========================GENERATION DE SPECTRE RANDOM============================================

get_metabolite_peaks <- function(metabolite_name) {
  df <- BdDReference_CPMG[[metabolite_name]]
  if (is.null(df) || nrow(df) == 0) return(NULL)  # cas où le métabolite n'existe pas ou est vide
  
  peaks <- lapply(seq_len(nrow(df)), function(i) {
    list(
      ppm = df$ppm[i],
      nH = df$nH[i],
      couplings = as.numeric(df$couplage[i])  # si besoin, convertir en nombre ou laisser en caractère
    )
  })
  return(peaks)
}



generate_random_spectrum_with_peaks <- function(BdD = BdDReference_CPMG,
                                                min_compounds = 5, max_compounds = 10,
                                                noise_sd = 0.004,
                                                seed = 126) {
  set.seed(seed)
  
  types <- names(BdD)
  n_compounds <- sample(min_compounds:max_compounds, 1)
  chosen <- sample(types, n_compounds, replace = FALSE)
  
  spectrum_total <- rep(0, length(ppm))
  all_peaks <- list()
  
  intensity_factors <- runif(n_compounds, 0.01, 1.2)
  if (n_compounds >= 3) intensity_factors[3] <- runif(1, 0.01, 0.015)
  if (n_compounds >= 4) intensity_factors[4] <- runif(1, 0.01, 0.015)
  if (n_compounds >= 5) intensity_factors[5] <- runif(1, 0.01, 0.015)
  if (n_compounds >= 6) intensity_factors[6] <- runif(1, 1.0, 1.2)
  if (n_compounds >= 7) intensity_factors[7] <- runif(1, 1.0, 1.2)
  intensity_factors <- sample(intensity_factors)
  
  # mapping notation -> nombre de couplages
  couplage_to_n <- function(c) {
    switch(c,
           "s" = 0,
           "d" = 1,
           "t" = 2,
           "q" = 3,
           "m" = 2,  # choix arbitraire
           0)
  }
  
  for (i in seq_along(chosen)) {
    type <- chosen[i]
    intensity_factor <- intensity_factors[i]
    groups <- BdD[[type]]
    
    spectrum_compound <- rep(0, length(ppm))
    
    for (j in seq_len(nrow(groups))) {
      grp <- groups[j, ]
      
      # traduction du couplage
      n_couplings <- couplage_to_n(grp[["couplage"]])
      
      fwhm_g <- runif(1, 0.005, 0.06)
      fwhm_l <- runif(1, 0.005, 0.06)
      
      spectrum_compound <- spectrum_compound + simulate_multiplet(
        center = grp[["ppm"]],
        n_couplings = n_couplings,
        J = 8,
        n_protons = grp[["nH"]],
        fwhm_g = fwhm_g,
        fwhm_l = fwhm_l
      )
      
      all_peaks[[length(all_peaks) + 1]] <- data.frame(
        compound_id = i,
        compound_type = type,
        ppm = grp[["ppm"]],
        nH = grp[["nH"]],
        couplage = grp[["couplage"]],
        n_couplings = n_couplings,
        intensity = intensity_factor * grp[["nH"]]
      )
    }
    
    spectrum_total <- spectrum_total + spectrum_compound * intensity_factor
  }
  
  spectrum_total <- spectrum_total + rnorm(length(ppm), mean = 0, sd = noise_sd)
  spectrum_total <- spectrum_total / max(abs(spectrum_total))
  
  return(list(
    spectrum = tibble(ppm = ppm, intensity = spectrum_total),
    true_peaks = bind_rows(all_peaks)
  ))
}




#===========================AFFICHAGE MULTIPLE POUR COMPARAISON========================================

spectres <- lapply(1:4, function(i) {
  generate_random_spectrum_with_peaks(min_compounds = 20, max_compounds = 34, noise_sd = 0.01, seed = 40 + i)
})

plots <- lapply(spectres, function(res) {
  p <- plot_ly(res$spectrum, x = ~ppm, y = ~intensity, type = "scatter", mode = "lines",
               line = list(color = "blue")) %>%
    layout(xaxis = list(autorange = "reversed"), yaxis = list(title = "Intensité"))
  
  for (ppm_peak in res$true_peaks$ppm) {
    p <- p %>%
      add_segments(x = ppm_peak, xend = ppm_peak,
                   y = 0, yend = 1,
                   line = list(dash = "dash", color = "red", width = 0.5),
                   inherit = FALSE,
                   showlegend = FALSE)
  }
  
  return(p)
})

# Affichage
subplot(plots, nrows = 2, margin = 0.05, shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE)

#==================================GENERATION D'UN DATASET===============================================


generate_spectrum_labels_full <- function(n_spectra = 1000,
                                          n_points = 2048,
                                          intensity_threshold = 0.2,
                                          center_margin = 0.2,
                                          seed = 123) {
  set.seed(seed)
  
  spectra_list <- list()
  y_class_list <- list()
  y_reg_list <- list()
  
  for (i in 1:n_spectra) {
    spectrum_seed <- sample.int(1e6, 1)
    res <- generate_random_spectrum_with_peaks(
      min_compounds = 20,
      max_compounds = 34,
      seed = spectrum_seed
    )
    
    spec_interp <- approx(res$spectrum$ppm, res$spectrum$intensity,
                          xout = seq(14, 0, length.out = n_points),
                          rule = 2)$y
    
    ppm_axis <- seq(14, 0, length.out = n_points)
    peaks_df <- res$true_peaks
    
    y_class <- rep(0, n_points)
    y_reg <- matrix(0, nrow = n_points, ncol = 3)
    
    peaks_idx <- sapply(peaks_df$ppm, function(ppm) which.min(abs(ppm_axis - ppm)))
    
    for (j in seq_along(peaks_idx)) {
      idx <- peaks_idx[j]
      
      if (idx >= 1 && idx <= n_points) {
        intensity <- peaks_df$intensity[j]
        ppm_val <- ppm_axis[idx]
        # estimation FWHH arbitraire, à améliorer si nécessaire
        fwhh_est <- 0.01
        
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

#=========================================TEST DE SIMULATION TOCSY================================

library(tibble)
library(dplyr)
library(ggplot2)

generate_dense_tocsy_1D_cut <- function(n_points = 2048,
                                        n_multiplets = 100,
                                        max_multiplet_order = 6,
                                        line_broadening = 2.5,
                                        noise_level = 0.000001,
                                        base_freq = 4.7,
                                        ppm_range = c(0,14), # De base mis à c(0,10)
                                        coupling_range = c(4,14), # De base mis à c(4,10)
                                        voigt_mix = 0.5,
                                        seed = NULL,
                                        plot = TRUE) {
  if (!is.null(seed)) set.seed(seed)
  
  # Grille ppm
  ppm <- seq(ppm_range[1], ppm_range[2], length.out = n_points)
  spectrum_total <- rep(0, n_points)
  all_peaks <- list()
  
  # Fonctions de forme
  gaussian <- function(x, mu, sigma, amp = 1) {
    amp * exp(-((x - mu)^2) / (2 * sigma^2))
  }
  
  lorentzian <- function(x, mu, gamma, amp = 1) {
    amp * (gamma^2 / ((x - mu)^2 + gamma^2))
  }
  
  voigt_approx <- function(x, center, sigma, gamma, amp = 1, mix = 0.5) {
    mix * lorentzian(x, center, gamma, amp) + (1 - mix) * gaussian(x, center, sigma, amp)
  }
  
  for (i in seq_len(n_multiplets)) {
    center_ppm <- runif(1, ppm_range[1] + 0.5, ppm_range[2] - 0.5)
    order <- sample(1:max_multiplet_order, 1)
    J <- runif(1, coupling_range[1], coupling_range[2]) / 400  # Hz -> ppm
    shifts <- seq(-J * (order - 1) / 2, J * (order - 1) / 2, length.out = order)
    rel_intens <- dbinom(0:(order - 1), size = order - 1, prob = 0.5)
    # intensity <- if (runif(1) < 0.9) {
    #   runif(1, 0.00008, 0.00015)
    # } else {
    #   runif(1, 0.4, 1)
    # }
    al <- runif(1)
    intensity <- if (al < 0.8) {
      runif(1, 0.000008, 0.0002)
    } else {
      runif(1, 0.0002, 1)
    }
    sigma <- runif(1, 0.0015, 0.0035)
    gamma <- runif(1, 0.0015, 0.004)
    
    peak_positions <- numeric(order)
    
    for (j in seq_along(shifts)) {
      pos_ppm <- center_ppm + shifts[j]
      peak_positions[j] <- pos_ppm
      spectrum_total <- spectrum_total +
        voigt_approx(ppm, pos_ppm, sigma, gamma, amp = intensity * rel_intens[j], mix = voigt_mix)
    }
    
    all_peaks[[i]] <- tibble(ppm = peak_positions, amplitude = intensity * rel_intens)
  }
  
  # Fond sinusoïdal + bruit
  spectrum_total <- spectrum_total #+ 0.01 * sin(seq(0, 12 * pi, length.out = n_points))
  spectrum_total <- spectrum_total + rnorm(n_points, mean = 0, sd = noise_level)
  
  spectrum_tbl <- tibble(ppm = ppm, intensity = spectrum_total)
  peaks_tbl <- bind_rows(all_peaks)
  
  if (plot) {
    ggplot(spectrum_tbl, aes(x = ppm, y = intensity)) +
      geom_line() +
      geom_vline(data = peaks_tbl, aes(xintercept = ppm), linetype = "dashed", color = "red", alpha = 0.5) +
      scale_x_reverse() +
      theme_minimal() +
      labs(title = "Coupe TOCSY 1D synthétique",
           x = "ppm", y = "Signal")
  }
  
  return(list(
    spectrum = spectrum_tbl,
    true_peaks = peaks_tbl
  ))
}

sim <- generate_dense_tocsy_1D_cut(seed = 42)

# Accéder au spectre et aux pics
sim$spectrum         # tibble des points
sim$true_peaks       # tibble des positions de pics

# Optionnel : traçage manuel (si plot = FALSE)
ggplot(sim$spectrum, aes(x = ppm, y = intensity)) +
  geom_line() +
  #geom_vline(data = sim$true_peaks, aes(xintercept = ppm),
  #linetype = "dashed", color = "red", alpha = 0.5) +
  scale_x_reverse() +
  theme_minimal()

#====================================GENERATION D'UN DATASET MIXTE==================================================

generate_spectrum_labels_full_mixed <- function(n_spectra = 1000,
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

data <- generate_spectrum_labels_full_mixed(n_spectra = 1000, mix_ratio = 0.5)
