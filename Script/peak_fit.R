library(dplyr)
library(minpack.lm)

# ------------------------------
# 1. Fonctions de base
lorentz <- function(x, gamma) {
  (gamma / pi) / (x^2 + gamma^2)
}

voigt <- function(x, sigma, gamma) {
  eta <- 0.5346
  eta * lorentz(x, gamma) + (1 - eta) * dnorm(x, 0, sigma)
}

# ------------------------------
# 2. Génération de spectres 2D typiques 
generate_spectrum_gaussian <- function(inten, sigmax, sigmay, centerx, centery, xdim, ydim) {
  spectrum <- matrix(0, nrow = ydim, ncol = xdim)
  for (i in seq_along(centerx)) {
    width <- min(15, floor(sigmax[i]*6))
    height <- min(15, floor(sigmay[i]*6))
    x_idx <- max(1, round(centerx[i]-width)):min(xdim, round(centerx[i]+width))
    y_idx <- max(1, round(centery[i]-height)):min(ydim, round(centery[i]+height))
    x_grid <- x_idx - centerx[i]
    y_grid <- y_idx - centery[i]
    kernel <- outer(y_grid, x_grid, function(y,x) exp(- (y^2)/(2*sigmay[i]^2) - (x^2)/(2*sigmax[i]^2)))
    spectrum[y_idx, x_idx] <- spectrum[y_idx, x_idx] + kernel * inten[i]
  }
  spectrum
}

generate_spectrum_voigt <- function(inten, sigmax, sigmay, gammax, gammay, centerx, centery, xdim, ydim) {
  spectrum <- matrix(0, nrow = ydim, ncol = xdim)
  for (i in seq_along(centerx)) {
    width <- min(15, floor(sigmax[i]*6))
    height <- min(15, floor(sigmay[i]*6))
    x_idx <- max(1, round(centerx[i]-width)):min(xdim, round(centerx[i]+width))
    y_idx <- max(1, round(centery[i]-height)):min(ydim, round(centery[i]+height))
    x_grid <- x_idx - centerx[i]
    y_grid <- y_idx - centery[i]
    kernel <- outer(y_grid, x_grid, Vectorize(function(y,x) voigt(x, sigmax[i], gammax[i]) * voigt(y, sigmay[i], gammay[i])))
    spectrum[y_idx, x_idx] <- spectrum[y_idx, x_idx] + kernel * inten[i]
  }
  spectrum
}

generate_spectrum_voigt_lorentz <- function(inten, sigmax, gammax, gammay, centerx, centery, xdim, ydim) {
  spectrum <- matrix(0, nrow = ydim, ncol = xdim)
  for (i in seq_along(centerx)) {
    width <- min(15, floor(sigmax[i]*6))
    height <- min(15, floor(gammay[i]*6))
    x_idx <- max(1, round(centerx[i]-width)):min(xdim, round(centerx[i]+width))
    y_idx <- max(1, round(centery[i]-height)):min(ydim, round(centery[i]+height))
    x_grid <- x_idx - centerx[i]
    y_grid <- y_idx - centery[i]
    kernel <- outer(y_grid, x_grid, Vectorize(function(y,x) lorentz(y, gammay[i]) * voigt(x, sigmax[i], gammax[i])))
    spectrum[y_idx, x_idx] <- spectrum[y_idx, x_idx] + kernel * inten[i]
  }
  spectrum
}

# ------------------------------
# 3. Clustering des pics (ATTENTION à faire sur des pics pas encore clusterisés, sinon logique ça bug)
cluster_peaks <- function(peaks_df, dist_cutoff = 5) {
  peaks <- peaks_df
  peaks$cluster <- NA
  cluster_id <- 1
  for (i in seq_len(nrow(peaks))) {
    if (!is.na(peaks$cluster[i])) next
    dx <- abs(peaks$F2 - peaks$F2[i])
    dy <- abs(peaks$F1 - peaks$F1[i])
    in_cluster <- which((dx <= dist_cutoff) & (dy <= dist_cutoff))
    peaks$cluster[in_cluster] <- cluster_id
    cluster_id <- cluster_id + 1
  }
  peaks
}

library(dbscan)

cluster_peaks <- function(peaks_df, eps = 0.02, minPts = 1) {
  if (!all(c("F1", "F2") %in% colnames(peaks_df))) {
    stop("peaks_df doit contenir au moins les colonnes F1 et F2")
  }
  
  coords <- as.matrix(peaks_df[, c("F1", "F2")])
  
  # DBSCAN clustering
  db <- dbscan(coords, eps = eps, minPts = minPts)
  
  # Ajouter les labels de cluster au data.frame
  peaks_df$cluster <- db$cluster
  peaks_df
}

# ------------------------------
# 4. Extraction de patch 
extract_patch_cluster <- function(spectrum, cluster_peaks, wx = 5, wy = 5) {
  x_min <- max(1, floor(min(cluster_peaks$F2) - wx))
  x_max <- min(ncol(spectrum), ceiling(max(cluster_peaks$F2) + wx))
  y_min <- max(1, floor(min(cluster_peaks$F1) - wy))
  y_max <- min(nrow(spectrum), ceiling(max(cluster_peaks$F1) + wy))
  
  if (x_min > x_max || y_min > y_max) {
    warning("Cluster ignoré car limites invalides")
    return(NULL)
  }
  
  list(
    patch = spectrum[y_min:y_max, x_min:x_max, drop = FALSE],
    x_idx = x_min:x_max,
    y_idx = y_min:y_max
  )
}

# ------------------------------
# 5. Fit cluster multi-pics
fit_cluster <- function(patch_info, cluster_peaks, peak_type = "voigt") {
  if (is.null(patch_info)) return(NULL)
  patch <- patch_info$patch
  x_idx <- patch_info$x_idx
  y_idx <- patch_info$y_idx
  x_grid <- rep(1:ncol(patch), each = nrow(patch))
  y_grid <- rep(1:nrow(patch), times = ncol(patch))
  z <- as.vector(patch)
  
  n <- nrow(cluster_peaks)
  
  start_list <- list()
  for (i in seq_len(n)) {
    start_list[[paste0("a", i)]] <- cluster_peaks$Intensity[i]
    start_list[[paste0("x0_", i)]] <- cluster_peaks$F2[i] - min(x_idx) + 1
    start_list[[paste0("y0_", i)]] <- cluster_peaks$F1[i] - min(y_idx) + 1
  }
  start_list[["sigmax"]] <- 2
  start_list[["sigmay"]] <- 2
  start_list[["gammax"]] <- 1
  start_list[["gammay"]] <- 1
  
  model_fun <- function(par) {
    val <- rep(0, length(z))
    for (i in seq_len(n)) {
      xi <- x_grid - par[[paste0("x0_", i)]]
      yi <- y_grid - par[[paste0("y0_", i)]]
      if (peak_type == "gaussian") {
        val <- val + par[[paste0("a", i)]] * exp(- (xi^2)/(2*par[["sigmax"]]^2) - (yi^2)/(2*par[["sigmay"]]^2))
      } else if (peak_type == "voigt") {
        val <- val + par[[paste0("a", i)]] * voigt(xi, par[["sigmax"]], par[["gammax"]]) * voigt(yi, par[["sigmay"]], par[["gammay"]])
      } else if (peak_type == "voigt-lorentz") {
        val <- val + par[[paste0("a", i)]] * voigt(xi, par[["sigmax"]], par[["gammax"]]) * lorentz(yi, par[["gammay"]])
      }
    }
    val
  }
  
  fit <- tryCatch(
    nlsLM(z ~ model_fun(par), start = start_list, control = nls.lm.control(maxiter = 200)),
    error = function(e) NULL
  )
  
  if (!is.null(fit)) coef(fit) else start_list
}

# ------------------------------
# 6. Pipeline complet
peak_fitting_full <- function(spectrum, peaks_df, dist_cutoff = 0.02, peak_type = "voigt", wx = 0.02, wy = 0.02) {
  peaks_clustered <- cluster_peaks(peaks_df, 0.02,1)
  cluster_ids <- unique(peaks_clustered$cluster)
  
  recon_spectrum <- matrix(0, nrow = nrow(spectrum), ncol = ncol(spectrum))
  all_results <- list()
  
  for (cid in cluster_ids) {
    cluster_peaks <- peaks_clustered %>% filter(cluster == cid)
    patch_info <- extract_patch_cluster(spectrum, cluster_peaks, wx, wy)
    fit_params <- fit_cluster(patch_info, cluster_peaks, peak_type)
    all_results[[cid]] <- fit_params
    
    if (!is.null(fit_params) && !is.null(patch_info)) {
      for (i in seq_len(nrow(cluster_peaks))) {
        if (peak_type == "gaussian") {
          recon_spectrum <- recon_spectrum +
            generate_spectrum_gaussian(
              inten = fit_params[[paste0("a", i)]],
              sigmax = fit_params[["sigmax"]], sigmay = fit_params[["sigmay"]],
              centerx = fit_params[[paste0("x0_", i)]] + min(patch_info$x_idx) - 1,
              centery = fit_params[[paste0("y0_", i)]] + min(patch_info$y_idx) - 1,
              xdim = ncol(spectrum), ydim = nrow(spectrum)
            )
        } else if (peak_type == "voigt") {
          recon_spectrum <- recon_spectrum +
            generate_spectrum_voigt(
              inten = fit_params[[paste0("a", i)]],
              sigmax = fit_params[["sigmax"]], sigmay = fit_params[["sigmay"]],
              gammax = fit_params[["gammax"]], gammay = fit_params[["gammay"]],
              centerx = fit_params[[paste0("x0_", i)]] + min(patch_info$x_idx) - 1,
              centery = fit_params[[paste0("y0_", i)]] + min(patch_info$y_idx) - 1,
              xdim = ncol(spectrum), ydim = nrow(spectrum)
            )
        } else if (peak_type == "voigt-lorentz") {
          recon_spectrum <- recon_spectrum +
            generate_spectrum_voigt_lorentz(
              inten = fit_params[[paste0("a", i)]],
              sigmax = fit_params[["sigmax"]],
              gammax = fit_params[["gammax"]],
              gammay = fit_params[["gammay"]],
              centerx = fit_params[[paste0("x0_", i)]] + min(patch_info$x_idx) - 1,
              centery = fit_params[[paste0("y0_", i)]] + min(patch_info$y_idx) - 1,
              xdim = ncol(spectrum), ydim = nrow(spectrum)
            )
        }
      }
    }
  }
  
  list(fit_params = all_results, reconstructed = recon_spectrum)
}

# ------------------------------
# 7. Exemple d'utilisation
res <- peak_fitting_full(
  spectrum = rr_norm,
  peaks_df = peaks_clean_filtered,
  dist_cutoff = 0.02,
  peak_type = "gaussian",
  wx = 0.02, wy = 0.02
)

library(ggplot2)
library(reshape2)
library(dplyr)

# --- 1. Récupérer la matrice
spectrum <- res$reconstructed
nF1 <- nrow(spectrum)
nF2 <- ncol(spectrum)

# --- 2. Convertir en data.frame long
recon_df <- melt(
  spectrum,
  varnames = c("y","x"),
  value.name = "intensity"
) %>%
  mutate(
    ppmF2 = seq(10, 0, length.out = nF2)[x],
    ppmF1 = seq(10, 0, length.out = nF1)[y]
  ) %>%
  filter(intensity > 1e-6)  # filtrer les valeurs nulles ou très faibles


# --- 3. Plot avec de petits points
ggplot(recon_df, aes(x = ppmF2, y = ppmF1, color = intensity)) +
  geom_point(size = 0.3) +   # très petits points
  scale_color_gradient(low = "black", high = "red") +  # simple gradient
  scale_x_reverse() +
  scale_y_reverse() +
  coord_equal() +
  labs(
    x = "F2 (ppm)",
    y = "F1 (ppm)",
    color = "Intensité",
    title = "Spectre RMN reconstruit (points)"
  ) +
  theme_minimal()


#=========================TEST DU PIPELINE==================================================

# Chargement des librairies et fonctions définies précédemment
library(dplyr)
library(minpack.lm)

# 1. Génération d'un petit spectre synthétique
xdim <- 30; ydim <- 30
true_inten  <- c(20, 15)
true_sigmax <- c(2, 3)
true_sigmay <- c(2, 1.5)
true_x0     <- c(10, 22)
true_y0     <- c(12, 18)

spectrum_small <- generate_spectrum_gaussian(
  inten   = true_inten,
  sigmax  = true_sigmax,
  sigmay  = true_sigmay,
  centerx = true_x0,
  centery = true_y0,
  xdim    = xdim,
  ydim    = ydim
)

# Data frame des pics initiaux
peaks_df <- data.frame(
  F2        = true_x0,
  F1        = true_y0,
  Intensity = true_inten
)

# 2. Application du pipeline sur ce petit spectre
res_small <- peak_fitting_full(
  spectrum    = spectrum_small,
  peaks_df    = peaks_df,
  dist_cutoff = 5,
  peak_type   = "gaussian",
  wx = 4, wy = 4
)

# 3. Affichage des paramètres ajustés
cat("Paramètres ajustés pour chaque cluster :\n")
print(res_small$fit_params)

# 4. Comparaison visuelle
par(mfrow = c(1, 2), mar = c(2,2,2,2))
image(t(apply(spectrum_small, 2, rev)),
      main = "Spectre original", axes = FALSE, col = heat.colors(20))
image(t(apply(res_small$reconstructed, 2, rev)),
      main = "Spectre reconstruit", axes = FALSE, col = heat.colors(20))

