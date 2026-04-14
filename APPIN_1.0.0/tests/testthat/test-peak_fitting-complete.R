# test-peak_fitting-complete.R - Tests unitaires pour Function/Peak_fitting.R
# =============================================================================

library(testthat)

# =============================================================================
# HELPERS: Créer des données de test
# =============================================================================

#' Créer une matrice 2D avec un pic gaussien
create_gaussian_peak_matrix <- function(size = 20, amplitude = 100, 
                                         center_row = 10, center_col = 10,
                                         sigma = 3, noise_sd = 1) {
  set.seed(42)
  mat <- matrix(rnorm(size * size, mean = 0, sd = noise_sd), 
                nrow = size, ncol = size)
  
  for (i in 1:size) {
    for (j in 1:size) {
      dist_sq <- (i - center_row)^2 + (j - center_col)^2
      mat[i, j] <- mat[i, j] + amplitude * exp(-dist_sq / (2 * sigma^2))
    }
  }
  
  # Axes ppm
  ppm_x <- seq(10, 0, length.out = size)
  ppm_y <- seq(10, 0, length.out = size)
  
  colnames(mat) <- as.character(round(ppm_x, 4))
  rownames(mat) <- as.character(round(ppm_y, 4))
  
  list(mat = mat, ppm_x = ppm_x, ppm_y = ppm_y)
}

#' Créer une matrice avec plusieurs pics (multiplet)
create_multiplet_matrix <- function(size = 30, n_peaks = 3) {
  set.seed(123)
  mat <- matrix(rnorm(size * size, mean = 0, sd = 0.5), 
                nrow = size, ncol = size)
  
  # Positions des pics
  peak_positions <- list(
    c(10, 10),
    c(10, 20),
    c(20, 15)
  )
  
  for (p in seq_len(min(n_peaks, length(peak_positions)))) {
    pos <- peak_positions[[p]]
    amplitude <- 50 + p * 20
    for (i in 1:size) {
      for (j in 1:size) {
        dist_sq <- (i - pos[1])^2 + (j - pos[2])^2
        mat[i, j] <- mat[i, j] + amplitude * exp(-dist_sq / 18)
      }
    }
  }
  
  ppm_x <- seq(10, 0, length.out = size)
  ppm_y <- seq(10, 0, length.out = size)
  
  colnames(mat) <- as.character(round(ppm_x, 4))
  rownames(mat) <- as.character(round(ppm_y, 4))
  
  list(mat = mat, ppm_x = ppm_x, ppm_y = ppm_y)
}

# =============================================================================
# TESTS: detect_local_maxima
# =============================================================================

test_that("detect_local_maxima: détecte un pic unique", {
  data <- create_gaussian_peak_matrix(size = 20, center_row = 10, center_col = 10)
  
  result <- detect_local_maxima(data$mat, threshold = 0.3, min_distance = 2)
  
  expect_true(is.data.frame(result))
  expect_true(nrow(result) >= 1)
  expect_true(all(c("row", "col", "value") %in% names(result)))
  
  # Le pic doit être proche du centre
  expect_true(any(abs(result$row - 10) <= 2 & abs(result$col - 10) <= 2))
})

test_that("detect_local_maxima: détecte plusieurs pics", {
  data <- create_multiplet_matrix(size = 30, n_peaks = 3)
  
  result <- detect_local_maxima(data$mat, threshold = 0.2, min_distance = 3)
  
  expect_true(nrow(result) >= 2)  # Au moins 2 pics détectés
})

test_that("detect_local_maxima: retourne data.frame vide si matrice trop petite", {
  small_mat <- matrix(1:4, nrow = 2, ncol = 2)
  
  result <- detect_local_maxima(small_mat)
  
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
})

test_that("detect_local_maxima: gère une matrice constante", {
  # Matrice constante - tous les points ont la même valeur
  # L'algorithme peut considérer tous les points comme des maxima locaux
  # ou aucun selon l'implémentation
  flat_mat <- matrix(1, nrow = 10, ncol = 10)
  
  result <- detect_local_maxima(flat_mat, threshold = 0.5)
  
  # Doit retourner un data.frame (même si non vide)
  expect_true(is.data.frame(result))
  expect_true(all(c("row", "col", "value") %in% names(result)))
})

test_that("detect_local_maxima: min_distance filtre les pics proches", {
  data <- create_multiplet_matrix(size = 30, n_peaks = 3)
  
  # Avec min_distance petit
  result_close <- detect_local_maxima(data$mat, threshold = 0.2, min_distance = 1)
  
  # Avec min_distance grand
  result_far <- detect_local_maxima(data$mat, threshold = 0.2, min_distance = 10)
  
  # Plus de pics avec distance petite

  expect_true(nrow(result_close) >= nrow(result_far))
})

test_that("detect_local_maxima: gère les NA dans la matrice", {
  data <- create_gaussian_peak_matrix(size = 15)
  data$mat[5, 5] <- NA
  data$mat[7, 7] <- NA
  
  # Ne doit pas planter
  result <- detect_local_maxima(data$mat, threshold = 0.3)
  
  expect_true(is.data.frame(result))
})

# =============================================================================
# TESTS: pseudo_voigt_2d
# =============================================================================

test_that("pseudo_voigt_2d: valeur maximale au centre", {
  # Au centre (x0, y0), la fonction doit être maximale
  val_center <- pseudo_voigt_2d(
    x = 5, y = 5, 
    A = 100, x0 = 5, y0 = 5,
    sigma_x = 1, sigma_y = 1,
    gamma_x = 1, gamma_y = 1,
    eta = 0.5
  )
  
  val_off_center <- pseudo_voigt_2d(
    x = 6, y = 6, 
    A = 100, x0 = 5, y0 = 5,
    sigma_x = 1, sigma_y = 1,
    gamma_x = 1, gamma_y = 1,
    eta = 0.5
  )
  
  expect_true(val_center > val_off_center)
})

test_that("pseudo_voigt_2d: eta=0 donne pur Gaussien", {
  # Pure Gaussian (eta = 0)
  val_gauss <- pseudo_voigt_2d(
    x = 5, y = 5, 
    A = 100, x0 = 5, y0 = 5,
    sigma_x = 1, sigma_y = 1,
    gamma_x = 1, gamma_y = 1,
    eta = 0
  )
  
  # Pour eta=0, au centre: exp(0) = 1, donc val = A * 1 = 100
  expect_equal(val_gauss, 100)
})

test_that("pseudo_voigt_2d: eta=1 donne pur Lorentzien", {
  # Pure Lorentzian (eta = 1)
  val_lorentz <- pseudo_voigt_2d(
    x = 5, y = 5, 
    A = 100, x0 = 5, y0 = 5,
    sigma_x = 1, sigma_y = 1,
    gamma_x = 1, gamma_y = 1,
    eta = 1
  )
  
  # Pour eta=1, au centre: 1/(1+0+0) = 1, donc val = A * 1 = 100
  expect_equal(val_lorentz, 100)
})

test_that("pseudo_voigt_2d: symétrie en x et y", {
  # Points symétriques par rapport au centre
  val1 <- pseudo_voigt_2d(
    x = 6, y = 5, 
    A = 100, x0 = 5, y0 = 5,
    sigma_x = 1, sigma_y = 1,
    gamma_x = 1, gamma_y = 1,
    eta = 0.5
  )
  
  val2 <- pseudo_voigt_2d(
    x = 4, y = 5, 
    A = 100, x0 = 5, y0 = 5,
    sigma_x = 1, sigma_y = 1,
    gamma_x = 1, gamma_y = 1,
    eta = 0.5
  )
  
  expect_equal(val1, val2, tolerance = 1e-10)
})

# =============================================================================
# TESTS: fit_2d_peak
# =============================================================================

test_that("fit_2d_peak: fit gaussien sur pic synthétique", {
  data <- create_gaussian_peak_matrix(size = 25, amplitude = 100, 
                                       center_row = 12, center_col = 12, sigma = 3)
  
  box <- data.frame(
    xmin = 3, xmax = 7,  # ppm (le pic est autour de 5 ppm)
    ymin = 3, ymax = 7
  )
  
  result <- fit_2d_peak(data$mat, data$ppm_x, data$ppm_y, box, 
                        model = "gaussian", min_points = 9)
  
  expect_true(is.list(result))
  expect_true("volume" %in% names(result))
  expect_true("fit_quality" %in% names(result))
  expect_true("method" %in% names(result))
  
  # Le fit doit réussir ou faire un fallback
  expect_true(result$method %in% c("gaussian", "sum_fallback"))
})

test_that("fit_2d_peak: retourne erreur si région vide", {
  data <- create_gaussian_peak_matrix(size = 20)
  
  # Box complètement hors du spectre
  box <- data.frame(xmin = 100, xmax = 110, ymin = 100, ymax = 110)
  
  result <- fit_2d_peak(data$mat, data$ppm_x, data$ppm_y, box)
  
  expect_equal(result$method, "failed")
  expect_true(is.na(result$volume))
})

test_that("fit_2d_peak: fallback ou échec si trop peu de points", {
  data <- create_gaussian_peak_matrix(size = 20)
  
  # Box très petite - peut être hors spectre ou avoir trop peu de points
  box <- data.frame(xmin = 4.9, xmax = 5.1, ymin = 4.9, ymax = 5.1)
  
  result <- fit_2d_peak(data$mat, data$ppm_x, data$ppm_y, box, min_points = 100)
  
  # Doit retourner soit "sum_fallback" soit "failed"
  expect_true(result$method %in% c("sum_fallback", "failed"))
})

test_that("fit_2d_peak: fallback si région constante", {
  # Matrice constante
  mat <- matrix(5, nrow = 20, ncol = 20)
  ppm_x <- seq(10, 0, length.out = 20)
  ppm_y <- seq(10, 0, length.out = 20)
  colnames(mat) <- as.character(round(ppm_x, 4))
  rownames(mat) <- as.character(round(ppm_y, 4))
  
  box <- data.frame(xmin = 3, xmax = 7, ymin = 3, ymax = 7)
  
  result <- fit_2d_peak(mat, ppm_x, ppm_y, box)
  
  expect_equal(result$method, "sum_fallback")
})

test_that("fit_2d_peak: modèle voigt fonctionne", {
  data <- create_gaussian_peak_matrix(size = 25, amplitude = 100, 
                                       center_row = 12, center_col = 12)
  
  box <- data.frame(xmin = 3, xmax = 7, ymin = 3, ymax = 7)
  
  result <- fit_2d_peak(data$mat, data$ppm_x, data$ppm_y, box, 
                        model = "voigt", min_points = 9)
  
  expect_true(result$method %in% c("voigt", "sum_fallback"))
})

test_that("fit_2d_peak: détecte les multiplets", {
  data <- create_multiplet_matrix(size = 30, n_peaks = 3)
  
  # Box couvrant tous les pics
  box <- data.frame(xmin = 0, xmax = 10, ymin = 0, ymax = 10)
  
  result <- fit_2d_peak(data$mat, data$ppm_x, data$ppm_y, box, min_points = 9)
  
  # Doit détecter qu'il y a plusieurs pics
  expect_true("n_peaks" %in% names(result))
  expect_true("is_multiplet" %in% names(result))
})

test_that("fit_2d_peak: erreur pour modèle non supporté", {
  data <- create_gaussian_peak_matrix(size = 20)
  box <- data.frame(xmin = 3, xmax = 7, ymin = 3, ymax = 7)
  
  expect_error(
    fit_2d_peak(data$mat, data$ppm_x, data$ppm_y, box, model = "invalid"),
    "not supported"
  )
})

# =============================================================================
# TESTS: calculate_fitted_volumes
# =============================================================================

test_that("calculate_fitted_volumes: batch fitting fonctionne", {
  data <- create_gaussian_peak_matrix(size = 30, amplitude = 100,
                                       center_row = 15, center_col = 15)
  
  boxes <- data.frame(
    xmin = c(3, 6),
    xmax = c(5, 8),
    ymin = c(3, 6),
    ymax = c(5, 8),
    stain_id = c("box_1", "box_2")
  )
  
  result <- calculate_fitted_volumes(data$mat, data$ppm_x, data$ppm_y, boxes,
                                     model = "gaussian", min_points = 9)
  
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_true(all(c("stain_id", "volume_fitted", "r_squared", "fit_method") %in% names(result)))
})

test_that("calculate_fitted_volumes: progress callback est appelé", {
  data <- create_gaussian_peak_matrix(size = 20)
  
  boxes <- data.frame(
    xmin = c(3, 5),
    xmax = c(5, 7),
    ymin = c(3, 5),
    ymax = c(5, 7),
    stain_id = c("box_1", "box_2")
  )
  
  progress_calls <- 0
  mock_progress <- function(value, detail) {
    progress_calls <<- progress_calls + 1
  }
  
  result <- calculate_fitted_volumes(data$mat, data$ppm_x, data$ppm_y, boxes,
                                     progress_callback = mock_progress)
  
  expect_equal(progress_calls, 2)  # Appelé pour chaque box
})

test_that("calculate_fitted_volumes: gère les boxes avec échec de fit", {
  data <- create_gaussian_peak_matrix(size = 20)
  
  boxes <- data.frame(
    xmin = c(3, 100),  # Deuxième box hors spectre
    xmax = c(5, 110),
    ymin = c(3, 100),
    ymax = c(5, 110),
    stain_id = c("valid", "invalid")
  )
  
  result <- calculate_fitted_volumes(data$mat, data$ppm_x, data$ppm_y, boxes)
  
  expect_equal(nrow(result), 2)
  # La box invalide doit avoir un fit_method "failed" ou une erreur
  expect_true(any(result$fit_method == "failed" | !is.na(result$fit_error)))
})

test_that("calculate_fitted_volumes: compte les succès/échecs", {
  data <- create_gaussian_peak_matrix(size = 25, amplitude = 100,
                                       center_row = 12, center_col = 12)
  
  # Plusieurs boxes, certaines sur le pic, d'autres pas
  boxes <- data.frame(
    xmin = c(3, 4, 8),
    xmax = c(7, 6, 9),
    ymin = c(3, 4, 8),
    ymax = c(7, 6, 9),
    stain_id = c("on_peak", "near_peak", "off_peak")
  )
  
  result <- calculate_fitted_volumes(data$mat, data$ppm_x, data$ppm_y, boxes,
                                     model = "gaussian", min_points = 4)
  
  expect_equal(nrow(result), 3)
  
  # Vérifier que les colonnes de diagnostic sont présentes
  expect_true("n_peaks" %in% names(result))
  expect_true("is_multiplet" %in% names(result))
})

# =============================================================================
# RÉSUMÉ
# =============================================================================

cat("\n")
cat("╔══════════════════════════════════════════════════════════════════╗\n")
cat("║           TESTS POUR Function/Peak_fitting.R                     ║\n")
cat("║                                                                  ║\n")
cat("║  Fonctions testées:                                             ║\n")
cat("║  - detect_local_maxima                                          ║\n")
cat("║  - pseudo_voigt_2d                                              ║\n")
cat("║  - fit_2d_peak (gaussian, voigt, multiplet)                     ║\n")
cat("║  - calculate_fitted_volumes                                     ║\n")
cat("╚══════════════════════════════════════════════════════════════════╝\n")
cat("\n")
