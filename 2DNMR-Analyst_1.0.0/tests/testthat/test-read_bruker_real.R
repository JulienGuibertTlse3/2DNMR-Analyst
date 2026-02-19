# test-read_bruker_real.R - Tests avec fichiers Bruker réels
# 2DNMR-Analyst v3.0
# 
# PRÉREQUIS: Placer un spectre UFCOSY dans tests/fixtures/UFCOSY_sample/pdata/1/
#
library(testthat)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Chemin vers les fixtures (relatif à tests/testthat/ où testthat exécute)
get_fixtures_path <- function() {
  # Essayer plusieurs chemins possibles
  paths <- c(
    file.path("..", "fixtures", "UFCOSY_sample", "pdata", "1"),
    file.path("tests", "fixtures", "UFCOSY_sample", "pdata", "1"),
    file.path("fixtures", "UFCOSY_sample", "pdata", "1")
  )
  
  for (p in paths) {
    if (dir.exists(p) && file.exists(file.path(p, "2rr"))) {
      return(p)
    }
  }
  
  if (exists("test_path", mode = "function")) {
    tp <- test_path("..", "fixtures", "UFCOSY_sample", "pdata", "1")
    if (dir.exists(tp)) return(tp)
  }
  
  return(NULL)
}

# Helper pour vérifier si les fixtures existent
fixtures_available <- function() {
  path <- get_fixtures_path()
  !is.null(path) && dir.exists(path) && file.exists(file.path(path, "2rr"))
}

# =============================================================================
# TEST: Lecture fichier UFCOSY réel
# =============================================================================
test_that("read_bruker lit un vrai fichier UFCOSY", {
  skip_if_not(fixtures_available(), "Fixtures UFCOSY non disponibles")
  
  ufcosy_path <- get_fixtures_path()
  result <- read_bruker(dir = ufcosy_path, dim = "2D")
  
  # Structure de base
  expect_type(result, "list")
  expect_true("spectrumData" %in% names(result))
  
  # Données spectrales
  expect_true(is.matrix(result$spectrumData))
  expect_true(nrow(result$spectrumData) > 0)
  expect_true(ncol(result$spectrumData) > 0)
  
  # Pas de valeurs NA
  expect_false(any(is.na(result$spectrumData)))
})

test_that("read_bruker retourne des axes ppm valides", {
  skip_if_not(fixtures_available(), "Fixtures UFCOSY non disponibles")
  
  ufcosy_path <- get_fixtures_path()
  result <- read_bruker(dir = ufcosy_path, dim = "2D")
  
  # Axes ppm dans rownames/colnames
  ppm_f1 <- as.numeric(rownames(result$spectrumData))
  ppm_f2 <- as.numeric(colnames(result$spectrumData))
  
  # Pas de NA dans les axes
  expect_false(any(is.na(ppm_f1)))
  expect_false(any(is.na(ppm_f2)))
  
  # Axes monotones (croissants ou décroissants)
  expect_true(all(diff(ppm_f1) <= 0) || all(diff(ppm_f1) >= 0))
  expect_true(all(diff(ppm_f2) <= 0) || all(diff(ppm_f2) >= 0))
  
  # Valeurs finies (pas d'Inf)
  expect_true(all(is.finite(ppm_f1)))
  expect_true(all(is.finite(ppm_f2)))
})

test_that("read_bruker retourne des intensités numériques", {
  skip_if_not(fixtures_available(), "Fixtures UFCOSY non disponibles")
  
  ufcosy_path <- get_fixtures_path()
  result <- read_bruker(dir = ufcosy_path, dim = "2D")
  
  # Type numérique
  expect_true(is.numeric(result$spectrumData))
  
  # Pas d'Inf
  expect_false(any(is.infinite(result$spectrumData)))
  
  # Valeurs non nulles (spectre non vide)
  expect_true(max(abs(result$spectrumData)) > 0)
})

# =============================================================================
# TEST: Paramètres spectraux
# =============================================================================
test_that("read_bruker extrait les paramètres spectraux", {
  skip_if_not(fixtures_available(), "Fixtures UFCOSY non disponibles")
  
  ufcosy_path <- get_fixtures_path()
  result <- read_bruker(dir = ufcosy_path, dim = "2D")
  
  # Vérifier que des paramètres sont retournés
  expect_true(length(result) > 1)
})

# =============================================================================
# TEST: Dimensions spectre UFCOSY
# =============================================================================
test_that("Spectre UFCOSY a des dimensions typiques", {
  skip_if_not(fixtures_available(), "Fixtures UFCOSY non disponibles")
  
  ufcosy_path <- get_fixtures_path()
  result <- read_bruker(dir = ufcosy_path, dim = "2D")
  
  # Dimensions minimales attendues (au moins 64x64)
  expect_true(nrow(result$spectrumData) >= 64)
  expect_true(ncol(result$spectrumData) >= 64)
  
  # Dimensions maximales raisonnables (< 16k x 16k)
  expect_true(nrow(result$spectrumData) <= 16384)
  expect_true(ncol(result$spectrumData) <= 16384)
})

# =============================================================================
# TEST: Intégration avec peak_pick_2d_nt2
# =============================================================================
test_that("peak_pick_2d_nt2 fonctionne sur spectre UFCOSY réel", {
  skip_if_not(fixtures_available(), "Fixtures UFCOSY non disponibles")
  skip_if_not(exists("peak_pick_2d_nt2"), "peak_pick_2d_nt2 non chargé")
  
  ufcosy_path <- get_fixtures_path()
  result <- read_bruker(dir = ufcosy_path, dim = "2D")
  mat <- result$spectrumData
  
  # Calculer un seuil automatique
  threshold <- quantile(abs(mat), 0.99)
  
  # Exécuter peak picking
  peaks_result <- peak_pick_2d_nt2(
    mat, 
    threshold_value = threshold,
    spectrum_type = "UFCOSY",
    verbose = FALSE
  )
  
  # Structure correcte
  expect_type(peaks_result, "list")
  expect_true("bounding_boxes" %in% names(peaks_result) || 
                "centroids" %in% names(peaks_result))
})

# =============================================================================
# TEST: Intégration avec find_nmr_peak_centroids_optimized
# =============================================================================
test_that("find_nmr_peak_centroids_optimized fonctionne sur spectre UFCOSY réel", {
  skip_if_not(fixtures_available(), "Fixtures UFCOSY non disponibles")
  skip_if_not(exists("find_nmr_peak_centroids_optimized"), "Fonction non chargée")
  
  ufcosy_path <- get_fixtures_path()
  result <- read_bruker(dir = ufcosy_path, dim = "2D")
  mat <- result$spectrumData
  
  # Calculer un seuil
  threshold <- quantile(abs(mat), 0.99)
  
  # Exécuter la détection (sans paramètre eps qui n'existe pas dans cette fonction)
  centroids_result <- find_nmr_peak_centroids_optimized(
    mat,
    spectrum_type = "UFCOSY",
    contour_start = threshold
  )
  
  # Structure correcte
  expect_type(centroids_result, "list")
})

# =============================================================================
# TEST: Seuil automatique sur spectre réel
# =============================================================================
test_that("seuil_bruit_multiplicatif fonctionne sur spectre réel", {
  skip_if_not(fixtures_available(), "Fixtures UFCOSY non disponibles")
  skip_if_not(exists("seuil_bruit_multiplicatif"), "Fonction non chargée")
  
  ufcosy_path <- get_fixtures_path()
  result <- read_bruker(dir = ufcosy_path, dim = "2D")
  mat <- result$spectrumData
  
  # Calculer le seuil
  seuil <- seuil_bruit_multiplicatif(mat, 3)
  
  # Le seuil doit être positif et raisonnable
  expect_true(seuil > 0)
  expect_true(seuil < max(abs(mat)))
})

test_that("seuil_max_pourcentage fonctionne sur spectre réel", {
  skip_if_not(fixtures_available(), "Fixtures UFCOSY non disponibles")
  skip_if_not(exists("seuil_max_pourcentage"), "Fonction non chargée")
  
  ufcosy_path <- get_fixtures_path()
  result <- read_bruker(dir = ufcosy_path, dim = "2D")
  mat <- result$spectrumData
  
  # 5% du max
  seuil <- seuil_max_pourcentage(mat, 0.05)
  
  expect_equal(seuil, 0.05 * max(abs(mat), na.rm = TRUE))
})

# =============================================================================
# TEST: Fitting sur spectre réel (si pics détectés)
# =============================================================================
test_that("fit_2d_peak fonctionne sur box de spectre réel", {
  skip_if_not(fixtures_available(), "Fixtures UFCOSY non disponibles")
  skip_if_not(exists("fit_2d_peak"), "Fonction non chargée")
  
  ufcosy_path <- get_fixtures_path()
  result <- read_bruker(dir = ufcosy_path, dim = "2D")
  mat <- result$spectrumData
  ppm_x <- as.numeric(colnames(mat))
  ppm_y <- as.numeric(rownames(mat))
  
  # Trouver le max du spectre et créer une box autour
  max_idx <- which(mat == max(mat), arr.ind = TRUE)[1, ]
  center_y <- ppm_y[max_idx[1]]
  center_x <- ppm_x[max_idx[2]]
  
  # Box de 0.1 ppm autour du max
  box <- data.frame(
    xmin = center_x - 0.05,
    xmax = center_x + 0.05,
    ymin = center_y - 0.05,
    ymax = center_y + 0.05
  )
  
  # Essayer le fitting
  fit_result <- fit_2d_peak(mat, ppm_x, ppm_y, box, "gaussian")
  
  # Doit retourner un résultat (même si fallback)
  expect_type(fit_result, "list")
  expect_true("volume" %in% names(fit_result))
  expect_true("method" %in% names(fit_result))
})

# =============================================================================
# TEST: Export/Import cohérent
# =============================================================================
test_that("Export puis import d'un spectre réel préserve les données", {
  skip_if_not(fixtures_available(), "Fixtures UFCOSY non disponibles")
  
  ufcosy_path <- get_fixtures_path()
  result <- read_bruker(dir = ufcosy_path, dim = "2D")
  mat <- result$spectrumData
  
  # Simuler un export de pics
  peaks <- data.frame(
    stain_id = "peak1",
    F2_ppm = as.numeric(rownames(mat))[50],
    F1_ppm = as.numeric(colnames(mat))[50],
    Volume = mat[50, 50]
  )
  
  # Export
  tmpfile <- tempfile(fileext = ".csv")
  write.csv2(peaks, tmpfile, row.names = FALSE)
  
  # Import
  imported <- read.csv2(tmpfile)
  
  # Vérifier cohérence
  expect_equal(imported$F2_ppm, peaks$F2_ppm)
  expect_equal(imported$F1_ppm, peaks$F1_ppm)
  expect_equal(imported$Volume, peaks$Volume)
  
  # Nettoyer
  unlink(tmpfile)
})