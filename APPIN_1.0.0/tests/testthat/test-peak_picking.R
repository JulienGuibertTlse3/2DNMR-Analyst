# test-peak_picking.R - Tests pour Peak_picking.R
library(testthat)

# Helper
create_spectrum <- function(nrow=100, ncol=100, peaks=list(list(r=30,c=30,a=50000)), noise=1000) {
  mat <- matrix(rnorm(nrow*ncol, 0, noise), nrow, ncol)
  for (p in peaks) for (i in 1:nrow) for (j in 1:ncol) mat[i,j] <- mat[i,j] + p$a*exp(-((i-p$r)^2+(j-p$c)^2)/100)
  rownames(mat) <- seq(10, 0, length.out=nrow); colnames(mat) <- seq(10, 0, length.out=ncol)
  mat
}

# =============================================================================
# TEST: peak_pick_2d_nt2() 
# =============================================================================
test_that("peak_pick_2d_nt2 exige rownames/colnames", {
  mat <- matrix(1:100, 10)
  expect_error(peak_pick_2d_nt2(mat, threshold_value=10), "row names.*column names")
})

test_that("peak_pick_2d_nt2 vide si threshold trop haut", {
  mat <- create_spectrum()
  result <- peak_pick_2d_nt2(mat, threshold_value=1e9, verbose=FALSE)
  expect_type(result, "list")
  expect_true(nrow(result$peaks) == 0 || nrow(result$centroids) == 0)
})

test_that("peak_pick_2d_nt2 structure correcte", {
  mat <- create_spectrum()
  result <- peak_pick_2d_nt2(mat, threshold_value=5000, spectrum_type="TOCSY", verbose=FALSE)
  expect_type(result, "list")
  expect_true("bounding_boxes" %in% names(result))
  expect_true("cluster_stats" %in% names(result))
})

test_that("peak_pick_2d_nt2 supporte tous spectrum_type", {
  mat <- create_spectrum()
  for (st in c("HSQC", "TOCSY", "COSY", "UFCOSY")) {
    result <- peak_pick_2d_nt2(mat, threshold_value=5000, spectrum_type=st, verbose=FALSE)
    expect_type(result, "list")
  }
})

# =============================================================================
# TEST: Calculs de filtrage
# =============================================================================
test_that("Elongation cluster carré = 1", {
  x_span <- 1; y_span <- 1
  elongation <- max(x_span/(y_span+1e-10), y_span/(x_span+1e-10))
  expect_equal(elongation, 1)
})

test_that("Elongation cluster allongé", {
  x_span <- 10; y_span <- 1
  elongation <- max(x_span/(y_span+1e-10), y_span/(x_span+1e-10))
  expect_equal(elongation, 10)
})

test_that("Détection diagonale TOCSY", {
  x_center <- 3.5; y_center <- 3.45
  is_diag <- abs(abs(x_center) - abs(y_center)) < 0.1
  expect_true(is_diag)
})

test_that("Détection hors diagonale", {
  x_center <- 3.5; y_center <- 7.0
  is_diag <- abs(abs(x_center) - abs(y_center)) < 0.1
  expect_false(is_diag)
})

test_that("Détection artefact horizontal", {
  x_span <- 0.5; y_span <- 0.01; y_var <- 0.00001; n_pts <- 50
  is_horiz <- x_span/(y_span+1e-10) > y_span/(x_span+1e-10)
  is_line <- is_horiz && y_var < 0.0001 && x_span > 0.08 && n_pts < 200
  expect_true(is_line)
})

test_that("Détection artefact vertical", {
  x_span <- 0.01; y_span <- 0.3; x_var <- 0.00001; n_pts <- 50
  is_vert <- y_span/(x_span+1e-10) > x_span/(y_span+1e-10)
  is_line <- is_vert && x_var < 0.00005 && y_span > 0.15 && n_pts < 200
  expect_true(is_line)
})

# =============================================================================
# TEST: Centroïdes pondérés
# =============================================================================
test_that("Centroïde pondéré poids égaux", {
  pts <- data.frame(x=c(1,2,3), y=c(1,2,3), level=c(100,200,100))
  cx <- sum(pts$x * pts$level) / sum(pts$level)
  cy <- sum(pts$y * pts$level) / sum(pts$level)
  expect_equal(cx, 2); expect_equal(cy, 2)
})

test_that("Centroïde pondéré poids asymétriques", {
  pts <- data.frame(x=c(1,2), y=c(1,2), level=c(300,100))
  cx <- sum(pts$x * pts$level) / sum(pts$level)
  expect_equal(cx, 1.25)
})
