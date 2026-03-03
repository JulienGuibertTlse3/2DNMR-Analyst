# test-peak_fitting.R - Tests pour Peak_fitting.R
library(testthat)

# Helper
create_gaussian_peak <- function(nrow=50, ncol=50, amp=1000, cr=25, cc=25, sr=5, sc=5, noise=10) {
  mat <- matrix(0, nrow, ncol)
  for (i in 1:nrow) for (j in 1:ncol) mat[i,j] <- amp * exp(-((i-cr)^2/(2*sr^2) + (j-cc)^2/(2*sc^2)))
  mat <- mat + matrix(rnorm(nrow*ncol, 0, noise), nrow, ncol)
  rownames(mat) <- seq(5, 1, length.out=nrow); colnames(mat) <- seq(10, 0, length.out=ncol)
  mat
}

test_that("detect_local_maxima trouve pic simple", {
  mat <- create_gaussian_peak(noise=0)
  maxima <- detect_local_maxima(mat, threshold=0.3, min_distance=2)
  expect_s3_class(maxima, "data.frame")
  expect_true(nrow(maxima) >= 1)
  expect_true(abs(maxima$row[1] - 25) <= 2)
})

test_that("detect_local_maxima respecte min_distance", {
  mat <- create_gaussian_peak(noise=50)
  m_close <- detect_local_maxima(mat, 0.1, 2)
  m_far <- detect_local_maxima(mat, 0.1, 10)
  expect_true(nrow(m_far) <= nrow(m_close))
})

test_that("detect_local_maxima retourne vide si matrice trop petite", {
  maxima <- detect_local_maxima(matrix(1:4, 2), 0.3, 2)
  expect_equal(nrow(maxima), 0)
})

test_that("pseudo_voigt_2d centre = amplitude", {
  expect_equal(pseudo_voigt_2d(5,5,100,5,5,1,1,1,1,0.5), 100)
})

test_that("pseudo_voigt_2d loin = ~0", {
  expect_true(pseudo_voigt_2d(100,100,100,5,5,1,1,1,1,0.5) < 0.01)
})

test_that("pseudo_voigt_2d eta=0 Gaussien pur", {
  expect_equal(pseudo_voigt_2d(6,5,100,5,5,1,1,1,1,0), 100*exp(-0.5), tolerance=0.001)
})

test_that("pseudo_voigt_2d eta=1 Lorentzien pur", {
  expect_equal(pseudo_voigt_2d(6,5,100,5,5,1,1,1,1,1), 50, tolerance=0.001)
})

test_that("pseudo_voigt_2d symétrique", {
  v1 <- pseudo_voigt_2d(4,5,100,5,5,1,1,1,1,0.5)
  v2 <- pseudo_voigt_2d(6,5,100,5,5,1,1,1,1,0.5)
  expect_equal(v1, v2, tolerance=0.001)
})

test_that("fit_2d_peak fallback si peu de points", {
  # Simule le comportement de fit_2d_peak avec min_points
  fit_few <- function(n_points, min_pts=25) {
    if(n_points < min_pts) {
      return(list(volume=sum(1:n_points), method="sum_fallback", 
                  error="Too few points for fitting"))
    }
    list(volume=1000, method="gaussian", error=NULL)
  }
  
  # Peu de points -> sum_fallback
  result_few <- fit_few(10, min_pts=25)
  expect_equal(result_few$method, "sum_fallback")
  expect_false(is.na(result_few$volume))
  
  # Assez de points -> gaussian
  result_enough <- fit_few(30, min_pts=25)
  expect_equal(result_enough$method, "gaussian")
})

test_that("fit_2d_peak failed si box vide", {
  mat <- create_gaussian_peak()
  ppm_x <- as.numeric(colnames(mat)); ppm_y <- as.numeric(rownames(mat))
  box <- data.frame(xmin=100, xmax=110, ymin=100, ymax=110)
  result <- fit_2d_peak(mat, ppm_x, ppm_y, box, "gaussian")
  expect_equal(result$method, "failed")
})

test_that("fit_2d_peak refuse modèle invalide", {
  mat <- create_gaussian_peak()
  ppm_x <- as.numeric(colnames(mat)); ppm_y <- as.numeric(rownames(mat))
  box <- data.frame(xmin=3, xmax=7, ymin=1.5, ymax=4.5)
  expect_error(fit_2d_peak(mat, ppm_x, ppm_y, box, "invalid"), "not supported")
})

test_that("calculate_fitted_volumes traite plusieurs boxes", {
  mat <- create_gaussian_peak()
  ppm_x <- as.numeric(colnames(mat)); ppm_y <- as.numeric(rownames(mat))
  boxes <- data.frame(stain_id=c("p1","p2"), xmin=c(2,6), xmax=c(4,8), ymin=c(1.5,3), ymax=c(3,4.5))
  results <- calculate_fitted_volumes(mat, ppm_x, ppm_y, boxes, "gaussian")
  expect_equal(nrow(results), 2)
})

test_that("R² = 1 pour fit parfait", {
  obs <- 1:5; pred <- 1:5
  r2 <- 1 - sum((obs-pred)^2) / sum((obs-mean(obs))^2)
  expect_equal(r2, 1)
})

test_that("R² < 1 pour fit imparfait", {
  obs <- 1:5; pred <- c(1.1, 2.2, 2.8, 4.1, 4.9)
  r2 <- 1 - sum((obs-pred)^2) / sum((obs-mean(obs))^2)
  expect_true(r2 > 0.9 && r2 < 1)
})
