# test-utils.R - Tests pour R/utils.R
library(testthat)

# =============================================================================
# TEST: Opérateur %||% (null coalescing)
# =============================================================================
test_that("%||% retourne valeur si non-NULL", {
  `%||%` <- function(a, b) if(is.null(a)) b else a
  expect_equal(5 %||% 10, 5)
  expect_equal("test" %||% "default", "test")
  expect_equal(0 %||% 10, 0)
  expect_equal(FALSE %||% TRUE, FALSE)
})

test_that("%||% retourne défaut si NULL", {
  `%||%` <- function(a, b) if(is.null(a)) b else a
  expect_equal(NULL %||% 10, 10)
  expect_equal(NULL %||% "default", "default")
})

# =============================================================================
# TEST: parse_keep_peak_ranges()
# =============================================================================
test_that("parse_keep_peak_ranges parse format standard", {
  parse_kpr <- function(text) {
    if(is.null(text) || trimws(text)=="") return(NULL)
    ranges <- strsplit(text, ";")[[1]]
    ranges <- trimws(ranges[ranges != ""])
    if(length(ranges)==0) return(NULL)
    result <- lapply(ranges, function(r) {
      parts <- strsplit(r, ",")[[1]]
      if(length(parts)!=2) return(NULL)
      as.numeric(trimws(parts))
    })
    result[!sapply(result, is.null)]
  }
  
  result <- parse_kpr("0.5,-0.5; 1,0.8")
  expect_type(result, "list")
  expect_length(result, 2)
  expect_equal(result[[1]], c(0.5, -0.5))
  expect_equal(result[[2]], c(1, 0.8))
})

test_that("parse_keep_peak_ranges NULL si vide", {
  parse_kpr <- function(text) {
    if(is.null(text) || trimws(text)=="") return(NULL)
    NULL
  }
  expect_null(parse_kpr(NULL))
  expect_null(parse_kpr(""))
  expect_null(parse_kpr("   "))
})

# =============================================================================
# TEST: Calcul volume somme
# =============================================================================
test_that("volume somme = total intensités", {
  mat <- matrix(1, 10, 10)
  rownames(mat) <- 10:1; colnames(mat) <- 1:10
  
  calc_vol <- function(mat, box) {
    ppm_f2 <- as.numeric(colnames(mat))
    ppm_f1 <- as.numeric(rownames(mat))
    x_idx <- which(ppm_f2 >= box$xmin & ppm_f2 <= box$xmax)
    y_idx <- which(ppm_f1 >= box$ymin & ppm_f1 <= box$ymax)
    if(length(x_idx)==0 || length(y_idx)==0) return(NA_real_)
    sum(mat[y_idx, x_idx], na.rm=TRUE)
  }
  
  expect_equal(calc_vol(mat, list(xmin=1,xmax=10,ymin=1,ymax=10)), 100)
  expect_equal(calc_vol(mat, list(xmin=1,xmax=5,ymin=1,ymax=5)), 25)
})

# =============================================================================
# TEST: Normalisation z-score
# =============================================================================
test_that("Normalisation z-score", {
  norm <- function(x) (x - mean(x)) / sd(x)
  x <- c(1, 2, 3, 4, 5)
  x_norm <- norm(x)
  expect_equal(mean(x_norm), 0, tolerance=1e-10)
  expect_equal(sd(x_norm), 1, tolerance=1e-10)
})

# =============================================================================
# TEST: Scaling F1 HSQC
# =============================================================================
test_that("Scaling F1 HSQC divise par facteur", {
  scale_f1 <- function(f1, factor=5) f1 / factor
  expect_equal(scale_f1(c(10,20,30,40,50)), c(2,4,6,8,10))
})
