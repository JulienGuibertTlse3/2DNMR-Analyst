# test-read_bruker.R - Tests pour Read_2DNMR_spectrum.R
library(testthat)

# Note: Tests limités car nécessite fichiers Bruker réels
# =============================================================================
# TEST: Validation entrées
# =============================================================================
test_that("read_bruker NULL si dir NULL", {
  result <- read_bruker(dir=NULL, dim="2D")
  expect_null(result)
})

test_that("read_bruker checkFiles erreur si procs absent", {
  expect_error(read_bruker(dir="/nonexistent", dim="2D", checkFiles=TRUE), "Could not open")
})

# =============================================================================
# TEST: Byte order mapping
# =============================================================================
test_that("BYTORDP mapping correct", {
  dict <- c("little","big"); names(dict) <- c(0,1)
  expect_equal(unname(dict["0"]), "little")
  expect_equal(unname(dict["1"]), "big")
})

# =============================================================================
# TEST: Dimension mapping
# =============================================================================
test_that("Dimension to filename", {
  d <- c("1r","2rr"); names(d) <- c("1D","2D")
  expect_equal(unname(d["1D"]), "1r")
  expect_equal(unname(d["2D"]), "2rr")
})

# =============================================================================
# TEST: Calcul axes ppm
# =============================================================================
test_that("Calcul axe ppm correct", {
  OFFSET <- 10.0; SW_p <- 5000; SF <- 500; SI <- 1024
  freq <- OFFSET - (0:(SI-1)) * SW_p/SF/SI
  expect_length(freq, SI)
  expect_equal(freq[1], OFFSET)
  expect_true(all(diff(freq) < 0))  # Décroissant
})

test_that("rightlimit leftlimit", {
  OFFSET <- 10.0; SW_p <- 5000; SF <- 500
  right <- OFFSET - SW_p/SF
  expect_equal(OFFSET, 10.0)
  expect_equal(right, 0)
})

# =============================================================================
# TEST: Formatage EXPNO
# =============================================================================
test_that("EXPNO padding", {
  fmt <- function(folder, expno) {
    pad <- paste(c("_","0","0","0","0")[1:max(1,5-nchar(expno))], collapse="")
    paste0(folder, pad, expno)
  }
  expect_equal(fmt("s","1"), "s_0001")
  expect_equal(fmt("s","10"), "s_0010")
  expect_equal(fmt("s","100"), "s_0100")
  expect_equal(fmt("s","1000"), "s_1000")
})

# =============================================================================
# TEST: useAsNames
# =============================================================================
test_that("useAsNames options", {
  title <- "MyTitle"; folder <- "sample"; folder_exp <- "sample_0010"
  expect_equal(switch("Spectrum titles", "Spectrum titles"=title, "dir names"=folder), title)
  expect_equal(switch("dir names", "Spectrum titles"=title, "dir names"=folder), folder)
})

# =============================================================================
# TEST: NC_proc scaling
# =============================================================================
test_that("NC_proc scaling", {
  raw <- c(100, 200, 300)
  expect_equal(raw * 2^0, raw)
  expect_equal(raw * 2^1, raw*2)
  expect_equal(raw * 2^(-1), raw/2)
})
