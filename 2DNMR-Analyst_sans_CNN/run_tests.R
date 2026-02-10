# run_tests.R - 2DNMR-Analyst Tests Unitaires
if (!require("testthat", quietly = TRUE)) install.packages("testthat")
library(testthat)

load_sources <- function(verbose = TRUE) {
  if (verbose) cat("\n📂 Chargement des sources...\n")
  files <- c("Function/Read_2DNMR_spectrum.R", "Function/Vizualisation.R",
             "Function/Peak_picking.R", "Function/Peak_fitting.R", "R/utils.R")
  for (f in files) {
    if (file.exists(f)) {
      tryCatch({ source(f); if (verbose) cat("  ✅", f, "\n") },
               error = function(e) if (verbose) cat("  ❌", f, "\n"))
    } else if (verbose) cat("  ⚠️", f, "(non trouvé)\n")
  }
}

run_all_tests <- function(load = TRUE) {
  cat("\n╔═══════════════════════════════════════════════════════╗\n")
  cat("║     2DNMR-Analyst v3.0 - Tests Unitaires              ║\n")
  cat("╚═══════════════════════════════════════════════════════╝\n")
  if (!dir.exists("tests/testthat")) stop("❌ tests/testthat non trouvé!")
  if (load) load_sources()
  cat("\n🧪 Exécution...\n\n")
  test_dir("tests/testthat", reporter = "summary")
}

run_test <- function(name) {
  if (!grepl("^test-", name)) name <- paste0("test-", name)
  if (!grepl("\\.R$", name)) name <- paste0(name, ".R")
  path <- file.path("tests", "testthat", name)
  if (!file.exists(path)) stop("❌ Non trouvé: ", path)
  cat(sprintf("\n🧪 %s\n\n", name))
  load_sources(FALSE)
  test_file(path, reporter = "summary")
}

list_tests <- function() {
  files <- list.files("tests/testthat", pattern = "^test-.*\\.R$")
  cat("\n📋 Tests disponibles:\n")
  for (f in files) {
    n <- sum(grepl("^test_that", readLines(file.path("tests/testthat", f), warn=FALSE)))
    cat(sprintf("  • %-25s (%d tests)\n", gsub("test-|\\.R", "", f), n))
  }
  cat("\nUsage: run_test('fitting')\n\n")
}

cat("\n╔═══════════════════════════════════════════════════════╗\n")
cat("║  run_all_tests()  - Tous les tests                    ║\n")
cat("║  run_test('name') - Un fichier spécifique             ║\n")
cat("║  list_tests()     - Voir les tests disponibles        ║\n")
cat("╚═══════════════════════════════════════════════════════╝\n\n")

