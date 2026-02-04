# ======================================================================================
# 2D NMR Spectra Analysis - Launch Script
# ===========================================================================================
# This script automatically installs the necessary packages and launches the application.
#
# USAGE:
# 1. Open this file in RStudio
# 2. Click on "Source" or run the entire script (Ctrl+Shift+Enter)
#
# REQUIRED STRUCTURE:
#2DNMR-Analyst/
# ├── run_app.R <- THIS FILE (entry point)
# ├── Shine.R <- Main application
#└──Function/
# ├── Read_2DNMR_spectrum.R
# ├── Peak_fitting.R
# ├── Visualization.R
# ├── Peak_picking.R
# └── CNN_shiny.R
# ============================================================================

cat("
╔══════════════════════════════════════════════════════════════════╗
║           2D NMR Spectra Analysis - Initialisation               ║
╚══════════════════════════════════════════════════════════════════╝
\n")

# ----------------------------------------------------------------------------
# 1. DEFINE THE WORKING DIRECTORY
# ----------------------------------------------------------------------------

# # Automatically set the working directory to the folder containing this script
if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
  script_path <- dirname(rstudioapi::getSourceEditorContext()$path)
  if (nchar(script_path) > 0) {
    setwd(script_path)
    cat("📁 Working directory :", getwd(), "\n\n")
  }
} else {
  cat("📁 Current working directory :", getwd(), "\n")
  cat("   (Make sure you are in the 2DNMR-Analyst folder)\n\n")
}

# ----------------------------------------------------------------------------
# 2. LIST OF REQUIRED PACKAGES
# ----------------------------------------------------------------------------

packages_required <- c(
  # Interface Shiny
  "shiny",
  "shinyFiles",
  "shinydashboard",
  "shinydashboardPlus",
  "shinyBS",
  "shinyjs",
  "shinycssloaders",
  
  # Visualisation
  "plotly",
  "ggplot2",
  "DT",
  
  # Manipulation de données
  "dplyr",
  "data.table",
  "magrittr",
  "zoo",
  "reshape2",
  
  # Analyse
  "dbscan",
  "sp",
  "matrixStats",
  "pracma",
  "minpack.lm",
  
  # Deep Learning
  "tensorflow",
  "keras",
  "imager",
  
  # Autres
  "Rcpp",
  "readr",
  "viridis",
  "abind"
)


# ----------------------------------------------------------------------------
# 3. VERIFICATION AND INSTALLATION OF REQUIRED VERSIONS
# ----------------------------------------------------------------------------

# Versions minimales requises pour les packages critiques
required_versions <- list(
  # reticulate = "1.41.0",
  # tensorflow = "2.9.0",
  # keras = "2.9.0",
  shiny = "1.7.0"
)

cat("🔍 Checking critical packages...\n\n")

# Check and install/update packages with specific versions
for (pkg_name in names(required_versions)) {
  required_version <- required_versions[[pkg_name]]
  needs_install <- FALSE
  
  if (!requireNamespace(pkg_name, quietly = TRUE)) {
    cat("   📦", pkg_name, "not installed\n")
    needs_install <- TRUE
  } else {
    current_version <- tryCatch(
      as.character(packageVersion(pkg_name)),
      error = function(e) "0.0.0"
    )
    if (package_version(current_version) < package_version(required_version)) {
      cat("   ⚠️ ", pkg_name, current_version, "< required version", required_version, "\n")
      needs_install <- TRUE
    } else {
      cat("   ✅", pkg_name, current_version, "\n")
    }
  }
  
  if (needs_install) {
    cat("      → Installation/update of", pkg_name, "...\n")
    install.packages(pkg_name, dependencies = TRUE)
  }
}

cat("\n🔍 Checking for other required packages...\n\n")

missing_packages <- packages_required[!sapply(packages_required, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  cat("📦 Installing missing packages :", paste(missing_packages, collapse = ", "), "\n\n")
  install.packages(missing_packages, dependencies = TRUE)
}


# ----------------------------------------------------------------------------
# 4. LOADING PACKAGES
# ----------------------------------------------------------------------------

cat("📚Loading packages...\n")

for (pkg in packages_required) {
  suppressWarnings(suppressPackageStartupMessages(library(pkg, character.only = TRUE)))
}

cat("   ✅ All packages loaded\n")

# ----------------------------------------------------------------------------
# 5. VERIFICATION OF SOURCE FILES
# ----------------------------------------------------------------------------

cat("\n🔍 Source file verification...\n")

source_files <- c(
  "Function/Read_2DNMR_spectrum.R",
  "Function/Vizualisation.R",
  "Function/Peak_picking.R",
  "Function/Peak_fitting.R",
  "Function/CNN_shiny.R"
)

all_files_ok <- TRUE

for (f in source_files) {
  if (file.exists(f)) {
    cat("   ✅", f, "\n")
  } else {
    cat("   ❌", f, "- MISSING!\n")
    all_files_ok <- FALSE
  }
}

if (!file.exists("Shine.R")) {
  cat("   ❌ Shine.R - MISSING!\n")
  all_files_ok <- FALSE
} else {
  cat("   ✅ Shine.R\n")
}

# ----------------------------------------------------------------------------
# 6. APPLICATION LAUNCH
# ----------------------------------------------------------------------------

if (all_files_ok) {
  cat("\n")
  cat("╔══════════════════════════════════════════════════════════════════╗\n")
  cat("║                 🚀 App launch                                    ║\n")
  cat("╚══════════════════════════════════════════════════════════════════╝\n")
  cat("\n")
  cat("The application will open in your browser...\n")
  cat("To stop: click on STOP in RStudio or press Esc\n\n")
  
  shiny::runApp("Shine.R") 
  
} else {
  cat("\n")
  cat("╔══════════════════════════════════════════════════════════════════╗\n")
  cat("║            ❌ERROR: Missing files                                ║\n")
  cat("╚══════════════════════════════════════════════════════════════════╝\n")
  cat("\n")
  cat("Please check that you have downloaded all files from :\n")
  cat("https://github.com/JulienGuibertTlse3/2DNMR-Analyst\n\n")
}