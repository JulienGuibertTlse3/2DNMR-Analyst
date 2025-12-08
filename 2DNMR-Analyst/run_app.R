# ============================================================================
# 2D NMR Spectra Analysis - Script de lancement
# ============================================================================
# Ce script installe automatiquement les packages nécessaires et lance l'application
# 
# UTILISATION :
#   1. Ouvrir ce fichier dans RStudio
#   2. Cliquer sur "Source" ou exécuter tout le script (Ctrl+Shift+Enter)
#
# STRUCTURE REQUISE :
#   2DNMR-Analyst/
#   ├── run_app.R              <- CE FICHIER (point d'entrée)
#   ├── Shine.R                <- Application principale
#   └── Function_test/
#       ├── Read_2DNMR_spectrum.R
#       ├── Vizualisation.R
#       ├── Integration.R
#       ├── Pping.R
#       └── CNN_shiny.R
# ============================================================================

cat("
╔══════════════════════════════════════════════════════════════════╗
║           2D NMR Spectra Analysis - Initialisation               ║
╚══════════════════════════════════════════════════════════════════╝
\n")

# ----------------------------------------------------------------------------
# 1. DÉFINIR LE RÉPERTOIRE DE TRAVAIL
# ----------------------------------------------------------------------------

# Automatiquement définir le répertoire de travail au dossier contenant ce script
if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
  script_path <- dirname(rstudioapi::getSourceEditorContext()$path)
  if (nchar(script_path) > 0) {
    setwd(script_path)
    cat("📁 Répertoire de travail :", getwd(), "\n\n")
  }
} else {
  cat("📁 Répertoire de travail actuel :", getwd(), "\n")
  cat("   (Assurez-vous d'être dans le dossier 2DNMR-Analyst)\n\n")
}

# ----------------------------------------------------------------------------
# 2. LISTE DES PACKAGES REQUIS
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
  "Rcpp"
)

# Check and install required reticulate version
required_reticulate <- "1.41.0"
if (!requireNamespace("reticulate", quietly = TRUE) || 
    packageVersion("reticulate") < required_reticulate) {
  message("Installing/updating reticulate package...")
  install.packages("reticulate")
}

# ----------------------------------------------------------------------------
# 3. INSTALLATION DES PACKAGES MANQUANTS
# ----------------------------------------------------------------------------

cat("🔍 Vérification des packages requis...\n\n")

missing_packages <- packages_required[!sapply(packages_required, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  cat("📦 Installation des packages manquants :", paste(missing_packages, collapse = ", "), "\n\n")
  install.packages(missing_packages, dependencies = TRUE)
}

# ----------------------------------------------------------------------------
# 4. CHARGEMENT DES PACKAGES
# ----------------------------------------------------------------------------

cat("📚 Chargement des packages...\n")

for (pkg in packages_required) {
  suppressWarnings(suppressPackageStartupMessages(library(pkg, character.only = TRUE)))
}

cat("   ✅ Tous les packages chargés\n")

# ----------------------------------------------------------------------------
# 5. VÉRIFICATION DES FICHIERS SOURCE
# ----------------------------------------------------------------------------

cat("\n🔍 Vérification des fichiers sources...\n")

source_files <- c(
  "Function/Read_2DNMR_spectrum.R",
  "Function/Vizualisation.R",
  "Function/Pping.R",
  "Function/CNN_shiny.R"
)

all_files_ok <- TRUE

for (f in source_files) {
  if (file.exists(f)) {
    cat("   ✅", f, "\n")
  } else {
    cat("   ❌", f, "- MANQUANT!\n")
    all_files_ok <- FALSE
  }
}

if (!file.exists("Shine.R")) {
  cat("   ❌ Shine.R - MANQUANT!\n")
  all_files_ok <- FALSE
} else {
  cat("   ✅ Shine.R\n")
}

# ----------------------------------------------------------------------------
# 6. LANCEMENT DE L'APPLICATION
# ----------------------------------------------------------------------------

if (all_files_ok) {
  cat("\n")
  cat("╔══════════════════════════════════════════════════════════════════╗\n")
  cat("║                 🚀 Lancement de l'application                    ║\n")
  cat("╚══════════════════════════════════════════════════════════════════╝\n")
  cat("\n")
  cat("L'application va s'ouvrir dans votre navigateur...\n")
  cat("Pour arrêter : cliquez sur STOP dans RStudio ou appuyez sur Échap\n\n")
  
  shiny::runApp("Shine.R") 
  
} else {
  cat("\n")
  cat("╔══════════════════════════════════════════════════════════════════╗\n")
  cat("║  ❌ ERREUR : Fichiers manquants                                  ║\n")
  cat("╚══════════════════════════════════════════════════════════════════╝\n")
  cat("\n")
  cat("Veuillez vérifier que vous avez téléchargé tous les fichiers depuis :\n")
  cat("https://github.com/JulienGuibertTlse3/2DNMR-Analyst\n\n")
}
