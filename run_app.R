# ============================================================================
# 2D NMR Spectra Analysis - Script de lancement
# ============================================================================
# Ce script vérifie l'environnement et lance l'application
# 
# UTILISATION :
#   1. Ouvrir ce fichier dans RStudio
#   2. Cliquer sur "Source" ou exécuter tout le script (Ctrl+Shift+Enter)
#
# PREMIÈRE UTILISATION :
#   Exécuter d'abord : source("setup.R")
#
# ============================================================================

cat("\n")
cat("╔══════════════════════════════════════════════════════════════════╗\n")
cat("║           2D NMR Spectra Analysis - Démarrage                    ║\n")
cat("╚══════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# ----------------------------------------------------------------------------
# 1. DÉFINIR LE RÉPERTOIRE DE TRAVAIL
# ----------------------------------------------------------------------------

if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
  script_path <- tryCatch(
    dirname(rstudioapi::getSourceEditorContext()$path),
    error = function(e) ""
  )
  if (nchar(script_path) > 0) {
    setwd(script_path)
  }
}

cat("📁 Répertoire de travail :", getwd(), "\n\n")

# ----------------------------------------------------------------------------
# 2. VÉRIFICATION DE L'ENVIRONNEMENT
# ----------------------------------------------------------------------------

cat("🔍 Vérification de l'environnement...\n\n")

# Vérifier si renv est initialisé
if (!file.exists("renv.lock")) {
  stop("❌ Fichier renv.lock non trouvé. Êtes-vous dans le bon dossier ?")
}

# Vérifier si l'environnement Python existe
venv_path <- file.path(getwd(), ".venv")
if (!dir.exists(venv_path)) {
  cat("⚠️  Environnement Python non trouvé.\n")
  cat("   Exécutez d'abord : source('setup.R')\n\n")
  stop("Installation requise. Lancez source('setup.R')")
}

cat("   ✅ renv.lock trouvé\n")
cat("   ✅ Environnement Python trouvé\n\n")

# ----------------------------------------------------------------------------
# 3. CONFIGURER PYTHON
# ----------------------------------------------------------------------------

cat("🐍 Configuration de Python...\n")

library(reticulate)

# Configurer reticulate pour utiliser notre environnement
use_virtualenv(venv_path, required = TRUE)

# Vérifier TensorFlow
tryCatch({
  tf <- import("tensorflow")
  cat("   ✅ TensorFlow", tf$`__version__`, "chargé\n\n")
}, error = function(e) {
  cat("   ⚠️  TensorFlow non disponible:", e$message, "\n")
  cat("   Le CNN ne fonctionnera pas, mais l'app peut démarrer.\n\n")
})

# ----------------------------------------------------------------------------
# 4. VÉRIFICATION DES FICHIERS SOURCE
# ----------------------------------------------------------------------------

cat("🔍 Vérification des fichiers sources...\n")

source_files <- c(
  "Function/Read_2DNMR_spectrum.R",
  "Function/Vizualisation.R",
  "Function/Pping.R",
  "Function/CNN_shiny.R",
  "Shine.R"
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

# ----------------------------------------------------------------------------
# 5. LANCEMENT DE L'APPLICATION
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
