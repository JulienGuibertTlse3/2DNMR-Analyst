# ============================================================================
# setup.R - Installation complète pour 2DNMR-Analyst
# ============================================================================
# 
# EXÉCUTER CE SCRIPT UNE SEULE FOIS sur une nouvelle machine :
#   source("setup.R")
#
# Ce script va :
#   1. Restaurer tous les packages R depuis renv.lock
#   2. Installer Python 3.10 (si nécessaire)
#   3. Créer un environnement virtuel Python dans .venv/
#   4. Installer TensorFlow et les dépendances Python
#
# ============================================================================

cat("\n")
cat("╔══════════════════════════════════════════════════════════════════╗\n")
cat("║          2DNMR-Analyst - Installation Setup                      ║\n")
cat("╚══════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# ----------------------------------------------------------------------------
# ÉTAPE 1 : Restaurer les packages R
# ----------------------------------------------------------------------------

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("ÉTAPE 1/4 : Restauration des packages R\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

if (!requireNamespace("renv", quietly = TRUE)) {
  cat("📦 Installation de renv...\n")
  install.packages("renv")
}

cat("📦 Restauration des packages depuis renv.lock...\n")
cat("   (Cela peut prendre 10-20 minutes la première fois)\n\n")

renv::init(bare = TRUE)

# 2. Maintenant restaurer
renv::restore(prompt = FALSE)

cat("\n✅ Packages R restaurés\n\n")

# ----------------------------------------------------------------------------
# ÉTAPE 2 : Configurer Python avec reticulate
# ----------------------------------------------------------------------------

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("ÉTAPE 2/4 : Configuration de Python\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

library(reticulate)

# Chemin de l'environnement virtuel
venv_path <- file.path(getwd(), ".venv")

# Vérifier si Python 3.10 est disponible
cat("🔍 Recherche de Python 3.10...\n")

python_installed <- tryCatch({
  versions <- reticulate::py_versions_windows()
  any(grepl("3\\.10", versions$version))
}, error = function(e) FALSE)

if (!python_installed) {
  cat("📥 Installation de Python 3.10 via reticulate...\n")
  cat("   (Téléchargement en cours, veuillez patienter...)\n\n")
  reticulate::install_python(version = "3.10:latest")
  cat("✅ Python 3.10 installé\n\n")
} else {
  cat("✅ Python 3.10 déjà disponible\n\n")
}

# ----------------------------------------------------------------------------
# ÉTAPE 3 : Créer l'environnement virtuel
# ----------------------------------------------------------------------------

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("ÉTAPE 3/4 : Création de l'environnement virtuel Python\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

if (dir.exists(venv_path)) {
  cat("⚠️  L'environnement .venv existe déjà.\n")
  cat("   Voulez-vous le recréer ? (Tapez 'oui' pour confirmer)\n")
  
  if (interactive()) {
    response <- readline(prompt = "   > ")
    if (tolower(response) == "oui") {
      cat("🗑️  Suppression de l'ancien environnement...\n")
      unlink(venv_path, recursive = TRUE)
    } else {
      cat("   Conservation de l'environnement existant.\n\n")
    }
  }
}

if (!dir.exists(venv_path)) {
  cat("📁 Création de l'environnement virtuel dans .venv/\n")
  reticulate::virtualenv_create(envname = venv_path, version = "3.10:latest")
  cat("✅ Environnement virtuel créé\n\n")
} else {
  cat("✅ Environnement virtuel existant utilisé\n\n")
}

# ----------------------------------------------------------------------------
# ÉTAPE 4 : Installer les packages Python
# ----------------------------------------------------------------------------

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("ÉTAPE 4/4 : Installation des packages Python (TensorFlow, etc.)\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

cat("📦 Installation de TensorFlow et dépendances...\n")
cat("   (Cela peut prendre quelques minutes)\n\n")

python_packages <- c(
  "numpy==1.26.4",
  "tensorflow==2.15.1",
  "scipy",
  "pandas",
  "h5py",
  "pillow"
)

reticulate::virtualenv_install(
  envname = venv_path,
  packages = python_packages,
  ignore_installed = FALSE
)

cat("✅ Packages Python installés\n\n")

# ----------------------------------------------------------------------------
# VÉRIFICATION FINALE
# ----------------------------------------------------------------------------

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("VÉRIFICATION DE L'INSTALLATION\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

# Utiliser le nouvel environnement
reticulate::use_virtualenv(venv_path, required = TRUE)

# Vérifier Python
cat("🐍 Python:\n")
py_config()

# Vérifier TensorFlow
cat("🧠 TensorFlow:\n")
tryCatch({
  tf <- reticulate::import("tensorflow")
  cat("   Version:", tf$`__version__`, "\n")
  cat("   ✅ TensorFlow fonctionne correctement\n\n")
}, error = function(e) {
  cat("   ❌ Erreur:", e$message, "\n\n")
})

# Vérifier NumPy
cat("🔢 NumPy:\n")
tryCatch({
  np <- reticulate::import("numpy")
  cat("   Version:", np$`__version__`, "\n")
  cat("   ✅ NumPy fonctionne correctement\n\n")
}, error = function(e) {
  cat("   ❌ Erreur:", e$message, "\n\n")
})

# ----------------------------------------------------------------------------
# RÉSUMÉ
# ----------------------------------------------------------------------------

cat("\n")
cat("╔══════════════════════════════════════════════════════════════════╗\n")
cat("║                    ✅ INSTALLATION TERMINÉE                      ║\n")
cat("╚══════════════════════════════════════════════════════════════════╝\n")
cat("\n")
cat("Prochaines étapes :\n")
cat("  1. Redémarrez R/RStudio (important!)\n")
cat("  2. Lancez l'application avec : shiny::runApp('Shine.R')\n")
cat("\n")
cat("En cas de problème :\n")
cat("  - Vérifiez que vous êtes dans le bon dossier\
  - Relancez ce script avec : source('setup.R')\n")
cat("  - Consultez le README sur GitHub\n")
cat("\n")
