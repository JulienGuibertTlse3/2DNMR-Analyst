#===============================================================================
#                    GUIDE DE RÉENTRAÎNEMENT DU CNN
#                    60 spectres réels + 5000 synthétiques
#===============================================================================
#
# STRUCTURE DES DOSSIERS À PRÉPARER :
#
# C:/Users/norouchon/Documents/CNN_Training/
# ├── spectres_reels/
# │   ├── TOCSY/
# │   │   ├── spectre_001/pdata/1/      (données Bruker)
# │   │   ├── spectre_002/pdata/1/
# │   │   └── ...
# │   ├── HSQC/
# │   │   ├── spectre_001/pdata/1/
# │   │   └── ...
# │   └── COSY/
# │       └── ...
# ├── annotations/
# │   ├── spectre_001_peaks.csv
# │   ├── spectre_002_peaks.csv
# │   └── ...
# └── modeles/
#     ├── weights_v1/                   (ancien modèle)
#     └── weights_v2/                   (nouveau modèle)
#
#===============================================================================

library(purrr)
library(ggplot2)
library(keras)
library(viridis)
library(plotly)
library(reshape2)
library(abind)
library(tibble)
library(dplyr)
library(gridExtra)
library(dbscan)
library(stats)
library(readr)

# Charger les bases de données de métabolites pour les spectres synthétiques
load("C:/Users/juguibert/Downloads/BdDReference_CPMG.Rdata")
load("C:/Users/juguibert/Downloads/BdDReference_13C.Rdata")

# Charger le générateur de spectres
source("spectrum_generator_random.R")

# Charger CNN_learn.R qui contient maintenant les fonctions de décomposition 2D
source("CNN_learn.R")

#===============================================================================
# FONCTIONS UTILITAIRES ROBUSTES
#===============================================================================

#' Lecture robuste de CSV (détecte automatiquement le séparateur)
#' Gère les formats français (;) et internationaux (,)
#'
#' @param file_path Chemin vers le fichier CSV
#' @return Un data.frame
read_csv_auto <- function(file_path) {
  # Lire les premières lignes pour détecter le format
  first_lines <- readLines(file_path, n = 2, warn = FALSE)
  
  if (length(first_lines) == 0) {
    # Fichier vide
    return(data.frame(F1_ppm = numeric(0), F2_ppm = numeric(0), intensity = numeric(0)))
  }
  
  first_line <- first_lines[1]
  
  # Détecter le séparateur
  n_semicolons <- lengths(regmatches(first_line, gregexpr(";", first_line)))
  n_commas <- lengths(regmatches(first_line, gregexpr(",", first_line)))
  
  if (n_semicolons > n_commas) {
    # Format français (point-virgule, virgule décimale)
    df <- tryCatch({
      read.csv2(file_path, stringsAsFactors = FALSE)
    }, error = function(e) {
      read.table(file_path, sep = ";", header = TRUE, stringsAsFactors = FALSE, dec = ",")
    })
  } else {
    # Format international (virgule, point décimal)
    df <- tryCatch({
      read.csv(file_path, stringsAsFactors = FALSE)
    }, error = function(e) {
      read.table(file_path, sep = ",", header = TRUE, stringsAsFactors = FALSE)
    })
  }
  
  # Nettoyer les noms de colonnes (supprimer BOM, espaces, etc.)
  names(df) <- trimws(gsub("[\xef\xbb\xbf]", "", names(df)))
  
  return(df)
}

#' Charge les pics 2D depuis un fichier CSV (version robuste)
#'
#' @param csv_path Chemin vers le fichier CSV d'annotation
#' @return Un data.frame avec F1_ppm, F2_ppm, intensity
load_2D_peaks_robust <- function(csv_path) {
  df <- read_csv_auto(csv_path)
  
  # Vérifier les colonnes requises
  required_cols <- c("F1_ppm", "F2_ppm")
  
  # Chercher les colonnes même avec des noms légèrement différents
  col_mapping <- list(
    F1_ppm = c("F1_ppm", "F1.ppm", "f1_ppm", "F1", "ppm1", "x_ppm"),
    F2_ppm = c("F2_ppm", "F2.ppm", "f2_ppm", "F2", "ppm2", "y_ppm"),
    intensity = c("intensity", "Intensity", "int", "amplitude", "height")
  )
  
  # Renommer les colonnes si nécessaire
  for (target_name in names(col_mapping)) {
    possible_names <- col_mapping[[target_name]]
    found <- intersect(possible_names, names(df))
    if (length(found) > 0 && !(target_name %in% names(df))) {
      names(df)[names(df) == found[1]] <- target_name
    }
  }
  
  # Vérifier que les colonnes essentielles existent
  if (!all(c("F1_ppm", "F2_ppm") %in% names(df))) {
    stop(sprintf("Colonnes manquantes dans %s. Colonnes trouvées: %s", 
                 csv_path, paste(names(df), collapse = ", ")))
  }
  
  # Ajouter intensity si manquante
  if (!"intensity" %in% names(df)) {
    df$intensity <- 1.0
  }
  
  # Convertir en numérique
  df$F1_ppm <- as.numeric(gsub(",", ".", df$F1_ppm))
  df$F2_ppm <- as.numeric(gsub(",", ".", df$F2_ppm))
  df$intensity <- as.numeric(gsub(",", ".", df$intensity))
  
  # Supprimer les lignes avec NA
  df <- df[complete.cases(df[, c("F1_ppm", "F2_ppm")]), ]
  
  return(df)
}


#===============================================================================
# ÉTAPE 1 : PRÉPARER LA LISTE DES SPECTRES RÉELS
#===============================================================================

cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║                         ÉTAPE 1 : INVENTAIRE                                 ║
║                    Lister tous les spectres à traiter                        ║
╚══════════════════════════════════════════════════════════════════════════════╝
")

# Option A : Définir manuellement la liste des spectres
spectres_info <- data.frame(
  id = character(),
  bruker_path = character(),
  peaks_csv = character(),
  type = character(),
  stringsAsFactors = FALSE
)

# Exemple de remplissage manuel :
# spectres_info <- rbind(spectres_info, data.frame(
#   id = "TOCSY_001",
#   bruker_path = "C:/Users/norouchon/Documents/Tocs/MTH_Melange_1.25mM/5/pdata/1",
#   peaks_csv = "C:/Users/norouchon/Documents/CNN_Training/annotations/TOCSY_001_peaks.csv",
#   type = "TOCSY"
# ))

# Option B : Scanner automatiquement un dossier
scan_spectres_folder <- function(base_dir, annotations_dir) {
  spectres_info <- data.frame(
    id = character(),
    bruker_path = character(),
    peaks_csv = character(),
    type = character(),
    stringsAsFactors = FALSE
  )
  
  # Scanner chaque type de spectre
  for (spec_type in c("TOCSY", "HSQC", "COSY", "UFCOSY")) {
    type_dir <- file.path(base_dir, spec_type)
    if (dir.exists(type_dir)) {
      folders <- list.dirs(type_dir, recursive = FALSE)
      
      for (folder in folders) {
        pdata_path <- file.path(folder, "pdata", "1")
        if (dir.exists(pdata_path)) {
          spec_id <- paste0(spec_type, "_", basename(folder))
          peaks_file <- file.path(annotations_dir, paste0(spec_id, "_peaks.csv"))
          
          spectres_info <- rbind(spectres_info, data.frame(
            id = spec_id,
            bruker_path = pdata_path,
            peaks_csv = peaks_file,
            type = spec_type,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  return(spectres_info)
}

# Exemple d'utilisation du scan automatique :
spectres_info <- scan_spectres_folder(
  base_dir = "C:/Users/juguibert/Documents/CNN_Training/spectres_reels",
  annotations_dir = "C:/Users/juguibert/Documents/CNN_Training/annotations"
)

cat("Nombre de spectres à traiter:", nrow(spectres_info), "\n")
print(table(spectres_info$type))


#===============================================================================
# ÉTAPE 2 : CRÉER LES FICHIERS D'ANNOTATION (si pas encore fait)
#===============================================================================

cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║                         ÉTAPE 2 : ANNOTATION                                 ║
║            Créer les fichiers CSV de pics pour chaque spectre                ║
╚══════════════════════════════════════════════════════════════════════════════╝
")

# FORMAT ATTENDU POUR LES FICHIERS CSV D'ANNOTATION :
#
# F1_ppm,F2_ppm,intensity
# 3.52,3.52,0.95
# 4.11,3.98,0.72
# 2.85,2.85,0.88
# 7.32,7.45,0.65
#
# Notes:
# - F1_ppm : position sur l'axe horizontal (1H pour tous les types)
# - F2_ppm : position sur l'axe vertical (1H pour TOCSY/COSY, 13C pour HSQC)
# - intensity : intensité relative du pic (0-1), optionnel (défaut = 1.0)

# Fonction pour créer un template d'annotation vide
create_annotation_template <- function(output_path, example_peaks = NULL) {
  if (is.null(example_peaks)) {
    # Template vide avec exemples commentés
    template <- data.frame(
      F1_ppm = numeric(0),
      F2_ppm = numeric(0),
      intensity = numeric(0)
    )
  } else {
    template <- example_peaks
  }
  
  write.csv(template, output_path, row.names = FALSE)
  cat("Template créé:", output_path, "\n")
}

# Créer les templates manquants
create_missing_templates <- function(spectres_info) {
  for (i in seq_len(nrow(spectres_info))) {
    csv_path <- spectres_info$peaks_csv[i]
    if (!file.exists(csv_path)) {
      # Créer le dossier si nécessaire
      dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)
      create_annotation_template(csv_path)
      cat("⚠️  Template vide créé pour:", spectres_info$id[i], "\n")
      cat("   -> Remplir manuellement avec les pics annotés\n")
    }
  }
}

# ASTUCE : Exporter les pics depuis ton app Shiny
# Dans ton app, après avoir validé les bounding boxes, tu peux exporter avec :
#
# export_peaks_for_training <- function(bounding_boxes, output_path) {
#   peaks_export <- bounding_boxes %>%
#     transmute(
#       F1_ppm = cx_ppm,
#       F2_ppm = cy_ppm,
#       intensity = stain_intensity / max(stain_intensity, na.rm = TRUE)
#     )
#   write.csv(peaks_export, output_path, row.names = FALSE)
# }


#===============================================================================
# ÉTAPE 3 : CHARGER ET CONVERTIR LES SPECTRES RÉELS
#===============================================================================

cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║                    ÉTAPE 3 : CHARGEMENT DES SPECTRES RÉELS                   ║
║                  Convertir chaque spectre 2D en coupes 1D                    ║
╚══════════════════════════════════════════════════════════════════════════════╝
")

# Fonction pour charger un spectre Bruker (adapte selon ta fonction read_bruker)
load_bruker_spectrum <- function(bruker_path) {
  # Utilise ta fonction read_bruker existante
  resss <- read_bruker(dir = bruker_path, dim = "2D")
  rr <- resss$spectrumData
  
  # Normalisation
  rr_abs <- abs(rr)
  rr_norm <- (rr_abs - min(rr_abs)) / (max(rr_abs) - min(rr_abs))
  
  # S'assurer que les noms de lignes/colonnes sont numériques
  rownames(rr_norm) <- as.numeric(rownames(rr))
  colnames(rr_norm) <- as.numeric(colnames(rr))
  
  return(rr_norm)
}

# Traitement de tous les spectres réels (VERSION ROBUSTE)
process_all_real_spectra <- function(spectres_info, verbose = TRUE) {
  
  #=============================================================================
  # VÉRIFICATION 1 : Exclure les spectres sans fichier d'annotation
  #=============================================================================
  missing_annotations <- !file.exists(spectres_info$peaks_csv)
  if (any(missing_annotations)) {
    cat("⚠️  Fichiers d'annotation manquants (", sum(missing_annotations), " spectres exclus):\n", sep = "")
    for (id in spectres_info$id[missing_annotations]) {
      cat("   - ", id, "\n", sep = "")
    }
    spectres_info <- spectres_info[!missing_annotations, ]
    cat("\n✓ Continuation avec", nrow(spectres_info), "spectres annotés\n\n")
  }
  
  # Vérifier qu'il reste des spectres
  
  if (nrow(spectres_info) == 0) {
    stop("❌ Aucun spectre avec annotation trouvé! Créez d'abord les fichiers CSV.")
  }
  
  #=============================================================================
  # VÉRIFICATION 2 : Exclure les annotations vides ou mal formatées
  #=============================================================================
  valid_annotations <- sapply(spectres_info$peaks_csv, function(f) {
    tryCatch({
      df <- read_csv_auto(f)
      # Vérifier qu'il y a des données et les bonnes colonnes
      has_data <- nrow(df) > 0
      has_cols <- all(c("F1_ppm", "F2_ppm") %in% names(df)) || 
        any(grepl("ppm", names(df), ignore.case = TRUE))
      return(has_data && has_cols)
    }, error = function(e) {
      return(FALSE)
    })
  })
  
  if (any(!valid_annotations)) {
    cat("⚠️  Fichiers d'annotation vides ou mal formatés (", sum(!valid_annotations), " spectres exclus):\n", sep = "")
    for (id in spectres_info$id[!valid_annotations]) {
      cat("   - ", id, "\n", sep = "")
    }
    spectres_info <- spectres_info[valid_annotations, ]
    cat("\n✓ Continuation avec", nrow(spectres_info), "spectres valides\n\n")
  }
  
  # Vérifier qu'il reste des spectres
  if (nrow(spectres_info) == 0) {
    stop("❌ Aucun spectre avec annotation valide trouvé!")
  }
  
  #=============================================================================
  # TRAITEMENT DES SPECTRES
  #=============================================================================
  
  # Grouper par type de spectre
  types <- unique(spectres_info$type)
  all_data <- list()
  
  for (spec_type in types) {
    cat(sprintf("\n📊 Traitement des spectres %s...\n", spec_type))
    
    subset_info <- spectres_info[spectres_info$type == spec_type, ]
    spectra_list <- list()
    peaks_list <- list()
    
    for (i in seq_len(nrow(subset_info))) {
      cat(sprintf("  [%d/%d] %s... ", i, nrow(subset_info), subset_info$id[i]))
      
      tryCatch({
        # Charger le spectre
        rr_norm <- load_bruker_spectrum(subset_info$bruker_path[i])
        
        # Charger les pics avec la fonction robuste
        peaks_2D <- load_2D_peaks_robust(subset_info$peaks_csv[i])
        
        spectra_list[[i]] <- rr_norm
        peaks_list[[i]] <- peaks_2D
        
        cat(sprintf("✓ (%d pics)\n", nrow(peaks_2D)))
        
      }, error = function(e) {
        cat(sprintf("✗ ERREUR: %s\n", e$message))
      })
    }
    
    # Supprimer les NULL (spectres en erreur)
    valid_idx <- !sapply(spectra_list, is.null)
    spectra_list <- spectra_list[valid_idx]
    peaks_list <- peaks_list[valid_idx]
    
    if (length(spectra_list) > 0) {
      # Combiner tous les spectres de ce type
      cat(sprintf("\n  Combinaison de %d spectres %s...\n", length(spectra_list), spec_type))
      
      data_type <- combine_multiple_2D_spectra(
        spectra_list = spectra_list,
        peaks_list = peaks_list,
        spectrum_type = spec_type
      )
      
      all_data[[spec_type]] <- data_type
    }
  }
  
  # Fusionner tous les types
  if (length(all_data) == 0) {
    stop("❌ Aucun spectre n'a pu être traité!")
  }
  
  cat("\n📦 Fusion de tous les types de spectres...\n")
  
  X_all <- do.call(rbind, lapply(all_data, function(d) d$X))
  y_class_all <- do.call(rbind, lapply(all_data, function(d) d$y_class))
  y_reg_all <- do.call(function(...) abind::abind(..., along = 1), 
                       lapply(all_data, function(d) d$y_reg))
  
  cat(sprintf("✅ Total spectres 1D réels: %d\n", nrow(X_all)))
  
  return(list(
    X = X_all,
    y_class = y_class_all,
    y_reg = y_reg_all
  ))
}

# EXÉCUTION (décommenter quand prêt)
data_real <- process_all_real_spectra(spectres_info)


#===============================================================================
# ÉTAPE 4 : GÉNÉRER LES SPECTRES SYNTHÉTIQUES
#===============================================================================

cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║                    ÉTAPE 4 : GÉNÉRATION SPECTRES SYNTHÉTIQUES                ║
║                           5000+ spectres simulés                             ║
╚══════════════════════════════════════════════════════════════════════════════╝
")

# Paramètres de génération
N_SYNTHETIC <- 5000
MIX_RATIO <- 0.5  # 50% TOCSY-like, 50% métabolites CPMG

cat(sprintf("Génération de %d spectres synthétiques (mix_ratio = %.1f)...\n", 
            N_SYNTHETIC, MIX_RATIO))
cat("⏳ Cela peut prendre plusieurs minutes...\n")

# Génération
set.seed(42)  # Pour reproductibilité

data_synthetic <- generate_spectrum_labels_full_mixed(
  n_spectra = N_SYNTHETIC,
  n_points = 2048,
  intensity_threshold = 0.2,
  center_margin = 0.2,
  seed = 42,
  mix_ratio = MIX_RATIO
)

cat(sprintf("✅ %d spectres synthétiques générés\n", nrow(data_synthetic$X)))
cat(sprintf("   - Pics labellisés: %d\n", sum(data_synthetic$y_class > 0)))


#===============================================================================
# ÉTAPE 5 : FUSIONNER RÉEL + SYNTHÉTIQUE
#===============================================================================

cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║                         ÉTAPE 5 : FUSION DES DATASETS                        ║
║                        Mélanger réel et synthétique                          ║
╚══════════════════════════════════════════════════════════════════════════════╝
")

# EXÉCUTION (décommenter quand data_real est prêt)
data_final <- merge_real_and_synthetic(
  real_data = data_real,
  synthetic_data = data_synthetic,
  shuffle = TRUE,
  seed = 42
)

# Pour tester sans données réelles :
data_final <- data_synthetic

cat(sprintf("\n📊 Dataset final:\n"))
cat(sprintf("   - Total spectres: %d\n", nrow(data_final$X)))
cat(sprintf("   - Points par spectre: %d\n", ncol(data_final$X)))
cat(sprintf("   - Pics classe 1 (forts): %d\n", sum(data_final$y_class == 1)))
cat(sprintf("   - Pics classe 2 (faibles): %d\n", sum(data_final$y_class == 2)))


#===============================================================================
# ÉTAPE 6 : PRÉPARER LES DONNÉES POUR KERAS
#===============================================================================

cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║                    ÉTAPE 6 : PRÉPARATION POUR KERAS                          ║
║                      Reshape et split train/validation                       ║
╚══════════════════════════════════════════════════════════════════════════════╝
")

n <- nrow(data_final$X)
n_points <- ncol(data_final$X)

# Reshape pour CNN 1D : [batch, timesteps, channels]
X_train <- array(data_final$X, dim = c(n, n_points, 1))
y_class_train <- data_final$y_class
y_reg_train <- data_final$y_reg

cat(sprintf("Dimensions X_train: [%d, %d, %d]\n", dim(X_train)[1], dim(X_train)[2], dim(X_train)[3]))
cat(sprintf("Dimensions y_class: [%d, %d]\n", dim(y_class_train)[1], dim(y_class_train)[2]))
cat(sprintf("Dimensions y_reg: [%d, %d, %d]\n", dim(y_reg_train)[1], dim(y_reg_train)[2], dim(y_reg_train)[3]))


#===============================================================================
# ÉTAPE 7 : CONSTRUIRE OU CHARGER LE MODÈLE
#===============================================================================

cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║                         ÉTAPE 7 : MODÈLE CNN                                 ║
║              Construire nouveau ou charger poids existants                   ║
╚══════════════════════════════════════════════════════════════════════════════╝
")

# Option A : Nouveau modèle from scratch
model <- build_peak_predictor()
cat("✅ Nouveau modèle construit\n")

# Option B : Fine-tuning à partir d'un modèle existant (décommenter si souhaité)
# model <- build_peak_predictor()
# load_model_weights_tf(model, "saved_model/weights")
# cat("✅ Poids existants chargés pour fine-tuning\n")

summary(model)


#===============================================================================
# ÉTAPE 8 : CONFIGURER L'ENTRAÎNEMENT
#===============================================================================

cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║                    ÉTAPE 8 : CONFIGURATION ENTRAÎNEMENT                      ║
║                     Callbacks, checkpoints, paramètres                       ║
╚══════════════════════════════════════════════════════════════════════════════╝
")

# Dossier pour sauvegarder les checkpoints
checkpoint_dir <- "saved_model/checkpoints"
dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

# Callbacks
callbacks_list <- list(
  # Sauvegarder le meilleur modèle
  callback_model_checkpoint(
    filepath = file.path(checkpoint_dir, "best_weights"),
    monitor = "val_loss",
    save_best_only = TRUE,
    save_weights_only = TRUE,
    verbose = 1
  ),
  
  # Early stopping si pas d'amélioration
  callback_early_stopping(
    monitor = "val_loss",
    patience = 50,
    restore_best_weights = TRUE,
    verbose = 1
  ),
  
  # Réduire le learning rate si plateau
  callback_reduce_lr_on_plateau(
    monitor = "val_loss",
    factor = 0.5,
    patience = 20,
    min_lr = 1e-7,
    verbose = 1
  )
)

# Paramètres d'entraînement
EPOCHS <- 200       # Plus d'epochs car early stopping
BATCH_SIZE <- 32
VALIDATION_SPLIT <- 0.2

cat(sprintf("Configuration:\n"))
cat(sprintf("   - Epochs max: %d\n", EPOCHS))
cat(sprintf("   - Batch size: %d\n", BATCH_SIZE))
cat(sprintf("   - Validation split: %.0f%%\n", VALIDATION_SPLIT * 100))
cat(sprintf("   - Early stopping patience: 50 epochs\n"))


#===============================================================================
# ÉTAPE 9 : ENTRAÎNER LE MODÈLE
#===============================================================================

cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║                         ÉTAPE 9 : ENTRAÎNEMENT                               ║
║                    ⏳ Cela peut prendre plusieurs heures                      ║
╚══════════════════════════════════════════════════════════════════════════════╝
")

# Heure de début
start_time <- Sys.time()
cat(sprintf("Début: %s\n\n", start_time))

# ENTRAÎNEMENT
history <- model %>% fit(
  x = X_train,
  y = list(
    class_output = y_class_train,
    reg_output = y_reg_train
  ),
  epochs = EPOCHS,
  batch_size = BATCH_SIZE,
  validation_split = VALIDATION_SPLIT,
  callbacks = callbacks_list,
  verbose = 1
)

# Heure de fin
end_time <- Sys.time()
duration <- difftime(end_time, start_time, units = "mins")
cat(sprintf("\n✅ Entraînement terminé en %.1f minutes\n", duration))


#===============================================================================
# ÉTAPE 10 : ÉVALUER ET SAUVEGARDER
#===============================================================================

cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║                    ÉTAPE 10 : ÉVALUATION ET SAUVEGARDE                       ║
╚══════════════════════════════════════════════════════════════════════════════╝
")

# Visualiser l'historique d'entraînement
plot(history)

# Sauvegarder le modèle final
final_weights_path <- "saved_model/weights_v2"
save_model_weights_tf(model, final_weights_path)
cat(sprintf("✅ Poids sauvegardés: %s\n", final_weights_path))

# Sauvegarder l'historique
saveRDS(history, "saved_model/training_history.rds")

# Résumé des performances
cat("\n📈 Résumé des performances:\n")
final_metrics <- tail(history$metrics, 1)
for (name in names(final_metrics)) {
  cat(sprintf("   - %s: %.4f\n", name, final_metrics[[name]]))
}


#===============================================================================
# ÉTAPE 11 : TESTER SUR UN SPECTRE RÉEL
#===============================================================================

cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║                         ÉTAPE 11 : TEST FINAL                                ║
║                  Vérifier sur un spectre non utilisé                         ║
╚══════════════════════════════════════════════════════════════════════════════╝
")

# Charger le nouveau modèle
test_model <- build_peak_predictor()
load_model_weights_tf(test_model, final_weights_path)

# Tester sur un spectre (adapter le chemin)
# test_spectrum <- read_bruker(dir = "chemin/vers/spectre/test/pdata/1", dim = "2D")
# results <- run_cnn_peak_picking(test_spectrum$spectrumData, test_model, params)

cat("
✅ RÉENTRAÎNEMENT TERMINÉ !

Prochaines étapes:
1. Tester le nouveau modèle sur des spectres non vus
2. Comparer avec l'ancien modèle
3. Si satisfait, remplacer 'saved_model/weights' par 'saved_model/weights_v2'
4. Mettre à jour CNN_shiny.R pour utiliser les nouveaux poids

")


#===============================================================================
# CHECKLIST RÉCAPITULATIVE
#===============================================================================

cat("
┌─────────────────────────────────────────────────────────────────────────────┐
│                           CHECKLIST                                         │
├─────────────────────────────────────────────────────────────────────────────┤
│ □ 1. Organiser les spectres Bruker dans des dossiers                        │
│ □ 2. Créer les fichiers CSV d'annotation (F1_ppm, F2_ppm, intensity)        │
│ □ 3. Remplir la variable spectres_info avec les chemins                     │
│ □ 4. Exécuter process_all_real_spectra()                                    │
│ □ 5. Vérifier que data_real contient bien les coupes 1D                     │
│ □ 6. Fusionner avec merge_real_and_synthetic()                              │
│ □ 7. Lancer l'entraînement                                                  │
│ □ 8. Évaluer les résultats                                                  │
│ □ 9. Tester sur des spectres non vus                                        │
│ □ 10. Déployer le nouveau modèle dans l'app Shiny                           │
└─────────────────────────────────────────────────────────────────────────────┘
")
