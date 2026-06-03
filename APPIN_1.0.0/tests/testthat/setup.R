# tests/testthat/setup.R
# =============================================================================
# Chargé automatiquement par testthat AVANT tous les fichiers test-*.R
# (au même titre que les helper-*.R).
#
# Rôle : exposer une racine projet fiable (APPIN_ROOT) quelle que soit la
# machine — poste Windows local OU runner GitHub Actions — pour que les
# `source(file.path(APPIN_ROOT, "Function/..."))` de tes tests fonctionnent
# partout sans modification.
# =============================================================================

# Détecte la racine du projet en remontant depuis le dossier courant jusqu'à
# trouver un dossier contenant à la fois Function/ et R/.
.find_appin_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in seq_len(6)) {
    if (dir.exists(file.path(path, "Function")) &&
        dir.exists(file.path(path, "R"))) {
      return(path)
    }
    parent <- dirname(path)
    if (identical(parent, path)) break  # racine du FS atteinte
    path <- parent
  }
  # Fallback : un cran au-dessus de tests/testthat/
  normalizePath("..", winslash = "/", mustWork = FALSE)
}

# Ne définit APPIN_ROOT que s'il n'est pas déjà fixé dans l'environnement,
# pour ne pas écraser un override manuel en local.
if (!exists("APPIN_ROOT", envir = globalenv())) {
  assign("APPIN_ROOT", .find_appin_root(), envir = globalenv())
}

# Dossier des fixtures (données réelles Bruker, modèles, snapshots).
# Les tests qui en dépendent doivent skip si le dossier est absent.
if (!exists("FIXTURES_DIR", envir = globalenv())) {
  assign(
    "FIXTURES_DIR",
    file.path(get("APPIN_ROOT", envir = globalenv()), "tests", "fixtures"),
    envir = globalenv()
  )
}

# Helper réutilisable : skip un test si une fixture donnée n'est pas présente
# (utile en CI où les gros fichiers binaires ne sont pas committés).
skip_if_no_fixture <- function(relative_path) {
  full <- file.path(get("FIXTURES_DIR", envir = globalenv()), relative_path)
  if (!file.exists(full) && !dir.exists(full)) {
    testthat::skip(sprintf("Fixture absente : %s", relative_path))
  }
  invisible(full)
}

# =============================================================================
# CHARGEMENT DU CODE MÉTIER
# =============================================================================
# Certains fichiers test-*.R appellent directement des fonctions de
# Function/*.R sans les sourcer eux-mêmes (ex. detect_local_maxima,
# pseudo_voigt_2d, fit_2d_peak...). En local ça marche car les fonctions
# sont déjà chargées dans la session ; en CI la session est vierge.
# On source donc tout le code métier ici, une fois pour toutes, dans
# l'environnement global pour qu'il soit visible de tous les tests.

.appin_root <- get("APPIN_ROOT", envir = globalenv())

# Ordre important : les fonctions de base d'abord, puis ce qui en dépend.
.source_files <- c(
  "Function/Read_2DNMR_spectrum.R",
  "Function/Vizualisation.R",
  "Function/Peak_picking.R",
  "Function/Peak_fitting.R",
  "Function/CNN_shiny.R",   # optionnel (CNN) ; ignoré s'il référence keras au chargement
  "R/utils.R"
)

for (f in .source_files) {
  full <- file.path(.appin_root, f)
  if (!file.exists(full)) {
    message("setup.R : fichier source absent, ignoré -> ", f)
    next
  }
  ok <- tryCatch({
    sys.source(full, envir = globalenv())
    TRUE
  }, error = function(e) {
    # On n'interrompt PAS la suite : un fichier optionnel (ex. CNN tirant
    # keras au chargement) ne doit pas faire échouer toute la suite.
    message("setup.R : échec du source de ", f, " -> ", conditionMessage(e))
    FALSE
  })
  if (ok) message("setup.R : sourcé -> ", f)
}

message("setup.R : APPIN_ROOT = ", .appin_root)
