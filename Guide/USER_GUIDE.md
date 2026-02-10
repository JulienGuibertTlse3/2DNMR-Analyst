<div align="center">

# 🧪 2DNMR-Analyst

### Interactive 2D NMR Spectroscopy Analysis Tool

**User Guide v2.0**

[![R](https://img.shields.io/badge/R-≥4.0-blue.svg)](https://cran.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-App-green.svg)](https://shiny.posit.co/)

*A comprehensive solution for metabolomics 2D NMR data analysis*

[Getting Started](#-getting-started) •
[Features](#-features) •
[Workflow](#-workflow) •
[Troubleshooting](#-troubleshooting)

---

</div>

## 📖 Overview

**2DNMR-Analyst** is a powerful Shiny application designed for loading, visualizing, processing, and exporting 2D NMR spectra from Bruker instruments. It provides a complete interactive interface optimized for metabolomics research.

### ✨ Key Features

| Feature | Description |
|---------|-------------|
| 🔬 **Multi-format Support** | TOCSY, COSY, HSQC, and UFCOSY experiments |
| 🎯 **Smart Peak Detection** | Local maxima with optional DBSCAN clustering |
| ✏️ **Interactive Editing** | Manual creation, modification, and fusion of peaks/boxes |
| 📊 **Advanced Integration** | Sum, Gaussian, and Voigt fitting methods |
| 📈 **Quality Metrics** | R² analysis and residuals visualization |
| 💾 **Flexible I/O** | Import/export peaks, boxes, and full sessions |
| ⚡ **Batch Processing** | Analyze multiple spectra simultaneously |

---

## 🚀 Getting Started

### Prerequisites

Before installing 2DNMR-Analyst, ensure you have:

- **R** (version ≥ 4.0) — [Download from CRAN](https://cran.r-project.org/)
- **RStudio** (recommended) — [Download RStudio Desktop](https://posit.co/download/rstudio-desktop/)

### Installation

#### Option A: Direct Download

1. Visit the [GitHub repository](https://github.com/JulienGuibertTlse3/2DNMR-Analyst)
2. Click the green **"Code"** button
3. Select **"Download ZIP"**
4. Extract the archive to your preferred location

#### Option B: Git Clone

```bash
git clone https://github.com/JulienGuibertTlse3/2DNMR-Analyst.git
cd 2DNMR-Analyst
```

### Launching the Application

1. Open **RStudio**
2. Open the `run_app.R` file
3. Click **"Source"** or press `Ctrl+Shift+Enter`

> 💡 **Note:** The script automatically installs all required packages on first run.

---

## 🗂️ Project Structure

```
2DNMR-Analyst/
│
├── Shine.R                    # Main application (~2063 lines)
│                              # Contains UI + main Server
│                              # Initializes and connects modules
│
├── run_app.R                  # Entry point
│                              # Auto-installation of missing packages
│                              # Launches shinyApp()
│
├── README.md                  # User documentation
│
├── R/                         # ═══ SHINY MODULES ═══
│   ├── utils.R                # Shared utility functions
│   │                          # - parse_keep_peak_ranges()
│   │                          # - %||% operator (null coalescing)
│   │
│   ├── mod_load_data.R        # Module: Data loading (~200 lines)
│   │                          # - Bruker directory selection
│   │                          # - Spectra listing and selection
│   │                          # - Read cache
│   │
│   ├── mod_peak_picking.R     # Module: Peak detection (~274 lines)
│   │                          # - Local Max + DBSCAN
│   │                          # - Clustering options
│   │                          # - Exclusion zones
│   │
│   ├── mod_manual_editing.R   # Wrapper module: Manual editing (~122 lines)
│   │   ├── mod_click_mode.R   # - Submodule: Click modes (~201 lines)
│   │   ├── mod_box_editor.R   # - Submodule: Box editing (~348 lines)
│   │   ├── mod_manual_add.R   # - Submodule: Manual addition (~163 lines)
│   │   ├── mod_fusion.R       # - Submodule: Peak fusion (~166 lines)
│   │   └── mod_pending_changes.R # - Submodule: Apply/Discard (~431 lines)
│   │
│   ├── mod_integration.R      # Module: Integration (~345 lines)
│   │                          # - Sum method (AUC)
│   │                          # - Gaussian/Voigt fitting
│   │                          # - R² threshold and fallback
│   │
│   ├── mod_save_export.R      # Wrapper module: Save/Export (~110 lines)
│   │   ├── mod_session.R      # - Submodule: Save/Load session (~238 lines)
│   │   ├── mod_import.R       # - Submodule: CSV import (~178 lines)
│   │   ├── mod_export.R       # - Submodule: CSV/batch export (~177 lines)
│   │   └── mod_reset.R        # - Submodule: Reset all (~121 lines)
│
├── Function/                  # ═══ BUSINESS FUNCTIONS ═══
│   ├── Read_2DNMR_spectrum.R  # Bruker file reading (165 lines)
│   │                          # - read_bruker()
│   │                          # - Parsing procs, proc2s, acqu
│   │
│   ├── Vizualisation.R        # Graphics + DBSCAN (224 lines)
│   │                          # - find_nmr_peak_centroids_optimized()
│   │                          # - process_nmr_centroids()
│   │                          # - make_bbox_outline()
│   │
│   ├── Peak_picking.R         # Local maxima detection (819 lines)
│   │                          # - peak_pick_2d_nt2()
│   │                          # - filter_noise_peaks()
│   │                          # - Filters by spectrum type
│   │
│   └── Peak_fitting.R         # 2D fitting (570 lines)
│                              # - fit_2d_peak()
│                              # - detect_local_maxima()
│                              # - pseudo_voigt_2d()
│                              # - calculate_fitted_volumes()
│
├── www/                       # ═══ WEB ASSETS ═══
│   ├── styles.css             # Externalized CSS styles (~200 lines)
│   │                          # - Accordion classes
│   │                          # - Info-box styles
│   │                          # - Responsive design
│   │
│   └── plotly_ticks.js        # Custom JavaScript (~150 lines)
│                              # - generateNiceTicks()
│                              # - updateTicksOnZoom()
│                              # - Shiny <-> JS communication
│
└── tests/                     # ═══ UNIT TESTS ═══
    ├── testthat/              # Tests with testthat (76 tests)
    │   ├── test-read_bruker.R # - Bruker reading tests
    │   ├── test-threshold.R   # - Noise thresholding tests
    │   ├── test-peak_fitting.R # - Voigt/Gaussian fitting tests
    │   ├── test-peak_picking.R # - Peak detection tests
    │   ├── test-visualization.R # - Visualization tests
    │   └── test-utils.R       # - Utility function tests
    ├── run_tests.R            # Main execution script
    └── README_TESTS.md        # Test documentation
```

---

## 🧭 Application Layout

The application consists of two main tabs:

| Tab | Purpose |
|-----|---------|
| **📖 Guide** | Introduction, documentation, and contextual help |
| **📊 Analysis** | Main workspace for spectrum visualization and analysis |

The **Analysis** tab is organized with:
- **Left Panel** — Control sections (collapsible accordions)
- **Right Panel** — Visualization tabs (Spectrum, Data, Pending, Fit Quality, Batch)

---

## 📋 Workflow

### Step 1: Load Data

<details>
<summary><b>📂 1. Load Data</b></summary>

#### Bruker File Requirements

Your Bruker data must follow this structure:
```
/<experiment_folder>/pdata/1/
    ├── 2rr        # Processed data
    ├── procs      # Processing parameters (F2)
    ├── proc2s     # Processing parameters (F1)
    └── ...
```

#### Loading Steps

1. Navigate to the **Analysis** tab
2. Expand **"1. Load Data"** section
3. Click **"Select Folder"**
4. Browse to your Bruker experiment folder
5. Check the spectra you want to analyze
6. Click **"Load Selected"**

> 💡 **Tip:** Use "Load All" for quick selection of all available spectra.

</details>

---

### Step 2: Configure Plot

<details>
<summary><b>📈 2. Plot Settings</b></summary>

#### Spectrum Types

| Type | Description |
|------|-------------|
| **TOCSY** | Homonuclear correlations through multiple bonds |
| **COSY** | Direct homonuclear correlations (2-3 bonds) |
| **HSQC** | Heteronuclear ¹H-¹³C single-bond correlations |
| **UFCOSY** | Ultrafast COSY acquisition |

#### Parameters

| Parameter | Description |
|-----------|-------------|
| **Threshold** | Minimum intensity for contour display |
| **Auto** | Automatically calculates optimal threshold |
| **Advanced** | Fine-tune noise threshold and contour settings |

Click **"Generate Plot"** to display the spectrum.

</details>

---

### Step 3: Peak Detection

<details>
<summary><b>🎯 3. Peak Picking</b></summary>

#### Detection Methods

| Method | Description |
|--------|-------------|
| **Local Max** | Detects local maxima with optional DBSCAN clustering |
| **CNN** | Convolutional neural network detection (if available) |

#### Clustering Options

| Option | Effect |
|--------|--------|
| **No clustering** | Each local maximum becomes a separate peak |
| **Epsilon (eps)** | Maximum distance between points in a cluster |
| **Delete ranges** | Remove peaks from specified ppm regions |

#### Epsilon Guidelines

| Epsilon | Result |
|---------|--------|
| ↑ Higher | More peaks grouped together |
| ↓ Lower | More individual peaks detected |

**Recommended values:**
- TOCSY/HSQC/COSY: `0.0068`
- UFCOSY: `0.014`

</details>

---

### Step 4: Manual Editing

<details>
<summary><b>✏️ 4. Manual Editing</b></summary>

#### 🖱️ Click Modes

| Mode | Action |
|------|--------|
| **Off** | Clicks have no effect |
| **Add box (2 clicks)** | Click two corners to create a bounding box |
| **Delete box on click** | Click inside a box to mark for deletion |

#### 🔗 Fusing Peaks

1. Go to the **Data** tab
2. Select multiple peaks/boxes using `Ctrl+Click`
3. Return to **Manual Editing**
4. Click **"Fuse Selected"**

#### 📦 Edit Selected Box

| Control | Function |
|---------|----------|
| **Coordinates** | Manually enter xmin, xmax, ymin, ymax |
| **Arrow buttons** | Move box (↑ ↓ ← →) |
| **+/- buttons** | Expand or shrink box size |
| **Step** | Movement increment in ppm |

#### ➕ Add Manually

- **Peak:** Enter F2 (ppm) and F1 (ppm) coordinates
- **Box:** Enter all four boundary coordinates

> ⚠️ **Important:** Click **"Apply"** to confirm changes or **"Discard"** to cancel.

</details>

---

### Step 5: Integration

<details>
<summary><b>📐 5. Integration</b></summary>

#### Integration Methods

| Method | Description | Best For |
|--------|-------------|----------|
| **Sum (AUC)** | Direct summation of intensities | Fast analysis, robust |
| **Gaussian** | 2D Gaussian peak fitting | Symmetric peaks |
| **Voigt** | Gaussian + Lorentzian profile | Asymmetric peaks |

#### Fitting Options

| Option | Description |
|--------|-------------|
| **Include R² in export** | Add quality metrics to output |
| **Min R² threshold** | Fallback to Sum if fit quality is below threshold |

Click **"Run Integration"** to process all boxes.

</details>

---

### Step 6: Quality Analysis

<details>
<summary><b>📊 Fit Quality Tab</b></summary>

When using Gaussian or Voigt fitting, the **Fit Quality** tab provides:

| Section | Content |
|---------|---------|
| **Fit Summary** | Statistics grouped by fitting method |
| **Fitted Boxes** | Sortable table with R² for each box |
| **R² Distribution** | Histogram of fit quality |
| **2D Fit View** | Visual representation of selected fit |
| **Residuals** | Distribution of fitting residuals |

#### R² Interpretation Guide

| R² Value | Quality | Recommendation |
|----------|---------|----------------|
| **> 0.9** | ✅ Excellent | Peak is well-defined |
| **0.7 - 0.9** | ⚠️ Good | Acceptable for quantification |
| **< 0.7** | ❌ Poor | Use Sum method or adjust box |

</details>

---

### Step 7: Save & Export

<details>
<summary><b>💾 6. Save & Export</b></summary>

#### Session Management

| Action | Description |
|--------|-------------|
| **Save** | Save complete session as `.rds` file |
| **Load** | Restore a previously saved session |

#### Import Options

| Format | Content |
|--------|---------|
| **Peaks CSV** | Peak coordinates (stain_id, F2_ppm, F1_ppm) |
| **Boxes CSV** | Box boundaries (stain_id, xmin, xmax, ymin, ymax) |

#### Export Options

| Export | Description |
|--------|-------------|
| **Peaks** | Export centroids to CSV |
| **Boxes** | Export bounding boxes to CSV |
| **Batch Export** | Project boxes onto all spectra and export intensities |

#### CSV File Formats

**Peaks:**
```csv
stain_id;F2_ppm;F1_ppm;Volume
peak1;1.234;3.456;123456
```

**Boxes:**
```csv
stain_id;xmin;xmax;ymin;ymax
peak1;1.200;1.268;3.400;3.512
```

> 💡 **Batch Processing Limits:** For optimal performance, limit to 25 spectra for TOCSY, or 50 for COSY/HSQC.

</details>

---

## 💡 Tips & Best Practices

### Workflow Optimization

1. **Start with QC samples** — Optimize parameters on your most intense spectrum before batch analysis

2. **Use Auto threshold** — Let the algorithm find optimal contour levels

3. **Check Pending tab** — Always review modifications before clicking Apply

4. **Validate with Fit Quality** — When using Gaussian/Voigt, check R² values

### Parameter Tuning

| Goal | Action |
|------|--------|
| Group more peaks | ↑ Increase epsilon |
| Separate overlapping peaks | ↓ Decrease epsilon |
| Reduce noise | ↑ Increase threshold |
| Detect weak signals | ↓ Decrease threshold |

---

## 🔧 Troubleshooting

| Problem | Solution |
|---------|----------|
| **No spectrum detected** | Verify Bruker folder structure (`pdata/1/`) |
| **Empty or error plot** | Adjust threshold or use Auto |
| **Too many/few peaks** | Adjust epsilon and intensity threshold |
| **Import fails** | Check CSV format (separator `;` or `,`) |
| **Application slow** | Reduce number of contours in Advanced settings |
| **Poor fit quality** | Use Sum method or adjust box boundaries |
| **Session won't load** | Ensure spectral data is available |

---

## 📚 Additional Resources

- **GitHub Repository:** [JulienGuibertTlse3/2DNMR-Analyst](https://github.com/JulienGuibertTlse3/2DNMR-Analyst)
- **Issue Tracker:** Report bugs and request features on GitHub

---

<div align="center">

## 📬 Contact

**Author:** Julien Guibert  
**Email:** julien.guibert@inrae.fr  

**Project Maintainer:**  Marie TREMBLAY-FRANCO

**Email:**  marie.tremblay-franco@inrae.fr

**Institution:** INRAe Toxalim / MetaboHUB

---

*2DNMR-Analyst v2.0 — Developed for metabolomics research*

*Last updated: February 2026*

</div>
