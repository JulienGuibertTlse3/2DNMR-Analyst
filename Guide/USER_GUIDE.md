<div align="center">

# 🧪 2DNMR-Analyst

### Interactive 2D NMR Spectroscopy Analysis Tool

**User Guide v2.0**

[![R](https://img.shields.io/badge/R-≥4.0-blue.svg)](https://cran.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-App-green.svg)](https://shiny.posit.co/)
[![License](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

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
| 🎯 **Smart Peak Detection** | Local maxima with DBSCAN clustering + CNN neural network |
| 🧠 **CNN Detection** | Deep learning-based peak detection for complex spectra |
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
├── run_app.R                 # 🚀 Entry point - run this file
├── Shine.R                   # Main application
│
├── R/                        # Shiny modules
│   ├── utils.R
│   ├── mod_load_data.R
│   ├── mod_peak_picking.R    # Local Max + CNN detection
│   ├── mod_manual_editing.R
│   ├── mod_integration.R
│   └── mod_save_export.R
│
├── Function/                 # Core functions
│   ├── Read_2DNMR_spectrum.R # Bruker data reading
│   ├── Vizualisation.R       # Visualization & clustering
│   ├── Peak_picking.R        # Peak detection algorithms
│   ├── Peak_fitting.R        # Gaussian/Voigt fitting
│   │
│   ├── CNN_shiny.R           # 🧠 CNN entry point (sources modules)
│   ├── CNN_model.R           # CNN architecture & weights loading
│   ├── CNN_detection.R       # Peak detection (batch + sequential)
│   ├── CNN_filtering.R       # Peak filtering functions
│   ├── CNN_clustering.R      # DBSCAN clustering & bounding boxes
│   └── CNN_main.R            # Main CNN pipeline function
│
├── saved_model/              # 🧠 CNN pre-trained weights
│   └── weights/
│
└── www/                      # Web assets
    ├── styles.css
    └── plotly_ticks.js
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

| Method | Description | Best For |
|--------|-------------|----------|
| **Local Max** | Detects local maxima with optional DBSCAN clustering | Fast detection, well-resolved spectra |
| **CNN** | Convolutional neural network for intelligent peak detection | Complex spectra, overlapping peaks |

---

#### 🔬 Local Max Method

Uses traditional local maximum detection with optional DBSCAN clustering to group nearby peaks.

| Option | Effect |
|--------|--------|
| **No clustering** | Each local maximum becomes a separate peak |
| **Epsilon (eps)** | Maximum distance between points in a cluster |
| **Delete ranges** | Remove peaks from specified ppm regions |

**Recommended epsilon values:**
- TOCSY/HSQC/COSY: `0.0068`
- UFCOSY: `0.014`

---

#### 🧠 CNN Method (Neural Network)

The CNN (Convolutional Neural Network) method uses a trained deep learning model to detect peaks more intelligently than traditional methods. It's particularly effective for:

- Spectra with overlapping peaks
- Low signal-to-noise conditions
- Complex multiplet patterns
- TOCSY spectra with t1 noise artifacts

##### CNN Parameters

| Parameter | Description | Default |
|-----------|-------------|---------|
| **Prediction threshold** | Minimum CNN confidence to detect a peak (0-1) | 0.3 |
| **Trace filter** | Removes t1 noise artifacts along F2 lines (% of line max) | 50% |

##### How CNN Detection Works

1. **Spectrum Normalization** — The spectrum is normalized using the 99.9th percentile
2. **Sliding Window Scan** — The CNN scans rows and columns using a sliding window approach
3. **Peak Classification** — Each point is classified as: background, peak edge, or peak center
4. **DBSCAN Clustering** — Detected points are grouped into peak clusters
5. **Bounding Box Generation** — Each cluster gets a bounding box with padding

##### CNN Tips

| Goal | Action |
|------|--------|
| Detect more peaks | ↓ Lower prediction threshold (e.g., 0.2) |
| Reduce false positives | ↑ Higher prediction threshold (e.g., 0.5) |
| Remove t1 noise traces | ↑ Higher trace filter (e.g., 70%) |
| Keep weak correlations | ↓ Lower trace filter (e.g., 30%) |

##### Progress Indicator

When running CNN detection, a progress bar displays the current step:
1. Preparing spectrum (5%)
2. Running neural network (20%)
3. Clustering contour data (45%)
4. Filtering by CNN detections (70%)
5. Updating plot (90%)
6. Complete (100%)

> 💡 **Note:** CNN detection may take longer than Local Max but typically produces better results on complex spectra.

---

#### Epsilon Guidelines (Both Methods)

| Epsilon | Result |
|---------|--------|
| ↑ Higher | More peaks grouped together |
| ↓ Lower | More individual peaks detected |

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
| **CNN model not loaded** | Verify `saved_model/weights` folder exists |
| **CNN too slow** | Normal for large spectra; progress bar shows status |
| **CNN finds no peaks** | Lower prediction threshold (e.g., 0.2) |
| **CNN too many false positives** | Increase prediction threshold and trace filter |
| **CNN t1 noise artifacts** | Increase trace filter to 60-80% |

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