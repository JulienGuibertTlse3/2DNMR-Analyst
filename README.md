<p align="center">
  <img src="docs/images/logo.png" alt="2DNMR-Analyst Logo" width="200"/>
</p>

<h1 align="center">2DNMR-Analyst</h1>

<p align="center">
  <strong>S</strong>harp <strong>P</strong>eak <strong>I</strong>dentification for <strong>2D</strong> NMR (SPI2D)
</p>

<p align="center">
  <a href="https://www.r-project.org/"><img src="https://img.shields.io/badge/R-%3E%3D%204.0-blue?logo=r&logoColor=white" alt="R Version"/></a>
  <a href="https://shiny.posit.co/"><img src="https://img.shields.io/badge/Shiny-Interactive-blue?logo=r&logoColor=white" alt="R Shiny"/></a>
  <a href="#-supported-spectrum-types"><img src="https://img.shields.io/badge/NMR-2D%20Analysis-green" alt="NMR 2D"/></a>
  <a href="#-supported-spectrum-types"><img src="https://img.shields.io/badge/MetaboHub-blue" alt="MTH"/></a>
  <a href="LICENSE.txt"><img src="https://img.shields.io/badge/License-CeCILL--2.1-yellow" alt="License CeCILL-2.1"/></a>
</p>

<p align="center">
  <a href="#-quick-start">Quick Start</a> •
  <a href="#-features">Features</a> •
  <a href="#-installation">Installation</a> •
  <a href="#-documentation">Documentation</a> •
  <a href="#-license">License</a>
</p>

---

## 📋 Overview

**2DNMR-Analyst** is an interactive R Shiny application designed for analyzing 2D NMR spectra. It provides automated peak detection, manual editing tools, and batch processing capabilities tailored for metabolomics research.

Developed at **INRAe Toxalim / MetaboHUB**, this tool streamlines the workflow from raw Bruker data to quantitative peak integration.

---

## 🚀 Quick Start

```
1. Load    → Select your Bruker data folder
2. Plot    → Generate contour plots  
3. Pick    → Detect peaks automatically
4. Edit    → Refine boxes manually if needed
5. Integrate → Calculate volumes (Sum or Fitting)
6. Export  → Save results to CSV or session
```

---

## ✨ Features

### 📂 Data Loading
- Load Bruker 2D NMR data (`ser`/`fid` files)
- Batch processing of multiple spectra
- Select specific spectra to analyze
- Automatic detection of valid datasets

### 📈 Visualization
- Interactive contour plots (zoom, pan)
- Adjustable intensity threshold
- Click-to-get coordinates
- Real-time display of peaks and bounding boxes

### 🎯 Peak Detection
| Method | Description |
|--------|-------------|
| **Local Max** | Local maxima detection + DBSCAN clustering |
| **CNN** | Deep learning-based detection (optional) |

- Automatic bounding box generation
- Configurable clustering parameters (epsilon, min points)

### ✏️ Manual Editing
- Add/remove boxes by clicking (two-click mode)
- Move and resize existing boxes with arrow controls
- Delete unwanted peaks or boxes
- Fuse multiple peaks into one

### 📐 Integration & Peak Fitting
| Method | Description |
|--------|-------------|
| **Sum** | Direct sum of intensities within box |
| **Gaussian** | 2D Gaussian peak fitting |
| **Voigt** | Pseudo-Voigt model (Gaussian-Lorentzian convolution) |

- Dedicated **Fit Quality** tab with R² metrics
- 2D fit visualization for each box
- Residuals analysis

### 💾 Save & Export
- **Session**: Complete save/load in `.rds` format (peaks, boxes, parameters)
- **Import**: CSV files for peaks and boxes
- **Export**: CSV with semicolon separator (`;`), batch export for multiple spectra
- **Pending system**: Apply or discard changes before export

---

## 🧪 Supported Spectrum Types

| Type | Description | Typical Use |
|------|-------------|-------------|
| **TOCSY** | Total Correlation Spectroscopy | ¹H-¹H correlations through bonds |
| **HSQC** | Heteronuclear Single Quantum Coherence | ¹H-¹³C direct correlations |
| **COSY** | Correlation Spectroscopy | ¹H-¹H vicinal couplings |
| **UFCOSY** | Ultra-Fast COSY | Rapid ¹H-¹H correlations |

---

## 📖 Detailed Workflow

### Step 1: Load Data
Select a folder containing Bruker NMR data. The tool automatically detects valid 2D spectra (folders containing `acqus` and `ser` or `fid` files). Use checkboxes to select which spectra to load.

### Step 2: Generate Plot
1. Choose the spectrum type (TOCSY, HSQC, COSY, UFCOSY)
2. Adjust the intensity threshold (or click **Auto** for automatic calculation)
3. Click **Generate Plot** to create the contour visualization

### Step 3: Peak Picking
- **Local Max method**: Uses local maxima detection followed by DBSCAN clustering. Adjust `epsilon` to control cluster size.
- **CNN method**: Uses a trained convolutional neural network for complex or overlapping peaks.

### Step 4: Manual Editing
- **Add boxes**: Enable "Two clicks" mode, then click two opposite corners
- **Edit boxes**: Select a box in the Data tab, use arrow buttons to move or +/- to resize
- **Fuse peaks**: Use lasso tool to select multiple peaks, then click "Fuse"

### Step 5: Integration
- **Direct (Sum)**: Sum of all intensities within bounding box
- **Peak Fitting**: Gaussian or Voigt model fitting with quality metrics

### Step 6: Save & Export
Three collapsible sections available:
- 💼 **Session**: Complete save/load in `.rds`
- 📥 **Import**: CSV files for peaks and boxes
- 📤 **Export**: CSV export, Batch Export for multiple spectra

---

## 💡 Tips & Best Practices

| Tip | Description |
|-----|-------------|
| 🎯 **Start with QC** | Use a QC sample or most intense spectrum first to optimize parameters |
| 🔧 **No clustering** | Disable clustering if you don't want to group multiplets |
| 📊 **Epsilon tuning** | Increase epsilon to get smaller clusters and more individual peaks |
| ✅ **Apply changes** | Always click "Apply" to confirm changes before exporting |
| 📋 **Data tab** | Use the Data tab to review and select boxes for editing |
| 🔄 **Batch workflow** | Process QC first, then reload all spectra and use "Batch Export" |
| ⚠️ **Batch limits** | Limit batches to ~25 spectra for TOCSY, ~50 for COSY/HSQC |

---

## 📄 Output Format

### Peaks CSV
```csv
stain_id;F2_ppm;F1_ppm;stain_intensity
peak1;3.456;1.234;123456
peak2;4.567;2.345;234567
```

### Boxes CSV
```csv
stain_id;xmin;xmax;ymin;ymax;stain_intensity
box1;3.400;3.500;1.200;1.300;123456
box2;4.500;4.600;2.300;2.400;234567
```

---

## 🛠️ Installation

### Prerequisites
- **R** (>= 4.0)
- **RStudio** (recommended)

### Option A: Download ZIP
1. Click on the **Code** button on GitHub
2. Select **Download ZIP**
3. Extract the archive

### Option B: Clone with Git
```bash
git clone https://github.com/JulienGuibertTlse3/2DNMR-Analyst.git
cd 2DNMR-Analyst
```

### Launch the Application
1. Open **RStudio**
2. Open the `run_app.R` file
3. Click **Source** or press `Ctrl+Shift+Enter`

> 💡 The script will automatically install all required packages on first run.

---

## 📁 Project Structure

```
2DNMR-Analyst/
├── run_app.R                 # Application launcher
├── Shine.R                   # Main Shiny application
├── Function/
│   ├── Read_2DNMR_spectrum.R # Bruker data reader
│   ├── Visualization.R       # Plot generation
│   ├── Peak_picking.R        # Peak detection algorithms
│   ├── Peak_fitting.R        # Gaussian/Voigt fitting
│   └── CNN_shiny.R           # CNN-based detection (optional)
├── docs/
│   ├── USER_GUIDE.md         # User documentation
│   └── DEVELOPER_GUIDE.md    # Developer documentation
├── LICENSE.txt               # CeCILL-2.1 license
└── README.md                 # This file
```

---

## 📚 Documentation

- [User Guide](docs/USER_GUIDE.md) - Detailed usage instructions
- [Developer Guide](docs/DEVELOPER_GUIDE.md) - Technical documentation

---

## 📝 License

This project is distributed under the **CeCILL-2.1** license (compatible with GNU-GPL).

See the [LICENSE.txt](LICENSE.txt) file for details.

---
## 👥 Authors

- **Julien Guibert** - *Development* - INRAe Toxalim / MetaboHUB

---

## 📧 Contact

For questions, bug reports, or feature requests:
- Open an [issue on GitHub](https://github.com/JulienGuibertTlse3/2DNMR-Analyst/issues)
- Contact the development team at INRAe Toxalim

---

## 🙏 Acknowledgments

- [MetaboHUB](https://www.metabohub.fr/) - French National Infrastructure for Metabolomics
- [INRAe Toxalim](https://toxalim.toulouse.hub.inrae.fr/) - Research unit

---

<p align="center">
  <a href="https://www.metabohub.fr/">
    <img src="docs/img/metabohub_logo.png" alt="MetaboHUB" height="60"/>
  </a>
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;   <!-- 10 espaces -->
  <a href="https://toxalim.toulouse.hub.inrae.fr/">
    <img src="docs/img/inrae_toxalim.png" alt="INRAe Toxalim" height="60"/>
  </a>
</p>

<p align="center">
  <em>Developed for metabolomics research</em>
</p>
