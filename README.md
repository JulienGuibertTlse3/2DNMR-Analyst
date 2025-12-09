<p align="center">
  <img src="2DNMR-Analyst/www/spin.png" alt="SPIN Logo" width="200"/>
</p>

<h1 align="center">SPIN</h1>

<p align="center">
  <strong>S</strong>harp <strong>P</strong>eak and <strong>I</strong>dentification for 2D <strong>N</strong>MR
</p>

<p align="center">
  <img src="https://img.shields.io/badge/R-Shiny-blue?logo=r" alt="R Shiny"/>
  <img src="https://img.shields.io/badge/NMR-2D%20Analysis-green" alt="NMR 2D"/>
  <img src="https://img.shields.io/badge/License-MIT-yellow" alt="License"/>
</p>

---

## 📋 Overview

**SPIN** is an interactive R Shiny application for analyzing 2D NMR spectra. It provides automated peak detection, manual editing tools, and batch processing capabilities for metabolomics research.

---

## 🚀 Quick Start

1. **Load** → Select your Bruker data folder
2. **Plot** → Generate contour plots
3. **Pick** → Detect peaks automatically
4. **Edit** → Refine boxes manually if needed
5. **Export** → Save results to CSV

---

## ✨ Features

### 📂 Data Loading
- Load Bruker 2D NMR data (ser/fid files)
- Batch processing of multiple spectra
- Select specific spectra to analyze
- Automatic detection of valid datasets

### 📈 Visualization
- Interactive contour plots (zoom, pan)
- Adjustable intensity threshold
- Click to get coordinates
- Real-time display of peaks and boxes

### 🎯 Peak Detection
- **Local Max method:** Local maxima + DBSCAN clustering
- **CNN method:** Deep learning detection
- Automatic bounding box generation
- Configurable clustering parameters

### ✏️ Manual Editing
- Add boxes by clicking (two-click mode)
- Move and resize existing boxes
- Delete unwanted peaks/boxes
- Fuse multiple peaks into one

---

## 🧪 Supported Spectrum Types

| Type | Description |
|------|-------------|
| **TOCSY** | Total Correlation Spectroscopy |
| **HSQC** | Heteronuclear Single Quantum Coherence |
| **COSY** | Correlation Spectroscopy |
| **UFCOSY** | Ultra-Fast COSY |

---

## 📖 Detailed Workflow

### Step 1: Load Data
Select a folder containing Bruker NMR data. The tool will automatically detect valid 2D spectra (folders containing `acqus` and `ser` or `fid` files). You can select which spectra to load using checkboxes.

### Step 2: Generate Plot
Choose the spectrum type and adjust the intensity threshold. Click **Auto** to calculate an optimal threshold based on noise level or maximum intensity. Then click **Generate Plot** to create the contour visualization.

### Step 3: Peak Picking
- **Local Max method:** Uses local maxima detection followed by DBSCAN clustering to group nearby points. Adjust `epsilon` to control cluster size.
- **CNN method:** Uses a trained convolutional neural network for peak detection. Better for complex or overlapping peaks.

### Step 4: Manual Editing
- **Add boxes:** Enable "Two clicks" mode, then click two opposite corners on the spectrum.
- **Edit boxes:** Select a box in the Data tab, then use arrow buttons to move or +/- to resize.
- **Fuse peaks:** Use the lasso tool to select multiple peaks, then click "Fuse".

### Step 5: Export Results
- **Peaks:** Export peak positions (F1, F2 coordinates) and intensities.
- **Boxes:** Export bounding box coordinates and integrated intensities.
- **Batch Export:** Apply the same boxes to all loaded spectra and export intensities for each.

---

## 💡 Tips

- Start with a QC sample or the most intense spectrum to optimize parameters
- Use "No clustering" option if you do not want to group multiplets
- Increase epsilon value to decrease size of cluster and get more solo peaks (e.g., TOCSY)
- Click "Apply" to confirm changes before exporting
- Use the "Data" tab to review and select boxes for editing
- For batch processing: select the folder, process only a QC or the most intense spectrum first (Step 1), then reload all spectra and use "Batch Export" with the peaks selected from the first spectrum

---

## 📄 Output Format

### Peaks CSV
```
stain_id, F2_ppm, F1_ppm, stain_intensity
```

### Boxes CSV
```
stain_id, xmin, xmax, ymin, ymax, stain_intensity
```

---

## 🛠️ Installation

### Prerequisites
- R (>= 4.0)
- RStudio (recommended)

### Download

- Option A : Direcly download it as a ZIP folder.
      Click on the "Code" button and select Download ZIP
  
- Option B : Cloning with git : git clone https://github.com/JulienGuibertTlse3/2DNMR-Analyst.git

### Launch the app

- Open RStudio
- Open run_app.R file
- Click on Source button or press Ctrl+Shift+Enter. The script will download anything necessary by itself.

## 📝 License

This project is licensed under the GNU GENERAL PUBLIC LICENSE

---

## 📧 Contact

For questions or bug reports, please contact the development team or open an issue on GitHub.

---

<p align="center">
  Developed for metabolomics research
</p>
