### Integration ----

peakVolume <- function(inFile, gran = 200, c.vol = FALSE, baselineCorr = FALSE) {
  if (!inherits(inFile, "matrix")) {
    stop("Input file must be a matrix with ppm values as row and column names.")
  }
  
  NewFile <- inFile
  
  if (!c.vol) {
    # Sum of visible data above threshold
    noise_est <- median(NewFile[NewFile > 0]) / 10
    volume <- sum(NewFile[NewFile >= noise_est], na.rm = TRUE)
    if (length(volume) == 0) volume <- NA
  } else {
    # Generate contour levels
    zlim <- c(median(NewFile[NewFile > 0]) / 10, max(NewFile, na.rm = TRUE))
    
    if (any(!is.finite(zlim))) {
      warning("Non-finite values in zlim. Check data range.")
      return(NA)
    }
    
    c.levels <- seq(zlim[1], zlim[2], length.out = gran)
    c.int <- diff(c.levels[1:2])
    
    contour.file <- contourLines(z = NewFile, levels = c.levels)
    
    if (length(contour.file) > 0) {
      volume <- sum(sapply(contour.file, function(cont) {
        (4 / 3 * pi * diff(range(cont$x)) * diff(range(cont$y)) * c.int)
      }))
    } else {
      volume <- NA
    }
  }
  
  return(volume)
}

calculate_peak_volumes <- function(spectrum, peaks, gran = 200, c.vol = TRUE) {
  if (!inherits(spectrum, "matrix")) {
    stop("Spectrum must be a matrix with ppm values as row and column names.")
  }
  
  ppm_F1 <- as.numeric(rownames(spectrum))
  ppm_F2 <- as.numeric(colnames(spectrum))
  
  resolution_f1 <- abs(diff(ppm_F1))[1]
  resolution_f2 <- abs(diff(ppm_F2))[1]
  size_multiplier <- 5
  
  roi_list <- apply(peaks, 1, function(peak) {
    list(
      x = c(peak["F2_ppm_auto"] - resolution_f2 * size_multiplier, peak["F2_ppm_auto"] + resolution_f2 * size_multiplier),
      y = c(peak["F1_ppm_auto"] - resolution_f1 * size_multiplier, peak["F1_ppm_auto"] + resolution_f1 * size_multiplier)
    )
  })
  
  subsetROI <- function(data, roi) {
    x_min <- which.min(abs(ppm_F2 - roi$x[1]))
    x_max <- which.min(abs(ppm_F2 - roi$x[2]))
    y_min <- which.min(abs(ppm_F1 - roi$y[1]))
    y_max <- which.min(abs(ppm_F1 - roi$y[2]))
    
    subset_data <- data[y_min:y_max, x_min:x_max]
    if (nrow(subset_data) < 2 || ncol(subset_data) < 2) {
      stop("Subset matrix must have at least 2 rows and 2 columns.")
    }
    
    return(subset_data)
  }
  
  volumes <- sapply(roi_list, function(roi) {
    roi_data <- subsetROI(spectrum, roi)
    peakVolume(roi_data, gran = gran, c.vol = c.vol)
  })
  
  volumesAUC <- sapply(roi_list, function(roi) {
    roi_data <- subsetROI(spectrum, roi)
    noise_est <- median(roi_data[roi_data > 0]) / 10
    sum(pmax(roi_data - noise_est, 0), na.rm = TRUE)
  })
  
  peaks$Volume <- volumes
  peaks$VolumeAUC <- volumesAUC
  return(peaks)
}


# # Calculate peak volumes
# peak_volumes <- calculate_peak_volumes(bruker_data, peaks, gran = 200, c.vol = TRUE)
# 
# # Display results
# print(peak_volumes)