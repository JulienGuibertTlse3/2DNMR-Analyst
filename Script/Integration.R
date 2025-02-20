### Integration ----
library(pracma) # For numerical integration

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
      x = c(peak["F2_ppm"] - resolution_f2 * size_multiplier, peak["F2_ppm"] + resolution_f2 * size_multiplier),
      y = c(peak["F1_ppm"] - resolution_f1 * size_multiplier, peak["F1_ppm"] + resolution_f1 * size_multiplier)
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



### NEW TEST ----

library(pracma)
library(dbscan)

# Function to estimate peak width dynamically per peak
find_peak_width_per_peak <- function(data, peak_pos, axis = "F1") {
  if (!inherits(data, "matrix")) {
    stop("Input must be a matrix.")
  }
  
  dim_index <- ifelse(axis == "F1", 1, 2)
  profile <- apply(data, dim_index, max, na.rm = TRUE)  # Extract max values along axis
  
  # Ensure profile is finite and valid
  profile <- ifelse(is.finite(profile), profile, 0)
  
  # Find closest peak index
  if (is.na(peak_pos) || length(profile) == 0) {
    return(5)  # Safe fallback width
  }
  
  peak_idx <- which.min(abs(profile - peak_pos))
  if (length(peak_idx) == 0 || is.na(peak_idx)) {
    return(5)  # Safe fallback width
  }
  
  peak_max <- profile[peak_idx]
  half_max <- peak_max / 2
  
  # Find the left and right half-max crossing points
  left_idx <- suppressWarnings(max(which(profile[1:peak_idx] < half_max), na.rm = TRUE))
  right_idx <- suppressWarnings(min(which(profile[peak_idx:length(profile)] < half_max), na.rm = TRUE) + peak_idx - 1)
  
  # Validate indices
  if (!is.finite(left_idx) || !is.finite(right_idx) || left_idx >= right_idx || is.na(left_idx) || is.na(right_idx)) {
    return(5)  # Return a safe fallback width
  }
  
  return(max(abs(right_idx - left_idx), 2))  # Ensure a minimum width of 2
}

# Function to calculate peak volumes with non-overlapping ROIs
calculate_peak_volumes <- function(spectrum, peaks, gran = 200, c.vol = TRUE, min_separation = 0.05) {
  if (!inherits(spectrum, "matrix")) {
    stop("Spectrum must be a matrix with ppm values as row and column names.")
  }
  
  ppm_F1 <- as.numeric(rownames(spectrum))
  ppm_F2 <- as.numeric(colnames(spectrum))
  
  roi_list <- apply(peaks, 1, function(peak) {
    width_F1 <- max(find_peak_width_per_peak(spectrum, peak["F1_ppm_auto"], axis = "F1"), 2)
    width_F2 <- max(find_peak_width_per_peak(spectrum, peak["F2_ppm_auto"], axis = "F2"), 2)
    
    # Ensure ROIs do not overlap too much by limiting proximity
    width_F1 <- min(width_F1, min_separation * diff(range(ppm_F1)))
    width_F2 <- min(width_F2, min_separation * diff(range(ppm_F2)))
    
    list(
      x = c(max(min(peak["F2_ppm_auto"] - width_F2, max(ppm_F2)), min(ppm_F2)),
            max(min(peak["F2_ppm_auto"] + width_F2, max(ppm_F2)), min(ppm_F2))),
      y = c(max(min(peak["F1_ppm_auto"] - width_F1, max(ppm_F1)), min(ppm_F1)),
            max(min(peak["F1_ppm_auto"] + width_F1, max(ppm_F1)), min(ppm_F1)))
    )
  })
  
  subsetROI <- function(data, roi) {
    x_min <- which.min(abs(ppm_F1 - roi$y[1]))
    x_max <- which.min(abs(ppm_F1 - roi$y[2]))
    y_min <- which.min(abs(ppm_F2 - roi$x[1]))
    y_max <- which.min(abs(ppm_F2 - roi$x[2]))
    
    if (length(x_min) == 0 || length(x_max) == 0 || length(y_min) == 0 || length(y_max) == 0 ||
        is.na(x_min) || is.na(x_max) || is.na(y_min) || is.na(y_max)) {
      stop("Error in ROI selection: indices are empty or out of range.")
    }
    
    subset_data <- data[x_min:x_max, y_min:y_max]
    if (nrow(subset_data) < 2 || ncol(subset_data) < 2) {
      stop("Subset matrix must have at least 2 rows and 2 columns.")
    }
    return(subset_data)
  }
  
  volumes <- vapply(roi_list, function(roi) {
    roi_data <- subsetROI(spectrum, roi)
    peakVolume(roi_data, gran = gran, c.vol = c.vol)
  }, numeric(1))
  
  volumesAUC <- vapply(roi_list, function(roi) {
    roi_data <- subsetROI(spectrum, roi)
    noise_est <- median(roi_data[roi_data > 0]) / 10
    sum(pmax(roi_data - noise_est, 0), na.rm = TRUE)
  }, numeric(1))
  
  peaks$Volume <- volumes
  peaks$VolumeAUC <- volumesAUC
  return(peaks)
}



# # Calculate peak volumes
# peak_volumes <- calculate_peak_volumes(bruker_data, peaks, gran = 200, c.vol = TRUE)
# 
# # Display results
# print(peak_volumes)