#### Integration ----


peakVolume <- function(inFile, gran = 200, c.vol = FALSE, baselineCorr = FALSE) {
    
    NewFile <- inFile
    
  if (inFile$file.par$number_dimensions > 1) {
    if (!c.vol) {
      # Sum of visible data
      volume <- sum(inFile$data[inFile$data >= inFile$file.par$noise_est * inFile$graphics.par$clevel])
      if (length(volume) == 0) volume <- NA
    } else {
      # Generate contour lines for data
      zlim <- c(inFile$file.par$noise_est * inFile$graphics.par$clevel, max(inFile$data, na.rm = TRUE))

      # Ensure zlim values are finite
      if (any(!is.finite(zlim))) {
        warning("Non-finite values in zlim. Check noise_est, clevel, or data range.")
        return(NA)
      }
      
      c.levels <- seq(zlim[1], zlim[2], length.out = gran)
      c.int <- diff(c.levels[1:2])
      
      print("non")
      
      z = as.matrix(NewFile$data)
      
      print(z)
      
      contour.file <- contourLines(z = z, levels = c.levels)
      
      # Estimate volume as stacked ellipsoids
      if (length(contour.file) > 0 && zlim[1] < zlim[2]) {
        volume <- NULL
        for (i in seq_along(contour.file)) {
          volume <- sum(volume, (4 / 3 * pi * diff(range(contour.file[[i]]$x)) *
                                   diff(range(contour.file[[i]]$y))*c.int))
        }
      } else {
        volume <- NA
      }
    }
  } else {
    # Area estimate for 1D data
    if (baselineCorr) inFile$data <- inFile$data - fivenum(inFile$data)[2]
    volume <- sum(inFile$data, na.rm = TRUE)
  }
  
  return(volume)
}


# Function to calculate volumes for each peak
calculate_peak_volumes <- function(bruker_data, peaks, gran = 200, c.vol = TRUE) {
  if (is.null(bruker_data$rr_data)) {
    stop("2D data (rr_data) is required for volume calculation but is not available.")
  }
  
  # Prepare inFile structure for peakVolume function
  inFile <- list(
    data = bruker_data$rr_data,
    file.par = list(
      number_dimensions = 2,  # Assuming 2D NMR spectrum
      bruker_data$rr_data[!is.finite(bruker_data$rr_data)] <- 0,
      
      finite_max = max(bruker_data$rr_data[is.finite(bruker_data$rr_data)], na.rm = TRUE),
      finite_min = min(bruker_data$rr_data[is.finite(bruker_data$rr_data)], na.rm = TRUE),
      
      # Replace Inf with the maximum finite value
      bruker_data$rr_data[bruker_data$rr_data == Inf] <- finite_max,
      
      # Replace -Inf with the minimum finite value
      bruker_data$rr_data[bruker_data$rr_data == -Inf] <- finite_min, 
      noise_est = median(bruker_data$rr_data[bruker_data$rr_data > 0]) / 10  # Exclude zero or negative values
    ),
    graphics.par = list(
      clevel = 1  # Default contour level multiplier
    )
  )
  
  
  # Define ROIs around each peak (adjust the size as needed)
  # Generate ROIs with dynamic size based on resolution
  resolution_f1 <- abs(diff(bruker_data$ppm_x))[1]
  resolution_f2 <- abs(diff(bruker_data$ppm_y))[1]
  size_multiplier <- 5  # Number of resolution steps to include
  
  roi_list <- apply(peaks, 1, function(peak) {
    dynamic_size_f1 <- resolution_f1 * size_multiplier
    dynamic_size_f2 <- resolution_f2 * size_multiplier
    list(
      x = c(peak["ppm_f2"] - dynamic_size_f1, peak["ppm_f2"] + dynamic_size_f1),
      y = c(peak["ppm_f1"] - dynamic_size_f2, peak["ppm_f1"] + dynamic_size_f2))
  })
  
  # Debug: Store roi_list globally for inspection
  assign("roi_list_debug", roi_list, envir = .GlobalEnv)
  
  subsetROI <- function(inFile, roi) {
    # Clamp ROI ppm values to valid ranges
    roi$x[1] <- max(min(bruker_data$ppm_y), min(roi$x[2]))
    roi$x[2] <- min(max(bruker_data$ppm_y), max(roi$x[1]))
    roi$y[1] <- max(min(bruker_data$ppm_x), min(roi$y[2]))
    roi$y[2] <- min(max(bruker_data$ppm_x), max(roi$y[1]))
    
    # Convert ROI ppm values to matrix indices
    x_min <- which.min(abs(bruker_data$ppm_x - roi$x[1]))
    x_max <- which.min(abs(bruker_data$ppm_x - roi$x[2]))
    y_min <- which.min(abs(bruker_data$ppm_y - roi$y[1]))
    y_max <- which.min(abs(bruker_data$ppm_y - roi$y[2]))
    
    # Ensure valid ranges
    if (x_min == x_max) {
      if (x_min > 1) {
        x_min <- x_min - 1
      } else if (x_max < 256) {
        x_max <- x_max + 1
      } else {
        stop("Cannot expand ROI in x-direction.")
      }
    }
    
    if (y_min == y_max) {
      if (y_min > 1) {
        y_min <- y_min - 1
      } else if (y_max < 2048) {
        y_max <- y_max + 1
      } else {
        stop("Cannot expand ROI in y-direction.")
      }
    }
    
    # Subset the data
    subset_data <- inFile$data[y_min:y_max, x_min:x_max]
    
    # Validate the subset dimensions
    if (nrow(subset_data) < 2 || ncol(subset_data) < 2) {
      stop("Subset matrix must have at least 2 rows and 2 columns.")
    }
    
    # Debugging output
    print(paste("ROI indices: x_min =", x_min, "x_max =", x_max, 
                "y_min =", y_min, "y_max =", y_max))
    print(dim(subset_data))
    
    # Return modified inFile with subset data
    inFile_subset <- inFile
    inFile_subset$data <- subset_data
    return(inFile_subset)
  }
  
  
  list_roi <- list()  # Initialize an empty list to store ROI details
  
  # Calculate volumes for each ROI
  volumes <- sapply(roi_list, function(roi) {
    roi_data <- subsetROI(inFile, roi)
    print(summary(roi_data$data))  # Debug: Check data summary for each ROI
    peakVolume(roi_data, gran = gran, c.vol = c.vol)
  })

  
  volumesAUC <- sapply(roi_list, function(roi) {
    roi_data <- subsetROI(inFile, roi)
    print(summary(roi_data$data))  # Debug: Check data summary for each ROI
    sum(pmax(roi_data$data - inFile$file.par$noise_est, 0), na.rm = TRUE)
    })
  
  # Combine volumes with peak data
  peaks$Volume <- volumes
  peaks$VolumeAUC <- volumesAUC
  
  return(peaks)
}

# Calculate peak volumes
peak_volumes <- calculate_peak_volumes(bruker_data, peaks, gran = 200, c.vol = TRUE)

# Display results
print(peak_volumes)