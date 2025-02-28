#### Peak pick R ----

peak_pick_2d_nt <- function(bruker_data, threshold = 5, threshold_type = "percentage") {
  # Ensure input is a matrix or dataframe with row and column names
  if (is.null(rownames(bruker_data)) || is.null(colnames(bruker_data))) {
    stop("The input object must have row names (F2 ppm) and column names (F1 ppm).")
  }
  
  # Convert row and column names to numeric ppm scales
  ppm_x <- as.numeric(rownames(bruker_data))  # F2 ppm
  ppm_y <- as.numeric(colnames(bruker_data))  # F1 ppm
  
  # Determine threshold based on user selection
  if (threshold_type == "percentage") {
    max_intensity <- max(bruker_data, na.rm = TRUE)
    threshold_value <- (threshold / 100) * max_intensity  # Threshold as percentage of max intensity
  } else if (threshold_type == "noise") {
    noise_level <- sd(bruker_data[bruker_data < quantile(bruker_data, 0.1, na.rm = TRUE)], na.rm = TRUE)
    threshold_value <- threshold * noise_level  # Threshold as multiple of noise level
  } else {
    stop("Invalid threshold_type. Choose 'percentage' or 'noise'.")
  }
  
  # Find significant points above the threshold
  significant_points <- which(bruker_data >= threshold_value, arr.ind = TRUE)
  
  # Handle case where no points meet threshold
  if (nrow(significant_points) == 0) {
    return(data.frame(F2_ppm = numeric(0), F1_ppm = numeric(0), Intensity = numeric(0)))
  }
  
  # Identify peaks (local maxima)
  is_peak <- apply(significant_points, 1, function(idx) {
    x <- idx[1]
    y <- idx[2]
    
    # Extract neighboring values
    x_range <- max(1, x - 1):min(nrow(bruker_data), x + 1)
    y_range <- max(1, y - 1):min(ncol(bruker_data), y + 1)
    
    neighbors <- bruker_data[x_range, y_range]
    
    # Check if the current point is a local maximum
    bruker_data[x, y] == max(neighbors, na.rm = TRUE) && 
      bruker_data[x, y] > threshold_value
  })
  
  # Extract peak positions
  peaks <- significant_points[is_peak, , drop = FALSE]
  
  # Handle case where no peaks are found
  if (nrow(peaks) == 0) {
    return(data.frame(F2_ppm = numeric(0), F1_ppm = numeric(0), Intensity = numeric(0)))
  }
  
  # Compile results into a dataframe
  peak_list <- data.frame(
    F2_ppm = ppm_x[peaks[, 1]],  # Convert row indices to ppm values
    F1_ppm = ppm_y[peaks[, 2]],  # Convert column indices to ppm values
    Intensity = bruker_data[peaks]
  )
  
  return(peak_list)
}

#### Main function for now ----

library(dplyr)
library(zoo)
library(matrixStats)
library(minpack.lm)  # For Gaussian fitting

peak_pick_2d_nt2 <- function(bruker_data, threshold = 5, threshold_type = "percentage", neighborhood_size = 11
                             , prominence_factor = 0.02, adaptive_peak_threshold = 0.000001) {
  # Ensure input is a matrix or dataframe with row names and column names
  if (is.null(rownames(bruker_data)) || is.null(colnames(bruker_data))) {
    stop("The input object must have row names (F2 ppm) and column names (F1 ppm).")
  }
  
  # Convert row and column names to numeric ppm scales
  ppm_x <- as.numeric(rownames(bruker_data))  # F2 ppm
  ppm_y <- as.numeric(colnames(bruker_data))  # F1 ppm
  
  # Determine threshold based on user selection
  if (threshold_type == "percentage") {
    max_intensity <- max(bruker_data, na.rm = TRUE)
    threshold_value <- (threshold / 100) * max_intensity  # Threshold as percentage of max intensity
  } else if (threshold_type == "noise") {
    noise_region <- bruker_data[bruker_data < quantile(bruker_data, 0.1, na.rm = TRUE)]
    median_noise <- median(noise_region, na.rm = TRUE)
    mad_noise <- mad(noise_region, na.rm = TRUE)  # Median Absolute Deviation
    threshold_value <- median_noise + threshold * mad_noise  # Adaptive MAD-based threshold
  } else {
    stop("Invalid threshold_type. Choose 'percentage' or 'noise'.")
  }
  
  # Find significant points above the threshold
  mask <- bruker_data >= threshold_value
  if (!any(mask)) {
    return(data.frame(F2_ppm = numeric(0), F1_ppm = numeric(0), Intensity = numeric(0)))
  }
  
  # Extract significant points indices
  significant_points <- which(mask, arr.ind = TRUE)
  peak_values <- bruker_data[mask]
  
  # Vectorized local maxima detection (neighborhood_size x neighborhood_size window)
  is_peak <- apply(significant_points, 1, function(idx) {
    x <- idx[1]
    y <- idx[2]
    
    x_range <- max(1, x - floor(neighborhood_size / 2)):min(nrow(bruker_data), x + floor(neighborhood_size / 2))
    y_range <- max(1, y - floor(neighborhood_size / 2)):min(ncol(bruker_data), y + floor(neighborhood_size / 2))
    
    neighbors <- as.vector(bruker_data[x_range, y_range])  # Ensure neighbors is a vector
    median_neighbors <- median(neighbors, na.rm = TRUE)
    mad_neighbors <- mad(neighbors, na.rm = TRUE)
    intensity <- bruker_data[x, y]
    
    # Adaptive prominence threshold based on local median + MAD
    prominence_threshold <- prominence_factor * (median_neighbors + mad_neighbors)
    
    # Relaxed Local Maxima Condition: Allowing Top X% Peaks
    sorted_neighbors <- sort(neighbors, decreasing = TRUE, na.last = NA)  # Remove NA values
    top_index <- ceiling(adaptive_peak_threshold * length(sorted_neighbors))
    top_threshold <- ifelse(top_index > 0, sorted_neighbors[top_index], sorted_neighbors[1])
    
    is_local_max <- intensity >= top_threshold  # Instead of strict max, allow within top X%
    prominence <- intensity - median_neighbors  # Peak prominence
    
    is_local_max && prominence > prominence_threshold
  })
  
  # Extract peak positions
  peaks <- significant_points[is_peak, , drop = FALSE]
  
  if (nrow(peaks) == 0) {
    return(data.frame(F2_ppm = numeric(0), F1_ppm = numeric(0), Intensity = numeric(0)))
  }
  
  # Compile results into a dataframe
  peak_list <- data.frame(
    F2_ppm = ppm_x[peaks[, 1]],  # Convert row indices to ppm values
    F1_ppm = ppm_y[peaks[, 2]],  # Convert column indices to ppm values
    Intensity = bruker_data[peaks]
  )
  
  # Sort peaks by intensity (descending order)
  peak_list <- peak_list[order(-peak_list$Intensity), ]
  
  return(peak_list)
}
# Run the peak picking on the spectrumData
result <- peak_pick_2d_nt2(bruker_data$spectrumData, threshold = 0.5, threshold_type = "percentage")

# Check the results
result$p1  # x (direct) positions of peaks
result$p2  # y (indirect) positions of peaks
result$intensities  # Intensities of the detected peaks



#### Confidence ----

confidence_pp <- function(bruker_data, threshold = 5, threshold_type = "percentage", neighborhood_size = 11, prominence_factor = 0.02, adaptive_peak_threshold = 0.000001, w_intensity = 0.3, w_prominence = 0.25, w_density = 0.15, w_snr = 0.1, w_symmetry = 0.1, w_isolation = 0.1) {
  # Ensure input is a matrix or dataframe with row names and column names
  if (is.null(rownames(bruker_data)) || is.null(colnames(bruker_data))) {
    stop("The input object must have row names (F2 ppm) and column names (F1 ppm).")
  }
  
  # Convert row and column names to numeric ppm scales
  ppm_x <- as.numeric(rownames(bruker_data))  # F2 ppm
  ppm_y <- as.numeric(colnames(bruker_data))  # F1 ppm
  
  # Determine threshold based on user selection
  if (threshold_type == "percentage") {
    max_intensity <- max(bruker_data, na.rm = TRUE)
    threshold_value <- (threshold / 100) * max_intensity  # Threshold as percentage of max intensity
  } else if (threshold_type == "noise") {
    noise_region <- bruker_data[bruker_data < quantile(bruker_data, 0.1, na.rm = TRUE)]
    median_noise <- median(noise_region, na.rm = TRUE)
    mad_noise <- mad(noise_region, na.rm = TRUE)  # Median Absolute Deviation
    threshold_value <- median_noise + threshold * mad_noise  # Adaptive MAD-based threshold
  } else {
    stop("Invalid threshold_type. Choose 'percentage' or 'noise'.")
  }
  
  # Find significant points above the threshold
  mask <- bruker_data >= threshold_value
  if (!any(mask)) {
    return(data.frame(F2_ppm = numeric(0), F1_ppm = numeric(0), Intensity = numeric(0), Confidence = numeric(0)))
  }
  
  # Extract significant points indices
  significant_points <- which(mask, arr.ind = TRUE)
  peak_values <- bruker_data[mask]
  
  # Compute Gaussian parameters for intensity
  peak_mean <- mean(peak_values, na.rm = TRUE)
  peak_sd <- sd(peak_values, na.rm = TRUE)
  
  # Compute prominence values for detected peaks
  prominence_values <- peak_values - median(peak_values, na.rm = TRUE)
  max_prominence <- max(prominence_values, na.rm = TRUE)
  max_prominence <- ifelse(max_prominence > 0, max_prominence, 1)  # Prevent division by zero
  
  # Vectorized local maxima detection (neighborhood_size x neighborhood_size window)
  confidence_scores <- numeric(nrow(significant_points))
  is_peak <- logical(nrow(significant_points))
  
  for (i in seq_len(nrow(significant_points))) {
    x <- significant_points[i, 1]
    y <- significant_points[i, 2]
    
    x_range <- max(1, x - floor(neighborhood_size / 2)):min(nrow(bruker_data), x + floor(neighborhood_size / 2))
    y_range <- max(1, y - floor(neighborhood_size / 2)):min(ncol(bruker_data), y + floor(neighborhood_size / 2))
    
    neighbors <- as.vector(bruker_data[x_range, y_range])  # Ensure neighbors is a vector
    median_neighbors <- median(neighbors, na.rm = TRUE)
    mad_neighbors <- mad(neighbors, na.rm = TRUE)
    intensity <- bruker_data[x, y]
    
    # Adaptive prominence threshold based on local median + MAD
    prominence_threshold <- prominence_factor * (median_neighbors + mad_neighbors)
    
    # Relaxed Local Maxima Condition: Allowing Top X% Peaks
    sorted_neighbors <- sort(neighbors, decreasing = TRUE, na.last = NA)  # Remove NA values
    top_index <- ceiling(adaptive_peak_threshold * length(sorted_neighbors))
    top_threshold <- ifelse(top_index > 0, sorted_neighbors[top_index], sorted_neighbors[1])
    
    is_local_max <- intensity >= top_threshold  # Instead of strict max, allow within top X%
    prominence <- intensity - median_neighbors  # Peak prominence
    
    # Compute Signal-to-Noise Ratio (SNR)
    snr <- ifelse(mad_neighbors > 0, intensity / mad_neighbors, 0)
    snr_score <- min(snr / 10, 1)  # Normalize (assuming max useful SNR = 10)
    
    # Compute Peak Symmetry Score (based on skewness)
    if (length(neighbors) > 3) {
      symmetry_score <- 1 - abs(mean(neighbors) - median(neighbors)) / mad_neighbors  # Normalize
    } else {
      symmetry_score <- 0.5  # Default to mid confidence if not enough data
    }
    
    # Compute Isolation Score
    local_peak_count <- sum(neighbors >= threshold_value)
    max_density <- neighborhood_size * neighborhood_size
    isolation_score <- 1 - (local_peak_count / max_density)
    
    # Gaussian Likelihood Score (penalizing outliers)
    gauss_confidence <- exp(-0.5 * ((intensity - peak_mean) / peak_sd)^2)
    
    # Compute final confidence score with new metrics
    confidence_scores[i] <- w_intensity * (intensity / max_intensity) +
      w_prominence * (prominence / max_prominence) +
      w_density * (local_peak_count / max_density) +
      w_snr * snr_score +
      w_symmetry * symmetry_score +
      w_isolation * isolation_score +
      w_prominence * gauss_confidence
    
    is_peak[i] <- is_local_max && prominence > prominence_threshold
  }
  
  # Extract peak positions
  peak_indices <- which(is_peak)
  peaks <- significant_points[peak_indices, , drop = FALSE]
  confidence_scores <- confidence_scores[peak_indices]
  
  if (nrow(peaks) == 0) {
    return(data.frame(F2_ppm = numeric(0), F1_ppm = numeric(0), Intensity = numeric(0), Confidence = numeric(0)))
  }
  
  # Normalize confidence scores to 0-1 scale safely
  confidence_min <- min(confidence_scores, na.rm = TRUE)
  confidence_max <- max(confidence_scores, na.rm = TRUE)
  if (confidence_max > confidence_min) {
    confidence_scores <- (confidence_scores - confidence_min) / (confidence_max - confidence_min)
  } else {
    confidence_scores <- rep(0.5, length(confidence_scores))  # If no variation, assign neutral confidence
  }
  
  # Compile results into a dataframe
  peak_list <- data.frame(
    F2_ppm = ppm_x[peaks[, 1]],  # Convert row indices to ppm values
    F1_ppm = ppm_y[peaks[, 2]],  # Convert column indices to ppm values
    Intensity = bruker_data[peaks],
    Confidence = confidence_scores  # Confidence scores correctly assigned
  )
  
  # Sort peaks by intensity (descending order)
  peak_list <- peak_list[order(-peak_list$Intensity), ]
  
  return(peak_list)
}


#### Db scan Test ----

library(dbscan)  # Load clustering library
library(matrixStats)  # Fast matrix computations
library(EBImage)  # For fast 2D filtering
library(data.table)  # Optimized dataframe handling
library(ggplot2)
library(plotly)

peak_pick_2d_hybrid <- function(bruker_data, threshold = 6.75, threshold_type = "percentage", neighborhood_size = 11, prominence_factor = 0.05, hdbscan_minPts = 3, adaptive_peak_threshold = 0.000001, initial_threshold_factor = 1, plot_results = TRUE) {
  # Ensure input has valid row and column names
  if (is.null(rownames(bruker_data)) || is.null(colnames(bruker_data))) {
    stop("The input object must have row names (F2 ppm) and column names (F1 ppm).")
  }
  
  # Convert row and column names to numeric ppm scales
  ppm_x <- as.numeric(rownames(bruker_data))  # F2 ppm
  ppm_y <- as.numeric(colnames(bruker_data))  # F1 ppm
  
  # Determine lower initial threshold
  initial_threshold_value <- if (threshold_type == "percentage") {
    max(bruker_data, na.rm = TRUE) * (initial_threshold_factor * threshold / 100)
  } else if (threshold_type == "noise") {
    noise_region <- bruker_data[bruker_data < quantile(bruker_data, 0.1, na.rm = TRUE)]
    median(noise_region, na.rm = TRUE) + (initial_threshold_factor * threshold) * mad(noise_region, na.rm = TRUE)
  } else {
    stop("Invalid threshold_type. Choose 'percentage' or 'noise'.")
  }
  
  # Extract potential peaks using lower threshold
  significant_points <- which(bruker_data >= initial_threshold_value, arr.ind = TRUE)
  if (nrow(significant_points) == 0) {
    return(data.frame(F2_ppm = numeric(0), F1_ppm = numeric(0), Intensity = numeric(0)))
  }
  
  # Convert significant points to data.table for memory efficiency
  candidate_peaks <- data.table(
    F2_ppm = ppm_x[significant_points[, 1]],
    F1_ppm = ppm_y[significant_points[, 2]],
    Intensity = bruker_data[significant_points]
  )
  
  # Apply HDBSCAN clustering
  hdbscan_result <- hdbscan(candidate_peaks[, .(F2_ppm, F1_ppm)], minPts = hdbscan_minPts)
  candidate_peaks[, cluster := hdbscan_result$cluster]
  
  # Keep clustered peaks
  clustered_candidates <- candidate_peaks[cluster > 0]
  
  # Process multiplets: Group clustered peaks into single multiplet peaks
  multiplet_peaks <- clustered_candidates[, .(
    F2_min = min(F2_ppm),
    F2_max = max(F2_ppm),
    F1_min = min(F1_ppm),
    F1_max = max(F1_ppm),
    Center_F2 = mean(c(min(F2_ppm), max(F2_ppm))),
    Center_F1 = mean(c(min(F1_ppm), max(F1_ppm))),
    Intensity = sum(Intensity)
  ), by = cluster]
  
  # Ensure multiplets do not share peaks
  multiplet_peaks[, `:=`(unique_id = .I)]
  unique_multiplets <- multiplet_peaks[!duplicated(F2_min) & !duplicated(F2_max) & !duplicated(F1_min) & !duplicated(F1_max)]
  
  # Extract isolated peaks (singlets)
  isolated_peaks <- candidate_peaks[cluster == 0, .(
    Center_F2 = F2_ppm,
    Center_F1 = F1_ppm,
    Intensity = Intensity,
    F2_min = F2_ppm,
    F2_max = F2_ppm,
    F1_min = F1_ppm,
    F1_max = F1_ppm
  )]
  
  # Combine multiplets and singlets
  final_peaks <- rbind(unique_multiplets, isolated_peaks, fill = TRUE)
  
  # Sort by intensity
  setorder(final_peaks, -Intensity)
  
  cat("Initial candidates detected:", nrow(candidate_peaks), "| Multiplets after clustering:", nrow(unique_multiplets), "| Singlets detected:", nrow(isolated_peaks), "\n")
  
  # Plot results if requested
  if (plot_results) {
    static_plot <- ggplot() +
      geom_point(data = candidate_peaks, aes(x = F2_ppm, y = F1_ppm), color = "grey", alpha = 0.5, size = 0.7) +
      geom_rect(data = unique_multiplets, mapping = aes(xmin = F2_min - 0.02, xmax = F2_max + 0.02, ymin = F1_min - 0.02, ymax = F1_max + 0.02),
                color = "black", fill = NA, linetype = "dashed", inherit.aes = FALSE) +
      scale_x_reverse() +
      scale_y_reverse() +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = "Selected Multiplets and Peaks", x = "F2 ppm", y = "F1 ppm")
    
    interactive_plot <- ggplotly(static_plot)  # Convert to interactive plot
    print(interactive_plot)  # Display interactive plot
  }
  
  return(final_peaks[, .(Center_F2, Center_F1, Intensity, F2_min, F2_max, F1_min, F1_max)])
}


result_db <- peak_pick_2d_hybrid(bruker_data$spectrumData, threshold = 2, threshold_type = "percentage", neighborhood_size = 11, prominence_factor = 0.05, hdbscan_minPts = 3, adaptive_peak_threshold = 0.000001, initial_threshold_factor = 1, plot_results = TRUE)

#### Function to visualize detected peaks ----

plot_peak_tt <- function(bruker_data, peak_list) {
  ggplot() +
    geom_tile(data = as.data.frame(as.table(bruker_data)), aes(x = as.numeric(Var1), y = as.numeric(Var2), fill = Freq)) +
    scale_fill_gradient(low = "white", high = "blue") +
    geom_point(data = peak_list, aes(x = F2_ppm, y = F1_ppm), color = "red", size = 2) +
    labs(title = "2D NMR Spectrum with Detected Peaks", x = "F2 ppm", y = "F1 ppm") +
    theme_minimal()
}


#### C++ Peak pick -----

# Load the Rcpp package
library(Rcpp)

cppFunction('
#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace Rcpp;

// Function to compute standard deviation of a vector
double compute_sd(std::vector<double> values) {
  int n = values.size();
  if (n < 2) return 0.0;

  double mean = std::accumulate(values.begin(), values.end(), 0.0) / n;
  double sum_sq = 0.0;

  for (double v : values) {
    sum_sq += (v - mean) * (v - mean);
  }

  return std::sqrt(sum_sq / (n - 1));
}

// [[Rcpp::export]]
DataFrame peak_pick_2d_nt(NumericMatrix bruker_data, NumericVector ppm_x, NumericVector ppm_y, 
                           double threshold, std::string threshold_type) {
  
  int rows = bruker_data.nrow();
  int cols = bruker_data.ncol();
  double threshold_value;
  
  // Determine threshold
  if (threshold_type == "percentage") {
    double max_intensity = max(bruker_data);
    threshold_value = (threshold / 100.0) * max_intensity;
  } else if (threshold_type == "noise") {
    std::vector<double> values;
    for (int i = 0; i < rows; i++) {
      for (int j = 0; j < cols; j++) {
        if (R_finite(bruker_data(i, j))) values.push_back(bruker_data(i, j));
      }
    }

    // Sort values and compute noise level based on the lowest 10% of intensities
    std::sort(values.begin(), values.end());
    int noise_sample_size = values.size() * 0.1;
    if (noise_sample_size < 2) noise_sample_size = values.size();  // Avoid very small samples
    double noise_level = compute_sd(std::vector<double>(values.begin(), values.begin() + noise_sample_size));
    threshold_value = threshold * noise_level;
  } else {
    stop("Invalid threshold_type.");
  }

  // Find peaks above threshold
  std::vector<double> peak_F2, peak_F1, peak_Intensity;
  
  for (int i = 1; i < rows - 1; i++) {
    for (int j = 1; j < cols - 1; j++) {
      if (bruker_data(i, j) >= threshold_value) {
        double max_neighbor = std::max({bruker_data(i-1, j), bruker_data(i+1, j), 
                                        bruker_data(i, j-1), bruker_data(i, j+1)});
        if (bruker_data(i, j) > max_neighbor) {
          peak_F2.push_back(ppm_x[i]);
          peak_F1.push_back(ppm_y[j]);
          peak_Intensity.push_back(bruker_data(i, j));
        }
      }
    }
  }

  return DataFrame::create(_["F2_ppm"] = peak_F2, _["F1_ppm"] = peak_F1, _["Intensity"] = peak_Intensity);
}')

# Run the Rcpp function with only three arguments
result <- peak_pick_2d_nt(bruker_data$currentSpectrum, 2, "noise")

print(result)


# 
# library(R.matlab)
# 
# # Save bruker_output1 as .mat
# writeMat("bruker_output1.mat", rr_data = bruker_data$rr_data, 
#          ppm_y = bruker_data$ppm_y, ppm_x = bruker_data$ppm_x)
# 
# 
# 
# 
# ### PeakPick NN ------
library(Rcpp)

# Embed the function
tf <- import("tensorflow")

# redo2Djg python file

model <- tf$keras$models$load_model("C:/Users/juguibert/Documents/cnn_peak_detector_rebuilt.h5")

# Create a dummy test spectrum (shape: [1, 64, 64, 1])
test_spectrum <- array(runif(64 * 64), dim = c(1, 64, 64, 1))

# Run inference
predictions <- model$predict(test_spectrum)

# Print the predictions
print(predictions)

# Threshold the predictions
predictions <- ifelse(predictions > 0.5, 1, 0)
print(predictions)

# Get the class with the highest probability
predicted_classes <- apply(predictions, 1, which.max)
print(predicted_classes)
