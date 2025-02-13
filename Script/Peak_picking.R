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

#### Test ----

library(signal)  # Load for optional smoothing

enhanced_peak_picking <- function(bruker_data, threshold = 5, threshold_type = "percentage", consider_negative = FALSE, smoothing = TRUE) {
  if (is.null(rownames(bruker_data)) || is.null(colnames(bruker_data))) {
    stop("The input object must have row names (F2 ppm) and column names (F1 ppm).")
  }
  
  ppm_x <- as.numeric(rownames(bruker_data))
  ppm_y <- as.numeric(colnames(bruker_data))
  
  # Apply Smoothing to Reduce Noise
  if (smoothing) {
    kernel <- matrix(1/9, 3, 3)  # Define a 3x3 smoothing kernel
    bruker_data <- convolve(bruker_data, kernel, type = "filter")  # Apply convolution
  }
  
  # Threshold Computation
  if (threshold_type == "percentage") {
    max_intensity <- max(bruker_data, na.rm = TRUE)
    threshold_value <- (threshold / 100) * max_intensity
  } else if (threshold_type == "noise") {
    noise_level <- sd(bruker_data[bruker_data < quantile(bruker_data, 0.1, na.rm = TRUE)], na.rm = TRUE)
    threshold_value <- threshold * noise_level
  } else {
    stop("Invalid threshold_type. Choose 'percentage' or 'noise'.")
  }
  
  significant_points <- which(abs(bruker_data) >= threshold_value, arr.ind = TRUE)
  
  if (nrow(significant_points) == 0) {
    return(data.frame(F2_ppm = numeric(0), F1_ppm = numeric(0), Intensity = numeric(0)))
  }
  
  # Local maxima detection using an adaptive approach
  is_peak <- apply(significant_points, 1, function(idx) {
    x <- idx[1]
    y <- idx[2]
    
    # Expand neighborhood to 3x3
    x_range <- max(1, x - 1):min(nrow(bruker_data), x + 1)
    y_range <- max(1, y - 1):min(ncol(bruker_data), y + 1)
    
    neighbors <- as.vector(bruker_data[x_range, y_range])
    central_val <- bruker_data[x, y]
    
    if (consider_negative) {
      abs_val <- abs(central_val)
      is_max <- all(abs_val > abs(neighbors), na.rm = TRUE)
    } else {
      is_max <- all(central_val > neighbors, na.rm = TRUE)
    }
    
    prominence <- central_val - mean(neighbors, na.rm = TRUE)  # Ensure peak prominence
    
    is_max && abs(central_val) > threshold_value && prominence > (0.1 * threshold_value)
  })
  
  peaks <- significant_points[is_peak, , drop = FALSE]
  
  if (nrow(peaks) == 0) {
    return(data.frame(F2_ppm = numeric(0), F1_ppm = numeric(0), Intensity = numeric(0)))
  }
  
  peak_list <- data.frame(
    F2_ppm = ppm_x[peaks[, 1]],
    F1_ppm = ppm_y[peaks[, 2]],
    Intensity = bruker_data[peaks]
  )
  
  return(peak_list)
}

# Run the peak picking on the spectrumData
result <- enhanced_peak_picking(bruker_data$spectrumData, threshold = 0.68)
# Check the results
result$p1  # x (direct) positions of peaks
result$p2  # y (indirect) positions of peaks
result$intensities  # Intensities of the detected peaks


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
