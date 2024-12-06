### PeakPick LM ------

dataT <- read.table(file = "Ok_smll.csv", sep = ";")
rownames(dataT) <- dataT[1,]
colnames(dataT) <- dataT[,1]
dataT <- dataT[-1,-1]

#### Peak pick R ----
peak_pick_2d <- function(bruker_output, threshold) {
  # Extract intensity and ppm axes from the Bruker output
  intensity <- bruker_output$rr_data
  ppm_f1 <- bruker_output$ppm_y
  ppm_f2 <- bruker_output$ppm_x
  
  # Replace Inf and -Inf with finite values
  finite_max <- max(intensity[is.finite(intensity)], na.rm = TRUE)
  finite_min <- min(intensity[is.finite(intensity)], na.rm = TRUE)
  intensity[is.infinite(intensity) & intensity > 0] <- finite_max
  intensity[is.infinite(intensity) & intensity < 0] <- finite_min
  
  
  # Define the threshold value (percentage of maximum normalized intensity)
  th <- threshold * finite_max / 100
  print(paste("Threshold value (normalized):", th))
  
  # Initialize list to store peaks
  peaks <- list()
  
  # Loop through the intensity matrix to identify local maxima
  for (i in 2:(nrow(intensity) - 1)) {
    for (j in 2:(ncol(intensity) - 1)) {
      # Skip if the current point or its neighbors are NA
      if (is.na(intensity[i, j]) ||
          is.na(intensity[i - 1, j]) || is.na(intensity[i + 1, j]) ||
          is.na(intensity[i, j - 1]) || is.na(intensity[i, j + 1])) {
        next
      }
      
      # Check if the current point is a local maximum
      if (intensity[i, j] > th &&
          intensity[i, j] > intensity[i - 1, j] &&
          intensity[i, j] > intensity[i + 1, j] &&
          intensity[i, j] > intensity[i, j - 1] &&
          intensity[i, j] > intensity[i, j + 1]) {
        
        # Record the peak information
        peaks <- append(peaks, list(
          list(
            ppm_f1 = ppm_f1[i],
            ppm_f2 = ppm_f2[j],
            intensity = intensity[i, j] * (finite_max - finite_min) + finite_min  # Scale back intensity
          )
        ))
      }
    }
  }
  
  # Convert peaks list to a data frame for easier handling
  if (length(peaks) > 0) {
    peak_df <- do.call(rbind, lapply(peaks, as.data.frame))
  } else {
    peak_df <- data.frame(ppm_f1 = numeric(0), ppm_f2 = numeric(0), intensity = numeric(0))
  }
  
  # Return the peak data frame
  return(peak_df)
}





#### C++ Peak pick -----

# Load the Rcpp package
library(Rcpp)

# Embed the C++ function directly in R
cppFunction('
DataFrame peak_pick_2d(List bruker_output, double threshold) {
  // Extract intensity and ppm axes from the bruker_output
  NumericMatrix intensity = bruker_output["rr_data"];
  NumericVector ppm_f1 = bruker_output["ppm_y"];
  NumericVector ppm_f2 = bruker_output["ppm_x"];
  
  int nrow = intensity.nrow();
  int ncol = intensity.ncol();
  
  // Find finite max and min values
  double finite_max = R_NegInf;
  double finite_min = R_PosInf;
  for (int i = 0; i < nrow; ++i) {
    for (int j = 0; j < ncol; ++j) {
      double value = intensity(i, j);
      if (R_finite(value)) {
        finite_max = std::max(finite_max, value);
        finite_min = std::min(finite_min, value);
      }
    }
  }
  
  // Replace Inf and -Inf values with finite max/min
  for (int i = 0; i < nrow; ++i) {
    for (int j = 0; j < ncol; ++j) {
      if (R_IsInf(intensity(i, j)) && intensity(i, j) > 0) {
        intensity(i, j) = finite_max;
      } else if (R_IsInf(intensity(i, j)) && intensity(i, j) < 0) {
        intensity(i, j) = finite_min;
      }
    }
  }
  
  // Define the threshold
  double th = threshold * finite_max / 100.0;
  Rcpp::Rcout << "Threshold value (normalized): " << th << std::endl;
  
  // Initialize vectors to store peak information
  std::vector<double> peak_ppm_f1;
  std::vector<double> peak_ppm_f2;
  std::vector<double> peak_intensity;
  
  // Loop through the intensity matrix to identify local maxima
  for (int i = 1; i < nrow - 1; ++i) {
    for (int j = 1; j < ncol - 1; ++j) {
      double current = intensity(i, j);
      
      // Skip if the current value or its neighbors are NA
      if (NumericMatrix::is_na(current) ||
          NumericMatrix::is_na(intensity(i - 1, j)) || NumericMatrix::is_na(intensity(i + 1, j)) ||
          NumericMatrix::is_na(intensity(i, j - 1)) || NumericMatrix::is_na(intensity(i, j + 1))) {
        continue;
      }
      
      // Check if the current point is a local maximum
      if (current > th &&
          current > intensity(i - 1, j) &&
          current > intensity(i + 1, j) &&
          current > intensity(i, j - 1) &&
          current > intensity(i, j + 1)) {
        // Scale back the intensity and record the peak
        double scaled_intensity = current * (finite_max - finite_min) + finite_min;
        peak_ppm_f1.push_back(ppm_f1[i]);
        peak_ppm_f2.push_back(ppm_f2[j]);
        peak_intensity.push_back(scaled_intensity);
      }
    }
  }
  
  // Return the results as a DataFrame
  return DataFrame::create(
    Named("ppm_f1") = peak_ppm_f1,
    Named("ppm_f2") = peak_ppm_f2,
    Named("intensity") = peak_intensity
  );
}
')

# Example usage in R
# Assuming bruker_output is the result of read_bruker_file
bruker_output <- list(
  rr_data = matrix(runif(10000, -1, 10), nrow = 100, ncol = 100),
  ppm_y = seq(0, 10, length.out = 100),
  ppm_x = seq(0, 10, length.out = 100)
)

# Perform peak picking
threshold <- 10
peaks <- peak_pick_2d(bruker_output, threshold)

# Display the peaks
print(peaks)





### PeakPick NN ------
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



### Viz ----

# Function to load and process the 2D NMR data
load_and_pick_peaks <- function(file_path, threshold) {
  # Load the data
  raw_data <- read.csv(file_path, header = TRUE, row.names = 1, check.names = FALSE)
  
  # Extract ppm scales and intensity matrix
  ppm_f2 <- as.numeric(colnames(raw_data))  # F2 ppm
  ppm_f1 <- as.numeric(rownames(raw_data))  # F1 ppm
  intensity <- as.matrix(raw_data)         # Intensity matrix
  
  # Perform peak picking
  peaks <- peak_pick_2d(intensity, ppm_f1, ppm_f2, threshold)
  
  return(list(peaks = peaks, intensity = intensity, ppm_f1 = ppm_f1, ppm_f2 = ppm_f2))
}

# Example usage
file_path <- "OK.csv"  # Replace with your file path
threshold <- 10  # Threshold as percentage of the maximum intensity
nmr_result <- load_and_pick_peaks(file_path, threshold)

# Inspect the peaks
print(nmr_result$peaks)

# Visualize the spectrum with peaks
plot_2D_with_peaks <- function(nmr_result) {
  image(
    x = nmr_result$ppm_f2,
    y = nmr_result$ppm_f1,
    z = nmr_result$intensity,
    col = heat.colors(100),
    xlab = "F2 (ppm)",
    ylab = "F1 (ppm)",
    main = "2D NMR Spectrum with Peaks",
    useRaster = TRUE
  )
  
  # Add peaks to the plot
  points(
    nmr_result$peaks$ppm_f2,
    nmr_result$peaks$ppm_f1,
    col = "red",
    pch = 4,
    cex = 1.2
  )
}

# Plot the spectrum with peaks
plot_2D_with_peaks(nmr_result)



### PLotting ----

# Function to plot 2D NMR spectrum and overlay peak-picking results
plot_2d_nmr_with_peaks <- function(spectrum_matrix, ppm_x, ppm_y, peaks) {
  # Check input dimensions
  if (!is.matrix(spectrum_matrix)) stop("spectrum_matrix must be a matrix.")
  if (length(ppm_x) != ncol(spectrum_matrix)) stop("ppm_x length must match the number of columns in the spectrum_matrix.")
  if (length(ppm_y) != nrow(spectrum_matrix)) stop("ppm_y length must match the number of rows in the spectrum_matrix.")
  
  # Plot the 2D NMR spectrum using a heatmap
  image(x = ppm_x, y = ppm_y, z = t(spectrum_matrix), 
        col = viridis::viridis(100), 
        xlab = "1H Chemical Shift (ppm)", 
        ylab = "13C Chemical Shift (ppm)", 
        main = "2D NMR Spectrum")
  
  # Add contour lines for better visualization
  contour(x = ppm_x, y = ppm_y, z = t(spectrum_matrix), add = TRUE, col = "white")
  
  # Overlay peak positions
  if (!is.null(peaks)) {
    points(peaks$ppm_x, peaks$ppm_y, col = "red", pch = 4, cex = 1.5, lwd = 2)
    text(peaks$ppm_x, peaks$ppm_y, labels = peaks$intensity, pos = 4, col = "red", cex = 0.8)
  }
}

# Example usage:
# Assuming:
# - `spectrum_matrix` is the 2D matrix of intensity values.
# - `ppm_x` and `ppm_y` are vectors of ppm values for the columns and rows of the matrix, respectively.
# - `peaks` is a data frame with columns `ppm_x`, `ppm_y`, and `intensity`.

# Simulate some example data for peaks (replace this with your real peak-picking results)
example_peaks <- data.frame(
  ppm_x = c(10.1, 10.05, 9.95),
  ppm_y = c(10.2, 10.15, 9.9),
  intensity = c(4000, 3500, 2000)
)

# Call the plotting function
plot_2d_nmr_with_peaks(
  spectrum_matrix = as.matrix(dataT),  # Your 2D NMR matrix
  ppm_x = ppm_x,  # PPM values for X-axis
  ppm_y = ppm_y,  # PPM values for Y-axis
  peaks = example_peaks  # Peak-picking results
)
