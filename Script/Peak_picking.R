### PeakPick LM ------

dataT <- read.table(file = "Ok_smll.csv", sep = ";")
rownames(dataT) <- dataT[1,]
colnames(dataT) <- dataT[,1]
dataT <- dataT[-1,-1]

#### Peak pick R ----

# Function to perform peak picking on a 2D spectrum (A refaire en C++)
peak_pick_2d <- function(intensity, ppm_f1, ppm_f2, threshold) {
  # Define the threshold value
  th <- threshold * max(intensity) / 100
  
  # Initialize list to store peaks
  peaks <- list()
  
  # Loop through the intensity matrix to identify local maxima
  for (i in 2:(nrow(intensity) - 1)) {
    for (j in 2:(ncol(intensity) - 1)) {
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
            intensity = intensity[i, j]
          )
        ))
      }
    }
  }
  
  # Convert peaks list to a data frame for easier handling
  peak_df <- do.call(rbind, lapply(peaks, as.data.frame))
  
  return(peak_df)
}


#### C++ Peak pick -----

# Load the Rcpp package
library(Rcpp)

# Embed the C++ function in the R script
cppFunction('
DataFrame peak_pick_2d(NumericMatrix intensity, NumericVector ppm_f1, NumericVector ppm_f2, double threshold) {
  // Define the threshold value
  double max_intensity = max(intensity);
  double th = threshold * max_intensity / 100.0;

  // Initialize vectors to store peak information
  std::vector<double> peak_ppm_f1;
  std::vector<double> peak_ppm_f2;
  std::vector<double> peak_intensity;

  // Loop through the intensity matrix to identify local maxima
  int nrow = intensity.nrow();
  int ncol = intensity.ncol();
  for (int i = 1; i < nrow - 1; i++) {
    for (int j = 1; j < ncol - 1; j++) {
      double current = intensity(i, j);

      // Check if the current point is above the threshold
      if (current > th &&
          current > intensity(i - 1, j) &&
          current > intensity(i + 1, j) &&
          current > intensity(i, j - 1) &&
          current > intensity(i, j + 1)) {
        // Record the peak information
        peak_ppm_f1.push_back(ppm_f1[i]);
        peak_ppm_f2.push_back(ppm_f2[j]);
        peak_intensity.push_back(current);
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

# Create a test intensity matrix
intensity <- matrix(
  c(0, 0, 0, 0, 0,
    0, 3, 6, 3, 0,
    0, 5, 9, 5, 0,
    0, 3, 6, 3, 0,
    0, 0, 0, 0, 0),
  nrow = 5,
  byrow = TRUE
)

# Create ppm vectors
ppm_f1 <- seq(1, 5)
ppm_f2 <- seq(1, 5)

# Set threshold percentage
threshold <- 50

# Call the function
result <- peak_pick_2d(intensity, ppm_f1, ppm_f2, threshold)

# Print the result
print(result)




### PeakPick NN ------


install.packages("keras")
library(keras)

# Install TensorFlow backend if not already installed
install_keras()


build_cnn <- function(input_shape) {
  model <- keras_model_sequential() %>%
    
    # First Convolutional Layer
    layer_conv_2d(filters = 40, kernel_size = 11, activation = 'relu', input_shape = input_shape) %>%
    layer_max_pooling_2d(pool_size = 3) %>%
    
    # Second Convolutional Layer
    layer_conv_2d(filters = 20, kernel_size = 1, activation = 'relu') %>%
    layer_max_pooling_1d(pool_size = 3) %>%
    
    # Third Convolutional Layer
    layer_conv_2d(filters = 10, kernel_size = 11, activation = 'relu') %>%
    layer_max_pooling_2d(pool_size = 3) %>%
    
    # Fully Connected Layers
    layer_flatten() %>%
    layer_dense(units = 18, activation = 'relu') %>%
    layer_dense(units = 3, activation = 'softmax')  # Output Layer (3 classes)
  
  return(model)
}



# Define dimensions
batch_size <- 10   # Number of samples in the batch
height <- 28       # Height of the image
width <- 28        # Width of the image
channels <- 1      # Number of channels (e.g., 1 for grayscale, 3 for RGB)

# Create random data for the tensor
input_tensor <- array(runif(batch_size * height * width * channels), 
                      dim = c(batch_size, height, width, channels))

model <- build_cnn(input_tensor)

# Compile the model
model %>% compile(
  optimizer = 'adam',
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)

# Display model summary
summary(model)


# Simulate NMR data
x_train <- array(runif(10000), dim = c(100, 100, 1))  # 100 samples of 1D spectra, each with 100 points
y_train <- to_categorical(sample(0:2, 100, replace = TRUE), num_classes = 3)

# Split into training and validation sets
x_val <- x_train[1:20,,]
y_val <- y_train[1:20,]
x_train <- x_train[21:100,,]
y_train <- y_train[21:100,]

history <- model %>% fit(
  x = x_train,
  y = y_train,
  epochs = 50,  # Number of training epochs
  batch_size = 32,
  validation_data = list(x_val, y_val)
)

# Plot training history
plot(history)








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
