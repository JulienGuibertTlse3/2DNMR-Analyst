### Vizualisation ----

library(ggplot2)
library(reshape2)
library(dplyr)
library(tibble)
library(tidyr)
library(Matrix)

rr_data <- as.data.frame(bruker_data$rr_data)

rr_data <- Matrix(bruker_data$rr_data, sparse = TRUE)


#### Working for COSY ----

# Function to visualize detected and unmatched peaks
plot_peaks2 <- function(rr_data, peak_list = NULL, unmatched_list = NULL, intensity_threshold = 1e+07) {
  if (is.null(rr_data) || !is.matrix(rr_data)) {
    stop("Invalid Bruker data. Ensure rr_data is a matrix with proper intensity values.")
  }
  
  # Extract ppm values
  ppm_x <- as.numeric(rownames(rr_data))  # F1 axis
  ppm_y <- as.numeric(colnames(rr_data))  # F2 axis
  
  SI1 <- nrow(rr_data)
  SI2 <- ncol(rr_data)
  
  if (length(ppm_x) != SI1 || length(ppm_y) != SI2) {
    stop("Mismatch between PPM scale and matrix dimensions.")
  }
  
  # Generate Proper Index Grid
  ppm_x_idx <- rep(1:SI1, times = SI2)  # Repeat each F1 index across all F2 values
  ppm_y_idx <- rep(1:SI2, each = SI1)  # Repeat each F2 index for all F1 values
  
  # Unroll the Matrix
  intensity_df <- data.frame(
    ppm_x_idx = ppm_x_idx,
    ppm_y_idx = ppm_y_idx,
    intensity = as.vector(rr_data)
  )
  
  # Map correct ppm values
  intensity_df$ppm_x <- ppm_x[intensity_df$ppm_x_idx]
  intensity_df$ppm_y <- ppm_y[intensity_df$ppm_y_idx]
  
  # Apply intensity threshold
  intensity_df <- intensity_df[intensity_df$intensity >= intensity_threshold, ]
  
  # Plot Spectrum
  p <- ggplot(intensity_df, aes(x = ppm_y, y = ppm_x)) +
    geom_point(size = 0.6, alpha = 0.2, color = "blue") +
    scale_x_reverse(limits = c(max(ppm_y), min(ppm_y))) +
    scale_y_reverse(limits = c(max(ppm_x), min(ppm_x))) +
    labs(
      x = "F2 (ppm)",
      y = "F1 (ppm)",
      title = "2D NMR Spectrum with Detected and Unmatched Peaks"
    ) +
    theme_minimal()
  
  # Highlight detected peaks
  if (!is.null(peak_list) && nrow(peak_list) > 0) {
    p <- p + geom_point(data = peak_list, aes(x = F2_ppm, y = F1_ppm), color = "red", size = 2)
  }
  
  # Highlight unmatched peaks in green
  if (!is.null(unmatched_list) && nrow(unmatched_list) > 0) {
    p <- p + geom_point(data = unmatched_list, aes(x = F2_ppm, y = F1_ppm), color = "green", size = 2, shape = 17)
  }
  
  return(p)
}

#### SParsec----
library(Matrix)
library(ggplot2)

plot_peaks2_sparse <- function(rr_data, intensity_threshold = 1e+07) {
  
  if (is.null(rr_data) || !inherits(rr_data, "dgCMatrix")) {
    stop("Invalid Bruker data. Ensure rr_data is a sparse matrix (dgCMatrix).")
  }
  
  # Extract ppm values from row and column names
  ppm_x <- as.numeric(rownames(rr_data))  # F1 axis
  ppm_y <- as.numeric(colnames(rr_data))  # F2 axis
  
  if (is.null(ppm_x) || is.null(ppm_y)) {
    stop("Row and column names must be numeric PPM values.")
  }
  
  # ✅ Extract only nonzero values efficiently
  sparse_data <- as.data.frame(summary(rr_data))  # Extracts row indices, col indices, and values
  colnames(sparse_data) <- c("ppm_x_idx", "ppm_y_idx", "intensity")
  
  # ✅ Convert indices to actual ppm values
  sparse_data$ppm_x <- ppm_x[sparse_data$ppm_x_idx]  # Map to actual PPM values
  sparse_data$ppm_y <- ppm_y[sparse_data$ppm_y_idx]
  
  # Apply intensity threshold
  cat("Before filtering:", nrow(sparse_data), "rows.\n")
  sparse_data <- sparse_data[sparse_data$intensity >= intensity_threshold, ]
  cat("After filtering:", nrow(sparse_data), "rows.\n")
  
  if (nrow(sparse_data) == 0) {
    stop("No data points exceed the intensity threshold. Try lowering the threshold.")
  }
  
  # Plot
  p <- ggplot(sparse_data, aes(x = ppm_x, y = ppm_y, color = intensity)) +
    geom_point(size = 0.6, alpha = 0.2) +
    scale_x_reverse(limits = c(max(ppm_x), min(ppm_x))) +
    scale_y_reverse(limits = c(max(ppm_y), min(ppm_y))) +
    scale_color_gradient(low = "blue", high = "red", name = "Intensity") +
    labs(
      x = "F2 (ppm)",
      y = "F1 (ppm)",
      title = "2D NMR NUS TOCSY Spectrum"
    ) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 10, face = "bold"),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
    )
  
  return(p)
}
