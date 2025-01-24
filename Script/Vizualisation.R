### Vizualisation ----

library(ggplot2)
library(reshape2)


plot_peaks <- function(bruker_data, intensity_threshold = 0.2) {
  if (is.null(bruker_data$rr_data) || is.null(bruker_data$ppm$x) || is.null(bruker_data$ppm$y) || is.null(bruker_data$experiment_type)) {
    stop("Invalid Bruker data. Ensure rr_data, ppm_x, ppm_y, and experiment_type are present.")
  }
  
  # bruker_data$rr_data[!is.finite(bruker_data$rr_data)] <- 0
  # 
  # finite_max <- max(bruker_data$rr_data[is.finite(bruker_data$rr_data)], na.rm = TRUE)
  # finite_min <- min(bruker_data$rr_data[is.finite(bruker_data$rr_data)], na.rm = TRUE)
  # 
  # bruker_data$rr_data[bruker_data$rr_data == Inf] <- finite_max
  # bruker_data$rr_data[bruker_data$rr_data == -Inf] <- finite_min
  
  nuclei <- strsplit(bruker_data$experiment_type, ",\\s*")[[1]]
  direct_nucleus <- sub("Direct: ", "", nuclei[1])
  indirect_nucleus <- sub("Indirect: ", "", nuclei[2])
  
  ppm_x_range <- range(bruker_data$ppm$x, na.rm = TRUE)
  ppm_y_range <- range(bruker_data$ppm$y, na.rm = TRUE)
  
  intensity_matrix <- as.matrix(bruker_data$rr_data)
  cat("Intensity matrix dimensions:", dim(intensity_matrix), "\n")
  cat("PPM X Range:", range(ppm_x_range, na.rm = TRUE), "\n")
  cat("PPM Y Range:", range(ppm_y_range, na.rm = TRUE), "\n")
  
  min_val <- min(intensity_matrix, na.rm = TRUE)
  max_val <- max(intensity_matrix, na.rm = TRUE)
  
  # if (max_val > min_val) {
  #   intensity_norm <- (intensity_matrix - min_val) / (max_val - min_val)
  # } else {
  #   intensity_norm <- matrix(0, nrow = nrow(intensity_matrix), ncol = ncol(intensity_matrix))
  # }
  # 
  # intensity_norm[intensity_norm < intensity_threshold] <- NA
  
  intensity_matrix[intensity_matrix < intensity_threshold] <- NA
  
  intensity_df <- reshape2::melt(intensity_matrix)
  colnames(intensity_df) <- c("ppm_y_idx", "ppm_x_idx", "intensity")
  
  ppm_x <- bruker_data$ppm$x
  ppm_y <- bruker_data$ppm$y
  intensity_df$ppm_x <- ppm_x[intensity_df$ppm_y_idx]
  intensity_df$ppm_y <- ppm_y[intensity_df$ppm_x_idx]
  
  intensity_df <- intensity_df[!is.na(intensity_df$intensity), ]
  
  cat("First few mapped points:\n")
  print(intensity_df[1:10, ])
  
  # Check if intensities match expected PPM values
  cat("PPM X of the first point:", ppm_x[intensity_df$ppm_x_idx[2]], "\n")
  cat("PPM Y of the first point:", ppm_y[intensity_df$ppm_y_idx[2]], "\n")
  cat("Intensity of the first point:", bruker_data$rr_data[intensity_df$ppm_y_idx[2], intensity_df$ppm_x_idx[2]], "\n")
  
  # Check for points below 150 ppm
  cat("Number of points with ppm_x > 150:", nrow(intensity_df[intensity_df$ppm_x > 150, ]), "\n")
  # 
  # intensity_df <- intensity_df[nrow(intensity_df):1,ncol(intensity_df):1]
  # 
  p <- ggplot(intensity_df, aes(x = ppm_y, y = ppm_x, colour = intensity)) +
    geom_point(size = 0.4, alpha = 1) +
    scale_x_reverse(limits = c(10,-1)) +
    scale_y_reverse(limits = c(10,-1)) +
    scale_color_gradient(low = "white", high = "red", name = "Intensity") +
    labs(
      x = paste0("F2 (", direct_nucleus, ", ppm)"),
      y = paste0("F1 (", indirect_nucleus, ", ppm)"),
      title = paste("NMR Spectrum Peaks (", direct_nucleus, "-", indirect_nucleus, ")")
    ) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 10, face = "bold"),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
    )
  
  return(p)
}


# Plot peaks with an intensity threshold
p <- plot_peaks(bruker_data_test, intensity_threshold = 2.8e+38)
print(p)