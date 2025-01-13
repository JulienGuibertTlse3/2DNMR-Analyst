### Vizualisation ----

library(ggplot2)
library(reshape2)


bruker_folder <- "C:/Users/juguibert/Documents/240202131_project/240202131_ech/240202131_Spectres_foie_2024/Spectres_UF_foie/404"


bruker_data <- read_bruker_file(bruker_folder)


bruker_data$rr_data[!is.finite(bruker_data$rr_data)] <- 0

finite_max = max(bruker_data$rr_data[is.finite(bruker_data$rr_data)], na.rm = TRUE)
finite_min = min(bruker_data$rr_data[is.finite(bruker_data$rr_data)], na.rm = TRUE)

# Replace Inf with the maximum finite value
bruker_data$rr_data[bruker_data$rr_data == Inf] <- finite_max

# Replace -Inf with the minimum finite value
bruker_data$rr_data[bruker_data$rr_data == -Inf] <- finite_min

plot_peaks <- function(bruker_data, intensity_threshold = 0.2) {
  if (is.null(bruker_data$rr_data) || is.null(bruker_data$ppm_x) || is.null(bruker_data$ppm_y) || is.null(bruker_data$experiment_type)) {
    stop("Invalid Bruker data. Ensure rr_data, ppm_x, ppm_y, and experiment_type are present.")
  }
  
  # Extract experiment type
  nuclei <- strsplit(bruker_data$experiment_type, ",\\s*")[[1]]
  direct_nucleus <- sub("Direct: ", "", nuclei[1])
  indirect_nucleus <- sub("Indirect: ", "", nuclei[2])
  
  # Set PPM ranges based on nuclei type
  ppm_y_range <- if (direct_nucleus == "<13C>") c(220, 0) else if (direct_nucleus == "<1H>") c(15, 0) else NULL
  ppm_x_range <- if (indirect_nucleus == "<13C>") c(220, 0) else if (indirect_nucleus == "<1H>") c(15, 0) else NULL
  
  if (is.null(ppm_x_range) || is.null(ppm_y_range)) {
    stop("Unsupported nucleus type in experiment. Expected 1H or 13C.")
  }
  
  # Extract and normalize intensity values
  intensity_matrix <- as.matrix(bruker_data$rr_data)
  min_val <- min(intensity_matrix, na.rm = TRUE)
  max_val <- max(intensity_matrix, na.rm = TRUE)
  
  if (max_val > min_val) {
    intensity_norm <- (intensity_matrix - min_val) / (max_val - min_val)
  } else {
    intensity_norm <- matrix(0, nrow = nrow(intensity_matrix), ncol = ncol(intensity_matrix))
  }
  
  # Apply intensity threshold
  intensity_norm[intensity_norm < intensity_threshold] <- NA
  
  # Convert matrix into a data frame for plotting
  intensity_df <- melt(intensity_norm)
  colnames(intensity_df) <- c("ppm_y_idx", "ppm_x_idx", "intensity")
  
  # Map indices to PPM values
  ppm_x <- bruker_data$ppm_y
  ppm_y <- bruker_data$ppm_x
  intensity_df$ppm_x <- ppm_x[intensity_df$ppm_x_idx]
  intensity_df$ppm_y <- ppm_y[intensity_df$ppm_y_idx]
  
  # Remove rows with NA intensities
  intensity_df <- intensity_df[!is.na(intensity_df$intensity), ]
  
  # Restrict data to calculated PPM ranges
  intensity_df <- intensity_df[
    intensity_df$ppm_x >= min(ppm_x_range) & intensity_df$ppm_x <= max(ppm_x_range) &
      intensity_df$ppm_y >= min(ppm_y_range) & intensity_df$ppm_y <= max(ppm_y_range),
  ]
  
  # Debug: Print PPM ranges and data frame
  cat("PPM X Range:", min(ppm_x_range), "-", max(ppm_x_range), "\n")
  cat("PPM Y Range:", min(ppm_y_range), "-", max(ppm_y_range), "\n")
  cat("Number of points to plot:", nrow(intensity_df), "\n")
  
  # Create ggplot
  p <- ggplot(intensity_df, aes(x = ppm_y, y = ppm_x, color = intensity)) +
    geom_point(size = 1, alpha = 0.8) +
    scale_x_reverse() +
    scale_y_reverse() +
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
p <- plot_peaks(bruker_data, intensity_threshold = 0.5)
print(p)