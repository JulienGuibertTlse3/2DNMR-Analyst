### Vizualisation ----


library(ggplot2)
library(grid)
library(gridExtra)

plot_2d_nmr <- function(bruker_data, 
                        w1Range = NULL, 
                        w2Range = NULL, 
                        pos.zlim = NULL, 
                        neg.zlim = NULL, 
                        levels = 20, 
                        pos.color = "blue", 
                        neg.color = "red", 
                        main_title = "2D NMR Spectrum") {
  # Extract data
  ppm_x <- bruker_data$ppm_x
  ppm_y <- bruker_data$ppm_y
  intensity_matrix <- bruker_data$rr_data
  
  if (is.null(ppm_x) || is.null(ppm_y) || is.null(intensity_matrix)) {
    stop("Invalid bruker_data: Ensure 'ppm_x', 'ppm_y', and 'rr_data' are not NULL.")
  }
  
  # Handle infinite or NaN values in the intensity matrix
  intensity_matrix[!is.finite(intensity_matrix)] <- 0
  
  # Ensure axes are sorted in increasing order for compatibility with image
  if (any(diff(ppm_x) < 0)) {
    ppm_x <- rev(ppm_x)
    intensity_matrix <- intensity_matrix[, ncol(intensity_matrix):1]
  }
  if (any(diff(ppm_y) < 0)) {
    ppm_y <- rev(ppm_y)
    intensity_matrix <- intensity_matrix[nrow(intensity_matrix):1, ]
  }
  
  # Set w1Range and w2Range if not provided
  if (is.null(w1Range)) {
    w1Range <- range(ppm_y, na.rm = TRUE)
  }
  if (is.null(w2Range)) {
    w2Range <- range(ppm_x, na.rm = TRUE)
  }
  
  # Set z-limits for positive and negative intensities
  if (is.null(pos.zlim)) {
    max_intensity <- max(intensity_matrix, na.rm = TRUE)
    pos.zlim <- c(max_intensity / levels, max_intensity)
  }
  if (is.null(neg.zlim)) {
    min_intensity <- min(intensity_matrix, na.rm = TRUE)
    neg.zlim <- c(min_intensity, min_intensity / levels)
  }
  
  # Open a new interactive window
  if (interactive()) {
    dev.new(width = 10, height = 8)
  }
  
  # Plot the spectrum
  image(
    x = ppm_x, y = ppm_y, z = t(intensity_matrix),
    col = colorRampPalette(c(neg.color, "white", pos.color))(levels),
    xlab = "F2 (ppm)",
    ylab = "F1 (ppm)",
    main = main_title,
    axes = FALSE
  )
  
  # Add axes with reversed labels for NMR convention
  axis(1, at = pretty(ppm_x), labels = rev(pretty(ppm_x)))
  axis(2, at = pretty(ppm_y), labels = rev(pretty(ppm_y)))
  box()
  
  # Add contours for better visualization
  contour(
    ppm_x, ppm_y, t(intensity_matrix),
    levels = seq(pos.zlim[1], pos.zlim[2], length.out = levels),
    col = pos.color, add = TRUE
  )
  contour(
    ppm_x, ppm_y, t(intensity_matrix),
    levels = seq(neg.zlim[1], neg.zlim[2], length.out = levels),
    col = neg.color, add = TRUE
  )
}



# Example usage
# bruker_data <- read_bruker_file("path/to/bruker/folder")
plot_2d_nmr(bruker_data, levels = 20)
