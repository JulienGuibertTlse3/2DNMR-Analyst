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



library(ggplot2)
library(metR)
library(dplyr)

plot_peaks2 <- function(rr_data, peak_list = NULL, unmatched_list = NULL, 
                        intensity_threshold = 1e+07, zoom_xlim = NULL, zoom_ylim = NULL) {
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
  
  # Convert matrix to data frame for plotting
  intensity_df <- expand.grid(ppm_x = ppm_x, ppm_y = ppm_y)
  intensity_df$intensity <- as.vector(rr_data)
  
  # Apply intensity threshold
  intensity_df <- intensity_df[intensity_df$intensity >= intensity_threshold, ]
  
  # Generate filled contours (only for computation)
  p <- ggplot(intensity_df, aes(x = ppm_y, y = ppm_x, z = intensity)) + 
    geom_contour_filled(bins = 20) +
    scale_x_reverse() +
    scale_y_reverse() +
    theme_minimal()
  
  # Add detected peaks (red)
  if (!is.null(peak_list) && nrow(peak_list) > 0) {
    p <- p + geom_point(data = peak_list, aes(x = F2_ppm, y = F1_ppm), 
                        color = "red", size = 2, inherit.aes = FALSE)
  }
  
  # Add unmatched peaks (green, triangle shape)
  if (!is.null(unmatched_list) && nrow(unmatched_list) > 0) {
    p <- p + geom_point(data = unmatched_list, aes(x = F2_ppm, y = F1_ppm), 
                        color = "green", size = 2, shape = 17, inherit.aes = FALSE)
  }
  
  # Apply Zoom if Specified
  if (!is.null(zoom_xlim) && !is.null(zoom_ylim)) {
    p <- p + coord_cartesian(xlim = zoom_xlim, ylim = zoom_ylim)
  }
  
  return(p)
}

#### SParsec----
library(ggplot2)
library(dplyr)
library(scales)

plot_nmr_spectrum <- function(rr_data, contour_start = 30000,
                              intensity_threshold = 1e+07,contour_num = 20, contour_factor = 1.2, 
                              peak_labels = NULL, zoom_xlim = NULL, zoom_ylim = NULL) {
  if (is.null(rr_data) || !is.matrix(rr_data)) {
    stop("Invalid Bruker data. Ensure rr_data is a matrix with proper intensity values.")
  }
  
  # Extract ppm values from matrix row and column names
  ppm_x <- as.numeric(rownames(rr_data))  # F1 axis (15N)
  ppm_y <- as.numeric(colnames(rr_data))  # F2 axis (13C)
  
  # Convert matrix to long format data frame
  intensity_df <- expand.grid(ppm_x = ppm_x, ppm_y = ppm_y)
  intensity_df$intensity <- as.vector(rr_data)
  
  # Apply intensity threshold
  intensity_df <- intensity_df[intensity_df$intensity >= intensity_threshold, ]
  
  # Compute contour levels like nmrglue
  contour_levels <- contour_start * contour_factor^(0:(contour_num - 1))
  
  # Plot contours using ggplot
  p <- ggplot(intensity_df, aes(x = ppm_y, y = ppm_x, z = intensity)) +
    geom_contour(color = "black", breaks = contour_levels) +  # Unfilled contour lines
    scale_x_reverse() +
    scale_y_reverse() +
    labs(title = "2D NMR Spectrum", x = "F2_ppm", y = "F1_ppm") +
    theme_minimal()
  
  # Add peak labels if provided
  if (!is.null(peak_labels) && nrow(peak_labels) > 0) {
    p <- p + geom_text(data = peak_labels, aes(x = F2_ppm, y = F1_ppm, label = Label), 
                       color = "red", size = 4, inherit.aes = FALSE)
  }
  
  # Apply zoom if needed
  if (!is.null(zoom_xlim) && !is.null(zoom_ylim)) {
    p <- p + coord_cartesian(xlim = zoom_xlim, ylim = zoom_ylim)
  }
  
  return(p)
}


### Functional function ----

library(Matrix)
library(ggplot2)
library(data.table)
library(dbscan)
library(FNN)
library(zoo)
library(stringr)  # For extracting stain IDs

find_nmr_peak_centroids <- function(rr_data, spectrum_type = NULL, 
                                    contour_start = NULL, intensity_threshold = NULL, 
                                    contour_num = NULL, contour_factor = NULL, 
                                    zoom_xlim = NULL, zoom_ylim = NULL, 
                                    f2_exclude_range = NULL,
                                    keep_peak_ranges = NULL) {
  if (is.null(rr_data) || !is.matrix(rr_data)) {
    stop("Invalid Bruker data. Ensure rr_data is a matrix with proper intensity values.")
  }
  
  # Default values based on spectrum type
  spectrum_defaults <- list(
    HSQC = list(contour_start = 8000, intensity_threshold = 200, contour_num = 8, contour_factor = 1.2),
    TOCSY = list(contour_start = 30000, intensity_threshold = 8000 , contour_num = 100, contour_factor = 10, f2_exclude_range = c(4.7, 5.0), keep_peak_ranges = list(c(0.5, -0.5), c(0.95, 0.85), c(1.55,1.45))),
    COSY  = list(contour_start = 1000, intensity_threshold = 20000, contour_num = 50, contour_factor = 1.3)
  )
  
  if (!is.null(spectrum_type)) {
    if (!spectrum_type %in% names(spectrum_defaults)) {
      stop("Invalid spectrum_type. Choose from 'HSQC', 'TOCSY', or 'COSY'.")
    }
    defaults <- spectrum_defaults[[spectrum_type]]
    
    contour_start <- ifelse(is.null(contour_start), defaults$contour_start, contour_start)
    intensity_threshold <- ifelse(is.null(intensity_threshold), defaults$intensity_threshold, intensity_threshold)
    contour_num <- ifelse(is.null(contour_num), defaults$contour_num, contour_num)
    contour_factor <- ifelse(is.null(contour_factor), defaults$contour_factor, contour_factor)
  }
  
  ppm_x <- as.numeric(rownames(rr_data))
  ppm_y <- as.numeric(colnames(rr_data))
  
  intensity_df <- expand.grid(ppm_x = ppm_x, ppm_y = ppm_y)
  intensity_df$intensity <- as.vector(rr_data)
  
  if (!is.null(f2_exclude_range) && length(f2_exclude_range) == 2) {
    intensity_df <- intensity_df %>%
      filter(!(ppm_y >= f2_exclude_range[1] & ppm_y <= f2_exclude_range[2]))
  }
  
  intensity_df <- intensity_df[intensity_df$intensity >= intensity_threshold, ]
  
  contour_levels <- contour_start * contour_factor^(0:(contour_num - 1))
  
  p <- ggplot(intensity_df, aes(x = ppm_y, y = ppm_x, z = intensity)) +
    geom_contour(color = "black", breaks = contour_levels) + 
    scale_x_reverse() +
    scale_y_reverse() +
    theme_minimal()
  
  contour_data <- ggplot_build(p)$data[[1]]
  
  if (nrow(contour_data) == 0 || !"group" %in% colnames(contour_data)) {
    warning("No contours generated from detected peaks.")
    return(list(plot = p + labs(title = "No Contours Generated"), 
                centroids = data.frame(F2_ppm = numeric(0), F1_ppm = numeric(0), stain_intensity = numeric(0))))
  }
  
  innermost_contours <- contour_data %>%
    group_by(group) %>%
    filter(level == max(level, na.rm = TRUE)) %>%
    ungroup()
  
  mean_x <- mean(innermost_contours$x)
  sd_x <- sd(innermost_contours$x)
  mean_y <- mean(innermost_contours$y)
  sd_y <- sd(innermost_contours$y)
  
  innermost_contours <- innermost_contours %>%
    mutate(
      x_scaled = (x - mean_x) / sd_x,
      y_scaled = (y - mean_y) / sd_y
    )
  
  if (nrow(innermost_contours) > 1) {
    db_clustering <- dbscan::dbscan(innermost_contours[, c("x_scaled", "y_scaled")], eps = 0.006, minPts = 0)
    innermost_contours$stain_id <- as.character(db_clustering$cluster)
  } else {
    innermost_contours$stain_id <- "1"
  }
  
  centroids_scaled <- innermost_contours %>%
    group_by(stain_id) %>%
    summarise(
      F2_scaled = sum(x_scaled * level, na.rm = TRUE) / sum(level, na.rm = TRUE),
      F1_scaled = sum(y_scaled * level, na.rm = TRUE) / sum(level, na.rm = TRUE),
      stain_intensity = sum(level, na.rm = TRUE),
      .groups = "drop"
    )
  
  centroids <- centroids_scaled %>%
    mutate(
      F2_ppm = (F2_scaled * sd_x) + mean_x,
      F1_ppm = (F1_scaled * sd_y) + mean_y
    ) %>%
    select(F2_ppm, F1_ppm, stain_intensity)
  
  # **Invert sign of coordinates (if needed)**
  centroids$F2_ppm <- -centroids$F2_ppm
  centroids$F1_ppm <- -centroids$F1_ppm
  
  # **Identify the most intense peaks within each specified range**
  if (!is.null(keep_peak_ranges) && is.list(keep_peak_ranges)) {
    first_range <- TRUE  # Flag to check if it's the first range
    for (range in keep_peak_ranges) {
      if (length(range) == 2) {
        # Filter centroids within the specified F2 range
        centroids_in_range <- centroids %>%
          filter(F2_ppm >= range[2] & F2_ppm <= range[1])
        
        # Determine how many peaks to keep
        num_peaks_to_keep <- if (first_range) 1 else 3
        first_range <- FALSE  # Mark first range as processed
        
        # Keep only the top N most intense peaks in this range
        top_peaks_in_range <- centroids_in_range %>%
          arrange(desc(stain_intensity)) %>%
          slice_head(n = num_peaks_to_keep)  
        
        # Remove all centroids in the range and add the selected ones
        centroids <- centroids %>%
          filter(!(F2_ppm >= range[2] & F2_ppm <= range[1])) %>%
          bind_rows(top_peaks_in_range)
      }
    }
  }
  
  p <- p + geom_point(data = centroids, aes(x = F2_ppm, y = F1_ppm, color = stain_intensity), 
                      size = 1.5, inherit.aes = FALSE) +
    scale_color_gradient(low = "blue", high = "green")
  
  if (!is.null(zoom_xlim) && !is.null(zoom_ylim)) {
    p <- p + coord_cartesian(xlim = zoom_xlim, ylim = zoom_ylim)
  }
  
  return(list(plot = p, centroids = centroids))
}


# test <- find_nmr_peak_centroids(bruker_data$spectrumData, spectrum_type = "TOCSY",
#                                 intensity_threshold = 12000,  ## Increase threshold
#                                 contour_start = 50000,        ## Start contours at a higher intensity
#                                 contour_num = 100,            ## Reduce contour levels
#                                 contour_factor = 10,         ## Adjust contour factor
#                                 f2_exclude_range = c(4.7, 5.0),
#                                 keep_peak_ranges = list(c(0.5, -0.5), c(0.95, 0.85), c(1.55,1.45)))#,zoom_xlim = c(0.95,0.85),zoom_ylim = c(1.75, 1.65))

# test1 <- find_nmr_peak_centroids(bruker_data$spectrumData, spectrum_type = "HSQC",
#   intensity_threshold = 200,  # Increase threshold
#   contour_start = 8000,        # Start contours at a higher intensity
#   contour_num = 8,            # Reduce contour levels
# contour_factor =1.2, zoom_xlim = c(1.6,1.4),zoom_ylim = c(20,17))


test2 <- find_nmr_peak_centroids(bruker_data$spectrumData, spectrum_type = "COSY", contour_start = 60000, intensity_threshold = 2000 , f2_exclude_range = c(4.7, 5.0))#, zoom_xlim = c(4,3),zoom_ylim = c(4, 3))


# find_nmr_peak_centroids(bruker_data$spectrumData, intensity_threshold = 0)
#print(test$p)
# print(test1$p)
print(test2$p)

peak_list <- peak_pick_from_centroids(test$centroids, threshold = 0.1)
peak_list2 <- simple_peak_pick(test$centroids, threshold = 0.1)
cmp <- compare_peak_lists(peak_list, data, 0.005, 0.005)
#print(unique(cmp$all_peaks))
cmp2 <- compare_peak_lists(peak_list2, data, 0.005, 0.005)
#print(unique(cmp2$all_peaks))
#HSQC Tolerance 0.005, 0.1
#TOCSY Tolerance 0.005, 0.005

num_matched_peaks <- sum(cmp$all_peaks$Matched == "Yes")
# Print the number of matched peaks
print(num_matched_peaks)


# POUR TOCSY bruit T1 (PeakViewer) les trainées sont prises en tant que pics
# Envoyer fichier HSQC complet avec tous les pics (pris et pas pris)
# Trier en fonction des déplacements chimiques pour fichier résultat Cecile

# Matrice SPARSE en ayant mis à 0 les valeurs, valuer sous seuil mis a 0 : post gestion en matric sparse

all_peaks <- cmp$all_peaks  # Extract all_peaks
all_peaks <- all_peaks %>% arrange(F2_ppm_auto)  # Sort by the first column

#write.csv(all_peaks, "tocsy.csv", row.names = FALSE)
#write.csv(cmp$unmatched, "tocsy_un.csv", row.names = FALSE)
#write.csv(bruker_data$spectrumData, "hsqc_data.csv", row.names = FALSE)

