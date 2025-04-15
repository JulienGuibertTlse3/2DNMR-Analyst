### Functional function ----

library(Matrix)
library(ggplot2)
library(data.table)
library(dbscan)
library(FNN)
library(zoo)
library(stringr)  # For extracting stain IDs
# 
# find_nmr_peak_centroids <- function(rr_data, spectrum_type = NULL, 
#                                     contour_start = NULL, intensity_threshold = NULL, 
#                                     contour_num = NULL, contour_factor = NULL, 
#                                     zoom_xlim = NULL, zoom_ylim = NULL, 
#                                     f2_exclude_range = NULL,
#                                     keep_peak_ranges = NULL) {
#   if (is.null(rr_data) || !is.matrix(rr_data)) {
#     stop("Invalid Bruker data. Ensure rr_data is a matrix with proper intensity values.")
#   }
#   
#   # Default values based on spectrum type
#   spectrum_defaults <- list(
#     HSQC = list(contour_start = 8000, intensity_threshold = 200, contour_num = 8, contour_factor = 1.2),
#     TOCSY = list(contour_start = 100000, intensity_threshold = 4000 , contour_num = 110, contour_factor = 1.3, f2_exclude_range = c(4.7, 5.0)),
#     COSY  = list(contour_start = 1000, intensity_threshold = 20000, contour_num = 60, contour_factor = 1.3)
#   )
#   
#   if (!is.null(spectrum_type)) {
#     if (!spectrum_type %in% names(spectrum_defaults)) {
#       stop("Invalid spectrum_type. Choose from 'HSQC', 'TOCSY', or 'COSY'.")
#     }
#     defaults <- spectrum_defaults[[spectrum_type]]
#     
#     contour_start <- ifelse(is.null(contour_start), defaults$contour_start, contour_start)
#     intensity_threshold <- ifelse(is.null(intensity_threshold), defaults$intensity_threshold, intensity_threshold)
#     contour_num <- ifelse(is.null(contour_num), defaults$contour_num, contour_num)
#     contour_factor <- ifelse(is.null(contour_factor), defaults$contour_factor, contour_factor)
#   }
#   
#   ppm_x <- as.numeric(rownames(rr_data))
#   ppm_y <- as.numeric(colnames(rr_data))
#   
#   intensity_df <- expand.grid(ppm_x = ppm_x, ppm_y = ppm_y)
#   intensity_df$intensity <- as.vector(rr_data)
#   
#   if (!is.null(f2_exclude_range) && length(f2_exclude_range) == 2) {
#     intensity_df <- intensity_df %>%
#       filter(!(ppm_y >= f2_exclude_range[1] & ppm_y <= f2_exclude_range[2]))
#   }
#   
#   intensity_df <- intensity_df[intensity_df$intensity >= intensity_threshold, ]
#   
#   contour_levels <- contour_start * contour_factor^(0:(contour_num - 1))
#   
#   p <- ggplot(intensity_df, aes(x = ppm_y, y = ppm_x, z = intensity)) +
#     geom_contour(color = "black", breaks = contour_levels) + 
#     scale_x_reverse() +
#     scale_y_reverse() +
#     theme_minimal()
#   
#   contour_data <- ggplot_build(p)$data[[1]]
#   
#   if (nrow(contour_data) == 0 || !"group" %in% colnames(contour_data)) {
#     warning("No contours generated from detected peaks.")
#     return(list(plot = p + labs(title = "No Contours Generated"), 
#                 centroids = data.frame(F2_ppm = numeric(0), F1_ppm = numeric(0), stain_intensity = numeric(0))))
#   }
#   
#   contour_density <- contour_data %>%
#     group_by(x, y) %>%
#     summarise(overlapping_contours = n(), .groups = "drop")
#   
#   threshold_dense_region <- quantile(contour_density$overlapping_contours, 0.95)
#   
#   contour_data <- left_join(contour_data, contour_density, by = c("x", "y"))
#   
#   high_intensity_threshold <- quantile(contour_data$level, 0.95)
#   high_intensity_data <- contour_data %>% filter(level >= high_intensity_threshold)
#   low_intensity_data <- contour_data %>% filter(level < high_intensity_threshold)
#   
#   normalize <- function(df) {
#     df %>%
#       mutate(
#         x_scaled = (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE),
#         y_scaled = (y - mean(y, na.rm = TRUE)) / sd(y, na.rm = TRUE)
#       )
#   }
#   
#   high_intensity_data <- normalize(high_intensity_data)
#   low_intensity_data <- normalize(low_intensity_data)
#   
#   high_clusters <- dbscan::dbscan(high_intensity_data[, c("x_scaled", "y_scaled")], eps = 0.0068, minPts = 0)
#   low_clusters <- dbscan::dbscan(low_intensity_data[, c("x_scaled", "y_scaled")], eps = 0.006, minPts = 0)
#   
#   high_intensity_data$stain_id <- as.character(high_clusters$cluster)
#   low_intensity_data$stain_id <- as.character(low_clusters$cluster)
#   
#   if (nrow(high_intensity_data) > 0 & nrow(low_intensity_data) > 0) {
#     max_high_id <- max(as.numeric(high_intensity_data$stain_id), na.rm = TRUE)
#     low_intensity_data$stain_id <- as.character(as.numeric(low_intensity_data$stain_id) + max_high_id)
#   }
#   
#   final_clusters <- bind_rows(high_intensity_data, low_intensity_data)
#   
#   centroids <- final_clusters %>%
#     group_by(stain_id) %>%
#     summarise(
#       F2_ppm = mean(x, na.rm = TRUE),
#       F1_ppm = mean(y, na.rm = TRUE),
#       stain_intensity = sum(level, na.rm = TRUE),
#       .groups = "drop"
#     )
#   
#   centroids$F2_ppm <- -centroids$F2_ppm
#   centroids$F1_ppm <- -centroids$F1_ppm
#   
#   bounding_boxes <- final_clusters %>%
#     group_by(stain_id) %>%
#     summarise(
#       xmin = min(-x, na.rm = TRUE),
#       xmax = max(-x, na.rm = TRUE),
#       ymin = min(-y, na.rm = TRUE),
#       ymax = max(-y, na.rm = TRUE),
#       intensity = sum(level, na.rm = TRUE),
#       .groups = "drop"
#     )
#   
#   # Step 2: Calculate density (i.e., the number of points inside each bounding box)
#   bounding_boxes <- bounding_boxes %>%
#     rowwise() %>%
#     mutate(
#       density = sum(
#         -final_clusters$x >= xmin & -final_clusters$x <= xmax &
#           -final_clusters$y >= ymin & -final_clusters$y <= ymax
#       )
#     )
#   
#   
#   # --- Remove lower-intensity centroids inside bounding boxes of stronger peaks ---
#   if (nrow(centroids) > 1) {
#     centroids_with_boxes <- centroids %>%
#       left_join(bounding_boxes, by = "stain_id")
#     
#     to_remove <- c()
#     
#     for (i in seq_len(nrow(centroids_with_boxes))) {
#       current <- centroids_with_boxes[i, ]
#       
#       for (j in seq_len(nrow(centroids_with_boxes))) {
#         if (i == j) next
#         
#         compare <- centroids_with_boxes[j, ]
#         
#         inside_box <- current$F2_ppm >= compare$xmin & current$F2_ppm <= compare$xmax &
#           current$F1_ppm >= compare$ymin & current$F1_ppm <= compare$ymax
#         lower_intensity <- current$stain_intensity < compare$intensity
#         
#         if (inside_box && lower_intensity) {
#           to_remove <- c(to_remove, current$stain_id)
#           break
#         }
#       }
#     }
#     
#     centroids <- centroids %>% filter(!stain_id %in% to_remove)
#     bounding_boxes <- bounding_boxes %>% filter(!stain_id %in% to_remove)
#   }
#   # ---------------------------------------------------------------------
#   
#   if (!is.null(keep_peak_ranges) && is.list(keep_peak_ranges)) {
#     first_range <- TRUE
#     for (range in keep_peak_ranges) {
#       if (length(range) == 2) {
#         centroids_in_range <- centroids %>%
#           filter(F2_ppm >= range[2] & F2_ppm <= range[1])
#         
#         num_peaks_to_keep <- if (first_range) 2 else 2
#         first_range <- FALSE
#         
#         top_peaks_in_range <- centroids_in_range %>%
#           arrange(desc(stain_intensity)) %>%
#           slice_head(n = num_peaks_to_keep)  
#         
#         centroids <- centroids %>%
#           filter(!(F2_ppm >= range[2] & F2_ppm <= range[1])) %>%
#           bind_rows(top_peaks_in_range)
#       }
#     }
#   }
#   
#   # Step 2: Link centroids to bounding boxes using stain_id
#   centroids_with_boxes <- centroids %>%
#     left_join(bounding_boxes, by = "stain_id")
#   
#   # Step 3: Keep only bounding boxes that have centroids inside them
#   bounding_boxes <- bounding_boxes %>%
#     filter(stain_id %in% centroids_with_boxes$stain_id)
#   
#   p <- p +
#     geom_rect(data = bounding_boxes, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               color = "red", fill = NA, linetype = "dashed", inherit.aes = FALSE) +
#     geom_point(data = centroids, aes(x = F2_ppm, y = F1_ppm, color = stain_intensity), 
#                size = 0.4, inherit.aes = FALSE) +
#     scale_color_gradient(low = "blue", high = "green")
#   
#   if (!is.null(zoom_xlim) && !is.null(zoom_ylim)) {
#     p <- p + coord_cartesian(xlim = zoom_xlim, ylim = zoom_ylim)
#   }
#   
#   
#   return(list(
#     plot = p,
#     centroids = centroids,
#     bounding_boxes = bounding_boxes,
#     contour_density = contour_density,
#     threshold_dense_region = threshold_dense_region,
#     contour_data = contour_data,
#     high_intensity_data = high_intensity_data
#   ))
# }
# 
# 
# # 
# # test <- find_nmr_peak_centroids(bruker_data$spectrumData, spectrum_type = "TOCSY",
# #                                 intensity_threshold = 30000,  ## Increase threshold
# #                                 contour_start = 100000,        ## Start contours at a higher intensity
# #                                 contour_num = 110,            ## Reduce contour levels
# #                                 contour_factor = 1.3,         ## Adjust contour factor
# #                                 f2_exclude_range = c(4.7, 5.0),
# #                                 keep_peak_ranges = list(c(0.5, -0.5), c(1, 0.8), c(1.55,1.45)),zoom_xlim = c(4,3),zoom_ylim = c(4, 3))
# # 
# # test1 <- find_nmr_peak_centroids(bruker_data$spectrumData, spectrum_type = "HSQC",
# #   intensity_threshold = 5000,  # Increase threshold
# #   contour_start = 15000,        # Start contours at a higher intensity
# #   contour_num = 80,            # Reduce contour levels
# # contour_factor =1.3, zoom_xlim = c(4,3),zoom_ylim = c(77,73))
# 
# # # 
# test2 <- find_nmr_peak_centroids(bruker_data$spectrumData, spectrum_type = "COSY", contour_start = 100000, intensity_threshold = 8000 , f2_exclude_range = c(4.7, 5.0),  keep_peak_ranges = list(c(0.5, -0.5), c(1, 0.8), c(1.55,1.45)))#, zoom_xlim = c(4,3),zoom_ylim = c(4,3))
# 
# # # 
# # # find_nmr_peak_centroids(bruker_data$spectrumData, intensity_threshold = 0)
# # print(test$p)
# # print(sum(nrow(test$centroids)))
# # print(sum(nrow(test$bounding_boxes)))
# 
# # print(test1$p)
# # print(sum(nrow(test1$centroids)))
# print(test2$p)
# print(sum(nrow(test2$centroids)))
# 
# # Result_HSQC <- test1$centroids
# Result_Cosy <- test2$centroids

# 
# # gt <- test2$bounding_boxes
# # View(gt)
# 
# peak_list <- peak_pick_from_centroids(test$centroids, threshold = 0.1)
# peak_list2 <- simple_peak_pick(test$centroids, threshold = 0.1)
# cmp <- compare_peak_lists(peak_list, data, 0.005, 0.005)
# #print(unique(cmp$all_peaks))
# cmp2 <- compare_peak_lists(Result_HSQC, data1, 0.005, 0.1)
# #print(unique(cmp2$all_peaks))
# #HSQC Tolerance 0.005, 0.1
# #TOCSY Tolerance 0.005, 0.005
# 
# num_matched_peaks <- sum(cmp$all_peaks$Matched == "Yes")
# # Print the number of matched peaks
# print(num_matched_peaks)
# 
# 
# # POUR TOCSY bruit T1 (PeakViewer) les trainées sont prises en tant que pics
# # Envoyer fichier HSQC complet avec tous les pics (pris et pas pris)
# # Trier en fonction des déplacements chimiques pour fichier résultat Cecile
# 
# # Matrice SPARSE en ayant mis à 0 les valeurs, valuer sous seuil mis a 0 : post gestion en matric sparse
# 
# all_peaks <- cmp2$all_peaks  # Extract all_peaks
# all_peaks <- all_peaks %>% arrange(F2_ppm_auto)  # Sort by the first column
# 
# write.csv(Result_Cosy, "cosy.csv", row.names = FALSE)
# #write.csv(cmp$unmatched, "tocsy_un.csv", row.names = FALSE)
# #write.csv(bruker_data$spectrumData, "hsqc_data.csv", row.names = FALSE)
# 




#### Nouvelle tentative ----

# Function for displaying contours
find_nmr_peak_centroids <- function(rr_data, spectrum_type = NULL, 
                                    contour_start = NULL, intensity_threshold = NULL, 
                                    contour_num = NULL, contour_factor = NULL, 
                                    zoom_xlim = NULL, zoom_ylim = NULL, 
                                    f2_exclude_range = NULL) {
  if (is.null(rr_data) || !is.matrix(rr_data)) {
    stop("Invalid Bruker data. Ensure rr_data is a matrix with proper intensity values.")
  }
  
  # Default values based on spectrum type
  spectrum_defaults <- list(
    HSQC = list(contour_start = 8000, intensity_threshold = 200, contour_num = 8, contour_factor = 1.2),
    TOCSY = list(contour_start = 100000, intensity_threshold = 4000 , contour_num = 110, contour_factor = 1.3, f2_exclude_range = c(4.7, 5.0)),
    COSY  = list(contour_start = 1000, intensity_threshold = 20000, contour_num = 60, contour_factor = 1.3)
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
    labs(y = "F1_ppm", x = "F2_ppm") +
    theme_minimal()
  
  contour_data <- ggplot_build(p)$data[[1]]
  
  return(list( plot = p, contour_data = contour_data))
}



# New function for processing centroids and bounding boxes
process_nmr_centroids <- function(rr_data, contour_data, contour_num = NULL, contour_factor = NULL, 
                                  intensity_threshold = NULL, keep_peak_ranges = NULL) {
  
  # Ensure contour data is available
  if (nrow(contour_data) == 0 || !"group" %in% colnames(contour_data)) {
    stop("No contours data available for processing.")
  }
  
  contour_density <- contour_data %>%
    group_by(x, y) %>%
    summarise(overlapping_contours = n(), .groups = "drop")
  
  threshold_dense_region <- quantile(contour_density$overlapping_contours, 0.95)
  
  contour_data <- left_join(contour_data, contour_density, by = c("x", "y"))
  
  high_intensity_threshold <- quantile(contour_data$level, 0.95)
  high_intensity_data <- contour_data %>% filter(level >= high_intensity_threshold)
  low_intensity_data <- contour_data %>% filter(level < high_intensity_threshold)
  
  normalize <- function(df) {
    df %>%
      mutate(
        x_scaled = (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE),
        y_scaled = (y - mean(y, na.rm = TRUE)) / sd(y, na.rm = TRUE)
      )
  }
  
  high_intensity_data <- normalize(high_intensity_data)
  low_intensity_data <- normalize(low_intensity_data)
  
  high_clusters <- dbscan::dbscan(high_intensity_data[, c("x_scaled", "y_scaled")], eps = 0.0068, minPts = 0)
  low_clusters <- dbscan::dbscan(low_intensity_data[, c("x_scaled", "y_scaled")], eps = 0.006, minPts = 0)
  
  high_intensity_data$stain_id <- as.character(high_clusters$cluster)
  low_intensity_data$stain_id <- as.character(low_clusters$cluster)
  
  if (nrow(high_intensity_data) > 0 & nrow(low_intensity_data) > 0) {
    max_high_id <- max(as.numeric(high_intensity_data$stain_id), na.rm = TRUE)
    low_intensity_data$stain_id <- as.character(as.numeric(low_intensity_data$stain_id) + max_high_id)
  }
  
  final_clusters <- bind_rows(high_intensity_data, low_intensity_data)
  
  centroids <- final_clusters %>%
    group_by(stain_id) %>%
    summarise(
      F2_ppm = mean(x, na.rm = TRUE),
      F1_ppm = mean(y, na.rm = TRUE),
      stain_intensity = sum(level, na.rm = TRUE),
      .groups = "drop"
    )
  
  centroids$F2_ppm <- -centroids$F2_ppm
  centroids$F1_ppm <- -centroids$F1_ppm
  
  bounding_boxes <- final_clusters %>%
    group_by(stain_id) %>%
    summarise(
      xmin = min(-x, na.rm = TRUE),
      xmax = max(-x, na.rm = TRUE),
      ymin = min(-y, na.rm = TRUE),
      ymax = max(-y, na.rm = TRUE),
      intensity = sum(level, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Step 2: Calculate density (i.e., the number of points inside each bounding box)
  bounding_boxes <- bounding_boxes %>%
    rowwise() %>%
    mutate(
      density = sum(
        -final_clusters$x >= xmin & -final_clusters$x <= xmax &
          -final_clusters$y >= ymin & -final_clusters$y <= ymax
      )
    )
  
  # --- Remove lower-intensity centroids inside bounding boxes of stronger peaks ---
  if (nrow(centroids) > 1) {
    centroids_with_boxes <- centroids %>%
      left_join(bounding_boxes, by = "stain_id")
    
    to_remove <- c()
    
    for (i in seq_len(nrow(centroids_with_boxes))) {
      current <- centroids_with_boxes[i, ]
      
      for (j in seq_len(nrow(centroids_with_boxes))) {
        if (i == j) next
        
        compare <- centroids_with_boxes[j, ]
        
        inside_box <- current$F2_ppm >= compare$xmin & current$F2_ppm <= compare$xmax &
          current$F1_ppm >= compare$ymin & current$F1_ppm <= compare$ymax
        lower_intensity <- current$stain_intensity < compare$intensity
        
        if (inside_box && lower_intensity) {
          to_remove <- c(to_remove, current$stain_id)
          break
        }
      }
    }
    
    centroids <- centroids %>% filter(!stain_id %in% to_remove)
    bounding_boxes <- bounding_boxes %>% filter(!stain_id %in% to_remove)
  }
  
  
  # ---------------------------------------------------------------------

  if (!is.null(keep_peak_ranges) && is.list(keep_peak_ranges)) {
    first_range <- TRUE
    for (range in keep_peak_ranges) {
      if (length(range) == 2) {
        centroids_in_range <- centroids %>%
          filter(F2_ppm >= range[2] & F2_ppm <= range[1])
        
        num_peaks_to_keep <- if (first_range) 2 else 2
        first_range <- FALSE
        
        top_peaks_in_range <- centroids_in_range %>%
          arrange(desc(stain_intensity)) %>%
          slice_head(n = num_peaks_to_keep)
        
        centroids <- centroids %>%
          filter(!(F2_ppm >= range[2] & F2_ppm <= range[1])) %>%
          bind_rows(top_peaks_in_range)
      }
    }
  }
  
  # Return the processed centroids and bounding boxes
  return(list(
    centroids = centroids,
    bounding_boxes = bounding_boxes
  ))
}


get_local_stain_intensity <- function(f2_ppm, f1_ppm, contour_data, eps_ppm = 0.004) {
  local_points <- contour_data %>%
    filter(
      x >= f2_ppm - eps_ppm & x <= f2_ppm + eps_ppm,
      y >= f1_ppm - eps_ppm & y <= f1_ppm + eps_ppm
    )
  
  if (nrow(local_points) == 0) {
    return(NA)
  }
  
  return(sum(local_points$level, na.rm = TRUE))
}

#Point1 : 3.7315 3.726
#Point2 : 3.7815 3.81

