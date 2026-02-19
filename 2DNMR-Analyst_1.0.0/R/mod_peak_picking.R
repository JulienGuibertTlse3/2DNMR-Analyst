# 2D NMR Analyst - Module: Peak Picking ----

# Author: Julien Guibert
# Description: Shiny module for automatic peak detection using local maxima
#              and DBSCAN clustering

## Module UI ----

mod_peak_picking_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      style = "display: flex; gap: 5px; margin-bottom: 10px;",
      actionButton(
        ns("generate_centroids"), 
        "Local Max",
        class = "btn-success btn-sm",
        style = "flex: 1; font-size: 11px; padding: 5px 2px;"
      ),
      ### CNN ### Bouton CNN
      actionButton(
        ns("generate_cnn"),
        "CNN",
        class = "btn-warning btn-sm",
        style = "flex: 1; font-size: 11px; padding: 5px 2px;"
      )
    ),
    
    tags$details(
      tags$summary("⚙️ Options"),
      div(
        checkboxInput(ns("disable_clustering"), "No clustering", value = FALSE),
        numericInput(ns("eps_value"), "Epsilon:", value = 0.0068, min = 0, step = 0.001),
        textAreaInput(
          ns("keep_peak_ranges_text"), 
          "Delete ranges:",
          value = "0.5,-0.5; 1,0.8; 1.55,1.45; 5.1,4.7;", 
          rows = 2
        ),
        ### CNN ### Paramètres CNN (sous-menu déroulant)
        tags$details(
          tags$summary("🧠 CNN Parameters"),
          div(
            numericInput(ns("cnn_pred_class_thres"), "Prediction threshold:", 
                         value = 0.3, min = 0, max = 1, step = 0.05),
            sliderInput(ns("cnn_trace_filter"), "Trace filter (% of F2 line max):",
                        min = 0, max = 100, value = 50, step = 5)
          )
        )
      )
    )
  )
}


## Module Server ----

mod_peak_picking_server <- function(id, 
                                    status_msg, 
                                    load_data, 
                                    data_reactives,
                                    rv,
                                    refresh_nmr_plot,
                                    parent_input) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # AUTO-UPDATE EPSILON BASED ON SPECTRUM TYPE ----
    observeEvent(parent_input$spectrum_type, {
      params <- switch(parent_input$spectrum_type,
                       "TOCSY" = list(eps_value = 0.0068),
                       "HSQC" = list(eps_value = 0.0068),
                       "COSY" = list(eps_value = 0.0068),
                       "UFCOSY" = list(eps_value = 0.014),
                       list(eps_value = 0.0068))
      updateNumericInput(session, "eps_value", value = params$eps_value)
    }, ignoreInit = TRUE)
    
    
    # ═══════════════════════════════════════════════════════════════════════
    # Local Max method ----
    # ═══════════════════════════════════════════════════════════════════════
    
    observeEvent(input$generate_centroids, {
      
      req(parent_input$selected_subfolder, data_reactives$result_data_list(), load_data$bruker_data())
      
      params <- data_reactives$spectrum_params()
      all_results <- data_reactives$result_data_list()
      selected_result <- all_results[[parent_input$selected_subfolder]]
      
      if (is.null(selected_result)) {
        showNotification("⚠️ No result found", type = "error")
        return()
      }
      
      status_msg("🔄 [1/4] Preparing data...")
      
      selected_spectrum <- load_data$bruker_data()$spectrumData
      if (is.null(selected_spectrum)) {
        showNotification("⚠️ Spectrum not found", type = "error")
        return()
      }
      
      keep_ranges <- parse_keep_peak_ranges(input$keep_peak_ranges_text)
      
      if (input$disable_clustering) {
        status_msg("🔄 [2/4] Detecting local maxima (no clustering)...")
        
        result_peaks <- tryCatch({
          peak_pick_2d_nt2(
            bruker_data = selected_spectrum,
            threshold_value = parent_input$contour_start,
            neighborhood_size = params$neighborhood_size,
            f2_exclude_range = c(4.7, 5.0),
            keep_peak_ranges = keep_ranges,
            spectrum_type = "TOCSY",
            diagnose_zones = c(0.9, 1.6),
            diagnose_radius = 0.1
          )
        }, error = function(e) {
          showNotification(paste("❌ Error:", e$message), type = "error")
          return(NULL)
        })
        
        if (is.null(result_peaks)) return()
        
        status_msg("🔄 [3/4] Processing peaks...")
        rv$centroids_data(result_peaks$peaks)
        
        if (!is.null(result_peaks$bounding_boxes) && nrow(result_peaks$bounding_boxes) > 0) {
          required_cols <- c("xmin", "xmax", "ymin", "ymax", "stain_id")
          if (all(required_cols %in% names(result_peaks$bounding_boxes))) {
            box_coords_only <- result_peaks$bounding_boxes[, required_cols, drop = FALSE]
          } else {
            box_coords_only <- data.frame(xmin = numeric(0), xmax = numeric(0),
                                          ymin = numeric(0), ymax = numeric(0), stain_id = character(0))
          }
        } else {
          box_coords_only <- data.frame(xmin = numeric(0), xmax = numeric(0),
                                        ymin = numeric(0), ymax = numeric(0), stain_id = character(0))
        }
        
      } else {
        status_msg("🔄 [2/4] Detecting peaks + DBSCAN clustering...")
        
        calc_contour <- data_reactives$calculated_contour_value()
        
        result1 <- tryCatch({
          process_nmr_centroids(
            rr_data = selected_spectrum,
            contour_data = selected_result$contour_data,
            intensity_threshold = modulate_threshold(parent_input$contour_start) %||%
              modulate_threshold(calc_contour),
            contour_num = params$contour_num,
            contour_factor = params$contour_factor,
            eps_value = input$eps_value,
            keep_peak_ranges = keep_ranges,
            spectrum_type = parent_input$spectrum_type
          )
        }, error = function(e) {
          showNotification(paste("❌ Error:", e$message), type = "error")
          NULL
        })
        
        if (is.null(result1)) return()
        
        status_msg("🔄 [3/4] Processing centroids...")
        rv$centroids_data(result1$centroids)
        
        if (!is.null(result1$bounding_boxes) && nrow(result1$bounding_boxes) > 0) {
          required_cols <- c("xmin", "xmax", "ymin", "ymax", "stain_id")
          if (all(required_cols %in% names(result1$bounding_boxes))) {
            box_coords_only <- result1$bounding_boxes[, required_cols, drop = FALSE]
          } else {
            box_coords_only <- data.frame(xmin = numeric(0), xmax = numeric(0),
                                          ymin = numeric(0), ymax = numeric(0), stain_id = character(0))
          }
        } else {
          box_coords_only <- data.frame(xmin = numeric(0), xmax = numeric(0),
                                        ymin = numeric(0), ymax = numeric(0), stain_id = character(0))
        }
      }
      
      status_msg("🔄 [4/4] Updating plot...")
      
      rv$fixed_boxes(box_coords_only)
      rv$modifiable_boxes(rv$fixed_boxes())
      rv$reference_boxes(rv$fixed_boxes())
      rv$contour_plot_base(selected_result$plot + ggplot2::labs(title = ""))
      refresh_nmr_plot()
      
      n_peaks <- nrow(rv$centroids_data() %||% data.frame())
      n_boxes <- nrow(box_coords_only)
      status_msg(paste0("✅ Peak picking complete: ", n_peaks, " peaks, ", n_boxes, " boxes"))
      showNotification(paste0("✅ Found ", n_peaks, " peaks and ", n_boxes, " boxes"),
                       type = "message", duration = 4)
    })
    
    
    ## CNN Peak Picking ----
    
    
    observeEvent(input$generate_cnn, {
      
      req(parent_input$selected_subfolder, data_reactives$result_data_list(), load_data$bruker_data())
      
      if (!exists("new_model")) {
        showNotification("❌ CNN model not loaded! Check CNN_shiny.R", type = "error")
        return()
      }
      
      all_results <- data_reactives$result_data_list()
      selected_result <- all_results[[parent_input$selected_subfolder]]
      
      if (is.null(selected_result)) {
        showNotification("⚠️ No result found", type = "error")
        return()
      }
      
      selected_spectrum <- load_data$bruker_data()$spectrumData
      if (is.null(selected_spectrum)) {
        showNotification("⚠️ Spectrum not found", type = "error")
        return()
      }
      
      contour_data <- selected_result$contour_data
      if (is.null(contour_data) || nrow(contour_data) == 0) {
        showNotification("⚠️ No contour data - generate plot first", type = "warning")
        return()
      }
      
      # ═══════════════════════════════════════════════════════════════════════
      # PROGRESS BAR FOR CNN PROCESSING
      # ═══════════════════════════════════════════════════════════════════════
      withProgress(message = "🧠 CNN Peak Picking", value = 0, {
        
        incProgress(0.05, detail = "Preparing spectrum...")
        status_msg("🧠 [1/5] CNN: Preparing spectrum...")
        
        # ═══════════════════════════════════════════════════════════════════
        # NORMALIZATION PER PERCENTILE
        # ═══════════════════════════════════════════════════════════════════
        rr_abs <- abs(selected_spectrum)
        
        # Use 99.9th percentile as "max" to avoid outliers
        max_val <- quantile(rr_abs, 0.999)
        rr_norm <- rr_abs / max_val
        rr_norm[rr_norm > 1] <- 1 
        
        rownames(rr_norm) <- rownames(selected_spectrum)
        colnames(rr_norm) <- colnames(selected_spectrum)
        
        # DIAGNOSTIC: Check normalized spectrum
        cat("\n╔═══════════════════════════════════════════════════════════════╗\n")
        cat("║         DIAGNOSTIC: Spectrum normalization                    ║\n")
        cat("╠═══════════════════════════════════════════════════════════════╣\n")
        cat("Original abs range:", min(rr_abs), "-", max(rr_abs), "\n")
        cat("99.9th percentile (used as max):", max_val, "\n")
        cat("Normalized range:", min(rr_norm), "-", max(rr_norm), "\n")
        
        # Check columns
        sample_cols <- c(1, 1000, 10000, 32768, 50000, 65000)
        sample_cols <- sample_cols[sample_cols <= ncol(rr_norm)]
        cat("\nSample columns after normalization:\n")
        for (col_idx in sample_cols) {
          col_max <- max(rr_norm[, col_idx])
          col_ppm <- as.numeric(colnames(rr_norm))[col_idx]
          cat(sprintf("  Col %5d (ppm=%.2f): max=%.4f\n", col_idx, col_ppm, col_max))
        }
        cat("╚═══════════════════════════════════════════════════════════════╝\n\n")
        
        incProgress(0.15, detail = "Running neural network...")
        status_msg("🧠 [2/5] CNN: Running neural network prediction...")
        
        cnn_params <- list(
          pred_class_thres = input$cnn_pred_class_thres,
          int_thres = 0,
          eps_value = input$eps_value,
          use_filters = FALSE,
          disable_clustering = input$disable_clustering,
          trace_filter_ratio = input$cnn_trace_filter / 100  # Filtre traces TOCSY (% -> ratio)
        )
        
        cnn_result <- tryCatch({
          run_cnn_peak_picking(
            rr_norm = rr_norm,
            model = new_model,
            params = cnn_params,
            method = "batch",
            verbose = TRUE
          )
        }, error = function(e) {
          showNotification(paste("❌ CNN Error:", e$message), type = "error")
          return(NULL)
        })
        
        if (is.null(cnn_result) || is.null(cnn_result$peaks) || nrow(cnn_result$peaks) == 0) {
          showNotification("⚠️ CNN detected no peaks", type = "warning")
          return()
        }
        
        cnn_peaks <- cnn_result$peaks
        cat("\n")
        cat("╔══════════════════════════════════════════════════════════════╗\n")
        cat("║                    DEBUG CNN PEAK PICKING                     ║\n")
        cat("╠══════════════════════════════════════════════════════════════╣\n")
        cat("║ STEP 1: CNN Raw Peaks                                        ║\n")
        cat("╠══════════════════════════════════════════════════════════════╣\n")
        cat("CNN detected", nrow(cnn_peaks), "raw peaks\n")
        cat("Columns in cnn_peaks:", paste(names(cnn_peaks), collapse=", "), "\n")
        cat("First 5 peaks:\n")
        print(head(cnn_peaks, 5))
        cat("\n")
        
        # Verify and normalize column's names
        if (!"F2_ppm" %in% names(cnn_peaks)) {
          cat("⚠️ F2_ppm not found, checking alternatives...\n")
          if ("F2" %in% names(cnn_peaks)) {
            # F2 may be define as index, convert in ppm
            ppm_rows <- as.numeric(rownames(rr_norm))
            ppm_cols <- as.numeric(colnames(rr_norm))
            cat("  Found F2 column, range:", min(cnn_peaks$F2), "-", max(cnn_peaks$F2), "\n")
            cat("  Spectrum row ppm range:", min(ppm_rows), "-", max(ppm_rows), "\n")
            cat("  Spectrum col ppm range:", min(ppm_cols), "-", max(ppm_cols), "\n")
            
            if (all(cnn_peaks$F2 == floor(cnn_peaks$F2)) && max(cnn_peaks$F2) > 10) {
              cat("  -> F2 looks like indices, converting to ppm\n")
              cnn_peaks$F2_ppm <- ppm_rows[pmin(pmax(round(cnn_peaks$F2), 1), length(ppm_rows))]
            } else {
              cat("  -> F2 looks like ppm already\n")
              cnn_peaks$F2_ppm <- cnn_peaks$F2
            }
          }
        }
        
        if (!"F1_ppm" %in% names(cnn_peaks)) {
          cat("⚠️ F1_ppm not found, checking alternatives...\n")
          if ("F1" %in% names(cnn_peaks)) {
            ppm_rows <- as.numeric(rownames(rr_norm))
            ppm_cols <- as.numeric(colnames(rr_norm))
            cat("  Found F1 column, range:", min(cnn_peaks$F1), "-", max(cnn_peaks$F1), "\n")
            
            if (all(cnn_peaks$F1 == floor(cnn_peaks$F1)) && max(cnn_peaks$F1) > 10) {
              cat("  -> F1 looks like indices, converting to ppm\n")
              cnn_peaks$F1_ppm <- ppm_cols[pmin(pmax(round(cnn_peaks$F1), 1), length(ppm_cols))]
            } else {
              cat("  -> F1 looks like ppm already\n")
              cnn_peaks$F1_ppm <- cnn_peaks$F1
            }
          }
        }
        
        cat("After normalization - F2_ppm range:", min(cnn_peaks$F2_ppm), "-", max(cnn_peaks$F2_ppm), "\n")
        cat("After normalization - F1_ppm range:", min(cnn_peaks$F1_ppm), "-", max(cnn_peaks$F1_ppm), "\n")
        cat("\n")
        
        incProgress(0.25, detail = "Clustering contour data...")
        status_msg("🧠 [3/5] CNN: Clustering contour data...")
        
        cat("╠══════════════════════════════════════════════════════════════╣\n")
        cat("║ STEP 2: Spectrum & Contour Data                              ║\n")
        cat("╠══════════════════════════════════════════════════════════════╣\n")
        cat("Spectrum dimensions:", nrow(selected_spectrum), "x", ncol(selected_spectrum), "\n")
        cat("Spectrum rownames (F2) range:", min(as.numeric(rownames(selected_spectrum))), "-", 
            max(as.numeric(rownames(selected_spectrum))), "\n")
        cat("Spectrum colnames (F1) range:", min(as.numeric(colnames(selected_spectrum))), "-", 
            max(as.numeric(colnames(selected_spectrum))), "\n")
        cat("Contour_data rows:", nrow(contour_data), "\n")
        cat("Contour_data columns:", paste(names(contour_data), collapse=", "), "\n")
        cat("Contour x range:", min(contour_data$x), "-", max(contour_data$x), "\n")
        cat("Contour y range:", min(contour_data$y), "-", max(contour_data$y), "\n")
        cat("\n")
        
        # ═══════════════════════════════════════════════════════════════════════
        # Same as process_nmr_centroids
        # ═══════════════════════════════════════════════════════════════════════
        
        # Z-score normalization (same as in process_nmr_centroids )
        contour_scaled <- contour_data %>%
          dplyr::mutate(
            F2_scaled = (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE),
            F1_scaled = (y - mean(y, na.rm = TRUE)) / sd(y, na.rm = TRUE)
          )
        
        # DBSCAN (same as in process_nmr_centroids)
        clusters <- dbscan::dbscan(
          contour_scaled[, c("F2_scaled", "F1_scaled")],
          eps = input$eps_value,
          minPts = 0
        )
        contour_scaled$stain_id <- as.character(clusters$cluster)
        
        # Remove noise (cluster 0) 
        contour_filtered <- contour_scaled %>% dplyr::filter(stain_id != "0")
        
        cat("╠══════════════════════════════════════════════════════════════╣\n")
        cat("║ STEP 3: DBSCAN Clustering                                    ║\n")
        cat("╠══════════════════════════════════════════════════════════════╣\n")
        cat("Unique clusters found:", length(unique(contour_scaled$stain_id)), "\n")
        cat("Points after removing noise (cluster 0):", nrow(contour_filtered), "\n")
        cat("Unique valid clusters:", length(unique(contour_filtered$stain_id)), "\n")
        cat("\n")
        
        if (nrow(contour_filtered) == 0) {
          showNotification("⚠️ No clusters found", type = "warning")
          return()
        }
        
        # Create bounding boxes
        bounding_boxes <- contour_filtered %>%
          dplyr::group_by(stain_id) %>%
          dplyr::summarise(
            xmin = min(-x, na.rm = TRUE),
            xmax = max(-x, na.rm = TRUE),
            ymin = min(-y, na.rm = TRUE),
            ymax = max(-y, na.rm = TRUE),
            intensity = sum(level, na.rm = TRUE),
            .groups = "drop"
          )
        
        # Find peaks 
        centroids <- contour_filtered %>%
          dplyr::group_by(stain_id) %>%
          dplyr::summarise(
            F2_ppm = -mean(x, na.rm = TRUE),
            F1_ppm = -mean(y, na.rm = TRUE),
            Volume = sum(level, na.rm = TRUE),
            .groups = "drop"
          )
        
        cat("╠══════════════════════════════════════════════════════════════╣\n")
        cat("║ STEP 4: Bounding Boxes                                       ║\n")
        cat("╠══════════════════════════════════════════════════════════════╣\n")
        cat("Number of bounding boxes:", nrow(bounding_boxes), "\n")
        cat("Box F2 (xmin/xmax) range:", min(bounding_boxes$xmin), "-", max(bounding_boxes$xmax), "\n")
        cat("Box F1 (ymin/ymax) range:", min(bounding_boxes$ymin), "-", max(bounding_boxes$ymax), "\n")
        cat("First 5 boxes:\n")
        print(head(bounding_boxes, 5))
        cat("\n")
        
        incProgress(0.25, detail = "Filtering by CNN detections...")
        status_msg("🧠 [4/5] CNN: Filtering by CNN detections...")
        
        cat("╠══════════════════════════════════════════════════════════════╣\n")
        cat("║ STEP 5: Matching CNN Peaks to Boxes                          ║\n")
        cat("╠══════════════════════════════════════════════════════════════╣\n")
        cat("CNN peaks F2_ppm range:", min(cnn_peaks$F2_ppm), "-", max(cnn_peaks$F2_ppm), "\n")
        cat("CNN peaks F1_ppm range:", min(cnn_peaks$F1_ppm), "-", max(cnn_peaks$F1_ppm), "\n")
        cat("Boxes F2 (xmin/xmax) range:", min(bounding_boxes$xmin), "-", max(bounding_boxes$xmax), "\n")
        cat("Boxes F1 (ymin/ymax) range:", min(bounding_boxes$ymin), "-", max(bounding_boxes$ymax), "\n")
        cat("\n")
        
        # Verify if range overlaps
        f2_overlap <- !(max(cnn_peaks$F2_ppm) < min(bounding_boxes$xmin) | 
                          min(cnn_peaks$F2_ppm) > max(bounding_boxes$xmax))
        f1_overlap <- !(max(cnn_peaks$F1_ppm) < min(bounding_boxes$ymin) | 
                          min(cnn_peaks$F1_ppm) > max(bounding_boxes$ymax))
        
        cat("F2 ranges overlap:", f2_overlap, "\n")
        cat("F1 ranges overlap:", f1_overlap, "\n")
        
        if (!f2_overlap || !f1_overlap) {
          cat("\n⚠️ WARNING: Ranges don't overlap! Checking if F1/F2 are swapped...\n")
          f2_overlap_swapped <- !(max(cnn_peaks$F1_ppm) < min(bounding_boxes$xmin) | 
                                    min(cnn_peaks$F1_ppm) > max(bounding_boxes$xmax))
          f1_overlap_swapped <- !(max(cnn_peaks$F2_ppm) < min(bounding_boxes$ymin) | 
                                    min(cnn_peaks$F2_ppm) > max(bounding_boxes$ymax))
          cat("With SWAPPED coords - F2 overlap:", f2_overlap_swapped, ", F1 overlap:", f1_overlap_swapped, "\n")
        }
        cat("\n")
        
        # ═══════════════════════════════════════════════════════════════════════
        # FILTER : keep only the clusters in which the CNN detected something
        # ═══════════════════════════════════════════════════════════════════════
        
        keep_ids <- sapply(bounding_boxes$stain_id, function(sid) {
          box <- bounding_boxes[bounding_boxes$stain_id == sid, ]
          
          # Margin = 50% of box width (adaptative for each cluster)
          margin_x <- (box$xmax - box$xmin) * 0.5
          margin_y <- (box$ymax - box$ymin) * 0.5
          
          # Margin of at least 0.02 ppm 
          margin_x <- max(margin_x, 0.02)
          margin_y <- max(margin_y, 0.02)
          
          any(
            cnn_peaks$F2_ppm >= (box$xmin - margin_x) & cnn_peaks$F2_ppm <= (box$xmax + margin_x) &
              cnn_peaks$F1_ppm >= (box$ymin - margin_y) & cnn_peaks$F1_ppm <= (box$ymax + margin_y)
          )
        })
        
        valid_ids <- bounding_boxes$stain_id[keep_ids]
        cat("Matched", length(valid_ids), "clusters out of", nrow(bounding_boxes), "\n")
        
        # Debug
        cat("\n--- Testing first 3 boxes manually ---\n")
        for (i in 1:min(3, nrow(bounding_boxes))) {
          box <- bounding_boxes[i, ]
          margin_x <- max((box$xmax - box$xmin) * 0.5, 0.02)
          margin_y <- max((box$ymax - box$ymin) * 0.5, 0.02)
          
          in_box <- cnn_peaks$F2_ppm >= (box$xmin - margin_x) & 
            cnn_peaks$F2_ppm <= (box$xmax + margin_x) &
            cnn_peaks$F1_ppm >= (box$ymin - margin_y) & 
            cnn_peaks$F1_ppm <= (box$ymax + margin_y)
          
          cat(sprintf("Box %s: F2=[%.2f, %.2f] F1=[%.2f, %.2f] -> %d peaks inside\n",
                      box$stain_id, box$xmin, box$xmax, box$ymin, box$ymax, sum(in_box)))
        }
        cat("\n")
        cat("╚══════════════════════════════════════════════════════════════╝\n\n")
        
        if (length(valid_ids) == 0) {
          showNotification("⚠️ No clusters match CNN peaks", type = "error")
          return()
        }
        
        # Filter
        bounding_boxes_filtered <- bounding_boxes %>% dplyr::filter(stain_id %in% valid_ids)
        centroids_filtered <- centroids %>% dplyr::filter(stain_id %in% valid_ids)
        
        # Rename with prefix : cnn_
        bounding_boxes_filtered$stain_id <- paste0("cnn_", seq_len(nrow(bounding_boxes_filtered)))
        centroids_filtered$stain_id <- paste0("cnn_", seq_len(nrow(centroids_filtered)))
        
        # Format for the app
        box_coords_only <- data.frame(
          xmin = bounding_boxes_filtered$xmin,
          xmax = bounding_boxes_filtered$xmax,
          ymin = bounding_boxes_filtered$ymin,
          ymax = bounding_boxes_filtered$ymax,
          stain_id = bounding_boxes_filtered$stain_id,
          stringsAsFactors = FALSE
        )
        
        peaks_df <- data.frame(
          F2_ppm = centroids_filtered$F2_ppm,
          F1_ppm = centroids_filtered$F1_ppm,
          stain_id = centroids_filtered$stain_id,
          stain_intensity = centroids_filtered$Volume,
          stringsAsFactors = FALSE
        )
        
        rv$centroids_data(peaks_df)
        
        incProgress(0.2, detail = "Updating plot...")
        status_msg("🧠 [5/5] CNN: Updating plot...")
        
        rv$fixed_boxes(box_coords_only)
        rv$modifiable_boxes(rv$fixed_boxes())
        rv$reference_boxes(rv$fixed_boxes())
        rv$contour_plot_base(selected_result$plot + ggplot2::labs(title = ""))
        refresh_nmr_plot()
        
        n_peaks <- nrow(peaks_df)
        n_boxes <- nrow(box_coords_only)
        
        incProgress(0.1, detail = "Complete!")
        status_msg(paste0("✅ CNN complete: ", n_peaks, " peaks, ", n_boxes, " boxes"))
        showNotification(paste0("🧠 CNN found ", n_peaks, " peaks and ", n_boxes, " boxes"),
                         type = "message", duration = 4)
        
      }) # End withProgress
    })
    
    
    # RETURN VALUES ----
    return(list(
      eps_value = reactive({ input$eps_value }),
      disable_clustering = reactive({ input$disable_clustering }),
      keep_peak_ranges_text = reactive({ input$keep_peak_ranges_text })
    ))
  })
}