
# 2D NMR Analyst - Module: Peak Picking ----

# Author: Julien Guibert
# Description: Shiny module for automatic peak detection using local maxima
#              and DBSCAN clustering



## Module UI ----


#' Peak Picking Module - UI
#'
#' Creates the UI components for the peak picking section.
#' Includes Local Max button and options for clustering and exclusion zones.
#'
#' @param id Character. The module's namespace ID
#' @return A tagList containing the module's UI elements
#' @export
mod_peak_picking_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Buttons on single line
    div(
      style = "display: flex; gap: 5px; margin-bottom: 10px;",
      actionButton(
        ns("generate_centroids"), 
        "Local Max",
        class = "btn-success btn-sm",
        style = "flex: 1; font-size: 11px; padding: 5px 2px;"
      )
      # CNN button commented out in original
    ),
    
    # Options (collapsible)
    tags$details(
      tags$summary("⚙️ Options"),
      div(
        checkboxInput(ns("disable_clustering"), "No clustering", value = FALSE),
        numericInput(ns("eps_value"), "Epsilon:", value = 0.01, min = 0, step = 0.001),
        textAreaInput(
          ns("keep_peak_ranges_text"), 
          "Delete ranges:",
          value = "0.5,-0.5; 1,0.8; 1.55,1.45", 
          rows = 2
        )
      )
    )
  )
}


## Module Server ----


#' Peak Picking Module - Server
#'
#' Server logic for the peak picking module. Handles automatic peak detection
#' using local maxima detection and optional DBSCAN clustering.
#'
#' @param id Character. The module's namespace ID
#' @param status_msg ReactiveVal. Shared status message reactive value
#' @param load_data List. Return value from mod_load_data_server containing:
#'   \itemize{
#'     \item \code{bruker_data}: Reactive for current spectrum data
#'   }
#' @param data_reactives List. Named list of reactive expressions:
#'   \itemize{
#'     \item \code{result_data_list}: Reactive for result data per spectrum
#'     \item \code{spectrum_params}: Reactive for spectrum-specific parameters
#'     \item \code{calculated_contour_value}: ReactiveVal for calculated threshold
#'   }
#' @param rv List. Named list of reactive values to update:
#'   \itemize{
#'     \item \code{centroids_data}: ReactiveVal for peak centroids
#'     \item \code{fixed_boxes}: ReactiveVal for fixed boxes
#'     \item \code{modifiable_boxes}: ReactiveVal for editable boxes
#'     \item \code{reference_boxes}: ReactiveVal for reference boxes
#'     \item \code{contour_plot_base}: ReactiveVal for contour plot base
#'   }
#' @param refresh_nmr_plot Function. Function to refresh the NMR plot
#' @param parent_input Shiny input. Parent input object for reading:
#'   \itemize{
#'     \item \code{selected_subfolder}: Currently selected spectrum
#'     \item \code{contour_start}: Contour threshold value
#'     \item \code{spectrum_type}: Type of spectrum (TOCSY, HSQC, etc.)
#'   }
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{eps_value}: Reactive returning the current epsilon value
#'   }
#' @export
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

    
    #' Update epsilon value when spectrum type changes
    #' Each spectrum type has optimal epsilon values for DBSCAN clustering
    observeEvent(parent_input$spectrum_type, {
      params <- switch(parent_input$spectrum_type,
                       "TOCSY" = list(eps_value = 0.0068),
                       "HSQC" = list(eps_value = 0.0068),
                       "COSY" = list(eps_value = 0.0068),
                       "UFCOSY" = list(eps_value = 0.014),
                       list(eps_value = 0.0068))  # default
      
      updateNumericInput(session, "eps_value", value = params$eps_value)
    }, ignoreInit = TRUE)
    

    # GENERATE CENTROIDS (Local Max method) ----

    
    #' Handle "Local Max" button click
    #'
    #' Detects peaks using local maxima detection, optionally followed by
    
    #' DBSCAN clustering to group nearby peaks.
    observeEvent(input$generate_centroids, {
      
      req(parent_input$selected_subfolder, data_reactives$result_data_list(), load_data$bruker_data())
      
      params <- data_reactives$spectrum_params()
      
      all_results <- data_reactives$result_data_list()
      selected_result <- all_results[[parent_input$selected_subfolder]]
      
      if (is.null(selected_result)) {
        showNotification("⚠️ No result found", type = "error")
        return()
      }
      
      # Progress message - Step 1
      status_msg("🔄 [1/4] Preparing data...")
      
      selected_spectrum <- load_data$bruker_data()$spectrumData
      if (is.null(selected_spectrum)) {
        showNotification("⚠️ Spectrum not found", type = "error")
        return()
      }
      
      keep_ranges <- parse_keep_peak_ranges(input$keep_peak_ranges_text)
      
      if (input$disable_clustering) {

        ## WITHOUT CLUSTERING ----

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
        
        # Check that bounding_boxes exists and has correct columns
        if (!is.null(result_peaks$bounding_boxes) && nrow(result_peaks$bounding_boxes) > 0) {
          required_cols <- c("xmin", "xmax", "ymin", "ymax", "stain_id")
          if (all(required_cols %in% names(result_peaks$bounding_boxes))) {
            box_coords_only <- result_peaks$bounding_boxes[, required_cols, drop = FALSE]
          } else {
            box_coords_only <- data.frame(
              xmin = numeric(0), xmax = numeric(0),
              ymin = numeric(0), ymax = numeric(0),
              stain_id = character(0)
            )
          }
        } else {
          box_coords_only <- data.frame(
            xmin = numeric(0), xmax = numeric(0),
            ymin = numeric(0), ymax = numeric(0),
            stain_id = character(0)
          )
        }
        
      } else {

        ## WITH DBSCAN CLUSTERING ----

        status_msg("🔄 [2/4] Detecting peaks + DBSCAN clustering...")
        
        # Get calculated contour value
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
        
        if (is.null(result1)) {
          return()
        }
        
        status_msg("🔄 [3/4] Processing centroids...")
        rv$centroids_data(result1$centroids)
        
        # Check that bounding_boxes exists and has correct columns
        if (!is.null(result1$bounding_boxes) && nrow(result1$bounding_boxes) > 0) {
          required_cols <- c("xmin", "xmax", "ymin", "ymax", "stain_id")
          if (all(required_cols %in% names(result1$bounding_boxes))) {
            box_coords_only <- result1$bounding_boxes[, required_cols, drop = FALSE]
          } else {
            box_coords_only <- data.frame(
              xmin = numeric(0), xmax = numeric(0),
              ymin = numeric(0), ymax = numeric(0),
              stain_id = character(0)
            )
          }
        } else {
          box_coords_only <- data.frame(
            xmin = numeric(0), xmax = numeric(0),
            ymin = numeric(0), ymax = numeric(0),
            stain_id = character(0)
          )
        }
      }
      

      # UPDATE BOXES AND PLOT ----

      status_msg("🔄 [4/4] Updating plot...")
      
      rv$fixed_boxes(box_coords_only)
      rv$modifiable_boxes(rv$fixed_boxes())
      rv$reference_boxes(rv$fixed_boxes())
      rv$contour_plot_base(selected_result$plot + ggplot2::labs(title = ""))
      refresh_nmr_plot()
      
      # Final summary
      n_peaks <- nrow(rv$centroids_data() %||% data.frame())
      n_boxes <- nrow(box_coords_only)
      status_msg(paste0("✅ Peak picking complete: ", n_peaks, " peaks, ", n_boxes, " boxes"))
      showNotification(
        paste0("✅ Found ", n_peaks, " peaks and ", n_boxes, " boxes"),
        type = "message",
        duration = 4
      )
    })
    

    # RETURN VALUES ----

    
    return(list(
      eps_value = reactive({ input$eps_value }),
      disable_clustering = reactive({ input$disable_clustering }),
      keep_peak_ranges_text = reactive({ input$keep_peak_ranges_text })
    ))
  })
}