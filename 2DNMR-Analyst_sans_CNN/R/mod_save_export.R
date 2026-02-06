
# 2D NMR Analyst - Module: Save & Export ----

# Author: Julien Guibert
# Description: Shiny module for session management, import/export of data



## Module UI ----


#' Save & Export Module - UI
#'
#' Creates the UI components for the save and export section.
#' This includes session save/load, import/export of peaks and boxes,
#' batch export, and reset functionality.
#'
#' @param id Character. The module's namespace ID
#' @return A tagList containing the module's UI elements
#' @export
#'
#' @examples
#' # In UI definition:
#' mod_save_export_ui("save_export")
mod_save_export_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Session save/load - collapsible
    tags$details(
      tags$summary("💼 Session"),
      div(
        fluidRow(
          column(6, 
                 div(style = "margin-top: 8px;",
                     downloadButton(ns("save_session"), "💾 Save", 
                                    class = "btn-success btn-sm btn-block")
                 )
          ),
          column(6, 
                 fileInput(ns("load_session_file"), NULL, accept = ".rds", 
                           buttonLabel = "📂 Load", width = "100%")
          )
        ),
        tags$small("Save/load your complete work (peaks, boxes, parameters)", 
                   style = "color: #666;")
      )
    ),
    
    # Import - collapsible
    tags$details(
      tags$summary("📥 Import Peaks & Boxes"),
      div(
        fileInput(ns("import_centroids_file"), "Peaks CSV:", accept = ".csv"),
        fileInput(ns("import_boxes_file"), "Boxes CSV:", accept = ".csv")
      )
    ),
    
    # Export - collapsible
    tags$details(
      tags$summary("📤 Export Data"),
      div(
        fluidRow(
          column(6, downloadButton(ns("export_centroids"), "Peaks", class = "btn-sm btn-block")),
          column(6, downloadButton(ns("export_boxes"), "Boxes", class = "btn-sm btn-block"))
        ),
        br(),
        downloadButton(ns("export_batch_box_intensities"), "📤 Batch Export (all spectra)", 
                       class = "btn-primary btn-sm btn-block")
      )
    ),
    
    hr(),
    actionButton(ns("reset_all"), "🔄 Reset All", class = "btn-outline-danger btn-sm btn-block")
  )
}


## Module Server ----


#' Save & Export Module - Server
#'
#' Server logic for the save and export module. Handles session management,
#' data import/export, and reset functionality.
#'
#' @param id Character. The module's namespace ID
#' @param status_msg ReactiveVal. Shared status message reactive value
#' @param rv List. Named list of reactive values for data access:
#'   \itemize{
#'     \item \code{centroids_data}: ReactiveVal for peak centroids
#'     \item \code{modifiable_boxes}: ReactiveVal for editable boxes
#'     \item \code{fixed_boxes}: ReactiveVal for fixed boxes
#'     \item \code{reference_boxes}: ReactiveVal for reference boxes
#'     \item \code{pending_centroids}: ReactiveVal for pending peak changes
#'     \item \code{pending_boxes}: ReactiveVal for pending box changes
#'     \item \code{pending_fusions}: ReactiveVal for pending fusions
#'     \item \code{fit_results_data}: ReactiveVal for fit results
#'     \item \code{last_fit_method}: ReactiveVal for last fit method used
#'     \item \code{nmr_plot}: ReactiveVal for NMR plot
#'     \item \code{contour_plot_base}: ReactiveVal for contour plot base
#'     \item \code{plot_cache}: ReactiveVal for plot cache
#'     \item \code{contour_cache}: ReactiveVal for contour cache
#'     \item \code{box_intensity_cache}: ReactiveVal for box intensity cache
#'     \item \code{imported_centroids}: ReactiveVal for imported centroids
#'     \item \code{centroids}: ReactiveVal for centroids
#'     \item \code{first_click_for_box}: ReactiveVal for box drawing state
#'     \item \code{last_click_coords}: ReactiveVal for last click coordinates
#'     \item \code{selected_box_for_edit}: ReactiveVal for selected box edit
#'     \item \code{selected_box_index}: ReactiveVal for selected box index
#'     \item \code{original_box_coords}: ReactiveVal for original box coordinates
#'     \item \code{box_has_been_modified}: ReactiveVal for box modification state
#'     \item \code{preview_trace_added}: ReactiveVal for preview trace state
#'   }
#' @param load_data List. Return value from mod_load_data_server containing:
#'   \itemize{
#'     \item \code{bruker_data}: Reactive for current spectrum data
#'     \item \code{spectra_list}: Reactive for list of loaded spectra
#'   }
#' @param data_reactives List. Named list of reactive expressions:
#'   \itemize{
#'     \item \code{result_data_list}: Reactive for result data per spectrum
#'     \item \code{bounding_boxes_data}: Reactive for bounding boxes
#'     \item \code{effective_integration_method}: Reactive for integration method
#'   }
#' @param refresh_nmr_plot Function. Function to refresh the NMR plot
#' @param parent_session Shiny session. Parent session for updating inputs
#' @param parent_input Shiny input. Parent input object for reading UI values
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{reset_triggered}: Reactive that increments when reset is triggered
#'   }
#' @export
mod_save_export_server <- function(id, 
                                   status_msg, 
                                   rv,
                                   load_data,
                                   data_reactives,
                                   refresh_nmr_plot,
                                   parent_session,
                                   parent_input) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    

    # LOCAL REACTIVE VALUES ----
    
    # Counter to notify parent when reset is triggered
    reset_triggered <- reactiveVal(0)
    
    
    # RESET ALL ----
    
    #' Handle "Reset All" button click
    #'
    #' Resets all reactive values to their initial state
    observeEvent(input$reset_all, {
      
      # Reset plots
      rv$nmr_plot(NULL)
      rv$contour_plot_base(NULL)
      
      # Reset centroids/peaks
      if (!is.null(rv$imported_centroids)) rv$imported_centroids(NULL)
      rv$centroids_data(NULL)
      if (!is.null(rv$centroids)) rv$centroids(NULL)
      
      # Reset boxes - ALL box-related variables
      rv$fixed_boxes(data.frame(xmin = numeric(), xmax = numeric(), 
                                ymin = numeric(), ymax = numeric()))
      rv$modifiable_boxes(data.frame())
      rv$reference_boxes(NULL)
      
      # Reset pending changes
      rv$pending_centroids(data.frame(
        F2_ppm = numeric(0), F1_ppm = numeric(0),
        Volume = numeric(0), stain_id = character(0),
        stringsAsFactors = FALSE
      ))
      rv$pending_boxes(data.frame(
        xmin = numeric(0), xmax = numeric(0),
        ymin = numeric(0), ymax = numeric(0)
      ))
      rv$pending_fusions(data.frame(
        stain_id = character(), F2_ppm = numeric(),
        F1_ppm = numeric(), Volume = numeric(),
        stringsAsFactors = FALSE
      ))
      
      # Reset clicks and selections
      if (!is.null(rv$first_click_for_box)) rv$first_click_for_box(NULL)
      if (!is.null(rv$last_click_coords)) rv$last_click_coords(NULL)
      if (!is.null(rv$selected_box_for_edit)) rv$selected_box_for_edit(NULL)
      if (!is.null(rv$selected_box_index)) rv$selected_box_index(NULL)
      if (!is.null(rv$original_box_coords)) rv$original_box_coords(NULL)
      if (!is.null(rv$box_has_been_modified)) rv$box_has_been_modified(FALSE)
      
      # Reset fit results
      rv$fit_results_data(NULL)
      rv$last_fit_method("sum")
      
      # Reset caches
      rv$plot_cache(list())
      rv$contour_cache(list())
      rv$box_intensity_cache(list())
      
      # Reset UI
      updateSelectInput(parent_session, "selected_subfolder", selected = "")
      
      # Remove preview if present
      if (!is.null(rv$preview_trace_added) && isTRUE(rv$preview_trace_added())) {
        tryCatch({
          plotlyProxy("interactivePlot", parent_session) %>%
            plotlyProxyInvoke("deleteTraces", -1L)
        }, error = function(e) NULL)
        rv$preview_trace_added(FALSE)
      }
      
      # Notify
      status_msg("🔁 All data reset")
      showNotification("🔁 All data has been reset", type = "message")
      
      # Increment counter to notify parent
      reset_triggered(reset_triggered() + 1)
    })
    
    
    # SESSION SAVE ----

    #' Handle session save download
    output$save_session <- downloadHandler(
      filename = function() {
        paste0("nmr_session_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      },
      content = function(file) {
        # Collect all session data
        session_data <- list(
          # Version for future compatibility
          version = "1.0",
          timestamp = Sys.time(),
          
          # Main data
          centroids = rv$centroids_data(),
          boxes = rv$modifiable_boxes(),
          fixed_boxes = rv$fixed_boxes(),
          
          # Pending changes
          pending_centroids = rv$pending_centroids(),
          pending_boxes = rv$pending_boxes(),
          pending_fusions = rv$pending_fusions(),
          
          # Fit results
          fit_results = rv$fit_results_data(),
          last_fit_method = rv$last_fit_method(),
          
          # UI parameters (from parent input)
          params = list(
            spectrum_type = parent_input$spectrum_type,
            contour_start = parent_input$contour_start,
            eps_value = parent_input$eps_value,
            neighborhood_size = parent_input$neighborhood_size,
            exclusion_zones = parent_input$exclusion_zones,
            filter_artifacts = parent_input$filter_artifacts,
            filter_diagonal = parent_input$filter_diagonal,
            diagonal_tolerance = parent_input$diagonal_tolerance,
            integration_method = parent_input$integration_method,
            integration_method_fit = parent_input$integration_method_fit
          ),
          
          # Folder path (for reference)
          data_path = if (!is.null(parent_input$directory)) {
            tryCatch(
              parseDirPath(c(Home = normalizePath("~"), getwd = getwd()), parent_input$directory),
              error = function(e) NULL
            )
          } else NULL
        )
        
        # Save as RDS
        saveRDS(session_data, file)
        showNotification("💾 Session saved successfully!", type = "message")
      }
    )
    

    # SESSION LOAD ----
    
    #' Handle session file upload
    observeEvent(input$load_session_file, {
      req(input$load_session_file)
      
      tryCatch({
        # Load RDS
        session_data <- readRDS(input$load_session_file$datapath)
        
        # Check version
        if (is.null(session_data$version)) {
          showNotification("⚠️ Old session format, some data may not load correctly", type = "warning")
        }
        
        # Clear the caches to force a recalculation
        rv$plot_cache(list())
        rv$contour_cache(list())
        rv$box_intensity_cache(list())
        
        # Restore main data
        if (!is.null(session_data$centroids) && nrow(session_data$centroids) > 0) {
          rv$centroids_data(session_data$centroids)
        }
        
        if (!is.null(session_data$boxes) && nrow(session_data$boxes) > 0) {
          rv$modifiable_boxes(session_data$boxes)
          # Also update fixed_boxes for consistency
          rv$fixed_boxes(session_data$boxes)
        }
        
        if (!is.null(session_data$fixed_boxes) && nrow(session_data$fixed_boxes) > 0) {
          rv$fixed_boxes(session_data$fixed_boxes)
        }
        
        # Restore pending changes
        if (!is.null(session_data$pending_centroids) && nrow(session_data$pending_centroids) > 0) {
          rv$pending_centroids(session_data$pending_centroids)
        }
        
        if (!is.null(session_data$pending_boxes) && nrow(session_data$pending_boxes) > 0) {
          rv$pending_boxes(session_data$pending_boxes)
        }
        
        if (!is.null(session_data$pending_fusions) && nrow(session_data$pending_fusions) > 0) {
          rv$pending_fusions(session_data$pending_fusions)
        }
        
        # Restore fit results
        if (!is.null(session_data$fit_results)) {
          rv$fit_results_data(session_data$fit_results)
        }
        
        if (!is.null(session_data$last_fit_method)) {
          rv$last_fit_method(session_data$last_fit_method)
        }
        
        # Restore UI parameters
        params <- session_data$params
        if (!is.null(params)) {
          if (!is.null(params$spectrum_type)) {
            updateSelectInput(parent_session, "spectrum_type", selected = params$spectrum_type)
          }
          if (!is.null(params$contour_start)) {
            updateNumericInput(parent_session, "contour_start", value = params$contour_start)
          }
          if (!is.null(params$eps_value)) {
            updateNumericInput(parent_session, "eps_value", value = params$eps_value)
          }
          if (!is.null(params$neighborhood_size)) {
            updateNumericInput(parent_session, "neighborhood_size", value = params$neighborhood_size)
          }
          if (!is.null(params$exclusion_zones)) {
            updateTextInput(parent_session, "exclusion_zones", value = params$exclusion_zones)
          }
          if (!is.null(params$filter_artifacts)) {
            updateCheckboxInput(parent_session, "filter_artifacts", value = params$filter_artifacts)
          }
          if (!is.null(params$filter_diagonal)) {
            updateCheckboxInput(parent_session, "filter_diagonal", value = params$filter_diagonal)
          }
          if (!is.null(params$diagonal_tolerance)) {
            updateNumericInput(parent_session, "diagonal_tolerance", value = params$diagonal_tolerance)
          }
        }
        
        # Success message with summary
        n_peaks <- if (!is.null(session_data$centroids)) nrow(session_data$centroids) else 0
        n_boxes <- if (!is.null(session_data$boxes)) nrow(session_data$boxes) else 0
        
        # Refresh plot if spectral data is loaded
        if (!is.null(load_data$bruker_data()) && !is.null(rv$contour_plot_base())) {
          # Force plot regeneration
          refresh_nmr_plot(force_recalc = TRUE)
          
          showNotification(
            paste0("✅ Session loaded! ", n_peaks, " peaks, ", n_boxes, " boxes"),
            type = "message",
            duration = 5
          )
        } else {
          # Spectral data not yet loaded
          showNotification(
            paste0("✅ Session data loaded! ", n_peaks, " peaks, ", n_boxes, " boxes. ",
                   "Load spectrum data to see them on the plot."),
            type = "message",
            duration = 8
          )
        }
        
        status_msg(paste0("📂 Session loaded: ", n_peaks, " peaks, ", n_boxes, " boxes"))
        
        # Show original path if available
        if (!is.null(session_data$data_path)) {
          showNotification(
            paste0("ℹ️ Original data path: ", session_data$data_path),
            type = "message",
            duration = 8
          )
        }
        
      }, error = function(e) {
        showNotification(paste("❌ Error loading session:", e$message), type = "error")
      })
    })
    

    # IMPORT CENTROIDS ----
    
    #' Handle centroids CSV import
    observeEvent(input$import_centroids_file, {
      req(input$import_centroids_file)
      
      imported <- tryCatch(
        read.csv(input$import_centroids_file$datapath, sep = ";", stringsAsFactors = FALSE),
        error = function(e) { 
          showNotification(paste("Import error:", e$message), type = "error")
          NULL 
        }
      )
      
      if (!is.null(imported) && all(c("stain_id", "Volume", "F2_ppm", "F1_ppm") %in% colnames(imported))) {
        rv$centroids_data(clean_centroids_df(imported))
        refresh_nmr_plot()
        showNotification("✅ Centroids imported", type = "message")
      } else {
        showNotification("❌ File must contain: stain_id, Volume, F2_ppm, F1_ppm", type = "error")
      }
    })
    
    
    # IMPORT BOXES ----
    
    #' Handle boxes CSV import
    observeEvent(input$import_boxes_file, {
      req(input$import_boxes_file)
      
      # Try first with semicolon, then with comma
      imported <- tryCatch({
        df <- read.csv(input$import_boxes_file$datapath, sep = ";", stringsAsFactors = FALSE)
        # If only one column, try using comma
        if (ncol(df) == 1) {
          df <- read.csv(input$import_boxes_file$datapath, sep = ",", stringsAsFactors = FALSE)
        }
        df
      }, error = function(e) {
        showNotification(paste("Import error:", e$message), type = "error")
        return(NULL)
      })
      
      if (is.null(imported)) return()
      
      required_cols <- c("stain_id", "xmin", "xmax", "ymin", "ymax")
      
      # Validate imported structure
      if (!all(required_cols %in% colnames(imported))) {
        showNotification(
          paste(
            "❌ File must contain columns:",
            paste(required_cols, collapse = ", "),
            "\nFound:", paste(colnames(imported), collapse = ", ")
          ),
          type = "error",
          duration = 10
        )
        return()
      }
      
      # Keep only required columns (+ Volume if present)
      cols_to_keep <- intersect(c(required_cols, "Volume"), colnames(imported))
      imported <- imported[, cols_to_keep, drop = FALSE]
      
      # Convert types - stain_id remains character!
      imported$stain_id <- as.character(imported$stain_id)
      imported$xmin <- as.numeric(imported$xmin)
      imported$xmax <- as.numeric(imported$xmax)
      imported$ymin <- as.numeric(imported$ymin)
      imported$ymax <- as.numeric(imported$ymax)
      
      # Check for valid data
      valid_rows <- !is.na(imported$xmin) & !is.na(imported$xmax) & 
        !is.na(imported$ymin) & !is.na(imported$ymax)
      
      if (sum(valid_rows) == 0) {
        showNotification("❌ No valid box coordinates found", type = "error")
        return()
      }
      
      imported <- imported[valid_rows, , drop = FALSE]
      
      # Update reactive values
      rv$fixed_boxes(imported)
      rv$modifiable_boxes(imported)
      rv$reference_boxes(imported)
      
      # Force plot refresh
      rv$box_intensity_cache(list())
      refresh_nmr_plot(force_recalc = TRUE)
      
      showNotification(paste("✅", nrow(imported), "bounding boxes imported"), type = "message")
    })
    

    # EXPORT CENTROIDS ----
    
    #' Handle centroids export download
    output$export_centroids <- downloadHandler(
      filename = function() paste0("centroids_", Sys.Date(), ".csv"),
      content = function(file) {
        df <- rv$centroids_data()
        # Use write.csv2 for ";" separator (French Excel compatible)
        if (!is.null(df) && nrow(df) > 0) {
          write.csv2(df, file, row.names = FALSE)
        } else {
          write.csv2(data.frame(), file)
        }
      }
    )
    

    # EXPORT BOXES ----

    #' Handle boxes export download
    output$export_boxes <- downloadHandler(
      filename = function() paste0("combined_box_intensities_", Sys.Date(), ".csv"),
      content = function(file) {
        req(data_reactives$result_data_list(), data_reactives$bounding_boxes_data())
        
        boxes_ref <- data_reactives$bounding_boxes_data() %>%
          dplyr::mutate(stain_id = dplyr::row_number(), 
                        F2_ppm = (xmin + xmax)/2, F1_ppm = (ymin + ymax)/2)
        
        intensity_list <- list()
        for (name in names(data_reactives$result_data_list())) {
          result <- data_reactives$result_data_list()[[name]]
          if (is.null(result$contour_data)) next
          contour_data <- result$contour_data
          
          Intensity <- vapply(seq_len(nrow(boxes_ref)), FUN.VALUE = 0.0, FUN = function(i) {
            rowb <- boxes_ref[i, ]
            sum(contour_data$level[
              contour_data$x >= -rowb$xmax & contour_data$x <= -rowb$xmin &
                contour_data$y >= -rowb$ymax & contour_data$y <= -rowb$ymin
            ], na.rm = TRUE)
          })
          
          tmp_df <- data.frame(stain_id = boxes_ref$stain_id, Intensity = Intensity)
          col_name <- paste0("Intensity_", make.names(basename(name)))
          colnames(tmp_df)[2] <- col_name
          intensity_list[[name]] <- tmp_df
        }
        
        if (length(intensity_list) == 0) {
          write.csv2(data.frame(message = "No contour_data found."), file, row.names = FALSE)
          return(invisible(NULL))
        }
        
        merged_data <- Reduce(function(x, y) dplyr::full_join(x, y, by = "stain_id"), intensity_list)
        final_data <- dplyr::left_join(boxes_ref, merged_data, by = "stain_id") %>%
          dplyr::select(stain_id, F2_ppm, F1_ppm, xmin, xmax, ymin, ymax, dplyr::starts_with("Intensity_"))
        
        # Use write.csv2 for ";" separator (French Excel compatible)
        write.csv2(final_data, file, row.names = FALSE)
      }
    )
    

    # EXPORT BATCH BOX INTENSITIES ----

    #' Handle batch export download
    output$export_batch_box_intensities <- downloadHandler(
      filename = function() {
        method <- data_reactives$effective_integration_method()
        method_suffix <- if (method == "sum") "" else paste0("_", method)
        paste0("batch_box_intensities", method_suffix, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(rv$reference_boxes(), load_data$spectra_list())
        
        # Get chosen method
        method <- data_reactives$effective_integration_method()
        model <- if (method %in% c("gaussian", "voigt")) method else "gaussian"
        
        status_msg(paste0("🔄 Calculating batch intensities (", method, " method)..."))
        
        # Progress bar
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Processing spectra", value = 0)
        
        tryCatch({
          ref_boxes <- rv$reference_boxes()
          
          if (is.null(ref_boxes) || nrow(ref_boxes) == 0) {
            showNotification("⚠️ No reference boxes found", type = "warning")
            return()
          }
          
          # Store used method
          rv$last_fit_method(method)
          
          # Call batch calculation
          batch_intensities <- calculate_batch_box_intensities(
            reference_boxes = ref_boxes,
            spectra_list = load_data$spectra_list(),
            apply_shift = FALSE,
            method = method,
            model = model,
            progress = function(value, detail) {
              progress$set(value = value, detail = detail)
            }
          )
          
          # Replace negative values with 0
          intensity_cols <- grep("^Intensity_", names(batch_intensities), value = TRUE)
          for (col in intensity_cols) {
            batch_intensities[[col]] <- pmax(batch_intensities[[col]], 0, na.rm = TRUE)
          }
          
          # Use write.csv2 for ";" separator (French Excel compatible)
          write.csv2(batch_intensities, file, row.names = FALSE)
          
          status_msg("✅ Batch export complete")
          showNotification(paste("✅ Exported", nrow(ref_boxes), "boxes,", 
                                 length(load_data$spectra_list()), "spectra"), type = "message")
          
        }, error = function(e) {
          showNotification(paste("❌ Export error:", e$message), type = "error")
          status_msg(paste("❌ Error:", e$message))
        })
      }
    )
    

    # RETURN VALUES ----
    
    return(list(
      reset_triggered = reset_triggered
    ))
  })
}
