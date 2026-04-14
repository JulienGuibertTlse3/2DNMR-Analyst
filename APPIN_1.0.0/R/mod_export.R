# =============================================================================
# Module: Export
# Description: Handles CSV export for peaks, boxes, and batch export
# =============================================================================

# MODULE UI ----

#' Export Module - UI
#'
#' @param id Character. The module's namespace ID
#' @return A tagList containing export controls
#' @export
mod_export_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$details(
      tags$summary("📤 Export Data"),
      div(
        fluidRow(
          column(6, downloadButton(ns("export_centroids"), "Peaks", class = "btn-sm btn-block")),
          column(6, downloadButton(ns("export_boxes"), "Boxes", class = "btn-sm btn-block"))
        ),
        br(),
        # Shift tolerance slider for batch export
        div(
          style = "background: #fff3e0; border-radius: 8px; padding: 10px; margin-bottom: 10px; border-left: 4px solid #ff9800;",
          tags$b("⚙️ Batch Export Options", style = "color: #e65100;"),
          sliderInput(
            ns("shift_tolerance_ppm"),
            "Shift tolerance (ppm):",
            min = 0, max = 0.1, value = 0, step = 0.005
          ),
          tags$small(
            "Compensates for chemical shift variations between spectra. ",
            "Each box is recentered on the local maximum within ±tolerance. ",
            "Typical: 0.01-0.03 ppm. Set to 0 to disable.",
            style = "color: #666;"
          )
        ),
        downloadButton(ns("export_batch_box_intensities"), "📤 Batch Export (all spectra)", 
                       class = "btn-primary btn-sm btn-block")
      )
    )
  )
}


# MODULE SERVER ----

#' Export Module - Server
#'
#' @param id Character. The module's namespace ID
#' @param status_msg ReactiveVal. Shared status message reactive value
#' @param rv List. Shared reactive values
#' @param load_data List. Return value from mod_load_data_server
#' @param data_reactives List. Reactive expressions
#'
#' @return NULL (side effects only)
#' @export
mod_export_server <- function(id, status_msg, rv, load_data, data_reactives) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # EXPORT CENTROIDS ----
    output$export_centroids <- downloadHandler(
      filename = function() paste0("centroids_", Sys.Date(), ".csv"),
      content = function(file) {
        df <- rv$centroids_data()
        # Use write.csv2 for ";" separator (French Excel compatible)
        if (!is.null(df) && nrow(df) > 0) {
          # Exclude Volume column from peaks export (Volume is for boxes/integration only)
          export_cols <- setdiff(names(df), c("Volume", "stain_intensity", "intensity_plot"))
          df_export <- df[, export_cols[export_cols %in% names(df)], drop = FALSE]
          write.csv2(df_export, file, row.names = FALSE)
        } else {
          write.csv2(data.frame(), file)
        }
      }
    )
    
    # EXPORT BOXES ----
    # Now uses the same calculate_batch_box_intensities() function as batch export
    # to ensure consistent intensity values across both export methods
    output$export_boxes <- downloadHandler(
      filename = function() {
        method <- data_reactives$effective_integration_method()
        method_suffix <- if (method == "sum") "" else paste0("_", method)
        paste0("box_intensities", method_suffix, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(data_reactives$bounding_boxes_data(), load_data$spectra_list())
        
        # Get chosen method (same as batch export)
        method <- data_reactives$effective_integration_method()
        model <- if (method %in% c("gaussian", "voigt")) method else "gaussian"
        
        status_msg(paste0("🔄 Calculating box intensities (", method, " method)..."))
        
        tryCatch({
          ref_boxes <- data_reactives$bounding_boxes_data()
          
          if (is.null(ref_boxes) || nrow(ref_boxes) == 0) {
            showNotification("⚠️ No boxes found", type = "warning")
            write.csv2(data.frame(message = "No boxes found."), file, row.names = FALSE)
            return(invisible(NULL))
          }
          
          # Use the same function as batch export for consistency
          box_intensities <- calculate_batch_box_intensities(
            reference_boxes = ref_boxes,
            spectra_list = load_data$spectra_list(),
            apply_shift = FALSE,
            method = method,
            model = model,
            progress = NULL  # No progress bar for single export
          )
          
          # Replace negative values with 0 (same as batch export)
          intensity_cols <- grep("^Intensity_", names(box_intensities), value = TRUE)
          for (col in intensity_cols) {
            box_intensities[[col]] <- pmax(box_intensities[[col]], 0, na.rm = TRUE)
          }
          
          # Use write.csv2 for ";" separator (French Excel compatible)
          write.csv2(box_intensities, file, row.names = FALSE)
          
          status_msg("✅ Box export complete")
          showNotification(paste("✅ Exported", nrow(ref_boxes), "boxes"), type = "message")
          
        }, error = function(e) {
          showNotification(paste("❌ Export error:", e$message), type = "error")
          status_msg(paste("❌ Error:", e$message))
        })
      }
    )
    
    # EXPORT BATCH BOX INTENSITIES ----
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
          
          # Get shift tolerance from UI
          shift_tol <- input$shift_tolerance_ppm %||% 0
          
          # Call batch calculation
          batch_intensities <- calculate_batch_box_intensities(
            reference_boxes = ref_boxes,
            spectra_list = load_data$spectra_list(),
            apply_shift = FALSE,
            method = method,
            model = model,
            progress = function(value, detail) {
              progress$set(value = value, detail = detail)
            },
            shift_tolerance_ppm = shift_tol
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
    
    invisible(NULL)
  })
}