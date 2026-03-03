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
    output$export_boxes <- downloadHandler(
      filename = function() paste0("combined_box_intensities_", Sys.Date(), ".csv"),
      content = function(file) {
        req(data_reactives$result_data_list(), data_reactives$bounding_boxes_data())
        
        boxes_ref <- data_reactives$bounding_boxes_data() %>%
          dplyr::mutate(stain_id = dplyr::row_number(), 
                        F2_ppm = (xmin + xmax)/2, 
                        F1_ppm = (ymin + ymax)/2)
        
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
          dplyr::select(stain_id, F2_ppm, F1_ppm, xmin, xmax, ymin, ymax, 
                        dplyr::starts_with("Intensity_"))
        
        # Use write.csv2 for ";" separator (French Excel compatible)
        write.csv2(final_data, file, row.names = FALSE)
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
    
    invisible(NULL)
  })
}