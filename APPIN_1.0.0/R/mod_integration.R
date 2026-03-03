
# 2D NMR Analyst - Module: Integration ----

# Author: Julien Guibert
# Description: Shiny module for peak integration (AUC and peak fitting methods)



## Module UI ----


#' Integration Module - UI
#'
#' Creates the UI components for the integration section.
#' Includes method selection (Sum/Gaussian/Voigt), fitting options,
#' and results display.
#'
#' @param id Character. The module's namespace ID
#' @return A tagList containing the module's UI elements
#' @export
mod_integration_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Integration method with visual groups
    h5(tags$b("Select method:")),
    
    # AUC Group
    div(
      style = "background: #e8f5e9; border-radius: 8px; padding: 10px; margin-bottom: 10px; border-left: 4px solid #4caf50;",
      tags$b("Area Under Curve (AUC)", style = "color: #2e7d32;"),
      radioButtons(
        ns("integration_method"), 
        NULL,
        choices = c("Sum (AUC)" = "sum"),
        selected = "sum",
        inline = TRUE
      ),
      tags$small("Direct integration of intensity values in the box", style = "color: #666;")
    ),
    
    # Peak Fitting Group
    div(
      style = "background: #e3f2fd; border-radius: 8px; padding: 10px; border-left: 4px solid #2196f3;",
      tags$b("Peak Fitting", style = "color: #1565c0;"),
      radioButtons(
        ns("integration_method_fit"), 
        NULL,
        choices = c(
          "Gaussian" = "gaussian",
          "Voigt (Gaussian + Lorentzian)" = "voigt"
        ),
        selected = character(0),
        inline = TRUE
      ),
      tags$small("Fits a mathematical model to the peak shape", style = "color: #666;")
    ),
    
    # Fitting options (conditional)
    conditionalPanel(
      condition = sprintf(
        "input['%s'] !== undefined && input['%s'] !== null && input['%s'].length > 0",
        ns("integration_method_fit"),
        ns("integration_method_fit"),
        ns("integration_method_fit")
      ),
      div(
        style = "margin-top: 10px; padding: 10px; background: #fff8e1; border-radius: 8px; border-left: 4px solid #ff9800;",
        tags$b("⚙️ Fitting options", style = "color: #e65100;"),
        checkboxInput(ns("show_fit_quality"), "Include R² in export", value = TRUE),
        sliderInput(
          ns("min_r_squared"), 
          "Min R² threshold:",
          min = 0, max = 1, value = 0.7, step = 0.05
        ),
        tags$small("Peaks with R² below threshold will use sum fallback", style = "color: #666;")
      )
    ),
    
    hr(),
    
    # Calculate button
    actionButton(ns("run_integration"), "▶️ Run Integration", class = "btn-success btn-block"),
    
    br(),
    
    # Integration result (conditional)
    conditionalPanel(
      condition = sprintf("output['%s']", ns("integration_done")),
      div(
        style = "margin-top: 10px; padding: 10px; background: #e8f5e9; border-radius: 8px; border: 1px solid #4caf50;",
        h5(tags$b("✅ Integration Results"), style = "color: #2e7d32;"),
        verbatimTextOutput(ns("integration_summary")),
        br(),
        downloadButton(ns("export_integration_results"), "📥 Download Results", class = "btn-primary btn-block")
      )
    )
  )
}


## Module Server ----

#' Integration Module - Server
#'
#' Server logic for the integration module. Handles method selection,
#' peak integration calculations, and results export.
#'
#' @param id Character. The module's namespace ID
#' @param status_msg ReactiveVal. Shared status message reactive value
#' @param load_data List. Return value from mod_load_data_server containing:
#'   \itemize{
#'     \item \code{bruker_data}: Reactive for current spectrum data
#'   }
#' @param rv List. Named list of reactive values:
#'   \itemize{
#'     \item \code{modifiable_boxes}: ReactiveVal for editable boxes
#'     \item \code{fit_results_data}: ReactiveVal for fit results (will be updated)
#'     \item \code{last_fit_method}: ReactiveVal for last fit method used (will be updated)
#'   }
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{effective_integration_method}: Reactive returning the selected method
#'     \item \code{integration_results}: ReactiveVal containing integration results
#'     \item \code{integration_done}: ReactiveVal indicating if integration is complete
#'   }
#' @export
mod_integration_server <- function(id, status_msg, load_data, rv) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    

    # LOCAL REACTIVE VALUES ----

    
    integration_results <- reactiveVal(NULL)
    integration_done <- reactiveVal(FALSE)
    
    # EFFECTIVE INTEGRATION METHOD ----
    
    #' Determine the effective integration method
    #'
    #' Returns the currently selected integration method.
    #' If a fit method is selected, it takes priority over AUC.
    effective_integration_method <- reactive({
      auc_method <- input$integration_method
      fit_method <- input$integration_method_fit
      
      # If a fit method is selected, use it
      if (!is.null(fit_method) && fit_method != "") {
        return(fit_method)
      }
      # Otherwise use AUC (sum)
      return("sum")
    })
    

    # METHOD SELECTION OBSERVERS ----

    
    #' When AUC is selected, deselect Peak Fitting
    observeEvent(input$integration_method, {
      if (!is.null(input$integration_method) && input$integration_method == "sum") {
        updateRadioButtons(session, "integration_method_fit", selected = character(0))
      }
    }, ignoreInit = TRUE)
    
    #' When Peak Fitting is selected, deselect AUC
    observeEvent(input$integration_method_fit, {
      if (!is.null(input$integration_method_fit) && input$integration_method_fit != "") {
        updateRadioButtons(session, "integration_method", selected = character(0))
      }
    }, ignoreInit = TRUE)
    

    # INTEGRATION DONE OUTPUT (for conditionalPanel) ----

    
    output$integration_done <- reactive({
      !is.null(integration_results())
    })
    outputOptions(output, "integration_done", suspendWhenHidden = FALSE)
    

    # RUN INTEGRATION ----

    
    #' Handle "Run Integration" button click
    observeEvent(input$run_integration, {
      req(load_data$bruker_data(), rv$modifiable_boxes())
      
      boxes <- rv$modifiable_boxes()
      if (is.null(boxes) || nrow(boxes) == 0) {
        showNotification("⚠️ No boxes to integrate", type = "warning")
        return()
      }
      
      method <- effective_integration_method()
      model <- if (method %in% c("gaussian", "voigt")) method else "gaussian"
      
      status_msg(paste0("🔄 Running integration (", method, " method)..."))
      
      # Progress bar
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Calculating intensities", value = 0)
      
      tryCatch({
        mat <- load_data$bruker_data()$spectrumData
        ppm_x <- suppressWarnings(as.numeric(colnames(mat)))
        ppm_y <- suppressWarnings(as.numeric(rownames(mat)))
        
        if (method == "sum") {
          # Simple AUC method
          intensities <- sapply(seq_len(nrow(boxes)), function(i) {
            progress$set(value = i / nrow(boxes), detail = paste("Box", i, "/", nrow(boxes)))
            box <- boxes[i, ]
            x_idx <- which(ppm_x >= box$xmin & ppm_x <= box$xmax)
            y_idx <- which(ppm_y >= box$ymin & ppm_y <= box$ymax)
            if (length(x_idx) == 0 || length(y_idx) == 0) return(NA_real_)
            sum(mat[y_idx, x_idx], na.rm = TRUE)
          })
          
          results <- data.frame(
            stain_id = boxes$stain_id,
            F2_ppm = (boxes$xmin + boxes$xmax) / 2,
            F1_ppm = (boxes$ymin + boxes$ymax) / 2,
            intensity = intensities,
            method = "sum",
            r_squared = NA_real_,
            n_peaks = 1L,
            stringsAsFactors = FALSE
          )
          
        } else {
          # Peak Fitting method
          fit_results <- calculate_fitted_volumes(
            mat, ppm_x, ppm_y,
            boxes[, c("xmin", "xmax", "ymin", "ymax", "stain_id")],
            model = model,
            progress_callback = function(value, detail) {
              progress$set(value = value, detail = detail)
            }
          )
          
          results <- data.frame(
            stain_id = fit_results$stain_id,
            F2_ppm = (boxes$xmin + boxes$xmax) / 2,
            F1_ppm = (boxes$ymin + boxes$ymax) / 2,
            intensity = fit_results$volume_fitted,
            method = fit_results$fit_method,
            r_squared = fit_results$r_squared,
            n_peaks = fit_results$n_peaks,
            stringsAsFactors = FALSE
          )
          
          # Also store for the Fit Quality tab
          rv$fit_results_data(
            fit_results %>% 
              dplyr::select(stain_id, r_squared, center_x, center_y, fit_method, n_peaks, is_multiplet)
          )
        }
        
        integration_results(results)
        integration_done(TRUE)
        rv$last_fit_method(method)
        
        status_msg(paste0("✅ Integration complete! ", nrow(results), " boxes processed."))
        showNotification("✅ Integration complete!", type = "message")
        
      }, error = function(e) {
        status_msg(paste0("❌ Error: ", e$message))
        showNotification(paste0("❌ Error: ", e$message), type = "error")
      })
    })
    

    # INTEGRATION SUMMARY OUTPUT ----

    
    output$integration_summary <- renderText({
      results <- integration_results()
      if (is.null(results)) return("No results yet.")
      
      method <- rv$last_fit_method()
      n_total <- nrow(results)
      
      if (method == "sum") {
        paste0(
          "Method: Sum (AUC)\n",
          "Boxes processed: ", n_total, "\n",
          "Total intensity: ", format(sum(results$intensity, na.rm = TRUE), big.mark = ",", scientific = FALSE)
        )
      } else {
        n_fitted <- sum(results$method %in% c("gaussian", "voigt", "multiplet_fit"), na.rm = TRUE)
        n_fallback <- sum(results$method == "sum_fallback", na.rm = TRUE)
        n_multiplets <- sum(results$n_peaks > 1, na.rm = TRUE)
        mean_r2 <- mean(results$r_squared, na.rm = TRUE)
        
        paste0(
          "Method: ", method, " (Peak Fitting)\n",
          "Boxes processed: ", n_total, "\n",
          "  - Successfully fitted: ", n_fitted, "\n",
          "  - Multiplets: ", n_multiplets, "\n",
          "  - Fallback to sum: ", n_fallback, "\n",
          "Mean R²: ", round(mean_r2, 3), "\n",
          "Total intensity: ", format(sum(results$intensity, na.rm = TRUE), big.mark = ",", scientific = FALSE)
        )
      }
    })
    

    # EXPORT INTEGRATION RESULTS ----
    
    output$export_integration_results <- downloadHandler(
      filename = function() {
        method <- rv$last_fit_method()
        paste0("integration_results_", method, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        results <- integration_results()
        if (!is.null(results)) {
          # Use write.csv2 for ";" separator (French Excel compatible)
          write.csv2(results, file, row.names = FALSE)
        }
      }
    )
    

    # RETURN VALUES ----
    
    return(list(
      effective_integration_method = effective_integration_method,
      integration_results = integration_results,
      integration_done = integration_done
    ))
  })
}
