# =============================================================================
# Module: Click Mode
# Description: Handles click modes (two-click box creation, delete on click)
#              and click coordinates display
# =============================================================================

# MODULE UI ----

#' Click Mode Module - UI
#'
#' @param id Character. The module's namespace ID
#' @return A tagList containing click mode controls
#' @export
mod_click_mode_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$details(
      tags$summary("🖱️ Click mode"),
      div(
        radioButtons(
          ns("box_click_mode"), NULL,
          choices = c(
            "Off" = "disabled",
            "Add box (2 clicks)" = "two_clicks",
            "Delete box on click" = "delete_click"
          ),
          selected = "disabled",
          inline = FALSE
        ),
        
        # Two-click mode indicator
        conditionalPanel(
          condition = sprintf("input['%s'] == 'two_clicks'", ns("box_click_mode")),
          uiOutput(ns("two_click_indicator")),
          actionButton(ns("cancel_first_click"), "Cancel", class = "btn-warning btn-xs")
        ),
        
        # Delete mode warning
        conditionalPanel(
          condition = sprintf("input['%s'] == 'delete_click'", ns("box_click_mode")),
          div(
            class = "warning-box",
            style = "padding: 8px; margin: 5px 0;",
            icon("exclamation-triangle"),
            tags$b(" Delete mode active"), br(),
            tags$small("Click inside a box to mark it for deletion")
          )
        ),
        
        # Click coordinates display
        div(class = "click-coords", textOutput(ns("click_coords_display")))
      )
    )
  )
}


# MODULE SERVER ----

#' Click Mode Module - Server
#'
#' @param id Character. The module's namespace ID
#' @param rv List. Shared reactive values (first_click_for_box, last_click_coords, pending_boxes)
#' @param data_reactives List. Reactive expressions (bounding_boxes_data)
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{box_click_mode}: Reactive returning current click mode
#'   }
#' @export
mod_click_mode_server <- function(id, rv, data_reactives) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # CLICK COORDINATES DISPLAY ----
    output$click_coords_display <- renderText({
      click_data <- plotly::event_data("plotly_click", source = "nmr_plot")
      if (is.null(click_data)) return("Click on the spectrum")
      sprintf("F2 = %.4f ppm, F1 = %.4f ppm", -click_data$x, -click_data$y)
    })
    
    # TWO-CLICK INDICATOR ----
    output$two_click_indicator <- renderUI({
      first_click <- rv$first_click_for_box()
      if (is.null(first_click)) {
        div(
          style = "background-color: #e3f2fd; padding: 10px;",
          icon("mouse-pointer"), " Click for first corner"
        )
      } else {
        div(
          style = "background-color: #fff3e0; padding: 10px;",
          icon("check"),
          sprintf(" Corner 1: F2=%.3f, F1=%.3f", first_click$f2, first_click$f1),
          br(),
          icon("mouse-pointer"), " Click for opposite corner"
        )
      }
    })
    
    # CANCEL FIRST CLICK ----
    observeEvent(input$cancel_first_click, {
      rv$first_click_for_box(NULL)
      showNotification("❌ First click cancelled", type = "warning", duration = 2)
    })
    
    # CLICK HANDLING ----
    observeEvent(plotly::event_data("plotly_click", source = "nmr_plot"), {
      click_data <- plotly::event_data("plotly_click", source = "nmr_plot")
      
      # Store last click coordinates
      if (!is.null(click_data) && !is.null(click_data$x) && !is.null(click_data$y) &&
          !is.na(click_data$x) && !is.na(click_data$y)) {
        rv$last_click_coords(list(F2_ppm = click_data$x, F1_ppm = click_data$y))
      }
      
      if (is.null(click_data$x) || is.null(click_data$y)) return()
      if (is.na(click_data$x) || is.na(click_data$y)) return()
      
      f2_ppm <- -click_data$x
      f1_ppm <- -click_data$y
      click_mode <- input$box_click_mode
      
      # DELETE mode
      if (!is.null(click_mode) && click_mode == "delete_click") {
        boxes <- data_reactives$bounding_boxes_data()
        
        if (is.null(boxes) || nrow(boxes) == 0) {
          showNotification("⚠️ No boxes to delete", type = "warning")
          return()
        }
        
        # Find clicked box
        clicked_box_idx <- which(
          boxes$xmin <= f2_ppm & boxes$xmax >= f2_ppm &
            boxes$ymin <= f1_ppm & boxes$ymax >= f1_ppm
        )
        
        if (length(clicked_box_idx) == 0) {
          showNotification("⚠️ No box at this location", type = "warning")
          return()
        }
        
        # If multiple boxes overlap, select the smallest
        if (length(clicked_box_idx) > 1) {
          box_areas <- (boxes$xmax[clicked_box_idx] - boxes$xmin[clicked_box_idx]) *
            (boxes$ymax[clicked_box_idx] - boxes$ymin[clicked_box_idx])
          clicked_box_idx <- clicked_box_idx[which.min(box_areas)]
        }
        
        box_to_delete <- boxes[clicked_box_idx, , drop = FALSE]
        box_to_delete$status <- "delete"
        rv$pending_boxes(dplyr::bind_rows(rv$pending_boxes(), box_to_delete))
        
        showNotification(
          paste("🗑️ Box", box_to_delete$stain_id, "marked for deletion. Click 'Apply' to confirm."),
          type = "message"
        )
        return()
      }
      
      # TWO_CLICKS mode
      if (!is.null(click_mode) && click_mode == "two_clicks") {
        first_click <- rv$first_click_for_box()
        
        if (is.null(first_click)) {
          rv$first_click_for_box(list(f2 = f2_ppm, f1 = f1_ppm))
          showNotification(sprintf("📍 Corner 1: F2=%.3f, F1=%.3f", f2_ppm, f1_ppm), duration = 4)
        } else {
          # Create new box from two corners
          new_box <- data.frame(
            xmin = min(first_click$f2, f2_ppm),
            xmax = max(first_click$f2, f2_ppm),
            ymin = min(first_click$f1, f1_ppm),
            ymax = max(first_click$f1, f1_ppm),
            stain_id = paste0("click_box_", format(Sys.time(), "%H%M%S")),
            Volume = NA_real_,
            status = "add",
            stringsAsFactors = FALSE
          )
          
          rv$pending_boxes(dplyr::bind_rows(rv$pending_boxes(), new_box))
          rv$first_click_for_box(NULL)
          
          showNotification(
            sprintf("🟦 Box: F2=[%.3f,%.3f], F1=[%.3f,%.3f]",
                    new_box$xmin, new_box$xmax, new_box$ymin, new_box$ymax),
            duration = 3
          )
        }
      }
    }, priority = 10)
    
    # RETURN VALUES ----
    return(list(
      box_click_mode = reactive({ input$box_click_mode })
    ))
  })
}
