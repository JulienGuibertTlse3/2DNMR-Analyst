
# 2D NMR Analyst - Module: Manual Editing ----

# Author: Julien Guibert
# Description: Shiny module for manual peak/box editing, including click modes,
#              fusion, move/resize boxes, and apply/discard changes



## Module UI ----


#' Manual Editing Module - UI
#'
#' Creates the UI components for the manual editing section.
#' Includes click mode, fusion, box editing, and manual addition controls.
#'
#' @param id Character. The module's namespace ID
#' @return A tagList containing the module's UI elements
#' @export
mod_manual_editing_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Click mode - collapsible
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
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'two_clicks'", ns("box_click_mode")),
          uiOutput(ns("two_click_indicator")),
          actionButton(ns("cancel_first_click"), "Cancel", class = "btn-warning btn-xs")
        ),
        
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
        
        div(class = "click-coords", textOutput(ns("click_coords_display")))
      )
    ),
    
    # Fusing Peaks and Boxes - collapsible
    tags$details(
      tags$summary("🔗 Fusing Peaks and Boxes"),
      div(
        actionButton(ns("fuse_btn"), "🔗 Fuse Selected", class = "btn-warning btn-sm btn-block")
      )
    ),
    
    # Edit box - compact version
    tags$details(
      tags$summary("📦 Edit selected box"),
      div(
        verbatimTextOutput(ns("selected_box_info")),
        
        # Coordinates in 2 compact lines
        div(
          class = "edit-box-inputs",
          fluidRow(
            column(6, numericInput(ns("edit_box_xmin"), "xmin:", value = NA, step = 0.01)),
            column(6, numericInput(ns("edit_box_xmax"), "xmax:", value = NA, step = 0.01))
          ),
          fluidRow(
            column(6, numericInput(ns("edit_box_ymin"), "ymin:", value = NA, step = 0.01)),
            column(6, numericInput(ns("edit_box_ymax"), "ymax:", value = NA, step = 0.01))
          )
        ),
        
        # Step and Move buttons on the same line
        div(
          style = "display: flex; align-items: flex-end; gap: 10px; margin-top: 10px;",
          
          # Step input compact
          div(
            class = "step-input-compact",
            style = "width: 70px;",
            numericInput(ns("move_box_step"), "Step:", value = 0.01, min = 0.001, step = 0.005)
          ),
          
          # Move buttons grid
          div(
            class = "move-btn-grid",
            # Line 1
            div(),
            actionButton(ns("move_box_up"), "↑", class = "btn-default btn-xs"),
            div(),
            # Line 2
            actionButton(ns("move_box_left"), "←", class = "btn-default btn-xs"),
            div(
              style = "display: flex; gap: 1px;",
              actionButton(ns("shrink_box"), "−", class = "btn-warning btn-xs"),
              actionButton(ns("expand_box"), "+", class = "btn-success btn-xs")
            ),
            actionButton(ns("move_box_right"), "→", class = "btn-default btn-xs"),
            # Line 3
            div(),
            actionButton(ns("move_box_down"), "↓", class = "btn-default btn-xs"),
            div()
          )
        ),
        
        br(),
        actionButton(ns("apply_box_edit"), "Apply Edit", class = "btn-primary btn-sm btn-block")
      )
    ),
    
    # Add manually
    tags$details(
      tags$summary("➕ Add manually"),
      div(
        tags$b("Peak:"),
        fluidRow(
          column(5, numericInput(ns("manual_f2"), "F2:", value = 4.0, step = 0.01)),
          column(5, numericInput(ns("manual_f1"), "F1:", value = 3.5, step = 0.01))
        ),
        actionButton(ns("add_manual_centroid"), "Add Peak", class = "btn-info btn-sm btn-block"),
        hr(),
        tags$b("Box:"),
        fluidRow(
          column(6, numericInput(ns("manual_xmin"), "xmin:", value = 3.5, step = 0.01)),
          column(6, numericInput(ns("manual_xmax"), "xmax:", value = 4.0, step = 0.01))
        ),
        fluidRow(
          column(6, numericInput(ns("manual_ymin"), "ymin:", value = 2.0, step = 0.01)),
          column(6, numericInput(ns("manual_ymax"), "ymax:", value = 3.0, step = 0.01))
        ),
        actionButton(ns("add_manual_bbox"), "Add Box", class = "btn-info btn-sm btn-block")
      )
    ),
    
    # Apply/Discard buttons
    fluidRow(
      column(6, actionButton(ns("apply_changes"), "✅ Apply", class = "btn-success btn-sm btn-block")),
      column(6, actionButton(ns("discard_changes"), "❌ Discard", class = "btn-secondary btn-sm btn-block"))
    )
  )
}


## Module Server ----


#' Manual Editing Module - Server
#'
#' Server logic for the manual editing module. Handles all manual peak/box
#' editing operations including add, delete, move, resize, fuse, and apply/discard.
#'
#' @param id Character. The module's namespace ID
#' @param status_msg ReactiveVal. Shared status message reactive value
#' @param load_data List. Return value from mod_load_data_server
#' @param rv List. Named list of reactive values (centroids_data, modifiable_boxes, etc.)
#' @param data_reactives List. Named list of reactive expressions
#' @param refresh_nmr_plot Function. Function to refresh the NMR plot
#' @param peak_picking List. Return value from mod_peak_picking_server (for eps_value)
#' @param parent_input Shiny input. Parent input object for table selections
#' @param parent_session Shiny session. Parent session for plotlyProxy
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{box_click_mode}: Reactive returning the current click mode
#'   }
#' @export
mod_manual_editing_server <- function(id,
                                      status_msg,
                                      load_data,
                                      rv,
                                      data_reactives,
                                      refresh_nmr_plot,
                                      peak_picking,
                                      parent_input,
                                      parent_session) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    

    # NOTE: Edit state reactive values are in rv (shared with other modules)
    # rv$selected_box_for_edit, rv$selected_box_index, rv$original_box_coords,
    # rv$box_has_been_modified, rv$preview_trace_added

    
    # Local reactive for preview trace index
    preview_trace_index <- reactiveVal(NULL)
    

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
    
 
    # ADD MANUAL CENTROID ----

    
    observeEvent(input$add_manual_centroid, {
      req(input$manual_f2, input$manual_f1)
      
      current <- rv$centroids_data()
      if (is.null(current)) {
        current <- data.frame(
          F2_ppm = numeric(0), F1_ppm = numeric(0),
          Volume = numeric(0), stain_id = character(0)
        )
      }
      
      existing_ids <- current$stain_id[grepl("^man", current$stain_id)]
      man_number <- if (length(existing_ids) == 0) 1 else
        max(as.integer(sub("man", "", existing_ids)), na.rm = TRUE) + 1
      
      contour_dat <- data_reactives$result_data()$contour_data
      eps <- peak_picking$eps_value()
      estimated_intensity <- 0
      
      if (!is.null(contour_dat) && nrow(contour_dat) > 0) {
        local_points <- contour_dat[
          abs(contour_dat$x + input$manual_f2) <= eps * 16 &
            abs(contour_dat$y + input$manual_f1) <= eps * 16, , drop = FALSE
        ]
        estimated_intensity <- sum(local_points$level, na.rm = TRUE)
      }
      
      new_point <- data.frame(
        F2_ppm = as.numeric(input$manual_f2),
        F1_ppm = as.numeric(input$manual_f1),
        Volume = as.numeric(estimated_intensity),
        stain_id = paste0("man", man_number),
        status = "add",
        stringsAsFactors = FALSE
      )
      
      missing_cols <- setdiff(colnames(current), colnames(new_point))
      for (mc in missing_cols) new_point[[mc]] <- NA
      
      rv$pending_centroids(dplyr::bind_rows(rv$pending_centroids(), new_point))
      showNotification(paste("✅ Peak added:", new_point$stain_id), type = "message")
    })
    
    # ADD MANUAL BOX ----

    
    observeEvent(input$add_manual_bbox, {
      req(input$manual_xmin, input$manual_xmax, input$manual_ymin, input$manual_ymax)
      
      current_boxes <- rv$modifiable_boxes()
      existing_manual_ids <- if (!is.null(current_boxes) && nrow(current_boxes) > 0 &&
                                   "stain_id" %in% names(current_boxes)) {
        current_boxes$stain_id[grepl("^manual_box", current_boxes$stain_id)]
      } else {
        character(0)
      }
      
      manual_number <- if (length(existing_manual_ids) == 0) 1 else
        max(as.integer(sub("manual_box", "", existing_manual_ids)), na.rm = TRUE) + 1
      
      new_box <- data.frame(
        xmin = input$manual_xmin, xmax = input$manual_xmax,
        ymin = input$manual_ymin, ymax = input$manual_ymax,
        stain_id = paste0("manual_box", manual_number),
        Volume = NA_real_, status = "add",
        stringsAsFactors = FALSE
      )
      
      rv$pending_boxes(dplyr::bind_rows(rv$pending_boxes(), new_box))
      showNotification(paste("🟦 Box added:", new_box$stain_id), type = "message")
    })
    

    # DELETE CENTROID (from table selection) ----

    
    observeEvent(parent_input$delete_centroid, {
      selected <- parent_input$centroid_table_rows_selected
      if (length(selected) > 0) {
        current <- rv$centroids_data()
        to_delete <- current[selected, , drop = FALSE]
        to_delete$status <- "delete"
        rv$pending_centroids(dplyr::bind_rows(rv$pending_centroids(), to_delete))
        rv$centroids_data(current[-selected, , drop = FALSE])
        showNotification("🗑️ Centroid marked for deletion", type = "message")
      } else {
        showNotification("⚠️ Select a centroid first", type = "warning")
      }
    })
    

    # DELETE BOX (from table selection) ----

    
    observeEvent(parent_input$delete_bbox, {
      selected <- parent_input$bbox_table_rows_selected
      if (length(selected) > 0) {
        current <- rv$modifiable_boxes()
        to_delete <- current[selected, , drop = FALSE]
        to_delete$status <- "delete"
        rv$pending_boxes(dplyr::bind_rows(rv$pending_boxes(), to_delete))
        showNotification(paste("🗑️ Box marked for deletion:", to_delete$stain_id[1]), type = "message")
      } else {
        showNotification("⚠️ Select a box first", type = "warning")
      }
    })
    

    # EDIT/MOVE BOX - Selection handler ----

    
    observeEvent(parent_input$bbox_table_rows_selected, {
      selected <- parent_input$bbox_table_rows_selected
      
      if (length(selected) > 0 && !is.null(selected)) {
        first_selected <- selected[1]
        boxes <- data_reactives$bounding_boxes_data()
        
        if (!is.null(boxes) && nrow(boxes) >= first_selected) {
          box <- boxes[first_selected, , drop = FALSE]
          rv$selected_box_for_edit(box)
          rv$selected_box_index(first_selected)
          rv$box_has_been_modified(FALSE)
          
          rv$original_box_coords(list(
            xmin = box$xmin, xmax = box$xmax,
            ymin = box$ymin, ymax = box$ymax
          ))
          
          updateNumericInput(session, "edit_box_xmin", value = round(box$xmin, 4))
          updateNumericInput(session, "edit_box_xmax", value = round(box$xmax, 4))
          updateNumericInput(session, "edit_box_ymin", value = round(box$ymin, 4))
          updateNumericInput(session, "edit_box_ymax", value = round(box$ymax, 4))
          
          # Preview coordinates
          x0 <- -box$xmin; x1 <- -box$xmax
          y0 <- -box$ymin; y1 <- -box$ymax
          if (x0 > x1) { tmp <- x0; x0 <- x1; x1 <- tmp }
          if (y0 > y1) { tmp <- y0; y0 <- y1; y1 <- tmp }
          
          # Add preview trace
          plotly::plotlyProxy("interactivePlot", parent_session) %>%
            plotly::plotlyProxyInvoke(
              "addTraces",
              list(
                x = c(x0, x1, x1, x0, x0),
                y = c(y0, y0, y1, y1, y0),
                type = "scatter", mode = "lines",
                line = list(color = "lime", width = 3, dash = "dash"),
                hoverinfo = "text",
                text = paste("Preview:", box$stain_id),
                showlegend = FALSE,
                name = "preview_box"
              )
            )
          
          rv$preview_trace_added(TRUE)
        }
      } else {
        # Deselection
        if (isTRUE(rv$preview_trace_added())) {
          plotly::plotlyProxy("interactivePlot", parent_session) %>%
            plotly::plotlyProxyInvoke("deleteTraces", -1L)
          rv$preview_trace_added(FALSE)
        }
        
        rv$selected_box_for_edit(NULL)
        rv$selected_box_index(NULL)
        rv$original_box_coords(NULL)
        rv$box_has_been_modified(FALSE)
      }
    })
    

    # MOVE BOX HANDLERS ----

    
    observeEvent(input$move_box_up, {
      req(rv$selected_box_for_edit())
      delta <- input$move_box_step %||% 0.01
      updateNumericInput(session, "edit_box_ymin", value = input$edit_box_ymin - delta)
      updateNumericInput(session, "edit_box_ymax", value = input$edit_box_ymax - delta)
      rv$box_has_been_modified(TRUE)
    })
    
    observeEvent(input$move_box_down, {
      req(rv$selected_box_for_edit())
      delta <- input$move_box_step %||% 0.01
      updateNumericInput(session, "edit_box_ymin", value = input$edit_box_ymin + delta)
      updateNumericInput(session, "edit_box_ymax", value = input$edit_box_ymax + delta)
      rv$box_has_been_modified(TRUE)
    })
    
    observeEvent(input$move_box_left, {
      req(rv$selected_box_for_edit())
      delta <- input$move_box_step %||% 0.01
      updateNumericInput(session, "edit_box_xmin", value = input$edit_box_xmin + delta)
      updateNumericInput(session, "edit_box_xmax", value = input$edit_box_xmax + delta)
      rv$box_has_been_modified(TRUE)
    })
    
    observeEvent(input$move_box_right, {
      req(rv$selected_box_for_edit())
      delta <- input$move_box_step %||% 0.01
      updateNumericInput(session, "edit_box_xmin", value = input$edit_box_xmin - delta)
      updateNumericInput(session, "edit_box_xmax", value = input$edit_box_xmax - delta)
      rv$box_has_been_modified(TRUE)
    })
    

    # RESIZE BOX HANDLERS ----

    
    observeEvent(input$expand_box, {
      req(rv$selected_box_for_edit())
      delta <- input$move_box_step %||% 0.01
      updateNumericInput(session, "edit_box_xmin", value = input$edit_box_xmin - delta)
      updateNumericInput(session, "edit_box_xmax", value = input$edit_box_xmax + delta)
      updateNumericInput(session, "edit_box_ymin", value = input$edit_box_ymin - delta)
      updateNumericInput(session, "edit_box_ymax", value = input$edit_box_ymax + delta)
      rv$box_has_been_modified(TRUE)
    })
    
    observeEvent(input$shrink_box, {
      req(rv$selected_box_for_edit())
      delta <- input$move_box_step %||% 0.01
      updateNumericInput(session, "edit_box_xmin", value = input$edit_box_xmin + delta)
      updateNumericInput(session, "edit_box_xmax", value = input$edit_box_xmax - delta)
      updateNumericInput(session, "edit_box_ymin", value = input$edit_box_ymin + delta)
      updateNumericInput(session, "edit_box_ymax", value = input$edit_box_ymax - delta)
      rv$box_has_been_modified(TRUE)
    })
    

    # PREVIEW UPDATE ON INPUT CHANGE ----

    
    observeEvent(c(input$edit_box_xmin, input$edit_box_xmax, input$edit_box_ymin, input$edit_box_ymax), {
      req(rv$selected_box_index())
      req(isTRUE(rv$preview_trace_added()))
      
      x0 <- -input$edit_box_xmin; x1 <- -input$edit_box_xmax
      y0 <- -input$edit_box_ymin; y1 <- -input$edit_box_ymax
      if (x0 > x1) { tmp <- x0; x0 <- x1; x1 <- tmp }
      if (y0 > y1) { tmp <- y0; y0 <- y1; y1 <- tmp }
      
      plotly::plotlyProxy("interactivePlot", parent_session) %>%
        plotly::plotlyProxyInvoke("deleteTraces", -1L) %>%
        plotly::plotlyProxyInvoke(
          "addTraces",
          list(
            x = c(x0, x1, x1, x0, x0),
            y = c(y0, y0, y1, y1, y0),
            type = "scatter", mode = "lines",
            line = list(color = "lime", width = 3, dash = "dash"),
            hoverinfo = "text", text = "Preview (modified)",
            showlegend = FALSE, name = "preview_box"
          )
        )
      
      original <- rv$original_box_coords()
      if (!is.null(original)) {
        if (abs(input$edit_box_xmin - original$xmin) > 1e-6 ||
            abs(input$edit_box_xmax - original$xmax) > 1e-6 ||
            abs(input$edit_box_ymin - original$ymin) > 1e-6 ||
            abs(input$edit_box_ymax - original$ymax) > 1e-6) {
          rv$box_has_been_modified(TRUE)
        }
      }
    }, ignoreInit = TRUE)
    

    # SELECTED BOX INFO OUTPUT ----
    
    output$selected_box_info <- renderText({
      box <- rv$selected_box_for_edit()
      modified <- rv$box_has_been_modified()
      if (is.null(box)) return("No box selected")
      status <- if (modified) " (modified)" else ""
      sprintf("Editing: %s%s", box$stain_id, status)
    })

        
    # APPLY BOX EDIT ----

    observeEvent(input$apply_box_edit, {
      box_to_edit <- rv$selected_box_for_edit()
      if (is.null(box_to_edit)) {
        showNotification("⚠️ Select a box first", type = "warning")
        return()
      }
      
      new_xmin <- input$edit_box_xmin
      new_xmax <- input$edit_box_xmax
      new_ymin <- input$edit_box_ymin
      new_ymax <- input$edit_box_ymax
      
      # Validation
      coords_valid <- TRUE
      error_msg <- ""
      
      if (is.na(new_xmin) || is.na(new_xmax) || is.na(new_ymin) || is.na(new_ymax)) {
        coords_valid <- FALSE
        error_msg <- "Coordinates cannot be empty (NA)"
      } else if (new_xmin == 0 && new_xmax == 0 && new_ymin == 0 && new_ymax == 0) {
        coords_valid <- FALSE
        error_msg <- "All coordinates cannot be zero. Use 'Delete' to remove a box."
      } else if (new_xmin >= new_xmax) {
        coords_valid <- FALSE
        error_msg <- "xmin must be less than xmax"
      } else if (new_ymin >= new_ymax) {
        coords_valid <- FALSE
        error_msg <- "ymin must be less than ymax"
      } else if ((new_xmax - new_xmin) < 0.001 || (new_ymax - new_ymin) < 0.001) {
        coords_valid <- FALSE
        error_msg <- "Box is too small (min size: 0.001 ppm)"
      }
      
      if (!coords_valid) {
        showNotification(paste("❌ Invalid coordinates:", error_msg), type = "error", duration = 5)
        return()
      }
      
      if (!rv$box_has_been_modified()) {
        showNotification("ℹ️ No changes to apply", type = "message")
        if (rv$preview_trace_added()) {
          plotly::plotlyProxy("interactivePlot", parent_session) %>%
            plotly::plotlyProxyInvoke("deleteTraces", list(-1))
          rv$preview_trace_added(FALSE)
        }
        rv$selected_box_for_edit(NULL)
        rv$selected_box_index(NULL)
        rv$original_box_coords(NULL)
        rv$box_has_been_modified(FALSE)
        return()
      }
      
      edited_box <- data.frame(
        xmin = new_xmin, xmax = new_xmax,
        ymin = new_ymin, ymax = new_ymax,
        stain_id = box_to_edit$stain_id,
        Volume = NA_real_, status = "edit",
        original_stain_id = box_to_edit$stain_id,
        stringsAsFactors = FALSE
      )
      
      rv$pending_boxes(dplyr::bind_rows(rv$pending_boxes(), edited_box))
      
      if (isTRUE(rv$preview_trace_added())) {
        plotly::plotlyProxy("interactivePlot", parent_session) %>%
          plotly::plotlyProxyInvoke("deleteTraces", -1L)
        rv$preview_trace_added(FALSE)
      }
      
      rv$selected_box_for_edit(NULL)
      rv$selected_box_index(NULL)
      rv$original_box_coords(NULL)
      rv$box_has_been_modified(FALSE)
      
      showNotification(paste("✏️ Box edit pending:", box_to_edit$stain_id), type = "message")
    })
    

    # FUSE POINTS ----

    
    observeEvent(input$fuse_btn, {
      req(rv$centroids_data())
      sel <- plotly::event_data("plotly_selected", source = "nmr_plot")
      
      if (is.null(sel) || nrow(sel) < 2) {
        showNotification("⚠️ Select at least 2 points", type = "error")
        return()
      }
      
      sel$x <- -sel$x
      sel$y <- -sel$y
      brushed <- dplyr::semi_join(rv$centroids_data(), sel, by = c("F2_ppm" = "x", "F1_ppm" = "y"))
      
      if (nrow(brushed) < 2) {
        showNotification("⚠️ Selection did not match enough points", type = "error")
        return()
      }
      
      first_peak_id <- brushed$stain_id[1]
      peak_number <- gsub("[^0-9]", "", first_peak_id)
      
      fused_point <- data.frame(
        stain_id = paste0("fused_point", peak_number),
        F2_ppm = mean(brushed$F2_ppm),
        F1_ppm = mean(brushed$F1_ppm),
        Volume = sum(as.numeric(brushed$Volume), na.rm = TRUE),
        stringsAsFactors = FALSE
      )
      
      remaining <- dplyr::anti_join(rv$centroids_data(), brushed, by = c("F2_ppm", "F1_ppm"))
      missing_cols <- setdiff(names(remaining), names(fused_point))
      for (mc in missing_cols) fused_point[[mc]] <- NA
      fused_point <- fused_point[, names(remaining), drop = FALSE]
      
      rv$centroids_data(rbind(remaining, fused_point))
      
      # Fuse boxes as well
      if (!is.null(rv$modifiable_boxes()) && nrow(rv$modifiable_boxes()) > 0) {
        boxes <- rv$modifiable_boxes()
        selected_boxes <- which(
          boxes$xmin <= max(brushed$F2_ppm) & boxes$xmax >= min(brushed$F2_ppm) &
            boxes$ymin <= max(brushed$F1_ppm) & boxes$ymax >= min(brushed$F1_ppm)
        )
        removed_boxes <- if (length(selected_boxes) > 0) boxes[selected_boxes, , drop = FALSE] else boxes[0, ]
        boxes <- if (length(selected_boxes) > 0) boxes[-selected_boxes, , drop = FALSE] else boxes
        
        if (nrow(removed_boxes) > 0) {
          new_box <- data.frame(
            xmin = min(removed_boxes$xmin), xmax = max(removed_boxes$xmax),
            ymin = min(removed_boxes$ymin), ymax = max(removed_boxes$ymax),
            stain_id = paste0("bbox_fused_point", peak_number)
          )
          missing_box_cols <- setdiff(names(boxes), names(new_box))
          for (mc in missing_box_cols) new_box[[mc]] <- NA
          new_box <- new_box[, names(boxes), drop = FALSE]
          boxes <- rbind(boxes, new_box)
        }
        rv$modifiable_boxes(boxes)
        rv$fixed_boxes(boxes)
      }
      
      rv$pending_fusions(dplyr::bind_rows(rv$pending_fusions(), fused_point))
      showNotification("✅ Points fused", type = "message")
    })
    

    # CLICK HANDLING FOR TWO-CLICK BOX CREATION ----

    
    observeEvent(plotly::event_data("plotly_click", source = "nmr_plot"), {
      click_data <- plotly::event_data("plotly_click", source = "nmr_plot")
      
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
        
        clicked_box_idx <- which(
          boxes$xmin <= f2_ppm & boxes$xmax >= f2_ppm &
            boxes$ymin <= f1_ppm & boxes$ymax >= f1_ppm
        )
        
        if (length(clicked_box_idx) == 0) {
          showNotification("⚠️ No box at this location", type = "warning")
          return()
        }
        
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
          showNotification(sprintf("📍 Coin 1: F2=%.3f, F1=%.3f", f2_ppm, f1_ppm), duration = 4)
        } else {
          new_box <- data.frame(
            xmin = min(first_click$f2, f2_ppm),
            xmax = max(first_click$f2, f2_ppm),
            ymin = min(first_click$f1, f1_ppm),
            ymax = max(first_click$f1, f1_ppm),
            stain_id = paste0("click_box_", format(Sys.time(), "%H%M%S")),
            Volume = NA_real_, status = "add",
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
    

    # CANCEL FIRST CLICK ----
    
    observeEvent(input$cancel_first_click, {
      rv$first_click_for_box(NULL)
      showNotification("❌ First click cancelled", type = "warning", duration = 2)
    })
    

    # APPLY CHANGES ----
    
    observeEvent(input$apply_changes, {
      current_centroids <- rv$centroids_data()
      pending_cents <- rv$pending_centroids()
      
      # Handle pending centroids
      if (!is.null(pending_cents) && nrow(pending_cents) > 0) {
        if (is.null(current_centroids)) current_centroids <- data.frame()
        
        if ("status" %in% names(pending_cents)) {
          cents_to_add <- pending_cents[is.na(pending_cents$status) | pending_cents$status != "delete", , drop = FALSE]
          cents_to_delete <- pending_cents[!is.na(pending_cents$status) & pending_cents$status == "delete", , drop = FALSE]
          
          if (nrow(cents_to_delete) > 0 && nrow(current_centroids) > 0) {
            if ("stain_id" %in% names(cents_to_delete) && "stain_id" %in% names(current_centroids)) {
              ids_to_delete <- cents_to_delete$stain_id
              current_centroids <- current_centroids[!current_centroids$stain_id %in% ids_to_delete, , drop = FALSE]
            }
          }
          
          if (nrow(cents_to_add) > 0) {
            cents_to_add$status <- NULL
            current_centroids <- dplyr::bind_rows(current_centroids, cents_to_add)
          }
        } else {
          current_centroids <- dplyr::bind_rows(current_centroids, pending_cents)
        }
        
        rv$centroids_data(current_centroids)
      }
      
      # Handle pending boxes
      current_boxes <- rv$modifiable_boxes()
      pending_bxs <- rv$pending_boxes()
      
      if (!is.null(pending_bxs) && nrow(pending_bxs) > 0) {
        if (!"status" %in% names(pending_bxs)) pending_bxs$status <- "add"
        pending_bxs$status[is.na(pending_bxs$status)] <- "add"
        
        boxes_to_add <- pending_bxs[pending_bxs$status == "add", , drop = FALSE]
        boxes_to_delete <- pending_bxs[pending_bxs$status == "delete", , drop = FALSE]
        boxes_to_edit <- pending_bxs[pending_bxs$status == "edit", , drop = FALSE]
        
        if (is.null(current_boxes)) {
          current_boxes <- data.frame(
            xmin = numeric(0), xmax = numeric(0),
            ymin = numeric(0), ymax = numeric(0),
            stain_id = character(0), Volume = numeric(0),
            stringsAsFactors = FALSE
          )
        }
        
        # Process deletions
        if (nrow(boxes_to_delete) > 0 && nrow(current_boxes) > 0) {
          ids_to_delete <- boxes_to_delete$stain_id
          current_boxes <- current_boxes[!current_boxes$stain_id %in% ids_to_delete, , drop = FALSE]
        }
        
        # Process edits
        if (nrow(boxes_to_edit) > 0 && nrow(current_boxes) > 0) {
          for (i in seq_len(nrow(boxes_to_edit))) {
            edit_row <- boxes_to_edit[i, ]
            original_id <- if ("original_stain_id" %in% names(edit_row) && !is.na(edit_row$original_stain_id)) {
              edit_row$original_stain_id
            } else {
              edit_row$stain_id
            }
            
            box_idx <- which(current_boxes$stain_id == original_id)
            
            if (length(box_idx) > 0) {
              current_boxes[box_idx, "xmin"] <- edit_row$xmin
              current_boxes[box_idx, "xmax"] <- edit_row$xmax
              current_boxes[box_idx, "ymin"] <- edit_row$ymin
              current_boxes[box_idx, "ymax"] <- edit_row$ymax
              
              mat <- load_data$bruker_data()$spectrumData
              if (!is.null(mat)) {
                ppm_x <- suppressWarnings(as.numeric(colnames(mat)))
                ppm_y <- suppressWarnings(as.numeric(rownames(mat)))
                current_boxes[box_idx, "Volume"] <- get_box_intensity(
                  mat, ppm_x, ppm_y, current_boxes[box_idx, , drop = FALSE]
                )
              }
            }
          }
        }
        
        # Process additions
        if (nrow(boxes_to_add) > 0) {
          if (!"stain_id" %in% names(boxes_to_add) || any(is.na(boxes_to_add$stain_id))) {
            boxes_to_add$stain_id <- paste0("box_", seq_len(nrow(boxes_to_add)))
          }
          
          mat <- load_data$bruker_data()$spectrumData
          if (!is.null(mat)) {
            ppm_x <- suppressWarnings(as.numeric(colnames(mat)))
            ppm_y <- suppressWarnings(as.numeric(rownames(mat)))
            boxes_to_add$Volume <- get_box_intensity(mat, ppm_x, ppm_y, boxes_to_add)
          }
          
          cols_to_remove <- c("status", "original_stain_id")
          boxes_to_add <- boxes_to_add[, !names(boxes_to_add) %in% cols_to_remove, drop = FALSE]
          
          all_cols <- unique(c(names(current_boxes), names(boxes_to_add)))
          for (col in all_cols) {
            if (!col %in% names(current_boxes)) current_boxes[[col]] <- NA
            if (!col %in% names(boxes_to_add)) boxes_to_add[[col]] <- NA
          }
          boxes_to_add <- boxes_to_add[, names(current_boxes), drop = FALSE]
          current_boxes <- rbind(current_boxes, boxes_to_add)
        }
        
        cols_to_clean <- c("status", "original_stain_id")
        for (col in cols_to_clean) {
          if (col %in% names(current_boxes)) current_boxes[[col]] <- NULL
        }
        
        rv$modifiable_boxes(current_boxes)
        rv$fixed_boxes(current_boxes)
        rv$reference_boxes(current_boxes)
      }
      
      # Reset pending
      rv$pending_centroids(data.frame(
        F2_ppm = numeric(0), F1_ppm = numeric(0),
        Volume = numeric(0), stain_id = character(0),
        stringsAsFactors = FALSE
      ))
      rv$pending_boxes(data.frame(
        xmin = numeric(0), xmax = numeric(0),
        ymin = numeric(0), ymax = numeric(0),
        stain_id = character(0), Volume = numeric(0), status = character(0),
        stringsAsFactors = FALSE
      ))
      rv$pending_fusions(data.frame(
        stain_id = character(0), F2_ppm = numeric(0),
        F1_ppm = numeric(0), Volume = numeric(0),
        stringsAsFactors = FALSE
      ))
      
      rv$box_intensity_cache(list())
      refresh_nmr_plot(force_recalc = TRUE)
      
      showNotification("✅ Changes applied", type = "message")
    })
    

    # DISCARD CHANGES ----

    
    observeEvent(input$discard_changes, {
      # Reset all pending
      rv$pending_centroids(data.frame(
        F2_ppm = numeric(0), F1_ppm = numeric(0),
        Volume = numeric(0), stain_id = character(0),
        stringsAsFactors = FALSE
      ))
      rv$pending_boxes(data.frame(
        xmin = numeric(0), xmax = numeric(0),
        ymin = numeric(0), ymax = numeric(0),
        stain_id = character(0), Volume = numeric(0), status = character(0),
        stringsAsFactors = FALSE
      ))
      rv$pending_fusions(data.frame(
        stain_id = character(0), F2_ppm = numeric(0),
        F1_ppm = numeric(0), Volume = numeric(0),
        stringsAsFactors = FALSE
      ))
      
      # Clean preview if exists
      if (isTRUE(rv$preview_trace_added())) {
        tryCatch({
          plotly::plotlyProxy("interactivePlot", parent_session) %>%
            plotly::plotlyProxyInvoke("deleteTraces", -1L)
        }, error = function(e) NULL)
        rv$preview_trace_added(FALSE)
      }
      
      rv$selected_box_for_edit(NULL)
      rv$selected_box_index(NULL)
      rv$original_box_coords(NULL)
      rv$box_has_been_modified(FALSE)
      
      showNotification("❌ All pending changes discarded", type = "warning")
    })
    

    # DELETE SELECTED PEAKS (from Data tab) ----

    
    observeEvent(parent_input$delete_selected_peaks, {
      selected_rows <- parent_input$centroid_table_rows_selected
      
      if (is.null(selected_rows) || length(selected_rows) == 0) {
        showNotification("⚠️ No peaks selected. Use Ctrl+Click to select multiple.", type = "warning")
        return()
      }
      
      df <- rv$centroids_data()
      if (is.null(df) || nrow(df) == 0) return()
      
      n_to_delete <- length(selected_rows)
      to_delete <- df[selected_rows, , drop = FALSE]
      to_delete$status <- "delete"
      
      rv$pending_centroids(dplyr::bind_rows(rv$pending_centroids(), to_delete))
      
      showNotification(paste("🗑️", n_to_delete, "peak(s) marked for deletion. Click 'Apply' to confirm."), type = "message")
    })
    

    # DELETE SELECTED BOXES (from Data tab) ----

    
    observeEvent(parent_input$delete_selected_boxes, {
      selected_rows <- parent_input$bbox_table_rows_selected
      
      if (is.null(selected_rows) || length(selected_rows) == 0) {
        showNotification("⚠️ No boxes selected. Use Ctrl+Click to select multiple.", type = "warning")
        return()
      }
      
      df <- data_reactives$bounding_boxes_data()
      if (is.null(df) || nrow(df) == 0) return()
      
      n_to_delete <- length(selected_rows)
      to_delete <- df[selected_rows, , drop = FALSE]
      to_delete$status <- "delete"
      
      rv$pending_boxes(dplyr::bind_rows(rv$pending_boxes(), to_delete))
      
      showNotification(paste("🗑️", n_to_delete, "box(es) marked for deletion. Click 'Apply' to confirm."), type = "message")
    })
    

    # DISCARD SELECTED PENDING CENTROIDS ----

    
    observeEvent(parent_input$discard_selected_centroid, {
      selected_rows <- parent_input$pending_centroids_table_rows_selected
      
      if (is.null(selected_rows) || length(selected_rows) == 0) {
        showNotification("⚠️ No pending peaks selected", type = "warning")
        return()
      }
      
      df <- rv$pending_centroids()
      if (is.null(df) || nrow(df) == 0) return()
      
      n_to_delete <- length(selected_rows)
      peaks_to_discard <- df[selected_rows, , drop = FALSE]
      df_remaining <- df[-selected_rows, , drop = FALSE]
      rv$pending_centroids(df_remaining)
      
      if ("status" %in% names(peaks_to_discard)) {
        n_delete <- sum(peaks_to_discard$status == "delete", na.rm = TRUE)
        n_add <- sum(is.na(peaks_to_discard$status) | peaks_to_discard$status != "delete")
        msg_parts <- c()
        if (n_delete > 0) msg_parts <- c(msg_parts, paste(n_delete, "deletion(s) cancelled"))
        if (n_add > 0) msg_parts <- c(msg_parts, paste(n_add, "addition(s) cancelled"))
        showNotification(paste("↩️", paste(msg_parts, collapse = ", ")), type = "message")
      } else {
        showNotification(paste("🗑️ Removed", n_to_delete, "pending peak(s)"), type = "message")
      }
    })
    

    # DISCARD SELECTED PENDING BOXES ----

    
    observeEvent(parent_input$discard_selected_box, {
      selected_rows <- parent_input$pending_boxes_table_rows_selected
      
      if (is.null(selected_rows) || length(selected_rows) == 0) {
        showNotification("⚠️ No pending boxes selected", type = "warning")
        return()
      }
      
      df <- rv$pending_boxes()
      if (is.null(df) || nrow(df) == 0) return()
      
      n_to_delete <- length(selected_rows)
      boxes_to_discard <- df[selected_rows, , drop = FALSE]
      df_remaining <- df[-selected_rows, , drop = FALSE]
      rv$pending_boxes(df_remaining)
      
      if (isTRUE(rv$preview_trace_added())) {
        tryCatch({
          plotly::plotlyProxy("interactivePlot", parent_session) %>%
            plotly::plotlyProxyInvoke("deleteTraces", -1L)
        }, error = function(e) NULL)
        rv$preview_trace_added(FALSE)
      }
      
      rv$selected_box_for_edit(NULL)
      rv$selected_box_index(NULL)
      rv$original_box_coords(NULL)
      rv$box_has_been_modified(FALSE)
      
      if ("status" %in% names(boxes_to_discard)) {
        n_delete <- sum(boxes_to_discard$status == "delete", na.rm = TRUE)
        n_add <- sum(boxes_to_discard$status == "add", na.rm = TRUE)
        n_edit <- sum(boxes_to_discard$status == "edit", na.rm = TRUE)
        msg_parts <- c()
        if (n_delete > 0) msg_parts <- c(msg_parts, paste(n_delete, "deletion(s) cancelled"))
        if (n_add > 0) msg_parts <- c(msg_parts, paste(n_add, "addition(s) cancelled"))
        if (n_edit > 0) msg_parts <- c(msg_parts, paste(n_edit, "edit(s) cancelled"))
        showNotification(paste("↩️", paste(msg_parts, collapse = ", ")), type = "message")
      } else {
        showNotification(paste("🗑️ Removed", n_to_delete, "pending box(es)"), type = "message")
      }
    })
    

    # DISCARD SELECTED PENDING FUSIONS ----
    
    observeEvent(parent_input$discard_selected_fusion, {
      selected_rows <- parent_input$pending_fusions_table_rows_selected
      
      if (is.null(selected_rows) || length(selected_rows) == 0) {
        showNotification("⚠️ No pending fusions selected", type = "warning")
        return()
      }
      
      df <- rv$pending_fusions()
      if (is.null(df) || nrow(df) == 0) return()
      
      n_to_delete <- length(selected_rows)
      df_remaining <- df[-selected_rows, , drop = FALSE]
      rv$pending_fusions(df_remaining)
      
      showNotification(paste("🗑️ Removed", n_to_delete, "pending fusion(s)"), type = "message")
    })
    

    # RETURN VALUES ----

    return(list(
      box_click_mode = reactive({ input$box_click_mode })
    ))
  })
}
