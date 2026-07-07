#' Polymorphic Hierarchical Event Entry Submodule Server
#' @export
server_event_page <- function(id, active_target) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    refresh_events_trigger <- reactiveVal(0)
    current_user <- dauPortalTools::get_user(session)

    output$event_history_pivoted <- DT::renderDT({
      refresh_events_trigger()
      req(active_target$id, active_target$type)

      raw_events <- dauPortalTools::db_ru_get_events(
        entity_id = active_target$id,
        entity_type = active_target$type
      )
      if (nrow(raw_events) == 0) {
        return(data.frame("Status" = "No structural event history found."))
      }

      all_responses <- lapply(raw_events$ruev_id, function(ev_id) {
        dauPortalTools::db_ru_get_event_action_responses(event_id = ev_id)
      }) |>
        dplyr::bind_rows()

      if (nrow(all_responses) == 0) {
        return(
          raw_events |>
            dplyr::select(
              ruev_date,
              event_type_name,
              event_sub_variety_name,
              ruev_summary_notes
            ) |>
            dplyr::rename(
              "Interaction Date" = ruev_date,
              "Primary Method" = event_type_name,
              "Cohort/Sub-Variety" = event_sub_variety_name,
              "Summary Notes" = ruev_summary_notes
            )
        )
      }

      flat_wide <- dauPortalTools::utils_ru_pivot_responses(all_responses)

      final_reporting_df <- raw_events |>
        dplyr::select(
          ruev_id,
          ruev_date,
          event_type_name,
          event_sub_variety_name,
          ruev_summary_notes
        ) |>
        dplyr::inner_join(flat_wide, by = "ruev_id") |>
        dplyr::select(-ruev_id) |>
        dplyr::rename(
          "Interaction Date" = ruev_date,
          "Primary Method" = event_type_name,
          "Cohort/Sub-Variety" = event_sub_variety_name,
          "Summary Notes" = ruev_summary_notes
        )

      DT::datatable(
        final_reporting_df,
        rownames = FALSE,
        selection = "none",
        options = list(pageLength = 10, dom = "tp")
      )
    })

    observeEvent(input$add_new_event_transaction, {
      req(active_target$id, active_target$type)

      types_df <- dauPortalTools::db_ru_get_event_types()

      showModal(modalDialog(
        title = "Log Point-in-Time Event Details",
        size = "l",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("submit_new_event_log"),
            "Commit Event Log",
            class = "btn-success"
          )
        ),
        tagList(
          fluidRow(
            column(
              6,
              selectInput(
                ns("new_evt_type_id"),
                "Primary Interaction Method Classification:",
                choices = setNames(types_df$ruevt_id, types_df$ruevt_name)
              )
            ),
            column(
              6,
              selectInput(
                ns("new_evt_sub_id"),
                "Cohort / Event Sub-Variety Focus:",
                choices = character(0)
              )
            )
          ),
          br(),
          fluidRow(
            column(
              6,
              dateInput(
                ns("new_evt_date"),
                "Execution Date:",
                value = Sys.Date()
              )
            ),
            column(
              6,
              textAreaInput(
                ns("new_evt_notes"),
                "Detailed Operational Interaction Summary Notes:",
                rows = 3,
                placeholder = "Enter contextual notes regarding items discussed, outcomes, or follow-ups..."
              )
            )
          ),
          hr(),
          tags$h5("Context-Driven Metrics Requirements:"),
          uiOutput(ns("dynamic_creation_fields_container"))
        )
      ))
    })

    observeEvent(input$new_evt_type_id, {
      req(input$new_evt_type_id)
      subs_df <- dauPortalTools::db_ru_get_event_sub_varieties(
        ruevt_id = as.integer(input$new_evt_type_id)
      )

      updateSelectInput(
        session,
        "new_evt_sub_id",
        choices = c(
          "No specific sub-option variety (Global Type Scope)" = 0,
          setNames(subs_df$ruesv_id, subs_df$ruesv_name)
        )
      )
    })

    output$dynamic_creation_fields_container <- renderUI({
      req(input$new_evt_type_id)
      sub_id <- if (
        is.null(input$new_evt_sub_id) || !nzchar(input$new_evt_sub_id)
      ) {
        0
      } else {
        as.integer(input$new_evt_sub_id)
      }

      fields_df <- dauPortalTools::db_ru_get_event_actions(
        ruevt_id = as.integer(input$new_evt_type_id),
        ruesv_id = sub_id
      )

      if (nrow(fields_df) == 0) {
        return(p(em(
          "No specialized metric metrics are required for this specific tracking cohort layout view."
        )))
      }

      lapply(seq_len(nrow(fields_df)), function(i) {
        row <- fields_df[i, ]
        input_id <- paste0("create_field_", row$rueva_id)

        label_text <- row$rueva_name
        if (nzchar(row$rueva_description %||% "")) {
          label_text <- HTML(paste0(
            row$rueva_name,
            "<br><small class='text-muted'>",
            row$rueva_description,
            "</small>"
          ))
        }

        if (identical(row$rueva_rule_type, "Integer")) {
          numericInput(ns(input_id), label = label_text, value = NULL, min = 0)
        } else if (identical(row$rueva_rule_type, "Date")) {
          dateInput(ns(input_id), label = label_text, value = Sys.Date())
        } else if (identical(row$rueva_rule_type, "Boolean")) {
          checkboxInput(ns(input_id), label = label_text, value = FALSE)
        } else {
          textInput(ns(input_id), label = label_text, value = "")
        }
      })
    })

    observeEvent(input$submit_new_event_log, {
      req(input$new_evt_type_id, input$new_evt_date)
      sub_id <- if (
        is.null(input$new_evt_sub_id) || !nzchar(input$new_evt_sub_id)
      ) {
        0
      } else {
        as.integer(input$new_evt_sub_id)
      }

      removeModal()

      new_id <- dauPortalTools::db_ru_add_event(
        event_type_id = as.integer(input$new_evt_type_id),
        event_sub_variety_id = sub_id,
        entity_id = as.integer(active_target$id),
        entity_type = active_target$type,
        event_date = format(as.Date(input$new_evt_date), "%Y-%m-%d"),
        summary_notes = trimws(input$new_evt_notes),
        user_id = current_user
      )

      fields_df <- dauPortalTools::db_ru_get_event_actions(
        ruevt_id = as.integer(input$new_evt_type_id),
        ruesv_id = sub_id
      )
      if (nrow(fields_df) > 0) {
        for (i in seq_len(nrow(fields_df))) {
          row <- fields_df[i, ]
          ui_val_raw <- input[[paste0("create_field_", row$rueva_id)]]

          string_cast <- if (
            is.null(ui_val_raw) || !nzchar(trimws(as.character(ui_val_raw)))
          ) {
            ""
          } else if (identical(row$rueva_rule_type, "Date")) {
            format(as.Date(ui_val_raw), "%Y-%m-%d")
          } else if (identical(row$rueva_rule_type, "Boolean")) {
            if (ui_val_raw == TRUE) "1" else "0"
          } else {
            as.character(ui_val_raw)
          }

          dauPortalTools::db_ru_save_event_action_response(
            event_id = new_id,
            rueva_id = row$rueva_id,
            response_value = string_cast,
            user_id = current_user
          )
        }
      }

      showNotification(
        "Hierarchical interaction event entry logged successfully.",
        type = "message"
      )
      refresh_events_trigger(refresh_events_trigger() + 1)
    })
  })
}
