#' Polymorphic Hub Provision Support Tracking Page Module Server
#'
#' Manages the master editing form for active tracking provision records and hooks
#' directly into point-in-time custom interactions using the dynamic cascading event system.
#'
#' @param id Character scalar. Shiny namespace identifier.
#' @param selected_support_id ReactiveVal containing the integer primary key ([ruhsr_id]).
#' @param active_target ReactiveValues object containing context variables tracking entity targets.
#' @export
server_hub_support_page <- function(id, selected_support_id, active_target) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    events_data <- reactiveVal(NULL)
    target_ruht_id <- reactiveVal(NULL)
    refresh_log <- reactiveVal(0)

    hubs_lookup <- reactive({
      dauPortalTools::db_ruh_get_hubs()
    })

    observeEvent(hubs_lookup(), {
      updateSelectInput(
        session,
        "hub_id",
        choices = setNames(hubs_lookup()$ruhb_id, hubs_lookup()$ruhb_name)
      )
    })

    observeEvent(input$hub_id, {
      req(input$hub_id)
      types_df <- dauPortalTools::db_ruh_get_support_types(
        hub_id = as.integer(input$hub_id)
      )

      updateSelectInput(
        session,
        "ruht_id",
        choices = setNames(types_df$ruht_id, types_df$ruht_name)
      )
    })

    observe({
      req(target_ruht_id(), input$hub_id)
      updateSelectInput(session, "ruht_id", selected = target_ruht_id())
      target_ruht_id(NULL)
    })

    observe({
      refresh_log()
      req(selected_support_id(), active_target$id, active_target$type)

      events_data(dauPortalTools::db_ru_get_events(
        entity_id = active_target$id,
        entity_type = active_target$type
      ))

      rec <- dauPortalTools::db_ruh_get_support_records(
        ruhsr_id = selected_support_id()
      )
      if (nrow(rec) > 0) {
        target_ruht_id(rec$ruht_id)

        updateSelectInput(session, "hub_id", selected = rec$ruhb_id)
        updateTextAreaInput(session, "comment", value = rec$ruhsr_comment)

        if (!is.null(rec$ruhsr_dateactive) && !is.na(rec$ruhsr_dateactive)) {
          d_act <- as.Date(rec$ruhsr_dateactive)
          updateSelectInput(
            session,
            "date_active_day",
            selected = format(d_act, "%d")
          )
          updateSelectInput(
            session,
            "date_active_month",
            selected = format(d_act, "%b")
          )
          updateSelectInput(
            session,
            "date_active_year",
            selected = format(d_act, "%Y")
          )
        }

        if (!is.null(rec$ruhsr_dateended) && !is.na(rec$ruhsr_dateended)) {
          d_end <- as.Date(rec$ruhsr_dateended)
          updateSelectInput(
            session,
            "date_ended_day",
            selected = format(d_end, "%d")
          )
          updateSelectInput(
            session,
            "date_ended_month",
            selected = format(d_end, "%b")
          )
          updateSelectInput(
            session,
            "date_ended_year",
            selected = format(d_end, "%Y")
          )
        } else {
          updateSelectInput(session, "date_ended_day", selected = "")
          updateSelectInput(session, "date_ended_month", selected = "")
          updateSelectInput(session, "date_ended_year", selected = "")
        }
      }
    })

    output$actions_table <- DT::renderDT({
      req(events_data())
      df <- events_data()
      if (nrow(df) == 0) {
        return(data.frame("Status" = "No event transaction history logged."))
      }

      DT::datatable(
        df |>
          dplyr::select(
            ruev_date,
            event_type_name,
            event_sub_variety_name,
            ruev_summary_notes
          ),
        colnames = c(
          "Interaction Date",
          "Primary Method",
          "Cohort / Sub-Variety",
          "Summary Notes"
        ),
        selection = "single",
        rownames = FALSE,
        options = list(pageLength = 5, dom = 'tp')
      )
    })

    observeEvent(input$add_action, {
      req(selected_support_id())
      types_df <- dauPortalTools::db_ru_get_event_types()

      showModal(modalDialog(
        title = glue::glue(
          "Log New Interaction Event for {active_target$name}"
        ),
        size = "l",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("save_new_dynamic_event"),
            "Save Interaction Event Log",
            class = "btn-success"
          )
        ),
        tagList(
          fluidRow(
            column(
              6,
              selectInput(
                ns("modal_evt_type_id"),
                "Primary Interaction Method Classification:",
                choices = setNames(types_df$ruevt_id, types_df$ruevt_name)
              )
            ),
            column(
              6,
              selectInput(
                ns("modal_evt_sub_id"),
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
                ns("modal_evt_date"),
                "Interaction Event Date:",
                value = Sys.Date()
              )
            ),
            column(
              6,
              textAreaInput(
                ns("modal_evt_notes"),
                "Top-level Summary Notes:",
                rows = 2
              )
            )
          ),
          hr(),
          tags$h5("Context-Driven Metrics Requirements:"),
          uiOutput(ns("dynamic_metrics_form_container"))
        )
      ))
    })

    observeEvent(input$modal_evt_type_id, {
      req(input$modal_evt_type_id)
      subs_df <- dauPortalTools::db_ru_get_event_sub_varieties(
        ruevt_id = as.integer(input$modal_evt_type_id)
      )

      updateSelectInput(
        session,
        "modal_evt_sub_id",
        choices = c(
          "No specific sub-option variety (Global Type Scope)" = 0,
          setNames(subs_df$ruesv_id, subs_df$ruesv_name)
        )
      )
    })

    output$dynamic_metrics_form_container <- renderUI({
      req(input$modal_evt_type_id)
      sub_id <- if (
        is.null(input$modal_evt_sub_id) || !nzchar(input$modal_evt_sub_id)
      ) {
        0
      } else {
        as.integer(input$modal_evt_sub_id)
      }

      fields_df <- dauPortalTools::db_ru_get_event_actions(
        ruevt_id = as.integer(input$modal_evt_type_id),
        ruesv_id = sub_id
      )
      if (nrow(fields_df) == 0) {
        return(p(em(
          "No additional dynamic form metric inputs are required for this specific validation context block."
        )))
      }

      lapply(seq_len(nrow(fields_df)), function(i) {
        row <- fields_df[i, ]
        input_id <- paste0("field_metric_", row$rueva_id)

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

    observeEvent(input$save_new_dynamic_event, {
      req(input$modal_evt_type_id, input$modal_evt_date)
      sub_id <- if (
        is.null(input$modal_evt_sub_id) || !nzchar(input$modal_evt_sub_id)
      ) {
        0
      } else {
        as.integer(input$modal_evt_sub_id)
      }

      removeModal()

      new_event_id <- dauPortalTools::db_ru_add_event(
        event_type_id = as.integer(input$modal_evt_type_id),
        event_sub_variety_id = sub_id,
        entity_id = as.integer(active_target$id),
        entity_type = active_target$type,
        event_date = format(as.Date(input$modal_evt_date), "%Y-%m-%d"),
        summary_notes = input$modal_evt_notes,
        user_id = dauPortalTools::get_user(session)
      )

      fields_df <- dauPortalTools::db_ru_get_event_actions(
        ruevt_id = as.integer(input$modal_evt_type_id),
        ruesv_id = sub_id
      )
      if (nrow(fields_df) > 0) {
        for (i in seq_len(nrow(fields_df))) {
          row <- fields_df[i, ]
          input_value_raw <- input[[paste0("field_metric_", row$rueva_id)]]

          string_cast <- if (
            is.null(input_value_raw) ||
              !nzchar(trimws(as.character(input_value_raw)))
          ) {
            ""
          } else if (identical(row$rueva_rule_type, "Date")) {
            format(as.Date(input_value_raw), "%Y-%m-%d")
          } else if (identical(row$rueva_rule_type, "Boolean")) {
            if (input_value_raw == TRUE) "1" else "0"
          } else {
            as.character(input_value_raw)
          }

          dauPortalTools::db_ru_save_event_action_response(
            event_id = new_event_id,
            rueva_id = row$rueva_id,
            response_value = string_cast,
            user_id = dauPortalTools::get_user(session)
          )
        }
      }

      showNotification(
        "Polymorphic dynamic interaction event logged successfully.",
        type = "message"
      )
      refresh_log(refresh_log() + 1)
    })

    observeEvent(input$save_support, {
      req(selected_support_id(), input$hub_id, input$ruht_id)
      req(
        input$date_active_day,
        input$date_active_month,
        input$date_active_year
      )

      clean_start <- input_to_date("date_active", input)
      clean_ended <- input_to_date("date_ended", input)

      if (is.na(clean_start)) {
        showNotification(
          "A valid Framework Start Date configuration is required.",
          type = "error"
        )
        return()
      }

      dauPortalTools::db_ruh_update_support_record(
        ruhsr_id = selected_support_id(),
        hub_id = as.integer(input$hub_id),
        ruht_id = as.integer(input$ruht_id),
        lead_school_id = NULL,
        date_active = format(clean_start, "%Y-%m-%d"),
        date_ended = if (is.na(clean_ended)) {
          NULL
        } else {
          format(clean_ended, "%Y-%m-%d")
        },
        is_active = if (is.na(clean_ended)) 1 else 0,
        comment = input$comment,
        user_id = dauPortalTools::get_user(session)
      )

      showNotification(
        "Polymorphic tracking record updated successfully.",
        type = "message"
      )
      refresh_log(refresh_log() + 1)
    })

    return(list(
      go_back = reactive({
        input$back_to_school
      })
    ))
  })
}
