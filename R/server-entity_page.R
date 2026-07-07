#' Entity Target Details Management Module Server
#'
#' @param id Character scalar. Shiny namespace identifier.
#' @param active_target ReactiveValues object tracking current session id and type.
#' @export
server_entity_page <- function(id, active_target) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    hubs_list_proxy <- DT::dataTableProxy("hubs_list", session)
    support_given_proxy <- DT::dataTableProxy("support_given", session)

    current_user <- dauPortalTools::get_user(session)
    refresh_log <- reactiveVal(0)

    output$dynamic_entity_overview_container <- renderUI({
      req(active_target$type, active_target$id)

      type <- tolower(active_target$type)
      target_id <- active_target$id

      if (type == "school") {
        dauPortalTools::school_render_overview(
          urn = target_id,
          id = ns("active_school_view")
        )
      } else if (type %in% c("trust", "la", "diocese")) {
        dauPortalTools::entity_render_overview(
          entity_type = type,
          entity_id = target_id,
          id = ns(paste0("active_parent_view_", type))
        )
      } else {
        shiny::div(
          class = "govuk-error-summary",
          tags$h2(
            class = "govuk-error-summary__title",
            "Routing Execution Error"
          ),
          p(
            class = "govuk-body",
            "The targeted operational entity layer type is unrecognized."
          )
        )
      }
    })

    output$hubs_list <- DT::renderDT(
      {
        req(active_target$id, active_target$type)

        dauPortalTools::db_ruh_get_support_records(
          entity_id = active_target$id,
          entity_type = active_target$type
        ) |>
          dplyr::select(
            ruhsr_id,
            ruhb_name,
            support_type_name,
            ruhsr_active,
            ruhsr_dateactive,
            ruhsr_dateended
          ) |>
          dplyr::rename(
            "Support ID" = ruhsr_id,
            "Associated Hub" = ruhb_name,
            "Framework Type" = support_type_name,
            "Support Active?" = ruhsr_active,
            "Date Started" = ruhsr_dateactive,
            "Date Ended" = ruhsr_dateended
          )
      },
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 10, dom = "tp"),
      callback = DT::JS(paste0(
        "
      table.on('dblclick', 'tr', function() {
        var data = table.row(this).data();
        if (data) { 
          Shiny.setInputValue('",
        ns("hub_support_dblclicked"),
        "', data[0], {priority: 'event'}); 
        }
      });
      "
      ))
    )

    output$support_given <- DT::renderDT(
      {
        req(active_target$id, active_target$type)

        if (!identical(tolower(active_target$type), "school")) {
          return(data.frame(
            "Status Notification" = "Lead hub center profiles are exclusively managed at the school URN tracking level."
          ))
        }

        clean_urn <- as.numeric(active_target$id)

        dauPortalTools::db_ruh_get_lead_schools() |>
          dplyr::filter(ruhl_urn == clean_urn) |>
          dplyr::select(
            ruhl_id,
            hub_name,
            ruhl_urn,
            ruhl_active,
            ruhl_dateactive,
            ruhl_dateended
          ) |>
          dplyr::rename(
            "Lead ID" = ruhl_id,
            "Lead Hub Name" = hub_name,
            "Lead URN" = ruhl_urn,
            "Lead Active?" = ruhl_active,
            "Date Started" = ruhl_dateactive,
            "Date Ended" = ruhl_dateended
          )
      },
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 5, dom = "tp"),
      callback = DT::JS(paste0(
        "
      table.on('dblclick', 'tr', function() {
        var data = table.row(this).data();
        if (data) { 
          Shiny.setInputValue('",
        ns("hub_lead_dblclicked"),
        "', data[0], {priority: 'event'}); 
        }
      });
      "
      ))
    )

    observeEvent(input$new_support_record, {
      req(active_target$id, active_target$type)

      hubs_df <- dauPortalTools::db_ruh_get_hubs()
      hubs_choices <- setNames(hubs_df$ruhb_id, hubs_df$ruhb_name)

      showModal(modalDialog(
        title = glue::glue(
          "Log New Support Provision for {active_target$type}"
        ),
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("submit_support_record"),
            "Save Assignment Track",
            class = "btn-success"
          )
        ),
        tagList(
          selectInput(
            ns("modal_sup_hub_id"),
            "Select Associated Hub Matrix:",
            choices = hubs_choices
          ),
          selectInput(
            ns("modal_sup_framework_id"),
            "Select Support Framework Sub-Category:",
            choices = character(0)
          ),
          ui_date_input(
            ns("modal_sup_date_start"),
            "Date Support Framework Started:",
            value = Sys.Date()
          ),
          textAreaInput(
            ns("modal_sup_comment"),
            "Comments / Initial Operational Context:",
            rows = 3
          )
        )
      ))
    })

    observeEvent(input$modal_sup_hub_id, {
      req(input$modal_sup_hub_id)

      frameworks_df <- dauPortalTools::db_ruh_get_support_types(
        hub_id = as.integer(input$modal_sup_hub_id)
      )
      framework_choices <- setNames(
        frameworks_df$ruht_id,
        frameworks_df$ruht_name
      )

      updateSelectInput(
        session = session,
        inputId = "modal_sup_framework_id",
        choices = framework_choices
      )
    })

    observeEvent(input$submit_support_record, {
      req(input$modal_sup_hub_id, input$modal_sup_framework_id)
      req(
        input$modal_sup_date_start_day,
        input$modal_sup_date_start_month,
        input$modal_sup_date_start_year
      )

      clean_date <- input_to_date("modal_sup_date_start", input)

      if (is.na(clean_date)) {
        showNotification(
          "Invalid Date selection format. Please verify day/month values.",
          type = "error"
        )
        return()
      }

      removeModal()

      dauPortalTools::db_ruh_add_blank_support_record(
        hub_id = as.integer(input$modal_sup_hub_id),
        ruht_id = as.integer(input$modal_sup_framework_id),
        entity_id = as.character(active_target$id),
        entity_type = active_target$type,
        user_id = current_user,
        date_start = format(clean_date, "%Y-%m-%d"),
        comment = input$modal_sup_comment
      )

      DT::replaceData(
        hubs_list_proxy,
        dauPortalTools::db_ruh_get_support_records(
          entity_id = active_target$id,
          entity_type = active_target$type
        ) |>
          dplyr::select(
            ruhsr_id,
            ruhb_name,
            support_type_name,
            ruhsr_active,
            ruhsr_dateactive,
            ruhsr_dateended
          ) |>
          dplyr::rename(
            "Support ID" = ruhsr_id,
            "Associated Hub" = ruhb_name,
            "Framework Type" = support_type_name,
            "Support Active?" = ruhsr_active,
            "Date Started" = ruhsr_dateactive,
            "Date Ended" = ruhsr_dateended
          ),
        resetPaging = FALSE
      )

      showNotification(
        glue::glue(
          "Provision tracking for {active_target$type} successfully registered!"
        ),
        type = "message"
      )
    })

    output$universal_events_timeline_table <- DT::renderDT(
      {
        refresh_log()
        req(active_target$id, active_target$type)

        df <- dauPortalTools::db_ru_get_events(
          entity_id = active_target$id,
          entity_type = active_target$type
        )
        if (is.null(df) || nrow(df) == 0) {
          return(data.frame(
            "Status" = "No event transaction history logged for this target."
          ))
        }

        df |>
          dplyr::select(
            ruev_id,
            ruev_date,
            event_type_name,
            event_sub_variety_name,
            ruev_summary_notes,
            user_id_created
          ) |>
          dplyr::rename(
            "Event ID" = ruev_id,
            "Log Date" = ruev_date,
            "Method Type" = event_type_name,
            "Focus Cohort" = event_sub_variety_name,
            "Summary Notes" = ruev_summary_notes,
            "Logged By" = user_id_created
          )
      },
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 10, dom = "tp"),
      callback = DT::JS(paste0(
        "
      table.on('dblclick', 'tr', function() {
        var data = table.row(this).data();
        if (data) { 
          Shiny.setInputValue('",
        ns("event_timeline_dblclicked"),
        "', data[0], {priority: 'event'}); 
        }
      });
      "
      ))
    )

    observeEvent(input$add_new_event_transaction, {
      req(active_target$id, active_target$type)

      types_df <- dauPortalTools::db_ru_get_event_types()

      showModal(modalDialog(
        title = glue::glue(
          "Log New Interaction Event for {active_target$type}"
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
        session = session,
        inputId = "modal_evt_sub_id",
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
        entity_id = as.character(active_target$id),
        entity_type = active_target$type,
        event_date = format(as.Date(input$modal_evt_date), "%Y-%m-%d"),
        summary_notes = input$modal_evt_notes,
        user_id = current_user
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
            user_id = current_user
          )
        }
      }

      showNotification(
        "Polymorphic dynamic interaction event logged successfully.",
        type = "message"
      )
      refresh_log(refresh_log() + 1)
    })

    observeEvent(input$new_lead_record, {
      req(active_target$id, active_target$type)

      if (!identical(tolower(active_target$type), "school")) {
        showNotification(
          "Lead designation operations are strictly reserved for Individual School nodes.",
          type = "warning"
        )
        return()
      }

      hubs_df <- dauPortalTools::db_ruh_get_hubs()
      hubs_choices <- setNames(hubs_df$ruhb_id, hubs_df$ruhb_name)

      showModal(modalDialog(
        title = "Register New Lead Hub Center Status",
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("submit_lead_record"),
            "Register Lead Status",
            class = "btn-primary"
          )
        ),
        tagList(
          selectInput(
            ns("modal_lead_hub_id"),
            "Select Associated Hub Framework Branch:",
            choices = hubs_choices
          ),
          ui_date_input(
            ns("modal_lead_date_start"),
            "Date Status Active From:",
            value = Sys.Date()
          ),
          textAreaInput(
            ns("modal_lead_comment"),
            "Comments / Designation Notes:",
            rows = 3
          )
        )
      ))
    })

    observeEvent(input$submit_lead_record, {
      req(input$modal_lead_hub_id)
      req(
        input$modal_lead_date_start_day,
        input$modal_lead_date_start_month,
        input$modal_lead_date_start_year
      )

      clean_date <- input_to_date("modal_lead_date_start", input)

      if (is.na(clean_date)) {
        showNotification(
          "Invalid Date selection format. Please verify day/month values.",
          type = "error"
        )
        return()
      }

      removeModal()

      dauPortalTools::db_ruh_add_blank_lead_school(
        hub_id = as.integer(input$modal_lead_hub_id),
        urn = as.numeric(active_target$id),
        user_id = current_user,
        date_start = format(clean_date, "%Y-%m-%d"),
        comment = input$modal_lead_comment
      )

      DT::replaceData(
        support_given_proxy,
        dauPortalTools::db_ruh_get_lead_schools() |>
          dplyr::filter(ruhl_urn == as.numeric(active_target$id)) |>
          dplyr::select(
            ruhl_id,
            hub_name,
            ruhl_urn,
            ruhl_active,
            ruhl_dateactive,
            ruhl_dateended
          ) |>
          dplyr::rename(
            "Lead ID" = ruhl_id,
            "Lead Hub Name" = hub_name,
            "Lead URN" = ruhl_urn,
            "Lead Active?" = ruhl_active,
            "Date Started" = ruhl_dateactive,
            "Date Ended" = ruhl_dateended
          ),
        resetPaging = FALSE
      )

      showNotification(
        "Lead Hub Designation Status recorded successfully!",
        type = "message"
      )
    })
  })
}
