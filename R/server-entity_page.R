#' Entity Target Details Management Module Server
#'
#' @param id Character scalar. Shiny namespace identifier.
#' @param active_target ReactiveValues object tracking current session id and type.
#' @param selected_urn ReactiveVal tracking the target entity selected for redirect.
#' @param selected_lead_id ReactiveVal tracking the target lead provider assignment selected for redirect.
#' @param main_navbar_session The parent Shiny session used to control page tab navigation.
#' @export
server_entity_page <- function(
  id,
  active_target,
  selected_urn,
  selected_lead_id,
  main_navbar_session
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    hubs_list_proxy <- DT::dataTableProxy("hubs_list", session)
    support_given_proxy <- DT::dataTableProxy("support_given", session)

    current_user <- dauPortalTools::get_user(session)
    refresh_log <- reactiveVal(0)
    refresh_hubs_trigger <- reactiveVal(0)
    refresh_leads_trigger <- reactiveVal(0)

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
        refresh_hubs_trigger()
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
        refresh_leads_trigger()
        req(active_target$id, active_target$type)

        dauPortalTools::db_ruh_get_lead_support_records(
          entity_type = active_target$type,
          entity_id = active_target$id
        ) |>
          dplyr::select(
            ruhls_id,
            lead_entity_type,
            lead_entity_id,
            hub_name,
            ruhl_active,
            ruhl_dateactive,
            ruhl_dateended
          ) |>
          dplyr::rename(
            "Assignment ID" = ruhls_id,
            "Provider Type" = lead_entity_type,
            "Provider ID/URN" = lead_entity_id,
            "Linked Hub Context" = hub_name,
            "Active Status" = ruhl_active,
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
        ns("lead_provider_row_dblclicked"),
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
        size = "l",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("submit_support_record"),
            "Save Assignment Track",
            class = "btn-success"
          )
        ),
        tagList(
          fluidRow(
            column(
              6,
              selectInput(
                ns("modal_sup_hub_id"),
                "Select Associated Hub Matrix:",
                choices = hubs_choices
              )
            ),
            column(
              6,
              selectInput(
                ns("modal_sup_framework_id"),
                "Select Support Framework Sub-Category:",
                choices = character(0)
              )
            )
          ),
          br(),
          fluidRow(
            column(
              6,
              ui_date_input(
                ns("modal_sup_date_start"),
                "Date Support Framework Started:",
                value = Sys.Date()
              )
            ),
            column(
              6,
              textAreaInput(
                ns("modal_sup_comment"),
                "Comments / Initial Operational Context:",
                rows = 2
              )
            )
          ),
          hr(),
          tags$h5("Context-Driven Framework Metric Actions:"),
          uiOutput(ns("modal_sup_dynamic_metrics_container"))
        )
      ))
    })

    output$modal_sup_dynamic_metrics_container <- renderUI({
      req(input$modal_sup_framework_id)
      fw_id <- as.integer(input$modal_sup_framework_id)

      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      fields_df <- DBI::dbGetQuery(
        conn,
        glue::glue_sql(
          "
          SELECT [ruhbf_id], [ruhbf_name], [ruhbf_description], [ruhbf_rule_type], [ruhbf_required], [ruht_id]
          FROM {utils_resolve_schema('db_schema_01r')}.[ruh_blueprint_fields]
          WHERE [ruhbf_active] = 1 
            AND ([ruht_id] = {fw_id} OR ([ruht_id] = 0 AND [ruhb_id] = {as.integer(input$modal_sup_hub_id)}));
          ",
          .con = conn
        )
      )

      if (is.null(fields_df) || nrow(fields_df) == 0) {
        return(p(em(
          "No dynamic action metrics are blueprinted for this specific framework pathway segment."
        )))
      }

      lapply(seq_len(nrow(fields_df)), function(i) {
        row <- fields_df[i, ]
        input_id <- paste0("hub_init_metric_", row$ruhbf_id)

        label_text <- if (row$ruhbf_required == 1) {
          HTML(paste0(row$ruhbf_name, " <span class='text-danger'>*</span>"))
        } else {
          row$ruhbf_name
        }

        scope_suffix <- if (row$ruht_id == 0) {
          " (Hub Global)"
        } else {
          " (Cohort Metric)"
        }
        label_text <- HTML(paste0(
          label_text,
          "<small class='text-muted'>",
          scope_suffix,
          "</small>"
        ))

        if (
          identical(row$ruhbf_rule_type, "Integer") ||
            identical(row$ruhbf_rule_type, "Numeric")
        ) {
          numericInput(ns(input_id), label = label_text, value = NULL, min = 0)
        } else if (identical(row$ruhbf_rule_type, "Date")) {
          dateInput(ns(input_id), label = label_text, value = Sys.Date())
        } else if (identical(row$ruhbf_rule_type, "Boolean")) {
          checkboxInput(ns(input_id), label = label_text, value = FALSE)
        } else if (identical(row$ruhbf_rule_type, "Dropdown")) {
          parsed_choices <- trimws(strsplit(row$ruhbf_description, ",")[[1]])
          selectInput(
            ns(input_id),
            label = label_text,
            choices = parsed_choices,
            width = "100%"
          )
        } else {
          textInput(ns(input_id), label = label_text, value = "")
        }
      })
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

      fw_id <- as.integer(input$modal_sup_framework_id)
      hub_id <- as.integer(input$modal_sup_hub_id)
      removeModal()

      new_support_id <- dauPortalTools::db_ruh_add_blank_support_record(
        hub_id = hub_id,
        ruht_id = fw_id,
        entity_id = as.character(active_target$id),
        entity_type = active_target$type,
        user_id = current_user,
        date_start = format(clean_date, "%Y-%m-%d"),
        comment = input$modal_sup_comment
      )

      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      blueprint_fields <- DBI::dbGetQuery(
        conn,
        glue::glue_sql(
          "
          SELECT [ruhbf_id], [ruhbf_rule_type]
          FROM {utils_resolve_schema('db_schema_01r')}.[ruh_blueprint_fields]
          WHERE [ruhbf_active] = 1 
            AND ([ruht_id] = {fw_id} OR ([ruht_id] = 0 AND [ruhb_id] = {hub_id}));
          ",
          .con = conn
        )
      )

      if (!is.null(blueprint_fields) && nrow(blueprint_fields) > 0) {
        for (i in seq_len(nrow(blueprint_fields))) {
          bf_id <- blueprint_fields$ruhbf_id[i]
          rule_type <- blueprint_fields$ruhbf_rule_type[i]

          input_value_raw <- input[[paste0("hub_init_metric_", bf_id)]]

          string_cast <- if (
            is.null(input_value_raw) ||
              !nzchar(trimws(as.character(input_value_raw)))
          ) {
            "[Pending Target Entry]"
          } else if (identical(rule_type, "Date")) {
            format(as.Date(input_value_raw), "%Y-%m-%d")
          } else if (identical(rule_type, "Boolean")) {
            if (isTRUE(input_value_raw) || input_value_raw == "TRUE") {
              "True"
            } else {
              "False"
            }
          } else {
            as.character(input_value_raw)
          }

          insert_query <- glue::glue_sql(
            "
            INSERT INTO {utils_resolve_schema('db_schema_01r')}.[ruh_support_school_actions]
            ([ruhs_id], [ruha_id], [ruhsa_date], [ruhsa_comment], [date_created], [user_id_created])
            VALUES (
              {as.integer(new_support_id)}, 
              {as.integer(bf_id)}, 
              {format(clean_date, '%Y-%m-%d')}, 
              {string_cast}, 
              SYSUTCDATETIME(), 
              {current_user}
            );
            ",
            .con = conn
          )
          DBI::dbExecute(conn, insert_query)
        }
      }

      refresh_hubs_trigger(refresh_hubs_trigger() + 1)
      showNotification(
        "Support tracking provision contract registered with initial metrics successfully.",
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
        if (row$rueva_required == 1) {
          label_text <- HTML(paste0(
            row$rueva_name,
            " <span class='text-danger'>*</span>"
          ))
        }

        if (nzchar(row$rueva_description %||% "")) {
          if (row$rueva_rule_type != "Dropdown") {
            label_text <- HTML(paste0(
              label_text,
              "<br><small class='text-muted'>",
              row$rueva_description,
              "</small>"
            ))
          }
        }

        if (
          identical(row$rueva_rule_type, "Integer") ||
            identical(row$rueva_rule_type, "Numeric")
        ) {
          numericInput(ns(input_id), label = label_text, value = NULL, min = 0)
        } else if (identical(row$rueva_rule_type, "Date")) {
          dateInput(ns(input_id), label = label_text, value = Sys.Date())
        } else if (identical(row$rueva_rule_type, "Boolean")) {
          checkboxInput(ns(input_id), label = label_text, value = FALSE)
        } else if (identical(row$rueva_rule_type, "Dropdown")) {
          parsed_choices <- trimws(strsplit(row$rueva_description, ",")[[1]])
          selectInput(
            ns(input_id),
            label = label_text,
            choices = parsed_choices,
            width = "100%"
          )
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
            if (row$rueva_required == 1) {
              "[Pending Target Requirement Entry]"
            } else {
              ""
            }
          } else if (identical(row$rueva_rule_type, "Date")) {
            format(as.Date(input_value_raw), "%Y-%m-%d")
          } else if (identical(row$rueva_rule_type, "Boolean")) {
            if (isTRUE(input_value_raw) || input_value_raw == "TRUE") {
              "True"
            } else {
              "False"
            }
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
        "Dynamic interaction event registered with instantiated checklist targets.",
        type = "message"
      )
      refresh_log(refresh_log() + 1)
    })

    observeEvent(input$new_lead_record, {
      req(active_target$id, active_target$type)

      hubs_df <- dauPortalTools::db_ruh_get_hubs()
      hubs_choices <- setNames(hubs_df$ruhb_id, hubs_df$ruhb_name)

      showModal(modalDialog(
        title = "Register New Lead Support Identity Status",
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("submit_lead_record"),
            "Register Support Context",
            class = "btn-primary"
          )
        ),
        tagList(
          selectInput(
            ns("modal_lead_hub_id"),
            "Select Associated Hub Framework Context:",
            choices = hubs_choices
          ),
          ui_date_input(
            ns("modal_lead_date_start"),
            "Date Support Assignment Active From:",
            value = Sys.Date()
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

      dauPortalTools::db_ruh_add_blank_lead_support(
        lead_type = active_target$type,
        lead_id = as.integer(active_target$id),
        hub_id = as.integer(input$modal_lead_hub_id),
        event_id = NULL,
        user_id = current_user,
        date_start = format(clean_date, "%Y-%m-%d"),
        comment = NULL
      )

      refresh_leads_trigger(refresh_leads_trigger() + 1)
      showNotification(
        "Lead Provider Contract designation initiated! Click through to details page to configure cohorts.",
        type = "message"
      )
    })
  })
}
