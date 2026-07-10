#' Polymorphic Event Instance Allocation & Action Response Submodule Server
#'
#' @param id Character scalar. Shiny namespace identifier.
#' @param selected_event_id ReactiveVal tracking the unique integer parent event [ruev_id].
#' @param active_target ReactiveValues object tracking current session context id and type.
#' @export
server_event_instance_page <- function(id, selected_event_id, active_target) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    current_user <- dauPortalTools::get_user(session)

    refresh_event_master <- reactiveVal(0)
    refresh_sub_actions <- reactiveVal(0)

    active_event_data <- reactive({
      req(selected_event_id())
      refresh_event_master()

      df <- dauPortalTools::db_ru_get_events(ruev_id = selected_event_id())
      if (is.null(df) || nrow(df) == 0) {
        return(NULL)
      }
      as.list(df[1, ])
    })

    assigned_lead_provider <- reactive({
      req(selected_event_id())
      refresh_event_master()

      df <- dauPortalTools::db_ruh_get_lead_support_records(
        event_id = selected_event_id()
      )
      if (is.null(df) || nrow(df) == 0) {
        return(NULL)
      }
      as.list(df[1, ])
    })

    output$read_only_event_meta <- renderUI({
      ev <- active_event_data()
      req(ev)

      tagList(
        fluidRow(
          column(
            6,
            tags$strong("Primary Interaction Method:"),
            p(ev$event_type_name)
          ),
          column(
            6,
            tags$strong("Cohort Focus Sub-Variety:"),
            p(ev$event_sub_variety_name)
          )
        ),
        br(),
        fluidRow(
          column(
            6,
            tags$strong("Interaction Event Date:"),
            p(as.character(ev$ruev_date))
          ),
          column(
            6,
            tags$strong("Transaction Status Frame:"),
            p(
              if (ev$ruev_completed == 1) {
                "Completed Log"
              } else {
                "Pending Pipeline Track"
              }
            )
          )
        ),
        br(),
        fluidRow(
          column(
            12,
            div(
              class = "d-flex justify-content-between align-items-center mb-2",
              tags$strong("Top-Level Event Container Summary Notes:"),
              actionButton(
                ns("btn_edit_event_meta"),
                "Edit Event Logs",
                class = "btn btn-sm btn-outline-secondary",
                icon = icon("edit")
              )
            ),
            p(
              style = "background-color: #f8f9fa; padding: 10px; border-radius: 4px; border: 1px solid #dee2e6;",
              if (
                is.null(ev$ruev_summary_notes) || !nzchar(ev$ruev_summary_notes)
              ) {
                "No detailed narrative parameters recorded."
              } else {
                ev$ruev_summary_notes
              }
            )
          )
        )
      )
    })

    output$lead_provider_status_banner <- renderUI({
      lead <- assigned_lead_provider()

      if (is.null(lead)) {
        div(
          class = "alert alert-warning d-flex justify-content-between align-items-center m-0",
          tags$span(
            icon("exclamation-triangle"),
            " No explicit Lead Provider supporting entity linked directly to this event sequence yet."
          ),
          actionButton(
            ns("btn_assign_lead_provider"),
            "Link Supporting Lead",
            class = "btn btn-primary btn-sm",
            icon = icon("link")
          )
        )
      } else {
        div(
          class = "alert alert-info d-flex justify-content-between align-items-center m-0",
          tags$span(
            icon("award"),
            tags$strong(glue::glue(
              " Managed By Lead {lead$lead_entity_type}: "
            )),
            glue::glue("{lead$lead_entity_id} (Track ID: {lead$ruhls_id})")
          ),
          actionButton(
            ns("btn_assign_lead_provider"),
            "Change Lead Provider",
            class = "btn btn-dark btn-sm",
            icon = icon("exchange-alt")
          )
        )
      }
    })

    observeEvent(input$btn_assign_lead_provider, {
      ev <- active_event_data()
      req(ev)

      showModal(modalDialog(
        title = "Link Operational Lead Supporting Node to Event Window",
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel Linkage Track"),
          actionButton(
            ns("submit_lead_provider_binding"),
            "Save Assignment Link",
            class = "btn-success"
          )
        ),
        tagList(
          p(em(
            "Select an active operational institutional lead configuration context to manage this specific event instance."
          )),
          hr(),
          DT::DTOutput(ns("available_leads_picker_table"))
        )
      ))
    })

    output$available_leads_picker_table <- DT::renderDT(
      {
        ev <- active_event_data()
        req(ev)

        all_leads <- dauPortalTools::db_ruh_get_lead_support_records()
        if (is.null(all_leads) || nrow(all_leads) == 0) {
          return(data.frame(
            "Status" = "No registered institutional providers exist in the master system profiles ledger."
          ))
        }

        current_lead <- assigned_lead_provider()
        exclude_lead_ids <- if (is.null(current_lead)) {
          integer(0)
        } else {
          current_lead$ruhls_id
        }

        filtered_pool <- all_leads[
          is.na(all_leads$ruev_id) | all_leads$ruhls_id == exclude_lead_ids,
        ]

        filtered_pool |>
          dplyr::select(
            ruhls_id,
            lead_entity_type,
            lead_entity_id,
            ruhl_dateactive,
            ruhl_active
          ) |>
          dplyr::rename(
            "Master Track ID" = ruhls_id,
            "Provider Type Index" = lead_entity_type,
            "Provider Core ID/URN" = lead_entity_id,
            "Assignment Date Registered" = ruhl_dateactive,
            "Active Flag State" = ruhl_active
          )
      },
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 5, dom = "tp")
    )

    observeEvent(input$submit_lead_provider_binding, {
      req(selected_event_id())
      selected_row_idx <- input$available_leads_picker_table_rows_selected

      if (length(selected_row_idx) == 0) {
        showNotification(
          "You must select an institutional row item to save the context binding.",
          type = "warning"
        )
        return()
      }

      ev <- active_event_data()
      all_leads <- dauPortalTools::db_ruh_get_lead_support_records()
      current_lead <- assigned_lead_provider()
      exclude_lead_ids <- if (is.null(current_lead)) {
        integer(0)
      } else {
        current_lead$ruhls_id
      }
      filtered_pool <- all_leads[
        is.na(all_leads$ruev_id) | all_leads$ruhls_id == exclude_lead_ids,
      ]

      target_ruhls_id <- filtered_pool$ruhls_id[selected_row_idx]
      removeModal()

      if (!is.null(current_lead)) {
        dauPortalTools::db_ruh_update_lead_support(
          ruhls_id = current_lead$ruhls_id,
          lead_type = current_lead$lead_entity_type,
          lead_id = current_lead$lead_entity_id,
          hub_id = current_lead$ruhb_id,
          event_id = NULL,
          date_active = current_lead$ruhl_dateactive,
          is_active = current_lead$ruhl_active,
          comment = current_lead$ruhl_comment,
          user_id = current_user
        )
      }

      selected_lead_meta <- all_leads[all_leads$ruhls_id == target_ruhls_id, ]
      dauPortalTools::db_ruh_update_lead_support(
        ruhls_id = target_ruhls_id,
        lead_type = selected_lead_meta$lead_entity_type,
        lead_id = selected_lead_meta$lead_entity_id,
        hub_id = selected_lead_meta$ruhb_id,
        event_id = as.integer(selected_event_id()),
        date_active = selected_lead_meta$ruhl_dateactive,
        is_active = selected_lead_meta$ruhl_active,
        comment = selected_lead_meta$ruhl_comment,
        user_id = current_user
      )

      showNotification(
        "Lead Provider support structure linked cleanly.",
        type = "message"
      )
      refresh_event_master(refresh_event_master() + 1)
    })

    output$sub_actions_executions_table <- DT::renderDT(
      {
        req(selected_event_id())
        refresh_sub_actions()

        df <- dauPortalTools::db_ru_get_event_action_responses(
          event_id = selected_event_id()
        )
        if (is.null(df) || nrow(df) == 0) {
          return(data.frame(
            "Form Data Metrics" = "No structured action metrics entries are compiled for this entity timeline layout frame yet."
          ))
        }

        df |>
          dplyr::select(
            ruevar_id,
            rueva_name,
            ruevar_value,
            rueva_rule_type,
            rueva_required
          ) |>
          dplyr::rename(
            "Response ID" = ruevar_id,
            "Required Action Parameter" = rueva_name,
            "Logged Value String" = ruevar_value,
            "Data Format Type" = rueva_rule_type,
            "Mandatory?" = rueva_required
          )
      },
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 10, dom = "tp"),
      callback = DT::JS(paste0(
        "
         table.on('dblclick', 'tr', function() {
           var data = table.row(this).data();
           if (data && data[0] !== 'No structured action metrics entries are compiled for this entity timeline layout frame yet.') { 
             Shiny.setInputValue('",
        ns("action_row_dblclicked"),
        "', data[0], {priority: 'event'});
           }
         });
         "
      ))
    )

    observeEvent(input$btn_edit_event_meta, {
      ev <- active_event_data()
      req(ev)

      types_df <- dauPortalTools::db_ru_get_event_types()

      showModal(modalDialog(
        title = "Edit Event Container Specification",
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("save_edited_event_meta"),
            "Save Changes",
            class = "btn-success"
          )
        ),
        tagList(
          fluidRow(
            column(
              6,
              selectInput(
                ns("edit_evt_type_id"),
                "Primary Interaction Method:",
                choices = setNames(types_df$ruevt_id, types_df$ruevt_name),
                selected = ev$ruevt_id
              )
            ),
            column(
              6,
              selectInput(
                ns("edit_evt_sub_id"),
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
                ns("edit_evt_date"),
                "Interaction Event Date:",
                value = as.Date(ev$ruev_date)
              )
            ),
            column(
              6,
              checkboxInput(
                ns("edit_evt_completed"),
                "Mark Event Track as Completed?",
                value = identical(as.integer(ev$ruev_completed), 1L)
              )
            )
          ),
          br(),
          textAreaInput(
            ns("edit_evt_notes"),
            "Top-level Summary Notes:",
            value = ev$ruev_summary_notes,
            rows = 4
          )
        )
      ))

      observe({
        req(input$edit_evt_type_id)
        subs_df <- dauPortalTools::db_ru_get_event_sub_varieties(
          ruevt_id = as.integer(input$edit_evt_type_id)
        )
        updateSelectInput(
          session = session,
          inputId = "edit_evt_sub_id",
          choices = c(
            "No specific sub-option variety" = 0,
            setNames(subs_df$ruesv_id, subs_df$ruesv_name)
          ),
          selected = ev$ruesv_id
        )
      })
    })

    observeEvent(input$save_edited_event_meta, {
      req(selected_event_id(), input$edit_evt_date, input$edit_evt_type_id)
      removeModal()

      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      update_query <- glue::glue_sql(
        "UPDATE {utils_resolve_schema('db_schema_01r')}.[ru_events]
         SET [ruevt_id] = {as.integer(input$edit_evt_type_id)},
             [ruesv_id] = {as.integer(input$edit_evt_sub_id)},
             [ruev_date] = {format(input$edit_evt_date, '%Y-%m-%d')},
             [ruev_summary_notes] = {input$edit_evt_notes},
             [ruev_completed] = {as.integer(input$edit_evt_completed)},
             [date_edited] = SYSUTCDATETIME(),
             [user_id_edited] = {current_user}
         WHERE [ruev_id] = {as.integer(selected_event_id())};",
        .con = conn
      )

      DBI::dbExecute(conn, update_query)
      showNotification(
        "Event layout definitions updated successfully.",
        type = "message"
      )
      refresh_event_master(refresh_event_master() + 1)
    })

    observeEvent(input$btn_add_action_response, {
      req(selected_event_id())
      ev <- active_event_data()
      req(ev)

      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      avail_actions <- DBI::dbGetQuery(
        conn,
        glue::glue_sql(
          "SELECT [rueva_id], [rueva_name], [rueva_rule_type], [rueva_description]
           FROM {utils_resolve_schema('db_schema_01r')}.[ru_event_actions]
           WHERE [ruevt_id] = {as.integer(ev$ruevt_id)}
             AND [rueva_id] NOT IN (
                 SELECT [rueva_id] 
                 FROM {utils_resolve_schema('db_schema_01r')}.[ru_event_action_responses] 
                 WHERE [ruev_id] = {as.integer(selected_event_id())}
             );",
          .con = conn
        )
      )

      if (is.null(avail_actions) || nrow(avail_actions) == 0) {
        showNotification(
          "All blueprinted form metrics have already been initialized on this instance container layer.",
          type = "warning"
        )
        return()
      }

      showModal(modalDialog(
        title = "Add Dynamic Parameter Action Response",
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("submit_new_action_response"),
            "Instantiate Response Record",
            class = "btn-success"
          )
        ),
        tagList(
          selectInput(
            ns("new_action_param_id"),
            "Select Metric Target Action:",
            choices = setNames(avail_actions$rueva_id, avail_actions$rueva_name)
          ),
          uiOutput(ns("new_action_dynamic_input_wrapper"))
        )
      ))
    })

    output$new_action_dynamic_input_wrapper <- renderUI({
      req(input$new_action_param_id)
      ev <- active_event_data()
      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      action_meta <- DBI::dbGetQuery(
        conn,
        glue::glue_sql(
          "SELECT [rueva_rule_type], [rueva_description] FROM {utils_resolve_schema('db_schema_01r')}.[ru_event_actions] WHERE [rueva_id] = {as.integer(input$new_action_param_id)};",
          .con = conn
        )
      )
      req(nrow(action_meta) == 1)

      rule_type <- action_meta$rueva_rule_type[1]

      if (identical(rule_type, "Integer") || identical(rule_type, "Numeric")) {
        numericInput(
          ns("new_action_param_val_numeric"),
          "Logged Numeric Value:",
          value = NULL,
          min = 0
        )
      } else if (identical(rule_type, "Date")) {
        dateInput(
          ns("new_action_param_val_date"),
          "Logged Calendar Date:",
          value = Sys.Date()
        )
      } else if (identical(rule_type, "Boolean")) {
        checkboxInput(
          ns("new_action_param_val_bool"),
          "Logged Condition Met State (True/False)",
          value = FALSE
        )
      } else if (identical(rule_type, "Dropdown")) {
        choices_arr <- trimws(strsplit(action_meta$rueva_description[1], ",")[[
          1
        ]])
        selectInput(
          ns("new_action_param_val_text"),
          "Select Option Entry:",
          choices = choices_arr,
          width = "100%"
        )
      } else {
        textInput(
          ns("new_action_param_val_text"),
          "Logged Configuration String Metric Value:",
          value = ""
        )
      }
    })

    observeEvent(input$submit_new_action_response, {
      req(selected_event_id(), input$new_action_param_id)

      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      action_meta <- DBI::dbGetQuery(
        conn,
        glue::glue_sql(
          "SELECT [rueva_rule_type] FROM {utils_resolve_schema('db_schema_01r')}.[ru_event_actions] WHERE [rueva_id] = {as.integer(input$new_action_param_id)};",
          .con = conn
        )
      )
      req(nrow(action_meta) == 1)
      rule_type <- action_meta$rueva_rule_type[1]

      final_val <- if (
        identical(rule_type, "Integer") || identical(rule_type, "Numeric")
      ) {
        as.character(input$new_action_param_val_numeric)
      } else if (identical(rule_type, "Date")) {
        format(as.Date(input$new_action_param_val_date), "%Y-%m-%d")
      } else if (identical(rule_type, "Boolean")) {
        if (isTRUE(input$new_action_param_val_bool)) "True" else "False"
      } else {
        as.character(input$new_action_param_val_text)
      }

      removeModal()

      dauPortalTools::db_ru_save_event_action_response(
        event_id = as.integer(selected_event_id()),
        rueva_id = as.integer(input$new_action_param_id),
        response_value = final_val,
        user_id = current_user
      )

      showNotification(
        "Action response appended seamlessly into instance layer.",
        type = "message"
      )
      refresh_sub_actions(refresh_sub_actions() + 1)
    })

    observeEvent(input$action_row_dblclicked, {
      resp_id <- as.integer(input$action_row_dblclicked)
      req(resp_id)

      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      resp_row <- DBI::dbGetQuery(
        conn,
        glue::glue_sql(
          "SELECT r.[ruevar_id], r.[ruevar_value], a.[rueva_name], a.[rueva_rule_type], a.[rueva_description]
           FROM {utils_resolve_schema('db_schema_01r')}.[ru_event_action_responses] r
           INNER JOIN {utils_resolve_schema('db_schema_01r')}.[ru_event_actions] a ON r.[rueva_id] = a.[rueva_id]
           WHERE r.[ruevar_id] = {resp_id};",
          .con = conn
        )
      )
      req(nrow(resp_row) == 1)

      rule_type <- resp_row$rueva_rule_type[1]
      parsed_val <- resp_row$ruevar_value[1]
      label_text_value <- paste0(
        "Modify Entry Value (Data Format: ",
        rule_type,
        "):"
      )

      showModal(modalDialog(
        title = paste0("Update Entry: ", resp_row$rueva_name),
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("save_edited_action_response"),
            "Update Parameter Entry",
            class = "btn-success"
          )
        ),
        tagList(
          tags$input(
            id = ns("edit_action_resp_id"),
            type = "hidden",
            value = resp_id
          ),
          tags$input(
            id = ns("edit_action_resp_rule_type"),
            type = "hidden",
            value = rule_type
          ),

          fluidRow(
            column(
              12,
              if (
                identical(rule_type, "Integer") ||
                  identical(rule_type, "Numeric")
              ) {
                numericInput(
                  ns("edit_action_resp_val_numeric"),
                  label = label_text_value,
                  value = as.numeric(parsed_val),
                  min = 0
                )
              } else if (identical(rule_type, "Date")) {
                dateInput(
                  ns("edit_action_resp_val_date"),
                  label = label_text_value,
                  value = as.Date(parsed_val)
                )
              } else if (identical(rule_type, "Boolean")) {
                checkboxInput(
                  ns("edit_action_resp_val_bool"),
                  label = HTML(paste0(
                    "<strong>",
                    label_text_value,
                    "</strong>"
                  )),
                  value = identical(parsed_val, "True")
                )
              } else if (identical(rule_type, "Dropdown")) {
                parsed_choices <- trimws(strsplit(
                  resp_row$rueva_description[1],
                  ","
                )[[1]])
                selectInput(
                  ns("edit_action_resp_val_text"),
                  label = label_text_value,
                  choices = parsed_choices,
                  selected = parsed_val,
                  width = "100%"
                )
              } else {
                textInput(
                  ns("edit_action_resp_val_text"),
                  label = label_text_value,
                  value = parsed_val,
                  width = "100%"
                )
              }
            )
          )
        )
      ))
    })

    observeEvent(input$save_edited_action_response, {
      req(input$edit_action_resp_id, input$edit_action_resp_rule_type)
      removeModal()

      resp_id <- as.integer(input$edit_action_resp_id)
      rule_type <- input$edit_action_resp_rule_type

      final_value <- if (
        identical(rule_type, "Integer") || identical(rule_type, "Numeric")
      ) {
        as.character(input$edit_action_resp_val_numeric)
      } else if (identical(rule_type, "Date")) {
        format(as.Date(input$edit_action_resp_val_date), "%Y-%m-%d")
      } else if (identical(rule_type, "Boolean")) {
        if (isTRUE(input$edit_action_resp_val_bool)) "True" else "False"
      } else {
        as.character(input$edit_action_resp_val_text)
      }

      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      update_resp_query <- glue::glue_sql(
        "UPDATE {utils_resolve_schema('db_schema_01r')}.[ru_event_action_responses]
         SET [ruevar_value] = {final_value},
             [date_edited] = SYSUTCDATETIME(),
             [user_id_edited] = {current_user}
         WHERE [ruevar_id] = {resp_id};",
        .con = conn
      )

      DBI::dbExecute(conn, update_resp_query)
      showNotification(
        "Metric transactional variable modified successfully.",
        type = "message"
      )
      refresh_sub_actions(refresh_sub_actions() + 1)
    })

    return(list(
      go_back = reactive({
        input$back_to_profile
      })
    ))
  })
}
