server_event_instance_page <- function(id, selected_event_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    refresh_header_view <- reactiveVal(0)
    refresh_subactions <- reactiveVal(0)

    parent_type_id_store <- reactiveVal(0)
    parent_sub_id_store <- reactiveVal(0)

    shinyjs::disable("display_type")
    shinyjs::disable("display_sub")
    shinyjs::disable("display_date")
    shinyjs::disable("display_completed")
    shinyjs::disable("display_notes")

    observe({
      req(selected_event_id())
      refresh_header_view()

      ev_df <- dauPortalTools::db_ru_get_events(
        ruev_id = as.integer(selected_event_id())
      )
      req(nrow(ev_df) > 0)

      parent_type_id_store(as.integer(ev_df$ruevt_id[1]))
      parent_sub_id_store(as.integer(ev_df$ruesv_id[1]))

      updateTextInput(session, "display_type", value = ev_df$event_type_name[1])
      updateTextInput(
        session,
        "display_sub",
        value = ev_df$event_sub_variety_name[1]
      )
      updateTextInput(
        session,
        "display_date",
        value = format(as.Date(ev_df$ruev_date[1]), "%A, %d %B %Y")
      )
      updateTextInput(
        session,
        "display_completed",
        value = ifelse(
          ev_df$ruev_completed[1] == 1,
          "Completed / Logged",
          "Pending Action Track"
        )
      )
      updateTextAreaInput(
        session,
        "display_notes",
        value = ev_df$ruev_summary_notes[1]
      )
    })

    output$sub_actions_executions_table <- DT::renderDT({
      refresh_subactions()
      req(selected_event_id())

      conn <- sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "SELECT ex.[rueae_id], act.[rueva_name], ex.[rueae_date], ex.[rueae_comment]
         FROM {utils_resolve_schema('db_schema_01r')}.[ru_event_action_executions] ex
         INNER JOIN {utils_resolve_schema('db_schema_01r')}.[ru_event_actions] act ON ex.[rueva_id] = act.[rueva_id]
         WHERE ex.[ruev_id] = {as.integer(selected_event_id())}
         ORDER BY ex.[rueae_date] DESC, ex.[date_created] DESC;",
        .con = conn
      )
      df <- DBI::dbGetQuery(conn, query)

      if (is.null(df) || nrow(df) == 0) {
        return(data.frame(
          "Status" = "No matching sub-actions executed against this event track container yet."
        ))
      }

      DT::datatable(
        df,
        colnames = c(
          "Execution Row ID",
          "Action Metric / Blueprint",
          "Execution Date",
          "Logged Values"
        ),
        selection = "single",
        rownames = FALSE,
        options = list(
          pageLength = 10,
          dom = "tp",
          columnDefs = list(list(visible = FALSE, targets = 0))
        ),
        callback = DT::JS(paste0(
          "
          table.on('dblclick', 'tr', function() {
            var data = table.row(this).data();
            if (data) { 
              Shiny.setInputValue('",
          ns("subaction_row_dblclicked"),
          "', data[0], {priority: 'event'}); 
            }
          });
          "
        ))
      )
    })

    observeEvent(input$trigger_edit_header_modal, {
      req(selected_event_id())

      ev_df <- dauPortalTools::db_ru_get_events(
        ruev_id = as.integer(selected_event_id())
      )
      req(nrow(ev_df) > 0)

      types_df <- dauPortalTools::db_ru_get_event_types()
      subs_df <- dauPortalTools::db_ru_get_event_sub_varieties(
        ruevt_id = as.integer(ev_df$ruevt_id[1])
      )

      showModal(modalDialog(
        title = "Modify Event Details",
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("submit_edit_header_btn"),
            "Commit Changes",
            class = "btn-warning"
          )
        ),
        tagList(
          fluidRow(
            column(
              6,
              selectInput(
                ns("modal_evt_type_id"),
                "Event Type:",
                choices = setNames(types_df$ruevt_id, types_df$ruevt_name),
                selected = ev_df$ruevt_id[1]
              )
            ),
            column(
              6,
              selectInput(
                ns("modal_evt_sub_id"),
                "Cohort Focus:",
                choices = c(
                  "No specific focus" = 0,
                  setNames(subs_df$ruesv_id, subs_df$ruesv_name)
                ),
                selected = ev_df$ruesv_id[1]
              )
            )
          ),
          br(),
          fluidRow(
            column(
              6,
              dateInput(
                ns("modal_evt_date"),
                "Interaction Date:",
                value = as.Date(ev_df$ruev_date[1])
              )
            ),
            column(
              6,
              selectInput(
                ns("modal_evt_completed"),
                "Transaction Status Flag:",
                choices = c(
                  "Completed / Logged" = 1,
                  "Pending Action Track" = 0
                ),
                selected = ev_df$ruev_completed[1]
              )
            )
          ),
          br(),
          textAreaInput(
            ns("modal_evt_notes"),
            "Notes:",
            value = ev_df$ruev_summary_notes[1],
            rows = 4,
            width = "100%"
          )
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
          "No specific variety" = 0,
          setNames(subs_df$ruesv_id, subs_df$ruesv_name)
        )
      )
    })

    observeEvent(input$submit_edit_header_btn, {
      req(selected_event_id(), input$modal_evt_type_id, input$modal_evt_date)
      removeModal()

      conn <- sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "UPDATE {utils_resolve_schema('db_schema_01r')}.[ru_events]
         SET [ruevt_id]           = {as.integer(input$modal_evt_type_id)},
             [ruesv_id]           = {as.integer(input$modal_evt_sub_id)},
             [ruev_date]          = {format(as.Date(input$modal_evt_date), '%Y-%m-%d')},
             [ruev_completed]     = {as.integer(input$modal_evt_completed)},
             [ruev_summary_notes] = {input$modal_evt_notes},
             [date_edited]        = SYSUTCDATETIME(),
             [user_id_edited]     = {dauPortalTools::get_user(session)}
         WHERE [ruev_id]          = {as.integer(selected_event_id())};",
        .con = conn
      )
      DBI::dbExecute(conn, query)

      showNotification(
        "Base event structural headers context successfully re-saved.",
        type = "message"
      )
      refresh_header_view(refresh_header_view() + 1)
      refresh_subactions(refresh_subactions() + 1)
    })

    observeEvent(input$trigger_add_subaction_modal, {
      req(selected_event_id(), parent_type_id_store())

      blueprint_fields <- dauPortalTools::db_ru_get_event_actions(
        ruevt_id = parent_type_id_store(),
        ruesv_id = parent_sub_id_store()
      )
      choices_vector <- setNames(
        blueprint_fields$rueva_id,
        blueprint_fields$rueva_name
      )

      showModal(modalDialog(
        title = "Execute Sub-Action Metric Requirement Block",
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("submit_subaction_execution_btn"),
            "Save Action Execution",
            class = "btn-success"
          )
        ),
        tagList(
          selectInput(
            ns("modal_exec_action_id"),
            "Select Custom Blueprint Field Target:",
            choices = choices_vector
          ),
          hr(),
          uiOutput(ns("modal_dynamic_datatype_input_container"))
        )
      ))
    })

    output$modal_dynamic_datatype_input_container <- renderUI({
      req(input$modal_exec_action_id, parent_type_id_store())

      blueprint_fields <- dauPortalTools::db_ru_get_event_actions(
        ruevt_id = parent_type_id_store(),
        ruesv_id = parent_sub_id_store()
      )
      selected_field <- blueprint_fields[
        blueprint_fields$rueva_id == as.integer(input$modal_exec_action_id),
      ]
      req(nrow(selected_field) > 0)

      rule_type <- selected_field$rueva_rule_type[1]
      label_text_value <- paste(
        "Enter Measurement Value (Expected Data Type Form:",
        rule_type,
        "):"
      )

      tagList(
        fluidRow(
          column(
            12,
            div(
              class = "alert alert-info",
              style = "margin-bottom: 20px;",
              tags$strong(icon("info-circle"), " Blueprint Guidance Notes:"),
              p(
                style = "margin: 5px 0 0 0; font-size: 0.95em;",
                if (nzchar(selected_field$rueva_description[1] %||% "")) {
                  selected_field$rueva_description[1]
                } else {
                  "No customized deployment instructions configured for this metric field blueprint."
                }
              )
            )
          )
        ),
        br(),
        fluidRow(
          column(
            12,
            if (identical(rule_type, "Integer")) {
              numericInput(
                ns("modal_exec_value_numeric"),
                label = label_text_value,
                value = NULL,
                min = 0,
                step = 1,
                width = "100%"
              )
            } else if (identical(rule_type, "Date")) {
              dateInput(
                ns("modal_exec_value_date"),
                label = label_text_value,
                value = Sys.Date(),
                width = "100%"
              )
            } else if (identical(rule_type, "Boolean")) {
              checkboxInput(
                ns("modal_exec_value_bool"),
                label = HTML(paste0("<strong>", label_text_value, "</strong>")),
                value = FALSE
              )
            } else {
              textInput(
                ns("modal_exec_value_text"),
                label = label_text_value,
                value = "",
                width = "100%"
              )
            }
          )
        ),
        br(),
        fluidRow(
          column(
            6,
            dateInput(
              ns("modal_exec_date"),
              "Action Execution Date:",
              value = Sys.Date(),
              width = "100%"
            )
          ),
          column(
            6,
            textInput(
              ns("modal_exec_audit_notes"),
              "Notes:",
              value = "",
              width = "100%"
            )
          )
        )
      )
    })

    observeEvent(input$submit_subaction_execution_btn, {
      req(
        input$modal_exec_action_id,
        input$modal_exec_date,
        selected_event_id()
      )

      blueprint_fields <- dauPortalTools::db_ru_get_event_actions(
        ruevt_id = parent_type_id_store(),
        ruesv_id = parent_sub_id_store()
      )
      selected_field <- blueprint_fields[
        blueprint_fields$rueva_id == as.integer(input$modal_exec_action_id),
      ]
      req(nrow(selected_field) > 0)

      rule_type <- selected_field$rueva_rule_type[1]

      raw_value_string <- if (identical(rule_type, "Integer")) {
        if (
          is.null(input$modal_exec_value_numeric) ||
            is.na(input$modal_exec_value_numeric)
        ) {
          ""
        } else {
          as.character(input$modal_exec_value_numeric)
        }
      } else if (identical(rule_type, "Date")) {
        if (is.null(input$modal_exec_value_date)) {
          ""
        } else {
          format(as.Date(input$modal_exec_value_date), "%Y-%m-%d")
        }
      } else if (identical(rule_type, "Boolean")) {
        if (isTRUE(input$modal_exec_value_bool)) "True" else "False"
      } else {
        as.character(input$modal_exec_value_text)
      }

      final_comment_entry <- raw_value_string
      if (nzchar(input$modal_exec_audit_notes %||% "")) {
        final_comment_entry <- paste0(
          raw_value_string,
          " | Notes: ",
          input$modal_exec_audit_notes
        )
      }

      removeModal()

      conn <- sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "INSERT INTO {utils_resolve_schema('db_schema_01r')}.[ru_event_action_executions] ([ruev_id], [rueva_id], [rueae_date], [rueae_comment], [user_id_created])
         VALUES ({as.integer(selected_event_id())}, {as.integer(input$modal_exec_action_id)}, {format(as.Date(input$modal_exec_date), '%Y-%m-%d')}, {final_comment_entry}, {dauPortalTools::get_user(session)});",
        .con = conn
      )
      DBI::dbExecute(conn, query)

      showNotification(
        "Sub-action execution row successfully logged.",
        type = "message"
      )
      refresh_subactions(refresh_subactions() + 1)
    })

    observeEvent(input$subaction_row_dblclicked, {
      row_id <- as.integer(input$subaction_row_dblclicked)
      req(row_id)

      conn <- sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "SELECT ex.[rueae_id], ex.[rueva_id], ex.[rueae_date], ex.[rueae_comment], act.[rueva_name], act.[rueva_description], act.[rueva_rule_type]
         FROM {utils_resolve_schema('db_schema_01r')}.[ru_event_action_executions] ex
         INNER JOIN {utils_resolve_schema('db_schema_01r')}.[ru_event_actions] act ON ex.[rueva_id] = act.[rueva_id]
         WHERE ex.[rueae_id] = {row_id};",
        .con = conn
      )
      exec_df <- DBI::dbGetQuery(conn, query)
      req(nrow(exec_df) > 0)

      raw_db_string <- exec_df$rueae_comment[1]
      parsed_val <- raw_db_string
      parsed_notes <- ""

      if (grepl(" \\| Notes: ", raw_db_string)) {
        splits <- strsplit(raw_db_string, " \\| Notes: ")[[1]]
        parsed_val <- splits[1]
        parsed_notes <- splits[2]
      }

      rule_type <- exec_df$rueva_rule_type[1]
      label_text_value <- paste(
        "Modify Measurement Value (Data Type:",
        rule_type,
        "):"
      )

      showModal(modalDialog(
        title = paste("Update Logged Sub-Action Execution ID:", row_id),
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("submit_update_subaction_btn"),
            "Save Changes",
            class = "btn-success"
          )
        ),
        tagList(
          conditionalPanel(
            condition = "false",
            textInput(ns("modal_edit_target_row_id"), value = row_id)
          ),
          conditionalPanel(
            condition = "false",
            textInput(ns("modal_edit_target_rule_type"), value = rule_type)
          ),
          conditionalPanel(
            condition = "false",
            textInput(
              ns("modal_edit_target_action_id"),
              value = exec_df$rueva_id[1]
            )
          ),

          fluidRow(
            column(
              12,
              div(
                class = "alert alert-info",
                tags$strong(icon("info-circle"), " Blueprint Guidance Notes:"),
                p(
                  style = "margin: 5px 0 0 0; font-size: 0.95em;",
                  if (nzchar(exec_df$rueva_description[1] %||% "")) {
                    exec_df$rueva_description[1]
                  } else {
                    "No instructions configured for this blueprint."
                  }
                )
              )
            )
          ),
          br(),
          fluidRow(
            column(
              12,
              if (identical(rule_type, "Integer")) {
                numericInput(
                  ns("modal_edit_value_numeric"),
                  label = label_text_value,
                  value = as.numeric(parsed_val),
                  min = 0,
                  step = 1,
                  width = "100%"
                )
              } else if (identical(rule_type, "Date")) {
                dateInput(
                  ns("modal_edit_value_date"),
                  label = label_text_value,
                  value = as.Date(parsed_val),
                  width = "100%"
                )
              } else if (identical(rule_type, "Boolean")) {
                checkboxInput(
                  ns("modal_edit_value_bool"),
                  label = HTML(paste0(
                    "<strong>",
                    label_text_value,
                    "</strong>"
                  )),
                  value = identical(parsed_val, "True")
                )
              } else {
                textInput(
                  ns("modal_edit_value_text"),
                  label = label_text_value,
                  value = parsed_val,
                  width = "100%"
                )
              }
            )
          ),
          br(),
          fluidRow(
            column(
              6,
              dateInput(
                ns("modal_edit_date"),
                "Action Execution Date:",
                value = as.Date(exec_df$rueae_date[1]),
                width = "100%"
              )
            ),
            column(
              6,
              textInput(
                ns("modal_edit_audit_notes"),
                "Additional Audit/Contextual Notes:",
                value = parsed_notes,
                width = "100%"
              )
            )
          )
        )
      ))
    })

    observeEvent(input$submit_update_subaction_btn, {
      req(
        input$modal_edit_target_row_id,
        input$modal_edit_target_rule_type,
        input$modal_edit_date
      )

      row_id <- as.integer(input$modal_edit_target_row_id)
      rule_type <- input$modal_edit_target_rule_type
      action_id <- as.integer(input$modal_edit_target_action_id)

      removeModal()

      updated_value_string <- if (identical(rule_type, "Integer")) {
        if (
          is.null(input$modal_edit_value_numeric) ||
            is.na(input$modal_edit_value_numeric)
        ) {
          ""
        } else {
          as.character(input$modal_edit_value_numeric)
        }
      } else if (identical(rule_type, "Date")) {
        if (is.null(input$modal_edit_value_date)) {
          ""
        } else {
          format(as.Date(input$modal_edit_value_date), "%Y-%m-%d")
        }
      } else if (identical(rule_type, "Boolean")) {
        if (isTRUE(input$modal_edit_value_bool)) "True" else "False"
      } else {
        as.character(input$modal_edit_value_text)
      }

      final_comment_entry <- updated_value_string
      if (nzchar(input$modal_edit_audit_notes %||% "")) {
        final_comment_entry <- paste0(
          updated_value_string,
          " | Notes: ",
          input$modal_edit_audit_notes
        )
      }

      conn <- sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "UPDATE {utils_resolve_schema('db_schema_01r')}.[ru_event_action_executions]
         SET [rueva_id]       = {action_id},
             [rueae_date]     = {format(as.Date(input$modal_edit_date), '%Y-%m-%d')},
             [rueae_comment]  = {final_comment_entry},
             [date_edited]    = SYSUTCDATETIME(),
             [user_id_edited] = {dauPortalTools::get_user(session)}
         WHERE [rueae_id]     = {row_id};",
        .con = conn
      )
      DBI::dbExecute(conn, query)

      showNotification(
        "Sub-action execution changes committed successfully.",
        type = "message"
      )
      refresh_subactions(refresh_subactions() + 1)
    })

    return(list(
      go_back = reactive({
        input$back_to_profile
      })
    ))
  })
}
