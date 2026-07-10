#' Polymorphic Hub Provision Support Tracking Page Module Server
#'
#' @param id Character scalar. Shiny namespace identifier.
#' @param selected_support_id ReactiveVal containing the integer primary key ([ruhsr_id]).
#' @param active_target ReactiveValues object containing context variables tracking entity targets.
#' @export
server_hub_support_page <- function(id, selected_support_id, active_target) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    current_user <- dauPortalTools::get_user(session)

    refresh_contract_master <- reactiveVal(0)
    refresh_timeline_actions <- reactiveVal(0)

    active_blueprint_field_id <- reactiveVal(0)

    support_contract_data <- reactive({
      req(selected_support_id())
      refresh_contract_master()

      df <- dauPortalTools::db_ruh_get_support_records(
        ruhsr_id = selected_support_id()
      )
      if (is.null(df) || nrow(df) == 0) {
        return(NULL)
      }
      as.list(df[1, ])
    })

    assigned_lead_provider <- reactive({
      req(selected_support_id())
      refresh_contract_master()

      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "
        SELECT l.* FROM {utils_resolve_schema('db_schema_01r')}.[ruh_lead_assigned_support_records] j
        INNER JOIN {utils_resolve_schema('db_schema_01r')}.[ruh_lead_support_records] l ON j.[ruhls_id] = l.[ruhls_id]
        WHERE j.[ruhsr_id] = {as.integer(selected_support_id())};
        ",
        .con = conn
      )
      df <- DBI::dbGetQuery(conn, query)
      if (nrow(df) == 0) {
        return(NULL)
      }
      as.list(df[1, ])
    })

    output$read_only_contract_profile <- renderUI({
      contract <- support_contract_data()
      req(contract)

      tagList(
        fluidRow(
          column(
            3,
            tags$strong("Target Entity Node:"),
            p(glue::glue(
              "{contract$ruhsr_entity_type} (ID: {contract$ruhsr_entity_id})"
            ))
          ),
          column(
            3,
            tags$strong("Associated Hub Matrix:"),
            p(contract$ruhb_name)
          ),
          column(
            3,
            tags$strong("Framework Classification:"),
            p(contract$support_type_name)
          ),
          column(
            3,
            tags$strong("Active Track Status:"),
            p(
              if (contract$ruhsr_active == 1) {
                "Active Provision"
              } else {
                "Concluded Track"
              }
            )
          )
        ),
        br(),
        fluidRow(
          column(
            3,
            tags$strong("Date Support Commenced:"),
            p(as.character(contract$ruhsr_dateactive))
          ),
          column(
            3,
            tags$strong("Date Support Concluded:"),
            p(
              if (
                is.na(contract$ruhsr_dateended) ||
                  is.null(contract$ruhsr_dateended)
              ) {
                "Ongoing Support Track"
              } else {
                as.character(contract$ruhsr_dateended)
              }
            )
          ),
          column(
            6,
            tags$strong("Narrative Context & Comments History:"),
            p(
              if (
                is.null(contract$ruhsr_comment) ||
                  !nzchar(contract$ruhsr_comment)
              ) {
                "No narrative comments logged."
              } else {
                contract$ruhsr_comment
              }
            )
          )
        )
      )
    })

    observeEvent(input$btn_open_edit_modal, {
      contract <- support_contract_data()
      req(contract)

      hubs_df <- dauPortalTools::db_hubs_lookup()
      hubs_choices <- setNames(hubs_df$ruhb_id, hubs_df$ruhb_name)

      showModal(modalDialog(
        title = "Modify Provision Contract Specifications",
        size = "m",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel Modifications"),
          actionButton(
            ns("submit_contract_master_update"),
            "Commit Modifications",
            class = "btn-success"
          )
        ),
        tagList(
          selectInput(
            ns("modal_edit_hub_id"),
            "Associated Hub Matrix Context:",
            choices = hubs_choices,
            selected = contract$ruhb_id
          ),
          selectInput(
            ns("modal_edit_framework_id"),
            "Target Support Framework / Cohort Type Classification:",
            choices = character(0)
          ),
          selectInput(
            ns("modal_edit_status"),
            "Configuration Active Allocation State Flag:",
            choices = list(
              "Active Support Provision" = 1,
              "Concluded Relationship Track" = 0
            ),
            selected = contract$ruhsr_active
          ),
          ui_date_input(
            ns("modal_edit_date_start"),
            "Framework Initiation Operational Start Date:",
            value = as.Date(contract$ruhsr_dateactive)
          ),
          textAreaInput(
            ns("modal_edit_comment"),
            "Operational Framework Narrative Context & Comments:",
            value = contract$ruhsr_comment,
            rows = 3
          )
        )
      ))
    })

    observeEvent(input$modal_edit_hub_id, {
      req(input$modal_edit_hub_id)
      contract <- support_contract_data()

      frameworks_df <- dauPortalTools::db_ruh_get_support_types(
        hub_id = as.integer(input$modal_edit_hub_id)
      )
      framework_choices <- setNames(
        frameworks_df$ruht_id,
        frameworks_df$ruht_name
      )

      selected_target <- if (
        as.integer(input$modal_edit_hub_id) == as.integer(contract$ruhb_id)
      ) {
        contract$ruht_id
      } else {
        NULL
      }

      updateSelectInput(
        session = session,
        inputId = "modal_edit_framework_id",
        choices = framework_choices,
        selected = selected_target
      )
    })

    observeEvent(input$submit_contract_master_update, {
      req(
        input$modal_edit_hub_id,
        input$modal_edit_framework_id,
        selected_support_id()
      )
      req(
        input$modal_edit_date_start_day,
        input$modal_edit_date_start_month,
        input$modal_edit_date_start_year
      )

      clean_start <- input_to_date("modal_edit_date_start", input)
      if (is.na(clean_start)) {
        showNotification(
          "Invalid Date selection criteria format entered. Re-check parameters.",
          type = "error"
        )
        return()
      }

      removeModal()

      dauPortalTools::db_ruh_update_support_record(
        ruhsr_id = selected_support_id(),
        hub_id = as.integer(input$modal_edit_hub_id),
        ruht_id = as.integer(input$modal_edit_framework_id),
        lead_school_id = NULL,
        date_active = format(clean_start, "%Y-%m-%d"),
        date_ended = if (as.integer(input$modal_edit_status) == 0) {
          format(Sys.Date(), "%Y-%m-%d")
        } else {
          NULL
        },
        is_active = as.integer(input$modal_edit_status),
        comment = input$modal_edit_comment,
        user_id = current_user
      )

      showNotification(
        "Support Provision framework and metadata tracking records modified safely.",
        type = "message"
      )
      refresh_contract_master(refresh_contract_master() + 1)
    })

    output$lead_provider_status_banner <- renderUI({
      lead <- assigned_lead_provider()

      if (is.null(lead)) {
        div(
          class = "alert alert-warning d-flex justify-content-between align-items-center m-0 w-100",
          tags$span(
            icon("exclamation-triangle"),
            " No explicit Lead Provider supporting entity linked to this contract relationship yet."
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
          class = "alert alert-info d-flex justify-content-between align-items-center m-0 w-100",
          tags$span(
            icon("award"),
            tags$strong(glue::glue(
              " Managed Under Lead {lead$lead_entity_type}: "
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
      contract <- support_contract_data()
      req(contract)

      showModal(modalDialog(
        title = "Assign / Re-Route Lead Provider Management Context",
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel Assignment Track"),
          actionButton(
            ns("submit_lead_provider_link"),
            "Save Choice Link",
            class = "btn-success"
          )
        ),
        tagList(
          p(em(glue::glue(
            "Select an active operational institutional lead configuration instance below matching framework: {contract$ruhb_name}."
          ))),
          hr(),
          DT::DTOutput(ns("available_leads_picker_table"))
        )
      ))
    })

    output$available_leads_picker_table <- DT::renderDT(
      {
        contract <- support_contract_data()
        req(contract)

        all_leads <- dauPortalTools::db_ruh_get_lead_support_records(
          hub_id = contract$ruhb_id
        )
        if (is.null(all_leads) || nrow(all_leads) == 0) {
          return(data.frame(
            "Status Notification" = "No registered institutional lead providers match this framework context index pool."
          ))
        }

        all_leads |>
          dplyr::select(
            ruhls_id,
            lead_entity_type,
            lead_entity_id,
            ruhl_dateactive,
            ruhl_active
          ) |>
          dplyr::rename(
            "Master Track ID" = ruhls_id,
            "Provider Type" = lead_entity_type,
            "Provider Core ID/URN" = lead_entity_id,
            "Date Registered" = ruhl_dateactive,
            "Active Status" = ruhl_active
          )
      },
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 5, dom = "tp")
    )

    observeEvent(input$submit_lead_provider_link, {
      req(selected_support_id())
      selected_row_idx <- input$available_leads_picker_table_rows_selected

      if (length(selected_row_idx) == 0) {
        showNotification(
          "You must highlight an institutional row element before saving changes.",
          type = "warning"
        )
        return()
      }

      contract <- support_contract_data()
      all_leads <- dauPortalTools::db_ruh_get_lead_support_records(
        hub_id = contract$ruhb_id
      )
      target_ruhls_id <- all_leads$ruhls_id[selected_row_idx]

      removeModal()

      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      clear_query <- glue::glue_sql(
        "DELETE FROM {utils_resolve_schema('db_schema_01r')}.[ruh_lead_assigned_support_records] WHERE [ruhsr_id] = {as.integer(selected_support_id())};",
        .con = conn
      )
      DBI::dbExecute(conn, clear_query)

      insert_query <- glue::glue_sql(
        "INSERT INTO {utils_resolve_schema('db_schema_01r')}.[ruh_lead_assigned_support_records] ([ruhls_id], [ruhsr_id]) VALUES ({as.integer(target_ruhls_id)}, {as.integer(selected_support_id())});",
        .con = conn
      )
      DBI::dbExecute(conn, insert_query)

      showNotification(
        "Lead Provider support architecture link synchronized safely.",
        type = "message"
      )
      refresh_contract_master(refresh_contract_master() + 1)
    })

    output$actions_table <- DT::renderDT({
      refresh_timeline_actions()
      req(selected_support_id())

      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      df <- DBI::dbGetQuery(
        conn,
        glue::glue_sql(
          "
          SELECT sa.[ruhsa_id], bf.[ruhbf_name], sa.[ruhsa_date], sa.[ruhsa_comment]
          FROM {utils_resolve_schema('db_schema_01r')}.[ruh_support_school_actions] sa
          INNER JOIN {utils_resolve_schema('db_schema_01r')}.[ruh_blueprint_fields] bf ON sa.[ruha_id] = bf.[ruhbf_id]
          WHERE sa.[ruhs_id] = {as.integer(selected_support_id())}
          ORDER BY sa.[ruhsa_date] DESC, sa.[date_created] DESC;
          ",
          .con = conn
        )
      )

      if (is.null(df) || nrow(df) == 0) {
        return(data.frame(
          "Status" = "No structured action metrics entries are compiled for this entity timeline layout frame yet."
        ))
      }

      DT::datatable(
        df,
        colnames = c(
          "Execution Row ID",
          "Action Input Field Label",
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
          ns("actions_table_row_dblclicked"),
          "', data[0], {priority: 'event'}); 
            }
          });
          "
        ))
      )
    })

    observeEvent(input$add_action, {
      contract <- support_contract_data()
      req(contract)
      req(contract$ruht_id)

      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      blueprint_fields <- DBI::dbGetQuery(
        conn,
        glue::glue_sql(
          "
          SELECT [ruhbf_id], [ruhbf_name] + CASE WHEN [ruht_id] = 0 THEN ' (Hub Global)' ELSE ' (Cohort Metric)' END AS [display_label]
          FROM {utils_resolve_schema('db_schema_01r')}.[ruh_blueprint_fields] 
          WHERE [ruht_id] = {as.integer(contract$ruht_id)} 
             OR ([ruht_id] = 0 AND [ruhb_id] = {as.integer(contract$ruhb_id)}); -- Clean relational match
          ",
          .con = conn
        )
      )

      if (is.null(blueprint_fields) || nrow(blueprint_fields) == 0) {
        showModal(modalDialog(
          title = "Configuration Warning",
          p(
            "No blueprint action fields match this specific hub workspace or cohort context framework track yet."
          ),
          easyClose = TRUE
        ))
        return()
      }

      choices_vector <- setNames(
        blueprint_fields$ruhbf_id,
        blueprint_fields$display_label
      )

      showModal(modalDialog(
        title = "Execute Sub-Action Metric Requirement Block",
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel Execution"),
          actionButton(
            ns("submit_subaction_execution_btn"),
            "Save Action Execution",
            class = "btn-success"
          )
        ),
        tagList(
          selectInput(
            ns("modal_exec_field_id"),
            "Select Custom Blueprint Field Target:",
            choices = choices_vector
          ),
          hr(),
          uiOutput(ns("modal_dynamic_datatype_input_container"))
        )
      ))
    })

    output$modal_dynamic_datatype_input_container <- renderUI({
      req(input$modal_exec_field_id)

      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      selected_field <- DBI::dbGetQuery(
        conn,
        glue::glue_sql(
          "SELECT [ruhbf_rule_type], [ruhbf_description] FROM {utils_resolve_schema('db_schema_01r')}.[ruh_blueprint_fields] WHERE [ruhbf_id] = {as.integer(input$modal_exec_field_id)};",
          .con = conn
        )
      )
      req(nrow(selected_field) > 0)

      rule_type <- selected_field$ruhbf_rule_type[1]
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
                if (rule_type == "Dropdown") {
                  "Select a value from the dropdown options pre-configured by your manager."
                } else if (
                  !is.null(selected_field$ruhbf_description[1]) &&
                    nzchar(selected_field$ruhbf_description[1])
                ) {
                  selected_field$ruhbf_description[1]
                } else {
                  "No deployment instructions configured."
                }
              )
            )
          )
        ),
        br(),
        fluidRow(
          column(
            12,
            if (
              identical(rule_type, "Integer") || identical(rule_type, "Numeric")
            ) {
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
            } else if (identical(rule_type, "Dropdown")) {
              raw_options <- selected_field$ruhbf_description[1]
              parsed_choices <- trimws(strsplit(raw_options, ",")[[1]])
              selectInput(
                ns("modal_exec_value_text"),
                label = label_text_value,
                choices = parsed_choices,
                width = "100%"
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
              "Additional Audit/Contextual Notes:",
              value = "",
              width = "100%"
            )
          )
        )
      )
    })

    observeEvent(input$submit_subaction_execution_btn, {
      req(
        input$modal_exec_field_id,
        input$modal_exec_date,
        selected_support_id()
      )

      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      selected_field <- DBI::dbGetQuery(
        conn,
        glue::glue_sql(
          "SELECT [ruhbf_rule_type] FROM {utils_resolve_schema('db_schema_01r')}.[ruh_blueprint_fields] WHERE [ruhbf_id] = {as.integer(input$modal_exec_field_id)};",
          .con = conn
        )
      )
      req(nrow(selected_field) > 0)
      rule_type <- selected_field$ruhbf_rule_type[1]

      raw_value_string <- if (
        identical(rule_type, "Integer") || identical(rule_type, "Numeric")
      ) {
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

      query <- glue::glue_sql(
        "INSERT INTO {utils_resolve_schema('db_schema_01r')}.[ruh_support_school_actions] 
         ([ruhs_id], [ruha_id], [ruhsa_date], [ruhsa_comment], [date_created], [user_id_created])
         VALUES (
           {as.integer(selected_support_id())}, 
           {as.integer(input$modal_exec_field_id)}, -- Correctly maps down to the chosen blueprint field ID
           {format(as.Date(input$modal_exec_date), '%Y-%m-%d')}, 
           {final_comment_entry}, 
           SYSUTCDATETIME(), 
           {current_user}
         );",
        .con = conn
      )
      DBI::dbExecute(conn, query)

      showNotification(
        "Sub-action metric execution successfully logged.",
        type = "message"
      )
      refresh_timeline_actions(refresh_timeline_actions() + 1)
    })

    observeEvent(input$actions_table_row_dblclicked, {
      row_id <- as.integer(input$actions_table_row_dblclicked)
      req(row_id)

      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "SELECT sa.[ruhsa_id], sa.[ruha_id], sa.[ruhsa_date], sa.[ruhsa_comment], bf.[ruhbf_name], bf.[ruhbf_description], bf.[ruhbf_rule_type]
         FROM {utils_resolve_schema('db_schema_01r')}.[ruh_support_school_actions] sa
         INNER JOIN {utils_resolve_schema('db_schema_01r')}.[ruh_blueprint_fields] bf ON sa.[ruha_id] = bf.[ruhbf_id]
         WHERE sa.[ruhsa_id] = {row_id};",
        .con = conn
      )
      exec_df <- DBI::dbGetQuery(conn, query)
      req(nrow(exec_df) > 0)

      raw_db_string <- exec_df$ruhsa_comment[1]
      parsed_val <- raw_db_string
      parsed_notes <- ""

      if (grepl(" \\| Notes: ", raw_db_string)) {
        splits <- strsplit(raw_db_string, " \\| Notes: ")[[1]]
        parsed_val <- splits[1]
        parsed_notes <- splits[2]
      }

      rule_type <- exec_df$ruhbf_rule_type[1]
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
            textInput(
              ns("modal_edit_target_row_id"),
              label = "",
              value = row_id
            )
          ),
          conditionalPanel(
            condition = "false",
            textInput(
              ns("modal_edit_target_rule_type"),
              label = "",
              value = rule_type
            )
          ),
          conditionalPanel(
            condition = "false",
            textInput(
              ns("modal_edit_target_action_id"),
              label = "",
              value = exec_df$ruha_id[1]
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
                  if (nzchar(exec_df$ruhbf_description[1] %||% "")) {
                    exec_df$ruhbf_description[1]
                  } else {
                    "No deployment instructions configured."
                  }
                )
              )
            )
          ),
          br(),
          fluidRow(
            column(
              12,
              if (
                identical(rule_type, "Integer") ||
                  identical(rule_type, "Numeric")
              ) {
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
              } else if (identical(rule_type, "Dropdown")) {
                raw_options <- exec_df$ruhbf_description[1]
                parsed_choices <- trimws(strsplit(raw_options, ",")[[1]])
                selectInput(
                  ns("modal_edit_value_text"),
                  label = label_text_value,
                  choices = parsed_choices,
                  selected = parsed_val,
                  width = "100%"
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
                value = as.Date(exec_df$ruhsa_date[1]),
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

      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "UPDATE {utils_resolve_schema('db_schema_01r')}.[ruh_support_school_actions]
         SET [ruha_id]        = {action_id},
             [ruhsa_date]     = {format(as.Date(input$modal_edit_date), '%Y-%m-%d')},
             [ruhsa_comment]  = {final_comment_entry},
             [date_edited]    = SYSUTCDATETIME(),
             [user_id_edited] = {current_user}
         WHERE [ruhsa_id]     = {row_id};",
        .con = conn
      )
      DBI::dbExecute(conn, query)

      showNotification(
        "Sub-action ledger entry modifications committed safely.",
        type = "message"
      )
      refresh_timeline_actions(refresh_timeline_actions() + 1)
    })

    return(list(
      go_back = reactive({
        input$back_to_school
      })
    ))
  })
}
