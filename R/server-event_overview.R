#' Event Detailed Configuration Overview Panel Server
#'
#' @param id Character scalar. Shiny namespace identifier.
#' @param selected_event_master_id ReactiveVal containing the integer primary key of the active Event group/container.
#' @param selected_urn ReactiveVal tracking the target entity selected for redirect.
#' @param selected_lead_id ReactiveVal tracking the target lead provider assignment selected for redirect.
#' @param main_navbar_session The parent Shiny session used to control page tab navigation.
#' @export
server_event_overview <- function(
  id,
  selected_event_master_id,
  selected_urn,
  selected_lead_id = NULL,
  main_navbar_session
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    refresh_meta_trigger <- reactiveVal(0)
    refresh_types <- reactiveVal(0)
    editing_type_id <- reactiveVal(NULL)
    active_category_id <- reactiveVal(0)
    active_category_name <- reactiveVal("Global / Event-Wide")

    observeEvent(input$event_types_table_rows_selected, {
      all_cohorts <- dauPortalTools::db_ru_get_event_sub_varieties(
        ruevt_id = selected_event_master_id()
      )
      if (
        nrow(all_cohorts) > 0 && !is.null(input$event_types_table_rows_selected)
      ) {
        selected_row <- all_cohorts[input$event_types_table_rows_selected, ]
        active_category_id(as.integer(selected_row$ruesv_id))
        active_category_name(selected_row$ruesv_name)
      } else {
        active_category_id(0)
        active_category_name("Global / Event-Wide")
      }
    })

    event_master_data <- reactive({
      refresh_meta_trigger()
      req(selected_event_master_id())
      dauPortalTools::db_ru_get_event_master_record(
        event_master_id = selected_event_master_id()
      )
    })

    output$type_meta_cards <- renderUI({
      req(event_master_data())
      row <- event_master_data()
      if (nrow(row) == 0) {
        return(p(em("Locating baseline records...")))
      }

      bslib::layout_column_wrap(
        width = 1 / 2,
        bslib::card(
          card_body(
            class = "d-flex justify-content-between align-items-center",
            div(
              tags$h6(
                "Selected Primary Method Classification:",
                class = "text-muted mb-1"
              ),
              tags$h3(
                row$ruevm_name[1],
                class = "mt-0 font-weight-bold text-primary"
              )
            ),
            actionButton(
              ns("edit_parent_type_btn"),
              "Edit Details",
              class = "btn btn-outline-primary btn-sm",
              icon = icon("edit")
            )
          )
        ),
        bslib::card(
          card_body(
            tags$h6(
              "Operational Framework Definition Directives:",
              class = "text-muted mb-1"
            ),
            p(
              row$ruevm_description[1] %||%
                tags$em("No operational guidelines declared.")
            )
          )
        )
      )
    })

    observeEvent(input$edit_parent_type_btn, {
      req(event_master_data())
      row <- event_master_data()

      showModal(modalDialog(
        title = "Amend Primary Event Type Settings",
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("save_parent_edits"),
            "Save Changes",
            class = "btn-primary"
          )
        ),
        tagList(
          textInput(
            ns("edit_parent_name"),
            "Update Method Title Label:",
            value = row$ruevm_name[1]
          ),
          textAreaInput(
            ns("edit_parent_desc"),
            "Amend Scope/Description Definitions:",
            value = row$ruevm_description[1],
            rows = 3
          )
        )
      ))
    })

    observeEvent(input$save_parent_edits, {
      req(input$edit_parent_name, selected_event_master_id())
      removeModal()

      dauPortalTools::db_ru_update_event_type(
        ruevt_id = as.integer(selected_event_master_id()),
        name = input$edit_parent_name,
        description = trimws(input$edit_parent_desc),
        user_id = dauPortalTools::get_user(session)
      )

      showNotification(
        "Primary categorization modifications committed safely.",
        type = "message"
      )
      refresh_meta_trigger(refresh_meta_trigger() + 1)
    })

    output$event_stats_boxes <- renderUI({
      req(selected_event_master_id())
      em_id <- selected_event_master_id()

      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      support_query <- glue::glue_sql(
        "SELECT [ruev_id], [ruev_entity_id] FROM {dauPortalTools::utils_resolve_schema('db_schema_01r')}.[ru_events] WHERE [ruevt_id] = {em_id};",
        .con = conn
      )
      support_df <- DBI::dbGetQuery(conn, support_query)
      cohorts_data <- dauPortalTools::db_ru_get_event_sub_varieties(
        ruevt_id = em_id
      )

      active_supported <- nrow(support_df)
      type_count <- nrow(cohorts_data)

      bslib::layout_column_wrap(
        width = 1 / 4,
        gap = "15px",
        fill = FALSE,
        bslib::value_box(
          title = "Total Recorded Allocations",
          value = active_supported,
          showcase = icon("check-circle"),
          theme = "primary"
        ),
        bslib::value_box(
          title = "Unique Entities Reached",
          value = length(unique(support_df$ruev_entity_id)),
          showcase = icon("history"),
          theme = "secondary"
        ),
        bslib::value_box(
          title = "Configured Sub-Varieties (Cohorts)",
          value = type_count,
          showcase = icon("layer-group"),
          theme = "info"
        )
      )
    })

    output$event_support_schools_table <- DT::renderDT({
      req(selected_event_master_id())

      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "SELECT e.[ruev_id], e.[ruev_entity_type], e.[ruev_entity_id], e.[ruev_date], t.[ruevt_name] AS [event_type_name]
         FROM {dauPortalTools::utils_resolve_schema('db_schema_01r')}.[ru_events] e
         LEFT JOIN {dauPortalTools::utils_resolve_schema('db_schema_01r')}.[ru_event_types] t ON e.[ruevt_id] = t.[ruevt_id]
         WHERE e.[ruevt_id] = {as.integer(selected_event_master_id())}
         ORDER BY e.[ruev_date] DESC;",
        .con = conn
      )
      df <- DBI::dbGetQuery(conn, query)

      if (is.null(df) || nrow(df) == 0) {
        return(data.frame(
          "Status" = "No matching event allocations recorded under this domain."
        ))
      }

      DT::datatable(
        df |>
          dplyr::select(
            ruev_id,
            ruev_entity_type,
            ruev_entity_id,
            event_type_name,
            ruev_date
          ),
        colnames = c(
          "Allocation ID",
          "Entity Node Type",
          "Receiver Identity Code/URN",
          "Event Cohort Type",
          "Logged Date"
        ),
        selection = "single",
        rownames = FALSE,
        options = list(pageLength = 6, dom = "tp"),
        callback = DT::JS(
          "table.on('dblclick', 'tr', function() {
            var data = table.row(this).data();
            if (data) Shiny.setInputValue('event_overview-event_school_dblclicked', data[2], {priority: 'event'});
          });"
        )
      )
    })

    output$event_lead_providers_table <- DT::renderDT({
      req(selected_event_master_id())

      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "SELECT DISTINCT [ruev_entity_type], [ruev_entity_id]
         FROM {dauPortalTools::utils_resolve_schema('db_schema_01r')}.[ru_events]
         WHERE [ruevt_id] = {as.integer(selected_event_master_id())} AND [ruev_entity_type] = 'Lead School';",
        .con = conn
      )
      df <- DBI::dbGetQuery(conn, query)

      if (is.null(df) || nrow(df) == 0) {
        return(data.frame(
          "Status" = "No designated lead entities mapped to active logs."
        ))
      }

      DT::datatable(
        df,
        colnames = c(
          "Provider Classification Layer",
          "Lead Provider Code / Identity URN"
        ),
        selection = "single",
        rownames = FALSE,
        options = list(pageLength = 6, dom = "tp"),
        callback = DT::JS(
          "table.on('dblclick', 'tr', function() {
            var data = table.row(this).data();
            if (data) Shiny.setInputValue('event_overview-event_lead_row_dblclicked', data[1], {priority: 'event'});
          });"
        )
      )
    })

    output$event_types_table <- DT::renderDT({
      refresh_types()
      req(selected_event_master_id())
      df <- dauPortalTools::db_ru_get_event_sub_varieties(
        ruevt_id = selected_event_master_id()
      )
      if (nrow(df) == 0) {
        return(data.frame(
          "Status" = "No sub-varieties declared under this tracking configuration layer."
        ))
      }

      DT::datatable(
        df |>
          dplyr::select(
            ruesv_id,
            Name = ruesv_name,
            Description = ruesv_description
          ),
        selection = "single",
        rownames = FALSE,
        options = list(
          pageLength = 5,
          dom = 'tp',
          columnDefs = list(list(visible = FALSE, targets = 0))
        ),
        callback = DT::JS(
          "table.on('dblclick', 'tr', function() {
            var data = table.row(this).data();
            if (data) Shiny.setInputValue('event_overview-event_cohort_row_dblclicked', data[0], {priority: 'event'});
          });"
        )
      )
    })

    output$sub_workspace_header_title <- renderUI({
      tags$span(
        style = "font-weight: bold;",
        glue::glue("2. Blueprint Fields for: {active_category_name()}")
      )
    })

    output$sub_workspace_dynamic_content <- renderUI({
      DT::dataTableOutput(ns("event_actions_blueprint_table"))
    })

    output$event_actions_blueprint_table <- DT::renderDT({
      refresh_types()
      req(selected_event_master_id())

      df <- dauPortalTools::db_ru_get_event_actions(
        ruevt_id = selected_event_master_id(),
        ruesv_id = active_category_id()
      )

      if (is.null(df) || nrow(df) == 0) {
        return(data.frame("Status" = "No blueprint metric fields defined."))
      }

      DT::datatable(
        df |>
          dplyr::select(rueva_id, rueva_name, rueva_rule_type, rueva_required),
        colnames = c(
          "Field ID",
          "Action Input Field Label",
          "Data Format Type Validation Rule",
          "Mandatory Entry Requirement?"
        ),
        rownames = FALSE,
        options = list(
          pageLength = 5,
          dom = "tp",
          columnDefs = list(list(visible = FALSE, targets = 0))
        ),
        callback = DT::JS(
          "table.on('dblclick', 'tr', function() {
            var data = table.row(this).data();
            if (data) Shiny.setInputValue('event_overview-event_blueprint_row_dblclicked', data[0], {priority: 'event'});
          });"
        )
      )
    })

    observeEvent(input$event_cohort_row_dblclicked, {
      req(input$event_cohort_row_dblclicked)
      cohort_id <- as.integer(input$event_cohort_row_dblclicked)

      all_types <- dauPortalTools::db_ru_get_event_sub_varieties(
        ruevt_id = selected_event_master_id()
      )
      record <- all_types[all_types$ruesv_id == cohort_id, ]
      req(nrow(record) == 1)

      editing_type_id(cohort_id)
      show_event_type_modal(row_data = record)
    })

    observeEvent(input$event_blueprint_row_dblclicked, {
      req(input$event_blueprint_row_dblclicked)
      blueprint_id <- as.integer(input$event_blueprint_row_dblclicked)

      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "SELECT [rueva_id], [rueva_name], [rueva_description], [rueva_rule_type], [rueva_required] 
         FROM {dauPortalTools::utils_resolve_schema('db_schema_01r')}.[ru_event_actions]
         WHERE [rueva_id] = {blueprint_id};",
        .con = conn
      )
      record <- DBI::dbGetQuery(conn, query)
      req(nrow(record) == 1)

      showModal(modalDialog(
        title = "Modify Complete Event Action Field Rule Configuration",
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("save_event_blueprint_full_edit"),
            "Save Framework Updates",
            class = "btn-success"
          )
        ),
        tagList(
          conditionalPanel(
            "false",
            textInput(
              ns("edit_event_blueprint_id_hidden"),
              label = "",
              value = blueprint_id
            )
          ),
          textInput(
            ns("edit_event_blueprint_name_input"),
            "Input Action Field Presentational Label Title:",
            value = record$rueva_name
          ),
          selectInput(
            ns("edit_event_blueprint_type_input"),
            "Storage Format Type Validation Rule:",
            choices = c(
              "Text / String Input" = "Character",
              "Numeric Integer" = "Integer",
              "Calendar Date" = "Date",
              "Binary Checkbox Toggle" = "Boolean",
              "Dropdown Selection Menu" = "Dropdown"
            ),
            selected = record$rueva_rule_type
          ),
          shiny::conditionalPanel(
            condition = sprintf(
              "input['%s'] == 'Dropdown'",
              ns("edit_event_blueprint_type_input")
            ),
            textAreaInput(
              ns("edit_event_blueprint_dropdown_options"),
              "Dropdown Menu Options (Comma-Separated):",
              value = if (record$rueva_rule_type == "Dropdown") {
                record$rueva_description
              } else {
                ""
              },
              placeholder = "e.g., Attended, Apologies",
              rows = 2
            )
          ),
          br(),
          shiny::conditionalPanel(
            condition = sprintf(
              "input['%s'] != 'Dropdown'",
              ns("edit_event_blueprint_type_input")
            ),
            textAreaInput(
              ns("edit_event_blueprint_desc_input"),
              "Form Input Guideline Note Hint Text Context:",
              value = if (record$rueva_rule_type != "Dropdown") {
                record$rueva_description
              } else {
                ""
              },
              rows = 2
            )
          ),
          checkboxInput(
            ns("edit_event_blueprint_req_input"),
            "Force entry selection as mandatory requirement?",
            value = as.logical(record$rueva_required)
          )
        )
      ))
    })

    observeEvent(input$save_event_blueprint_full_edit, {
      req(
        input$edit_event_blueprint_id_hidden,
        input$edit_event_blueprint_name_input,
        input$edit_event_blueprint_type_input
      )
      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)
      removeModal()

      final_description <- if (
        input$edit_event_blueprint_type_input == "Dropdown"
      ) {
        req(input$edit_event_blueprint_dropdown_options)
        trimws(input$edit_event_blueprint_dropdown_options)
      } else {
        input$edit_event_blueprint_desc_input
      }

      query <- glue::glue_sql(
        "UPDATE {dauPortalTools::utils_resolve_schema('db_schema_01r')}.[ru_event_actions]
         SET [rueva_name] = {input$edit_event_blueprint_name_input}, [rueva_description] = {final_description}, [rueva_rule_type] = {input$edit_event_blueprint_type_input}, [rueva_required] = {if (input$edit_event_blueprint_req_input) 1 else 0}
         WHERE [rueva_id] = {as.integer(input$edit_event_blueprint_id_hidden)};",
        .con = conn
      )
      DBI::dbExecute(conn, query)
      refresh_types(refresh_types() + 1)
      showNotification(
        "Event tracking framework field rule configuration updated.",
        type = "message"
      )
    })

    show_event_type_modal <- function(row_data = NULL) {
      showModal(modalDialog(
        title = if (is.null(editing_type_id())) {
          "Register Event Category / Cohort"
        } else {
          paste("Modify Schema Mode:", row_data$ruesv_name)
        },
        size = "m",
        textInput(
          ns("type_name"),
          "Category Cohort Presentation Title Name:",
          value = row_data$ruesv_name %||% ""
        ),
        textAreaInput(
          ns("type_desc"),
          "Method Guidelines / Scope Definition Context Notes:",
          value = row_data$ruesv_description %||% "",
          rows = 4
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("save_type"),
            "Commit Type Spec",
            class = "btn-success"
          )
        ),
        easyClose = TRUE
      ))
    }

    observeEvent(input$add_event_type, {
      editing_type_id(NULL)
      show_event_type_modal()
    })

    observeEvent(input$save_type, {
      req(input$type_name, selected_event_master_id())
      user_id <- dauPortalTools::get_user(session)

      if (is.null(editing_type_id())) {
        dauPortalTools::db_ru_add_event_sub_variety(
          ruevt_id = selected_event_master_id(),
          name = input$type_name,
          description = input$type_desc,
          user_id = user_id
        )
        msg <- "New regional category tracking layer created."
      } else {
        conn <- dauPortalTools::sql_manager("dit")
        on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)
        query <- glue::glue_sql(
          "UPDATE {dauPortalTools::utils_resolve_schema('db_schema_01r')}.[ru_event_sub_varieties]
           SET [ruesv_name] = {input$type_name}, [ruesv_description] = {input$type_desc}
           WHERE [ruesv_id] = {as.integer(editing_type_id())};",
          .con = conn
        )
        DBI::dbExecute(conn, query)
        msg <- "Event configuration rules committed."
      }
      removeModal()
      refresh_types(refresh_types() + 1)
      showNotification(msg, type = "message")
    })

    observeEvent(input$add_blueprint_field_btn, {
      showModal(modalDialog(
        title = glue::glue(
          "Configure Action Input Rule for: {active_category_name()}"
        ),
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("save_event_field"),
            "Commit Action Rule",
            class = "btn-success"
          )
        ),
        tagList(
          textInput(
            ns("field_name"),
            "Input Action Field Presentational Label Title:",
            placeholder = "e.g., Attendance Log Count Sheet Submitted"
          ),
          selectInput(
            ns("field_type"),
            "Storage Format Type Validation Rule:",
            choices = c(
              "Text / String Input" = "Character",
              "Numeric Integer" = "Integer",
              "Calendar Date" = "Date",
              "Binary Checkbox Toggle" = "Boolean",
              "Dropdown Selection Menu" = "Dropdown"
            )
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'Dropdown'", ns("field_type")),
            textAreaInput(
              ns("field_dropdown_options"),
              "Dropdown Menu Options (Comma-Separated):",
              placeholder = "e.g., Attended, Apologies",
              rows = 2
            )
          ),
          br(),
          textAreaInput(
            ns("field_desc"),
            "Form Input Guideline Note Hint Text Context:",
            rows = 2
          ),
          checkboxInput(
            ns("field_req"),
            "Force entry selection as mandatory requirement?",
            value = FALSE
          )
        )
      ))
    })

    observeEvent(input$save_event_field, {
      req(input$field_name, input$field_type, selected_event_master_id())
      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)
      removeModal()

      final_description <- if (input$field_type == "Dropdown") {
        req(input$field_dropdown_options)
        trimws(input$field_dropdown_options)
      } else {
        input$field_desc
      }

      query <- glue::glue_sql(
        "INSERT INTO {dauPortalTools::utils_resolve_schema('db_schema_01r')}.[ru_event_actions] 
         ([ruevt_id], [ruesv_id], [rueva_name], [rueva_description], [rueva_rule_type], [rueva_required])
         VALUES ({as.integer(selected_event_master_id())}, {as.integer(active_category_id())}, {input$field_name}, {final_description}, {input$field_type}, {if (input$field_req) 1 else 0});",
        .con = conn
      )
      DBI::dbExecute(conn, query)
      showNotification(
        "Point-in-time interaction blueprint rule appended safely.",
        type = "message"
      )
      refresh_types(refresh_types() + 1)
    })

    observeEvent(input$back_to_events_search, {
      updateNavbarPage(
        session = main_navbar_session,
        inputId = "main_navbar",
        selected = "events_search"
      )
    })
  })
}
