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

    refresh_types <- reactiveVal(0)
    editing_type_id <- reactiveVal(NULL)
    active_category_id <- reactiveVal(0)
    active_category_name <- reactiveVal("Global / Event-Wide")

    observeEvent(input$event_types_table_rows_selected, {
      all_types <- dauPortalTools::db_ru_get_event_types(
        event_master_id = selected_event_master_id()
      )
      if (
        nrow(all_types) > 0 && !is.null(input$event_types_table_rows_selected)
      ) {
        selected_row <- all_types[input$event_types_table_rows_selected, ]
        active_category_id(as.integer(selected_row$ruevt_id))
        active_category_name(selected_row$ruevt_name)
      } else {
        active_category_id(0)
        active_category_name("Global / Event-Wide")
      }
    })

    event_master_data <- reactive({
      req(selected_event_master_id())
      dauPortalTools::db_ru_get_event_master_record(
        event_master_id = selected_event_master_id()
      )
    })

    observeEvent(event_master_data(), {
      updateTextInput(
        session,
        "event_name_edit",
        value = event_master_data()$ruevm_name
      )
    })

    observeEvent(input$save_event_details, {
      req(selected_event_master_id(), input$event_name_edit)
      dauPortalTools::db_ru_update_event_master(
        event_master_id = selected_event_master_id(),
        event_name = input$event_name_edit,
        user_id = dauPortalTools::get_user(session)
      )
      showNotification(
        "Event metadata name record committed safely.",
        type = "message"
      )
    })

    output$event_stats_boxes <- renderUI({
      req(selected_event_master_id())
      em_id <- selected_event_master_id()

      support_data <- dauPortalTools::db_ru_get_event_support_records(
        event_master_id = em_id
      )
      lead_data <- dauPortalTools::db_ru_get_event_lead_records(
        event_master_id = em_id
      )
      types_data <- dauPortalTools::db_ru_get_event_types(
        event_master_id = em_id
      )

      active_supported <- if (is.null(support_data)) {
        0
      } else {
        length(unique(support_data$ruevsr_entity_id[
          support_data$ruevsr_active == 1
        ]))
      }
      all_time_supported <- if (is.null(support_data)) {
        0
      } else {
        length(unique(support_data$ruevsr_entity_id))
      }
      active_leads <- if (is.null(lead_data)) {
        0
      } else {
        length(unique(lead_data$lead_entity_id[lead_data$ruevl_active == 1]))
      }
      type_count <- nrow(types_data)

      bslib::layout_column_wrap(
        width = 1 / 4,
        gap = "15px",
        fill = FALSE,
        bslib::value_box(
          title = "Active Event Entities",
          value = active_supported,
          showcase = icon("check-circle"),
          theme = "primary"
        ),
        bslib::value_box(
          title = "All-Time Event Footprint",
          value = all_time_supported,
          showcase = icon("history"),
          theme = "secondary"
        ),
        bslib::value_box(
          title = "Active Event Leaders",
          value = active_leads,
          showcase = icon("star"),
          theme = "success"
        ),
        bslib::value_box(
          title = "Configured Event Modes",
          value = type_count,
          showcase = icon("layer-group"),
          theme = "info"
        )
      )
    })

    output$event_support_schools_table <- DT::renderDT({
      req(selected_event_master_id())
      df <- dauPortalTools::db_ru_get_event_support_records(
        event_master_id = selected_event_master_id()
      )

      if (is.null(df) || nrow(df) == 0) {
        return(data.frame(
          "Status" = "No matching event contract allocations recorded under this domain."
        ))
      }
      if (!input$include_inactive) {
        df <- df |> dplyr::filter(ruevsr_active == 1)
      }
      if (nrow(df) == 0) {
        return(data.frame(
          "Status" = "No matching active event allocations found."
        ))
      }

      DT::datatable(
        df |>
          dplyr::select(
            ruevsr_id,
            ruevsr_entity_type,
            ruevsr_entity_id,
            event_type_name,
            ruevsr_dateactive
          ),
        colnames = c(
          "Event Provision ID",
          "Entity Node Type",
          "Receiver Identity Code/URN",
          "Event Cohort Type",
          "Initialization Date"
        ),
        selection = "single",
        rownames = FALSE,
        options = list(pageLength = 6, dom = "tp"),
        callback = DT::JS(
          "table.on('dblclick', 'tr', function() {
            var data = table.row(this).data();
            if (data) Shiny.setInputValue('event_school_dblclicked', data[2], {priority: 'event'});
          });"
        )
      )
    })

    output$event_lead_providers_table <- DT::renderDT({
      req(selected_event_master_id())
      leads_df <- dauPortalTools::db_ru_get_event_lead_records(
        event_master_id = selected_event_master_id()
      )

      if (is.null(leads_df) || nrow(leads_df) == 0) {
        return(data.frame(
          "Status" = "No designated lead provider entities mapped to this event framework."
        ))
      }

      compiled_rows <- lapply(seq_len(nrow(leads_df)), function(i) {
        lead <- leads_df[i, ]
        cohorts <- dauPortalTools::db_ru_get_event_lead_cohorts(
          lead$ruevls_id
        )$cohort_id
        cohort_str := if (length(cohorts) == 0 || all(cohorts == 0)) {
          "Global/Event-Wide"
        } else {
          paste(cohorts, collapse = ", ")
        }
        cases <- dauPortalTools::db_ru_get_assigned_event_records(
          lead$ruevls_id
        )
        case_count <- if (is.null(cases)) 0 else nrow(cases)

        data.frame(
          ruevls_id = lead$ruevls_id,
          provider_name = paste0(
            lead$lead_entity_type,
            ": ",
            lead$lead_entity_id
          ),
          cohorts = cohort_str,
          caseload = case_count,
          status = if (lead$ruevl_active == 1) "Active" else "Concluded",
          stringsAsFactors = FALSE
        )
      }) |>
        dplyr::bind_rows()

      DT::datatable(
        compiled_rows,
        colnames = c(
          "Master Track ID",
          "Lead Provider Name/Identity",
          "Assigned Cohort Scopes",
          "Entities Supported",
          "Operational Status"
        ),
        selection = "single",
        rownames = FALSE,
        options = list(pageLength = 6, dom = "tp"),
        callback = DT::JS(
          "table.on('dblclick', 'tr', function() {
            var data = table.row(this).data();
            if (data) Shiny.setInputValue('event_lead_row_dblclicked', data[0], {priority: 'event'});
          });"
        )
      )
    })

    observeEvent(input$event_school_dblclicked, {
      req(input$event_school_dblclicked)
      selected_urn(as.character(input$event_school_dblclicked))
      updateNavbarPage(
        session = main_navbar_session,
        inputId = "main_navbar",
        selected = "school_overview"
      )
    })

    observeEvent(input$event_lead_row_dblclicked, {
      req(input$event_lead_row_dblclicked, selected_lead_id)
      selected_lead_id(as.integer(input$event_lead_row_dblclicked))
      updateNavbarPage(
        session = main_navbar_session,
        inputId = "main_navbar",
        selected = "event_lead_management"
      )
    })

    output$event_types_table <- DT::renderDT({
      refresh_types()
      req(selected_event_master_id())
      df <- dauPortalTools::db_ru_get_event_types(
        event_master_id = selected_event_master_id()
      )
      if (nrow(df) == 0) {
        return(data.frame(
          "Status" = "No local event variant configuration types declared."
        ))
      }

      DT::datatable(
        df |>
          dplyr::select(
            ruevt_id,
            ruevm_id,
            Name = ruevt_name,
            Description = ruevt_description
          ),
        selection = "single",
        rownames = FALSE,
        options = list(
          pageLength = 5,
          dom = 'tp',
          columnDefs = list(list(visible = FALSE, targets = 0:1))
        )
      ) |>
        DT::formatStyle(
          'ruevm_id',
          target = 'row',
          backgroundColor = DT::styleEqual(0, '#f8f9fa')
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

      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "
        SELECT [rueva_name], [rueva_rule_type], [rueva_required] 
        FROM {dauPortalTools::utils_resolve_schema('db_schema_01r')}.[ru_event_actions]
        WHERE [ruevt_id] = {as.integer(active_category_id())} AND [ruevm_id] = {as.integer(selected_event_master_id())};
        ",
        .con = conn
      )
      df <- DBI::dbGetQuery(conn, query)

      if (is.null(df) || nrow(df) == 0) {
        return(data.frame(
          "Status" = "No blueprint metric fields defined for this specific configuration state context yet."
        ))
      }

      DT::datatable(
        df,
        colnames = c(
          "Action Input Field Label",
          "Data Format Type Validation Rule",
          "Mandatory Entry Requirement?"
        ),
        rownames = FALSE,
        options = list(pageLength = 5, dom = "tp")
      )
    })

    show_event_type_modal <- function(row_data = NULL) {
      showModal(modalDialog(
        title = if (is.null(row_data)) {
          "Register Event Category / Cohort"
        } else {
          paste("Modify Schema Mode:", row_data$ruevt_name)
        },
        size = "m",
        textInput(
          ns("type_name"),
          "Category Cohort Presentation Title Name:",
          value = row_data$ruevt_name %% ""
        ),
        textAreaInput(
          ns("type_desc"),
          "Method Guidelines / Scope Definition Context Notes:",
          value = row_data$ruevt_description %|% "",
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
        dauPortalTools::db_ru_add_event_type(
          selected_event_master_id(),
          input$type_name,
          input$type_desc,
          user_id
        )
        msg <- "New point-in-time regional event category tracking layer created."
      } else {
        dauPortalTools::db_ru_update_event_type(
          editing_type_id(),
          input$type_name,
          input$type_desc,
          user_id
        )
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
              placeholder = "e.g., Attended, Apologies, No Show",
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
        "
        INSERT INTO {dauPortalTools::utils_resolve_schema('db_schema_01r')}.[ru_event_actions] 
        ([ruevt_id], [ruevm_id], [ruesv_id], [rueva_name], [rueva_description], [rueva_rule_type], [rueva_required], [rueva_active], [created_date], [created_by])
        VALUES (
          {as.integer(active_category_id())}, 
          {as.integer(selected_event_master_id())},
          0,
          {input$field_name}, 
          {final_description}, 
          {input$field_type}, 
          {if (input$field_req) 1 else 0}, 
          1, 
          SYSUTCDATETIME(), 
          {dauPortalTools::get_user(session)}
        );
        ",
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
