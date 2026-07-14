#' Local Helper: Resolve presentational names for various receiver entities
#' @param entity_type Character scalar (e.g., 'School', 'Trust', 'LA', 'Diocese')
#' @param entity_ids Character vector of URNs, UID codes, or LA numbers
#' @return A named character vector mapping IDs to Names
local_resolve_entity_names <- function(entity_type, entity_ids) {
  if (length(entity_ids) == 0) {
    return(character(0))
  }

  conn <- dauPortalTools::sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  type <- tolower(trimws(entity_type))

  unique_ids <- unique(entity_ids)
  id_list <- paste0("'", unique_ids, "'", collapse = ",")

  if (type == "school") {
    query <- glue::glue(
      "SELECT [urn] AS [id], [school_name] AS [name] FROM {utils_resolve_schema('db_schema_01r')}.[vw_ru_search_schools] WHERE [urn] IN ({id_list});"
    )
  } else if (type == "trust") {
    query <- glue::glue(
      "SELECT [trust_id] AS [id], [trust_name] AS [name] FROM {utils_resolve_schema('db_schema_01r')}.[vw_ru_search_trusts] WHERE [UID] IN ({id_list});"
    )
  } else if (type == "la") {
    query <- glue::glue(
      "SELECT [la_code] AS [id], [la_name] AS [name] FROM {utils_resolve_schema('db_schema_01r')}.[vw_ru_search_la] WHERE [LA_Code] IN ({id_list});"
    )
  } else if (type == "diocese") {
    query <- glue::glue(
      "SELECT [diocese_id] AS [id], [diocese_name] AS [name] FROM {utils_resolve_schema('db_schema_01r')}.[vw_ru_search_diocese] WHERE [Diocese_Code] IN ({id_list});"
    )
  } else {
    return(setNames(unique_ids, unique_ids))
  }

  res <- tryCatch(DBI::dbGetQuery(conn, query), error = function(e) NULL)
  if (is.null(res) || nrow(res) == 0) {
    return(setNames(unique_ids, unique_ids))
  }

  setNames(res$name, res$id)
}

#' Hub Detailed Configuration Overview Panel Server
#'
#' @param id Character scalar. Shiny namespace identifier.
#' @param selected_hub_id ReactiveVal containing the integer primary key of the active Hub.
#' @param selected_urn ReactiveVal tracking the target entity selected for redirect.
#' @param selected_lead_id ReactiveVal tracking the target lead provider assignment selected for redirect.
#' @param main_navbar_session The parent Shiny session used to control page tab navigation.
#' @export
server_hub_overview <- function(
  id,
  selected_hub_id,
  selected_urn,
  selected_lead_id = NULL,
  main_navbar_session
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    refresh_types <- reactiveVal(0)
    editing_type_id <- reactiveVal(NULL)
    active_category_id <- reactiveVal(0)
    active_category_name <- reactiveVal("Global / Hub-Wide")

    observeEvent(input$support_types_table_rows_selected, {
      all_types <- dauPortalTools::db_ruh_get_support_types(
        hub_id = selected_hub_id()
      )
      if (
        nrow(all_types) > 0 && !is.null(input$support_types_table_rows_selected)
      ) {
        selected_row <- all_types[input$support_types_table_rows_selected, ]
        active_category_id(as.integer(selected_row$ruht_id))
        active_category_name(selected_row$ruht_name)
      } else {
        active_category_id(0)
        active_category_name("Global / Hub-Wide")
      }
    })

    hub_data <- reactive({
      req(selected_hub_id())
      dauPortalTools::db_ruh_get_hubs(hub_id = selected_hub_id())
    })

    observeEvent(hub_data(), {
      updateTextInput(session, "hub_name_edit", value = hub_data()$ruhb_name)
    })

    observeEvent(input$save_hub_details, {
      req(selected_hub_id(), input$hub_name_edit)
      dauPortalTools::db_ruh_update_hub(
        hub_id = selected_hub_id(),
        hub_name = input$hub_name_edit,
        user_id = dauPortalTools::get_user(session)
      )
      showNotification(
        "Hub metadata name record committed safely.",
        type = "message"
      )
    })

    output$hub_stats_boxes <- renderUI({
      req(selected_hub_id())
      hid <- selected_hub_id()

      support_data <- dauPortalTools::db_ruh_get_support_records(hub_id = hid)
      lead_data <- dauPortalTools::db_ruh_get_lead_support_records(hub_id = hid)
      types_data <- dauPortalTools::db_ruh_get_support_types(hub_id = hid)

      active_supported <- if (is.null(support_data)) {
        0
      } else {
        length(unique(support_data$ruhsr_entity_id[
          support_data$ruhsr_active == 1
        ]))
      }
      all_time_supported <- if (is.null(support_data)) {
        0
      } else {
        length(unique(support_data$ruhsr_entity_id))
      }
      active_leads <- if (is.null(lead_data)) {
        0
      } else {
        length(unique(lead_data$lead_entity_id[lead_data$ruhl_active == 1]))
      }
      type_count <- nrow(types_data)

      bslib::layout_column_wrap(
        width = 1 / 4,
        gap = "15px",
        fill = FALSE,
        bslib::value_box(
          title = "Active Supported",
          value = active_supported,
          showcase = icon("check-circle"),
          theme = "primary"
        ),
        bslib::value_box(
          title = "All-Time Supported",
          value = all_time_supported,
          showcase = icon("history"),
          theme = "secondary"
        ),
        bslib::value_box(
          title = "Active Lead Providers",
          value = active_leads,
          showcase = icon("star"),
          theme = "success"
        ),
        bslib::value_box(
          title = "Configured Support Modes",
          value = type_count,
          showcase = icon("layer-group"),
          theme = "info"
        )
      )
    })

    output$hub_support_schools_table <- DT::renderDT({
      req(selected_hub_id())
      df <- dauPortalTools::db_ruh_get_support_records(
        hub_id = selected_hub_id()
      )

      if (is.null(df) || nrow(df) == 0) {
        return(data.frame(
          "Status" = "No matching contract allocations recorded under this hub domain."
        ))
      }
      if (!input$include_inactive_support) {
        df <- df |> dplyr::filter(ruhsr_active == 1)
      }
      if (nrow(df) == 0) {
        return(data.frame("Status" = "No matching active allocations found."))
      }

      df$ruhsr_entity_name <- NA_character_
      unique_types <- unique(df$ruhsr_entity_type)

      for (etype in unique_types) {
        sub_rows <- which(df$ruhsr_entity_type == etype)
        ids_to_resolve <- df$ruhsr_entity_id[sub_rows]
        name_map <- local_resolve_entity_names(etype, ids_to_resolve)

        df$ruhsr_entity_name[
          sub_rows
        ] <- name_map[as.character(df$ruhsr_entity_id[sub_rows])]
      }

      df$ruhsr_entity_name <- ifelse(
        is.na(df$ruhsr_entity_name) | df$ruhsr_entity_name == "",
        df$ruhsr_entity_id,
        df$ruhsr_entity_name
      )

      DT::datatable(
        df |>
          dplyr::select(
            ruhsr_id,
            ruhsr_entity_type,
            ruhsr_entity_id,
            ruhsr_entity_name,
            support_type_name,
            ruhsr_dateactive
          ),
        colnames = c(
          "Record ID",
          "Type",
          "ID/URN",
          "Name",
          "Cohort/Framework",
          "Start Date"
        ),
        selection = "single",
        rownames = FALSE,
        options = list(pageLength = 6, dom = "tp"),
        callback = DT::JS(
          "table.on('dblclick', 'tr', function() {
            var data = table.row(this).data();
            if (data) Shiny.setInputValue('hub_school_dblclicked', data[2], {priority: 'event'});
          });"
        )
      )
    })

    output$hub_lead_providers_table <- DT::renderDT({
      req(selected_hub_id())
      leads_df <- dauPortalTools::db_ruh_get_lead_support_records(
        hub_id = selected_hub_id()
      )

      if (is.null(leads_df) || nrow(leads_df) == 0) {
        return(data.frame(
          "Status" = "No designated lead provider entities mapped to this hub framework."
        ))
      }

      if (!input$include_inactive_leads) {
        leads_df <- leads_df |> dplyr::filter(ruhl_active == 1)
      }

      compiled_rows <- lapply(seq_len(nrow(leads_df)), function(i) {
        lead <- leads_df[i, ]
        cohorts <- dauPortalTools::db_ruh_get_lead_cohorts(
          lead$ruhls_id
        )$cohort_id
        cohort_str <- if (length(cohorts) == 0 || all(cohorts == 0)) {
          "Global/Hub-Wide"
        } else {
          paste(cohorts, collapse = ", ")
        }
        cases <- dauPortalTools::db_ruh_get_assigned_support_records(
          lead$ruhls_id
        )
        case_count <- if (is.null(cases)) 0 else nrow(cases)

        resolved_name_vec <- local_resolve_entity_names(
          lead$lead_entity_type,
          lead$lead_entity_id
        )
        resolved_name <- resolved_name_vec[as.character(lead$lead_entity_id)]

        display_name <- if (!is.na(resolved_name) && nzchar(resolved_name)) {
          resolved_name
        } else {
          lead$lead_entity_id
        }

        data.frame(
          ruhls_id = lead$ruhls_id,
          type = lead$lead_entity_type,
          provider_code = lead$lead_entity_id,
          provider_name = display_name,
          cohorts = cohort_str,
          caseload = case_count,
          stringsAsFactors = FALSE
        )
      }) |>
        dplyr::bind_rows()

      DT::datatable(
        compiled_rows,
        colnames = c(
          "Record ID",
          "Type",
          "ID/URN",
          "Name",
          "Cohort/Framework",
          "Entities Supported"
        ),
        selection = "single",
        rownames = FALSE,
        options = list(pageLength = 6, dom = "tp"),
        callback = DT::JS(
          "table.on('dblclick', 'tr', function() {
            var data = table.row(this).data();
            if (data) Shiny.setInputValue('hub_lead_row_dblclicked', data[0], {priority: 'event'});
          });"
        )
      )
    })

    output$support_types_table <- DT::renderDT({
      refresh_types()
      req(selected_hub_id())
      df <- dauPortalTools::db_ruh_get_support_types(hub_id = selected_hub_id())
      if (nrow(df) == 0) {
        return(data.frame(
          "Status" = "No local variant configuration types declared."
        ))
      }

      DT::datatable(
        df |>
          dplyr::select(
            ruht_id,
            ruhb_id,
            Name = ruht_name,
            Description = ruht_description
          ),
        selection = "single",
        rownames = FALSE,
        options = list(
          pageLength = 5,
          dom = 'tp',
          columnDefs = list(list(visible = FALSE, targets = 0:1))
        ),
        callback = DT::JS(
          "table.on('dblclick', 'tr', function() {
            var data = table.row(this).data();
            if (data) Shiny.setInputValue('hub_overview_module-cohort_row_dblclicked', data[0], {priority: 'event'});
          });"
        )
      ) |>
        DT::formatStyle(
          'ruhb_id',
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
      DT::dataTableOutput(ns("hub_actions_blueprint_table"))
    })

    output$hub_actions_blueprint_table <- DT::renderDT({
      refresh_types()
      req(selected_hub_id())

      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      cat_id <- active_category_id()

      if (is.null(cat_id) || as.integer(cat_id) == 0) {
        query <- glue::glue_sql(
          "
          SELECT [ruhbf_id], [ruhbf_name], [ruhbf_rule_type], [ruhbf_required]
          FROM {dauPortalTools::utils_resolve_schema('db_schema_01r')}.[ruh_blueprint_fields]
          WHERE [ruht_id] = 0 AND [ruhb_id] = {as.integer(selected_hub_id())};
          ",
          .con = conn
        )
      } else {
        query <- glue::glue_sql(
          "
          SELECT [ruhbf_id], [ruhbf_name], [ruhbf_rule_type], [ruhbf_required]
          FROM {dauPortalTools::utils_resolve_schema('db_schema_01r')}.[ruh_blueprint_fields]
          WHERE [ruht_id] = {as.integer(cat_id)};
          ",
          .con = conn
        )
      }

      df <- DBI::dbGetQuery(conn, query)

      if (is.null(df) || nrow(df) == 0) {
        return(data.frame(
          "Status" = "No blueprint metric fields defined for this specific configuration state context yet."
        ))
      }

      DT::datatable(
        df,
        colnames = c(
          "Field Record ID",
          "Name",
          "Data Type",
          "Required?"
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
            if (data) Shiny.setInputValue('hub_overview_module-blueprint_row_dblclicked', data[0], {priority: 'event'});
          });"
        )
      )
    })

    observeEvent(input$cohort_row_dblclicked, {
      req(input$cohort_row_dblclicked)
      cohort_id <- as.integer(input$cohort_row_dblclicked)

      all_types <- dauPortalTools::db_ruh_get_support_types(
        hub_id = selected_hub_id()
      )
      record <- all_types[all_types$ruht_id == cohort_id, ]
      req(nrow(record) == 1)

      editing_type_id(cohort_id)
      show_support_type_modal(row_data = record)
    })

    observeEvent(input$blueprint_row_dblclicked, {
      req(input$blueprint_row_dblclicked)
      blueprint_id <- as.integer(input$blueprint_row_dblclicked)

      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "SELECT [ruhbf_id], [ruhbf_name], [ruhbf_description], [ruhbf_rule_type], [ruhbf_required] 
         FROM {dauPortalTools::utils_resolve_schema('db_schema_01r')}.[ruh_blueprint_fields]
         WHERE [ruhbf_id] = {blueprint_id};",
        .con = conn
      )
      record <- DBI::dbGetQuery(conn, query)
      req(nrow(record) == 1)

      showModal(modalDialog(
        title = paste(
          "Modify Complete Blueprint Action Field Rule Configuration"
        ),
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("save_blueprint_edit"),
            "Save Framework Updates",
            class = "btn-success"
          )
        ),
        tagList(
          conditionalPanel(
            "false",
            textInput(
              ns("edit_blueprint_id_hidden"),
              label = "",
              value = blueprint_id
            )
          ),

          textInput(
            inputId = ns("edit_blueprint_name_input"),
            label = "Input Action Field Presentational Label Title:",
            value = record$ruhbf_name
          ),

          selectInput(
            inputId = ns("edit_blueprint_type_input"),
            label = "Storage Format Type Validation Rule:",
            choices = c(
              "Text / String Input" = "Character",
              "Numeric Integer" = "Integer",
              "Calendar Date" = "Date",
              "Binary Checkbox Toggle" = "Boolean",
              "Dropdown Selection Menu" = "Dropdown"
            ),
            selected = record$ruhbf_rule_type
          ),

          shiny::conditionalPanel(
            condition = sprintf(
              "input['%s'] == 'Dropdown'",
              ns("edit_blueprint_type_input")
            ),
            textAreaInput(
              inputId = ns("edit_blueprint_dropdown_options"),
              label = "Dropdown Menu Options (Comma-Separated):",
              value = if (record$ruhbf_rule_type == "Dropdown") {
                record$ruhbf_description
              } else {
                ""
              },
              placeholder = "e.g., Red, Amber, Green",
              rows = 2
            )
          ),
          br(),

          shiny::conditionalPanel(
            condition = sprintf(
              "input['%s'] != 'Dropdown'",
              ns("edit_blueprint_type_input")
            ),
            textAreaInput(
              inputId = ns("edit_blueprint_desc_input"),
              label = "Form Input Guideline Note Hint Text Context:",
              value = if (record$ruhbf_rule_type != "Dropdown") {
                record$ruhbf_description
              } else {
                ""
              },
              rows = 2,
              placeholder = "Optional guidance instructions..."
            )
          ),

          checkboxInput(
            inputId = ns("edit_blueprint_req_input"),
            label = "Force entry selection as mandatory requirement?",
            value = as.logical(record$ruhbf_required)
          )
        )
      ))
    })

    observeEvent(input$save_blueprint_edit, {
      req(
        input$edit_blueprint_id_hidden,
        input$edit_blueprint_name_input,
        input$edit_blueprint_type_input
      )

      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      removeModal()

      final_description <- if (input$edit_blueprint_type_input == "Dropdown") {
        req(input$edit_blueprint_dropdown_options)
        trimws(input$edit_blueprint_dropdown_options)
      } else {
        input$edit_blueprint_desc_input
      }

      query <- glue::glue_sql(
        "UPDATE {dauPortalTools::utils_resolve_schema('db_schema_01r')}.[ruh_blueprint_fields]
         SET 
           [ruhbf_name] = {input$edit_blueprint_name_input},
           [ruhbf_description] = {final_description},
           [ruhbf_rule_type] = {input$edit_blueprint_type_input},
           [ruhbf_required] = {if (input$edit_blueprint_req_input) 1 else 0},
           [modified_date] = SYSUTCDATETIME(), -- Tracking audit timestamps
           [modified_by] = {dauPortalTools::get_user(session)}
         WHERE [ruhbf_id] = {as.integer(input$edit_blueprint_id_hidden)};",
        .con = conn
      )

      tryCatch(
        {
          DBI::dbExecute(conn, query)
          refresh_types(refresh_types() + 1)
          showNotification(
            "Blueprint core validation specifications updated successfully.",
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            paste("Database layout adjustment aborted:", e$message),
            type = "error"
          )
        }
      )
    })

    show_support_type_modal <- function(row_data = NULL) {
      showModal(modalDialog(
        title = if (is.null(editing_type_id())) {
          "Register Support Category / Cohort"
        } else {
          paste("Modify Schema Mode:", row_data$ruht_name)
        },
        size = "m",
        textInput(
          ns("type_name"),
          "Category Cohort Presentation Title Name:",
          value = row_data$ruht_name %||% ""
        ),
        textAreaInput(
          ns("type_desc"),
          "Method Guidelines / Scope Definition Context Notes:",
          value = row_data$ruht_description %||% "",
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

    observeEvent(input$add_support_type, {
      editing_type_id(NULL)
      show_support_type_modal()
    })

    observeEvent(input$save_type, {
      req(input$type_name, selected_hub_id())
      user_id <- dauPortalTools::get_user(session)
      if (is.null(editing_type_id())) {
        dauPortalTools::db_ruh_add_support_type(
          selected_hub_id(),
          input$type_name,
          input$type_desc,
          user_id
        )
        msg <- "New regional cohort tracking layer created."
      } else {
        dauPortalTools::db_ruh_update_support_type(
          editing_type_id(),
          input$type_name,
          input$type_desc,
          user_id
        )
        msg <- "Cohort configuration rules committed."
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
            ns("save_hub_field"),
            "Commit Action Rule",
            class = "btn-success"
          )
        ),
        tagList(
          textInput(
            ns("field_name"),
            "Input Action Field Presentational Label Title:",
            placeholder = "e.g., Initial Action Plan Signed Date"
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
              placeholder = "e.g., Red, Amber, Green",
              rows = 2
            )
          ),
          br(),
          textAreaInput(
            ns("field_desc"),
            "Form Input Guideline Note Hint Text Context:",
            rows = 2,
            placeholder = "Optional guidance instructions..."
          ),
          checkboxInput(
            ns("field_req"),
            "Force entry selection as mandatory requirement?",
            value = FALSE
          )
        )
      ))
    })

    observeEvent(input$save_hub_field, {
      req(input$field_name, input$field_type, selected_hub_id())

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
        INSERT INTO {dauPortalTools::utils_resolve_schema('db_schema_01r')}.[ruh_blueprint_fields] 
        ([ruht_id], [ruhb_id], [ruhsc_id], [ruhbf_name], [ruhbf_description], [ruhbf_rule_type], [ruhbf_required], [ruhbf_active], [created_date], [created_by])
        VALUES (
          {as.integer(active_category_id())}, 
          {as.integer(selected_hub_id())}, 
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
        "Dynamic metric action tracking blueprint rule appended safely.",
        type = "message"
      )
      refresh_types(refresh_types() + 1)
    })

    observeEvent(input$back_to_hubs_search, {
      updateNavbarPage(
        session = main_navbar_session,
        inputId = "main_navbar",
        selected = "hubs_search"
      )
    })
  })
}
