#' Hub Detailed Configuration Overview Panel Server
#' @export
server_hub_overview <- function(
  id,
  selected_hub_id,
  selected_urn,
  main_navbar_session
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    refresh_types <- reactiveVal(0)
    editing_type_id <- reactiveVal(NULL)
    active_category_id <- reactiveVal(NULL)
    active_category_name <- reactiveVal("")

    observeEvent(input$support_types_table_rows_selected, {
      all_types <- db_ruh_get_support_types(hub_id = selected_hub_id())
      if (
        nrow(all_types) > 0 && !is.null(input$support_types_table_rows_selected)
      ) {
        selected_row <- all_types[input$support_types_table_rows_selected, ]
        active_category_id(selected_row$ruht_id)
        active_category_name(selected_row$ruht_name)
      }
    })

    hub_data <- reactive({
      req(selected_hub_id())
      db_ruh_get_hubs(hub_id = selected_hub_id())
    })

    observeEvent(hub_data(), {
      updateTextInput(session, "hub_name_edit", value = hub_data()$ruhb_name)
    })

    observeEvent(input$save_hub_details, {
      req(selected_hub_id(), input$hub_name_edit)
      db_ruh_update_hub(
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
      support_data <- db_ruh_get_support_schools() |>
        dplyr::filter(ruhb_id == hid)
      lead_data <- db_ruh_get_lead_schools(hub_id = hid)
      types_data <- db_ruh_get_support_types(hub_id = hid)

      active_supported <- length(unique(support_data$ruhs_urn[
        support_data$ruhs_active == 1
      ]))
      all_time_supported <- length(unique(support_data$ruhs_urn))
      active_leads <- length(unique(lead_data$ruhl_urn[
        lead_data$ruhl_active == 1
      ]))
      type_count <- nrow(types_data)

      bslib::layout_column_wrap(
        width = 1 / 4,
        gap = "15px",
        fill = FALSE,
        bslib::value_box(
          title = "Active Supported Schools",
          value = active_supported,
          showcase = icon("check-circle"),
          theme = "primary"
        ),
        bslib::value_box(
          title = "All-Time Provision Footprint",
          value = all_time_supported,
          showcase = icon("history"),
          theme = "secondary"
        ),
        bslib::value_box(
          title = "Active Lead Systems Operational",
          value = active_leads,
          showcase = icon("star"),
          theme = "success"
        ),
        bslib::value_box(
          title = "Configured Provision Modes",
          value = type_count,
          showcase = icon("layer-group"),
          theme = "info"
        )
      )
    })

    output$hub_support_schools_table <- DT::renderDT({
      req(selected_hub_id())
      df <- db_ruh_get_support_schools() |>
        dplyr::filter(ruhb_id == selected_hub_id())
      if (!input$include_inactive) {
        df <- df |> dplyr::filter(ruhs_active == 1)
      }
      if (nrow(df) == 0) {
        return(data.frame(
          "Status" = "No matching school allocations recorded under this hub domain."
        ))
      }

      DT::datatable(
        df |> dplyr::select(ruhs_id, ruhs_urn, ruhs_active, ruhs_dateactive),
        colnames = c(
          "Support Provision ID",
          "School URN Reference",
          "Active Assignment?",
          "Framework Initialization Date"
        ),
        selection = "single",
        rownames = FALSE,
        options = list(pageLength = 8, dom = "tp"),
        callback = DT::JS(
          "table.on('dblclick', 'tr', function() {
            var data = table.row(this).data();
            if (data) Shiny.setInputValue('hub_school_dblclicked', data[1], {priority: 'event'});
          });"
        )
      )
    })

    output$support_types_table <- DT::renderDT({
      refresh_types()
      req(selected_hub_id())
      df <- db_ruh_get_support_types(hub_id = selected_hub_id())
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
          pageLength = 8,
          dom = 'tp',
          columnDefs = list(list(visible = FALSE, targets = 0:1))
        )
      ) |>
        DT::formatStyle(
          'ruhb_id',
          target = 'row',
          backgroundColor = DT::styleEqual(0, '#f8f9fa')
        )
    })

    observeEvent(input$hub_school_dblclicked, {
      req(input$hub_school_dblclicked)
      selected_urn(as.integer(input$hub_school_dblclicked))
      updateNavbarPage(
        session = main_navbar_session,
        inputId = "main_navbar",
        selected = "school_overview"
      )
    })

    output$sub_workspace_header_title <- renderUI({
      if (is.null(active_category_id())) {
        tags$span(
          style = "font-weight: bold;",
          "2. Action Blueprint Fields Configuration"
        )
      } else {
        tags$span(
          style = "font-weight: bold;",
          glue::glue("2. Blueprint Fields for: {active_category_name()}")
        )
      }
    })

    output$sub_workspace_dynamic_content <- renderUI({
      if (is.null(active_category_id())) {
        return(shiny::div(
          style = "text-align: center; color: #505a5f; padding-top: 80px;",
          icon("mouse-pointer", class = "fa-2x"),
          br(),
          br(),
          "Select a cohort or support category from the table on the left to configure its field blueprint tracking specifications."
        ))
      }

      DT::dataTableOutput(ns("hub_actions_blueprint_table"))
    })

    output$hub_actions_blueprint_table <- DT::renderDT({
      req(active_category_id())
      refresh_types()

      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      df <- DBI::dbGetQuery(
        conn,
        glue::glue_sql(
          "
        SELECT [ruhbf_name], [ruhbf_rule_type], [ruhbf_required] 
        FROM {dauPortalTools::utils_resolve_schema('db_schema_01r')}.[ruh_blueprint_fields]
        WHERE [ruht_id] = {active_category_id()} AND [ruhbf_active] = 1;
      ",
          .con = conn
        )
      )

      if (nrow(df) == 0) {
        return(data.frame(
          "Status" = "No fields blueprinted for this specific category cohort."
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
        options = list(pageLength = 8, dom = "tp")
      )
    })

    show_support_type_modal <- function(row_data = NULL) {
      showModal(modalDialog(
        title = if (is.null(row_data)) {
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
        db_ruh_add_support_type(
          selected_hub_id(),
          input$type_name,
          input$type_desc,
          user_id
        )
        msg <- "New regional cohort tracking layer created."
      } else {
        db_ruh_update_support_type(
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
      req(active_category_id())
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
          textAreaInput(
            ns("field_desc"),
            "Form Input Guideline Note Hint Text Context:",
            rows = 2
          ),
          selectInput(
            ns("field_type"),
            "Storage Format Type Validation Rule:",
            choices = c(
              "Text / String Input" = "Character",
              "Numeric Integer" = "Integer",
              "Calendar Date" = "Date",
              "Binary Checkbox Toggle" = "Boolean"
            )
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
      req(input$field_name, input$field_type, active_category_id())
      removeModal()

      dauPortalTools::db_ruh_add_blueprint_field(
        ruht_id = as.integer(active_category_id()),
        ruhsc_id = 0,
        field_name = input$field_name,
        description = input$field_desc,
        rule_type = input$field_type,
        is_required = if (input$field_req) 1 else 0,
        user_id = dauPortalTools::get_user(session)
      )
      showNotification(
        "Dynamic metric action tracking validation rule appended.",
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
