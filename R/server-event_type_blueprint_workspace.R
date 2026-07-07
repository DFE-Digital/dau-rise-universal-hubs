#' Event Type Blueprint Hierarchy Workspace Server with Live Editing & BI Metrics
#' @export
server_event_type_blueprint_workspace <- function(
  id,
  global_selected_event_type_id,
  main_navbar_session
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    refresh_meta_trigger <- reactiveVal(0)
    refresh_sub_trigger <- reactiveVal(0)
    refresh_fields_trigger <- reactiveVal(0)

    target_type_data <- reactive({
      refresh_meta_trigger()
      req(global_selected_event_type_id())

      all_types <- dauPortalTools::db_ru_get_event_types()
      all_types |>
        dplyr::filter(
          as.integer(ruevt_id) == as.integer(global_selected_event_type_id())
        )
    })

    output$type_meta_cards <- renderUI({
      req(target_type_data())
      row <- target_type_data()
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
                row$ruevt_name[1],
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
              row$ruevt_description[1] %||%
                tags$em("No operational guidelines declared.")
            )
          )
        )
      )
    })

    observeEvent(input$edit_parent_type_btn, {
      req(target_type_data())
      row <- target_type_data()

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
            value = row$ruevt_name[1]
          ),
          textAreaInput(
            ns("edit_parent_desc"),
            "Amend Scope/Description Definitions:",
            value = row$ruevt_description[1],
            rows = 3
          )
        )
      ))
    })

    observeEvent(input$save_parent_edits, {
      req(input$edit_parent_name, global_selected_event_type_id())
      removeModal()

      dauPortalTools::db_ru_update_event_type(
        ruevt_id = as.integer(global_selected_event_type_id()),
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

    output$bi_metrics_panel <- renderUI({
      req(global_selected_event_type_id())

      conn <- dauPortalTools::sql_manager("dit")
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      events_query <- glue::glue_sql(
        "SELECT [ruev_id], [ruesv_id], [ruev_entity_id], [ruev_entity_type] 
         FROM {dauPortalTools::utils_resolve_schema('db_schema_01r')}.[ru_events] 
         WHERE [ruevt_id] = {global_selected_event_type_id()};",
        .con = conn
      )
      events_df <- DBI::dbGetQuery(conn, events_query)

      selected_sub_row <- input$sub_varieties_table_rows_selected
      active_sub_id <- NULL
      cohort_label <- "All Combined Sub-Cohorts"

      if (!is.null(selected_sub_row) && nrow(sub_varieties_data()) > 0) {
        active_sub_id <- sub_varieties_data()$ruesv_id[selected_sub_row]
        cohort_label <- paste(
          "Cohort Scope:",
          sub_varieties_data()$ruesv_name[selected_sub_row]
        )
        events_df <- events_df |>
          dplyr::filter(as.integer(ruesv_id) == as.integer(active_sub_id))
      }

      total_transactions <- nrow(events_df)
      distinct_entities <- length(unique(paste0(
        events_df$ruev_entity_type,
        "-",
        events_df$ruev_entity_id
      )))
      total_sub_cohorts <- length(unique(sub_varieties_data()$ruesv_id))

      bslib::layout_column_wrap(
        width = 1 / 3,
        gap = "10px",
        bslib::value_box(
          title = paste("Total Recorded Events (", cohort_label, ")"),
          value = total_transactions,
          showcase = icon("chart-line"),
          theme = "primary"
        ),
        bslib::value_box(
          title = "Unique Institutional Nodes Linked",
          value = distinct_entities,
          showcase = icon("school"),
          theme = "info"
        ),
        bslib::value_box(
          title = "Total Configured Sub-Cohorts",
          value = if (is.null(active_sub_id)) {
            total_sub_cohorts
          } else {
            "N/A (Filter Applied)"
          },
          showcase = icon("folder-open"),
          theme = "secondary"
        )
      )
    })

    sub_varieties_data <- reactive({
      refresh_sub_trigger()
      req(global_selected_event_type_id())
      dauPortalTools::db_ru_get_event_sub_varieties(
        ruevt_id = global_selected_event_type_id()
      )
    })

    output$sub_varieties_table <- DT::renderDT({
      req(sub_varieties_data())
      df <- sub_varieties_data()
      if (nrow(df) == 0) {
        return(data.frame("Status" = "No sub-varieties created yet."))
      }

      DT::datatable(
        df |> dplyr::select(ruesv_id, ruesv_name, ruesv_description),
        colnames = c(
          "Sub ID",
          "Sub-Variety Focus Name",
          "Cohort Framework Definition Description"
        ),
        rownames = FALSE,
        selection = "single",
        options = list(pageLength = 8, dom = "tp")
      )
    })

    observeEvent(input$new_sub_variety_btn, {
      showModal(modalDialog(
        title = "Register New Sub-Variety Focus Option",
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("save_new_sub"),
            "Commit Sub-Variety",
            class = "btn-success"
          )
        ),
        tagList(
          textInput(
            ns("sub_name"),
            "Sub-Variety Name Name String:",
            placeholder = "e.g., 2026 Attendance Conference"
          ),
          textAreaInput(
            ns("sub_desc"),
            "Cohort Definition / Target Framework Details:",
            rows = 3
          )
        )
      ))
    })

    observeEvent(input$save_new_sub, {
      req(input$sub_name, global_selected_event_type_id())
      removeModal()

      dauPortalTools::db_ru_add_event_sub_variety(
        ruevt_id = as.integer(global_selected_event_type_id()),
        name = input$sub_name,
        description = trimws(input$sub_desc),
        user_id = dauPortalTools::get_user(session)
      )

      showNotification(
        "Sub-variety successfully saved underneath parent classification layer.",
        type = "message"
      )
      refresh_sub_trigger(refresh_sub_trigger() + 1)
    })

    fields_data <- reactive({
      refresh_fields_trigger()
      req(global_selected_event_type_id())

      selected_sub_row <- input$sub_varieties_table_rows_selected
      sub_id_filter <- 0

      if (!is.null(selected_sub_row) && nrow(sub_varieties_data()) > 0) {
        sub_id_filter <- sub_varieties_data()$ruesv_id[selected_sub_row]
      }

      dauPortalTools::db_ru_get_event_actions(
        ruevt_id = as.integer(global_selected_event_type_id()),
        ruesv_id = as.integer(sub_id_filter)
      )
    })

    output$fields_config_table <- DT::renderDT({
      req(fields_data())
      df <- fields_data()
      if (nrow(df) == 0) {
        return(data.frame(
          "Status" = "No actions configured for this current layer view context."
        ))
      }

      DT::datatable(
        df |>
          dplyr::select(
            rueva_id,
            rueva_name,
            rueva_description,
            rueva_rule_type,
            rueva_required
          ),
        colnames = c(
          "Field ID",
          "Field Title Input Label",
          "Field Context Description Note Hint",
          "Value Rule Type",
          "Required?"
        ),
        rownames = FALSE,
        options = list(pageLength = 8, dom = "tp")
      )
    })

    observeEvent(input$new_field_btn, {
      req(global_selected_event_type_id())

      subs_df <- dauPortalTools::db_ru_get_event_sub_varieties(
        ruevt_id = global_selected_event_type_id()
      )

      scope_choices <- c(
        "General Event Type Level Scope (Applies to All)" = 0,
        setNames(
          subs_df$ruesv_id,
          paste("Strict Cohort Scope:", subs_df$ruesv_name)
        )
      )

      selected_sub_row <- input$sub_varieties_table_rows_selected
      default_sel <- 0
      if (!is.null(selected_sub_row) && nrow(subs_df) > 0) {
        default_sel <- subs_df$ruesv_id[selected_sub_row]
      }

      showModal(modalDialog(
        title = "Configure Dynamic Metric Field Rule Input Definition",
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("save_new_field"),
            "Commit Field Mapping Rule",
            class = "btn-success"
          )
        ),
        tagList(
          selectInput(
            ns("field_scope_target"),
            "Target Inheritance Rule Layer Scope Allocation:",
            choices = scope_choices,
            selected = default_sel
          ),
          hr(),
          textInput(
            ns("field_name"),
            "Input Field Presentation Label Name:",
            placeholder = "e.g., Total Attendees Count"
          ),
          textAreaInput(
            ns("field_desc"),
            "User Form Input Helper Hint Text / Guideline Notes Description:",
            rows = 2,
            placeholder = "e.g., Enter the total physical attendance numbers."
          ),
          selectInput(
            ns("field_type"),
            "System Storage Format Validation Rule Type:",
            choices = c(
              "Text / String Input" = "Character",
              "Numeric Whole Integer" = "Integer",
              "Calendar Target Date" = "Date",
              "Binary Toggle Checkbox" = "Boolean"
            )
          ),
          checkboxInput(
            ns("field_req"),
            "Force Validation as Mandatory Field Entry?",
            value = FALSE
          )
        )
      ))
    })

    observeEvent(input$save_new_field, {
      req(input$field_name, input$field_type, input$field_scope_target)
      removeModal()

      chosen_scope_id <- as.integer(input$field_scope_target)

      # Pass the dynamic description field element value straight to your tracking table
      dauPortalTools::db_ru_add_event_action(
        event_type_id = as.integer(global_selected_event_type_id()),
        action_name = input$field_name,
        description = trimws(input$field_desc),
        rule_type = input$field_type,
        is_required = if (input$field_req) 1 else 0,
        ruesv_id = chosen_scope_id,
        user_id = dauPortalTools::get_user(session)
      )

      showNotification(
        "Dynamic metric input field appended to the targeted blueprint layer.",
        type = "message"
      )
      refresh_fields_trigger(refresh_fields_trigger() + 1)
    })

    observeEvent(input$back_to_catalog, {
      updateNavbarPage(
        main_navbar_session,
        "main_navbar",
        selected = "events_master_catalog"
      )
    })
  })
}
