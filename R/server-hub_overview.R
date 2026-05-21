server_hub_overview <- function(id, selected_hub_id, selected_urn) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    refresh_types <- reactiveVal(0)
    editing_type_id <- reactiveVal(NULL)

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
      showNotification("Hub name updated successfully.", type = "message")
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
        fill = FALSE,
        style = "margin-bottom: 20px;",

        bslib::value_box(
          title = "Schools Supported (Active)",
          value = active_supported,
          showcase = icon("check-circle"),
          theme = "primary"
        ),

        bslib::value_box(
          title = "Schools Supported (All Time)",
          value = all_time_supported,
          showcase = icon("history"),
          theme = "secondary"
        ),

        bslib::value_box(
          title = "Lead Schools (Active)",
          value = active_leads,
          showcase = icon("star"),
          theme = "warning"
        ),

        bslib::value_box(
          title = "Support Types",
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

      DT::datatable(
        df |> dplyr::select(ruhs_id, ruhs_urn, ruhs_active, ruhs_dateactive),
        colnames = c("Support ID", "School URN", "Active?", "Start Date"),
        selection = "single",
        rownames = FALSE,
        callback = DT::JS(
          "table.on('dblclick', 'tr', function() {
            var data = table.row(this).data();
            if (data) {
              Shiny.setInputValue('hub_school_dblclicked', data[1], {priority: 'event'});
            }
          });"
        )
      )
    })

    output$support_types_table <- DT::renderDT({
      refresh_types()
      req(selected_hub_id())

      df <- db_ruh_get_support_types(hub_id = selected_hub_id())

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
          dom = 't',
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
      selected_urn(as.character(input$hub_school_dblclicked))
    })

    show_support_type_modal <- function(row_data = NULL) {
      showModal(modalDialog(
        title = if (is.null(row_data)) {
          "Add Support Type"
        } else {
          paste("Edit:", row_data$ruht_name)
        },
        textInput(
          ns("type_name"),
          "Type Name",
          value = row_data$ruht_name %||% ""
        ),
        textAreaInput(
          ns("type_desc"),
          "Description",
          value = row_data$ruht_description %||% "",
          rows = 4
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_type"), "Save Changes", class = "btn-success")
        ),
        easyClose = TRUE
      ))
    }

    observeEvent(input$add_support_type, {
      editing_type_id(NULL)
      show_support_type_modal()
    })

    observeEvent(input$support_types_table_rows_selected, {
      req(input$support_types_table_rows_selected)
      all_types <- db_ruh_get_support_types(hub_id = selected_hub_id())
      selected_row <- all_types[input$support_types_table_rows_selected, ]

      if (selected_row$ruhb_id == 0) {
        showNotification(
          "Global support types are read-only.",
          type = "warning"
        )
        return()
      }

      editing_type_id(selected_row$ruht_id)
      show_support_type_modal(selected_row)
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
        msg <- "Support type added."
      } else {
        db_ruh_update_support_type(
          editing_type_id(),
          input$type_name,
          input$type_desc,
          user_id
        )
        msg <- "Support type updated."
      }

      removeModal()
      refresh_types(refresh_types() + 1)
      showNotification(msg, type = "message")
    })
  })
}
