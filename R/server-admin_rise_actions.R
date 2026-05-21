server_rise_actions_admin <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data <- reactiveVal(NULL)
    selected_id <- reactiveVal(NULL)
    editing_row_cache <- reactiveVal(NULL)

    hubs_map <- reactive({
      df <- db_ruh_get_hub_summary()
      if (is.null(df) || nrow(df) == 0) {
        return(character(0))
      }
      setNames(df$ruhb_id, df$hub_name)
    })

    output$add_hub_ui <- renderUI({
      selectInput(
        ns("add_hub"),
        "Associated Hub Scope",
        choices = c("Leave blank (Global / Not Hub Specific)" = 0, hubs_map()),
        selected = 0
      )
    })

    output$add_type_ui <- renderUI({
      chosen_hub <- as.integer(input$add_hub %||% 0)

      df_types <- db_ruh_get_support_types(hub_id = NULL) |>
        dplyr::filter(ruhb_id == 0 | ruhb_id == chosen_hub)

      choices_vector <- setNames(df_types$ruht_id, df_types$ruht_name)

      selectInput(
        ns("add_type"),
        "Parent Support Type",
        choices = c("Leave blank (Unassigned / General)" = 0, choices_vector),
        selected = 0
      )
    })

    output$edit_hub_ui <- renderUI({
      req(editing_row_cache())
      selectInput(
        ns("edit_hub"),
        "Associated Hub Scope",
        choices = c("Leave blank (Global / Not Hub Specific)" = 0, hubs_map()),
        selected = editing_row_cache()$ruhb_id
      )
    })

    output$edit_type_ui <- renderUI({
      req(editing_row_cache(), input$edit_hub)
      chosen_hub <- as.integer(input$edit_hub)

      df_types <- db_ruh_get_support_types(hub_id = NULL) |>
        dplyr::filter(ruhb_id == 0 | ruhb_id == chosen_hub)

      choices_vector := setNames(df_types$ruht_id, df_types$ruht_name)

      current_saved_type <- editing_row_cache()$ruht_id
      fallback_selection <- if (current_saved_type %in% choices_vector) {
        current_saved_type
      } else {
        0
      }

      selectInput(
        ns("edit_type"),
        "Parent Support Type",
        choices = c("Leave blank (Unassigned / General)" = 0, choices_vector),
        selected = fallback_selection
      )
    })

    observeEvent(
      TRUE,
      {
        data(db_ruh_get_actions())
      },
      once = TRUE
    )

    output$table <- DT::renderDT({
      req(data())
      DT::datatable(
        data(),
        selection = "single",
        rownames = FALSE,
        options = list(pageLength = 15, dom = 'ftp')
      )
    })

    observeEvent(input$table_rows_selected, {
      req(input$table_rows_selected)
      row <- data()[input$table_rows_selected, ]

      selected_id(row$ruha_id)
      editing_row_cache(row)

      showModal(rise_action_modal_edit(ns, row))
    })

    observeEvent(input$save_edit, {
      req(
        selected_id(),
        input$edit_name,
        !is.null(input$edit_hub),
        !is.null(input$edit_type)
      )

      db_ruh_update_action(
        ruha_id = selected_id(),
        hub_id = as.integer(input$edit_hub),
        ruht_id = as.integer(input$edit_type),
        action_name = input$edit_name,
        description = input$edit_desc,
        user_id = dauPortalTools::get_user(session)
      )

      removeModal()
      selected_id(NULL)
      editing_row_cache(NULL)
      data(db_ruh_get_actions())
      showNotification(
        "Action blueprint updated successfully.",
        type = "message"
      )
    })

    observeEvent(input$add_new, {
      showModal(rise_action_modal_add(ns))
    })

    observeEvent(input$save_new, {
      req(input$add_name, !is.null(input$add_hub), !is.null(input$add_type))

      db_ruh_add_action(
        hub_id = as.integer(input$add_hub),
        ruht_id = as.integer(input$add_type),
        action_name = input$add_name,
        description = input$add_desc,
        user_id = dauPortalTools::get_user(session)
      )

      removeModal()
      data(db_ruh_get_actions())
      showNotification("New action blueprint created.", type = "message")
    })
  })
}
