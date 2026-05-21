server_rise_support_types_admin <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data <- reactiveVal(NULL)
    selected_id <- reactiveVal(NULL)

    hubs_map <- reactive({
      df <- db_ruh_get_hub_summary()
      if (is.null(df) || nrow(df) == 0) {
        return(character(0))
      }

      setNames(df$ruhb_id, df$hub_name)
    })

    observeEvent(
      TRUE,
      {
        res <- db_ruh_get_support_types(hub_id = NULL)
        data(res)
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
      selected_id(row$ruht_id)

      showModal(rise_support_type_modal_edit(ns, row, hubs_map()))
    })

    observeEvent(input$save_edit, {
      req(selected_id())
      req(input$edit_name)

      db_ruh_update_support_type(
        ruht_id = selected_id(),
        name = input$edit_name,
        description = input$edit_desc,
        hub_id = as.integer(input$edit_hub_id),
        user_id = dauPortalTools::get_user(session)
      )

      removeModal()
      selected_id(NULL)
      data(db_ruh_get_support_types(hub_id = NULL))
      showNotification("Support type updated successfully.", type = "message")
    })

    observeEvent(input$add_new, {
      showModal(rise_support_type_modal_add(ns, hubs_map()))
    })

    observeEvent(input$save_new, {
      req(input$add_name)

      db_ruh_add_support_type(
        hub_id = as.integer(input$add_hub_id),
        name = input$add_name,
        description = input$add_desc,
        user_id = dauPortalTools::get_user(session)
      )

      removeModal()
      data(db_ruh_get_support_types(hub_id = NULL))
      showNotification(
        "New support type created successfully.",
        type = "message"
      )
    })
  })
}
