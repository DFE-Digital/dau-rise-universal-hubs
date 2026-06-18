server_hub_support_page <- function(id, selected_support_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    actions_data <- reactiveVal(NULL)

    target_ruht_id <- reactiveVal(NULL)

    hubs_lookup <- reactive({
      db_ruh_get_hubs()
    })

    observeEvent(hubs_lookup(), {
      updateSelectInput(
        session,
        "hub_id",
        choices = setNames(hubs_lookup()$ruhb_id, hubs_lookup()$ruhb_name)
      )
    })

    observeEvent(input$hub_id, {
      req(input$hub_id)

      types_df <- db_ruh_get_support_types(hub_id = as.integer(input$hub_id))

      updateSelectInput(
        session,
        "ruht_id",
        choices = setNames(types_df$ruht_id, types_df$ruht_name)
      )
    })

    observe({
      req(target_ruht_id(), input$hub_id)

      updateSelectInput(session, "ruht_id", selected = target_ruht_id())

      target_ruht_id(NULL)
    })

    observeEvent(selected_support_id(), {
      req(selected_support_id())

      actions_data(db_ruh_get_support_school_actions(
        ruhs_id = selected_support_id()
      ))

      rec <- db_ruh_get_support_schools(ruhs_id = selected_support_id())
      if (nrow(rec) > 0) {
        target_ruht_id(rec$ruht_id)

        updateSelectInput(session, "hub_id", selected = rec$ruhb_id)
        updateTextAreaInput(session, "comment", value = rec$ruhs_comment)

        if (!is.null(rec$ruhs_dateactive) && !is.na(rec$ruhs_dateactive)) {
          d_act <- to_date(rec$ruhs_dateactive)
          updateSelectInput(
            session,
            "date_active_day",
            selected = format(d_act, "%d")
          )
          updateSelectInput(
            session,
            "date_active_month",
            selected = format(d_act, "%b")
          )
          updateSelectInput(
            session,
            "date_active_year",
            selected = format(d_act, "%Y")
          )
        }

        if (!is.null(rec$ruhs_dateended) && !is.na(rec$ruhs_dateended)) {
          d_end <- to_date(rec$ruhs_dateended)
          updateSelectInput(
            session,
            "date_ended_day",
            selected = format(d_end, "%d")
          )
          updateSelectInput(
            session,
            "date_ended_month",
            selected = format(d_end, "%b")
          )
          updateSelectInput(
            session,
            "date_ended_year",
            selected = format(d_end, "%Y")
          )
        } else {
          updateSelectInput(session, "date_ended_day", selected = "")
          updateSelectInput(session, "date_ended_month", selected = "")
          updateSelectInput(session, "date_ended_year", selected = "")
        }
      }
    })

    observeEvent(input$back_to_school, {
      updateNavbarPage(session, "main_navbar", selected = "school_overview")
    })

    output$actions_table <- DT::renderDT({
      req(actions_data())

      DT::datatable(
        actions_data() |> dplyr::select(ruhsa_date, ruha_name, ruhsa_comment),
        colnames = c("Date", "Action / Milestone Type", "Notes & Outcomes"),
        selection = "single",
        rownames = FALSE,
        options = list(pageLength = 5, dom = 'tp')
      )
    })

    observeEvent(input$add_action, {
      req(selected_support_id())

      active_hub_id <- as.integer(input$hub_id)
      active_ruht_id <- as.integer(input$ruht_id)

      catalog <- db_ruh_get_actions()

      filtered_catalog <- catalog |>
        dplyr::filter(
          (ruhb_id == active_hub_id & ruht_id == active_ruht_id) |
            (ruhb_id == 0 & ruht_id == 0) |
            (ruhb_id == active_hub_id & ruht_id == 0) |
            (ruhb_id == 0 & ruht_id == active_ruht_id)
        )

      choices_list <- if (nrow(filtered_catalog) > 0) {
        setNames(filtered_catalog$ruha_id, filtered_catalog$ruha_name)
      } else {
        c("No actions match this context" = "")
      }

      showModal(modalDialog(
        title = "Log New Intervention",
        tagList(
          selectInput(
            ns("new_ruha_id"),
            "Action Type",
            choices = choices_list
          ),
          ui_date_input(ns("new_date"), "Date of Action", value = Sys.Date()),
          textAreaInput(ns("new_comment"), "Notes & Outcomes", rows = 4)
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("save_new_action"),
            "Save to Log",
            class = "btn-success"
          )
        )
      ))
    })

    observeEvent(input$save_new_action, {
      req(input$new_ruha_id, input$new_ruha_id != "")
      req(input$new_date_day, input$new_date_month, input$new_date_year)

      clean_action_date <- input_to_date("new_date", input)
      if (is.na(clean_action_date)) {
        showNotification(
          "Please supply a valid Action Date setting.",
          type = "error"
        )
        return()
      }

      db_ruh_add_support_school_action(
        ruhs_id = selected_support_id(),
        ruha_id = input$new_ruha_id,
        action_date = format(clean_action_date, "%Y-%m-%d"),
        comment = input$new_comment,
        user_id = dauPortalTools::get_user(session)
      )

      removeModal()
      actions_data(db_ruh_get_support_school_actions(
        ruhs_id = selected_support_id()
      ))
    })

    observeEvent(input$save_support, {
      req(selected_support_id(), input$hub_id, input$ruht_id)
      req(
        input$date_active_day,
        input$date_active_month,
        input$date_active_year
      )

      clean_start <- input_to_date("date_active", input)
      clean_ended <- input_to_date("date_ended", input)

      if (is.na(clean_start)) {
        showNotification(
          "A valid Framework Start Date is required.",
          type = "error"
        )
        return()
      }

      db_ruh_update_support_school(
        ruhs_id = selected_support_id(),
        hub_id = as.integer(input$hub_id),
        ruht_id = as.integer(input$ruht_id),
        lead_school_id = NULL,
        date_active = format(clean_start, "%Y-%m-%d"),
        date_ended = if (is.na(clean_ended)) {
          NULL
        } else {
          format(clean_ended, "%Y-%m-%d")
        },
        is_active = if (is.na(clean_ended)) 1 else 0,
        comment = input$comment,
        user_id = dauPortalTools::get_user(session)
      )

      showNotification(
        "Framework status updated successfully.",
        type = "message"
      )
    })
  })
}
