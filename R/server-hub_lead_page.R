server_hub_lead_page <- function(id, selected_lead_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

    observeEvent(selected_lead_id(), {
      req(selected_lead_id())

      rec <- db_ruh_get_lead_schools(ruhl_id = selected_lead_id())
      if (nrow(rec) > 0) {
        updateSelectInput(session, "hub_id", selected = rec$ruhb_id)
        updateTextAreaInput(session, "comment", value = rec$ruhl_comment)

        if (!is.null(rec$ruhl_dateactive) && !is.na(rec$ruhl_dateactive)) {
          d_act <- to_date(rec$ruhl_dateactive)
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

        if (!is.null(rec$ruhl_dateended) && !is.na(rec$ruhl_dateended)) {
          d_end <- to_date(rec$ruhl_dateended)
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

    observeEvent(input$save_lead, {
      req(selected_lead_id(), input$hub_id)
      req(
        input$date_active_day,
        input$date_active_month,
        input$date_active_year
      )

      clean_start <- input_to_date("date_active", input)
      clean_ended <- input_to_date("date_ended", input)

      if (is.na(clean_start)) {
        showNotification(
          "A valid Status Commencement Date is required.",
          type = "error"
        )
        return()
      }

      db_ruh_update_lead_school(
        ruhl_id = selected_lead_id(),
        hub_id = as.integer(input$hub_id),
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
        "Lead designation status updated successfully.",
        type = "message"
      )
    })
  })
}
