server_user_menu <- function(
  id,
  user,
  user_role,
  sigchange_data,
  sigchange_types,
  giaschange_types,
  selected_sigchange,
  selected_urn,
  original_record
) {
  shiny::moduleServer(id, function(input, output, session) {
    # ----------------------------
    # Profile
    # ----------------------------
    output$profile_user <- shiny::renderText({
      user()
    })

    output$profile_role <- shiny::renderText({
      user_role()
    })

    # ----------------------------
    # Stats
    # ----------------------------
    output$user_live_records <- shiny::renderText({
      shiny::req(sigchange_data())

      dat <- sigchange_data()
      u <- user()

      if (!is.null(u)) {
        dat <- dplyr::filter(
          dat,
          delivery_lead == u | RSCContact == u
        )
      }

      n <- dplyr::filter(dat, all_actions_completed != 1) |> nrow()

      format(n, big.mark = ",")
    })

    output$user_monthly_updates <- shiny::renderText({
      shiny::req(sigchange_data())

      dat <- sigchange_data()
      u <- user()

      if (!is.null(u)) {
        dat <- dplyr::filter(dat, user_name_edit_change == u)
      }

      cutoff <- Sys.Date() - 30

      n <- dat |>
        dplyr::filter(
          !is.na(change_edit_date),
          as.Date(change_edit_date) >= cutoff
        ) |>
        nrow()

      format(n, big.mark = ",")
    })

    output$user_quality_issues <- shiny::renderText({
      shiny::req(sigchange_data())

      dat <- sigchange_data()
      u <- user()

      if (!is.null(u)) {
        dat <- dplyr::filter(
          dat,
          delivery_lead == u | RSCContact == u
        )
      }

      n <- dat |>
        dplyr::filter(!isTRUE(coerce_to_bool(all_actions_completed))) |>
        nrow()

      format(n, big.mark = ",")
    })
    # ----------------------------
    # Assigned Work Table
    # ----------------------------
    output$assigned_tbl <- DT::renderDataTable({
      shiny::req(
        sigchange_data(),
        sigchange_types(),
        giaschange_types()
      )

      dat <- sigchange_data()
      u <- user()

      if (!is.null(u) && !identical(toupper(u), "ADMIN")) {
        u_clean <- toupper(u)

        dat <- dplyr::filter(
          dat,
          toupper(delivery_lead) == u_clean |
            toupper(RSCContact) == u_clean
        )
      }

      dat <- dat |>
        dplyr::left_join(
          sigchange_types() |>
            dplyr::select(
              type_of_sig_change_id,
              type_of_sig_change
            ),
          by = "type_of_sig_change_id"
        ) |>
        dplyr::left_join(
          giaschange_types() |>
            dplyr::select(
              type_of_gias_change_id,
              type_of_gias_change
            ),
          by = "type_of_gias_change_id"
        )

      ns <- session$ns

      DT::datatable(
        dat,
        selection = "none",
        callback = DT::JS(paste0(
          "
    table.on('dblclick', 'tr', function() {
      var data = table.row(this).data();
      if (data) {
        Shiny.setInputValue('",
          ns("assigned_dblclick"),
          "', data[1], {priority: 'event'});
      }
    });
  "
        ))
      )
    })

    observeEvent(input$assigned_dblclick, {
      log_event(paste("User double clicked:", input$assigned_dblclick))

      sig_id <- input$assigned_dblclick
      log_event(paste(sig_id))

      req(!is.na(sig_id))
      log_event("1")
      sc <- sigchange_data() |>
        dplyr::filter(sig_change_id == sig_id)
      log_event("2")

      req(nrow(sc) == 1)
      log_event("3")

      row <- sc[1, , drop = FALSE]
      log_event("4")

      selected_sigchange(as.list(row))
      original_record(as.list(row))
      selected_urn(as.integer(row$URN))
      log_event("5")

      bslib::nav_select(
        id = "main_navbar",
        selected = "sigchange",
        session = session$rootScope()
      )
    })

    output$quality_tbl <- DT::renderDataTable({
      data.frame()
    })
  })
}
