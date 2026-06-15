server_school_page <- function(id, selected_urn) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    hubs_list_proxy <- DT::dataTableProxy("hubs_list", session)
    support_given_proxy <- DT::dataTableProxy("support_given", session)

    current_user <- "system_user"

    output$school_details <- renderUI({
      req(selected_urn())
      dauPortalTools::school_render_overview(
        urn = as.numeric(selected_urn()),
        id = paste0("school_overview_school_", selected_urn())
      )
    })

    output$hubs_list <- DT::renderDT(
      {
        req(selected_urn())

        db_ruh_get_support_schools() |>
          dplyr::filter(ruhs_urn == as.numeric(selected_urn())) |>
          dplyr::select(
            ruhs_id,
            ruhb_name,
            ruht_name,
            ruhs_active,
            ruhs_dateactive,
            ruhs_dateended
          ) |>
          dplyr::rename(
            "Support ID" = ruhs_id,
            "Associated Hub" = ruhb_name,
            "Framework Type" = ruht_name,
            "Support Active?" = ruhs_active,
            "Date Started" = ruhs_dateactive,
            "Date Ended" = ruhs_dateended
          )
      },
      selection = "single",
      callback = DT::JS(paste0(
        "
  table.on('dblclick', 'tr', function() {
    var data = table.row(this).data();
    if (data) { 
      Shiny.setInputValue('",
        ns("hub_support_dblclicked"),
        "', data[1], {priority: 'event'}); 
    }
  });
"
      ))
    )

    output$support_given <- DT::renderDT(
      {
        req(selected_urn())

        db_ruh_get_lead_schools() |>
          dplyr::filter(ruhl_urn == as.numeric(selected_urn())) |>
          dplyr::select(
            ruhl_id,
            ruhb_name,
            ruhl_urn,
            ruhl_active,
            ruhl_dateactive,
            ruhl_dateended
          ) |>
          dplyr::rename(
            "Lead ID" = ruhl_id,
            "Lead Hub Name" = ruhb_name,
            "Lead URN" = ruhl_urn,
            "Lead Active?" = ruhl_active,
            "Date Started" = ruhl_dateactive,
            "Date Ended" = ruhl_dateended
          )
      },
      selection = "single",
      callback = DT::JS(paste0(
        "
  table.on('dblclick', 'tr', function() {
    var data = table.row(this).data();
    if (data) { 
      Shiny.setInputValue('",
        ns("hub_lead_dblclicked"),
        "', data[1], {priority: 'event'}); 
    }
  });
"
      ))
    )

    observeEvent(input$new_support_record, {
      req(selected_urn())

      hubs_df <- db_ruh_get_hubs()
      hubs_choices <- setNames(hubs_df$ruhb_id, hubs_df$ruhb_name)

      showModal(modalDialog(
        title = "Log New Support Framework Assignment",
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("submit_support_record"),
            "Save Assignment",
            class = "btn-success"
          )
        ),
        tagList(
          selectInput(
            ns("modal_sup_hub_id"),
            "Select Associated Hub:",
            choices = hubs_choices
          ),
          selectInput(
            ns("modal_sup_framework_id"),
            "Select Support Framework Type:",
            choices = character(0)
          ),

          ui_date_input(
            ns("modal_sup_date_start"),
            "Date Support Started:",
            value = Sys.Date()
          ),

          textAreaInput(
            ns("modal_sup_comment"),
            "Comments / Initial Context:",
            rows = 3
          )
        )
      ))
    })

    observeEvent(input$modal_sup_hub_id, {
      req(input$modal_sup_hub_id)

      frameworks_df <- db_ruh_get_support_types(
        hub_id = as.integer(input$modal_sup_hub_id)
      )
      framework_choices <- setNames(
        frameworks_df$ruht_id,
        frameworks_df$ruht_name
      )

      updateSelectInput(
        session,
        "modal_sup_framework_id",
        choices = framework_choices
      )
    })

    observeEvent(input$submit_support_record, {
      req(input$modal_sup_hub_id, input$modal_sup_framework_id)
      req(
        input$modal_sup_date_start_day,
        input$modal_sup_date_start_month,
        input$modal_sup_date_start_year
      )

      clean_date <- input_to_date("modal_sup_date_start", input)

      if (is.na(clean_date)) {
        showNotification(
          "Invalid Date selection format. Please verify day/month values.",
          type = "error"
        )
        return()
      }

      removeModal()

      db_ruh_add_blank_support_school(
        hub_id = as.integer(input$modal_sup_hub_id),
        ruht_id = as.integer(input$modal_sup_framework_id),
        urn = as.numeric(selected_urn()),
        user_id = current_user,
        date_start = format(clean_date, "%Y-%m-%d"),
        comment = input$modal_sup_comment
      )

      DT::replaceData(
        hubs_list_proxy,
        db_ruh_get_support_schools() |>
          dplyr::filter(ruhs_urn == as.numeric(selected_urn())) |>
          dplyr::select(
            ruhs_id,
            ruhb_name,
            ruht_name,
            ruhs_active,
            ruhs_dateactive,
            ruhs_dateended
          ) |>
          dplyr::rename(
            "Support ID" = ruhs_id,
            "Associated Hub" = ruhb_name,
            "Framework Type" = ruht_name,
            "Support Active?" = ruhs_active,
            "Date Started" = ruhs_dateactive,
            "Date Ended" = ruhs_dateended
          ),
        resetPaging = FALSE
      )
      showNotification(
        "Support tracking assignment successfully registered!",
        type = "message"
      )
    })

    observeEvent(input$new_lead_record, {
      req(selected_urn())

      hubs_df <- db_ruh_get_hubs()
      hubs_choices <- setNames(hubs_df$ruhb_id, hubs_df$ruhb_name)

      showModal(modalDialog(
        title = "Register New Lead Hub Status",
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("submit_lead_record"),
            "Register Lead Status",
            class = "btn-primary"
          )
        ),
        tagList(
          selectInput(
            ns("modal_lead_hub_id"),
            "Select Associated Hub Framework:",
            choices = hubs_choices
          ),

          ui_date_input(
            ns("modal_lead_date_start"),
            "Date Status Active From:",
            value = Sys.Date()
          ),

          textAreaInput(
            ns("modal_lead_comment"),
            "Comments / Designation Notes:",
            rows = 3
          )
        )
      ))
    })

    observeEvent(input$submit_lead_record, {
      req(input$modal_lead_hub_id)
      req(
        input$modal_lead_date_start_day,
        input$modal_lead_date_start_month,
        input$modal_lead_date_start_year
      )

      clean_date <- input_to_date("modal_lead_date_start", input)

      if (is.na(clean_date)) {
        showNotification(
          "Invalid Date selection format. Please verify day/month values.",
          type = "error"
        )
        return()
      }

      removeModal()

      db_ruh_add_blank_lead_school(
        hub_id = as.integer(input$modal_lead_hub_id),
        urn = as.numeric(selected_urn()),
        user_id = current_user,
        date_start = format(clean_date, "%Y-%m-%d"),
        comment = input$modal_lead_comment
      )

      DT::replaceData(
        support_given_proxy,
        db_ruh_get_lead_schools() |>
          dplyr::filter(ruhl_urn == as.numeric(selected_urn())) |>
          dplyr::select(
            ruhl_id,
            ruhb_name,
            ruhl_urn,
            ruhl_active,
            ruhl_dateactive,
            ruhl_dateended
          ) |>
          dplyr::rename(
            "Lead ID" = ruhl_id,
            "Lead Hub Name" = ruhb_name,
            "Lead URN" = ruhl_urn,
            "Lead Active?" = ruhl_active,
            "Date Started" = ruhl_dateactive,
            "Date Ended" = ruhl_dateended
          ),
        resetPaging = FALSE
      )
      showNotification(
        "Lead Hub Designation Status recorded!",
        type = "message"
      )
    })
  })
}
