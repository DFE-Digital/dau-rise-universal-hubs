server <- function(input, output, session) {
  # ------------------------------------------------------------------
  # Session identity
  # ------------------------------------------------------------------
  user <- reactiveVal(NULL)
  user_role <- reactiveVal(NULL)

  observeEvent(
    TRUE,
    {
      u <- dauPortalTools::get_user(session)
      user(u)

      r <- tryCatch(
        dauPortalTools::get_user_role(u),
        error = function(e) NULL
      )
      user_role(r)

      dauPortalTools::db_record_login(u)
    },
    once = TRUE
  )

  # ------------------------------------------------------------------
  # Navigation menu
  # ------------------------------------------------------------------
  observeEvent(
    user_role(),
    {
      req(user_role())
      cleaned_role <- tolower(trimws(user_role()))

      menu <- build_admin_menu(cleaned_role)
      req(menu)

      insertTab(
        inputId = "main_navbar",
        tab = menu,
        position = "after",
        select = FALSE
      )

      if (identical(cleaned_role, "admin")) {
        server_rise_support_types_admin("rise_support_types")
        server_rise_actions_admin("rise_actions")
        dauPortalTools::server_portal_user_admin("admin")
      } else if (identical(cleaned_role, "regional_admin")) {
        server_quality_wrapper("quality", app_id = utils_get_app_id())
      }
    },
    once = TRUE
  )
  # ------------------------------------------------------------------
  # Home
  # ------------------------------------------------------------------
  output$welcome_user <- renderText({
    paste0("Welcome, ", user())
  })
  output$profile_user <- renderText(user())
  output$profile_role <- renderText(user_role())

  output$news <- renderUI({
    ui_show_news()
  })

  output$summary_metrics <- renderUI({
    dauPortalTools::sc_render_summary()
  })

  # ------------------------------------------------------------------
  # Search page
  # ------------------------------------------------------------------
  output$filters <- renderUI({
    tagList(
      textInput("filter_urn", "URN"),
      textInput("filter_schoolname", "School name"),
      textInput("filter_trustid", "Trust ID"),
      textInput("filter_trustname", "Trust name"),
      selectInput(
        "filter_has_support",
        "Receiving Hub Support",
        choices = c("All", "Yes Only", "No Only"),
        selected = "All"
      ),
      selectInput(
        "filter_is_lead",
        "Is a Lead Hub School",
        choices = c("All", "Yes Only", "No Only"),
        selected = "All"
      )
    )
  })

  schools_data <- reactiveVal(NULL)
  hub_support_urns <- reactiveVal(NULL)
  hub_lead_urns <- reactiveVal(NULL)

  refresh_schools <- function() {
    schools_data(db_get_school_list())
  }

  refresh_hub_statuses <- function() {
    hub_support_urns(db_get_hub_support_urns())
    hub_lead_urns(db_get_hub_lead_urns())
  }

  observeEvent(
    TRUE,
    {
      refresh_schools()
      refresh_hub_statuses()
    },
    once = TRUE
  )

  filtered_schools <- reactive({
    req(schools_data(), hub_support_urns(), hub_lead_urns())

    schools <- schools_data()
    support_list <- hub_support_urns()$URN
    lead_list <- hub_lead_urns()$URN

    schools$HasHubSupport <- ifelse(schools$URN %in% support_list, "Yes", "No")
    schools$IsLeadSchool <- ifelse(schools$URN %in% lead_list, "Yes", "No")

    if (!is.null(input$filter_urn) && nzchar(input$filter_urn)) {
      schools <- dplyr::filter(schools, grepl(input$filter_urn, URN))
    }
    if (!is.null(input$filter_schoolname) && nzchar(input$filter_schoolname)) {
      schools <- dplyr::filter(
        schools,
        grepl(input$filter_schoolname, schoolname, ignore.case = TRUE)
      )
    }
    if (!is.null(input$filter_trustid) && nzchar(input$filter_trustid)) {
      schools <- dplyr::filter(schools, grepl(input$filter_trustid, trustid))
    }
    if (!is.null(input$filter_trustname) && nzchar(input$filter_trustname)) {
      schools <- dplyr::filter(
        schools,
        grepl(input$filter_trustname, trustname, ignore.case = TRUE)
      )
    }

    if (
      !is.null(input$filter_has_support) &&
        !identical(input$filter_has_support, "All")
    ) {
      target_val <- ifelse(input$filter_has_support == "Yes Only", "Yes", "No")
      schools <- dplyr::filter(schools, HasHubSupport == target_val)
    }
    if (
      !is.null(input$filter_is_lead) && !identical(input$filter_is_lead, "All")
    ) {
      target_val <- ifelse(input$filter_is_lead == "Yes Only", "Yes", "No")
      schools <- dplyr::filter(schools, IsLeadSchool == target_val)
    }

    schools
  })

  output$school_table <- DT::renderDataTable(
    {
      filtered_schools() |>
        dplyr::select(
          URN,
          schoolname,
          schooltype,
          phase,
          gor,
          trustname,
          HasHubSupport,
          IsLeadSchool
        ) |>
        dplyr::rename(
          `School Name` = schoolname,
          `School Type` = schooltype,
          Phase = phase,
          Region = gor,
          `Trust Name` = trustname,
          `Receives Support` = HasHubSupport,
          `Is Lead School` = IsLeadSchool
        )
    },
    callback = DT::JS(
      "
    table.on('dblclick', 'tr', function() {
      var data = table.row(this).data();
      if (data) {
        Shiny.setInputValue('school_dblclicked', data[1], {priority: 'event'});
      }
    });
    "
    )
  )

  # ------------------------------------------------------------------
  # Shared state
  # ------------------------------------------------------------------
  selected_urn <- reactiveVal(NULL)
  selected_sigchange <- reactiveVal(NULL)
  original_record <- reactiveVal(NULL)

  # ------------------------------------------------------------------
  # School navigation
  # ------------------------------------------------------------------
  observeEvent(input$school_dblclicked, {
    urn <- suppressWarnings(as.integer(input$school_dblclicked))
    req(!is.na(urn))
    selected_urn(urn)
    updateNavbarPage(session, "main_navbar", selected = "school_overview")
  })

  server_school_page(
    id = "schoolpage",
    selected_urn = selected_urn
  )

  # ------------------------------------------------------------------
  # Deep link (?scid=) - work needed
  # ------------------------------------------------------------------
  observeEvent(
    session$clientData$url_search,
    {
      query <- parseQueryString(session$clientData$url_search)
      req(query$scid, sigchange_data())

      sig_id <- suppressWarnings(as.integer(query$scid))
      req(!is.na(sig_id))

      sc <- dplyr::filter(sigchange_data(), sig_change_id == sig_id)
      req(nrow(sc) == 1)

      row <- sc[1, , drop = FALSE]

      selected_sigchange(as.list(row))
      original_record(as.list(row))
      selected_urn(as.integer(sc$URN))

      updateNavbarPage(session, "main_navbar", selected = "sigchange")
    }
  )

  selected_support_id <- reactiveVal(NULL)
  selected_lead_id <- reactiveVal(NULL)

  server_hub_support_page(
    "hub_support_page",
    selected_support_id = selected_support_id
  )
  server_hub_lead_page("hub_lead_page", selected_lead_id = selected_lead_id)

  observeEvent(input$`schoolpage-hub_support_dblclicked`, {
    id_val <- input$`schoolpage-hub_support_dblclicked`
    req(id_val)
    print(paste("🎯 Routing to Support Page with ID:", id_val))

    selected_support_id(as.integer(id_val))
    bslib::nav_select("main_navbar", "hub_support_page")
  })

  observeEvent(input$`schoolpage-hub_lead_dblclicked`, {
    id_val <- input$`schoolpage-hub_lead_dblclicked`
    req(id_val)
    print(paste("🎯 Routing to Lead Page with ID:", id_val))

    selected_lead_id(as.integer(id_val))
    bslib::nav_select("main_navbar", "hub_lead_page")
  })

  observeEvent(selected_support_id(), {
    if (
      is.null(selected_support_id()) && input$main_navbar == "hub_support_page"
    ) {
      updateNavbarPage(session, "main_navbar", selected = "school_overview")
    }
  })

  observeEvent(selected_lead_id(), {
    if (is.null(selected_lead_id()) && input$main_navbar == "hub_lead_page") {
      updateNavbarPage(session, "main_navbar", selected = "school_overview")
    }
  })

  observeEvent(input$school_table_rows_selected, {
    req(input$school_table_rows_selected)

    row_idx <- input$school_table_rows_selected
    urn_val <- filtered_schools()[row_idx, "URN"]

    selected_urn(urn_val)
    updateNavbarPage(session, "main_navbar", selected = "school_overview")
  })

  # ------------------------------------------------------------------
  # Hubs Search Page
  # ------------------------------------------------------------------

  refresh_hubs <- reactiveVal(0)
  selected_hub <- reactiveVal(0)

  filtered_hubs <- reactive({
    refresh_hubs()

    df <- db_ruh_get_hub_summary()

    if (nzchar(input$filter_hub_name %||% "")) {
      df <- df |>
        dplyr::filter(
          grepl(input$filter_hub_name, hub_name, ignore.case = TRUE)
        )
    }
    df
  })

  output$hub_table <- DT::renderDataTable({
    dat <- filtered_hubs()

    DT::datatable(
      dat,
      colnames = c(
        "ID",
        "Hub Name",
        "Active Support Schools",
        "Active Lead Schools",
        "Support Types"
      ),
      rownames = FALSE,
      selection = "none",
      options = list(
        pageLength = 25,
        order = list(list(1, "asc")),
        columnDefs = list(
          list(visible = FALSE, targets = 0)
        )
      ),
      callback = DT::JS(
        "
      table.on('dblclick', 'tr', function() {
        var data = table.row(this).data();
        if (data) {
          Shiny.setInputValue(
            'hub_dblclicked', 
            data[0], 
            {priority: 'event'}
          );
        }
      });
      "
      )
    )
  })

  observeEvent(input$new_hub_record, {
    showModal(modalDialog(
      title = "Add New RISE Hub",
      textInput(
        "new_hub_name_input",
        "Enter Hub Name",
        placeholder = "e.g., North West Regional Hub"
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_new_hub", "Save Hub", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$save_new_hub, {
    req(input$new_hub_name_input)

    new_id <- tryCatch(
      {
        db_ruh_add_hub(
          hub_name = input$new_hub_name_input,
          user_id = dauPortalTools::get_user(session)
        )
      },
      error = function(e) {
        message("DB Error: ", e$message)
        NULL
      }
    )

    if (!is.null(new_id)) {
      removeModal()
      refresh_hubs(refresh_hubs() + 1)
      showNotification(
        glue::glue(
          "Successfully created '{input$new_hub_name_input}' (ID: {new_id})"
        ),
        type = "message"
      )
    } else {
      showNotification(
        "Failed to create Hub. Please check if the name is unique and try again.",
        type = "error"
      )
    }
  })

  # ------------------------------------------------------------------
  # Hub Overview
  # ------------------------------------------------------------------

  observeEvent(input$hub_dblclicked, {
    hub_id_val <- suppressWarnings(as.integer(input$hub_dblclicked))

    req(!is.na(hub_id_val))

    selected_hub(hub_id_val)

    bslib::nav_select("main_navbar", "hub_overview")
  })

  server_hub_overview("hub_overview_module", selected_hub, selected_urn)

  observeEvent(input[["hub_overview_module-hub_school_dblclicked"]], {
    bslib::nav_select("main_navbar", "school_overview")
  })
  # ------------------------------------------------------------------
  # User Profile
  # ------------------------------------------------------------------

  server_user_menu(
    "user_menu",
    user = user,
    user_role = user_role,
    sigchange_data = sigchange_data,
    sigchange_types = sigchange_types,
    giaschange_types = giaschange_types,
    selected_sigchange = selected_sigchange,
    selected_urn = selected_urn,
    original_record = original_record
  )

  # ------------------------------------------------------------------
  # Admin / Quality
  # ------------------------------------------------------------------
  observeEvent(
    TRUE,
    {
      dauPortalTools::server_portal_user_admin("admin")
    },
    once = TRUE
  )

  server_quality_wrapper("quality", app_id = utils_get_app_id())
}
