server <- function(input, output, session) {
  # ------------------------------------------------------------------
  # Session Identity & Security
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
  # Dynamic Navigation Menu
  # ------------------------------------------------------------------
  output$admin_side_nav_container <- renderUI({
    req(user_role())
    role <- tolower(trimws(user_role()))

    side_nav_styles <- tags$style(HTML(
      "
      .gds-side-nav-item {
        padding: 12px 15px !important;
        border-bottom: 1px solid #b1b4b6 !important;
        list-style-type: none !important;
        margin: 0 !important;
      }
      .gds-side-nav-link {
        font-weight: bold !important;
        text-decoration: none !important;
        color: #1d70b8 !important;
        display: block !important;
        width: 100% !important;
      }
      .gds-side-nav-link:hover {
        color: #003078 !important;
        text-decoration: underline !important;
      }
    "
    ))

    dashboard_home_link <- tags$li(
      class = "gds-side-nav-item",
      style = "border-bottom: 3px solid #1d70b8 !important; margin-bottom: 15px !important; background: #ffffff;",
      shiny::actionLink(
        "side_nav_console_home",
        "💻 Console Overview",
        class = "gds-side-nav-link",
        style = "color: #1d70b8 !important;"
      )
    )

    menu_list <- if (role == "admin") {
      tags$ul(
        style = "padding-left: 0; margin: 0; list-style-type: none;",
        dashboard_home_link,
        tags$li(
          class = "gds-side-nav-item",
          shiny::actionLink(
            "side_nav_sup_types",
            "📁 RISE Support Types",
            class = "gds-side-nav-link"
          )
        ),
        tags$li(
          class = "gds-side-nav-item",
          shiny::actionLink(
            "side_nav_actions",
            "⚙️ RISE Action Catalog",
            class = "gds-side-nav-link"
          )
        )
      )
    } else if (role == "regional_admin") {
      tags$ul(
        style = "padding-left: 0; margin: 0; list-style-type: none;",
        dashboard_home_link,
        tags$li(
          class = "gds-side-nav-item",
          shiny::actionLink(
            "side_nav_quality",
            "⚠️ Regional Quality Issues",
            class = "gds-side-nav-link"
          )
        )
      )
    } else {
      NULL
    }

    tagList(side_nav_styles, menu_list)
  })
  # ------------------------------------------------------------------
  # Service Navigation Link Interceptors
  # ------------------------------------------------------------------
  output$dynamic_gds_service_navigation <- renderUI({
    req(user_role())

    nav_links <- build_service_nav_links(user_role())

    shinyGovstyle::service_navigation(
      service_name = "",
      links = nav_links
    )
  })

  observeEvent(input$home, {
    bslib::nav_select("main_navbar", "home")
  })

  observeEvent(input$search, {
    bslib::nav_select("main_navbar", "search")
  })

  observeEvent(input$hubs_search, {
    bslib::nav_select("main_navbar", "hubs_search")
  })

  observeEvent(input$events_master_catalog, {
    bslib::nav_select("main_navbar", "events_master_catalog")
  })

  observeEvent(input$support, {
    bslib::nav_select("main_navbar", "support")
  })

  observeEvent(input$user_menu, {
    bslib::nav_select("main_navbar", "user_menu")
  })

  observeEvent(input$admin_dashboard, {
    bslib::nav_select("main_navbar", "admin_dashboard")
    bslib::nav_select("admin_sub_pages", "sub_admin_landing")
  })

  observeEvent(input$user_menu, {
    bslib::nav_select("main_navbar", "user_menu")
    bslib::nav_select("user_menu-user_sub_pages", "user_sub_profile")
  })

  observeEvent(input$side_nav_console_home, {
    bslib::nav_select("admin_sub_pages", "sub_admin_landing")
  })

  observeEvent(input$side_nav_sup_types, {
    print("---> Side Nav Clicked: Support Types")
    bslib::nav_select("admin_sub_pages", "sub_support_types")
  })

  observeEvent(input$side_nav_actions, {
    print("---> Side Nav Clicked: Action Catalog")
    bslib::nav_select("admin_sub_pages", "sub_action_catalog")
  })

  observeEvent(input$side_nav_quality, {
    print("---> Side Nav Clicked: Regional Quality")
    bslib::nav_select("admin_sub_pages", "sub_regional_quality")
  })

  # ------------------------------------------------------------------
  # Home / Landing Views
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
    dauPortalTools::ru_render_summary()
  })

  # ------------------------------------------------------------------
  # Shared Polymorphic Universal State Tracker
  # ------------------------------------------------------------------
  active_target <- reactiveValues(
    id = NULL,
    type = "School",
    name = NULL
  )

  selected_sigchange <- reactiveVal(NULL)
  original_record <- reactiveVal(NULL)
  selected_support_id <- reactiveVal(NULL)
  selected_lead_id <- reactiveVal(NULL)

  # ------------------------------------------------------------------
  # Universal Metadata-Driven Search Engine
  # ------------------------------------------------------------------
  output$filters <- renderUI({
    tagList(
      radioButtons(
        "filter_entity_type",
        "Administrative Tracking Layer:",
        choices = c(
          "Schools" = "School",
          "Multi-Academy Trusts" = "Trust",
          "Local Authorities" = "LA",
          "Dioceses" = "Diocese"
        ),
        selected = "School",
        inline = TRUE
      ),
      hr(),
      textInput("filter_search_id", "Search Unique ID Value (URN/Code):"),
      textInput("filter_search_name", "Search Entity Title Name:"),
      selectInput(
        "filter_has_support",
        "Receiving Ongoing Provision Framework Support",
        choices = c("All", "Yes Only", "No Only"),
        selected = "All"
      )
    )
  })

  raw_entities_list <- reactiveVal(NULL)
  engaged_provision_ids <- reactiveVal(NULL)

  observeEvent(input$filter_entity_type, {
    req(input$filter_entity_type)
    raw_entities_list(dauPortalTools::db_get_search_entities(
      input$filter_entity_type
    ))
    engaged_df <- dauPortalTools::db_get_engaged_entity_ids(
      input$filter_entity_type
    )
    engaged_provision_ids(
      if (nrow(engaged_df) > 0) engaged_df$ID else integer(0)
    )
  })

  filtered_entities_data <- reactive({
    req(raw_entities_list(), !is.null(engaged_provision_ids()))
    df <- raw_entities_list()
    engaged_list <- engaged_provision_ids()

    id_header <- dauPortalTools::db_resolve_entity_key_label(
      input$filter_entity_type
    )
    name_header <- names(df)[2]

    df$`Receives Support` <- ifelse(
      df[[id_header]] %in% engaged_list,
      "Yes",
      "No"
    )

    if (nzchar(input$filter_search_id %||% "")) {
      df <- dplyr::filter(
        df,
        grepl(input$filter_search_id, .data[[id_header]], ignore.case = TRUE)
      )
    }
    if (nzchar(input$filter_search_name %||% "")) {
      df <- dplyr::filter(
        df,
        grepl(
          input$filter_search_name,
          .data[[name_header]],
          ignore.case = TRUE
        )
      )
    }
    if (
      nzchar(input$filter_has_support %||% "") &&
        !identical(input$filter_has_support, "All")
    ) {
      target_val <- ifelse(input$filter_has_support == "Yes Only", "Yes", "No")
      df <- dplyr::filter(df, `Receives Support` == target_val)
    }

    df
  })

  output$school_table <- DT::renderDataTable({
    req(filtered_entities_data())

    DT::datatable(
      filtered_entities_data(),
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 15, dom = "tp"),
      callback = DT::JS(
        "
        table.on('dblclick', 'tr', function() {
          var data = table.row(this).data();
          if (data) {
            Shiny.setInputValue('entity_row_dblclicked', data, {priority: 'event'});
          }
        });
        "
      )
    )
  })

  # ------------------------------------------------------------------
  # Session Profile Context
  # ------------------------------------------------------------------
  observeEvent(input$school_table_rows_selected, {
    req(input$school_table_rows_selected)
    row_idx <- input$school_table_rows_selected
    df <- filtered_entities_data()
    selected_row <- df[row_idx, ]

    id_header <- dauPortalTools::db_resolve_entity_key_label(
      input$filter_entity_type
    )
    name_header <- names(df)[2]

    if (identical(tolower(input$filter_entity_type), "school")) {
      active_target$id <- as.integer(selected_row[[id_header]])
    } else {
      active_target$id <- as.character(selected_row[[id_header]])
    }

    active_target$type <- input$filter_entity_type
    active_target$name <- as.character(selected_row[[name_header]])

    updateNavbarPage(session, "main_navbar", selected = "school_overview")
  })

  observeEvent(input$entity_row_dblclicked, {
    req(input$entity_row_dblclicked)
    row_data <- input$entity_row_dblclicked

    if (identical(tolower(input$filter_entity_type), "school")) {
      active_target$id <- as.integer(row_data[1])
    } else {
      active_target$id <- as.character(row_data[1])
    }

    active_target$type <- input$filter_entity_type
    active_target$name <- as.character(row_data[2])

    updateNavbarPage(session, "main_navbar", selected = "school_overview")
  })

  # ------------------------------------------------------------------
  # Polymorphic Overview & Sub-Page Submodules Wiring
  # ------------------------------------------------------------------
  support_page_server <- server_hub_support_page(
    "hub_support_page",
    selected_support_id = selected_support_id,
    active_target = active_target
  )

  observeEvent(support_page_server$go_back(), {
    req(support_page_server$go_back() > 0)
    updateNavbarPage(session, "main_navbar", selected = "school_overview")
  })

  lead_page_server <- server_hub_lead_page(
    "hub_lead_page",
    selected_lead_id = selected_lead_id,
    active_target = active_target
  )

  observeEvent(lead_page_server$go_back(), {
    req(lead_page_server$go_back() > 0)
    updateNavbarPage(session, "main_navbar", selected = "school_overview")
  })

  observeEvent(input$`schoolpage-hub_support_dblclicked`, {
    id_val <- input$`schoolpage-hub_support_dblclicked`
    req(id_val)
    selected_support_id(as.integer(id_val))
    bslib::nav_select("main_navbar", "hub_support_page")
  })

  observeEvent(input$`schoolpage-hub_lead_dblclicked`, {
    id_val <- input$`schoolpage-hub_lead_dblclicked`
    req(id_val)
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

  # ------------------------------------------------------------------
  # Deep Link Parser Engine (?scid= Workflow Integration)
  # ------------------------------------------------------------------
  observeEvent(session$clientData$url_search, {
    query <- parseQueryString(session$clientData$url_search)
    req(query$scid, sigchange_data())

    sig_id <- suppressWarnings(as.integer(query$scid))
    req(!is.na(sig_id))

    sc <- dplyr::filter(sigchange_data(), sig_change_id == sig_id)
    req(nrow(sc) == 1)

    row <- sc[1, , drop = FALSE]

    selected_sigchange(as.list(row))
    original_record(as.list(row))

    active_target$id <- as.integer(sc$URN)
    active_target$type <- "School"
    active_target$name <- as.character(sc$schoolname)

    updateNavbarPage(session, "main_navbar", selected = "sigchange")
  })

  # ------------------------------------------------------------------
  # Hub Master Administration Sub-system
  # ------------------------------------------------------------------
  refresh_hubs <- reactiveVal(0)
  selected_hub <- reactiveVal(0)

  filtered_hubs <- reactive({
    refresh_hubs()
    df <- dauPortalTools::db_ruh_get_hub_summary()

    if (nzchar(input$filter_hub_name %||% "")) {
      df <- df |>
        dplyr::filter(grepl(
          input$filter_hub_name,
          hub_name,
          ignore.case = TRUE
        ))
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
        "Active Schools Tracked",
        "Active Trusts Tracked",
        "Active LAs Tracked",
        "Active Lead Centers",
        "Total Active Categories"
      ),
      rownames = FALSE,
      selection = "none",
      options = list(
        pageLength = 25,
        order = list(list(1, "asc")),
        columnDefs = list(list(visible = FALSE, targets = 0))
      ),
      callback = DT::JS(
        "
        table.on('dblclick', 'tr', function() {
          var data = table.row(this).data();
          if (data) {
            Shiny.setInputValue('hub_dblclicked', data[0], {priority: 'event'});
          }
        });
        "
      )
    )
  })

  observeEvent(input$new_hub_record, {
    showModal(modalDialog(
      title = "Add New RISE Hub Master Specification",
      textInput(
        "new_hub_name_input",
        "Enter Unique Hub Name",
        placeholder = "e.g., North West Regional Hub"
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_new_hub", "Save Hub Master", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$save_new_hub, {
    req(input$new_hub_name_input)

    new_id <- tryCatch(
      {
        dauPortalTools::db_ruh_add_hub(
          hub_name = input$new_hub_name_input,
          user_id = dauPortalTools::get_user(session)
        )
      },
      error = function(e) {
        message("Database Exception: ", e$message)
        NULL
      }
    )

    if (!is.null(new_id)) {
      removeModal()
      refresh_hubs(refresh_hubs() + 1)
      showNotification(
        glue::glue(
          "Successfully registered '{input$new_hub_name_input}' (ID: {new_id})"
        ),
        type = "message"
      )
    } else {
      showNotification(
        "Failed to write Hub profile record. Ensure naming configurations are distinct.",
        type = "error"
      )
    }
  })

  observeEvent(input$hub_dblclicked, {
    hub_id_val <- suppressWarnings(as.integer(input$hub_dblclicked))
    req(!is.na(hub_id_val))
    selected_hub(hub_id_val)
    updateNavbarPage(session, "main_navbar", selected = "hub_overview")
  })

  server_hub_overview(
    id = "hub_overview_module",
    selected_hub_id = selected_hub,
    selected_urn = active_target$id,
    main_navbar_session = session
  )

  observeEvent(input[["hub_overview_module-hub_school_dblclicked"]], {
    updateNavbarPage(session, "main_navbar", selected = "school_overview")
  })

  # ------------------------------------------------------------------
  # Profiles, Admin Wrappers & Statically Initialized Layers
  # ------------------------------------------------------------------
  server_user_menu(
    "user_menu",
    user = user,
    user_role = user_role,
    sigchange_data = sigchange_data,
    sigchange_types = sigchange_types,
    giaschange_types = giaschange_types,
    selected_sigchange = selected_sigchange,
    selected_urn = reactive({
      active_target$id
    }),
    original_record = original_record
  )

  observeEvent(
    TRUE,
    {
      dauPortalTools::server_portal_user_admin("admin")
    },
    once = TRUE
  )

  server_quality_wrapper("quality", app_id = utils_get_app_id())

  global_selected_event_type_id <- reactiveVal(0)

  server_event_catalog_page(
    "events_master_catalog",
    global_selected_event_type_id,
    session
  )
  server_event_type_blueprint_workspace(
    "event_type_blueprint_workspace",
    global_selected_event_type_id,
    session
  )

  # ------------------------------------------------------------------
  # Master Apps Routing
  # ------------------------------------------------------------------
  selected_event_id_token <- reactiveVal(NULL)

  server_entity_page(
    id = "schoolpage",
    active_target = active_target
  )

  event_instance_router <- server_event_instance_page(
    id = "event_instance_panel",
    selected_event_id = selected_event_id_token
  )

  observeEvent(input$`schoolpage-event_timeline_dblclicked`, {
    id_val <- input$`schoolpage-event_timeline_dblclicked`
    req(id_val)

    selected_event_id_token(as.integer(id_val))
    print(paste("Navigating directly to event instance entry ID:", id_val))

    updateNavbarPage(
      session = session,
      inputId = "main_navbar",
      selected = "universal_event_instance_page"
    )
  })

  observeEvent(event_instance_router$go_back(), {
    selected_event_id_token(NULL)

    updateNavbarPage(
      session = session,
      inputId = "main_navbar",
      selected = "school_overview"
    )
  })

  # =================================================================================
  # ADMINISTRATION SYSTEM WORKSPACES
  # =================================================================================

  tryCatch(
    {
      server_rise_support_types_admin("rise_support_types")
    },
    error = function(e) {
      log_event(glue::glue(
        "Error spinning up support types admin server: {e$message}"
      ))
    }
  )

  tryCatch(
    {
      server_rise_actions_admin("rise_actions")
    },
    error = function(e) {
      log_event(glue::glue(
        "Error spinning up actions catalog admin server: {e$message}"
      ))
    }
  )

  tryCatch(
    {
      server_quality_wrapper("quality", app_id = utils_get_app_id())
    },
    error = function(e) {
      log_event(glue::glue(
        "Error spinning up regional quality admin server: {e$message}"
      ))
    }
  )
}
