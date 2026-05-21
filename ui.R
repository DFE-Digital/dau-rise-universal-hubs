ui <- bslib::page_fluid(
  # Clipboard helper script
  tags$script(HTML(
    "
    document.addEventListener('click', function(e) {
      if (e.target.classList && e.target.classList.contains('copy-btn')) {
        const text = e.target.getAttribute('data-copy-text');
        const msgId = e.target.getAttribute('data-msg-id');
        navigator.clipboard.writeText(text).then(() => {
          const el = document.getElementById(msgId);
          if (el) {
            el.style.display = 'inline';
            setTimeout(() => { el.style.display = 'none'; }, 2000);
          }
        });
      }
    });
  "
  )),

  dfeshiny::header(
    "RISE Universal Hubs Tracker",
    logo_alt_text = "Department for Education logo",
    main_alt_text = "RISE Universal Hubs Tracker"
  ),
  shinyGovstyle::banner("beta banner", "Beta", "This dashboard is in beta."),

  # ---- TOP NAVBAR ----
  bslib::page_navbar(
    id = "main_navbar",

    # --- Home ---
    bslib::nav_panel(
      title = tagList(icon("home")),
      value = "home",
      shiny::tags$div(
        style = "display: block; height: auto; padding: 20px;",
        fluidRow(
          column(
            8,
            textOutput("welcome_user"),
            br(),
            p(
              "This tool helps policy colleagues track and manage RISE Universal Hubs."
            ),
            br(),
            uiOutput("summary_metrics")
          ),
          column(
            4,
            uiOutput("news")
          )
        )
      )
    ),

    # --- Search ---
    bslib::nav_panel(
      title = "Search",
      value = "search",
      layout_sidebar(
        sidebar = sidebar(
          title = "Search Filters",
          uiOutput("filters")
        ),
        layout_column_wrap(
          width = 1,
          card(
            card_header("Filtered Schools"),
            div(
              style = "height: 1000px; overflow-y: auto;",
              dataTableOutput("school_table")
            )
          )
        )
      )
    ),

    # --- School Overview ---
    bslib::nav_panel(
      title = "School Details",
      value = "school_overview",
      school_page_ui("schoolpage")
    ),

    # --- Hub Support Page ---
    bslib::nav_panel(
      title = "Hub Support",
      value = "hub_support_page",
      ui_hub_support_page("hub_support_page")
    ),

    # --- Hub Lead Page ---
    bslib::nav_panel(
      title = "Hub Lead Status",
      value = "hub_lead_page",
      ui_hub_lead_page("hub_lead_page")
    ),

    # --- Hubs Search Page ---
    bslib::nav_panel(
      title = "Hubs Search",
      value = "hubs_search",
      layout_sidebar(
        sidebar = sidebar(
          title = "Search Filters",
          textInput("filter_hub_name", "Hub Name")
        ),
        actionButton(
          "new_hub_record",
          "Add New Hub",
          class = "btn govuk-button"
        ),
        layout_column_wrap(
          width = 1,
          card(
            card_header("RISE Hubs"),
            div(
              style = "height: 800px; overflow-y: auto;",
              DT::dataTableOutput("hub_table")
            )
          )
        )
      )
    ),

    # --- Hubs Overview Page ---
    bslib::nav_panel(
      title = "Hub Overview",
      value = "hub_overview",
      ui_hub_overview("hub_overview_module")
    ),

    # --- Support Page ---
    bslib::nav_panel(
      title = "Support",
      value = "support",
      ui_about_page()
    ),

    # --- USER MENU ---
    ui_user_menu("user_menu")
  ),

  shinyGovstyle::footer(TRUE)
)
