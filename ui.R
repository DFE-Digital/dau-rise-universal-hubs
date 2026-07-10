ui <- bslib::page_fluid(
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
    header = "RISE Universal Portal",
    logo_alt_text = "Department for Education logo"
  ),
  shinyGovstyle::banner("beta banner", "Beta", "This dashboard is in beta."),

  shiny::uiOutput("dynamic_gds_service_navigation"),
  br(),

  bslib::navset_hidden(
    id = "main_navbar",

    # --- Home View panel ---
    bslib::nav_panel(
      title = "Home",
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

    # --- Search Matrix Panel ---
    bslib::nav_panel(
      title = "Search",
      value = "search",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          title = "Search Filters",
          uiOutput("filters")
        ),
        bslib::layout_column_wrap(
          width = 1,
          bslib::card(
            bslib::card_header("Filtered Entities"),
            div(
              style = "height: 1000px; overflow-y: auto;",
              dataTableOutput("school_table")
            )
          )
        )
      )
    ),

    # --- Polymorphic Entity Overview ---
    bslib::nav_panel(
      title = "Entity Target Details",
      value = "school_overview",
      school_page_ui("schoolpage")
    ),

    # --- Hub Support Panel ---
    bslib::nav_panel(
      title = "Hub Support Page",
      value = "hub_support_page",
      ui_hub_support_page("hub_support_page")
    ),

    # --- Hub Lead Panel ---
    bslib::nav_panel(
      title = "Hub Lead Page",
      value = "hub_lead_page",
      ui_hub_lead_page("hub_lead_page")
    ),

    # --- Event Panel ---
    bslib::nav_panel(
      title = "Event Page",
      value = "event_instance_page",
      ui_event_instance_page("event_instance_page")
    ),

    # --- Hubs Search Panel ---
    bslib::nav_panel(
      title = "Hubs Search",
      value = "hubs_search",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          title = "Search Filters",
          textInput("filter_hub_name", "Hub Name")
        ),
        actionButton(
          "new_hub_record",
          "Add New Hub",
          class = "btn govuk-button"
        ),
        bslib::layout_column_wrap(
          width = 1,
          bslib::card(
            bslib::card_header("RISE Hubs"),
            div(
              style = "height: 800px; overflow-y: auto;",
              DT::dataTableOutput("hub_table")
            )
          )
        )
      )
    ),

    # --- Hubs Detailed Overview Sub-Panel ---
    bslib::nav_panel(
      title = "Hub Overview Page",
      value = "hub_overview",
      ui_hub_overview("hub_overview_module")
    ),

    # --- Events Config Master Catalog Panel ---
    bslib::nav_panel(
      title = "Events Search Page",
      value = "events_master_catalog",
      ui_event_catalog_page("events_master_catalog")
    ),

    # --- Event Workspace Blueprint Sub-Panel ---
    bslib::nav_panel(
      title = "Event Overview Page",
      value = "event_overview",
      ui_event_overview("event_overview")
    ),

    # --- Global Support Guide Page ---
    bslib::nav_panel(
      title = "Support",
      value = "support",
      ui_about_page()
    ),

    # --- Administration Console---
    bslib::nav_panel(
      title = "Admin Dashboard",
      value = "admin_dashboard",

      tags$style(HTML(
        "
        .admin-console-container {
          display: flex;
          min-height: calc(100vh - 200px);
          margin-top: 15px;
        }
        .admin-sidebar {
          width: 280px;
          background-color: #f3f2f1; /* Official GOV.UK Light Grey */
          border-right: 3px solid #b1b4b6;
          padding: 20px 15px;
          flex-shrink: 0;
        }
        .admin-main-window {
          flex-grow: 1;
          padding: 10px 40px;
          background-color: #ffffff;
        }
        .admin-sidebar-title {
          font-family: 'GDS Transport', arial, sans-serif;
          font-weight: bold;
          font-size: 19px;
          color: #0b0c0c;
          margin-bottom: 15px;
          padding-left: 10px;
        }
      "
      )),

      shiny::div(
        class = "admin-console-container",

        shiny::div(
          class = "admin-sidebar",
          shiny::div(class = "admin-sidebar-title", "Admin Navigation"),
          shiny::uiOutput("admin_side_nav_container")
        ),

        shiny::div(
          class = "admin-main-window",
          bslib::navset_hidden(
            id = "admin_sub_pages",

            bslib::nav_panel(
              title = NULL,
              value = "sub_admin_landing",

              shiny::div(
                style = "max-width: 800px; padding: 20px 0;",
                shinyGovstyle::heading_text(
                  "Administration Console",
                  size = "l"
                ),
                br(),
                shiny::tags$p(
                  class = "govuk-body govuk-!-font-size-19",
                  style = "font-weight: bold; color: #0b0c0c; margin-bottom: 20px;",
                  "Welcome to the central command hub for the RISE Universal data infrastructure."
                ),
                shiny::tags$p(
                  class = "govuk-body",
                  style = "color: #505a5f; line-height: 1.6; font-size: 16px;",
                  "Please utilize the vertical sidebar navigation panel to manage global framework configuration types, modify the master action validation schemas, or process active regional quality assurance issues."
                )
              )
            ),

            bslib::nav_panel(
              title = NULL,
              value = "sub_support_types",
              ui_rise_support_types_admin("rise_support_types")
            ),

            bslib::nav_panel(
              title = NULL,
              value = "sub_action_catalog",
              ui_rise_actions_admin("rise_actions")
            ),

            bslib::nav_panel(
              title = NULL,
              value = "sub_regional_quality",
              ui_quality_wrapper("quality")
            )
          )
        )
      )
    ),

    # --- User Profile Settings Submodule ---
    ui_user_menu("user_menu")
  ),

  br(),
  shinyGovstyle::footer(TRUE)
)
