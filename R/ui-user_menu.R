ui_user_menu <- function(id) {
  ns <- shiny::NS(id)

  bslib::nav_panel(
    title = "User Workspace",
    value = "user_menu",

    tags$style(HTML(
      "
      .user-console-container {
        display: flex;
        min-height: calc(100vh - 200px);
        margin-top: 15px;
      }
      .user-sidebar {
        width: 280px;
        background-color: #f3f2f1;
        border-right: 3px solid #b1b4b6;
        padding: 20px 15px;
        flex-shrink: 0;
      }
      .user-main-window {
        flex-grow: 1;
        padding: 10px 40px;
        background-color: #ffffff;
      }
      .user-side-nav-item {
        padding: 12px 15px !important;
        border-bottom: 1px solid #b1b4b6 !important;
        list-style-type: none !important;
        margin: 0 !important;
      }
      .user-side-nav-link {
        font-weight: bold !important;
        text-decoration: none !important;
        color: #1d70b8 !important;
        display: block !important;
        width: 100% !important;
      }
    "
    )),

    shiny::div(
      class = "user-console-container",

      shiny::div(
        class = "user-sidebar",
        shiny::div(
          style = "font-weight: bold; font-size: 19px; margin-bottom: 15px; padding-left: 10px;",
          "Account Settings"
        ),
        tags$ul(
          style = "padding-left: 0; margin: 0; list-style-type: none;",
          tags$li(
            class = "user-side-nav-item",
            shiny::actionLink(
              ns("side_nav_profile"),
              "👤 Profile Overview",
              class = "user-side-nav-link"
            )
          ),
          tags$li(
            class = "user-side-nav-item",
            shiny::actionLink(
              ns("side_nav_work"),
              "📋 Assigned Work",
              class = "user-side-nav-link"
            )
          ),
          tags$li(
            class = "user-side-nav-item",
            shiny::actionLink(
              ns("side_nav_quality"),
              "⚠️ Quality Assertions",
              class = "user-side-nav-link"
            )
          )
        )
      ),

      shiny::div(
        class = "user-main-window",
        bslib::navset_hidden(
          id = ns("user_sub_pages"),

          bslib::nav_panel(
            title = NULL,
            value = "user_sub_profile",

            shinyGovstyle::heading_text("User Profile", size = "l"),
            br(),
            bslib::layout_column_wrap(
              width = 1,
              bslib::card(
                style = "background: #f3f2f1; border-left: 4px solid #1d70b8;",
                shiny::h3("Signed in as", style = "margin-top: 0;"),
                tags$strong(shiny::textOutput(
                  ns("profile_user"),
                  inline = TRUE
                )),
                shiny::textOutput(ns("profile_role"))
              )
            ),
            br(),
            bslib::layout_column_wrap(
              width = 1 / 3,
              bslib::card(
                style = "border-top: 4px solid #1d70b8;",
                bslib::card_header("Your Live Records"),
                tags$h2(
                  shiny::textOutput(ns("user_live_records")),
                  style = "margin: 0; font-weight: bold;"
                )
              ),
              bslib::card(
                style = "border-top: 4px solid #f47738;",
                bslib::card_header("Your Updates This Month"),
                tags$h2(
                  shiny::textOutput(ns("user_monthly_updates")),
                  style = "margin: 0; font-weight: bold;"
                )
              ),
              bslib::card(
                style = "border-top: 4px solid #d4351c;",
                bslib::card_header("Quality Issues Assigned to You"),
                tags$h2(
                  shiny::textOutput(ns("user_quality_issues")),
                  style = "margin: 0; font-weight: bold; color: #d4351c;"
                )
              )
            )
          ),

          bslib::nav_panel(
            title = NULL,
            value = "user_sub_work",
            shinyGovstyle::heading_text("Assigned Work Tracker", size = "m"),
            p(
              "Review open tracking rows assigned under your staff credentials context:"
            ),
            br(),
            shiny::div(
              style = "overflow-y: auto;",
              DT::dataTableOutput(ns("assigned_tbl"))
            )
          ),

          bslib::nav_panel(
            title = NULL,
            value = "user_sub_quality",
            shinyGovstyle::heading_text("Quality Issues Ledger", size = "m"),
            br(),
            shiny::div(
              style = "overflow-y: auto;",
              DT::dataTableOutput(ns("quality_tbl"))
            )
          )
        )
      )
    )
  )
}
