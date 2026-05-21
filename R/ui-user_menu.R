ui_user_menu <- function(id) {
  ns <- shiny::NS(id)

  bslib::nav_menu(
    title = shiny::tagList(shiny::icon("user"), "User"),
    align = "right",

    bslib::nav_panel(
      "Profile",

      bslib::layout_column_wrap(
        width = 1,

        bslib::card(
          shiny::h3("Signed in as"),
          shiny::textOutput(ns("profile_user")),
          shiny::textOutput(ns("profile_role"))
        )
      ),

      bslib::layout_column_wrap(
        width = 1 / 3,

        bslib::card(
          bslib::card_header("Your Live Records"),
          shiny::textOutput(ns("user_live_records"))
        ),

        bslib::card(
          bslib::card_header("Your Updates This Month"),
          shiny::textOutput(ns("user_monthly_updates"))
        ),

        bslib::card(
          bslib::card_header("Quality Issues Assigned to You"),
          shiny::textOutput(ns("user_quality_issues"))
        )
      )
    ),

    bslib::nav_panel(
      "Assigned Work",

      bslib::layout_column_wrap(
        width = 1,

        bslib::card(
          bslib::card_header("Your open Sig Change records"),
          shiny::div(
            style = "height: 70vh; overflow-y: auto;",
            DT::dataTableOutput(ns("assigned_tbl"))
          )
        )
      )
    ),

    bslib::nav_panel(
      "Quality",

      bslib::layout_column_wrap(
        width = 1,

        bslib::card(
          bslib::card_header("Quality issues"),
          shiny::div(
            style = "height: 70vh; overflow-y: auto;",
            DT::dataTableOutput(ns("quality_tbl"))
          )
        )
      )
    )
  )
}
