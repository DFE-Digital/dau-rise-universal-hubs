#' Build Dynamic Admin Side Navigation Menu
#'
#' @param user_role Character string representing the user's role.
#' @return A shiny::tagList containing the CSS styles and HTML navigation list.
build_admin_side_nav <- function(user_role) {
  role <- tolower(trimws(user_role))

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
}
