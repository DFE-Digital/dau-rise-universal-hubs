build_admin_menu <- function(role) {
  role <- tolower(trimws(role %||% ""))

  if (role == "admin") {
    return(
      bslib::nav_menu(
        title = tagList(icon("gear"), "Admin"),
        align = "right",
        bslib::nav_panel(
          "RISE Support Types",
          layout_column_wrap(
            width = 1,
            card(ui_rise_support_types_admin("rise_support_types"))
          )
        ),
        bslib::nav_panel(
          "RISE Action Catalog",
          layout_column_wrap(
            width = 1,
            card(ui_rise_actions_admin("rise_actions"))
          )
        )
      )
    )
  }

  if (role == "regional_admin") {
    return(
      bslib::nav_menu(
        title = tagList(icon("gear"), "Admin"),
        align = "right",
        bslib::nav_panel(
          "Regional Quality Issues",
          layout_column_wrap(
            width = 1,
            card(
              ui_quality_wrapper("quality")
            )
          )
        )
      )
    )
  }

  bslib::nav_menu(
    title = tagList(icon("gear"), "Admin"),
    align = "right",
    bslib::nav_item("No admin permissions")
  )
}
