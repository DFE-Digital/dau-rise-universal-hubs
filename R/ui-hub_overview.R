ui_hub_overview <- function(id) {
  ns <- NS(id)

  bslib::nav_panel(
    title = "Hub Overview",
    value = "hub_overview",

    bslib::layout_column_wrap(
      width = 1,
      bslib::card(
        bslib::card_header("Hub Configuration"),
        layout_column_wrap(
          width = 1,
          div(
            style = "display: flex; gap: 1rem; align-items: flex-end;",
            textInput(ns("hub_name_edit"), "Hub Name", width = "400px"),
            actionButton(
              ns("save_hub_details"),
              "Update Hub Name",
              class = "btn-primary",
              icon = icon("save"),
              style = "margin-bottom: 15px;"
            )
          )
        )
      )
    ),

    uiOutput(ns("hub_stats_boxes")),

    bslib::card(
      bslib::card_header(
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          "Schools Supported by this Hub",
          checkboxInput(
            ns("include_inactive"),
            "Show inactive assignments",
            value = FALSE
          )
        )
      ),
      DT::dataTableOutput(ns("hub_support_schools_table")),
      helpText("Double-click a row to view the School Details page.")
    ),

    bslib::card(
      bslib::card_header(
        div(
          style = "display: flex; justify-content: space-between;",
          "Available Support Types",
          actionButton(
            ns("add_support_type"),
            "Add Type",
            icon = icon("plus"),
            class = "btn-sm"
          )
        )
      ),
      DT::dataTableOutput(ns("support_types_table"))
    )
  )
}
