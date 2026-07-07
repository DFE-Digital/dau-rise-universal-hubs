#' Master Event Types Catalog Search UI
#' @export
ui_event_catalog_page <- function(id) {
  ns <- NS(id)

  bslib::page_fluid(
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        textInput(ns("filter_evt_name"), "Search Event Type Title:")
      ),
      actionButton(
        ns("new_event_type_btn"),
        "Define New Event Type",
        class = "btn btn-success btn-sm",
        icon = icon("plus")
      ),
      br(),
      card(
        card_header("Registered Event Types"),
        div(
          style = "height: 600px; overflow-y: auto;",
          DT::DTOutput(ns("event_types_table"))
        )
      )
    )
  )
}
