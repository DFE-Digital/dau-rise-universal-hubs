#' Event Type Blueprint Custom Metrics Workspace UI
#' @export
ui_event_type_blueprint_workspace <- function(id) {
  ns <- NS(id)

  bslib::page_fluid(
    bslib::card(
      bslib::card_header(
        class = "bg-primary text-white d-flex justify-content-between align-items-center",
        tags$span("Event Blueprint Hierarchy Workspace"),
        actionButton(
          ns("back_to_catalog"),
          "Return to Catalog Matrix",
          class = "btn btn-light btn-sm",
          icon = icon("arrow-left")
        )
      ),
      uiOutput(ns("type_meta_cards"))
    ),
    br(),
    uiOutput(ns("bi_metrics_panel")),
    br(),
    bslib::layout_column_wrap(
      width = 1 / 2,
      gap = "15px",
      bslib::card(
        bslib::card_header(
          class = "bg-secondary text-white d-flex justify-content-between align-items-center",
          tags$span("Configured Sub-Varieties / Cohorts"),
          actionButton(
            ns("new_sub_variety_btn"),
            "Add Sub-Variety",
            class = "btn btn-success btn-sm",
            icon = icon("plus")
          )
        ),
        div(
          style = "padding: 10px; height: 400px; overflow-y: auto;",
          DT::DTOutput(ns("sub_varieties_table"))
        )
      ),
      bslib::card(
        bslib::card_header(
          class = "bg-dark text-white d-flex justify-content-between align-items-center",
          tags$span("Dynamic Metrics Form Input Blueprints (Actions)"),
          actionButton(
            ns("new_field_btn"),
            "Configure New Metric Field",
            class = "btn btn-success btn-sm",
            icon = icon("plus")
          )
        ),
        div(
          style = "padding: 10px; height: 400px; overflow-y: auto;",
          DT::DTOutput(ns("fields_config_table"))
        )
      )
    )
  )
}
