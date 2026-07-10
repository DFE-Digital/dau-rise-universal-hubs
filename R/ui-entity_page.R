#' Entity Target Details Management Page Module UI
#'
#' @param id Character scalar. Shiny namespace identifier.
#' @export
school_page_ui <- function(id) {
  ns <- NS(id)

  bslib::page_fluid(
    shinyjs::useShinyjs(),

    bslib::card(
      shiny::uiOutput(ns("dynamic_entity_overview_container"))
    ),
    br(),

    bslib::card(
      bslib::card_header(
        class = "bg-dark text-white d-flex justify-content-between align-items-center",
        tags$span("Hubs Support Contracts"),
        actionButton(
          ns("new_support_record"),
          "Log New Hub Support",
          class = "btn btn-success btn-sm",
          icon = icon("plus")
        )
      ),
      div(
        style = "padding: 15px; overflow-y: auto;",
        DT::DTOutput(ns("hubs_list"))
      )
    ),

    br(),

    bslib::card(
      bslib::card_header(
        class = "bg-primary text-white d-flex justify-content-between align-items-center",
        tags$span("RISE Universal Events Timeline"),
        actionButton(
          ns("add_new_event_transaction"),
          "Log Event",
          class = "btn btn-success btn-sm",
          icon = icon("calendar-plus")
        )
      ),
      div(
        style = "padding: 15px;",
        DT::DTOutput(ns("universal_events_timeline_table"))
      )
    ),
    br(),
    bslib::card(
      bslib::card_header(
        class = "bg-dark text-white d-flex justify-content-between align-items-center",
        tags$span("Lead & Provider Matrix Assignments"),
        actionButton(
          ns("new_lead_record"),
          "Register Provider Status",
          class = "btn btn-primary btn-sm",
          icon = icon("award")
        )
      ),
      div(
        style = "padding: 15px; overflow-y: auto;",
        DT::DTOutput(ns("support_given"))
      )
    )
  )
}
