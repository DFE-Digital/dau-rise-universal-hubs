#' Event Instance Record Management Page Module UI
#'
#' @param id Character scalar. Shiny namespace identifier.
#' @export
ui_event_instance_page <- function(id) {
  ns <- NS(id)

  bslib::page_fluid(
    shinyjs::useShinyjs(),

    bslib::card(
      bslib::card_header(
        class = "bg-primary text-white d-flex justify-content-between align-items-center",
        tags$h5(
          style = "margin: 0;",
          "Manage Dynamic Interaction Event Instance Workspace Profile"
        ),
        actionButton(
          ns("back_to_profile"),
          "Return to Target Profile View",
          class = "btn btn-light btn-sm",
          icon = icon("arrow-left")
        )
      ),
      div(style = "padding: 10px;", uiOutput(ns("lead_provider_status_banner")))
    ),
    br(),

    bslib::layout_column_wrap(
      width = 1 / 2,
      gap = "20px",

      bslib::card(
        bslib::card_header(
          class = "bg-dark text-white",
          "Event Container Parent Metadata Context Details Parameters"
        ),
        div(style = "padding: 20px;", uiOutput(ns("read_only_event_meta")))
      ),

      bslib::card(
        bslib::card_header(
          class = "bg-dark text-white",
          "Executed Custom Metrics Responses Tracking Ledger Ledger"
        ),
        div(
          style = "padding: 15px; overflow-y: auto;",
          p(em(
            class = "text-muted",
            "Double-click an metric execution line value parameter item below to safely view history modifications."
          )),
          DT::DTOutput(ns("sub_actions_executions_table"))
        )
      )
    )
  )
}
