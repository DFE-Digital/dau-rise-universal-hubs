#' Polymorphic Hub Provision Support Tracking Page Module UI
#'
#' @param id Character scalar. Shiny namespace identifier.
#' @export
ui_hub_support_page <- function(id) {
  ns <- NS(id)

  bslib::page_fluid(
    shinyjs::useShinyjs(),

    bslib::card(
      style = "margin-bottom: 20px;",
      bslib::card_header(
        class = "bg-primary text-white d-flex justify-content-between align-items-center",
        tags$h5(
          style = "margin: 0;",
          "RISE Framework Support Provision Workspace"
        ),
        actionButton(
          ns("back_to_school"),
          "Return to Node Profile Context",
          class = "btn btn-light btn-sm",
          icon = icon("arrow-left")
        )
      )
    ),

    bslib::card(
      style = "margin-bottom: 20px;",
      bslib::card_header(
        class = "bg-dark text-white d-flex justify-content-between align-items-center",
        tags$span("Core Provision Contract Information Profile Summary"),
        actionButton(
          ns("btn_open_edit_modal"),
          "Modify Contract Parameters",
          class = "btn btn-warning btn-sm",
          icon = icon("edit")
        )
      ),
      div(style = "padding: 20px;", uiOutput(ns("read_only_contract_profile")))
    ),

    bslib::card(
      style = "margin-bottom: 20px;",
      bslib::card_header(
        class = "bg-secondary text-white",
        "Designated Framework Lead Supporter Node"
      ),
      div(style = "padding: 15px;", uiOutput(ns("lead_provider_status_banner")))
    ),

    bslib::card(
      style = "margin-bottom: 25px;",
      bslib::card_header(
        class = "bg-dark text-white d-flex justify-content-between align-items-center",
        tags$span(
          "Actions/Data: Interaction Milestone Blueprint Actions Timeline"
        ),
        actionButton(
          ns("add_action"),
          "Execute Sub-Action Metric Requirement Block",
          class = "btn btn-success btn-sm",
          icon = icon("calendar-plus")
        )
      ),
      div(
        style = "padding: 15px; overflow-y: auto;",
        p(em(
          class = "text-muted",
          "💡 Double-click any active tracking requirement line parameter item below to view or alter logs safely."
        )),
        DT::DTOutput(ns("actions_table"))
      )
    )
  )
}
