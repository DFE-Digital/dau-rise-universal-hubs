#' Polymorphic Hub Lead & Provider Contract Management Page Module UI
#'
#' @param id Character scalar. Shiny namespace identifier.
#' @export
ui_hub_lead_page <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        12,
        div(
          style = "margin-bottom: 20px; margin-top: 10px;",
          actionButton(
            ns("back_to_profile"),
            "Return to Node Profile Context",
            icon = icon("arrow-left"),
            class = "btn-secondary"
          )
        )
      )
    ),

    bslib::card(
      bslib::card_header(
        class = "bg-dark text-white d-flex justify-content-between align-items-center",
        tags$span("Provider Assignment Core Information Profile Summary"),
        actionButton(
          ns("btn_open_edit_modal"),
          "Modify Assignment Meta Scope",
          class = "btn-warning btn-sm",
          icon = icon("edit")
        )
      ),
      div(style = "padding: 20px;", uiOutput(ns("read_only_profile")))
    ),
    br(),

    uiOutput(ns("summary_stats_container")),
    br(),

    fluidRow(
      column(
        5,
        bslib::card(
          bslib::card_header(
            class = "bg-primary text-white d-flex justify-content-between align-items-center",
            tags$span("Assigned Program Cohort Blocks Focus"),
            actionButton(
              ns("btn_add_cohort"),
              "Assign Cohort Index",
              class = "btn-success btn-sm",
              icon = icon("plus")
            )
          ),
          div(style = "padding: 10px;", DT::DTOutput(ns("cohorts_table")))
        )
      ),
      column(
        7,
        bslib::card(
          bslib::card_header(
            class = "bg-primary text-white d-flex justify-content-between align-items-center",
            tags$span("Granular Connected Entity Support Contracts"),
            actionButton(
              ns("btn_add_entity_link"),
              "Link Active Sub-Contracts",
              class = "btn-success btn-sm",
              icon = icon("link")
            )
          ),
          div(
            style = "padding: 10px;",
            DT::DTOutput(ns("assigned_entities_table"))
          )
        )
      )
    )
  )
}
