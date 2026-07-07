#' Polymorphic Hub Provision Support Tracking Page Module UI
#'
#' Renders the master form panel for updating tracking records and triggers dynamic event inputs.
#'
#' @param id Character scalar.
#' @export
ui_hub_support_page <- function(id) {
  ns <- NS(id)

  bslib::page_fluid(
    bslib::card(
      bslib::card_header(
        class = "bg-secondary text-white d-flex justify-content-between align-items-center",
        tags$span("Active Provision Track Administration Panel"),
        actionButton(
          ns("back_to_school"),
          "Return to Target Profile",
          class = "btn btn-light btn-sm",
          icon = icon("arrow-left")
        )
      ),

      div(
        style = "padding: 20px;",
        fluidRow(
          column(
            4,
            selectInput(
              ns("hub_id"),
              "Parent Hub Regional Matrix Location:",
              choices = character(0)
            )
          ),
          column(
            4,
            selectInput(
              ns("ruht_id"),
              "Framework Track Category:",
              choices = character(0)
            )
          )
        ),
        br(),
        fluidRow(
          column(
            12,
            textAreaInput(
              ns("comment"),
              "Provision Context / Strategic Baseline Notes:",
              rows = 3,
              width = "100%"
            )
          )
        ),
        br(),
        fluidRow(
          column(
            6,
            ui_date_input(
              ns("date_active"),
              "Framework Commencement Date active from:"
            )
          ),
          column(
            6,
            ui_date_input(
              ns("date_ended"),
              "Framework Track Termination Date (Optional):"
            )
          )
        ),
        br(),
        actionButton(
          ns("save_support"),
          "Commit Provision Changes",
          class = "btn btn-primary",
          icon = icon("save")
        )
      )
    ),
    br(),

    bslib::card(
      bslib::card_header(
        class = "bg-dark text-white d-flex justify-content-between align-items-center",
        tags$span(
          "Point-in-Time Interaction History & Evaluation Metrics Logs"
        ),
        actionButton(
          ns("add_action"),
          "Log New Interaction Event",
          class = "btn btn-success btn-sm",
          icon = icon("calendar-plus")
        )
      ),
      div(
        style = "padding: 15px;",
        DT::DTOutput(ns("actions_table"))
      )
    )
  )
}
