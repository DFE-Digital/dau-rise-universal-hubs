#' Event Instance Record Management Page Module UI
#'
#' Renders a clean read-only parent layout panel paired alongside an embedded
#' one-to-many Action Executions Ledger table list view layout grid.
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
        tags$h4(
          style = "margin: 0;",
          "Manage Interaction Event Log & Associated Sub-Actions Matrix"
        ),
        actionButton(
          ns("back_to_profile"),
          "Return to Target Profile",
          class = "btn btn-light btn-sm",
          icon = icon("arrow-left")
        )
      )
    ),
    br(),

    bslib::layout_column_wrap(
      width = 1 / 2,
      gap = "20px",

      bslib::card(
        bslib::card_header(
          class = "bg-dark text-white d-flex justify-content-between align-items-center",
          tags$span("Event Container Parent Metadata Parameters"),
          actionButton(
            ns("trigger_edit_header_modal"),
            "Edit Headers",
            class = "btn btn-warning btn-sm",
            icon = icon("edit")
          )
        ),
        div(
          style = "padding: 15px;",
          fluidRow(
            column(
              6,
              textInput(
                ns("display_type"),
                "Primary Interaction Method:",
                value = ""
              )
            ),
            column(
              6,
              textInput(
                ns("display_sub"),
                "Cohort Focus Sub-Variety:",
                value = ""
              )
            )
          ),
          br(),
          fluidRow(
            column(
              6,
              textInput(
                ns("display_date"),
                "Interaction Event Date:",
                value = ""
              )
            ),
            column(
              6,
              textInput(
                ns("display_completed"),
                "Transaction Status / Flag:",
                value = ""
              )
            )
          ),
          br(),
          textAreaInput(
            ns("display_notes"),
            "Top-Level Event Container Summary Notes:",
            rows = 6,
            width = "100%"
          )
        )
      ),

      bslib::card(
        bslib::card_header(
          class = "bg-dark text-white d-flex justify-content-between align-items-center",
          tags$span("Executed Custom Actions Ledger (One-To-Many Sub-Records)"),
          actionButton(
            ns("trigger_add_subaction_modal"),
            "Execute Action Field",
            class = "btn btn-success btn-sm",
            icon = icon("plus-circle")
          )
        ),
        div(
          style = "padding: 15px; overflow-y: auto;",
          p(em(
            class = "text-muted",
            "Double-click an execution row below to view or modify details safely."
          )),
          DT::DTOutput(ns("sub_actions_executions_table"))
        )
      )
    )
  )
}
