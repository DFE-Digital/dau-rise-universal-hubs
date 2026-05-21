ui_hub_support_page <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        12,
        div(
          style = "margin-bottom: 20px; margin-top: 10px;",
          actionButton(
            ns("back_to_school"),
            "Back to School Profile",
            icon = icon("arrow-left"),
            class = "govuk-button--secondary"
          )
        )
      )
    ),

    layout_column_wrap(
      width = 1,

      bslib::card(
        bslib::card_header("Support Framework Status"),

        layout_column_wrap(
          width = 1 / 4,
          selectInput(ns("hub_id"), "Managing Hub", choices = NULL),
          selectInput(ns("ruht_id"), "Support Type", choices = NULL),

          ui_date_input(ns("date_active"), "Date Support Started"),
          ui_date_input(ns("date_ended"), "Date Support Ended", value = NA)
        ),

        textAreaInput(ns("comment"), "Overall Progress Notes", rows = 3),
        actionButton(
          ns("save_support"),
          "Update Framework Status",
          class = "btn-primary"
        )
      ),

      bslib::card(
        bslib::card_header("Intervention Log (Actions)"),
        p("Record specific meetings, visits, or support milestones here."),
        DT::DTOutput(ns("actions_table")),
        div(
          style = "margin-top: 15px;",
          actionButton(
            ns("add_action"),
            "Log New Action",
            class = "btn-success",
            icon = icon("plus")
          )
        )
      )
    )
  )
}
