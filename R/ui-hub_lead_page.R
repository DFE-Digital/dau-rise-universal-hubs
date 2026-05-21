ui_hub_lead_page <- function(id) {
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
        bslib::card_header("Lead Hub Designation Status"),
        layout_column_wrap(
          width = 1 / 3,
          selectInput(ns("hub_id"), "Associated Hub Framework", choices = NULL),

          ui_date_input(ns("date_active"), "Date Status Active From"),
          ui_date_input(ns("date_ended"), "Date Status Concluded", value = NA)
        ),
        textAreaInput(
          ns("comment"),
          "Designation Notes / Authority Profile",
          rows = 3
        ),
        actionButton(
          ns("save_lead"),
          "Update Designation Status",
          class = "btn-primary"
        )
      )
    )
  )
}
