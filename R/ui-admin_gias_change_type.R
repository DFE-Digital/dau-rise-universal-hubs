ui_gias_change_type_admin <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Manage GIAS Change Types"),
    p("Use this screen to manage or add new GIAS Types."),
    actionButton(ns("add_new"), "Add New", class = "btn govuk-button"),
    br(),
    DTOutput(ns("table")),
    uiOutput(ns("modal_ui"))
  )
}
