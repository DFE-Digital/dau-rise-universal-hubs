school_page_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("school_details")),

    hr(),

    h4("Support Received from Hub Frameworks"),
    DTOutput(ns("hubs_list")),
    br(),
    actionButton(
      ns("new_support_record"),
      "Log New Support Framework Assignment",
      class = "btn-success",
      icon = icon("plus")
    ),

    br(),
    br(),
    hr(),

    h4("Support and Oversight Given (As Lead Hub School)"),
    DTOutput(ns("support_given")),
    br(),
    actionButton(
      ns("new_lead_record"),
      "Register New Lead Hub Status",
      class = "btn-primary",
      icon = icon("shield-halved")
    )
  )
}
