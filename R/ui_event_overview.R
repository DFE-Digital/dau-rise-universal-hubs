#' Event Detailed Configuration Overview Panel UI
#' @export
ui_event_overview <- function(id) {
  ns <- NS(id)

  bslib::page_fluid(
    bslib::card(
      style = "border-top: 4px solid #005ea5; margin-bottom: 20px;",
      bslib::card_header(
        class = "bg-primary text-white d-flex justify-content-between align-items-center",
        tags$span("Event Blueprint Hierarchy Workspace"),
        actionButton(
          ns("back_to_events_search"),
          "Return to Events Directory",
          class = "btn btn-light btn-sm",
          icon = icon("arrow-left")
        )
      ),
      uiOutput(ns("type_meta_cards"))
    ),
    br(),
    uiOutput(ns("event_stats_boxes")),
    br(),

    fluidRow(
      column(
        6,
        bslib::card(
          style = "margin-bottom: 25px;",
          bslib::card_header(
            class = "bg-secondary text-white d-flex justify-content-between align-items-center",
            tags$span(
              style = "font-weight: bold;",
              "Tracked Institutional Event Allocations (Provisions)"
            ),
            checkboxInput(
              ns("include_inactive"),
              "Show Inactive",
              value = FALSE
            )
          ),
          div(
            style = "padding: 15px; height: 380px; overflow-y: auto;",
            DT::dataTableOutput(ns("event_support_schools_table"))
          ),
          tags$div(
            class = "govuk-hint",
            style = "padding: 0 15px 15px 15px; font-size: 13px;",
            "💡 Double-click any row to jump straight to that Entity Target Details page."
          )
        )
      ),
      column(
        6,
        bslib::card(
          style = "margin-bottom: 25px;",
          bslib::card_header(
            class = "bg-dark text-white",
            tags$span(
              style = "font-weight: bold;",
              "Designated Event Providers & System Supporters"
            )
          ),
          div(
            style = "padding: 15px; height: 380px; overflow-y: auto;",
            DT::dataTableOutput(ns("event_lead_providers_table"))
          ),
          tags$div(
            class = "govuk-hint",
            style = "padding: 0 15px 15px 15px; font-size: 13px;",
            "💡 Double-click any provider row to jump directly to its Lead Assignment details dashboard."
          )
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
          tags$span(
            style = "font-weight: bold;",
            "1. Regional Event Categories (Cohorts)"
          ),
          actionButton(
            ns("add_event_type"),
            "Add Category / Cohort",
            icon = icon("plus"),
            class = "btn btn-success btn-sm"
          )
        ),
        div(
          style = "padding: 15px; min-height: 350px; overflow-y: auto;",
          DT::dataTableOutput(ns("event_types_table"))
        )
      ),
      bslib::card(
        bslib::card_header(
          class = "bg-primary text-white d-flex justify-content-between align-items-center",
          uiOutput(ns("sub_workspace_header_title")),
          actionButton(
            ns("add_blueprint_field_btn"),
            "Configure Action Field Rule",
            class = "btn btn-success btn-sm",
            icon = icon("plus")
          )
        ),
        div(
          style = "padding: 15px; min-height: 350px; overflow-y: auto;",
          uiOutput(ns("sub_workspace_dynamic_content"))
        )
      )
    )
  )
}
