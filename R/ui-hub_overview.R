#' Hub Detailed Configuration Overview Panel UI
#' @export
ui_hub_overview <- function(id) {
  ns <- NS(id)

  bslib::page_fluid(
    bslib::card(
      style = "border-top: 4px solid #1d70b8; margin-bottom: 20px;",
      bslib::card_header(
        class = "bg-light d-flex justify-content-between align-items-center",
        style = "border-bottom: none; padding: 15px 20px;",
        tags$span(
          style = "font-size: 20px; font-weight: bold; color: #0b0c0c;",
          "RISE Regional Hub Profile Workspace"
        ),
        actionButton(
          ns("back_to_hubs_search"),
          "Return to Hubs Directory",
          class = "btn btn-outline-dark btn-sm",
          icon = icon("arrow-left")
        )
      ),
      bslib::card_body(
        style = "padding: 10px 20px 20px 20px;",
        div(
          style = "display: flex; gap: 1.5rem; align-items: flex-end; flex-wrap: wrap;",
          textInput(
            ns("hub_name_edit"),
            "Modify Active Regional Hub Corporate Title Name:",
            width = "450px"
          ),
          actionButton(
            ns("save_hub_details"),
            "Update Hub Name Spec",
            class = "btn btn-primary",
            icon = icon("save"),
            style = "margin-bottom: 15px; font-weight: bold;"
          )
        )
      )
    ),

    uiOutput(ns("hub_stats_boxes")),
    br(),

    fluidRow(
      bslib::card(
        style = "margin-bottom: 25px;",
        bslib::card_header(
          class = "bg-secondary text-white d-flex justify-content-between align-items-center",
          tags$span(
            style = "font-weight: bold;",
            "Entities Supported by Hub"
          ),
          checkboxInput(
            ns("include_inactive_support"),
            "Show Inactive",
            value = FALSE
          )
        ),
        div(
          style = "padding: 15px; height: 380px; overflow-y: auto;",
          DT::dataTableOutput(ns("hub_support_schools_table"))
        ),
        tags$div(
          class = "govuk-hint",
          style = "padding: 0 15px 15px 15px; font-size: 13px;",
          "💡 Double-click any row to jump directly to its details."
        )
      )
    ),
    fluidRow(
      bslib::card(
        style = "margin-bottom: 25px;",
        bslib::card_header(
          class = "bg-secondary text-white d-flex justify-content-between align-items-center",
          tags$span(
            style = "font-weight: bold;",
            "Entities Supporting Delivery"
          ),
          checkboxInput(
            ns("include_inactive_leads"),
            "Show Inactive",
            value = FALSE
          )
        ),
        div(
          style = "padding: 15px; height: 380px; overflow-y: auto;",
          DT::dataTableOutput(ns("hub_lead_providers_table"))
        ),
        tags$div(
          class = "govuk-hint",
          style = "padding: 0 15px 15px 15px; font-size: 13px;",
          "💡 Double-click any row to jump directly to its details."
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
            "1. Regional Support Categories (Cohorts)"
          ),
          actionButton(
            ns("add_support_type"),
            "Add Category / Cohort",
            icon = icon("plus"),
            class = "btn btn-success btn-sm"
          )
        ),
        div(
          style = "padding: 15px; min-height: 350px; overflow-y: auto;",
          DT::dataTableOutput(ns("support_types_table"))
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
