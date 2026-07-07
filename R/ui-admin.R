#' Build Dynamic Service Navigation Links by Role
#'
#' @param role Character scalar. The role profile of the authenticated user.
#' @return A named character vector of tracking navigation links.
#' @export
build_service_nav_links <- function(role) {
  role <- tolower(trimws(role %||% ""))

  base_links <- c(
    "Home" = "home",
    "Search Entities" = "search",
    "Hubs Search" = "hubs_search",
    "Events Search" = "events_master_catalog",
    "Support Help" = "support",
    "My Profile" = "user_menu"
  )

  if (role %in% c("admin", "regional_admin")) {
    base_links <- c(base_links, "Admin Dashboard" = "admin_dashboard")
  }

  return(base_links)
}

gias_change_type_modal_edit <- function(ns, row) {
  modalDialog(
    title = paste("Edit GIAS Change Type:", row$type_of_gias_change),
    easyClose = TRUE,
    footer = NULL,
    tagList(
      tags$label("ID"),
      tags$input(
        id = ns("edit_id"),
        name = ns("edit_id"),
        type = "text",
        class = "form-control",
        value = row$type_of_gias_change_id,
        readonly = "readonly"
      ),
      br(),

      textInput(
        ns("edit_name"),
        "Type of GIAS Change",
        value = row$type_of_gias_change
      ),

      actionButton(ns("save_edit"), "Save Changes", class = "btn-primary")
    )
  )
}

gias_change_type_modal_add <- function(ns) {
  modalDialog(
    title = "Add New GIAS Change Type",
    easyClose = TRUE,
    footer = NULL,
    tagList(
      textInput(ns("add_name"), "Type of GIAS Change"),
      actionButton(ns("save_new"), "Create", class = "btn-primary")
    )
  )
}
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
ui_rise_actions_admin <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Manage Action Blueprints"),
    p(
      "Configure actionable menu selections mapped to specific support tiers and hubs."
    ),
    actionButton(
      ns("add_new"),
      "Add New Action",
      class = "btn govuk-button"
    ),
    br(),
    DT::DTOutput(ns("table"))
  )
}

rise_action_modal_add <- function(ns) {
  modalDialog(
    title = "Add New Action Blueprint",
    tagList(
      textInput(ns("add_name"), "Action Name"),
      textAreaInput(ns("add_desc"), "Description", rows = 3),

      uiOutput(ns("add_hub_ui")),

      uiOutput(ns("add_type_ui"))
    ),
    easyClose = TRUE,
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("save_new"), "Create Action", class = "btn-primary")
    )
  )
}

rise_action_modal_edit <- function(ns, row) {
  modalDialog(
    title = paste("Edit Action:", row$ruha_name),
    tagList(
      textInput(ns("edit_name"), "Action Name", value = row$ruha_name),
      textAreaInput(
        ns("edit_desc"),
        "Description",
        value = row$ruha_description,
        rows = 3
      ),

      uiOutput(ns("edit_hub_ui")),
      uiOutput(ns("edit_type_ui"))
    ),
    easyClose = TRUE,
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("save_edit"), "Save Changes", class = "btn-primary")
    )
  )
}
ui_rise_support_types_admin <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Manage RISE Support Types"),
    p(
      "Define the high-level categories of support available across all hubs or specific to individual ones."
    ),
    actionButton(
      ns("add_new"),
      "Add New Support Type",
      class = "btn govuk-button"
    ),
    br(),
    DT::DTOutput(ns("table")),
    uiOutput(ns("modal_ui"))
  )
}

rise_support_type_modal_add <- function(ns, hubs_list) {
  modalDialog(
    title = "Add New Support Type",
    tagList(
      textInput(ns("add_name"), "Type Name"),
      textAreaInput(ns("add_desc"), "Description"),
      selectInput(
        ns("add_hub_id"),
        "Associated Hub Scope",
        choices = c("Leave blank (Global / Not Hub Specific)" = 0, hubs_list),
        selected = 0
      ),
      helpText(
        "Note: If this support type is universal across all regions, leave it unassigned."
      )
    ),
    easyClose = TRUE,
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("save_new"), "Create Support Type", class = "btn-primary")
    )
  )
}

rise_support_type_modal_edit <- function(ns, row, hubs_list) {
  modalDialog(
    title = paste("Edit Support Type:", row$ruht_name),
    tagList(
      textInput(ns("edit_name"), "Type Name", value = row$ruht_name),
      textAreaInput(
        ns("edit_desc"),
        "Description",
        value = row$ruht_description
      ),
      selectInput(
        ns("edit_hub_id"),
        "Associated Hub Scope",
        choices = c("Leave blank (Global / Not Hub Specific)" = 0, hubs_list),
        selected = row$ruhb_id
      )
    ),
    easyClose = TRUE,
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("save_edit"), "Save Changes", class = "btn-primary")
    )
  )
}
