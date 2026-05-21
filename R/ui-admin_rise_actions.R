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
