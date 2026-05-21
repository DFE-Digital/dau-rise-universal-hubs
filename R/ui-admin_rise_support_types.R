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
