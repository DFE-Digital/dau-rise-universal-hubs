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
