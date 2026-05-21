safe_input <- function(
  field,
  value,
  column_types,
  sigchange_types = NULL,
  giaschange_types = NULL
) {
  conf <- dauPortalTools::get_config()

  val <- if (is.null(value) || length(value) == 0) "" else value

  label_base <- if (
    !is.null(conf$friendly_names) &&
      field %in% names(conf$friendly_names)
  ) {
    conf$friendly_names[[field]]
  } else {
    field
  }

  is_required <- field %in% (conf$required_fields %||% character())

  label <- if (is_required) {
    HTML(paste0(
      "<strong>",
      label_base,
      " <span style='color:red'>*</span></strong>"
    ))
  } else {
    label_base
  }

  type <- tolower(
    if (!is.null(column_types) && field %in% names(column_types)) {
      column_types[[field]]
    } else {
      "varchar"
    }
  )

  if (field %in% c("sig_change_id", "URN")) {
    return(read_only_text_input(field, label, safe_text_value(val)))
  }

  get_choices <- function(name) {
    conf$dropdowns[[name]] %||% character(0)
  }

  if (field == "application_type") {
    return(selectInput(
      field,
      label,
      get_choices("application_type"),
      selected = as.character(val)[1]
    ))
  }

  if (field == "application_escalated_to") {
    return(selectInput(
      field,
      label,
      get_choices("application_escalated_to"),
      selected = as.character(val)[1]
    ))
  }

  if (field == "decision") {
    return(selectInput(
      field,
      label,
      get_choices("decision"),
      selected = as.character(val)[1]
    ))
  }

  if (field == "decision_maker_grade") {
    return(selectInput(
      field,
      label,
      get_choices("decision_maker_grade"),
      selected = as.character(val)[1]
    ))
  }

  if (field == "type_of_sig_change_id" && !is.null(sigchange_types)) {
    choices <- setNames(
      as.character(sigchange_types$type_of_sig_change_id),
      sigchange_types$type_of_sig_change
    )
    return(selectInput(field, label, choices, selected = as.character(val)[1]))
  }

  if (field == "type_of_gias_change_id" && !is.null(giaschange_types)) {
    choices <- setNames(
      as.character(giaschange_types$type_of_gias_change_id),
      giaschange_types$type_of_gias_change
    )
    return(selectInput(field, label, choices, selected = as.character(val)[1]))
  }

  if (field %in% c("comments", "decision_comment", "giaschangedetail")) {
    return(textAreaInput(field, label, safe_text_value(val)))
  }

  if (grepl("date|time", type)) {
    return(ui_date_input(field, label, val))
  }

  if (type == "bit" || looks_bitish(val)) {
    logical_value <- isTRUE(
      tryCatch(coerce_to_bool(val), error = function(e) FALSE)
    )

    return(checkboxInput(field, label, logical_value))
  }

  textInput(field, label, safe_text_value(val))
}
