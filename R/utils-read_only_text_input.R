read_only_text_input <- function(inputId, label, value) {
  tagList(
    tags$div(
      style = "margin-bottom: 0.5em;",
      tags$label(
        `for` = inputId,
        label,
        style = "margin-bottom: 0px; display: block; font-weight: bold;"
      ),
      tags$input(
        id = inputId,
        type = "text",
        class = "form-control readonly-field",
        value = value,
        readonly = NA,
        style = "background-color: #f5f5f5; font-style: italic; border: 1px solid #ccc; margin-top: 2px;"
      )
    )
  )
}
