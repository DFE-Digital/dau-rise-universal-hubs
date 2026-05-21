ui_quality_wrapper <- function(id) {
  ns <- NS(id)

  tagList(
    selectInput(
      ns("region_filter"),
      "Region:",
      choices = c(
        "All",
        "East Midlands",
        "East of England",
        "London",
        "North East",
        "North West",
        "South East",
        "South West",
        "West Midlands",
        "Yorkshire and the Humber"
      ),
      selected = "All"
    ),

    selectInput(
      ns("with_rcs_filter"),
      "With RCS?",
      choices = c("All", "Yes" = 1, "No" = 0),
      selected = "All"
    ),

    DTOutput(ns("filtered_table"))
  )
}
