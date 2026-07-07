#' Pivot and Cast Dynamic Event Responses
#'
#' Collapses vertical string response entries into a wide, flat analytical data frame
#' while dynamically casting characters back to native R types based on database rules.
#'
#' @param raw_responses A data.frame containing raw vertical event responses.
#' @return A wide, flattened \code{data.frame} with typed columns.
#' @importFrom stats reshape
#' @export
utils_ru_pivot_responses <- function(raw_responses) {
  log_event("Starting utils_ru_pivot_responses")

  if (is.null(raw_responses) || nrow(raw_responses) == 0) {
    return(data.frame())
  }

  required_cols <- c("ruev_id", "rueva_name", "ruevar_value", "rueva_rule_type")
  missing_cols <- setdiff(required_cols, names(raw_responses))
  if (length(missing_cols) > 0) {
    stop(paste(
      "Missing required reporting columns in input data frame:",
      paste(missing_cols, collapse = ", ")
    ))
  }

  type_map <- unique(raw_responses[, c("rueva_name", "rueva_rule_type")])

  wide_df <- reshape(
    raw_responses[, c("ruev_id", "rueva_name", "ruevar_value")],
    idvar = "ruev_id",
    timevar = "rueva_name",
    direction = "wide"
  )

  names(wide_df) <- gsub("^ruevar_value\\.", "", names(wide_df))

  for (i in seq_len(nrow(type_map))) {
    col_name <- type_map$rueva_name[i]
    rule_type <- type_map$rueva_rule_type[i]

    if (!col_name %in% names(wide_df)) {
      next
    }

    wide_df[[col_name]] <- switch(
      rule_type,
      "Integer" = as.integer(wide_df[[col_name]]),
      "Date" = as.Date(wide_df[[col_name]], format = "%Y-%m-%d"),
      "Boolean" = wide_df[[col_name]] == "1",
      as.character(wide_df[[col_name]])
    )
  }

  log_event("Finished utils_ru_pivot_responses successfully")
  return(wide_df)
}
