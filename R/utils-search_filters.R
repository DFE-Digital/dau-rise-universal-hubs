#' Filter Entities Dataset Based on Search Criteria
#'
#' @param df Data frame containing the raw entities.
#' @param entity_type Character string identifying the tracking layer (School, Trust, etc.).
#' @param engaged_list Vector of IDs receiving ongoing framework support.
#' @param search_id Character string searching unique ID values.
#' @param search_name Character string searching title names.
#' @param support_status Character string selection ("All", "Yes Only", "No Only").
#' @return A filtered data frame.
apply_entity_search_filters <- function(
  df,
  entity_type,
  engaged_list,
  search_id,
  search_name,
  support_status
) {
  id_header <- dauPortalTools::db_resolve_entity_key_label(entity_type)
  name_header <- names(df)[2]

  df$`Receives Support` <- ifelse(
    df[[id_header]] %in% engaged_list,
    "Yes",
    "No"
  )

  if (nzchar(search_id %||% "")) {
    df <- dplyr::filter(
      df,
      grepl(search_id, .data[[id_header]], ignore.case = TRUE)
    )
  }

  if (nzchar(search_name %||% "")) {
    df <- dplyr::filter(
      df,
      grepl(search_name, .data[[name_header]], ignore.case = TRUE)
    )
  }

  if (nzchar(support_status %||% "") && !identical(support_status, "All")) {
    target_val <- ifelse(support_status == "Yes Only", "Yes", "No")
    df <- dplyr::filter(df, `Receives Support` == target_val)
  }

  df
}
