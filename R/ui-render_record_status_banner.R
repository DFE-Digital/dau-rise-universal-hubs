render_record_status_banner <- function(
  quality = NULL,
  record,
  date_col = "Date Identified",
  overdue_days = 30L
) {
  banner <- function(colour, title, text = NULL) {
    HTML(
      paste0(
        "<strong class='govuk-tag govuk-tag--",
        colour,
        "'>",
        title,
        "</strong>",
        if (!is.null(text)) paste0("<p>", text, "</p>") else ""
      )
    )
  }

  is_completed <- isTRUE(coerce_to_bool(record[["all_actions_completed"]]))
  is_withdrawn <- isTRUE(coerce_to_bool(record[["withdrawn"]]))
  no_quality <- is.null(quality) || nrow(quality) == 0

  if (!is_withdrawn && no_quality) {
    return(tagList(
      banner(
        "green",
        "Significant Change Completed"
      )
    ))
  }

  if (is_withdrawn && no_quality) {
    return(tagList(
      banner(
        "green",
        "Significant Change Withdrawn"
      )
    ))
  }

  if (!(date_col %in% names(quality))) {
    if (is_withdrawn) {
      return(tagList(
        banner(
          "yellow",
          "Significant Change Withdrawn with Errors",
          "Waiting on quality issues being resolved."
        )
      ))
    }

    return(tagList(
      banner(
        "yellow",
        "Significant Change Completed with Errors",
        "Waiting on quality issues being resolved."
      )
    ))
  }

  raw_dates <- quality[[date_col]]
  dates <- as_Date_vec(raw_dates)

  dates[is_sentinel_1900(dates)] <- NA

  if (all(is.na(dates))) {
    min_date <- as.Date(NA)
  } else {
    min_date <- suppressWarnings(min(dates, na.rm = TRUE))
    if (is.infinite(min_date)) min_date <- as.Date(NA)
  }

  overdue_threshold <- Sys.Date() - as.integer(overdue_days)
  overdue <- !is.na(min_date) && min_date < overdue_threshold

  if (is_withdrawn) {
    if (overdue) {
      return(tagList(
        banner(
          "red",
          "Significant Change Withdrawn with Errors",
          "Waiting on quality issues being resolved, with one or more over 30 days old."
        )
      ))
    }

    return(tagList(
      banner(
        "yellow",
        "Significant Change Withdrawn with Errors",
        "Waiting on quality issues being resolved."
      )
    ))
  }

  if (overdue) {
    return(tagList(
      banner(
        "red",
        "Significant Change Completed with Errors",
        "Waiting on quality issues being resolved, with one or more over 30 days old."
      )
    ))
  }

  return(tagList(
    banner(
      "yellow",
      "Significant Change Completed with Errors",
      "Waiting on quality issues being resolved."
    )
  ))
}
