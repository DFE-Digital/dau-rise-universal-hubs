#' Clean a value to a scalar
#'
#' Drops lists, reduces multi-length atomic vectors to the first element, and
#' returns the input otherwise.
#'
#' @param v Any R object.
#' @return `NULL` if `v` is a list; `v[1]` if `length(v) > 1`; otherwise `v`.
#' @examples
#' clean_scalar(list(a = 1))    # NULL
#' clean_scalar(c(10, 20))      # 10
#' clean_scalar("x")            # "x"
#' @keywords internal

clean_scalar <- function(v) {
  if (is.list(v)) {
    return(NULL)
  }
  if (length(v) > 1) {
    return(v[1])
  }
  v
}

#' Normalize a value for comparison based on a field type
#'
#' Coerces `v` according to the declared column type for `field`. Supported types:
#' `"date"`, `"datetime"`, `"smalldatetime"` → converted to `Date`; `"bit"` → logical;
#' other types → converted to character. Empty-ish values become `NULL`.
#'
#' @param field Character scalar, the field (column) name.
#' @param v The value to normalize.
#' @details
#' Expects a `column_types` object in scope: a named character vector of field types.
#' Missing types default to `"varchar"`. Empty-ish values are detected by [is_emptyish()].
#' Dates parsed via [to_date()]; bit/logical coercion via [coerce_to_bool()].
#'
#' @return A normalized value suitable for `identical()` comparison (e.g., `Date`, `logical`, or `character`), or `NULL`.
#' @examples
#' column_types <- c(start_date = "date", active = "bit", notes = "varchar")
#' normalize_for_compare("start_date", "2024-12-01")
#' normalize_for_compare("active", "yes")
#' normalize_for_compare("notes", 123)
#' @seealso [to_date()], [is_emptyish()], [coerce_to_bool()]
#' @export
normalize_for_compare <- function(value, field, column_types) {
  # ---- Get type safely ----
  if (!is.null(column_types) && field %in% names(column_types)) {
    tp <- tolower(column_types[[field]])
  } else {
    tp <- "varchar"
  }

  # ---- Empty values → NULL ----
  if (
    is.null(value) ||
      length(value) == 0 ||
      identical(value, "") ||
      all(is.na(value))
  ) {
    return(NULL)
  }

  # ---- Date types ----
  if (tp %in% c("date", "datetime", "smalldatetime")) {
    d <- tryCatch(
      as.Date(value),
      error = function(e) NA
    )

    if (is.na(d)) {
      return(NULL)
    }

    return(d)
  }

  # ---- Boolean / bit ----
  if (tp == "bit") {
    return(isTRUE(coerce_to_bool(value)))
  }

  # ---- Default ----
  as.character(value)
}


#' Detect per-field changes between two records
#'
#' Compares `original` and `current` (lists or list-like) on the intersection of
#' their names (optionally intersected with `valid_columns` if present). Values are
#' normalized via [normalize_for_compare()] to reduce spurious diffs.
#'
#' @param original A named list representing the baseline/original state.
#' @param current A named list representing the current/updated state.
#' @details
#' If an object `valid_columns` exists in scope, only those fields are considered.
#' Each field is preprocessed with [clean_scalar()] then normalized using
#' [normalize_for_compare()]. A field is considered changed if `!identical(old, new)`.
#'
#' @return A named list of changed fields with the *raw* new values from `current`.
#'         Returns an empty list if no changes are found.
#' @examples
#' column_types <- c(start_date = "date", active = "bit")
#' original <- list(start_date = "2024-01-01", active = "yes", notes = "A")
#' current  <- list(start_date = "01/01/2024", active = "no",  notes = "A")
#' detect_changes(original, current)
#' @seealso [normalize_for_compare()], [clean_scalar()]
#' @export
detect_changes <- function(original, current, column_types) {
  clean <- function(x) {
    if (is.null(x) || length(x) == 0) {
      return(NULL)
    }
    if (length(x) > 1) {
      return(x[1])
    }
    x
  }

  changed <- list()

  candidate_fields <- intersect(names(original), names(current))

  if (exists("valid_columns")) {
    candidate_fields <- intersect(candidate_fields, valid_columns)
  }

  candidate_fields <- candidate_fields[
    !is.na(candidate_fields) & nzchar(candidate_fields)
  ]

  log_event(paste("FIELDS CHECKED:", paste(candidate_fields, collapse = ", ")))

  for (field in candidate_fields) {
    old_val <- clean(original[[field]])
    new_val <- clean(current[[field]])

    old_n <- normalize_for_compare(old_val, field, column_types)
    new_n <- normalize_for_compare(new_val, field, column_types)

    log_event(paste0(
      "FIELD: ",
      field,
      " | OLD(raw)=",
      format(old_val),
      " | NEW(raw)=",
      format(new_val),
      " | OLD(norm)=",
      format(old_n),
      " | NEW(norm)=",
      format(new_n)
    ))

    if (!identical(old_n, new_n)) {
      changed[[field]] <- current[[field]]

      log_event(paste0(
        "CHANGED: ",
        field,
        " -> ",
        format(current[[field]])
      ))
    }
  }

  if (length(changed) == 0) {
    log_event("No changes detected")
  } else {
    log_event(paste(
      "Changed fields:",
      paste(names(changed), collapse = ", ")
    ))
  }

  changed
}
