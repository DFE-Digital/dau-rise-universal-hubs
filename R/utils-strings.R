#' Determine if a value is "empty-ish"
#'
#' Heuristic to treat `NULL`, zero-length, single `NA`, single trimmed empty string,
#' or literal `"NULL"` as empty.
#'
#' @param x Any R object.
#' @return Logical scalar.
#' @examples
#' is_emptyish(NULL); is_emptyish(character()); is_emptyish(NA)
#' is_emptyish("   "); is_emptyish("NULL")
#' @export
is_emptyish <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(TRUE)
  }
  if (isTRUE(is.na(x))) {
    return(TRUE)
  }
  if (inherits(x, "Date") && is_sentinel_1900(x)) {
    return(TRUE)
  }
  if (identical(x, "")) {
    return(TRUE)
  }
  FALSE
}


#' Safely coerce to non-NA character
#'
#' Converts empty-ish inputs (see [is_emptyish()]) to `""`, otherwise returns
#' `as.character(x)`.
#'
#' @param x Any R object.
#' @return A character scalar (empty string for empty-ish input).
#' @examples
#' safe_text_value(NA)     # ""
#' safe_text_value(" abc") # " abc"
#' @export
safe_text_value <- function(x) {
  if (is_emptyish(x)) "" else as.character(x)
}
