#' Heuristic: does a value look like a SQL bit / boolean?
#'
#' Checks if the first element of `value` can be interpreted as a boolean, accepting
#' logicals, 0/1 numerics, and common character variants (e.g., "true"/"false",
#' "yes"/"no", "y"/"n", "0"/"1").
#'
#' @param value Any object; only `value[[1]]` is inspected.
#' @return Logical scalar `TRUE`/`FALSE`.
#' @examples
#' looks_bitish(TRUE); looks_bitish(0); looks_bitish("yes"); looks_bitish("N")
#' @seealso [coerce_to_bool()]
#' @keywords internal
looks_bitish <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(FALSE)
  }
  v <- value[[1]]
  if (is.logical(v)) {
    return(TRUE)
  }
  if (is.numeric(v)) {
    return(!is.na(v) & v %in% c(0, 1))
  }
  if (is.factor(v)) {
    v <- as.character(v)
  }
  if (is.character(v)) {
    lv <- tolower(trimws(v))
    return(
      lv %in% c("0", "1", "true", "false", "t", "f", "yes", "no", "y", "n")
    )
  }
  FALSE
}

#' Coerce a single value to logical
#'
#' Coerces a single value (logical, numeric, factor, or character) to `TRUE`/`FALSE`
#' using common conventions: `1/0`, `"true"/"false"`, `"yes"/"no"`, `"y"/"n"`.
#' Non-matches return `FALSE`.
#'
#' @param value A length-1 value to coerce.
#' @return A logical scalar. Unrecognized inputs yield `FALSE`.
#' @examples
#' coerce_to_bool("Y"); coerce_to_bool(1); coerce_to_bool("no"); coerce_to_bool(7)
#' @seealso [looks_bitish()]
#' @export
coerce_to_bool <- function(value) {
  if (is.logical(value) && length(value) == 1 && !is.na(value)) {
    return(value)
  }
  if (is.numeric(value) && length(value) == 1) {
    return(value == 1)
  }
  if (is.factor(value) && length(value) == 1) {
    value <- as.character(value)
  }
  if (is.character(value) && length(value) == 1) {
    lv <- tolower(trimws(value))
    if (lv %in% c("true", "t", "yes", "y", "1")) {
      return(TRUE)
    }
    if (lv %in% c("false", "f", "no", "n", "0")) return(FALSE)
  }
  FALSE
}
