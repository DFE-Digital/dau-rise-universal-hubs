#' Translate input names using a reverse lookup
#'
#' Maps element names of `input_list` through a lookup vector `reverse_friendly_names`,
#' if present. Empty, `NULL`, or `NA` names are converted to `""`. Names with no
#' match are left as-is.
#'
#' @param input_list A named list whose names will be translated.
#' @details
#' Expects a mapping object `reverse_friendly_names` to exist in scope, typically a
#' named character vector where names are friendly labels and values are canonical names.
#'
#' @return The same list with possibly modified names.
#' @examples
#' reverse_friendly_names <- c("Date of birth" = "dob", "School" = "school_name")
#' translate_input_names(list("Date of birth" = "2001-01-01", x = 1))
#' @seealso [base::names()], [vapply()]
#' @export
translate_input_names <- function(input_list) {
  out <- input_list
  nms <- names(out)

  if (is.null(nms)) {
    return(out)
  }

  map_one <- function(n) {
    if (is.null(n) || is.na(n) || !nzchar(n)) {
      return(n %||% "")
    }

    val <- reverse_friendly_names[n]

    if (length(val) == 0 || is.na(val)) {
      n
    } else {
      unname(val)
    }
  }

  new_names <- vapply(nms, map_one, FUN.VALUE = character(1))
  names(out) <- new_names
  out
}
