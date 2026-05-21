#' Null-coalescing infix operator
#'
#' Returns `x` if it is not `NULL`, otherwise returns `y`.
#'
#' @name %||%
#' @param x Any R object; typically a possibly-`NULL` value.
#' @param y Fallback value returned when `x` is `NULL`.
#' @return `x` when not `NULL`, otherwise `y`.
#' @examples
#' "a" %||% "b"    # "a"
#' NULL %||% "b"   # "b"
#' @keywords internal
#' @rdname null_coalesce

`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

#' Null-or-empty-string coalescing infix operator
#'
#' Returns `a` if it is neither `NULL` nor an empty string (via `nzchar()`), otherwise returns `b`.
#'
#' @name %|||%
#' @param a A scalar character or any object convertible to character.
#' @param b Fallback value returned when `a` is `NULL` or empty (`""`).
#' @return `a` when non-empty and not `NULL`, otherwise `b`.
#' @examples
#' "x" %|||% "y"   # "x"
#' "" %|||% "y"    # "y"
#' NULL %|||% "y"  # "y"
#' @keywords internal
#' @rdname null_or_empty_coalesce

`%|||%` <- function(a, b) {
  if (!is.null(a) && nzchar(a)) a else b
}
