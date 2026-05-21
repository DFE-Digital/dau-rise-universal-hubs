#' Detect sentinel date "1900-01-01"
#'
#' Returns `TRUE` where `d` equals the sentinel `as.Date("1900-01-01")`.
#'
#' @param d A `Date` vector.
#' @return A logical vector of the same length as `d`.
#' @examples
#' is_sentinel_1900(as.Date(c("1900-01-01", "2024-01-01")))
#' @keywords internal

is_sentinel_1900 <- function(d) {
  inherits(d, "Date") && identical(d, as.Date("1900-01-01"))
}
