build_set_clause <- function(changes, column_types) {
  if (length(changes) == 0) {
    return(NULL)
  }

  parts <- vapply(
    names(changes),
    function(field) {
      v <- clean_scalar(changes[[field]])

      # ---- Safe type lookup ----
      tp <- if (!is.null(column_types) && field %in% names(column_types)) {
        tolower(column_types[[field]])
      } else {
        "varchar"
      }

      # ---- NULL handling ----
      if (is.null(v) || is_emptyish(v)) {
        return(paste0(field, " = NULL"))
      }

      # ---- Date handling ----
      if (inherits(v, "Date")) {
        return(
          paste0(
            field,
            " = '",
            format(v, "%Y-%m-%d"),
            "'"
          )
        )
      }

      # ---- Bit handling ----
      if (tp == "bit") {
        b <- ifelse(isTRUE(coerce_to_bool(v)), 1L, 0L)
        return(paste0(field, " = ", b))
      }

      # ---- Default (string) ----
      sv <- gsub("'", "''", as.character(v))
      return(paste0(field, " = '", sv, "'"))
    },
    FUN.VALUE = character(1)
  )

  paste(parts, collapse = ", ")
}
