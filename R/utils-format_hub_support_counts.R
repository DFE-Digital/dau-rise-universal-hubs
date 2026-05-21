utils_format_hub_support_counts <- function(counts_df) {
  tibble::tibble(
    Metric = c(
      "Schools currently supported",
      "Schools previously supported",
      "Schools ever supported"
    ),
    Value = c(
      counts_df$current_schools,
      counts_df$previous_schools,
      counts_df$ever_schools
    )
  )
}
