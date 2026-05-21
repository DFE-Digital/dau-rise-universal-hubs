utils_prepare_hub_schools_table <- function(df) {
  df |>
    dplyr::rename(
      `School Name` = EstablishmentName,
      `Region` = "GOR (name)",
      `Active Support?` = ruhs_active
    ) |>
    dplyr::mutate(
      `Active Support?` = ifelse(`Active Support?`, "Yes", "No")
    )
}
