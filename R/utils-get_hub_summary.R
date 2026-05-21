utils_get_hub_summary <- function(selected_hub_row) {
  tibble::tibble(
    `Hub Name` = selected_hub_row$HubName,
    `Active Support Episodes` = selected_hub_row$ActiveSupportEpisodes,
    `Schools Supported` = selected_hub_row$SchoolsSupported
  )
}
utils_get_school_list_with_hubs <- function() {
  schools <- db_get_school_list()

  active_support <- tryCatch(
    {
      db_ruh_get_support_schools() |>
        dplyr::filter(ruhs_active == 1) |>
        dplyr::select(URN = ruhs_urn) |>
        dplyr::distinct() |>
        dplyr::mutate(HasHubSupport = "Yes")
    },
    error = function(e) {
      data.frame(URN = numeric(), HasHubSupport = character())
    }
  )

  schools |>
    dplyr::left_join(active_support, by = "URN") |>
    dplyr::mutate(HasHubSupport = tidyr::replace_na(HasHubSupport, "No"))
}
