#' Retrieve school list enriched with active RISE hub support
#'
#' @description
#' Returns a school-level dataset indicating whether each school
#' is currently receiving RISE hub support.
#'
#' The function:
#' - Starts from the canonical school list
#' - Joins active hub support episodes
#' - Adds schools that have hub support but are missing from
#'   the base school list (outer-join behaviour)
#'
#' @return
#' A data.frame with one row per school and a `HasHubSupport` flag.
#'
#' @author Ben7 Smith

utils_get_school_list_with_hubs <- function() {
  school_list <- db_get_school_list()

  hub_flag <- db_get_hub_support_list() |>
    dplyr::filter(ruhs_active == 1) |>
    dplyr::distinct(URN) |>
    dplyr::mutate(HasHubSupport = "Yes")

  df <- school_list |>
    dplyr::left_join(hub_flag, by = "URN") |>
    dplyr::mutate(
      HasHubSupport = dplyr::coalesce(HasHubSupport, "No")
    )

  missing_schools <- hub_flag |>
    dplyr::anti_join(school_list, by = "URN") |>
    dplyr::transmute(
      URN,
      schoolname = NA_character_,
      schooltype = NA_character_,
      phase = NA_character_,
      gor = NA_character_,
      la = NA_character_,
      openstatus = NA_character_,
      trustid = NA_character_,
      trustname = NA_character_,
      trustregion = NA_character_,
      HasHubSupport = "Yes"
    )

  dplyr::bind_rows(df, missing_schools)
}
