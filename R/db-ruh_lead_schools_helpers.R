#' Retrieve Lead School Records
#'
#' Returns operational lead schools with optional filters.
#'
#' @param ruhl_id Integer scalar or `NULL`. Optional Lead School ID filter.
#' @param hub_id Integer scalar or `NULL`. Optional Hub ID filter.
#' @param db_get_query Function used to execute the query.
#' @return A `data.frame` containing lead school data.
#' @export
#' Get Lead Schools with Metadata Lookups
#' @export
db_ruh_get_lead_schools <- function(
  ruhl_id = NULL,
  hub_id = NULL,
  db_get_query = utils_db_get_query
) {
  log_event("Starting db_ruh_get_lead_schools")
  result <- data.frame()

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event(glue::glue(
        "Finished db_ruh_get_lead_schools (rows_returned = {nrow(result)})"
      ))
    },
    add = TRUE
  )

  conditions <- character()
  if (!is.null(ruhl_id)) {
    conditions <- c(
      conditions,
      glue_sql("l.[ruhl_id] = {ruhl_id}", .con = conn)
    )
  }
  if (!is.null(hub_id)) {
    conditions <- c(conditions, glue_sql("l.[ruhb_id] = {hub_id}", .con = conn))
  }

  where_clause <- if (length(conditions) > 0) {
    glue_sql(
      "WHERE {DBI::SQL(paste(conditions, collapse = ' AND '))}",
      .con = conn
    )
  } else {
    DBI::SQL("")
  }

  query <- glue_sql(
    "
    SELECT l.[ruhl_id], l.[ruhb_id], l.[ruhl_urn], l.[ruhl_dateactive], l.[ruhl_dateended], 
           l.[ruhl_active], l.[ruhl_comment], l.[date_created], l.[user_id_created], l.[date_edited], l.[user_id_edited],
           h.[ruhb_name]
    FROM [Data_Insight_Team].[01_RISE].[ruh_lead_schools] l
    LEFT JOIN [Data_Insight_Team].[01_RISE].[ruh_hubs] h 
      ON l.[ruhb_id] = h.[ruhb_id]
    {where_clause};
    ",
    .con = conn
  )

  result <- db_get_query(conn, query)
  return(result)
}

#' Add a Linked Lead School Record
#' @export
db_ruh_add_blank_lead_school <- function(
  hub_id,
  urn,
  user_id,
  date_start,
  comment = NULL,
  db_get_query = utils_db_get_query
) {
  log_event(glue::glue("Adding explicit lead school context for URN {urn}"))
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue_sql(
    "
    INSERT INTO [Data_Insight_Team].[01_RISE].[ruh_lead_schools] (
      [ruhb_id], [ruhl_urn], [ruhl_dateactive], [ruhl_active], [ruhl_comment], [date_created], [user_id_created]
    ) 
    OUTPUT INSERTED.[ruhl_id]
    VALUES (
      {as.integer(hub_id)}, {as.integer(urn)}, {date_start}, 1, {comment %||% DBI::SQL('NULL')}, SYSUTCDATETIME(), {user_id}
    );
    ",
    .con = conn
  )

  res <- db_get_query(conn, query)
  as.integer(res[[1]])
}

#' Update an Existing Linked Lead School Record
#'
#' Writes edits back to the SQL database following lead school sub-form modifications.
#'
#' @param ruhl_id Integer scalar. The primary key of the lead record.
#' @param hub_id Integer scalar. Structural Hub location (ruhb_id).
#' @param date_active Date/Character scalar. Date school assumed a hub lead status.
#' @param date_ended Date/Character scalar or `NULL`. Date assignment ended.
#' @param is_active Integer/Logical scalar. Active status flag (1 or 0).
#' @param comment Character scalar or `NULL`. Descriptive tracking notes.
#' @param user_id Character scalar. User tracking metric for tracking modifications.
#' @param db_get_query Function used to execute the command.
#' @export
db_ruh_update_lead_school <- function(
  ruhl_id,
  hub_id,
  date_active,
  date_ended = NULL,
  is_active,
  comment = NULL,
  user_id,
  db_get_query = utils_db_get_query
) {
  log_event(glue::glue("Updating lead school record ID: {ruhl_id}"))
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  # Safe SQL mapping expressions for nullable variables
  ended_val <- if (is.null(date_ended) || date_ended == "") {
    DBI::SQL("NULL")
  } else {
    date_ended
  }
  comm_val <- if (is.null(comment) || !nzchar(trimws(comment))) {
    DBI::SQL("NULL")
  } else {
    comment
  }

  query <- glue_sql(
    "
    UPDATE [Data_Insight_Team].[01_RISE].[ruh_lead_schools]
    SET 
      [ruhb_id]           = {as.integer(hub_id)},
      [ruhl_dateactive]   = {date_active},
      [ruhl_dateended]    = {ended_val},
      [ruhl_active]       = {as.integer(is_active)},
      [ruhl_comment]      = {comm_val},
      [date_edited]       = SYSUTCDATETIME(),
      [user_id_edited]    = {user_id}
    WHERE [ruhl_id]       = {as.integer(ruhl_id)};
    ",
    .con = conn
  )

  db_get_query(conn, query)
  invisible(TRUE)
}

#' Get list of distinct URNs acting as a Lead Hub School
#' @export
db_get_hub_lead_urns <- function(db_get_query = utils_db_get_query) {
  log_event("Starting db_get_hub_lead_urns")
  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_get_hub_lead_urns")
    },
    add = TRUE
  )

  query <- glue_sql(
    "SELECT DISTINCT [ruhl_urn] AS URN FROM [Data_Insight_Team].[01_RISE].[ruh_lead_schools];",
    .con = conn
  )
  db_get_query(conn, query)
}
