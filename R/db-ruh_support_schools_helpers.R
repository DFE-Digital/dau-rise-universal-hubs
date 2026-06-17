#' Get Support Schools with Metadata Lookups
#' @export
db_ruh_get_support_schools <- function(
  ruhs_id = NULL,
  hub_id = NULL,
  db_get_query = utils_db_get_query
) {
  log_event("Starting db_ruh_get_support_schools")
  result <- data.frame()

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event(glue::glue(
        "Finished db_ruh_get_support_schools (rows_returned = {nrow(result)})"
      ))
    },
    add = TRUE
  )

  conditions <- character()
  if (!is.null(ruhs_id)) {
    conditions <- c(
      conditions,
      glue_sql("s.[ruhs_id] = {ruhs_id}", .con = conn)
    )
  }
  if (!is.null(hub_id)) {
    conditions <- c(conditions, glue_sql("s.[ruhb_id] = {hub_id}", .con = conn))
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
    SELECT s.[ruhs_id], s.[ruhl_id], s.[ruhb_id], s.[ruht_id], s.[ruhs_urn], 
           s.[ruhs_dateactive], s.[ruhs_dateended], s.[ruhs_active], s.[ruhs_comment],
           s.[date_created], s.[user_id_created], s.[date_edited], s.[user_id_edited],
           h.[ruhb_name], 
           t.[ruht_name]
    FROM  {utils_resolve_schema('db_schema_01r')}.[ruh_support_schools] s
    LEFT JOIN  {utils_resolve_schema('db_schema_01r')}.[ruh_hubs] h 
      ON s.[ruhb_id] = h.[ruhb_id]
    LEFT JOIN  {utils_resolve_schema('db_schema_01r')}.[ruh_support_types] t 
      ON s.[ruht_id] = t.[ruht_id]
    {where_clause};
    ",
    .con = conn
  )

  result <- db_get_query(conn, query)
  result
}

#' Add a Blank Linked Support School Record
#'
#' Designed for Button Clicks. Initializes an active target school placeholder record and returns its new ID.
#'
#' @param hub_id Integer scalar. Structural Hub location (ruhb_id).
#' @param ruht_id Integer scalar. Categorization framework ID (ruht_id).
#' @param urn Integer scalar. Target school identity reference (ruhs_urn).
#' @param lead_school_id Integer scalar or `NULL`. Optional oversight school link (ruhl_id).
#' @param user_id Character scalar. User tracking metric.
#' @param db_get_query Function used to execute the command and return data.
#' @return Integer scalar. The newly generated `ruhs_id`.
#' @export
db_ruh_add_blank_support_school <- function(
  hub_id,
  ruht_id,
  urn,
  lead_school_id = NULL,
  user_id,
  date_start,
  comment = NULL,
  db_get_query = utils_db_get_query
) {
  log_event(glue::glue(
    "Adding explicit support school assignment for URN {urn}"
  ))
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  lead_val <- if (is.null(lead_school_id)) {
    DBI::SQL("NULL")
  } else {
    as.integer(lead_school_id)
  }

  query <- glue_sql(
    "
    INSERT INTO  {utils_resolve_schema('db_schema_01r')}.[ruh_support_schools] (
      [ruhl_id], [ruhb_id], [ruht_id], [ruhs_urn], 
      [ruhs_dateactive], [ruhs_active], [ruhs_comment], [date_created], [user_id_created]
    ) 
    OUTPUT INSERTED.[ruhs_id]
    VALUES (
      {lead_val}, {as.integer(hub_id)}, {as.integer(ruht_id)}, {as.integer(urn)}, 
      {date_start}, 1, {comment %||% DBI::SQL('NULL')}, SYSUTCDATETIME(), {user_id}
    );
    ",
    .con = conn
  )

  res <- db_get_query(conn, query)
  as.integer(res[[1]])
}

#' Update an Existing Linked Support School Record
#'
#' Writes edits back to the SQL database following sub-form modifications.
#'
#' @param ruhs_id Integer scalar. The primary key of the support record.
#' @param hub_id Integer scalar. Structural Hub location (ruhb_id).
#' @param ruht_id Integer scalar. Categorization framework ID (ruht_id).
#' @param lead_school_id Integer scalar or `NULL`. Optional oversight school link (ruhl_id).
#' @param date_active Date/Character scalar. Date support framework began.
#' @param date_ended Date/Character scalar or `NULL`. Date support ended.
#' @param is_active Integer/Logical scalar. Active status flag (1 or 0).
#' @param comment Character scalar or `NULL`. Descriptive metadata annotations.
#' @param user_id Character scalar. User tracking metric for tracking modifications.
#' @param db_get_query Function used to execute the command.
#' @export
db_ruh_update_support_school <- function(
  ruhs_id,
  hub_id,
  ruht_id,
  lead_school_id = NULL,
  date_active,
  date_ended = NULL,
  is_active,
  comment = NULL,
  user_id,
  db_get_query = utils_db_get_query
) {
  log_event(glue::glue("Updating support school record ID: {ruhs_id}"))
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  lead_val <- if (is.null(lead_school_id)) {
    DBI::SQL("NULL")
  } else {
    as.integer(lead_school_id)
  }
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
    UPDATE  {utils_resolve_schema('db_schema_01r')}.[ruh_support_schools]
    SET 
      [ruhl_id]           = {lead_val},
      [ruhb_id]           = {as.integer(hub_id)},
      [ruht_id]           = {as.integer(ruht_id)},
      [ruhs_dateactive]   = {date_active},
      [ruhs_dateended]    = {ended_val},
      [ruhs_active]       = {as.integer(is_active)},
      [ruhs_comment]      = {comm_val},
      [date_edited]       = SYSUTCDATETIME(),
      [user_id_edited]    = {user_id}
    WHERE [ruhs_id]       = {as.integer(ruhs_id)};
    ",
    .con = conn
  )

  db_get_query(conn, query)
  invisible(TRUE)
}

#' Get list of distinct URNs currently receiving Hub Support
#' @export
db_get_hub_support_urns <- function(db_get_query = utils_db_get_query) {
  log_event("Starting db_get_hub_support_urns")
  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_get_hub_support_urns")
    },
    add = TRUE
  )

  query <- glue_sql(
    "SELECT DISTINCT [ruhs_urn] AS URN FROM  {utils_resolve_schema('db_schema_01r')}.[ruh_support_schools];",
    .con = conn
  )
  db_get_query(conn, query)
}
