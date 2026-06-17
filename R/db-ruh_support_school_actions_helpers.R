#' Retrieve Tracked Logs of Completed School Actions
#'
#' Fetches historical audit interventions executed against specific receiving schools.
#'
#' @param ruhsa_id Integer scalar or `NULL`. Filter for unique instance log row.
#' @param ruhs_id Integer scalar or `NULL`. Filter logs targeting a specific support school instance.
#' @param db_get_query Function used to execute the query.
#' @return A `data.frame` of recorded real-world action instances.
#' @export
#' Get Support School Intervention Actions joined with Type catalogs
#' @export
db_ruh_get_support_school_actions <- function(
  ruhs_id,
  db_get_query = utils_db_get_query
) {
  log_event(glue::glue("Fetching Intervention Log for Support ID {ruhs_id}"))
  result <- data.frame()

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
    },
    add = TRUE
  )

  query <- glue_sql(
    "
    SELECT sa.[ruhsa_id], sa.[ruhs_id], sa.[ruha_id], sa.[ruhsa_date], sa.[ruhsa_comment],
           a.[ruha_name]
    FROM  {utils_resolve_schema('db_schema_01r')}.[ruh_support_school_actions] sa
    LEFT JOIN  {utils_resolve_schema('db_schema_01r')}.[ruh_actions] a 
      ON sa.[ruha_id] = a.[ruha_id]
    WHERE sa.[ruhs_id] = {as.integer(ruhs_id)}
    ORDER BY sa.[ruhsa_date] DESC, sa.[date_created] DESC;
    ",
    .con = conn
  )

  result <- db_get_query(conn, query)
  result
}

#' Log an Executed School Intervention Action
#'
#' Submits qualitative observation results via form entry and returns its new ID.
#'
#' @param ruhs_id Integer scalar. Receiving school index identity (ruhs_id).
#' @param ruha_id Integer scalar. Core operational blueprint assignment identity (ruha_id).
#' @param action_date Date scalar or String 'YYYY-MM-DD'. Calendar day of execution (ruhsa_date).
#' @param comment Character scalar. Field evaluation notes tracking outcomes (ruhsa_comment).
#' @param user_id Character scalar. Audit tracking user identification.
#' @param db_get_query Function used to execute the command and return data.
#' @return Integer scalar. The newly generated `ruhsa_id`.
#' @export
db_ruh_add_support_school_action <- function(
  ruhs_id,
  ruha_id,
  action_date,
  comment,
  user_id,
  db_get_query = utils_db_get_query
) {
  log_event("Starting db_ruh_add_support_school_action")

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_ruh_add_support_school_action")
    },
    add = TRUE
  )

  query <- glue_sql(
    "
    INSERT INTO  {utils_resolve_schema('db_schema_01r')}.[ruh_support_school_actions] (
      [ruhs_id], [ruha_id], [ruhsa_date], [ruhsa_comment], [date_created], [user_id_created]
    ) 
    OUTPUT INSERTED.[ruhsa_id]
    VALUES (
      {as.integer(ruhs_id)}, {as.integer(ruha_id)}, CAST({action_date} AS DATE), {comment}, SYSUTCDATETIME(), {user_id}
    );
  ",
    .con = conn
  )

  res <- db_get_query(conn, query)
  as.integer(res[[1]])
}

#' Update a Logged Support Action
#'
#' @param ruhsa_id Integer. The action log ID.
#' @param action_date Date. When it happened.
#' @param comment Character. Evaluation notes.
#' @param user_id Character.
#' @export
db_ruh_update_support_school_action <- function(
  ruhsa_id,
  action_date,
  comment,
  user_id,
  db_get_query = utils_db_get_query
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue_sql(
    "
    UPDATE  {utils_resolve_schema('db_schema_01r')}.[ruh_support_school_actions]
    SET [ruhsa_date] = {action_date},
        [ruhsa_comment] = {comment},
        [date_edited] = SYSUTCDATETIME(),
        [user_id_edited] = {user_id}
    WHERE [ruhsa_id] = {ruhsa_id};
  ",
    .con = conn
  )

  db_get_query(conn, query)
}
