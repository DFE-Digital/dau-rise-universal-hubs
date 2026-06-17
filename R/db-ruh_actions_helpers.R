#' Retrieve Action Configuration Catalog Definitions
#'
#' Returns action profiles configurable per Hub and Support Framework track.
#'
#' @param ruha_id Integer scalar or `NULL`. Optional specific master item filter.
#' @param db_get_query Function used to execute the query.
#' @return A `data.frame` containing master workflow items.
#' @export
db_ruh_get_actions <- function(
  ruha_id = NULL,
  db_get_query = utils_db_get_query
) {
  log_event("Starting db_ruh_get_actions")

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event(glue::glue(
        "Finished db_ruh_get_actions (rows_returned = {nrow(result)})"
      ))
    },
    add = TRUE
  )

  action_filter <- if (!is.null(ruha_id)) {
    glue_sql("WHERE ruha_id = {ruha_id}", .con = conn)
  } else {
    DBI::SQL("")
  }

  query <- glue_sql(
    "
  SELECT 
    a.[ruha_id], 
    a.[ruhb_id], 
    a.[ruht_id],
    ISNULL(h.[ruhb_name], 'Global Framework Scope') AS [hub_name],
    ISNULL(t.[ruht_name], 'Unassigned / General') AS [support_type_name],
    a.[ruha_name], 
    a.[ruha_description],
    a.[date_created], 
    a.[user_id_created], 
    a.[date_edited], 
    a.[user_id_edited]
  FROM  {utils_resolve_schema('db_schema_01r')}.[ruh_actions] a
  LEFT JOIN  {utils_resolve_schema('db_schema_01r')}.[ruh_hubs] h 
    ON a.[ruhb_id] = h.[ruhb_id]
  LEFT JOIN  {utils_resolve_schema('db_schema_01r')}.[ruh_support_types] t 
    ON a.[ruht_id] = t.[ruht_id]
  {action_filter};
  ",
    .con = conn
  )
  result <- db_get_query(conn, query)
  result
}

#' Add a Pre-Configured Action Definition
#'
#' Inserts an actionable task profile template via UI forms and returns its new ID.
#'
#' @param hub_id Integer scalar. Target Hub framework domain (ruhb_id).
#' @param ruht_id Integer scalar. Categorized intervention track context (ruht_id).
#' @param action_name Character scalar. Structured string name of task milestone (ruha_name).
#' @param description Character scalar. Instructions regarding deployment metrics (ruha_description).
#' @param user_id Character scalar. Audit accountability identifier.
#' @param db_get_query Function used to execute the command and return data.
#' @return Integer scalar. The newly generated `ruha_id`.
#' @export
#' Add a Pre-Configured Action Definition
#'
#' Inserts an actionable task profile template via UI forms and returns its new ID.
#'
#' @param hub_id Integer scalar. Target Hub framework domain (ruhb_id).
#' @param ruht_id Integer scalar. Categorized intervention track context (ruht_id).
#' @param action_name Character scalar. Structured string name of task milestone (ruha_name).
#' @param description Character scalar. Instructions regarding deployment metrics (ruha_description).
#' @param user_id Character scalar. Audit accountability identifier.
#' @param db_get_query Function used to execute the command and return data.
#' @return Integer scalar. The newly generated `ruha_id`.
#' @export
db_ruh_add_action <- function(
  hub_id,
  ruht_id,
  action_name,
  description,
  user_id,
  db_get_query = utils_db_get_query
) {
  log_event("Starting db_ruh_add_action")

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_ruh_add_action")
    },
    add = TRUE
  )

  query <- glue_sql(
    "
    INSERT INTO  {utils_resolve_schema('db_schema_01r')}.[ruh_actions] (
      [ruhb_id], 
      [ruht_id], 
      [ruha_name], 
      [ruha_description], 
      [date_created], 
      [user_id_created]
    ) 
    OUTPUT INSERTED.[ruha_id]
    VALUES (
      {as.integer(hub_id)}, 
      {as.integer(ruht_id)}, 
      {action_name}, 
      {description}, 
      SYSUTCDATETIME(), 
      {user_id}
    );
  ",
    .con = conn
  )

  res <- db_get_query(conn, query)
  as.integer(res[[1]])
}

#' Update an Existing Action Catalog Item
#'
#' @param ruha_id Integer. The ID of the action to update.
#' @param action_name Character. The updated title.
#' @param description Character. Updated guidelines.
#' @param user_id Character. User tracking.
#' @export
db_ruh_update_action <- function(
  ruha_id,
  hub_id, # Added parameter
  ruht_id, # Added parameter
  action_name,
  description,
  user_id,
  db_get_query = utils_db_get_query
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue_sql(
    "
    UPDATE {utils_resolve_schema('db_schema_01r')}.[ruh_actions]
    SET [ruhb_id] = {hub_id},
        [ruht_id] = {ruht_id},
        [ruha_name] = {action_name}, 
        [ruha_description] = {description}, 
        [date_edited] = SYSUTCDATETIME(), 
        [user_id_edited] = {user_id}
    WHERE [ruha_id] = {ruha_id};
  ",
    .con = conn
  )

  db_get_query(conn, query)
}
