#' Retrieve Hub Records
#'
#' Returns hub records from the database, with an optional filter for a specific hub ID.
#'
#' @param hub_id Integer scalar or `NULL`. Optional hub ID used to filter results.
#' @param db_get_query Function used to execute the query.
#' @return A `data.frame` containing hub records.
#' @export
db_ruh_get_hubs <- function(hub_id = NULL, db_get_query = utils_db_get_query) {
  log_event("Starting db_ruh_get_hubs")

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event(glue::glue(
        "Finished db_ruh_get_hubs (rows_returned = {nrow(result)})"
      ))
    },
    add = TRUE
  )

  hub_filter <- if (!is.null(hub_id)) {
    glue_sql("WHERE ruhb_id = {hub_id}", .con = conn)
  } else {
    DBI::SQL("")
  }

  query <- glue_sql(
    "
    SELECT [ruhb_id], [ruhb_name], [date_created], [user_id_created], [date_edited], [user_id_edited]
    FROM  {utils_resolve_schema('db_schema_01r')}.[ruh_hubs]
    {hub_filter};
  ",
    .con = conn
  )

  result <- db_get_query(conn, query)
  result
}

#' Add a New Hub Record
#'
#' @param hub_name Character scalar.
#' @param user_id Character scalar.
#' @export
db_ruh_add_hub <- function(
  hub_name,
  user_id,
  db_get_query = utils_db_get_query
) {
  log_event("Starting db_ruh_add_hub")

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_ruh_add_hub")
    },
    add = TRUE
  )

  query <- glue_sql(
    "
    INSERT INTO  {utils_resolve_schema('db_schema_01r')}.[ruh_hubs] (
      [ruhb_name],
      [user_id_created],
      [date_created]
    ) 
    OUTPUT INSERTED.[ruhb_id]
    VALUES (
      {hub_name}, 
      {user_id},
      SYSUTCDATETIME()
    );
    ",
    .con = conn
  )

  res <- db_get_query(conn, query)
  as.integer(res[[1]])
}

#' Retrieve Hub Performance Summary
#'
#' Returns a data frame of all hubs with aggregated metrics for active support
#' instances and unique support types currently being delivered.
#'
#' @param db_get_query Function used to execute the query (default: utils_db_get_query).
#' @return A data.frame with columns: ruhb_id, ruhb_name, schools_supported_active, support_types_count.
#' @export
db_ruh_get_hub_summary <- function(db_get_query = utils_db_get_query) {
  log_event("Starting db_ruh_get_hub_summary")

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_ruh_get_hub_summary")
    },
    add = TRUE
  )

  query <- glue_sql(
    "
    SELECT 
        h.[ruhb_id], 
        h.[ruhb_name] AS hub_name,
        COUNT(DISTINCT CASE WHEN s.[ruhs_active] = 1 THEN s.[ruhs_urn] END) AS schools_supported_active,
        COUNT(DISTINCT CASE WHEN l.[ruhl_active] = 1 THEN l.[ruhl_urn] END) AS lead_schools_active,
        COUNT(DISTINCT t.[ruht_id]) AS support_types_count
    FROM  {utils_resolve_schema('db_schema_01r')}.[ruh_hubs] h
    LEFT JOIN  {utils_resolve_schema('db_schema_01r')}.[ruh_support_schools] s 
        ON h.[ruhb_id] = s.[ruhb_id]
    LEFT JOIN  {utils_resolve_schema('db_schema_01r')}.[ruh_lead_schools] l
        ON h.[ruhb_id] = l.[ruhb_id]
    LEFT JOIN  {utils_resolve_schema('db_schema_01r')}.[ruh_support_types] t
        ON h.[ruhb_id] = t.[ruhb_id]
    GROUP BY 
        h.[ruhb_id], 
        h.[ruhb_name]
    ORDER BY 
        h.[ruhb_name] ASC;
    ",
    .con = conn
  )

  result <- db_get_query(conn, query)
  return(result)
}

db_ruh_update_hub <- function(hub_id, hub_name, user_id) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE))

  query <- glue_sql(
    "UPDATE  {utils_resolve_schema('db_schema_01r')}.[ruh_hubs]
     SET [ruhb_name] = {hub_name},
         [date_edited] = GETDATE(),
         [user_id_edited] = {user_id}
     WHERE [ruhb_id] = {hub_id}",
    .con = conn
  )

  DBI::dbExecute(conn, query)
}
