#' Retrieve Support Type Records
#' @param hub_id Integer. The current Hub ID.
db_ruh_get_support_types <- function(
  hub_id = NULL,
  db_get_query = utils_db_get_query
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  where_clause <- if (!is.null(hub_id)) {
    glue_sql("WHERE ruhb_id IN (0, {hub_id})", .con = conn)
  } else {
    DBI::SQL("")
  }

  query <- glue_sql(
    "
    SELECT [ruht_id], [ruhb_id], [ruht_name], [ruht_description]
    FROM [Data_Insight_Team].[01_RISE].[ruh_support_types]
    {where_clause}
    ORDER BY [ruhb_id] ASC, [ruht_name] ASC;
  ",
    .con = conn
  )

  db_get_query(conn, query)
}

#' Add a New Support Type
db_ruh_add_support_type <- function(
  hub_id,
  name,
  description,
  user_id,
  db_get_query = utils_db_get_query
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue_sql(
    "
    INSERT INTO [Data_Insight_Team].[01_RISE].[ruh_support_types] (
      [ruhb_id], [ruht_name], [ruht_description], [date_created], [user_id_created]
    ) 
    OUTPUT INSERTED.[ruht_id]
    VALUES ({hub_id}, {name}, {description}, SYSUTCDATETIME(), {user_id});
  ",
    .con = conn
  )

  res <- db_get_query(conn, query)
  as.integer(res[[1]])
}

#' Update an Existing Support Type
#' @export
db_ruh_update_support_type <- function(
  ruht_id,
  name,
  description,
  hub_id,
  user_id
) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue_sql(
    "
    UPDATE [Data_Insight_Team].[01_RISE].[ruh_support_types]
    SET [ruht_name] = {name}, 
        [ruht_description] = {description}, 
        [ruhb_id] = {hub_id},
        [date_edited] = SYSUTCDATETIME(), 
        [user_id_edited] = {user_id}
    WHERE [ruht_id] = {ruht_id};
    ",
    .con = conn
  )

  DBI::dbExecute(conn, query)
}
