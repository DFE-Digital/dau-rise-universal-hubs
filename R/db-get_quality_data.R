db_get_quality_data <- function(
  sig_change_id,
  db_get_query = utils_db_get_query
) {
  conn <- sql_manager("dit")
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  query <- glue_sql(
    "
    SELECT *
    FROM {utils_resolve_schema('db_schema_01a')}.[quality_list]
    WHERE record_id = {sig_change_id}
    ",
    .con = conn
  )

  db_get_query(conn, query)
}
