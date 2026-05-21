db_get_school_list <- function(
  db_get_query = utils_db_get_query
) {
  log_event("Starting db_get_school_list")

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_get_school_list")
    },
    add = TRUE
  )

  query <- glue_sql(
    "
    SELECT *
    FROM {utils_resolve_schema('db_schema_01a')}.[school_list]
    ",
    .con = conn
  )

  db_get_query(conn, query)
}
