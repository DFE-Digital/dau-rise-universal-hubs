test_that("db_update_giaschange_type executes update", {
  executed <- FALSE

  expect_silent(
    db_update_giaschange_type(
      id = 42,
      name = "Updated GIAS change type",
      user = "ben.smith",
      db_execute = function(conn, query) {
        executed <<- TRUE
        invisible(1L)
      }
    )
  )

  expect_true(executed)
  expect_match(
    tail(.local_log, 1),
    "Finished db_update_giaschange_type"
  )
})
