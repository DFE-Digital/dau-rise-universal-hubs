test_that("db_add_giaschange_type executes DB insert", {
  executed <- FALSE

  expect_silent(
    db_add_giaschange_type(
      name = "New GIAS change type",
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
    "Finished db_add_giaschange_type"
  )
})
