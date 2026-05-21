test_that("db_update_sigchange_type executes update", {
  executed <- FALSE

  expect_silent(
    db_update_sigchange_type(
      id = 7,
      name = "Updated sig change type",
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
    "Finished db_update_sigchange_type"
  )
})
