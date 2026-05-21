test_that("db_update_close_app executes update", {
  executed <- FALSE

  expect_silent(
    db_update_close_app(
      sig_change_id = 12345,
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
    "Finished db_update_close_app"
  )
})
