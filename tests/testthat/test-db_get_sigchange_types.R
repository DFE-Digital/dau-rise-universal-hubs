test_that("db_get_sigchange_types returns sigchange type list", {
  executed <- FALSE

  fake_df <- data.frame(
    type_of_sig_change_id = c(1, 2),
    type_of_sig_change = c("Conversion", "Amalgamation"),
    stringsAsFactors = FALSE
  )

  out <- db_get_sigchange_types(
    db_get_query = function(conn, query) {
      executed <<- TRUE
      fake_df
    }
  )

  # DB path exercised
  expect_true(executed)

  # Return shape and content
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
  expect_equal(out$type_of_sig_change[1], "Conversion")

  # Logging lifecycle completed
  expect_match(
    tail(.local_log, 1),
    "Finished db_get_sigchange_types"
  )
})
