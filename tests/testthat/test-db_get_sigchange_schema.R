test_that("db_get_sigchange_schema returns tracker column metadata", {
  executed <- FALSE

  fake_df <- data.frame(
    COLUMN_NAME = c("sig_change_id", "URN"),
    DATA_TYPE = c("int", "nvarchar"),
    stringsAsFactors = FALSE
  )

  out <- db_get_sigchange_schema(
    db_get_query = function(conn, query) {
      executed <<- TRUE
      fake_df
    }
  )

  # ---- behaviour assertions ----
  expect_true(executed)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)

  # ---- content assertions ----
  expect_equal(out$COLUMN_NAME[1], "sig_change_id")
  expect_equal(out$DATA_TYPE[2], "nvarchar")

  # ---- lifecycle/logging assertion ----
  expect_match(
    tail(.local_log, 1),
    "Finished db_get_sigchange_schema"
  )
})
