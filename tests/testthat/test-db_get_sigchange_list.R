test_that("db_get_sigchange_list returns sigchange records", {
  fake_df <- data.frame(
    sig_change_id = c(1, 2),
    URN = c(100001, 100002),
    application_type = c("Change", "Change"),
    stringsAsFactors = FALSE
  )

  out <- db_get_sigchange_list(
    db_get_query = function(conn, query) {
      fake_df
    }
  )

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
  expect_equal(out$sig_change_id[1], 1)
  expect_match(
    tail(.local_log, 1),
    "Finished db_get_sigchange_list"
  )
})
