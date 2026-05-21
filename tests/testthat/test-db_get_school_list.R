test_that("db_get_school_list returns school data", {
  fake_df <- data.frame(
    urn = c(100001, 100002),
    school_name = c("Alpha School", "Beta School"),
    stringsAsFactors = FALSE
  )

  out <- db_get_school_list(
    db_get_query = function(conn, query) {
      fake_df
    }
  )

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
  expect_equal(out$school_name[1], "Alpha School")
  expect_match(tail(.local_log, 1), "Finished db_get_school_list")
})
