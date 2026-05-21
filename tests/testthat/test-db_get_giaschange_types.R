test_that("db_get_giaschange_types returns GIAS change types", {
  fake_df <- data.frame(
    type_of_gias_change_id = c(1, 2),
    type_of_gias_change = c("Establishment status", "Phase change"),
    stringsAsFactors = FALSE
  )

  out <- db_get_giaschange_types(
    db_get_query = function(conn, query) {
      fake_df
    }
  )

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
  expect_equal(out$type_of_gias_change[1], "Establishment status")
  expect_match(tail(.local_log, 1), "Finished db_get_giaschange_types")
})
