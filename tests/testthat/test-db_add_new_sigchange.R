test_that("db_add_new_sigchange inserts record and returns new ID", {
  fake_result <- data.frame(
    sig_change_id = 12345
  )

  out <- db_add_new_sigchange(
    urn = 100123,
    db_get_query = function(conn, query) {
      fake_result
    }
  )

  expect_s3_class(out, "data.frame")
  expect_equal(out$sig_change_id[1], 12345)
  expect_match(.local_log[length(.local_log)], "Finished db_add_new_sigchange")
})
