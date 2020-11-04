file =  system.file("extdata", "ax3_testfile.cwa.gz", package = "pycwa")
xyz = c("x", "y", "z")
testthat::test_that("py_read_cwa consistent", {
  skip_python()
  df = py_read_cwa(file)
  res = colMeans(df[, xyz])
  testthat::expect_equal(
    res,
    c(x = 0.773731466666667, y = 0.128860533333333, z = 0.288421733333333)
  )
})

testthat::test_that("summary data giving same answer", {
  skip_python()
  res = py_convert_cwa(file)
  sums = activity_summary(res$epochFile, model_dir = tempdir())

  testthat::expect_equal(
    mean(sums$epochData$enmoTrunc),
    0.0242792
  )
  testthat::expect_equal(
    sums$labels,
    c("moderate", "sedentary", "sleep", "tasks-light", "walking")
  )
})




