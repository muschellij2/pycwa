testthat::test_that("install requirements", {
  testthat::skip_on_cran()
  pycwa:::cp_java()
  acc_file = system.file("acc", "java", "AccelerometerParser.class",
                         package = "pycwa")
  testthat::expect_true(file.exists(acc_file))
  if (!pycwa:::have_python_requirements() &
      reticulate::py_available(initialize = FALSE)) {
    pycwa:::install_python_requirements()
    pycwa:::cp_java()
    testthat::expect_true(pycwa:::have_python_requirements())
  }
})


testthat::test_that("dl model", {
  testthat::skip_on_cran()

  res = pycwa::download_activity_model(outdir = tempdir())
  fname = "featureCols.txt"
  out = untar(res, exdir = tempdir(), files = fname)
  file = file.path(tempdir(), fname)
  x = readLines(file)
  cat(x, sep = ",")
  bad = tools::showNonASCII(file)
  testthat::expect_length(bad, 0)
  testthat::expect_true("zfft1" %in% x)
})
