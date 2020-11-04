testthat::test_that("install requirements", {
  testthat::skip_on_cran()
  if (!pycwa:::have_python_requirements() &
      reticulate::py_available(initialize = FALSE)) {
    pycwa:::install_python_requirements()
    pycwa:::cp_java()
    testthat::expect_true(pycwa:::have_python_requirements())
  }
})
