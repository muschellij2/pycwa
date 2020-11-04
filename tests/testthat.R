library(testthat)
library(pycwa)

skip_python = function() {
  testthat::skip_if(!pycwa:::have_python_requirements(),
                    message = "Python, or Python module not installed")
}

testthat::test_check("pycwa")
