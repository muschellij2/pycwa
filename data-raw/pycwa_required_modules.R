## code to prepare `pycwa_required_modules` dataset goes here
# pycwa_required_modules = readLines("inst/acc/requirements.txt")
pycwa_required_modules = c(
             "joblib",
             "numpy",
             "scipy",
             "pandas",
             "scikit-learn" = "sklearn",
             "statsmodels"
)
usethis::use_data(pycwa_required_modules, overwrite = TRUE)
