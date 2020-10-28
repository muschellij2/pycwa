## code to prepare `pycwa_required_modules` dataset goes here
pycwa_required_modules = readLines("inst/acc/requirements.txt")
usethis::use_data(pycwa_required_modules, overwrite = TRUE)
