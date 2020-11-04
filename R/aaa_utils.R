have_python_requirements = function(
  packages = pycwa::pycwa_required_modules
) {
  res = check_python_requirements(packages)
  if (any(!res)) {
    no_pkg = names(res)[!res]
    tfile = tempfile()
    dput(no_pkg, file = tfile)
    np = readLines(tfile)
    msg = paste0(paste(no_pkg, collapse = ", "),
                 " packages not found, please try",
                 " to install using reticulate::py_install(",
                 np, ")\n",
                 "pycwa may not work")
    warning(msg)
  }
  return(all(res))
}

check_python_requirements = function(
  packages = pycwa::pycwa_required_modules
) {
  if (!reticulate::py_available(initialize = TRUE)) {
    return(FALSE)
  }
  n = names(packages)
  names(packages)[n == ""] = packages[n == ""]
  sapply(packages, reticulate::py_module_available)
  res = sapply(packages, reticulate::py_module_available)
  return(res)
}

install_python_requirements = function(
  packages = pycwa::pycwa_required_modules,
  force = FALSE
) {
  res = check_python_requirements(packages = packages)
  if (any(!res) || force) {
    no_pkg = names(res)[!res]
    reticulate::py_install(no_pkg, channel = c("defaults", "conda-forge", "bioconda"))
  }
  res = check_python_requirements(packages = packages)
  return(res)
}

cp_java = function() {
  x = system.file("acc", "java", "AccelerometerParser.class", package = "pycwa")
  if (!file.exists(x)) {
    java_dir = system.file("acc", "java", package = "pycwa")
    java_dir = normalizePath(java_dir, winslash = "/")
    jar =  paste0(java_dir, "/JTransforms-3.1-with-dependencies.jar")
    args = c("-cp", jar, paste0(java_dir, "/*.java"))
    java = Sys.which("javac")
    if (!file.exists(java)) {
      warning("javac is required for py_convert_cwa, cannot find java in path")
    }
    system2("javac", args = args)
    # try(system2("javac.exe", args = args))
  }
}
