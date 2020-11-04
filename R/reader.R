#' Read Axivity CWA file
#'
#' @param file the <.cwa/.cwa.gz> file to process
#'                      (e.g. sample.cwa.gz). If the file path contains
#'                      spaces,it must be enclosed in quote marks
#optional inputs
#' @param timeZone timezone in country/city format to
#' be used for daylight savings crossover check
#' @param timeShift  time shift to be applied, e.g.
#'     -15 will shift the device internal time by -15
#'     minutes. Not to be confused with timezone offsets.
#' @param startTime removes data before this
#'     time (local) in the final analysis
#' @param endTime removes data after this
#'     time (local) in the final analysis
#' @param epochPeriod length in seconds of a single epoch
#' @param sampleRate resample data to n Hz
#' @param useFilter Filter ENMO trunc values?
#' @param csvStartTime start time for csv file
#'     when time column is not available (e.g. 2020-01-01T00:01)
#' @param csvSampleRate sample rate for csv file
#'     when time column is not available in Hz, or samples/second
##### calibration parameters
#' @param skipCalibration  skip calibration?
#' @param calOffset accelerometer calibration offset
#' @param calSlope accelerometer calibration slope linking
#'     offset to temperature
#' @param calTemp mean temperature in degrees Celsius of
#'     stationary data for calibration
#' @param meanTemp  mean calibration temperature in degrees
#' @param stationaryStd stationary mg threshold
##### circadian rhythm options
#' @param outputFolder folder for all of the output files
#' @param verbose enable verbose logging?
##### calling helper processes and conducting multi-threading
#' @param rawDataParser type of parser to use.  If \code{omconvert}, this
#' must be installed in your environment path
#' @param javaHeapSpace amount of heap space (in MB) allocated to the
#' java subprocesses, useful for limiting RAM usage
#'
#' @export
#' @return A list of files for the output
#' @examples
#' reticulate::py_config()
#' file =  system.file("extdata", "ax3_testfile.cwa.gz", package = "pycwa")
#' if (pycwa:::have_python_requirements()) {
#'     res = pycwa::py_convert_cwa(file, verbose = 2)
#' }
#' \donttest{
#'   if (pycwa:::have_python_requirements()) {
#'     df = pycwa::py_read_cwa(file, startTime = "2019-02-26T10:55:15")
#'   }
#' }
py_convert_cwa = function(
  file,
  skipCalibration = TRUE,
  outputFolder = NULL,
  verbose = FALSE,
  timeZone = "Europe/London",
  timeShift = 0 ,
  startTime = NULL,
  endTime = NULL ,
  epochPeriod = 30L,
  sampleRate = 100L,
  useFilter = TRUE ,
  csvStartTime = NULL,
  csvSampleRate = NULL,
  calOffset = c(0.0, 0.0, 0.0),
  calSlope = c(1.0, 1.0, 1.0),
  calTemp = c(0.0, 0.0, 0.0),
  meanTemp = 20.0,
  stationaryStd = 13,
  rawDataParser = c("AccelerometerParser",
                    "omconvert"),
  javaHeapSpace = NULL) {

  check_python_requirements()
  java = Sys.which("java")
  if (!file.exists(java)) {
    warning("Java is required for py_convert_cwa, cannot find java in path")
  }
  cp_java()

  rawOutput = TRUE
  npyOutput = TRUE

  file = normalizePath(file, winslash = "/", mustWork = TRUE)
  bn = sub("[.]cwa.*$", "", basename(file), ignore.case = TRUE)
  char_null = function(x) {
    if (is.null(x)) x = ""
    as.character(x)
  }
  py_dir = system.file("acc", package = "pycwa")
  accelerometer = reticulate::import_from_path(
    "accelerometer", py_dir)

  csvStartRow = 1L
  csvTimeXYZColsIndex = as.integer(c(0,1,2,3))

  csvTimeFormat = "yyyy-MM-dd HH:mm:ss.SSSxxxx '['VV']'"

  useFilter = as.logical(useFilter)

  timeShift = as.integer(timeShift)
  epochPeriod = as.integer(epochPeriod)
  sampleRate = as.integer(sampleRate)
  # mgCutPointMVPA = as.integer(mgCutPointMVPA)
  # mgCutPointVPA = as.integer(mgCutPointVPA)

  csvStartTime = as.integer(csvStartTime)
  csvSampleRate = as.integer(csvSampleRate)

  rawOutput = as.logical(rawOutput)
  npyOutput = as.logical(npyOutput)
  skipCalibration = as.logical(skipCalibration)

  calOffset = as.integer(calOffset)
  calTemp = as.integer(calTemp)
  meanTemp = as.integer(meanTemp)
  stationaryStd = as.numeric(stationaryStd)
  # calibrationSphereCriteria = as.numeric(calibrationSphereCriteria)

  verbose = as.logical(verbose)

  rawDataParser = match.arg(rawDataParser)
  if (rawDataParser == "omconvert") {
    om = Sys.which("omconvert")
    if (!file.exists(om)) {
      warning(paste0(
        "omconvert not found, may need to install from ",
        "https://github.com/digitalinteraction/openmovement/"))
    }
    javaClassPath = ""
  } else {
    acc_file = system.file("acc", "java", "AccelerometerParser.class",
                           package = "pycwa")
    if (!file.exists(acc_file)) {
      stop("AccelerometerParser.class file not found - will fail!")
    }

    dir = system.file("acc", "java", package = "pycwa")
    dir = normalizePath(dir, winslash = "/")
    jar =  paste0(dir, "/JTransforms-3.1-with-dependencies.jar")
    javaClassPath = paste0(dir, ":", jar)
  }


  if (is.null(outputFolder)) {
    outputFolder = tempfile()
    dir.create(outputFolder, recursive = TRUE)
  }

  summaryFolder = outputFolder
  epochFolder = outputFolder
  timeSeriesFolder = outputFolder
  nonWearFolder = outputFolder
  rawFolder = outputFolder
  npyFolder = outputFolder

  javaHeapSpace = char_null(javaHeapSpace)

  deleteIntermediateFiles = FALSE

  rawDataParser = as.character(rawDataParser)


  epochFile = file.path(outputFolder, paste0(bn,  "-epoch.csv.gz"))
  stationaryFile = file.path(outputFolder, paste0(bn,  "-stationaryPoints.csv.gz"))
  rawFile = file.path(outputFolder, paste0(bn,  ".csv.gz"))
  npyFile = file.path(outputFolder, paste0(bn,  ".npy"))


  summary = reticulate::py_to_r(reticulate::dict())

  if (verbose > 1) {
    message(paste0("javaClassPath is ", javaClassPath))
  }
  info = accelerometer$device$processInputFileToEpoch(
    inputFile = file,
    timeZone = timeZone,
    summary = summary,
    stationaryFile = stationaryFile,
    timeShift = timeShift,
    startTime = startTime,
    endTime = endTime,
    epochFile = epochFile,
    stationaryFile = stationaryFile,
    rawFile = rawFile,
    npyFile = npyFile,
    epochPeriod = epochPeriod,
    sampleRate = sampleRate,
    useFilter = useFilter,
    csvStartTime = csvStartTime,
    csvSampleRate = csvSampleRate,
    csvTimeFormat = csvTimeFormat,
    csvStartRow = csvStartRow,
    csvTimeXYZColsIndex = csvTimeXYZColsIndex,
    rawOutput = rawOutput,
    npyOutput = npyOutput,
    skipCalibration = skipCalibration,
    xyzSlope = calSlope,
    xyzIntercept = calOffset,
    xyzTemp = calTemp,
    meanTemp = meanTemp,
    stationaryStd = stationaryStd,
    verbose = verbose,
    javaClassPath = javaClassPath,
    rawDataParser = rawDataParser,
    javaHeapSpace = javaHeapSpace
  )



  L = list(
    epochFile = epochFile,
    stationaryFile = stationaryFile,
    rawFile = rawFile,
    npyFile = npyFile
  )
  L = L[sapply(L, file.exists)]
  L$info = info
  L$time_zone = timeZone
  L
}

#' @param ... not used
#' @rdname py_convert_cwa
#' @export
py_read_cwa = function(file, ...) {
  result = py_convert_cwa(file, ...)
  out = readr::read_csv(
    result$rawFile,
    col_types = readr::cols(
      time = readr::col_character(),
      x = readr::col_double(),
      y = readr::col_double(),
      z = readr::col_double()
    )
  )
  attr(out, "files") = result
  out$time = lubridate::as_datetime(out$time)
  # out$time = lubridate::with_tz(out$time, tzone = "UTC")
  out
}
