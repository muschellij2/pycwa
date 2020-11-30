
#' Calculate overall activity summary from epoch data
#'
#' @details
#' Get overall activity summary from input epoch file. This is achieved by
#' 1) get interrupt and data error summary vals
#' 2) check if data occurs at a daylight savings crossover
#' 3) calculate wear-time statistics, and write nonWear episodes to file
#' 4) predict activity from features, and add label column
#' 5) calculate imputation values to replace nan PA metric values
#' 6) calculate empirical cumulative distribution function of vector magnitudes
#' 7) derive main movement summaries (overall, weekday/weekend, and hour)
#'
#' @param epochFile Input csv.gz file of processed epoch data
#' @param nonWearFile Output filename for non wear .csv.gz episodes
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
#' @param minNonWearDuration Minimum duration of nonwear events (minutes)
#' @param mgCutPointMVPA MVPA threshold for cut point based activity definition
#' @param mgCutPointVPA VPA threshold for cut point based activity
#' @param useRecommendedImputation Highly recommended method to impute
#'     missing data using data from other days around the same time
#' @param activityClassification Use pre-trained random forest to predict
#'     activity type
#' @param activityModel trained activity model .tar file.  If a file is
#' passed, then it will be used, otherwise, it will be passed to
#' \code{\link{download_activity_model}}
#' @param fourierFrequency Calculate dominant frequency of sleep for circadian rhythm analysis
#' @param psd Calculate power spectral density for 24 hour circadian period
#' @param fourierWithAcc Do the Fourier analysis of circadian
#' rhythms (for PSD and Fourier Frequency) with acceleration data instead of sleep signal
#' @param m10l5 Calculate relative amplitude of most and
#' least active acceleration periods for circadian rhythm analysis
#' @param outputFolder folder for all of the output files
#' @param model_dir directory to download model to,
#' passed to \code{outdir} in \code{\link{download_activity_model}}
#' @param stationaryStd stationary mg threshold
#' @param verbose print diagnostic messages
#'
#' @export
#'
#' @return A list of data
#' @examples
#' epoch_file =  system.file("extdata", "ax3_testfile-epoch.csv.gz",
#'     package = "pycwa")
#' \donttest{
#'   if (pycwa:::have_python_requirements()) {
#'     sums = pycwa::activity_summary(epoch_file, model_dir = tempdir(), verbose = 2)
#'   }
#' }
activity_summary = function(
  epochFile,
  nonWearFile = NULL,
  outputFolder = NULL,
  verbose = FALSE,
  timeZone = "Europe/London",
  timeShift = 0 ,
  startTime = NULL,
  endTime = NULL ,
  epochPeriod = 30L,
  stationaryStd = 13L,
  mgCutPointMVPA = 100L,
  mgCutPointVPA = 425L,
  minNonWearDuration=60L,
  useRecommendedImputation = TRUE,
  activityClassification = TRUE,
  activityModel = c("walmsley-nov20", "doherty-may20", "willetts-may20"),
  model_dir = NULL,
  psd = TRUE,
  fourierFrequency = TRUE,
  fourierWithAcc = TRUE ,
  m10l5 = FALSE
) {

  check_python_requirements()

  py_dir = system.file("acc", package = "pycwa")
  accelerometer = reticulate::import_from_path(
    "accelerometer", py_dir, convert = FALSE)

  bn = sub("[.]csv.*", "", basename(epochFile))
  bn = sub("-epoch", "", bn)

  activityClassification = as.logical(activityClassification)
  fe = file.exists(activityModel)
  # need to download
  if (!any(fe)) {
    activityModel = download_activity_model(
      model = activityModel,
      outdir = model_dir,
      quiet = !verbose)
  } else {
    activityModel = activityModel[fe][1]
  }

  summary = reticulate::py_to_r(reticulate::dict())


  if (is.null(outputFolder)) {
    outputFolder = tempfile()
    dir.create(outputFolder, recursive = TRUE)
  }


  if (is.null(nonWearFile)) {
    nonWearFile = file.path(outputFolder, paste0(bn,  "-nonWearBouts.csv.gz"))
  }

  intensityDistribution = FALSE
  summary = reticulate::py_to_r(reticulate::dict())
  out = accelerometer$summariseEpoch$getActivitySummary(
    epochFile = epochFile,
    nonWearFile = nonWearFile,
    summary = summary,
    activityClassification = activityClassification,
    timeZone = timeZone,
    startTime = startTime,
    endTime = endTime,
    epochPeriod = epochPeriod,
    stationaryStd = stationaryStd,
    mgCutPointMVPA = mgCutPointMVPA,
    mgCutPointVPA = mgCutPointVPA,
    minNonWearDuration = minNonWearDuration,
    activityModel = activityModel,
    intensityDistribution = intensityDistribution,
    useRecommendedImputation = useRecommendedImputation,
    psd = psd,
    fourierFrequency = fourierFrequency,
    fourierWithAcc = fourierWithAcc,
    m10l5 = m10l5,
    verbose = verbose
  )
  if (verbose > 1) {
    print(out)
  }
  out = reticulate::py_to_r(out)
  names(out) = c("epochData", "labels", "time")
  if (verbose > 1) {
    print(utils::head(out))
  }
  out$time = out$time$time
  if (!"time" %in% colnames(out$epochData)) {
    out$epochData$time = out$time
  }
  out$time = NULL
  rownames(out$epochData) = NULL

  out$nonWearFile = nonWearFile
  out
}
