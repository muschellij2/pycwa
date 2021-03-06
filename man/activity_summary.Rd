% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reader2.R
\name{activity_summary}
\alias{activity_summary}
\title{Calculate overall activity summary from epoch data}
\usage{
activity_summary(
  epochFile,
  nonWearFile = NULL,
  outputFolder = NULL,
  verbose = FALSE,
  timeZone = "Europe/London",
  timeShift = 0,
  startTime = NULL,
  endTime = NULL,
  epochPeriod = 30L,
  stationaryStd = 13L,
  mgCutPointMVPA = 100L,
  mgCutPointVPA = 425L,
  minNonWearDuration = 60L,
  useRecommendedImputation = TRUE,
  activityClassification = TRUE,
  activityModel = c("walmsley-nov20", "doherty-may20", "willetts-may20"),
  model_dir = NULL,
  psd = TRUE,
  fourierFrequency = TRUE,
  fourierWithAcc = TRUE,
  m10l5 = FALSE
)
}
\arguments{
\item{epochFile}{Input csv.gz file of processed epoch data}

\item{nonWearFile}{Output filename for non wear .csv.gz episodes}

\item{outputFolder}{folder for all of the output files}

\item{verbose}{print diagnostic messages}

\item{timeZone}{timezone in country/city format to
be used for daylight savings crossover check}

\item{timeShift}{time shift to be applied, e.g.
-15 will shift the device internal time by -15
minutes. Not to be confused with timezone offsets.}

\item{startTime}{removes data before this
time (local) in the final analysis}

\item{endTime}{removes data after this
time (local) in the final analysis}

\item{epochPeriod}{length in seconds of a single epoch}

\item{stationaryStd}{stationary mg threshold}

\item{mgCutPointMVPA}{MVPA threshold for cut point based activity definition}

\item{mgCutPointVPA}{VPA threshold for cut point based activity}

\item{minNonWearDuration}{Minimum duration of nonwear events (minutes)}

\item{useRecommendedImputation}{Highly recommended method to impute
missing data using data from other days around the same time}

\item{activityClassification}{Use pre-trained random forest to predict
activity type}

\item{activityModel}{trained activity model .tar file.  If a file is
passed, then it will be used, otherwise, it will be passed to
\code{\link{download_activity_model}}}

\item{model_dir}{directory to download model to,
passed to \code{outdir} in \code{\link{download_activity_model}}}

\item{psd}{Calculate power spectral density for 24 hour circadian period}

\item{fourierFrequency}{Calculate dominant frequency of sleep for circadian rhythm analysis}

\item{fourierWithAcc}{Do the Fourier analysis of circadian
rhythms (for PSD and Fourier Frequency) with acceleration data instead of sleep signal}

\item{m10l5}{Calculate relative amplitude of most and
least active acceleration periods for circadian rhythm analysis}
}
\value{
A list of data
}
\description{
Calculate overall activity summary from epoch data
}
\details{
Get overall activity summary from input epoch file. This is achieved by
\enumerate{
\item get interrupt and data error summary vals
\item check if data occurs at a daylight savings crossover
\item calculate wear-time statistics, and write nonWear episodes to file
\item predict activity from features, and add label column
\item calculate imputation values to replace nan PA metric values
\item calculate empirical cumulative distribution function of vector magnitudes
\item derive main movement summaries (overall, weekday/weekend, and hour)
}
}
\examples{
epoch_file =  system.file("extdata", "ax3_testfile-epoch.csv.gz",
    package = "pycwa")
\donttest{
  if (pycwa:::have_python_requirements()) {
    sums = pycwa::activity_summary(epoch_file, model_dir = tempdir(), verbose = 2)
  }
}
}
