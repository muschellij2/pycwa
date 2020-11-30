#' Download Activity Model
#'
#' @param model Model to download for activity classification
#' @param outdir output directory for tarball
#' @param ... additional arguments to pass to \code{\link{download.file}},
#' other than \code{mode}
#'
#' @return The file path to the tarball
#' @export
#' @examples
#' \donttest{
#' download_activity_model(outdir = tempdir())
#' }
download_activity_model = function(
  model = c("walmsley-nov20", "doherty-may20", "willetts-may20"),
  outdir = NULL,
  ...
) {
  model = match.arg(model)
  url="http://gas.ndph.ox.ac.uk/aidend/accModels/"
  url = paste0(url, paste0(model, ".tar"))
  if (is.null(outdir)) {
    outdir = system.file(package = "pycwa")
  }
  destfile = file.path(outdir, paste0(model, ".tar"))
  if (!file.exists(destfile)) {
    res = utils::download.file(url = url, destfile = destfile, mode = "wb", ...)
  }
  if (!file.exists(destfile)) {
    warning("Model did not download")
  }
  destfile = normalizePath(destfile, winslash = "/")
  return(destfile)
}
