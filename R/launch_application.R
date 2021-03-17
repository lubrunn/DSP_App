#' @export
runExample <- function() {
  appDir <- system.file( package = "SentimentApp")
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `SentimentApp`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
