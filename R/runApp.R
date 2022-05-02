#' @export
runExample <- function() {
  appDir <- system.file("app", package = "mypackage")
  if (appDir == "") {
    stop("Could not find app. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
