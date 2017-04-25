#' hetviz: Treatment effect heterogeneity (TEH) visualization
#'
#' To launch the application, the user should call \code{hetviz()} (no
#' arguments) from the console. This call will launch
#' the eponymous \code{shiny} application locally,
#' within the default web browser of the user's machine.
#' There are no arguments in this call; all arguments will be provided to the package
#' through the graphical user interface.
#'
#' @export
#' @import ggplot2
hetviz <- function() {
  appDir <- system.file("shiny", package = "hetviz")
  if (appDir == "") {
    stop("Could not find directory containing GUI. Try re-installing `hetviz`.",
         call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
