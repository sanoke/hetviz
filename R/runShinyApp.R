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
hetviz <- function() {
  appDir <- system.file("shiny", package = "hetviz")
  if (appDir == "") {
    stop("Could not find directory containing GUI. Try re-installing `hetviz`.",
         call. = FALSE)
  }
  #helperFiles <- c("covarProfiles-plotFcn.R",
  #                 "forestPlot-plotFcns.R",
  #                 "subgrouProfiles-plotFcn.R",
  #                 "summaryFcns.R",
  #                 "vizByCovar-plotFcn.R",
  #                 "vizBySubgroup-plotFcns.R")
  #print(helperFiles)
  #for(h in helperFiles)
  #  print(h)
  #  source(h, local = TRUE)
  shiny::runApp(appDir, display.mode = "normal")
}
