#' @export
runVanessaDamSa <- function() {
  appDir <- system.file("vanessadamsa", package = "vanessadamsa")
  shiny::runApp(appDir, display.mode = "normal")
}
