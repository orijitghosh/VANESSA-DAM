#' @export
runVanessaDamCra <- function() {
  appDir <- system.file("vanessadamcra", package = "vanessadamcra")
  shiny::runApp(appDir, display.mode = "normal")
}
