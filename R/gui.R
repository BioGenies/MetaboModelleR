#' easyR Graphical User Interface
#'
#' @description Launches graphical user interface.
#' 
#' @importFrom shiny runApp
#' 
#' @param port The TCP port. See \code{\link[shiny]{runApp}}.
#' 
#' @section Warning : Any ad-blocking software may cause malfunctions.
#' 
#' @export run_MetaboModelleR

run_MetaboModelleR <- function(port = getOption("shiny.port"))
  runApp(system.file("app", package = "MetaboModelleR"), port = port, launch.browser = TRUE)
