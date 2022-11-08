

run_MetaboModelleR <- function(port = getOption("shiny.port"))
  runApp(system.file("app", package = "MetaboModelleR"), port = port, launch.browser = TRUE)
