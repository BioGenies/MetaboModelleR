is_renv <- try(find.package("renv"), silent = TRUE)
if(inherits(is_renv, "try-error")) {
  install.packages("renv")
}
renv::restore(lockfile = "https://raw.githubusercontent.com/michbur/easyR/main/renv.lock")
download.file("https://raw.githubusercontent.com/michbur/easyR/main/app.R", destfile = "app.R")
print("Setup complete - running the app.")
shiny::runApp()
