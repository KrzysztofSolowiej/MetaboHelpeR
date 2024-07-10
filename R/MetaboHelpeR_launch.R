MetaboHelpeR_launch <- function() {
  appDir <- system.file("app", "app.R", package = "MetaboHelpeR")
  if (appDir == "") {
    stop("Try re-installing `MetaboHelpeR`.", call. = FALSE)
  }

  source(appDir, local = TRUE)
  MetaboHelpeR()
}
