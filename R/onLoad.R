#' Adds the content of inst/assets/ to moffitt/
#'
#' @importFrom shiny addResourcePath
#'
#' @noRd
#'
.onLoad <- function(...) {
  shiny::addResourcePath("mccpkg", system.file("assets", package = "mccpkg"))

  invisible(NULL)
}
