#' Adds the content of inst/assets/ to mcc/
#'
#' @importFrom shiny addResourcePath
#'
#' @noRd
#'
.onLoad <- function(...) {
  shiny::addResourcePath("mcc", system.file("assets", package = "mccpkg"))
   
  invisible(NULL)
}
