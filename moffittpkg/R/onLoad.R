#' #' Adds the content of inst/assets/ to moffitt/
#' #'
#' #' @importFrom shiny addResourcePath
#' #'
#' #' @noRd
#' #'
#' .onLoad <- function(...) {
#'   shiny::addResourcePath("moffittpkg", system.file("assets", package = "moffittpkg"))
#'   
#'   invisible(NULL)
#' }
