#' @title Launch the labVisualizeR
#'
#' @param port The port, baseApp is running on (default: 3838)
#' @param db_name A character string. The name of the database.
#' @param utils_path A character string. The path to the utilities-folder.
#' @param lib_path A character string. The path to the utilities-folder.
#'
#' @return labVisualizeR application
#'
#' @import shiny shinydashboard
#' @importFrom data.table .N ":="
#' @importFrom magrittr "%>%"
#'
#' @export
#'
launch_app <- function(
  port = 3838,
  db_name,
  utils_path = system.file(
    "application/_utilities",
    package = "labVisualizeR"
  ),
  lib_path
) {

  DIZutils::global_env_hack(
    "utils_path",
    utils_path,
    1L
  )

  DIZutils::global_env_hack(
    "lib_path",
    lib_path,
    1L
  )

  DIZutils::global_env_hack(
    "db_name",
    db_name,
    1L
  )

  options(shiny.port = port)
  shiny::shinyAppDir(
    appDir = system.file("application", package = "labVisualizeR")
  )
}
