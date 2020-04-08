#' @title module_frequency_server
#'
#' @param input Shiny server input object
#' @param output Shiny server output object
#' @param session Shiny session object
#' @param rv The global 'reactiveValues()' object, defined in server.R
#' @param input_re The Shiny server input object, wrapped into a reactive
#'   expression: input_re = reactive({input})
#'
#' @export
#'
# module_frequency_server
module_frequency_server <- function(input,
                                    output,
                                    session,
                                    rv,
                                    input_re) {
}


#' @title module_frequency_ui
#'
#' @param id A character. The identifier of the shiny object
#'
#' @export
#'
# module_frequency_ui
module_frequency_ui <- function(id) {
  ns <- NS(id)
  
  tagList(# first row
    fluidRow(
    )
  )
}