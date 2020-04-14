#' @title module_rawdata_server
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
# module_rawdata_server
module_rawdata_server <- function(input,
                                  output,
                                  session,
                                  rv,
                                  input_re) {
  # render table of raw data for debugging
  observe({
    # wait for db_data
    req(rv$db_data)

    # renter table tab
    output$rawdata_table <- DT::renderDataTable({
      DT::datatable(rv$db_data,
                    options = list(scrollX = TRUE, pageLength = 20)) %>%
        DT::formatRound(columns = 1, digits = 3)
    })
  })
}


#' @title module_rawdata_ui
#'
#' @param id A character. The identifier of the shiny object
#'
#' @export
#'
# module_rawdata_ui
module_rawdata_ui <- function(id) {
  ns <- NS(id)

  tagList(# first row
    fluidRow(
      DT::dataTableOutput(ns("rawdata_table")),
      tags$hr()
    )
  )
}
