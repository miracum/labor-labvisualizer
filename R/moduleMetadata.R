#' @title module_metadata_server
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
# module_metadata_server
module_metadata_server <- function(input,
                                   output,
                                   session,
                                   rv,
                                   input_re) {
  ### Render Metatdata Tab
  observe({
    req(rv$db_metadata)

    output$m1 <- reactive({
      paste0("Code: ", rv$db_metadata[, get("ANA_CODE")])
    })
    output$m2 <- reactive({
      paste0("Name: ", rv$db_metadata[, get("ANA_NAME")])
    })
    output$m3 <- reactive({
      paste0("Shortname: ", rv$db_metadata[, get("ANA_SHORTNAME")])
    })
    output$m4 <- reactive({
      paste0("Description: ", rv$db_metadata[, get("ANA_DESCRIPTION")])
    })
    output$m5 <- reactive({
      paste0("Description 2: ", rv$db_metadata[, get("ANA_DESCRIPTION2")])
    })
    output$m6 <- reactive({
      paste0("Unit: ", rv$db_metadata[, get("ANA_UNIT")])
    })
    output$m7 <- reactive({
      paste0("Method: ", rv$db_metadata[, get("ANA_METHOD")])
    })
    output$m8 <- reactive({
      paste0("Type: ", rv$db_metadata[, get("ANA_TYPE")])
    })
    output$m9 <- reactive({
      paste0("Labname: ", rv$db_metadata[, get("LAB_NAME")])
    })
    output$m10 <- reactive({
      paste0("Lab shortname: ", rv$db_metadata[, get("LAB_SHORTNAME")])
    })
  })
}


#' @title module_metadata_ui
#'
#' @param id A character. The identifier of the shiny object
#'
#' @export
#'
# module_metadata_ui
module_metadata_ui <- function(id) {
  ns <- NS(id)

  tagList(# first row
    fluidRow(
      verbatimTextOutput(ns("m1")),
      verbatimTextOutput(ns("m2")),
      verbatimTextOutput(ns("m3")),
      verbatimTextOutput(ns("m4")),
      verbatimTextOutput(ns("m5")),
      verbatimTextOutput(ns("m6")),
      verbatimTextOutput(ns("m7")),
      verbatimTextOutput(ns("m8")),
      tags$hr(),
      verbatimTextOutput(ns("m9")),
      verbatimTextOutput(ns("m10")),
      tags$hr()
    )
  )
}
