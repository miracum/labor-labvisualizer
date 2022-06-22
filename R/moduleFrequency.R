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

  observe({
    req(rv$db_data_subset_present)
    if (!is.null(rv$x) && isTRUE(rv$db_got_num)) {
      # generate Table with frequency counts of VALUE_NUM
      # Freq-Panel
      output$freq_table <- DT::renderDataTable({
        if (rv$db_data_subset_present[
          , nlevels(factor(get("VALUE_NUM")))
        ] > 20) {
          dat <-
            rv$db_data_subset_present[, .N, by = c("VALUE_NUM", "VALUE_TEXT")
            ][
              order(get("N"), decreasing = TRUE)
            ][
              1:20,
            ][
              , "% Valid" := round( # nolint
                (get("N") /
                   nrow(rv$db_data_subset_present)
                ) * 100, 2)]
        } else {
          dat <-
            rv$db_data_subset_present[, .N, by = c("VALUE_NUM", "VALUE_TEXT")
            ][
              order(get("N"), decreasing = TRUE)
            ][
              , "% Valid" := round( # nolint
                (get("N") / nrow(rv$db_data_subset_present)
                ) * 100, 2)]
        }
        DT::datatable(dat, options = list(scrollX = TRUE, pageLength = 20)) %>%
          DT::formatRound(columns = 1, digits = 3)
      })

      # if we have categorical data
    } else if (isTRUE(rv$db_got_cat)) {
      # generate Table with frequency counts of VALUE_TEXT
      # Freq-Panel
      output$freq_table <- DT::renderDataTable({
        dat <- rv$db_data_subset_present[, .N, by = "VALUE_TEXT"
        ][
          order(get("N"), decreasing = TRUE)
        ][
          , "% Valid" := round( # nolint
            (get("N") / nrow(rv$db_data_subset_present)
            ) * 100, 2)]
        DT::datatable(dat, options = list(scrollX = TRUE, pageLength = 20))
      })
    }
  })
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
      DT::dataTableOutput(ns("freq_table")),
      tags$hr()
    )
  )
}
