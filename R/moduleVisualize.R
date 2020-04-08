#' @title module_visualize_server
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
# module_visualize_server
module_visualize_server <- function(input,
                                    output,
                                    session,
                                    rv,
                                    input_re) {
  
  ### BEGIN: DB-query and basic UI-initialization
  # get query_var from url
  observe({
    if (isFALSE(rv$error)){
      query_var <- utils::URLdecode(as.character(session$clientData$url_search))
      if (!is.null(query_var)){
        rv$query_var <- gsub("^\\?", "", query_var)
        
        cat("\nOriginal queryvar:", query_var, "\n")
        
        # check if regex matches pattern
        pattern = "^[0-9A-Za-zÄÖÜäöü_=/! ´\\.\\+-]+$"
        
        if (isFALSE(grepl(pattern, rv$query_var))){
          # close db-connection
          DBI::dbDisconnect(rv$db_con)
          
          showModal(modalDialog(
            "Diese Abfrage widerspricht den implementierten Konventionen für die Zusammensetung des Analyt-Codes",
            title = "ACHTUNG",
            footer = modalButton("OK")
          ))
          rv$error <- TRUE
        }
      }
      cat("\nQuery var: ", rv$query_var, "\n")
    }
  })
  
  
  # get metadata from database
  observe({
    # wait for db_con, then create sql
    req(rv$db_con)
    
    if (!is.null(rv$query_var)){
      if (is.null(rv$db_metadata) && isFALSE(rv$error)) {
        message(paste0(
          "First call of app; rv$db_metadata not present --> fire SQL statment"
          ))
        withProgress(message = "Getting metadata from server", value = 0, {
          # avoid sql-injection
          # https://db.rstudio.com/best-practices/run-queries-safely/
          ## metadata
          sql_meta <- DBI::sqlInterpolate(rv$db_con, rv$sql[["sql_meta"]], code = rv$query_var)
          incProgress(0.5, detail = "... working hard to get metadata ...")
          # get metadata
          rv$db_metadata <- data.table(
            DBI::dbGetQuery(rv$db_con, sql_meta),
            stringsAsFactors = F
          )
          # set unit 
          rv$unitdisp <- as.character(rv$db_metadata[, get("ANA_UNIT")])
          setProgress(1, detail = "Metadata loaded")
        })
      }
    }
  })
  
  
  # get tmp_data
  observe({
    # only procede, if query_var is present
    req(rv$db_con)
    
    if (!is.null(rv$query_var)){
      # only run, if rv$db_data is NULL --> usually on first call of the app
      if (is.null(rv$db_data) && isFALSE(rv$error)) {
        message(paste0(
          "First call of app; rv$db_data not present --> fire SQL statment"
        ))
        # render headline with query_var
        output$headline <- renderUI({
          h <- h2(paste0("Choose values: ", rv$query_var))
          do.call(tagList, list(h))
        })
        withProgress(message = "Getting data from server", value = 0, {
          # avoid sql-injection
          # https://db.rstudio.com/best-practices/run-queries-safely/
          # insert query_var into statement and save it as string
          sql <- DBI::sqlInterpolate(
            rv$db_con,
            rv$sql[["sql_data"]],
            code = rv$query_var
          )
          incProgress(0.5, detail = "... working hard to get data ...")
          # query database and save result in temporary object
          rv$tmp_data <- data.table(
            DBI::dbGetQuery(rv$db_con, sql),
            stringsAsFactors = TRUE
          )
          setProgress(1, detail = "Data loaded!")
        })
      }
    }
  })
  
  # get data content to display later
  observe({
    
    req(rv$tmp_data)
    
    # only run, if rv$db_data is NULL --> usually on first call of the app
    if (is.null(rv$db_data)){
      print(rv$tmp_data)
      insp <- rv$tmp_data
      # check if there are enough rows for visualization in temporary object
      if (nrow(rv$tmp_data) < 5){
        cat("\nGot no data --> exit\n")
        showModal(modalDialog(
          "This query returned less then 5 data points.",
          title = "Less than 5 data points",
          footer = modalButton("OK")
        ))
      } else {
        
        # save original data in rv$db_data (to be able to create gender subsets later and still have the originial data here)
        rv$db_data <- rv$tmp_data[, `:=` (
          VALUE_NUM = as.numeric(as.character(get("VALUE_NUM"))),
          VALUE_TEXT = as.character(get("VALUE_TEXT")),
          AGE = as.integer(as.character(get("AGE"))),
          SEX = as.character(get("SEX"))
        )]
        # save another copy in rv$db_data_subset (on first call, this is always the whole dataset)
        rv$db_data_subset <- rv$db_data
        if (is.na(
          e1071::skewness(
            rv$db_data_subset[!is.na(get("VALUE_NUM")), get("VALUE_NUM")],
            na.rm = T))
        ) {
          variable_type <- "categorical"
        } else {
          variable_type <- "numerical"
        }
        message(paste0("variable_type: ", variable_type))

        if (variable_type == "categorical"){
          message(paste0("Got categorical data"))
          # remove Visualization-Tab
          removeTab(inputId = "tabs", target = "visualize")
          # at the end, set db_got_cat TRUE
          rv$db_got_cat <- TRUE
        } else if (variable_type == "numerical") {
          message(paste0("Got numerical data"))
          # as we have numeric data here, we add a slider to adjust
          # the range to be displayed in the graphs and
          # summary statistic
          ## set min/max first here
          rv$min <- round(min(rv$db_data[, get("VALUE_NUM")], na.rm = T), 2)
          rv$max <- round(max(rv$db_data[, get("VALUE_NUM")], na.rm = T), 2)
          if (abs(e1071::skewness(rv$db_data[, get("VALUE_NUM")], na.rm = T)) > 30) {
            rv$outlier.default <- T
          }
          ## render rangeSlider
          output$range_slider <- renderUI({
            s.range <- sliderInput(
              "rangeSlider",
              label = h3("Spannweite"),
              min = rv$min,
              max = rv$max,
              value = c(rv$min, rv$max),
              dragRange = FALSE
            )
            do.call(tagList, list(s.range, tags$hr()))
          })
          # at the end, set db_got_num TRUE to tell other functions
          # that we got numerical data
          rv$db_got_num <- TRUE
        }
        # no matter if we got numerical or categorical data,
        # add age-slider and gender-radiobuttons to allow users to
        # adjust data to be displayed in visualisations and statistics
        # ageSlider
        ## set min/max first here
        rv$min_age <- round(min(rv$db_data[, get("AGE")], na.rm = T), 2)
        rv$max_age <- round(max(rv$db_data[, get("AGE")], na.rm = T), 2)
        ## render ageSlider
        output$age_slider <- renderUI({
          s.age <- sliderInput(
            "ageSlider",
            label = h3("Alter"),
            min = rv$min_age,
            max = rv$max_age,
            value = c(rv$min_age, rv$max_age),
            dragRange = FALSE
          )
          do.call(tagList, list(s.age, tags$hr()))
        })
        ## render sex radio buttons
        # FIXME this is not generic but data dependend
        output$sex_radio <- renderUI({
          r.sex <- div(class = "row", style = "margin: 0.5%;",
                       radioButtons(inputId = "gender", 
                                    label = "Gender:",
                                    choices = list("all" = "ALL", 
                                                   "F" = "W", 
                                                   "M" = "M",
                                                   "O" = "U",
                                                   "NA" = "NA"), 
                                    selected = rv$gender_present, 
                                    inline = TRUE))
          b <- actionButton("reset", "Reset")
          do.call(tagList, list(r.sex, tags$hr(), b, tags$hr()))
        })
      }
    }
    # delete tmp_data
    rv$tmp_data <- NULL
  })
  
  
  
  
  
  # render plots
  observe({
    req(rv$x)
    # render histogram
    output$p1 <- renderPlot({
      # calculate bins here:
      # get histinfo here as reactive value, to adjust breaks by our radiobuttons and be able to update plot
      tryCatch({
        rv$histinfo <- graphics::hist(
          rv$x[, get("VALUE_NUM")],
          breaks = input$hist_bins
        )
      }, error = function(e){
        cat("\nError creating bins: using Sturges algorithm.")
        rv$histinfo <- graphics::hist(
          rv$x[, get("VALUE_NUM")],
          breaks = "Sturges"
        )
      })
      
      ggplot2::ggplot(
        data = rv$x,
        ggplot2::aes_string(x = "VALUE_NUM")
      ) + 
        ggplot2::geom_histogram(
          ggplot2::aes_string(y = "..density.."),
          color="darkgray",
          fill="white",
          breaks = rv$histinfo$breaks
        ) +
        ggplot2::geom_density() + 
        ggplot2::labs(
          title = paste("Histogram:", rv$db_metadata[, get("ANA_NAME")]),
          x = rv$unitdisp
        ) + 
        ggplot2::theme_minimal() +
        ggplot2::scale_x_continuous()
    })
    
    # render violin plot
    output$p2 <- renderPlot({
      ggplot2::ggplot(
        data = rv$x,
        ggplot2::aes_string(x = "1",
                            y = "VALUE_NUM")
      ) + 
        ggplot2::geom_violin() +
        ggplot2::geom_boxplot(width = 0.1) +
        ggplot2::labs(
          title = paste("Violin-Plot:",
                        rv$db_metadata[, get("ANA_NAME")]),
          x = " ",
          y = rv$unitdisp
        ) + 
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                       axis.ticks.y = ggplot2::element_blank()) +
        ggplot2::coord_flip()
    })
  })
}


#' @title module_visualize_ui
#'
#' @param id A character. The identifier of the shiny object
#'
#' @export
#'
# module_visualize_ui
module_visualize_ui <- function(id) {
  ns <- NS(id)
  
  tagList(# first row
    fluidRow(
      div(
        class="row", style="margin:0.5%",
        div(
          class="col-sm-6", style="text-align: left",
          div(class = "row", style = "margin: 0.5%;",
              radioButtons(
                inputId = ns("hist_bins"), 
                label = "Select bin calculation algorithm:",
                choices = list("Sturges" = "Sturges", 
                               "Scott" = "Scott", 
                               "Freedman-Diaconis" = "FD"), 
                selected = "Sturges", 
                inline = TRUE)
          )
        ),
        div(
          class="col-sm-6", style="text-align: right",
          checkboxInput(
            inputId = ns("outswitch"),
            label = "Remove outlier"
          )
        )
      ),
      tags$hr(),
      plotOutput(ns("p1")),
      tags$hr(),
      plotOutput(ns("p2")),
      tags$hr()
    )
  )
}