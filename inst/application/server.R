shiny::shinyServer(function(input, output, session) {
    # define reactive values here
    rv <- shiny::reactiveValues(
        db_name = db_name,
        utils_path = DIZutils::clean_path_name(utils_path),
        lib_path = lib_path,
        headless = FALSE,
        gender_present = NULL,
        outlier_default = FALSE,
        db_got_num = FALSE,
        db_got_cat = FALSE,
        error = FALSE
    )

    input_reactive <- reactive({
        input
    })

    "%notin%" <- utils::getFromNamespace(
        x = "%notin%",
        ns = "DIZutils"
    )

    output$version <- renderText({
        shiny::HTML(paste0(
            "Version: ", utils::packageVersion("labVisualizeR"),
            "<br/><br/>\u00A9 Universitätsklinikum Erlangen<br/>"
        ))
    })

    # TODO perform db-connection here
    # Read credentials from env variables
    observe({
        if (is.null(rv$db_con)) {
            rv$db_con <- DIZutils::db_connection(
                db_name = rv$db_name,
                db_type = "oracle",
                headless = FALSE,
                lib_path = rv$lib_path
            )
        }

        if (is.null(rv$sql)) {
            rv$sql <- load_sql(
                db_name = rv$db_name,
                utils_path = rv$utils_path
            )
        }
    })

    observeEvent(input$reset, {
        rv$check_for_reset <- NULL
        if (!is.null(rv$db_data)) {
            if (abs(
                e1071::skewness(
                    rv$db_data[, get("VALUE_NUM")], na.rm = T
                )
            ) > 30) {
                rv$outlier_default <- TRUE
            } else {
                rv$outlier_default <- FALSE
            }
            rv$db_data_subset <- NULL
            rv$db_data_subset_present <- NULL
            rv$min <- NULL
            rv$min_age <- NULL
            rv$max <- NULL
            rv$max_age <- NULL
            rv$rendered_age_slider <- NULL
            rv$rendered_range_slider <- NULL
            rv$gender_present <- "ALL"
        }
    })

    # Custom "Modal-Dismiss" Button
    observeEvent(input$dismiss_and_reset, {
        rv$check_for_reset <- NULL
        if (!is.null(rv$db_data)) {
            if (abs(
                e1071::skewness(
                    rv$db_data[, get("VALUE_NUM")], na.rm = T
                )
            ) > 30) {
                rv$outlier_default <- TRUE
            } else {
                rv$outlier_default <- FALSE
            }
            rv$db_data_subset <- NULL
            rv$db_data_subset_present <- NULL
            rv$min <- NULL
            rv$min_age <- NULL
            rv$max <- NULL
            rv$max_age <- NULL
            rv$rendered_age_slider <- NULL
            rv$rendered_range_slider <- NULL
            rv$gender_present <- "ALL"
        }
        removeModal()
    })


    # remove tab in case of categorical data
    observe({
        req(rv$db_got_cat)
        if (is.null(rv$viztab_removed)) {
            # remove Visualization-Tab
            removeTab(
                session = session,
                inputId = "tabs",
                target = "visualize"
            )
            msg <- paste(
                "Removed tab 'visualize'"
            )
            DIZtools::feedback(
                print_this = msg,
                type = "Info",
                logjs = TRUE,
                headless = rv$headless
            )
            rv$db_got_cat <- TRUE
        }
    })

    ### BEGIN: render UI
    observe({
        req(rv$max)
        if (!is.null(rv$max) && !is.null(rv$min)) {
            if (is.null(rv$rendered_range_slider)) {
                ## render rangeSlider
                output$range_slider <- renderUI({
                    s_range <- sliderInput(
                        "rangeSlider",
                        label = h3("Range"),
                        min = rv$min,
                        max = rv$max,
                        value = c(rv$min, rv$max),
                        dragRange = FALSE
                    )
                    do.call(tagList, list(s_range, tags$hr()))
                })
                rv$rendered_range_slider <- TRUE
            }
        }
    })

    observe({
        req(rv$max_age)
        if (!is.null(rv$max_age) && !is.null(rv$min_age)) {
            if (is.null(rv$rendered_age_slider)) {
                ## render ageSlider
                output$age_slider <- renderUI({
                    s_age <- sliderInput(
                        "ageSlider",
                        label = h3("Age"),
                        min = rv$min_age,
                        max = rv$max_age,
                        value = c(rv$min_age, rv$max_age),
                        dragRange = FALSE
                    )
                    do.call(tagList, list(s_age, tags$hr()))
                })
                rv$rendered_age_slider <- TRUE
            }
        }
    })

    observe({
        req(rv$gender_present)

        ## render sex radio buttons
        # FIXME this is not generic but data dependend
        output$sex_radio <- renderUI({
            r_sex <- div(
                class = "row", style = "margin: 0.5%;",
                radioButtons(inputId = "gender",
                             label = "Gender:",
                             choices = list("all" = "ALL",
                                            "F" = "W",
                                            "M" = "M",
                                            "O" = "U",
                                            "NA" = "NA"),
                             selected = rv$gender_present,
                             inline = TRUE)
            )
            b <- actionButton("reset", "Reset")
            do.call(tagList, list(r_sex, tags$hr(), b, tags$hr()))
        })
    })
    ### END: render UI

    ### BEGIN: observe slider stuff here.
    observe({
        req(rv$db_data)
        few_datapoints <- FALSE
        # if all sexes, reset subset to db_data
        if (rv$gender_present == "ALL") {
            msg <- "Gender = ALL"
            DIZtools::feedback(
                print_this = msg,
                logjs = TRUE,
                headless = FALSE
            )
            db_data_subset <- rv$db_data

            # else: select subset based on radiobutton value
        } else if (rv$gender_present %in% c("W", "M")) {
            msg <- paste("Gender =", rv$gender_present)
            DIZtools::feedback(
                print_this = msg,
                logjs = TRUE,
                headless = FALSE
            )
            db_data_subset <-
                rv$db_data[get("SEX") == rv$gender_present, ]

            # else: gender unknown
        } else if (rv$gender_present == "U") {
            if (rv$db_data[get("SEX") %notin% c("W", "M", NA), .N] < 5) {
                msg <- paste("Gender = U not present")
                DIZtools::feedback(
                    print_this = msg,
                    type = "Warning",
                    logjs = TRUE,
                    headless = FALSE
                )

                # modal, if there are too few observations
                message <- paste(
                    "This query resulted in",
                    rv$db_data[get("SEX") %notin% c("W", "M", NA), .N],
                    "data points. Resetting view..."
                )
                few_datapoints <- TRUE

            } else {
                # else: show observations of unknown
                db_data_subset <- rv$db_data[
                    get("SEX") %notin% c("W", "M", NA),
                ]
            }

        } else if (rv$gender_present == "NA") {
            if (rv$db_data[is.na(get("SEX")), .N] < 5) {
                msg <- paste("Gender =NA not present")
                DIZtools::feedback(
                    print_this = msg,
                    type = "Warning",
                    logjs = TRUE,
                    headless = FALSE
                )
                message <- paste(
                    "This query resulted in",
                    rv$db_data[is.na(get("SEX")), .N],
                    "data points. Resetting view..."
                )
                few_datapoints <- TRUE
            } else {
                # else: show observations of unknown
                db_data_subset <- rv$db_data[is.na(get("SEX")), ]
            }
        }
        if (isTRUE(few_datapoints)) {
            # modal, if there are too few observations
            showModal(modalDialog(
                message,
                title = "Too few data points",
                footer = actionButton("dismiss_and_reset", "OK")
            ))
            db_data_subset <- rv$db_data
        }

        if (isTRUE(rv$db_got_cat)) {
            rv$db_data_subset <- db_data_subset
        } else if (isTRUE(rv$db_got_num)) {
            # if we want to remove outlier
            if (isFALSE(rv$outlier_default)) {

                rv$db_data_subset <- db_data_subset[!is.na(get("VALUE_NUM")), ]

            } else {
                # N
                n_pre_out <- db_data_subset[!is.na(get("VALUE_NUM")), .N]

                # remove outliers
                rv$db_data_subset <- db_data_subset[!is.na(get("VALUE_NUM")),
                ][
                    , ("VALUE_NUM") :=
                        cap_outliers(get("VALUE_NUM"))]
                # N
                n_post_out <- rv$db_data_subset[!is.na(get("VALUE_NUM")), .N]

                # create modal message
                if (n_pre_out > n_post_out) {
                    message <- paste(n_pre_out - n_post_out, "outlier removed.")
                } else {
                    message <- "No outlier present."
                }

                # show message
                showModal(modalDialog(
                    message,
                    title = "Outlier",
                    footer = modalButton("OK")
                ))
            }
            rv$min <- round(
                min(rv$db_data_subset[, get("VALUE_NUM")], na.rm = T), 2
            )
            rv$max <- round(
                max(rv$db_data_subset[, get("VALUE_NUM")], na.rm = T), 2
            )

            updateSliderInput(
                session,
                inputId = "range_slider",
                value = c(rv$min, rv$max)
            )
        }

        # always update age slider
        rv$min_age <- round(
            min(rv$db_data_subset[, get("AGE")], na.rm = T), 2
        )
        rv$max_age <- round(
            max(rv$db_data_subset[, get("AGE")], na.rm = T), 2
        )

        # always update age slider
        # update ageSlider to min/max
        updateSliderInput(
            session,
            inputId = "age_slider",
            value = c(rv$min_age, rv$max_age)
        )
    })

    # create x
    observe({
        req(rv$rendered_range_slider)
        if (!is.null(input$rangeSlider)) {
            if (input$rangeSlider[1] < input$rangeSlider[2]) {
                # select based on rangeSlider and ageSlider values:
                rv$db_data_subset_present <-
                    rv$db_data_subset[!is.na(get("VALUE_NUM")), ][
                        get("VALUE_NUM") >= input$rangeSlider[1] &
                            get("VALUE_NUM") <= input$rangeSlider[2],
                    ][
                        get("AGE") >= input$ageSlider[1] &
                            get("AGE") <= input$ageSlider[2],
                    ]

                # create x here
                rv$x <- rv$db_data_subset_present[
                    , c("VALUE_NUM", "SEX"), with = FALSE
                ]
                rv$check_for_reset <- TRUE
            }
        }
    })

    observe({
        req(rv$rendered_age_slider)
        if (!is.null(input$ageSlider)) {
            if (input$ageSlider[1] < input$ageSlider[2]) {
                # select based on rangeSlider and ageSlider values:
                rv$db_data_subset_present <-
                    rv$db_data_subset[
                        get("AGE") >= input$ageSlider[1] &
                            get("AGE") <= input$ageSlider[2],
                    ]
                rv$check_for_reset <- TRUE
            }
        }
    })

    observe({
        req(rv$check_for_reset)

        # if data is continuous, check also for x
        if (isTRUE(rv$db_got_num)) {
            if (rv$x[, .N] < 5) {
                check_error <- TRUE
            } else {
                check_error <- FALSE
            }
        } else if (isTRUE(rv$db_got_cat)) {
            # check data populating the age slider first
            if (rv$db_data_subset_present[, .N] < 5) {
                check_error <- TRUE
            } else {
                check_error <- FALSE
            }
        }

        if (isTRUE(check_error)) {
            rv$db_data_subset <- NULL
            rv$db_data_subset_present <- NULL
            rv$min <- NULL
            rv$min_age <- NULL
            rv$max <- NULL
            rv$max_age <- NULL
            rv$rendered_age_slider <- NULL
            rv$rendered_range_slider <- NULL
            showModal(modalDialog(
                paste(
                    "Found too few data points"
                ),
                title = "Too few data points",
                footer = actionButton("dismiss_and_reset", "OK")
            ))
        } else if (isFALSE(check_error)) {
            rv$check_for_reset <- NULL
            rv$render_plots <- TRUE
        }
    })
    ### END: observe slider stuff


    ### BEGIN: observe user inputs here:
    # range slider
    observeEvent(input$rangeSlider, {
        # always check, that slider values are not equal; if so, adjust them
        if (input$rangeSlider[1] >= input$rangeSlider[2]) {
            updateSliderInput(
                session,
                inputId = "range_slider",
                value = c(input$rangeSlider[1],
                          input$rangeSlider[2] + 1))
        }

        # wait until input$rangeSlider is not NULL --> necessary due to
        # initialization of app. at the beginning right after the sql query,
        # we have no min/max values.
        # this way, we avoid heckmeck in the backend
        if (!is.null(input$rangeSlider)) {

            msg <- paste(
                "rangeSlider value:",
                input$rangeSlider
            )
            DIZtools::feedback(
                print_this = msg,
                type = "Info",
                logjs = TRUE,
                headless = rv$headless
            )
        }
    })

    # age slider
    observeEvent(input$ageSlider, {
        # always check, that slider values are not equal
        if (input$ageSlider[1] >= input$ageSlider[2]) {
            updateSliderInput(
                session,
                inputId = "age_slider",
                value = c(input$ageSlider[1], input$ageSlider[2] + 1)
            )
        }

        if (!is.null(input$ageSlider)) {

            msg <- paste(
                "ageSlider value:",
                input$ageSlider
            )
            DIZtools::feedback(
                print_this = msg,
                type = "Info",
                logjs = TRUE,
                headless = rv$headless
            )
        }
    })

    # gender selection
    observeEvent(input$gender, {

        msg <- paste(
            "sexRadio value:",
            input$gender
        )
        DIZtools::feedback(
            print_this = msg,
            type = "Info",
            logjs = TRUE,
            headless = rv$headless
        )

        # update reactive value
        rv$gender_present <- input$gender
    })
    ### END: observe user inputs
    ###
    ###
    ### BEGIN: work with data to be visualized here:
    # create summary statistics
    observe({
        # wait for db_data_subset.present; we can only present statistics,
        # if there is data to present
        req(rv$db_data_subset_present)

        # catch data type here
        # if datatype is numerical -> rv$db_got_num = TRUE
        if (isTRUE(rv$db_got_num)) {
            # render summarystatistics-UI
            output$summary <- renderUI({
                s_h <- h2("Summary")
                d <- div(
                    class = "row", style = "height:800%;margin: 0.5%;",
                    div(
                        class = "col-sm-6",
                        div(class = "row", h4(paste0("Variable: ",
                                                     rv$query_var))),
                        div(class = "row", tableOutput("num_summary"))
                    ),
                    div(
                        class = "col-sm-6",
                        div(class = "row", h4("Age:")),
                        div(class = "row", tableOutput("age_summary")))
                )

                do.call(tagList, list(s_h, d, tags$hr()))
            })

            # calculate and present summary statistics with our own
            # "summary_function" function
            output$num_summary <- renderTable(summary_function(
                rv$db_data_subset_present[, get("VALUE_NUM")])
            )
            output$age_summary <- renderTable(summary_function(
                rv$db_data_subset_present[, get("AGE")])
            )
        }
    })


    # create N: this is independent of data-type. we just calculate the
    # number of rows in rv$db_data_subset.present
    observe({
        # wait for db_got_num
        req(rv$db_data_subset_present)

        msg <- paste(
            "N:",
            rv$db_data_subset_present[, .N]
        )
        DIZtools::feedback(
            print_this = msg,
            type = "Info",
            logjs = TRUE,
            headless = rv$headless
        )

        output$display_n <- renderUI({
            n1 <- div(
                class = "row", style = "margin: 0.5%;",
                h4(paste0("Caculations are based on"))
            )
            n2 <- div(
                class = "row", style = "margin: 0.5%;",
                h4(paste0("N = ",
                          rv$db_data_subset_present[, .N],
                          " data points"))
            )
            do.call(tagList, list(n1, n2, tags$hr()))
        })

    })
    ### END: work with data to be visulalized here:



    ######################
    ## Visualize Tab
    ######################
    shiny::callModule(
        module_visualize_server,
        "moduleVisualize",
        rv = rv,
        input_re = input_reactive
    )

    ######################
    ## Frequency Tab
    ######################
    shiny::callModule(
        module_frequency_server,
        "moduleFrequency",
        rv = rv,
        input_re = input_reactive
    )

    ######################
    ## Metadata Tab
    ######################
    shiny::callModule(
        module_metadata_server,
        "moduleMetadata",
        rv = rv,
        input_re = input_reactive
    )

    ######################
    ## Rawdata Tab
    ######################
    shiny::callModule(
        module_rawdata_server,
        "moduleRawdata",
        rv = rv,
        input_re = input_reactive
    )
})
