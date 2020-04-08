shiny::shinyServer(function(input, output, session) {
    # define reactive values here
    rv <- shiny::reactiveValues(
        db_name = db_name,
        utils_path = utils_path,
        lib_path = lib_path
    )

    input_reactive <- reactive({
        input
    })
    
    # TODO perform db-connection here
    # Read credentials from env variables
    observe({
        # if (is.null(rv$db_con)) {
        #     rv$db_con <- db_connection(
        #         db_name = rv$db_name,
        #         lib_path = rv$lib_path
        #     )
        # }
        
        # if (is.null(rv$sql)) {
        #     rv$sql <- load_sql(
        #         utils_path = rv$utils_path
        #     )
        # }
    })
    
    # disconnect from db, if data present
    observe({
        req(rv$tmp_data)
        if (!is.null(rv$tmp_data) && !is.null(rv$db_metadata)){
            cat("\nDisconnect DB\n")
            DBI::dbDisconnect(rv$db_con)
        }
    })
    
    
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
