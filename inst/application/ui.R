shiny::shinyUI(
  shiny::tagList(
    
    shiny::fluidPage(
      shiny::titlePanel("labVisualizeR"),
      
      shiny::sidebarLayout(
        sidebarPanel(
          uiOutput("headline"),
          
          uiOutput("range_slider"),
          
          uiOutput("age_slider"),
          
          uiOutput("sex_radio"),
          
          uiOutput("summary"),
          
          uiOutput("display_N"),
          
          shiny::div(
            class = "sidebar-menu",
            style = paste0("position:fixed; bottom:0; ",
                           "left:0; white-space: normal;",
                           "text-align:left;",
                           "padding: 9.5px 9.5px 9.5px 9.5px;",
                           "margin: 6px 10px 6px 10px;",
                           "box-sizing:border-box;",
                           "heigth: auto; width: 230px;"),
            shiny::HTML(
              paste0(
                "Version: ", utils::packageVersion("labVisualizeR"),
                "<br/><br/>\u00A9 Universitätsklinikum Erlangen<br/>"
              )
            )),
          width = 3
        ),
        
        shiny::mainPanel(
          shiny::tabsetPanel(
            id = "tabs",
            shiny::tabPanel(
              module_visualize_ui("moduleVisualize"),
              title = "Visualizations",
              value = "visualize",
              icon = icon("chart-bar")
            ),
            shiny::tabPanel(
              module_frequency_ui("moduleFrequency"),
              title = "Frequency Counts",
              value = "frequency",
              icon = icon("cubes")
            ),
            shiny::tabPanel(
              module_metadata_ui("moduleMetadata"),
              title = "Metadata",
              value = "metadata",
              icon = icon("database")
            ),
            shiny::tabPanel(
              module_rawdata_ui("moduleRawdata"),
              title = "Raw data",
              value = "rawdata",
              icon = icon("file")
            )
          )
        )
      )
    )
  )
)