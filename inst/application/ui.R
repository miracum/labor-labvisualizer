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

          uiOutput("display_n"),

          htmlOutput("version"),

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
