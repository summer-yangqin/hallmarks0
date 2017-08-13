library(shiny)

libraryPanel <- function()  
    div(style="display:inline-block",
        selectInput('cancer', 'Cancers', Cancers),
        selectInput('study',  'Studies', Studies),
        selectInput('filter', 'Filter Based on the following terms', NULL, multiple=TRUE, selectize=TRUE)
    )



function(request) {

    fluidPage(
      fluidRow(
           column(width=5, tags$h2("Oncology Model Fidelity Score")),
           column(width=5, bookmarkButton())
      ),
      fluidRow( 
        column(width=3, 
            tabsetPanel(
                tabPanel("Library", libraryPanel())
            )
        ),
        column(width=9, 
            checkboxInput("showOnlySelectedSamples", "Show only selected samples", FALSE),
            rHandsontableOutput("hot"))
      ),
      fluidRow(
            column(width=7,
                checkboxInput("zodiac", "Hallmark Zodiac", value = TRUE, width = NULL),
                radarChartOutput("radarchart")),
            column(width=5,
                tags$div(class="legend-div",
                    tags$p("Legend"),
                    tags$ul(class="legend-ul"))
                # tags$div(class="legend", tags$p("Legend:"))
            )))

}
