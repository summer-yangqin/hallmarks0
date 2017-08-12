library(shiny)
function(request) {

    fluidPage(
      tags$h2("Oncology Model Fidelity Score"),
      fluidRow(
        column(width=3, 
            div(style="display:inline-block",
                selectInput('cancer', 'Cancers', Cancers),
                selectInput('study',  'Studies', Studies),
                selectInput('filter', 'Filter Based on the following terms', NULL, multiple=TRUE, selectize=TRUE),
                    checkboxInput("showOnlySelectedSamples", "Show only selected samples", FALSE)),
                    checkboxInput("zodiac", "Hallmark Zodiac", value = TRUE, width = NULL),
                    bookmarkButton()),
          column(width=9, 
            rHandsontableOutput("hot"))),
      fluidRow(
            column(width=7,
                radarChartOutput("radarchart")),
            column(width=5,
                tags$div(class="legend-div",
                    tags$p("Legend"),
                    tags$ul(class="legend-ul"))
                # tags$div(class="legend", tags$p("Legend:"))
            )))

}
