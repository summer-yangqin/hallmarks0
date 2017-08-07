library(shiny)
function(request) {

    fluidPage(
      tags$h2("Oncology Model Fidelity Score"),
        column(width=12, 
          fluidRow(
            selectInput('filter', 'Filter Based on the following terms', NULL, multiple=TRUE, selectize=TRUE),

            column(width=1, 
                    checkboxInput("showOnlySelectedSamples", "Show only selected samples", FALSE)),
            column(width=1, 
                    checkboxInput("showOnlySelectedSamples", "Show only selected samples", FALSE)),
            column(width=1, 
                    verbatimTextOutput("verbatim")),
            column(width=1, 
                    checkboxInput("zodiac", "Hallmark Zodiac", value = TRUE, width = NULL)),
                    bookmarkButton())),

      fluidRow(
            rHandsontableOutput("hot"),
            column(width=6,
                radarChartOutput("radarchart")),
            column(width=6,
                tags$div(class="legend-div",
                    tags$p("Legend"),
                    tags$ul(class="legend-ul"))
                # tags$div(class="legend", tags$p("Legend:"))
            )))

}
