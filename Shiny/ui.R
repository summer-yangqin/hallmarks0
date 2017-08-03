library(shiny)
function(request) {

    fluidPage(
      tags$h2("Oncology Model Fidelity Score"),
      fluidRow(
        column(width=9, 
            selectInput('filter', 'Filter Based on the following terms', NULL, multiple=TRUE, selectize=TRUE),

          fluidRow(
                div(style="display: inline-block;vertical-align:top; width: 150px;",
                    checkboxInput("showOnlySelectedSamples", "Show only selected samples", FALSE)),
                div(style="display: inline-block;vertical-align:top; width: 150px;",
                    checkboxInput("zodiac", "Hallmark Zodiac", value = TRUE, width = NULL)),
                div(style="display: inline-block;vertical-align:top; width: 150px;",
                    bookmarkButton())),

            verbatimTextOutput("verbatim"),
            rHandsontableOutput("hot"),
            radarChartOutput("radarchart"),
            tags$div(class="legend", tags$p("Legend:")))))

}
