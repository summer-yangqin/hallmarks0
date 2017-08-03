library(shiny)
function(request) {

    fluidPage(
      tags$h2("Oncology Model Fidelity Score"),
      fluidRow(
        column(width=9, 
            selectInput('filter', 'Options', NULL, multiple=TRUE, selectize=TRUE),
            checkboxInput("showOnlySelectedSamples", "Show only selected samples", FALSE),
            rHandsontableOutput("hot"),
            radarChartOutput("radarchart"),
            tags$div(class="legend", tags$p("Legend:"))),
        column(width=3,

          selectInput("cancer", 
            label = "Cancer",
            choices = Cancers,
            selected = Cancers[1]),
          
          selectInput("study", 
            label = "Study",
            choices = Studies,
            selected = Studies[1]),
          
          selectInput("sample", 
                      label = "Sample",
                      choices = Samples,
                      multiple = TRUE,
                      selected = Samples[1]),
          
          tags$i("You can select multiple samples"),

          checkboxInput("zodiac", "Hallmark Zodiac", value = TRUE, width = NULL),
          bookmarkButton()
        )
      )
    )

}
