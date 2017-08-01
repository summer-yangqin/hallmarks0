library(shiny)

fluidPage(
  tags$h2("Oncology Model Fidelity Score"),
  fluidRow(
    column(width=9, 
        tags$div(class="legend", tags$p("Legend:")),
        rHandsontableOutput("hot", height = 350),
        verbatimTextOutput('selected'),
        radarChartOutput("radarchart")),
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

