fluidPage(
  tags$h2("JavaScript output binding example"),
  fluidRow(
    column(width=9,
      radarChartOutput("radarchart")
    ),
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
      
      tags$i("hint: You can select multiple")

    )
  )
)
