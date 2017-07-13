fluidPage(
  tags$h2("JavaScript output binding example"),
  fluidRow(
    column(width=9,
      radarChartOutput("radarchart")
    ),
    column(width=3,

      selectInput("cancer", 
        label = "Choose a cancer to display",
        choices = Cancers,
        selected = Cancers[1]),
      
      selectInput("study", 
        label = "Choose a study to display",
        choices = Studies,
        selected = Studies[1])
      

    )
  )
)
