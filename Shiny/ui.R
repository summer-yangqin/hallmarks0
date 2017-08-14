library(shiny)

libraryPanel <- function()  
   wellPanel(
        div(style="display:inline-block",
            selectInput('cancer', 'Cancers', Cancers),
            selectInput('study',  'Studies', Studies),
            selectInput('filter', 'Filter Based on the following terms', NULL, multiple=TRUE, selectize=TRUE)))

uploadPanel <- function() 
   wellPanel(
      selectInput('tissue', 'Tissue', Tissues),
      fileInput('file1', 'Choose file to upload',
                accept = c(
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )),
      tags$hr(),
      p('If you want a sampl file to upload,',
        'you can first download the sample',
         a(href = 'min.txt', 'min.txt')
      )
  )


function(request) {

    fluidPage(
      tags$style("input[type=checkbox] { transform: scale(1.5); }"),

      fluidRow(
           column(width=5, tags$h2("Oncology Model Fidelity Score")),
           column(width=5, bookmarkButton())
      ),
      fluidRow( 
        column(width=3, 
                libraryPanel(),
                uploadPanel()
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
