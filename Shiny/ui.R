library(shiny)

libraryPanel <- function()  
        div(style="display:inline-block",
            selectInput('cancer', 'Cancers', Cancers),
            selectInput('study',  'Studies', Studies),
            selectInput('filter', 'Filter Based on the following terms', NULL, multiple=TRUE, selectize=TRUE)
        )

uploadPanel <- function() 
   wellPanel(
      fileInput('file1', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      tags$hr(),
      p('If you want a sample .csv or .tsv file to upload,',
        'you can first download the sample',
        a(href = 'mtcars.csv', 'mtcars.csv'), 'or',
        a(href = 'pressure.tsv', 'pressure.tsv'),
        'files, and then try uploading them.'
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
            tabsetPanel(
                tabPanel("Library", libraryPanel()),
                tabPanel("Upload", uploadPanel())
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
