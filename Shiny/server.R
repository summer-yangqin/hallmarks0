read.table.preferred = function(name)  {
    table = read.table(name, header=TRUE, as.is=TRUE, fill=TRUE, sep="\t")

    
}

# Cross-session reactive file reader. all sessions DB and this reader (so cool!)
# This may need to be sped up
DB <- reactiveFileReader(1000, NULL, 'DB.txt', read.table.preferred)


function(input, output, session) {

#  onBookmark(function(state) {
#    vals$savedTime <- Sys.time()
#    # state is a mutable reference object, and we can add arbitrary values
#    # to it.
#    state$values$time <- vals$savedTime
#  })
#
#  onRestore(function(state) {
#    vals$savedTime <- state$values$time
#  })
  

  observeEvent( input$cancer, {
      if (input$cancer != "All") {
          # cancer <- paste0("%", input$cancer, "%")
          m <- Metadata[grep(input$cancer, Metadata$Cancer),];
          updateSelectInput(session, "study", choices = as.list(m$Study));
      }

  })
  observeEvent( input$study, {
    if (input$study != "All") {
      m <- Metadata[grep(input$study, Metadata$Study),];
      filename = m[1,"File"]
      if (filename != "N/A") {
          sampleData <- read.table(paste0("datasets/", filename), header=TRUE, row.names=1, sep="\t");
          updateSelectInput(session, "sample", choices = row.names(sampleData));
      }
    }
  })
  
  output$radarchart <- renderRadarChart({
    # Return a data frame. Each column will be a series in the line chart.
    df = TCGA

      if (!is.null(input$cancer) && input$cancer != "All") {
        df <- df[input$cancer,]
      }

      if (!is.null(input$sample) && input$sample != "N/A") {
        m <- Metadata[grep(input$study, Metadata$Study),];
        filename = m[1,"File"]
        if (filename != "N/A") {
            sampleData <- read.table(paste0("datasets/", filename), header=TRUE, row.names=1, sep="\t");
            colnames(sampleData) = lapply(colnames(sampleData), simpleCap);
            df <- rbind(df, sampleData[input$sample, ])
        }
      }
      
    data.frame(
      colnames = colnames(df),
      df = t(df),
      zodiac = input$zodiac
    )
  })


  cache_tbl = NULL

  onRestore(function(state) {
    tmp = state$input$hot
    tmp$data = jsonlite::fromJSON(
      jsonlite::toJSON(tmp$data), simplifyVector = FALSE)
    cache_tbl <<- tmp
  })

  data = reactive({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else if (!is.null(cache_tbl)) {
      DF = hot_to_r(cache_tbl)
      cache_tbl <<- NULL
    } else {
      DF = data.frame(val = 1:10, bool = TRUE, nm = LETTERS[1:10],
                      dt = seq(from = Sys.Date(), by = "days", length.out = 10),
                      stringsAsFactors = F)
    }
    DF
  })

  output$hot <- renderRHandsontable({
    if (!is.null(DB))
      rhandsontable(DB(), useTypes = TRUE, stretchH = "all", filter = TRUE)
  })
}

