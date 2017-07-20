function(input, output, session) {
  

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
      df = t(df)
    )
  })
}
