function(input, output, session) {
  
  testdata = read.table("testdata", row.names=1, header=TRUE)
  output$radarchart <- renderRadarChart({
    # Return a data frame. Each column will be a series in the hallmark radar chart.
      cat("hello");
      data.frame(testdata)
  })



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
      SampleData <- read.table(paste0("datasets/", filename), header=TRUE, row.names=1, sep="\t");
      updateSelectInput(session, "sample", choices = row.names(SampleData));
    }
    
  })

}
