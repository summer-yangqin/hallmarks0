function(input, output, session) {
  
  testdata = read.table("testdata", row.names=1, header=TRUE)
  output$radarchart <- renderRadarChart({
    # Return a data frame. Each column will be a series in the hallmark radar chart.
      cat("hello");
      data.frame(testdata)
  })

  observeEvent( input$cancer, {
      
      cat("cancer\n")
      cat(input$cancer)
      cat("folder\n")
      cat(Folders[input$cancer])

  })

}
