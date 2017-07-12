function(input, output, session) {
  
  testdata = read.table("testdata", row.names=1, header=TRUE)
  output$radarchart <- renderRadarChart({
    # Return a data frame. Each column will be a series in the hallmark radar chart.
    data.frame(testdata)
  })

}
