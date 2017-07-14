library(shiny)
library(jsonlite)

# To be called from ui.R
radarChartOutput <- function(inputId, width="100%", height="400px") {
  style <- sprintf("width: %s; height: %s;",
    validateCssUnit(width), validateCssUnit(height))
  
  tagList(
    # Include CSS/JS dependencies. Use "singleton" to make sure that even
    # if multiple radarChartOutputs are used in the same page, we'll still
    # only include these chunks once.
    singleton(tags$head(
      tags$script(src="d3/d3.v3.min.js"),
      tags$script(src="radar-chart-binding.js"),
      tags$link(rel="stylesheet", type="text/css", href="radar-chart.css"),
      tags$script(src="radar-chart.js")
    )),
    div(id=inputId, class="hallmark-chart", style=style, tag("svg", list()))

  )
}
# To be called from server.R
renderRadarChart <- function(expr, env=parent.frame(), quoted=FALSE) {
  # This piece of boilerplate converts the expression `expr` into a
  # function called `func`. It's needed for the RStudio IDE's built-in
  # debugger to work properly on the expression.
 installExprFunction(expr, "func", env, quoted)
  
#  function() {
#    dataframe <- func()
#
#    mapply(function(col, name) {
#
#      values <- mapply(function(val, i) {
#        list(x = i, y = val)
#      }, col, 1:nrow(dataframe), SIMPLIFY=FALSE, USE.NAMES=FALSE)
#
#      list(key = name, values = values)
#      
#    }, dataframe, names(dataframe), SIMPLIFY=FALSE, USE.NAMES=FALSE)
#  }
   df = eval(expr, env, NULL)
   rownames= row.names(df)
   colnames= colnames(df)
   function() (toJSON(list(rownames=rownames,colnames=colnames,df=df), pretty=TRUE));
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}


cat("loading signatures\n");
start.time = Sys.time()
Signatures <- fromJSON("signatures.json")
end.time = Sys.time()
time.taken <- end.time - start.time
cat(time.taken)
Folders = names(Signatures$index)
Cancers = lapply(Folders, function(e) { simpleCap(gsub("_", " ", e))})
names(Folders) <- Cancers;

cat("loading metadata\n");
Metadata = read.table("datasets/metadata.txt", sep="\t", header=TRUE, row.names=1)
Studies = as.list(Metadata$Study_Title)
