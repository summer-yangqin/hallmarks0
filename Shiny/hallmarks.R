############################################################################################

library(shiny)
library(shinyjqui)

# Don't use (jsonlite) 
library(RJSONIO)

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
      tags$script(src="OMFScell.js"),
      tags$script(src="handsontable.full.js"),
      tags$link(rel="stylesheet", type="text/css", href="handsontable.full.css"),
      tags$link(rel="stylesheet", type="text/css", href="radar-chart.css"),
      tags$link(rel="stylesheet", type="text/css", href="OMFS.css"),
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
  
  function() {
    dataframe <- func()

    mapply(function(col, name) {

      values <- mapply(function(val, i) {
        list(x = i, y = val)
      }, col, 1:nrow(dataframe), SIMPLIFY=FALSE, USE.NAMES=FALSE)

      list(key = name, values = values)
      
    }, dataframe, names(dataframe), SIMPLIFY=FALSE, USE.NAMES=FALSE)
  }
}


simpleCap <- function(x) {
  listOfWords <- strsplit(x, "[-_ .]")
  listOfWords = lapply(listOfWords, function(s) {
      paste0(toupper(substring(s, 1,1)), substring(s, 2))
  })
  paste(unlist(listOfWords), sep="", collapse=" ")
}


# Signatures <- RJSONIO::fromJSON("../Signatures/signatures")
Signatures <- RJSONIO::fromJSON("signatures")
TCGA = data.frame();
for (sig in Signatures$signatures) {
    m = round(mean( sig$reference$score[sig$reference$labels == 1] ))
    c = simpleCap(sig$cancer)

    TCGA[c, sig$hallmark] = m
    # TCGA[c, "show"] <- TRUE
    TCGA[c, "Type"] = simpleCap(sig$cancer)
    TCGA[c, "Subtype"] = simpleCap(sig$tissue)
    TCGA[c, "Species"] = "Homo Sapien"
    TCGA[c, "Study.Title"] <- "Mean average of samples"

    TCGA[c, "PI"] <- "TCGA"
    TCGA[c, "ImmPort.Study.ID"] <- ""
    TCGA[c, "PubMed"] <- ""
    TCGA[c, "Experiment.ID"] <- ""
    TCGA[c, "Cohort"] <- ""
    TCGA[c, "BioSample.ID"] <- ""
    TCGA[c, "Repository.Accession"] <- ""
    TCGA[c, "Biosample.Name"] <- ""
    TCGA[c, "Biosample.Description"] <- ""
    TCGA[c, "Strain"] <- ""
}
colnames(TCGA) <- unlist(lapply(colnames(TCGA), function(x) gsub("Tumor.", "Tumor.", gsub(" ", "_", x))))


DB <- reactiveFileReader(1000, NULL, 'DB.txt', read.table.hot)
read.table.hot = function(name)  {
    table = read.table(name, header=TRUE, as.is=TRUE, fill=TRUE, sep="\t")
    row.names(table) = table$BioSample.ID

# write(sort(colnames(table)), file="db")
# write(sort(colnames(TCGA)), file="tcga")
    colOrder = colnames(table)
    table = rbind(TCGA, table)
    table = table[, colOrder];
    cbind(show=rep(FALSE, dim(table)[1]), table)
}

db = isolate(DB())



Cancers = c("All", unique(sort(db$Type)))

SelectStudies = function(db) {
   fields = c("ImmPort.Study.ID", "PI",  "Study.Title")
   c("All", unique(sort(apply(unique(db[,fields]), 1, function(x) paste(x, collapse=" ")))))
}


Studies = SelectStudies(db)

enableBookmarking(store = "url")

