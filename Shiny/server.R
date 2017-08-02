library(dplyr)

hallmark_columns = c(
    "Evading_growth_suppressors",
    "Evading_immune_destruction",
    "Genome_instability",
    "Replicative_immortality",
    "Reprogramming_energy_metabolism",
    "Resisting_cell_death",
    "Sustained_angiogenesis",
    "Sustaining_proliferative_signaling",
    "Tissue_invasion_and_metastasis",
    "Tumor.promoting_inflammation")


read.table.hot = function(name)  {
    table = read.table(name, header=TRUE, as.is=TRUE, fill=TRUE, sep="\t")
    row.names(table) = table[,1]
    show = rep(FALSE, dim(table)[1])
    cbind(show=show, table)
}

# Cross-session reactive file reader. all sessions DB and this reader (so cool!)
# This may need to be sped up
DB <- reactiveFileReader(1000, NULL, 'DB.txt', read.table.hot)

hot_show = function(hot) {
    r = hot_to_r(hot);
    show  = r$show;
    ids  = r[1,]
    ids[show];
}


DB <- reactiveFileReader(1000, NULL, 'DB.txt', read.table.hot)

function(input, output, session) {
  setBookmarkExclude(c("hot", "hot_select" ))
  UserState <- reactiveValues();

  onBookmark(function(state) {
    state$values$savedTime <- Sys.time()
    state$values$show = hot_show(state$input$hot)
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
      if (filename != "N/A") {
          sampleData <- read.table(paste0("datasets/", filename), header=TRUE, row.names=1, sep="\t");
          updateSelectInput(session, "sample", choices = row.names(sampleData));
      }
    }
  })
  
  output$radarchart <- renderRadarChart({
    db = DB()
    db <- db[UserState$SamplesShown, hallmark_columns]
    if (dim(db)[1] == 0)
       db = TCGA
    # df <- rbind(dd, db[UserState$SamplesShown, ])
      
    data.frame(
      colnames = colnames(db),
      df = t(db),
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
  cache_show = NULL

  onRestore(function(state) {
    cache_show <<- state$values$show
  })

  output$hot <- renderRHandsontable({
    if (!is.null(DB))
      db = DB()
      db[ db$Biosample.ID %in% cache_show,"show"] = TRUE
      rhandsontable(db,rowHeaders = NULL,
                    useTypes = TRUE, stretchH = "all", filter = TRUE, selectCallback=TRUE,
                    readOnly = TRUE, renderer="html"
                    #, rowHeaderWidth = 100
                    ) %>%
          hot_table( fixedColumnsLeft=2, contextMenu=TRUE, manualColumnFreeze=TRUE) %>%
          hot_col("show", readOnly = FALSE)
  })

  
   observeEvent( input$hot$changes,  
       {
          tryCatch( 
              {
                row = input$hot$changes$changes[[1]][[1]]
                db = DB() 
                shown = UserState$SamplesShown
                row = row + 1 # Javascript based in zero, R is one based.

                sample = db[row, 2] # second column is the SampleID
                newVal = input$hot$changes$changes[[1]][[4]]
                if (newVal && !(sample %in% shown)) {
                    shown = c(sample, shown)
                    cat(shown);
                    UserState$SamplesShown = shown
                } else {
                    shown = shown[shown != sample]
                    cat(shown);
                    UserState$SamplesShown = shown
                }
               },  
               error=function(cond) NULL ) # end of tryCatch
       }) # end of observeEvent
} # end of server.R singletonfunction

