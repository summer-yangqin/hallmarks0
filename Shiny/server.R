library(dplyr)
library(pracma)

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

DBdata = read.table.hot("DB.txt");
# DBdata = bind_rows(DBdata, TCGA)


# DB <- reactiveFileReader(1000, NULL, 'DB.txt', read.table.hot)
DB = function() DBdata

hot_show = function(hot) {
    r = hot_to_r(hot);
    show  = r$show;
    ids  = r[1,]
    ids[show];
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

extractTerms <- function(df) {
    df$show <- NULL
    d <- unlist(df);
    d <- unname(d) # remove labels
    d <- unique(d) # compress

    numbers = suppressWarnings(  as.numeric(d[!is.na(as.numeric(d))]))
    numbers = numbers[numbers > 100000]
    numbers = unlist(lapply(numbers,as.character))

    notnumbers = suppressWarnings(  d[is.na(as.numeric(d))])
    notnumbers = unlist(list(lapply(notnumbers, trim)))

    words <- unlist(lapply(notnumbers, function(s) strsplit(s, "(\\s+)|(?!')(?=[[:punct:]])", perl = TRUE)))
    all <- c(notnumbers, words, numbers)

    all <- unique(all)
    all <- all[nchar(all) > 2]
    sort(all)
}  


DB <- reactiveFileReader(1000, NULL, 'DB.txt', read.table.hot)

function(input, output, session) {

  setBookmarkExclude(c("hot", "hot_select" ))
  UserState <- reactiveValues();

  idb = isolate(DB());
  AllTerms = extractTerms(idb)

  updateSelectizeInput(session, 'filter', choices = AllTerms)



  onBookmark(function(state) {
    state$values$savedTime <- Sys.time()
    state$values$show = UserState$SamplesShown
  })

  output$verbatim <- renderPrint({
    paste(length(UserState$SamplesShown), "selected")
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



  onRestore(function(state) {
    UserState$SamplesShown <<-  state$values$show
    updateSelectizeInput(session, 'filter', choices = AllTerms, selected= state$input$filter)
  })

  output$hot <- renderRHandsontable({
     df = DB()
     terms = input$filter

     grepF <- function(row) {
         all(unlist(lapply(terms, function(term) length(grep(term, row)) > 0)))
     }
     boolVec = apply(df, 1, grepF)
     boolVec[UserState$SamplesShown] <- TRUE;

     df <- df[boolVec,]   

     if (input$showOnlySelectedSamples) {
       df = df[ UserState$SamplesShown,]
     } 

     if (dim(df)[1] > 0) {


         UserState$Samples <- row.names(df)
         df[  UserState$SamplesShown,"show"] = TRUE

         m = dim(df)[1] # rows
         n = dim(df)[2]  #columns

         mergeCells = c();
         for (i in 2:(n)) { # columns
             j = 1
             while (j < m) { # rows
                 k = 1
                 while ((j+k) < m && !is.na(df[j,i]) && !is.na(df[j+k,i]) && df[j,i] == df[j+k,i])  {
                    k = k + 1
                 }

                 if (k > 1) {
                      mergeCells = append(mergeCells, list(list(row= j-1, col= i-1, rowspan= k, colspan= 1)))
                      j = j + k
                 } else {
                      j = j + 1
                 }
              }
         }

         rhandsontable(df,rowHeaders = NULL,
                        useTypes = TRUE, stretchH = "all",  filter = TRUE, selectCallback=TRUE,
                        readOnly = TRUE, renderer="html" , rowHeaderWidth = 100 , height = 400,
                        mergeCells = mergeCells, wordWrap=TRUE,
                        BioSampleID = df$BioSample.ID,
                        colWidths = c(40,80,70,60,200, 
                                     120,80,80,80,80,
                                     80,120,80,80,80,
                                    80)
                        ) %>%
              hot_table( height=350, fixedColumnsLeft=2, contextMenu=TRUE, manualColumnFreeze=TRUE) %>%
              hot_cols(
                manualColumnMove=TRUE,
                manualColumnResize=TRUE,
                renderer = "
                function(instance, td, row, col, prop, value, cellProperties) {
                    if (value == true || value == false) 
                        Handsontable.CheckboxRenderer.apply(this, arguments)
                    else
                        Handsontable.HtmlRenderer.apply(this, arguments)


                    if (instance.params) {
                        var ids = instance.params.BioSampleID;
                        td.classList = 'all-sample-info sample-' + ids[row];
                        if (document.ROWCOLORSHACK && ids[row] in document.ROWCOLORSHACK)
                            td.style.backgroundColor = document.ROWCOLORSHACK[ ids[row] ]
                    }
                    // if (instance.params && hcols.includes(col)) td.style.background = 'red';
                    // if (instance.params && hrows.includes(row)) td.style.background = 'yellow';
                }") %>%
              hot_col("show", readOnly = FALSE)
      }  
  }
)

  
   observeEvent( input$hot$changes,  
       {
          tryCatch( 
              {
                row = input$hot$changes$changes[[1]][[1]]
                shown = UserState$SamplesShown
                row = row + 1 # Javascript based in zero, R is one based.
                # n = input$hot$params$rRowHeaders;
                n = UserState$Samples

                sample = n[row]
                newVal = input$hot$changes$changes[[1]][[4]]
                if (newVal && !(sample %in% shown)) {
                    shown = c(sample, shown)
                    UserState$SamplesShown = shown
                } else {
                    shown = shown[shown != sample]
                    UserState$SamplesShown = shown

                }
               },  
               error=function(cond) NULL ) # end of tryCatch
       }) # end of observeEvent
} # end of server.R singletonfunction

