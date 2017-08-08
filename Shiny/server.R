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


displayed_columns  = c(
    "show",

    "Type",
    "Subtype",
    "Species",
    #"Study.Title",
    #"PI",
    #"ImmPort.Study.ID",
    #"PubMed",
    "Experiment.ID",
    "Cohort",
    "BioSample.ID",
    #"Repository.Accession",
    "Biosample.Name",
    #"Biosample.Description",
    "Strain",
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
    paste(
        length(UserState$Samples), "matching", 
        length(UserState$SamplesShown), "selected"
    )
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
         cancer =  input$cancer;
         if (!is.null(cancer)) {
             if (!("All" %in% cancer)) {
                 if (!(row["Type"]  %in% cancer)) {
                    return(FALSE)
                 }
             }
         }
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

#         mergeCells = c();
#         for (i in 2:(n)) { # columns
#             j = 1
#             while (j < m) { # rows
#                 k = 1
#                 while ((j+k) < m && !is.na(df[j,i]) && !is.na(df[j+k,i]) && df[j,i] == df[j+k,i])  {
#                    k = k + 1
#                 }
#
#                 if (k > 1) {
#                      mergeCells = append(mergeCells, list(list(row= j-1, col= i-1, rowspan= k, colspan= 1)))
#                      j = j + k
#                 } else {
#                      j = j + 1
#                 }
#              }
#         }
         ddf <- df[,displayed_columns]

         rhandsontable(ddf,rowHeaders = NULL,
                        useTypes = TRUE, stretchH = "all",  filter = TRUE, selectCallback=TRUE,
                        readOnly = TRUE, renderer="html" , rowHeaderWidth = 100 , height = 400,
#                       mergeCells = mergeCells, 
                        wordWrap=TRUE,
                        BioSampleID = ddf$BioSample.ID
#                        colWidths = c(40,80,70,60,200, 120,80,80,80,80, 80,120,80,80,80, 80)
                        ) %>%
              hot_table( height=350, fixedColumnsLeft=2, contextMenu=TRUE, manualColumnFreeze=TRUE) %>%
              hot_cols(
                manualColumnMove=TRUE,
                manualColumnResize=TRUE,
                renderer = "
                function(instance, td, row, col, prop, value, cellProperties) {
                        return OMFScell.apply(this, arguments)
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

