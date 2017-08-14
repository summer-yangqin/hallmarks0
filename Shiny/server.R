library(dplyr)
library(pracma)

printf <- function(...) cat(sprintf(...))

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
    "Tumor_promoting_inflammation")


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
    "Tumor_promoting_inflammation")



maxS = 3.0
minS = -3.0
diffS = maxS - minS;

rescale= function(a) {
  b = scale(a, center = T, scale = T);
  c = diffS/(max(b)-min(b))*(b-min(b))+minS
  return(c);
}


computeSignatureScore = function(X, tissue) {
    index <- Signatures$index[[tissue]];
    signaturesForTissue <- Signatures$signatures[index];

    possible = row.names(X)
    X <- apply(X, 2, rescale)
    row.names(X) <- possible
    X <- data.frame(X)
    scores = data.frame()
    
    n = length(signaturesForTissue)
    signature <- NULL
 
    for (i in 1:n) {
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("Doing part", i, "of", n))

        signature    <- signaturesForTissue[[i]];
        hallmark <- signature$hallmark;
        
        should  <- names(signature$w)
        genes    <- intersect(should, possible)

        printf("should=%d possible=%d actual=%d\n", length(should),length(possible),length(genes));

        score = data.frame();
        posScale <- signature$posScale;
        negScale <- signature$negScale;
        w = signature$w[genes]
    
        XX <- t(X[genes,])
        #cat(XX);
      
    
    
        raw = XX %*% w + signature$b;
        heat= XX * w + signature$b;
    
        for (j in 1:length(raw)) {
            value = raw[j];
            if (value < 0) {
                score[1,j] = round(500  - (negScale * raw[j]));
            } else {
                score[1,j] = round( (posScale * raw[j]) + 500);
            }
        }
        scores = rbind(scores, score);
    }

    colnames(scores) = colnames(X);
    scores = t(scores)
    colnames(scores) = unlist(lapply(signaturesForTissue,function(sig) sig$hallmark))
    scores = scores[,1:10]

    n = nrow(scores)
    scores = cbind(scores, 
        data.frame(
            show = rep( FALSE, n),
            Type = rep( simpleCap(signature$cancer), n),
            Subtype = rep( simpleCap(signature$tissue), n),
            Species = rep( "none", n),
            Study.Title = rep( "none", n),
            PI = rep( "User", n),
            ImmPort.Study.ID = rep( "REF", n),
            PubMed = rep( "none", n),
            Experiment.ID = rep( "none", n),
            Cohort = rep( "none", n),
            BioSample.ID = rep( c, n),
            Repository.Accession = rep( "none", n),
            Biosample.Name = rep( "none", n),
            Biosample.Description = rep( "none", n),
            Strain = rep( "none", n)));

print("scores")
         print(colnames(scores))
    return (scores);
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



function(input, output, session) {

  setBookmarkExclude(c("hot", "hot_select" ))
  UserState <- reactiveValues();
  UserState$Upload = NULL

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
    if (nrow(db) == 0)
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
     if (! is.null(UserState$Upload))  {
         up = UserState$Upload
print("up")
         print(colnames(up))
print("df")
         print(colnames(df))
         df = rbind(UserState$Upload, df)
    }

     terms = input$filter

     studyID <- sub(" .*", "", input$study) 


     boolVec = apply(df, 1, 
      function(row) {

        if (studyID != "All" && studyID != row["ImmPort.Study.ID"]) {  # short circuit if we are filtering on input$study
            return(FALSE)
        }

         cancer =  input$cancer;
         if (!is.null(cancer)) {
             if (!("All" %in% cancer)) {
                 if (!(row["Type"]  %in% cancer)) {
                    return(FALSE)
                 }
             }
         }
         all(unlist(lapply(terms, function(term) length(grep(term, row)) > 0)))
     })


     boolVec[UserState$SamplesShown] <- TRUE;

     df <- df[boolVec,]   

     if (input$showOnlySelectedSamples) {
       df = df[ UserState$SamplesShown,]
     } 

     if (nrow(df) > 0) {


         UserState$Samples <- row.names(df)
         df[  UserState$SamplesShown,"show"] = TRUE

         m = nrow(df)
         n = ncol(df)

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

   observeEvent( input$file1,  {
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.

    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    userData = read.csv(inFile$datapath, header = input$header,
             row.names=1,
             sep = input$sep, quote = input$quote)

    # rownames(UserData) <- lapply(rownames(userData), function(nm) { if (nm %in% Mus_Homologues) nm = Mus_Homologues[nm] nm })

    withProgress(message = 'Making plot', value = 0, {

        ldf = computeSignatureScore(userData, input$tissue)
        UserState$Upload = ldf
    })
  })
  
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

