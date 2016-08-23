#' get duplicates
#' 
#' The method implements a procedure described by Fritz Kliche, Andre Blessing,
#' Urlich Heid and Jonathan Sonntag in the paper "The eIdentity Text
#' ExplorationWorkbench" presented at LREC 2014
#' (see \url{http://www.lrec-conf.org/proceedings/lrec2014/pdf/332_Paper.pdf}).
#' 
#' The method calls the (internal) \code{"getDuplicates"}-method that will make choices as follows:
#' (a) If two similar articles have been published on the same day, the shorter article will
#' be considered the duplicate; (b) if two similar articles were published on different days,
#' the article that appeared later will be considered the duplicate.
#' 
#' @param .Object a \code{"partitionBundle"} object
#' @param chars a regex providing the characters to keep
#' @param sAttribute the s-attribute providing the date
#' @param sample will be passed as param size into sample-method to achieve a subset of partitionBundle and make char count faster
#' @param n number of days before and after a document was published
#' @param threshold numeric (0 < x < 1), the minimum similarity to qualify two documents as duplicates
#' @param mc whether to use multicore
#' @param verbose logical, whether to be verbose
#' @param ... further parameters (verbose, progress, mc) to be passed into functions
#' @examples 
#' \dontrun{
#' foo <- partitionBundle(
#'   "KEYWORDS",
#'   def=list(text_newspaper="guardian"),
#'   var=list(text_id=sAttributes("KEYWORDS", "text_id")[1:500]),
#'   pAttribute=NULL
#'  )
#' doubled <- duplicates(foo)
#' }
#' @exportClass Duplicates
#' @rdname duplicates-method
#' @importFrom parallel mclapply
#' @import data.table
setRefClass(
  "Duplicates",
  fields = list(
    partitionBundle = "partitionBundle",
    corpus = "character",
    chars = "character",
    charCount = "numeric",
    n = "integer",
    pAttribute = "character",
    sAttribute = "character",
    sample = "integer",
    threshold = "numeric",
    duplicates = "data.table",
    whatToCompare = "simple_triplet_matrix",
    similarityMatrix = "simple_triplet_matrix",
    ngramDocumentMatrix = "TermDocumentMatrix",
    datePrep = "function"
  ),
  
  methods = list(
    initialize = function(chars="[a-zA-Z]", pAttribute="word", sAttribute="text_date", datePrep = NULL, sample = 1000L, n=1L, threshold=0.9){
      chars <<- chars
      sAttribute <<- sAttribute
      pAttribute <<- pAttribute
      sample <<- as.integer(sample)
      n <<- as.integer(n)
      threshold <<- threshold
      if (is.null(datePrep)) datePrep <<- function(x) x
    },
    
    getWhatToCompare = function(x, reduce = TRUE, verbose = FALSE, progress = TRUE, mc = FALSE){
      if (!is.null(.self$sAttribute)){
        if (requireNamespace("chron", quietly=TRUE)){
          message("... chron-package required and loaded")
        } else {
          stop("the 'chron'-package needs to be installed but is not available")
        }
        if (verbose == TRUE) message("... getting files to be compared")
        dates <- unlist(lapply(setNames(x@objects, names(x)), function(y) sAttributes(y, .self$sAttribute)))
        if (!is.null(.self$datePrep)) dates <- sapply(dates, .self$datePrep)
        objectSplittedByDate <- split(c(1:length(x)), f = dates)
        .getWhatToCompare <- function(i){
          dateOfDoc <- try(as.POSIXct(unname(dates[i])))
          if (is(dateOfDoc)[1] == "try-error") return(NULL)
          dateRange <- chron::seq.dates(
            from = strftime(dateOfDoc - 1 - (.self$n - 1) * 86400, format = "%m/%d/%Y"),
            to = strftime(dateOfDoc + 1 + (.self$n - 1) * 86400, format = "%m/%d/%Y"),
            by = "days", format = "%Y-%m-%d"
          )
          datesToGet <- sapply(c(1:length(dateRange)), function(j) {
            as.character(strftime(dateRange[j], format="%Y-%m-%d"))
          })
          unlist(lapply(datesToGet, function(y) objectSplittedByDate[[y]]))
        }
        if (mc == FALSE){
          docsToCompare <- lapply(c(1:length(x)), .getWhatToCompare)
        } else {
          docsToCompare <- parallel::mclapply(c(1:length(x)), .getWhatToCompare)
        }
        
        docsToCompareMatrix <- simple_triplet_matrix(
          i = unlist(docsToCompare),
          j = unlist(lapply(c(1:length(docsToCompare)), function(i) rep(i, times=length(docsToCompare[[i]])))),
          v = rep(NA, times=length(unlist(docsToCompare))),
          ncol = length(x),
          nrow = length(x),
          dimnames = list(rows = names(x), columns = names(x))
        )
        if (reduce == TRUE){
          keepOrDrop <- sapply(
            c(1:length(docsToCompareMatrix$i)),
            function(i) ifelse(docsToCompareMatrix$i[i] < docsToCompareMatrix$j[i], TRUE, FALSE)
          )
          for (x in c("i", "j", "v")) docsToCompareMatrix[[x]] <- docsToCompareMatrix[[x]][keepOrDrop]
        }
        return(docsToCompareMatrix)
      } else {
        stop("so far, getting comparables is only implemented based on dates")
      }
    },
    
    getDuplicates = function(x, mc = FALSE, progress = TRUE, verbose = TRUE){
      if (verbose) message("... applying threshold")
      if (mc == FALSE) mc <- 1
      dates <- unlist(lapply(
        setNames(x@objects, names(x)),
        function(y) sAttributes(y, .self$sAttribute))
      )
      indexDuplicates <- which(.self$similarityMatrix$v >= .self$threshold)
      if (length(indexDuplicates) > 0){
        # keep only those values in similarity matrix that are above the threshold
        for (what in c("i", "j", "v")) .self$similarityMatrix[[what]] <- .self$similarityMatrix[[what]][indexDuplicates]  
        duplicateList <- lapply(
          c(1:length(.self$similarityMatrix$i)),
          function(i){
            iName <- .self$similarityMatrix$dimnames[[1]][.self$similarityMatrix$i[i]]
            jName <- .self$similarityMatrix$dimnames[[1]] [.self$similarityMatrix$j[i]]
            iDate <- as.POSIXct(dates[[iName]])
            iSize <- x@objects[[iName]]@size
            jDate <- as.POSIXct(dates[[jName]])
            jSize <- x@objects[[jName]]@size
            value <- .self$similarityMatrix$v[i]
            if(iDate == jDate){
              if (iSize >= jSize){
                return(c(name=iName, date=as.character(iDate), size=iSize, duplicate_name=jName, duplicate_date=as.character(jDate), duplicate_size=jSize, similarity=value))
              } else {
                return(c(name=jName, date=as.character(jDate), size=jSize, duplicate_name=iName, duplicate_date=as.character(iDate), duplicate_size=iSize, similarity=value))
              }
            } else if (iDate < jDate){
              return(c(name=iName, date=as.character(iDate), size=iSize, duplicate_name=jName, duplicate_date=as.character(jDate), duplicate_size=jSize, similarity=value))
            } else if (iDate > jDate){
              return(c(name=jName, date=as.character(jDate), size=jSize, duplicate_name=iName, duplicate_date=as.character(iDate), duplicate_size=iSize, similarity=value))
            }
          })
        duplicateDT <- data.table(do.call(rbind, duplicateList))
        count <- function(y) return(y)
        if (verbose) message("... x")
        DT <- duplicateDT[, count(.N), by=.(name, date, size, duplicate_name, duplicate_date, duplicate_size, similarity)]
        if (verbose) message("... xx")
        DT[, V1 := NULL]
        if (verbose) message("... xxx")
        DT[, size := as.numeric(size)][, duplicate_size := as.numeric(duplicate_size)][, similarity := as.numeric(similarity)]
        return(DT)
      } else {
        message("... no duplicates found")
        return(NULL)
      }
    },
    
    detectDuplicates = function(x, verbose = TRUE, mc = FALSE, progress = TRUE){
      partitionBundle <<- x
      corpus <<- unique(sapply(.self$partitionBundle@objects, function(x) x@corpus))
      stopifnot(length(.self$corpus) == 1)
      if (verbose == TRUE) message("... counting characters")
      if (is.numeric(.self$sample)){
        bundleSample <- sample(x, size = .self$sample)
        nChars <- nchars(
          bundleSample, pAttribute = .self$pAttribute, regexCharsToKeep = .self$chars,
          toLower = TRUE, decreasing = FALSE,
          mc = FALSE, progress = progress
        )
        rm(bundleSample)
      } else {
        nChars <- nchars(
          x, .self$pAttribute, regexCharsToKeep = .self$chars, toLower=TRUE, decreasing=FALSE,
          mc=FALSE, progress = progress
        )
      }
      charCount <<- setNames(as.numeric(nChars), names(nChars))
      if (verbose) message("... preparing ngram matrix")
      ngramBundle <- ngrams(x, n = 4, char = names(.self$charCount[1:10]), mc = mc, progress = progress)
      ngramDocumentMatrix <<- as.TermDocumentMatrix(ngramBundle, col="count")
      ngramDocumentMatrix <<- polmineR::weigh(.self$ngramDocumentMatrix, method="tfidf")
      if (verbose) message("... identifying comparables")
      whatToCompare <<- .self$getWhatToCompare(x = x, verbose = verbose, mc = mc, progress = progress)
      if (verbose) message("... calculating cosine similarity")
      similarityMatrix <<- similarity(
        .self$ngramDocumentMatrix,
        select = .self$whatToCompare, return.simil = FALSE,
        mc = mc, progress = progress)
      if (verbose) message("... preparing data.table")
      # here: If duplicates slot not empty, add rows
      newDuplicateDT <- .self$getDuplicates(x = x, mc = mc, verbose = verbose, progress = TRUE)
      if (is.null(.self$duplicates)){
        duplicates <<- newDuplicateDT
      } else {
        if (verbose) message("... data.table with duplicates alread present, appending new results")
        .self$duplicates <- rbind(.self$duplicates, newDuplicateDT)
      }
      if (verbose) message("FINISHED")
    },
    
    addAnnotation = function(sAttribute){
      sAttr <- sAttributes(corpus, sAttribute)
      cposList <- lapply(
        c(0:(length(sAttr) - 1)),
        function(i) CQI$struc2cpos(corpus, sAttribute, i)
        )
      cposMatrix <- do.call(rbind, cposList)
      colnames(cposMatrix) <- c("cpos_left", "cpos_right")
      cposDT <- data.table(cposMatrix)
      cposDT[, sAttribute := sAttr, with = FALSE]
      setkeyv(cposDT, sAttribute)
      
      duplicates_df <- as.data.frame(.self$duplicates[, c("name", "duplicate_name"), with = FALSE])
      G <- igraph::graph_from_data_frame(duplicates_df)
      chunks <- igraph::decompose(G)
      duplicateList <- lapply(
        chunks,
        function(x){
          indegree <- igraph::degree(x, mode = "in")
          original <- names(indegree)[which(indegree == 0)[1]]
          duplicated <- names(indegree)[which(!names(indegree) %in% original)]
          list(
            original = rep(original, times = length(duplicated)),
            duplicate = duplicated
          )
        }
      )
      duplicatesDT <- data.table(
        original = unlist(lapply(duplicateList, function(x) x$original)),
        duplicate = unlist(lapply(duplicateList, function(x) x$duplicate))
      )
      setkeyv(duplicatesDT, "duplicate")
      
      DUPL <- duplicatesDT[cposDT]
      setnames(DUPL, old = "duplicate", new = sAttribute)
      DUPL[, "duplicate" := !is.na(DUPL[["original"]])]
      DUPL[, "original" := sapply(DUPL[["original"]], function(x) ifelse(is.na(x), "", x))]
      setcolorder(DUPL, c("cpos_left", "cpos_right", sAttribute, "duplicate", "original"))
      
      .as_cwb_encode_infile <- function(x, cols){
        M <- as.matrix(
          data.frame(
            lapply(
              setNames(cols, cols),
              function(col) as.character(x[[col]]))
            )
          )
        paste(apply(M, 1, function(row) paste(row, collapse = "\t")), collapse = "\n", sep = "")
      }
      
      .makeEncodeCmd <- function(filename, attribute){
        paste(c(
          "cwb-s-encode",
          "-d", parseRegistry(corpus)[["HOME"]],
          "-f", filename, "-V", attribute
        ), collapse = " ")
      }
      
      for (what in c("duplicate", "original")){
        content <- .as_cwb_encode_infile(DUPL, cols = c("cpos_left", "cpos_right", what))  
        filename <- tempfile()
        cat(content, file = filename)
        cat(.makeEncodeCmd(filename, attribute = paste("text", what, sep="_")))
      }
      
    }
  )
)

