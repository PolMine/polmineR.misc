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
#' @exportMethod duplicates
#' @rdname duplicates-method
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
    comparisons = "simple_triplet_matrix",
    similarityMatrix = "simple_triplet_matrix",
    ngramDocumentMatrix = "TermDocumentMatrix",
    datePrep = "function"
  ),
  
  methods = list(
    initialize = function(chars="[a-zA-Z]", pAttribute="word", sAttribute="text_date", datePrep = NULL, sample = 1000L, n=2L, threshold=0.9){
      chars <<- chars
      sAttribute <<- sAttribute
      pAttribute <<- pAttribute
      sample <<- as.integer(sample)
      n <<- as.integer(n)
      threshold <<- threshold
      if (is.null(datePrep)) datePrep <<- function(x) x
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
      ngramBundle <- ngrams(x, n = 4, char = names(.self$charCount[1:10]), progress = progress)
      ngramDocumentMatrix <<- as.TermDocumentMatrix(ngramBundle, col="count")
      ngramDocumentMatrix <<- polmineR::weigh(.self$ngramDocumentMatrix, method="tfidf")
      if (verbose) message("... identifying comparables")
      comparisons <<- comparables(.Object, date = sAttribute, n = n, datePrep = .self$datePrep, progress = progress)
      if (verbose) message("... calculating cosine similarity")
      similarityMatrix <<- similarity(
        .self$ngramDocumentMatrix,
        select = .self$comparisons, return.simil = FALSE,
        mc = mc, progress = progress)
      if (verbose) message("... preparing data.table")
      # here: If duplicates slot not empty, add rows
      duplicates <<- getDuplicates(
        x, similarityMatrix = .self$similarityMatrix, threshold = .self$threshold,
        date = .self$sAttribute, progress = TRUE
      )
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
          duplicates <- names(indegree)[which(!names(indegree) %in% original)]
          list(
            original = rep(original, times = length(duplicates)),
            duplicate = duplicates
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

# dt <- readRDS("/home/blaette/Lab/tmp/duplicates_keywords_it.RData")
