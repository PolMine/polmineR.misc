#' @include polmineR.misc_package.R
NULL


#' Detect Duplicates
#' 
#' Class for duplicate detection.
#' 
#' The class implements a procedure described by Fritz Kliche, Andre Blessing,
#' Urlich Heid and Jonathan Sonntag in the paper "The eIdentity Text
#' ExplorationWorkbench" presented at LREC 2014
#' (see \url{http://www.lrec-conf.org/proceedings/lrec2014/pdf/332_Paper.pdf}).
#' 
#' To detect duplicates, choices are made as follows:
#' (a) If two similar articles have been published on the same day, the shorter article will
#' be considered the duplicate; (b) if two similar articles were published on different days,
#' the article that appeared later will be considered the duplicate.
#' 
#' Different `partition_bundle`-objects can be passed into the \code{detectDuplicates}-method successively. The field
#' \code{duplicates} will be appended by the duplicates that are newly detected. 
#' 
#' @field corpus the CWB corpus the (last) `partition_bundle` used describes
#' @field charRegex regex defining the characters to keep
#' @field charCount count of the characters in the `partition_bundle`
#' @field number of days before and after a document was published
#' @field pAttribute the p-attribute used (defaults to "word")
#' @field sAttribute the s-attribute of the date of a text in the corpus
#' @field sample size of the sample of the `partition_bundle` that the character count is based on
#' @field threshold minimum similarity value to identify two texts as duplicates
#' @field whatToCompare  a \code{simple_triplet_matrix} with the texts to be compared
#' @field similarityMatrix a \code{simple_triplet_matrix} with similarities of texts
#' @field ngramDocumentMatrix a matrix (inheriting from \code{TermDocumentMatrix}) with ngram counts in the documents of the `partition_bundle`
#' @field datePrep function to rework dates if not in the DD-MM-YYYY standard format
#' @field annotation a \code{data.table} with corpus positions
#' 
#' @param x a `partition_bundle` object defining the documents that will be
#'   compared to detect duplicates
#' @param charRegex a regex defining the characters to keep
#' @param sAttribute the s-attribute providing the date
#' @param sample number of documents to define a subset of `partition_bundle` to
#'   speed up character count
#' @param n number of days before and after a document was published
#' @param threshold numeric (0 < x < 1), the minimum similarity to qualify two documents as duplicates
#' @param mc logical, whether to use multicore
#' @param verbose logical, whether to be verbose
#' @param progress logical, whether to show progress bar
#' @examples 
#' \dontrun{
#' foo <- partition_bundle(
#'   "KEYWORDS",
#'   def = list(text_newspaper="guardian"),
#'   var=list(text_id=sAttributes("KEYWORDS", "text_id")[1:500]),
#'   pAttribute=NULL
#'  )
#' doubled <- duplicates(foo)
#' }
#' @export Duplicates
#' @exportClass Duplicates
#' @rdname Duplicates
#' @importFrom parallel mclapply
#' @importFrom pbapply pblapply
#' @importFrom stats setNames
#' @import data.table
Duplicates <- setRefClass(
  
  "Duplicates",
  
  fields = list(
    corpus = "character",
    charRegex = "character",
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
    datePrep = "function",
    annotation = "data.table"
  ),
  
  methods = list(
    
    initialize = function(charRegex = "[a-zA-Z]", pAttribute = "word", sAttribute = "text_date", datePrep = NULL, sample = 1000L, n = 1L, threshold = 0.9){
      
      "Initialize object of class 'Duplicates'."
      
      .self$charRegex <- charRegex
      .self$sAttribute <- sAttribute
      .self$pAttribute <- pAttribute
      .self$sample <- as.integer(sample)
      .self$n <- as.integer(n)
      .self$threshold <- threshold
      if (is.null(datePrep)) .self$datePrep <- function(x) x
      
    },
    
    
    getWhatToCompare = function(x, reduce = TRUE, verbose = FALSE, progress = TRUE, mc = FALSE){
      
      "Identify documents that will be compared (based on date of documents)."
      
      if (!.self$sAttribute %in% sAttributes(.self$corpus)){
        stop("no valid s-attribute in field 'sAttribute'")
      }
      if (requireNamespace("chron", quietly = TRUE)){
        message("... required package 'chron' is available")
      } else {
        stop("the 'chron'-package needs to be installed but is not available")
      }
      if (verbose) message("... getting docs to be compared")
      dates <- unlist(lapply(setNames(x@objects, names(x)), function(y) sAttributes(y, .self$sAttribute)))
      if (!is.null(.self$datePrep)) dates <- sapply(dates, .self$datePrep)
      objectSplittedByDate <- split(1:length(x), f = dates)
      .getWhatToCompare <- function(i){
        dateOfDoc <- try(as.POSIXct(unname(dates[i])))
        if (is(dateOfDoc)[1] == "try-error") return(NULL)
        if (.self$n > 0){
          dateRange <- chron::seq.dates(
            from = strftime(dateOfDoc - 1 - (.self$n - 1) * 86400, format = "%m/%d/%Y"),
            to = strftime(dateOfDoc + 1 + (.self$n - 1) * 86400, format = "%m/%d/%Y"),
            by = "days", format = "%Y-%m-%d"
          )
        } else {
          dateRange <- dateOfDoc
        }
        datesToGet <- as.character(strftime(dateRange, format = "%Y-%m-%d"))
        unlist(lapply(datesToGet, function(y) objectSplittedByDate[[y]]))
      }
      docsToCompare <- pblapply(1:length(x), FUN = .getWhatToCompare, cl = getOption("polmineR.cores"))
      
      docsToCompareMatrix <- simple_triplet_matrix(
        i = unlist(docsToCompare),
        j = unlist(lapply(1:length(docsToCompare), function(i) rep(i, times = length(docsToCompare[[i]])))),
        v = rep(NA, times = length(unlist(docsToCompare))),
        ncol = length(x),
        nrow = length(x),
        dimnames = list(rows = names(x), columns = names(x))
      )
      if (reduce){
        if (verbose) message("... reduction of document comparisons")
        keepOrDrop <- ifelse(docsToCompareMatrix$i < docsToCompareMatrix$j, TRUE, FALSE)
        for (x in c("i", "j", "v")) docsToCompareMatrix[[x]] <- docsToCompareMatrix[[x]][keepOrDrop]
      }
      return( docsToCompareMatrix )
    },
    
    makeDuplicateDataTable = function(x, mc = FALSE, progress = TRUE, verbose = TRUE){
      
      "Turn similarities of documents into a data.table that identifies original document and duplicate."
      
      if (verbose) message("... applying threshold")
      if (mc == FALSE) mc <- 1
      dates <- unlist(lapply(
        setNames(x@objects, names(x)),
        function(y) sAttributes(y, .self$sAttribute))
      )
      dates <- sapply(dates, .self$datePrep)
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
        DT <- duplicateDT[, count(.N), by=.(name, date, size, duplicate_name, duplicate_date, duplicate_size, similarity)]
        DT[, V1 := NULL]
        DT[, size := as.numeric(size)][, duplicate_size := as.numeric(duplicate_size)][, similarity := as.numeric(similarity)]
        return(DT)
      } else {
        message("... no duplicates found")
        return(NULL)
      }
    },
    
    #' @param n The number of characters to use for shingling (`integer` value),
    #'   passed as argument `n` into `polmineR::ngrams()`. Defaults to 5, in 
    #'   line with Kliche et al. 2014: 695.
    #' @param character_selection Numeric/integer vector used for indexing
    #'   `$charCount` to select the characters to keep. Defaults to 1:12, in
    #'   line with Kliche et al. 2014: 695.
    detectDuplicates = function(x, n = 5L, character_selection = 1:12, verbose = TRUE, mc = FALSE, progress = TRUE){
      
      "Wrapper that implements the entire workflow for duplicate detection."
      
      .self$corpus <- unique(sapply(x@objects, function(x) x@corpus))
      stopifnot(length(.self$corpus) == 1)
      if (verbose) message("... counting characters")
      if (is.numeric(.self$sample)){
        bundleSample <- sample(x, size = .self$sample)
        nChars <- nchars(
          bundleSample, pAttribute = .self$pAttribute, regexCharsToKeep = .self$charRegex,
          toLower = TRUE, decreasing = FALSE,
          mc = FALSE, progress = progress
        )
        rm(bundleSample)
      } else {
        nChars <- nchars(
          x, .self$pAttribute, regexCharsToKeep = .self$charRegex, toLower = TRUE, decreasing = FALSE,
          mc = FALSE, progress = progress
        )
      }
      .self$charCount <- setNames(as.numeric(nChars), names(nChars))
      if (verbose) message("... preparing ngram matrix")
      ngramBundle <- ngrams(x, n = n, char = names(.self$charCount[character_selection]), mc = mc, progress = progress)
      .self$ngramDocumentMatrix <- as.TermDocumentMatrix(ngramBundle, col = "count")
      .self$ngramDocumentMatrix <- weigh(.self$ngramDocumentMatrix, method = "tfidf")
      
      if (.self$n == 0){
        if (verbose) message("... getting dates, using s-attribute ", .self$sAttribute)
        dates <- pblapply(x@objects, function(P) sAttributes(P, .self$sAttribute))
        groups <- split(x = names(dates), f = as.factor(unname(unlist(dates))))
        # drop groups with only one id (nothing to compare)
        for (i in rev(unname(which(sapply(groups, length) <= 1)))) groups[[i]] <- NULL
          
        Ms <- pblapply(
          groups,
          function(ids){
            M <- as.matrix(.self$ngramDocumentMatrix[,ids])
            emptyRows <- unname(which(rowSums(M) == 0))
            if (length(emptyRows) > 0) M <- M[-emptyRows,]
            C <- cosine_similarity(x = t(M), how = "proxy")
            dt <- data.table(melt(as.matrix(C), variable.factor = FALSE))
            aIsB <- which(ifelse(dt[["Var1"]] == dt[["Var2"]], TRUE, FALSE) == TRUE)
            if (length(aIsB) > 0) dt <- dt[-aIsB]
            dt
          }
        )
        simDT <- rbindlist(Ms)
        # factors in columns - turn it into character vectors
        for (col in c("Var1", "Var2")) simDT[[col]] <- as.character(simDT[[col]])
        ids <- unique(c(simDT[["Var1"]], simDT[["Var2"]]))
        newIndex <- setNames(1:length(ids), ids)
        simDT[["i"]] <- unname( newIndex[ simDT[["Var1"]] ] )
        simDT[["j"]] <- unname( newIndex[ simDT[["Var2"]] ] )
        # keep only one similarity score per pair
        # simDTmin <- simDT[which(ifelse(simDT[["i"]] < simDT[["j"]], TRUE, FALSE) == TRUE)]
        .self$similarityMatrix <- simple_triplet_matrix(
          i = simDT[["i"]], j = simDT[["j"]], v = simDT[["value"]],
          dimnames = list(names(newIndex), names(newIndex))
          )
      } else {
        if (verbose) message("... identifying comparables")
        .self$whatToCompare <- .self$getWhatToCompare(x = x, verbose = verbose, mc = mc, progress = progress)
        if (verbose) message("... calculating cosine similarity")
        .self$similarityMatrix <- cosine_similarity(
          x = .self$ngramDocumentMatrix, y = .self$whatToCompare,
          mc = mc, progress = progress
        )
        if (verbose) message("... preparing data.table")
        # here: If duplicates slot not empty, add rows
        
      }
      newDuplicateDT <- .self$makeDuplicateDataTable(x = x, mc = mc, verbose = verbose, progress = TRUE)
      if (is.null(.self$duplicates)){
        .self$duplicates <- newDuplicateDT
      } else {
        if (verbose) message("... data.table with duplicates alread present, appending new results")
        .self$duplicates <- rbind(.self$duplicates, newDuplicateDT)
      }
      if (verbose) message("FINISHED")
    },
    
    makeAnnotation = function(sAttributeID){
      
      "Turn data.table with duplicates into file with corpus positions and annotation of duplicates,
      generate cwb-s-encode command and execute it, if wanted."
      
      sAttr <- sAttributes(.self$corpus, sAttributeID, unique = FALSE)
      cposList <- lapply(
        c(0:(length(sAttr) - 1)),
        function(i) CQI$struc2cpos(.self$corpus, sAttributeID, i)
        )
      cposMatrix <- do.call(rbind, cposList)
      colnames(cposMatrix) <- c("cpos_left", "cpos_right")
      cposDT <- data.table(cposMatrix)
      cposDT[, sAttributeID := sAttr]
      setnames(cposDT, old = "sAttributeID", new = sAttributeID)
      setkeyv(cposDT, sAttributeID)
      
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
      
      .self$annotation <- duplicatesDT[cposDT]
      setnames(.self$annotation, old = "duplicate", new = sAttributeID)
      .self$annotation[, "duplicate" := !is.na(.self$annotation[["original"]])]
      .self$annotation[, "original" := sapply(.self$annotation[["original"]], function(x) ifelse(is.na(x), "", x))]
      setcolorder(.self$annotation, c("cpos_left", "cpos_right", sAttributeID, "duplicate", "original"))
      setorderv(.self$annotation, cols = "cpos_left")
    },
    
    encode = function(exec = FALSE, filenames = list(duplicate = tempfile(), original = tempfile())){
      
      "Add structural attributes to CWB corpus based on the annotation data that has been generated
      (data.table in field annotation)."
      
      # helper function 
      .as_cwb_encode_infile <- function(x, cols){
        M <- as.matrix(
          data.frame(
            lapply(
              setNames(cols, cols),
              function(col) as.character(x[[col]]))
            )
          )
        paste(paste(apply(M, 1, function(row) paste(row, collapse = "\t")), collapse = "\n", sep = ""), "\n", sep = "")
      }
      
      .makeEncodeCmd <- function(filename, attribute){
        paste(
          c(
          "cwb-s-encode", "-d", parseRegistry(corpus)[["HOME"]],
          "-f", filename, "-V", attribute
          ),
          collapse = " ")
      }
      
      for (what in c("duplicate", "original")){
        content <- .as_cwb_encode_infile(.self$annotation, cols = c("cpos_left", "cpos_right", what))  
        cat(content, file = filenames[[what]])
        encodeCmd <- .makeEncodeCmd(
          filenames[[what]],
          attribute = paste(strsplit(.self$sAttribute, "_")[[1]][1], what, sep="_")
          )
        cat(encodeCmd)
        cat("\n")
        if (exec) system(encodeCmd)
      }
    }
  )
)

