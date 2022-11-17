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
#' - If two similar articles have been published on the same day, the shorter article will
#' be considered the duplicate;
#' - if two similar articles were published on different days,
#' the article that appeared later will be considered the duplicate.
#' 
#' Different `partition_bundle`-objects can be passed into the
#' \code{detect}-method successively. The field `duplicates` will be
#' appended by the duplicates that are newly detected.
#' 
#' @param x a `partition_bundle` object defining the documents that will be
#'   compared to detect duplicates
#' @param char_regex a regex defining the characters to keep
#' @param s_attribute the s-attribute providing the date
#' @param sample number of documents to define a subset of `partition_bundle` to
#'   speed up character count
#' @param n number of days before and after a document was published
#' @param threshold numeric (0 < x < 1), the minimum similarity to qualify two documents as duplicates
#' @param mc logical, whether to use multicore
#' @param verbose logical, whether to be verbose
#' @param progress logical, whether to show progress bar
#' @export Duplicates
#' @rdname Duplicates
#' @importFrom parallel mclapply
#' @importFrom pbapply pblapply
#' @importFrom stats setNames
#' @importFrom RcppCWB get_region_matrix
#' @importFrom cli cli_progress_step
#' @importFrom R6 R6Class
#' @import data.table
#' @examples
#' library(polmineR)
#' 
#' if ("NADIRASZ" %in% corpus()$corpus){
#'   D <- Duplicates$new(
#'     corpus = "NADIRASZ",
#'     char_regex = "[a-zA-ZäöüÄÖÜ]",
#'     p_attribute = "word",
#'     s_attribute = "article_date",
#'     date_preprocessor = NULL,
#'     sample = 50L,
#'     n = 1L,
#'     threshold = 0.6 # default is 0.9
#'   )
#' 
#'   article_bundle <- corpus("NADIRASZ") |>
#'     subset(article_date == "2000-01-01") |> 
#'     split(s_attribute = "article_id")
#' 
#'   D$detect(x = article_bundle, mc = 3L)
#'   
#'   # To inspect result
#'   D$duplicates
#'   
#'   if (interactive()){
#'     for (i in 1L:nrow(D$duplicates)){
#'     
#'       print(i)
#'       
#'       corpus("NADIRASZ") %>%
#'         subset(article_id == !!D$duplicates[i][["name"]]) %>%
#'         read() %>%
#'         show()
#'         
#'       readline()
#'   
#'       corpus("NADIRASZ") %>%
#'         subset(article_id == !!D$duplicates[i][["duplicate_name"]]) %>%
#'         read() %>%
#'         show()
#'         
#'       readline()
#'     }
#'   }
#' }
Duplicates <- R6::R6Class(
  
  "Duplicates",
  
  public = list(
    
    #' @field corpus ID of the CWB corpus (derived from `partition_bundle`).
    corpus = NULL,
    
    #' @field char_regex Regular expression defining the characters to keep.
    char_regex = NULL,
    
    #' @field char_count Count of the characters in the `partition_bundle`.
    char_count = NULL,
    
    #' @field n Number of days before and after a document was published.
    n = NULL,
    
    #' @field p_attribute the p-attribute used (defaults to "word")
    p_attribute = NULL,
    
    #' @field s_attribute the s-attribute of the date of a text in the corpus
    s_attribute = NULL,
    
    #' @field sample size of the sample of the `partition_bundle` that the character count is based on
    sample = NULL,
    
    #' @field threshold Minimum similarity value to consider two texts as
    #'   duplicates.
    threshold = NULL,
    
    #' @field duplicates A `data.table` with documents considered as duplicates.
    duplicates = NULL,
    
    #' @field similarities a \code{simple_triplet_matrix} with similarities of texts
    similarities = "simple_triplet_matrix",
    
    #' @field date_preprocessor function to rework dates if not in the DD-MM-YYYY standard format
    date_preprocessor = "function",
    
    #' @field annotation a \code{data.table} with corpus positions.
    annotation = "data.table",
    
    #' @description 
    #' Initialize object of class `Duplicates`.
    #' @param corpus ID of the CWB corpus that will be explored.
    initialize = function(corpus, char_regex = "[a-zA-Z]", p_attribute = "word", s_attribute = "text_date", date_preprocessor = NULL, sample = 1000L, n = 1L, threshold = 0.9){
      
      stopifnot(isFALSE(missing(corpus)))
      self$corpus <- corpus
      self$char_regex <- char_regex
      self$s_attribute <- s_attribute
      self$p_attribute <- p_attribute
      self$sample <- as.integer(sample)
      self$n <- as.integer(n)
      self$threshold <- threshold
      if (is.null(date_preprocessor)) self$date_preprocessor <- function(x) x
      
    },
    
    
    #' @description
    #' Identify documents that will be compared (based on date of documents).
    get_comparisons = function(x, reduce = TRUE, verbose = FALSE, progress = TRUE, mc = FALSE){

      if (!self$s_attribute %in% s_attributes(self$corpus)){
        stop("no valid s-attribute in field 's_attribute'")
      }
      
      if (!requireNamespace("chron", quietly = TRUE)){
        stop("the 'chron'-package needs to be installed but is not available")
      }
      
      if (verbose) message("... getting docs to be compared")
      dates <- unlist(lapply(setNames(x@objects, names(x)), function(y) s_attributes(y, self$s_attribute)))
      if (!is.null(self$date_preprocessor)) dates <- sapply(dates, self$date_preprocessor)
      objectSplittedByDate <- split(1L:length(x), f = dates)
      .get_comparisons <- function(i){
        dateOfDoc <- try(as.POSIXct(unname(dates[i])))
        if (is(dateOfDoc)[1] == "try-error"){
          warning(paste("cannot parse date:", dates[i]))
          return(NULL)
        }
        
        if (self$n > 0){
          dateRange <- chron::seq.dates(
            from = strftime(dateOfDoc - 1 - (self$n - 1) * 86400, format = "%m/%d/%Y"),
            to = strftime(dateOfDoc + 1 + (self$n - 1) * 86400, format = "%m/%d/%Y"),
            by = "days", format = "%Y-%m-%d"
          )
        } else {
          dateRange <- dateOfDoc
        }
        datesToGet <- as.character(strftime(dateRange, format = "%Y-%m-%d"))
        unlist(lapply(datesToGet, function(y) objectSplittedByDate[[y]]))
      }
      docsToCompare <- pblapply(1:length(x), FUN = .get_comparisons, cl = getOption("polmineR.cores"))
      
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
    
    #' @description
    #' Turn similarities of documents into a data.table that identifies original
    #' document and duplicate.
    #' @param similarities A `TermDocumentMatrix` with cosine similarities.
    similarities_matrix_to_dt = function(x, similarities, mc = FALSE, progress = TRUE, verbose = TRUE){
      
      if (mc == FALSE) mc <- 1L
      
      dates <- unlist(lapply(
        setNames(x@objects, names(x)),
        s_attributes,
        s_attribute = self$s_attribute
      ))
      dates <- sapply(dates, self$date_preprocessor)
      indexDuplicates <- which(similarities$v >= self$threshold)
      
      if (length(indexDuplicates) == 0L){
        message("... no duplicates found")
        return(NULL)
      }
      
      # keep only those values in similarity matrix that are above the threshold
      for (what in c("i", "j", "v"))
        similarities[[what]] <- similarities[[what]][indexDuplicates]  
      
      duplicateList <- lapply(
        1L:length(similarities$i),
        function(i){
          i_name <- similarities$dimnames[[1]][similarities$i[i]]
          j_name <- similarities$dimnames[[1]] [similarities$j[i]]
          i_date <- as.POSIXct(dates[[i_name]])
          i_size <- x@objects[i_name][[1]]@size
          j_date <- as.POSIXct(dates[[j_name]])
          j_size <- x@objects[j_name][[1]]@size
          value <- similarities$v[i]
          if (i_date == j_date){
            if (i_size >= j_size){
              return(
                c(
                  name = i_name,
                  date = as.character(i_date),
                  size = i_size,
                  duplicate_name = j_name,
                  duplicate_date = as.character(j_date),
                  duplicate_size = j_size,
                  similarity=value
                )
              )
            } else {
              return(
                c(
                  name = j_name,
                  date = as.character(j_date),
                  size = j_size,
                  duplicate_name = i_name,
                  duplicate_date = as.character(i_date),
                  duplicate_size = i_size,
                  similarity = value
                )
              )
            }
          } else if (i_date < j_date){
            return(
              c(
                name = i_name,
                date = as.character(i_date),
                size = i_size,
                duplicate_name = j_name,
                duplicate_date = as.character(j_date),
                duplicate_size = j_size,
                similarity = value
              )
            )
          } else if (i_date > j_date){
            return(
              c(
                name = j_name,
                date = as.character(j_date),
                size = j_size,
                duplicate_name = i_name,
                duplicate_date = as.character(i_date),
                duplicate_size = i_size,
                similarity = value
              )
            )
          }
        })
      
      duplicateDT <- data.table(do.call(rbind, duplicateList))
      count <- function(y) return(y)
      DT <- duplicateDT[, count(.N), by = .(name, date, size, duplicate_name, duplicate_date, duplicate_size, similarity)]
      DT[, V1 := NULL]
      DT[, size := as.numeric(size)][, duplicate_size := as.numeric(duplicate_size)][, similarity := as.numeric(similarity)]
      DT
    },
    
    #' @description
    #' Wrapper that implements the entire workflow for duplicate detection.
    #' @param x A `partition_bundle` or `subcorpus_bundle` object.
    #' @param n The number of characters to use for shingling (`integer` value),
    #'   passed as argument `n` into `polmineR::ngrams()`. Defaults to 5, in 
    #'   line with Kliche et al. 2014: 695.
    #' @param character_selection Numeric/integer vector used for indexing
    #'   `$char_count` to select the characters to keep. Defaults to 1:12, in
    #'   line with Kliche et al. 2014: 695.
    #' @param how Implementation used to compute similarities - passed into 
    #'   `cosine_similarity()`.
    #' @return The updated content of slot `$duplicates` is returned invisibly.
    detect = function(x, n = 5L, character_selection = 1:12, how = "coop", verbose = TRUE, mc = FALSE, progress = TRUE){
      
      if (x@corpus != self$corpus){
        stop("The corpus ID configured in the Duplicates engine and of the bundle are not identical.")
      }

      if (verbose) cli_progress_step("counting characters")
      if (is.numeric(self$sample)){
        bundle_sample <- sample(x, size = self$sample)
        charcount <- nchars(
          bundle_sample, p_attribute = self$p_attribute, regexCharsToKeep = self$char_regex,
          toLower = TRUE, decreasing = FALSE,
          mc = FALSE, progress = progress
        )
        rm(bundle_sample)
      } else {
        charcount <- nchars(
          x, self$p_attribute, regexCharsToKeep = self$char_regex, toLower = TRUE, decreasing = FALSE,
          mc = FALSE, progress = progress
        )
      }
      self$char_count <- setNames(as.numeric(charcount), names(charcount))
      
      if (verbose) cli_progress_step("get data for ngram matrix")
      ngram_bundle <- ngrams(x, n = n, char = names(self$char_count[character_selection]), mc = mc, progress = progress)
      
      if (verbose) cli_progress_step("assemble ngram matrix")
      ngram_matrix <- as.TermDocumentMatrix(ngram_bundle, col = "count") |>
        weigh(method = "tfidf")
      
      if (self$n == 0){
        if (verbose) cli_progress_step(paste("getting dates, using s-attribute", self$s_attribute))
        dates <- lapply(x@objects, s_attributes, s_attribute = self$s_attribute)
        
        if (verbose) cli_progress_step(paste("create groups to compare", self$s_attribute))
        groups <- split(x = names(dates), f = as.factor(unname(unlist(dates))))
        # drop groups with only one id (nothing to compare)
        for (i in rev(unname(which(sapply(groups, length) <= 1L))))
          groups[[i]] <- NULL
        
        if (verbose) cli_progress_step("compute similarities")
        .get_similarities <- function(groupname){
          if (verbose) message("... compute similarities for: ", groupname)
          ids <- groups[[groupname]]
          m <- as.matrix(ngram_matrix[,ids])
          empty_rows <- unname(which(rowSums(m) == 0L))
          if (length(empty_rows) > 0L) m <- m[-empty_rows,]
          sim <- cosine_similarity(x = t(m), how = how)
          dt <- data.table(reshape2::melt(as.matrix(sim)))
          a_is_b <- which(ifelse(dt[["Var1"]] == dt[["Var2"]], TRUE, FALSE))
          if (length(a_is_b) > 0L) dt <- dt[-a_is_b]
          dt[value >= self$threshold]
        }
        if (progress){
          dts <- pblapply(names(groups), .get_similarities, cl = mc)
        } else {
          if (mc){
            dts <- mclapply(names(groups), .get_similarities, mc.cores = mc)
          } else {
            dts <- lapply(names(groups), .get_similarities)
          }
        }
        dt <- rbindlist(dts)
        
        if (verbose) cli_progress_step("create simple_triplet_matrix")
        # factors in columns - turn it into character vectors
        for (col in c("Var1", "Var2")) dt[, (col) := as.character(dt[[col]])]
        ids <- unique(c(dt[["Var1"]], dt[["Var2"]]))
        index_new <- setNames(1L:length(ids), ids)
        dt[, "i" := unname( index_new[dt[["Var1"]]] )]
        dt[, "j" := unname( index_new[dt[["Var2"]]] )]
        # keep only one similarity score per pair
        dt <- dt[which(ifelse(dt[["i"]] < dt[["j"]], TRUE, FALSE))]
        similarities <- simple_triplet_matrix(
          i = dt[["i"]], j = dt[["j"]], v = dt[["value"]],
          nrow = length(index_new),
          ncol = length(index_new),
          dimnames = list(names(index_new), names(index_new))
        )
      } else {
        if (verbose) cli_progress_step("identifying comparisons")
        comparisons <- self$get_comparisons(x = x, verbose = verbose, mc = mc, progress = progress)
        
        if (verbose) cli_progress_step("calculating cosine similarity")
        similarities <- cosine_similarity(
          x = ngram_matrix, y = comparisons,
          mc = mc, progress = progress
        )
        # here: If duplicates slot not empty, add rows
      }
      
      if (verbose) cli_progress_step("preparing data.table")
      newDuplicateDT <- self$similarities_matrix_to_dt(
        x = x,
        similarities = similarities,
        mc = mc,
        verbose = verbose,
        progress = TRUE
      )
      if (is.null(self$duplicates)){
        self$duplicates <- newDuplicateDT
      } else {
        if (verbose) message("... data.table with duplicates alread present, appending new results")
        self$duplicates <- rbind(self$duplicates, newDuplicateDT)
      }
      invisible(self$duplicates)
    },
    
    #' @description
    #' Turn data.table with duplicates into file with corpus positions and
    #' annotation of duplicates, generate cwb-s-encode command and execute it,
    #' if wanted.
    annotate = function(s_attribute){
      
      sAttr <- s_attributes(self$corpus, s_attribute, unique = FALSE)
      
      cposMatrix <- RcppCWB::get_region_matrix(
        corpus = self$corpus,
        s_attribute = s_attribute,
        struc = 0L:(length(sAttr) - 1L)
      )
      colnames(cposMatrix) <- c("cpos_left", "cpos_right")
      
      cposDT <- data.table(cposMatrix)
      cposDT[, s_attribute := sAttr]
      setnames(cposDT, old = "s_attribute", new = s_attribute)
      setkeyv(cposDT, s_attribute)
      
      duplicates_df <- as.data.frame(self$duplicates[, c("name", "duplicate_name"), with = FALSE])
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
      
      self$annotation <- duplicatesDT[cposDT]
      setnames(self$annotation, old = "duplicate", new = s_attribute)
      self$annotation[, "duplicate" := !is.na(self$annotation[["original"]])]
      self$annotation[, "original" := sapply(self$annotation[["original"]], function(x) ifelse(is.na(x), "", x))]
      setcolorder(self$annotation, c("cpos_left", "cpos_right", s_attribute, "duplicate", "original"))
      setorderv(self$annotation, cols = "cpos_left")
    },
    
    #' @description´
    #' Add structural attributes to CWB corpus based on the annotation data that
    #' has been generated (data.table in field annotation).
    encode = function(exec = FALSE, filenames = list(duplicate = tempfile(), original = tempfile())){
      
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
        content <- .as_cwb_encode_infile(self$annotation, cols = c("cpos_left", "cpos_right", what))  
        cat(content, file = filenames[[what]])
        encodeCmd <- .makeEncodeCmd(
          filenames[[what]],
          attribute = paste(strsplit(self$s_attribute, "_")[[1]][1], what, sep="_")
        )
        cat(encodeCmd)
        cat("\n")
        if (exec) system(encodeCmd)
      }
    }
  )
)

