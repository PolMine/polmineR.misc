#' Count the number of characters
#' 
#' @param x Object to process.
#' @param progress A `logical` value.
#' @param char_regex If not NULL, a regular expression - defaults to "[a-zA-Z]".
#' @param lowercase whether to lower tokens
#' @param mc logical
#' @param ... Argument passed into `blapply()`.
#' @param decreasing logical, passed into order call 
#' @exportMethod nchars
#' @rdname nchars
setGeneric("nchars", function(x, ...) standardGeneric("nchars"))


#' @param p_attribute the p-attribute
#' @param char_regex if NULL, counts for all charactrs will be returned, else a regex indicating which characters to include in the counting
#' @param lowercase whether to lower tokens
#' @param mc logical
#' @param decreasing logical, passed into order call 
#' @exportMethod nchars
#' @rdname nchars
#' @examples
#' library(polmineR)
#' use("RcppCWB")
#' 
#' partition("REUTERS", id = "127") %>%
#'   nchars()
#'   
#' corpus("REUTERS") %>%
#'   subset(id == "127") %>%
#'   nchars()
#'   
#' corpus("REUTERS") %>%
#'   partition_bundle(s_attribute = "id") %>%
#'   nchars()
#'   
#' corpus("REUTERS") %>%
#'   split(s_attribute = "id") %>%
#'   nchars()
setMethod("nchars", "partition", function(x, p_attribute = "word", char_regex = "[a-zA-Z]", lowercase = TRUE, decreasing = TRUE){
  .Object <- x
  charSoup <- get_token_stream(.Object, p_attribute = p_attribute, collapse = "")
  if (isTRUE(lowercase)) charSoup <- tolower(charSoup)
  charCount <- table(unlist(strsplit(charSoup, "")))
  if(!is.null(char_regex)){
    charCount <- charCount[grep(char_regex, names(charCount))]
  }
  y <- charCount[order(charCount, decreasing = decreasing)]
  setNames(as.integer(y), names(y))
})

#' @rdname nchars
setMethod("nchars", "subcorpus", function(x, p_attribute = "word", char_regex = "[a-zA-Z]", lowercase = TRUE, decreasing = TRUE){
  nchars(
    x = as(x, "partition"),
    p_attribute = p_attribute,
    char_regex = char_regex,
    lowercase = lowercase,
    decreasing = decreasing
  )
})

#' @rdname nchars
setMethod("nchars", "partition_bundle", function(x, mc = FALSE, progress = TRUE, decreasing = TRUE, ...){
  partitionCount <- if (isFALSE(mc)){
    lapply(x@objects, function(obj) nchars(x = obj, ...))
  } else {
    if (progress){
      pblapply(x@objects, function(obj) nchars(x = obj, ...), cl = mc)
    } else {
      mclapply(x@objects, function(obj) nchars(x = obj, ...), mc.cores = mc)
    }
  }
  
  charCount <- tapply(
    unname(unlist(partitionCount)),
    INDEX = unlist(sapply(partitionCount, function(x) names(x))),
    FUN = sum
  )
  
  y <- charCount[order(charCount, decreasing = decreasing)]
  setNames(as.integer(y), names(y))
})

#' @rdname nchars
setMethod("nchars", "subcorpus_bundle", function(x, decreasing = TRUE, mc = FALSE, progress = TRUE, ...){
  nchars(
    x = as(x, "partition_bundle"),
    decreasing = decreasing,
    mc = mc,
    progress = progress
  )
})


#' @param verbose Whether to output progress messages.
#' @param sample An `integer` or `numeric` value defining the number of sample
#'   tokens extracted from the (entirely decoded) token stream to be evaluated.
#' @rdname nchars
#' @importFrom polmineR decode
#' @importFrom stringi stri_count_fixed stri_opts_fixed
#' @examples
#' library(polmineR)
#' use("RcppCWB")
#' n <- corpus("REUTERS") %>% nchars(sample = 4000)
setMethod("nchars", "corpus", function(x, p_attribute = "word", lowercase = TRUE, sample = 5000000L, char_regex = "[a-zA-Z]", decreasing = TRUE, verbose = TRUE){
  # optime get_token_stream(), use it here and callNextMethod() for 
  # partition- and subcorpus-method
  cli::cli_progress_step(msg = "decode token stream")
  tokens <- decode(
    0L:(x@size - 1L),
    corpus = x,
    boost = TRUE,
    p_attributes = p_attribute
  )
  
  if (isTRUE(lowercase)){
    cli::cli_progress_step(msg = "lowercase token stream")
    tokens <- tolower(tokens)
  }
  
  if (is.numeric(sample)){
    cli::cli_progress_step(msg = "draw sample")
    tokens <- sample(tokens, size = sample)
    gc()
  }
  
  cli::cli_progress_step(msg = "split into characters")
  chars <- unlist(strsplit(tokens, "", fixed = TRUE), recursive = FALSE)
  
  cli::cli_progress_step(msg = "tabulate")
  n <- table(chars)
  
  cli::cli_progress_step(msg = "apply regular expression")
  if(!is.null(char_regex)) n <- n[grep(char_regex, names(n))]
  
  cli::cli_progress_step(msg = "order result")
  y <- n[order(n, decreasing = decreasing)]
  
  setNames(as.integer(y), names(y))
})