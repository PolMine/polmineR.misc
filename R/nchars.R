#' Count the number of characters
#' 
#' @param x Object to process.
#' @param progress A `logical` value.
#' @param regexCharsToKeep if not NULL, a regex
#' @param toLower whether to lower tokens
#' @param mc logical
#' @param ... Argument passed into `blapply()`.
#' @param decreasing logical, passed into order call 
#' @exportMethod nchars
#' @rdname nchars
setGeneric("nchars", function(x, ...) standardGeneric("nchars"))


#' @param p_attribute the p-attribute
#' @param regexCharsToKeep if NULL, counts for all charactrs will be returned, else a regex indicating which characters to include in the counting
#' @param toLower whether to lower tokens
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
setMethod("nchars", "partition", function(x, p_attribute = "word", regexCharsToKeep = "[a-zA-Z]", toLower = TRUE, decreasing = TRUE){
  .Object <- x
  charSoup <- get_token_stream(.Object, p_attribute = p_attribute, collapse = "")
  if (isTRUE(toLower)) charSoup <- tolower(charSoup)
  charCount <- table(unlist(strsplit(charSoup, "")))
  if(!is.null(regexCharsToKeep)){
    charCount <- charCount[grep(regexCharsToKeep, names(charCount))]
  }
  y <- charCount[order(charCount, decreasing = decreasing)]
  setNames(as.integer(y), names(y))
})

#' @rdname nchars
setMethod("nchars", "subcorpus", function(x, p_attribute = "word", regexCharsToKeep = "[a-zA-Z]", toLower = TRUE, decreasing = TRUE){
  nchars(
    x = as(x, "partition"),
    p_attribute = p_attribute,
    regexCharsToKeep = regexCharsToKeep,
    toLower = toLower,
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

#' @param sample An `integer` or `numeric` value defining the number of sample
#'   tokens extracted from the (entirely decoded) token stream to be evaluated.
#' @rdname nchars
#' @importFrom polmineR decode
#' @importFrom stringi stri_count_fixed stri_opts_fixed
#' @examples
#' library(polmineR)
#' use("RcppCWB")
#' n <- corpus("REUTERS") %>% nchars(sample = 4000)
setMethod("nchars", "corpus", function(x, p_attribute = "word", toLower = TRUE, sample = 5000000L, regexCharsToKeep = "[a-zA-Z]", decreasing = TRUE, mc = FALSE, progress = TRUE){
  # optime get_token_stream(), use it here and callNextMethod() for 
  # partition- and subcorpus-method
  tokens <- decode(
    0L:(x@size - 1L),
    corpus = x,
    boost = TRUE,
    p_attributes = p_attribute
  )
  if (isTRUE(toLower)) tokens <- tolower(tokens)
  if (is.numeric(sample)){
    tokens <- sample(tokens, size = sample)
    gc()
  }
  
  n <- table(unlist(strsplit(tokens, "")))
  if(!is.null(regexCharsToKeep)) n <- n[grep(regexCharsToKeep, names(n))]
  
  y <- n[order(n, decreasing = decreasing)]
  setNames(as.integer(y), names(y))
})