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
  charCount[order(charCount, decreasing = decreasing)]
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
  
  charCount[order(charCount, decreasing = decreasing)]
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