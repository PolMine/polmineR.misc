#' Count the number of characters
#' 
#' @param x Object to process.
#' @param progress A `logical` value.
#' @param regexCharsToKeep if not NULL, a regex
#' @param toLower whether to lower tokens
#' @param mc logical
#' @param ... parameters that will be passed
#' @param decreasing logical, passed into order call 
#' @exportMethod nchars
#' @rdname nchars
setGeneric("nchars", function(x, ...) standardGeneric("nchars"))


#' count characters
#' 
#' @param p_attribute the p-attribute
#' @param regexCharsToKeep if NULL, counts for all charactrs will be returned, else a regex indicating which characters to include in the counting
#' @param toLower whether to lower tokens
#' @param mc logical
#' @param ... parameters that will be passed
#' @param decreasing logical, passed into order call 
#' @exportMethod nchars
#' @rdname nchars
setMethod("nchars", "partition", function(x, p_attribute = "word", regexCharsToKeep = "[a-zA-Z]", toLower = TRUE, decreasing = TRUE, ...){
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
setMethod("nchars", "partition_bundle", function(x, decreasing = TRUE, mc = FALSE, progress = TRUE, ...){
  .Object <- x
  partitionCount <- blapply(.Object@objects, f = nchars, mc = mc, progress = progress, ...)
#   if (mc == FALSE){
#     partitionCount <- lapply(.Object@objects, function(x) nchars(x, ...))
#   } else {
#     partitionCount <- mclapply(
#       .Object@objects,
#       function(x) nchars(x, ...),
#       mc.cores=ifelse(mc == TRUE, getOption("polmineR.cores"))
#       )
#   }
  charCount <- tapply(
    unname(unlist(partitionCount)),
    INDEX=unlist(sapply(partitionCount, function(x) names(x))),
    FUN=sum
  )
  charCount[order(charCount, decreasing=decreasing)]
})