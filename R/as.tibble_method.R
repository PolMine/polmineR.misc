#' @rdname as.tibble
setGeneric("as.tibble", function(.Object, ...) standardGeneric("as.tibble"))

#' @param .Object An object the method is defined for.
#' @param p_attribute A positional attribute / p-attribute.
#' @param ... Further arguments.
#' @param mc Number of cores to use.
#' @importFrom tibble tibble
#' @rdname as.tibble
setMethod("as.tibble", "partition", function(.Object, p_attribute){
  stopifnot(length(p_attribute) == 1 && p_attribute %in% p_attributes(.Object))
  T <- tibble(token = getTokenStream(.Object, p_attribute = p_attribute))
  colnames(T) <- p_attribute
  for (x in names(.Object@sAttributes)) T[[x]] <- .Object@sAttributes[[x]]
  T[["partition_name"]] <- .Object@name
  T
})

#' Turn partition into tibble.
#' 
#' @importFrom dplyr bind_rows
#' @importFrom pbapply pblapply
#' @examples
#' \dontrun{
#' library(polmineR)
#' library(tibble)
#' library(plyr)
#' library(dplyr)
#' library(pbapply)
#' 
#' plprbt <- partition("PLPRBT", speaker_type = "speech", speaker_year = "2010")
#' speeches <- as.speeches(
#'   plprbt,
#'   sAttributeDates = "speaker_date", sAttributeNames = "speaker_name", gap = 500,
#'   mc = TRUE, progress = TRUE
#' )
#' for (i in rev(which(summary(speeches)[["token"]] == 0))) speeches@@objects[[i]] <- NULL
#' tib <- as.tibble(speeches, p_attribute = "word")
#' Y <- ddply(
#'   .data = tib,
#'   .variable = .(speaker_type, speaker_date, speaker_name, partition_name),
#'   .fun = function(x) paste(x[["word"]], collapse = " "),
#'   .progress = "text"
#'   )
#' colnames(Y)[5] <- "fulltext"
#' }
#' @rdname as.tibble
setMethod("as.tibble", "partition_bundle", function(.Object, p_attribute, mc = getOption("polmineR.mc")){
  if (mc == FALSE) mc <- 1
  Ts <- pblapply(
    .Object@objects,
    function(.Object) as.tibble(.Object, p_attribute = p_attribute),
    cl = mc
    )
  T <- dplyr::bind_rows(Ts)
  T
})
