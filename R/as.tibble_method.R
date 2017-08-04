#' @rdname as.tibble
setGeneric("as.tibble", function(.Object, ...) standardGeneric("as.tibble"))

#' @importFrom tibble tibble
#' @rdname as.tibble
setMethod("as.tibble", "partition", function(.Object, pAttribute){
  stopifnot(length(pAttribute) == 1 && pAttribute %in% pAttributes(.Object))
  T <- tibble(token = getTokenStream(.Object, pAttribute = pAttribute))
  colnames(T) <- pAttribute
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
#' tib <- as.tibble(speeches, pAttribute = "word")
#' Y <- ddply(
#'   .data = tib,
#'   .variable = .(speaker_type, speaker_date, speaker_name, partition_name),
#'   .fun = function(x) paste(x[["word"]], collapse = " "),
#'   .progress = "text"
#'   )
#' colnames(Y)[5] <- "fulltext"
#' }
#' @rdname as.tibble
setMethod("as.tibble", "partitionBundle", function(.Object, pAttribute, mc = getOption("polmineR.mc")){
  if (mc == FALSE) mc <- 1
  Ts <- pblapply(
    .Object@objects,
    function(.Object) as.tibble(.Object, pAttribute = pAttribute),
    cl = mc
    )
  T <- dplyr::bind_rows(Ts)
  T
})
