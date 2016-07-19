setOldClass("simple_triplet_matrix")
setGeneric("similarity", function(.Object, ...) standardGeneric("similarity"))

#' get cosine similarity for a sparse matrix
#' 
#' @param .Object a simple_triplet_matrix
#' @param select a simple_triplet_matrix
#' @param method defaults to "cosine", no other method is implemented
#' @param weighting defaults to "tfidf"
#' @param progress logical
#' @param verbose logical
#' @param mc logical, or if numeric, providing number of cores
#' @exportMethod similarity
#' @importFrom slam simple_triplet_matrix
setMethod("similarity", "simple_triplet_matrix", function(.Object, select=NULL, method="cosine", weighting="tfidf", progress=TRUE, verbose=FALSE, mc=FALSE){
  # the sparse matrix needs to be inflated, the alternative is far too slow
  if (verbose) message("... applying weighting algorithm")
  if (weighting %in% c("tfidf")) .Object <- polmineR::weigh(.Object, method=weighting)
  if (verbose) message("... turning sparse matrix into 'ordinary'/conventional matrix")
  xNonsparse <- as.matrix(.Object)
  if (verbose) message("... calculating similarities")
  if (is.null(select)){
    if (verbose) message("... generating select matrix")
    iIndex <- unlist(lapply(c(1:ncol(.Object)), function(i) rep(i, times= i-1)))
    select <- simple_triplet_matrix(
      i=iIndex,
      j=unlist(lapply(c(2:ncol(.Object)), function(i) c(1:(i-1)))),
      v=rep(NA, times=length(iIndex)),
      ncol=ncol(.Object), nrow=ncol(.Object),
      dimnames=list(colnames(.Object), colnames(.Object))
    )
  }
  nValuesMatrix <- length(select$i)
  .cosine <- function(x, ...){
    a <- xNonsparse[,select$i[x]]
    b <- xNonsparse[,select$j[x]]
    cProd <- crossprod(a) * crossprod(b)
    cProdSqrt <- sqrt(cProd)
    crossprod(a, b)/cProdSqrt
  }
  cosineValues <- blapply(c(1:nValuesMatrix), f=.cosine, mc=mc, progress=progress, select=select)
  if (verbose) message("... preparing matrix to be returned")
  similarityMatrix <- select
  similarityMatrix$v <- unlist(cosineValues)
  return(similarityMatrix)
})

setMethod("similarity", "matrix", function(.Object, method="cosine", progress=TRUE, verbose=TRUE, mc=FALSE){
  if (verbose) message("... turning .Object into simple_triplet_matrix")
  slamObject <- slam::as.simple_triplet_matrix(.Object)
  proto <- similarity(.Object=slamObject, select=NULL, weighting=FALSE, method=method, progress=progress, verbose=verbose, mc=mc)
  as.matrix(proxy::as.simil(as.matrix(proto)))
})