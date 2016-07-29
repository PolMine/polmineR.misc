setOldClass("simple_triplet_matrix")


.cosine <- function(i, X, Y, ...){
  a <- X[,Y$i[i]]
  b <- X[,Y$j[i]]
  cProd <- crossprod(a) * crossprod(b)
  cProdSqrt <- sqrt(cProd)
  crossprod(a, b) / cProdSqrt
}


#' Calculate Jaccard Distance
#' 
#' Fast way by using matrix multiplication.
#' 
#' http://stats.stackexchange.com/questions/49453/calculating-jaccard-or-other-association-coefficient-for-binary-data-using-matri
#' @param X input matrix
#' @export
jaccard_distance <- function(X){
  Xint <- matrix(as.integer(X), ncol = ncol(X))
  NOT <- matrix(as.integer(!(X)), ncol=ncol(X))
  A <- crossprod(Xint)
  D <- crossprod(NOT)
  Jsimil <- A / (nrow(X) - D)
  proxy::pr_simil2dist(Jsimil)
}


setGeneric("similarity", function(.Object, ...) standardGeneric("similarity"))

#' Similarity Calculations for Matrices
#'
#' @param .Object a simple_triplet_matrix
#' @param select a simple_triplet_matrix
#' @param method defaults to "cosine", no other method is implemented
#' @param progress logical
#' @param verbose logical
#' @param mc logical, or if numeric, providing number of cores
#' @exportMethod similarity
#' @importFrom slam simple_triplet_matrix
#' @importFrom proxy as.simil
setMethod("similarity", "virtualMatrixClass", function(.Object, chunks = 1, select=NULL, method="cosine", progress=TRUE, verbose=TRUE, mc=FALSE){
  if (chunks == 1){
    if (is.null(select)){
      combinations <- utils::combn(1:ncol(.Object), 2)
      select <- slam::simple_triplet_matrix(
        i = combinations[1,],
        j = combinations[2,],
        v = rep(FALSE, times = ncol(combinations))
        dimnames = list(colnames(.Object), colnames(.Object))
        )
    }
    if (verbose) message("... calculating similarities")
    cosineValues <- blapply(
      c(1:length(select$i)),
      f=.cosine,
      X=.Object, Y=select,
      mc=mc, progress=progress, select=select
    )
    if (verbose) message("... preparing matrix to be returned")
    select$v <- unlist(cosineValues)
    return(proxy::as.simil(as.matrix(select)))
  } else if (chunks > 1){
    M <- blapply(
      .Object, f = proxy::simil,
      method = method, chunks = chunks,
      progress = progress, mc = mc, verbose = FALSE
      )
    return(proxy::as.simil(M))
  }
})

