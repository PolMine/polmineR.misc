#' Calculate cosine Similarity
#' 
#' Calculate cosine similarity of two vectors.
#' Note: For pairwise comparisons of vectors, this is much faster 
#' than proxy::simil
#' @param i integer index, the n-th position of the simple triplet matrix Y
#' @param X matrix from which the values are taken
#' @param Y simple triplet matrix
#' @param ... further parameters (for use with blapply)
cosine_similarity <- function(i, X, Y, ...){
  a <- as.vector(X[,Y$i[i]])
  b <- as.vector(X[,Y$j[i]])
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


#' Similarity Calculations for Matrices
#'
#' @param .Object a simple_triplet_matrix
#' @param select a simple_triplet_matrix
#' @param method defaults to "cosine", no other method is implemented
#' @param progress logical
#' @param verbose logical
#' @param mc logical, or if numeric, providing number of cores
#' @export similarity
#' @importFrom slam simple_triplet_matrix
#' @importFrom proxy as.simil
similarity <- function(.Object, chunks = 1, select=NULL, method = "cosine", return.simil = TRUE, progress = TRUE, verbose = TRUE, mc = FALSE){
  stopifnot(is(.Object)[1] %in% c("TermDocumentMatrix", "simple_triplet_matrix", "Matrix"))
  if (chunks == 1){
    if (is.null(select)){
      combinations <- utils::combn(1:ncol(.Object), 2)
      select <- slam::simple_triplet_matrix(
        i = combinations[1,],
        j = combinations[2,],
        v = rep(FALSE, times = ncol(combinations)),
        dimnames = list(colnames(.Object), colnames(.Object))
        )
    }
    if (verbose) message("... calculating similarities")
    
    cosineValues <- blapply(
      c(1:length(select$i)),
      f = cosine_similarity,
      X = .Object, Y = select,
      mc = mc, progress = progress
    )
    
    if (verbose) message("... preparing matrix to be returned")
    select$v <- unlist(cosineValues)
    if (return.simil == TRUE){
      return(proxy::as.simil(as.matrix(select)))  
    } else {
      return(select)
    }
  } else if (chunks > 1){
    M <- blapply(
      .Object, f = proxy::simil,
      method = method, chunks = chunks,
      progress = progress, mc = mc, verbose = FALSE
      )
    return(proxy::as.simil(M))
  }
}