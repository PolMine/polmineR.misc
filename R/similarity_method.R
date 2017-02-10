#' Calculate cosine similarity.
#' 
#' Calculate cosine similarity of two vectors.
#' Note: Much faster for pairwise comparisons than proxy::simil.
#' @param i integer index, the n-th position of the simple triplet matrix Y
#' @param X matrix from which the values are taken
#' @param Y simple triplet matrix
#' @param tool 
#' @param ... further parameters (for use with blapply)
#' 
setGeneric("cosine_similarity", function(.Object, ...) standardGeneric("cosine_similarity"))

setMethod("cosine_similarity", "numeric", function(.Object, X, Y, ...){
  a <- as.vector(X[,Y$i[.Object]])
  b <- as.vector(X[,Y$j[.Object]])
  cProd <- crossprod(a) * crossprod(b)
  cProdSqrt <- sqrt(cProd)
  crossprod(a, b) / cProdSqrt
})




#' Similarity Calculations for Matrices.
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
setMethod("cosine_similarity", "simple_triplet_matrix", function(.Object, chunks = 1, select = NULL, tool = "simil", method = "cosine", return.simil = TRUE, progress = TRUE, verbose = TRUE, mc = FALSE){
  stopifnot(is(.Object)[1] %in% c("TermDocumentMatrix", "simple_triplet_matrix", "Matrix"))
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
})

setMethod("cosine_similarity", "matrix", function(.Object, tool = c("proxy", "coop", "handmade"), chunks = 1, progress = TRUE, verbose = TRUE, mc = FALSE){
  if (tool == "proxy"){
    if (chunks > 1){
      M <- blapply(
        .Object, f = proxy::simil,
        method = "cosine", chunks = chunks,
        progress = progress, mc = mc, verbose = FALSE
      )
    } else {
      M <- proxy::simil(.Object, method = "cosine", by_rows = TRUE)
    }
  } else if (tool == "coop"){
    M <- coop::cosine(t(.Object))
  } else if (tool == "handmade"){
    cp <- crossprod(t(.Object))
    rtdg <- sqrt(diag(.Object))
    foo <- tcrossprod(rtdg)
    M <- cp / foo
  }
  proxy::as.simil(M)
})

setMethod("cosine_similarity", "big.matrix", function(.Object){
  M_transposed <- as.big.matrix(t(.Object))
  cp <- .Object %*% M_transposed
  diag <- sapply(1:ncol(cp), function(i) cp[i,i])
  rtdg <- sqrt(diag)
  tcross <- as.big.matrix(matrix(rtdg, ncol = 1)) %*% as.big.matrix(matrix(rtdg, nrow = 1))
  pbapply::pblapply(
    1:nrow(cp),
    function(i) cp[i,] <- cp[i,] / tcross[i,] 
  )
  options(bigmemory.allow.dimnames = TRUE)
  rownames(cp) <- rownames(vec)
  colnames(cp) <- rownames(vec)
  cp
})