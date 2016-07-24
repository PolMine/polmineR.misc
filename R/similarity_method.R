setOldClass("simple_triplet_matrix")

#' @importClassesFrom bigmemory big.matrix
setClassUnion(name="matrix_virtual_class", members=c("matrix", "big.matrix", "simple_triplet_matrix"))

.symMatrixTemplate <- function(x, diag = FALSE, verbose = TRUE){
  redux <- ifelse(diag, 0, 1)
  if (verbose) message("... generating select matrix")
  iIndex <- unlist(lapply(c(1:x), function(i) rep(i, times = i - redux)))
  slam::simple_triplet_matrix(
    i=iIndex,
    j=unlist(lapply(c((1 + redux):x), function(i) c(1:(i - redux)))),
    v=rep(NA, times=length(iIndex)),
    ncol=x, nrow=x
  )
}


.cosine <- function(i, X, Y, ...){
  a <- X[,Y$i[i]]
  b <- X[,Y$j[i]]
  cProd <- crossprod(a) * crossprod(b)
  cProdSqrt <- sqrt(cProd)
  crossprod(a, b) / cProdSqrt
}


setGeneric("similarity", function(.Object, ...) standardGeneric("similarity"))

# #' get cosine similarity for a matrix
# #' 
# #' @param .Object a simple_triplet_matrix
# #' @param select a simple_triplet_matrix
# #' @param method defaults to "cosine", no other method is implemented
# #' @param weighting defaults to "tfidf"
# #' @param progress logical
# #' @param verbose logical
# #' @param mc logical, or if numeric, providing number of cores
# #' @exportMethod similarity
# #' @importFrom slam simple_triplet_matrix
# setMethod("similarity", "simple_triplet_matrix", function(.Object, select=NULL, method="cosine", weighting="tfidf", progress=TRUE, verbose=FALSE, mc=FALSE){
#   if (verbose) message("... applying weighting algorithm")
#   if (weighting %in% c("tfidf")) .Object <- polmineR::weigh(.Object, method=weighting)
#   # the sparse matrix needs to be inflated, the alternative is far too slow
#   if (verbose) message("... turning sparse matrix into 'ordinary'/conventional matrix")
#   xNonsparse <- as.matrix(.Object)
#   similarity(.Object=x, select=select, method=method, progress=progress, mc=mc, verbose=verbose)
# })

#' importFrom proxy as.simil
setMethod("similarity", "matrix_virtual_class", function(.Object, chunks = 1, select=NULL, method="cosine", progress=TRUE, verbose=TRUE, mc=FALSE){
  if (chunks == 1){
    if(is.null(select)){
      select <- .symMatrixTemplate(x = ncol(.Object), diag = FALSE, verbose = verbose)  
      select$dimnames <- list(colnames(.Object), colnames(.Object))
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
    colChunks <- divide(1:ncol(.Object), n=chunks)
    chunkMatrix <- .symMatrixTemplate(length(colChunks), diag = TRUE)
    if (verbose) message("... similarity calculation")
    similList <- blapply(
      x = c(1:length(chunkMatrix$i)),
      f = function(i, M, colChunks, chunkMatrix, ...){
        diag <- ifelse(chunkMatrix$i[i] == chunkMatrix$j[i], TRUE, FALSE)
        S <- proxy::simil(
          x = as.matrix(M[,colChunks[[chunkMatrix$i[i]]]]),
          y = as.matrix(M[,colChunks[[chunkMatrix$j[i]]]]),
          method="cosine", by_rows=FALSE
        )
        S2 <- apply(S, 1, function(x) x)
        dimnames(S2) <- dimnames(S)
        reshape2::melt(S2)
      },
      M = .Object, colChunks = colChunks, chunkMatrix = chunkMatrix,
      progress=progress, mc=mc, verbose=FALSE
      )
    SIMIL <- do.call(rbind, similList)
    F <- reshape2::acast(SIMIL, Var1 ~ Var2, value.var="value")
    proxy::as.simil(F)
  }
})