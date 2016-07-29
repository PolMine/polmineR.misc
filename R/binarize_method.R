#' Binarize Matrix
#' 
#' An input matrix (class matrix or simple_triplet_matrix) with numeric
#' values will be turned into a binary (logical) matrix. The top values
#' of each column will be assigned TRUE values.
#' 
#' @param .Object input object
#' @param top an integer value, the n values with the highes values that will be TRUE
#' @param verbose logical
#' @rdname binarize
setGeneric("binarize", function(.Object, ...) standardGeneric("binarize"))


#' @rdname binarize
setMethod("binarize", "simple_triplet_matrix", function(.Object, top, verbose = TRUE){
  vList <- split(.Object$v, .Object$j)
  indices <- lapply(c(1:length(vList)), function(i) order(vList[[i]], decreasing = T)[1:top])
  
  iList <- split(.Object$i, .Object$j)
  iListBin <- lapply(c(1:length(iList)), function(i) iList[[i]][indices[[i]]])
  M <- slam::simple_triplet_matrix(
    i = unlist(iListBin),
    j = unlist(lapply(1:ncol(.Object), function(i) rep(i, times=top))),
    v = rep(1, times = ncol(.Object) * top),
    dimnames = dimnames(.Object),
    nrow = nrow(.Object), ncol = ncol(.Object)
  )
})


#' @rdname binarize
setMethod("binarize", "matrix", function(.Object, top, verbose = TRUE){
  retval <- apply(
    .Object, 2,
    function(x){
      vect <- rep(FALSE, times = length(x))
      vect[order(x, decreasing = T)[1:top]] <- TRUE
      vect
    })
  dimnames(retval) <- dimnames(.Object)
  retval
})