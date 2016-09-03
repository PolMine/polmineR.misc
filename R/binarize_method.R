#' @include polmineR.misc_package.R
NULL

#' Binarize Matrix
#' 
#' An input matrix (class matrix or simple_triplet_matrix) with numeric
#' values will be turned into a binary (logical) matrix. The top values
#' of each column will be assigned TRUE values.
#' 
#' @param .Object input object
#' @param top an integer value, the n values with the highes values that will be TRUE
#' @param reduce logical, whether to drop row labels of a simple_triplet_matrix with no TRUE value in the matrix
#' @param verbose logical
#' @rdname binarize
#' @exportMethod binarize
setGeneric("binarize", function(.Object, ...) standardGeneric("binarize"))


#' @rdname binarize
setMethod("binarize", "simple_triplet_matrix", function(.Object, top, reduce = TRUE, mc = FALSE, progress = TRUE, verbose = TRUE){
  vList <- split(.Object$v, .Object$j)
  if (verbose) message("... creating indices")
  indices <- blapply(
    c(1:length(vList)),
    function(i, ...) order(vList[[i]], decreasing = T)[1:top],
    mc = mc, progress = progress, verbose = FALSE
    )
  if (verbose) message("... applying indices")
  iList <- split(.Object$i, .Object$j)
  iListBin <- lapply(c(1:length(iList)), function(i) iList[[i]][indices[[i]]])
  if (verbose) message("... preparing simple_triplet_matrix")
  M <- slam::simple_triplet_matrix(
    i = unlist(iListBin),
    j = unlist(lapply(1:ncol(.Object), function(i) rep(i, times=top))),
    v = rep(1, times = ncol(.Object) * top),
    dimnames = dimnames(.Object),
    nrow = nrow(.Object), ncol = ncol(.Object)
  )
  if (reduce == TRUE){
    if (verbose) message("... dropping column labels not used")
    termsThatOccur <- unique(M$i)
    newIndex <- setNames(c(1:length(termsThatOccur)), as.character(termsThatOccur))
    M <- slam::simple_triplet_matrix(
      i = newIndex[as.character(M$i)],
      j = M$j, v = M$v,
      dimnames = list(dimnames(M)[[1]][termsThatOccur], dimnames(M)[[2]]),
      nrow = length(termsThatOccur), ncol = ncol(M)
    )
  }
  M
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