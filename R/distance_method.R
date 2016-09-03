#' Calculate Distance
#' 
#' Accelerated way using matrix multiplication for jaccard distance computation.
#' http://stats.stackexchange.com/questions/49453/calculating-jaccard-or-other-association-coefficient-for-binary-data-using-matri
#' @param x input matrix
#' @param method defaults to "jaccard", in fact, this is the only method that is implemented
#' @return object of class \code{dist}
#' @exportMethod distance
setGeneric("distance", function(x, ...) standardGeneric("distance"))

#' @rdname distance
setMethod("distance", "matrix", function(x, method = "jaccard", verbose = TRUE){
  stopifnot(method %in% c("jaccard"))
  if (method == "jaccard"){
    if (is.logical(x)) x <- matrix(as.integer(x), ncol = ncol(x))
    NOT <- matrix(as.integer(!x), ncol = ncol(x))
    if (verbose) message("... crossproduct for matrix")
    A <- crossprod(x)
    if (verbose) message("... crossproduct for inverse matrix")
    D <- crossprod(NOT)
    if (verbose) message("... division")
    Jsimil <- A / (nrow(x) - D)
    distMatrix <- proxy::pr_simil2dist(Jsimil)
    retval <- as.dist(distMatrix)
    names(retval) <- colnames(x)
    return(retval)
  }
})

#' @rdname distance
setMethod("distance", "simple_triplet_matrix", function(x, method = "jaccard", verbose = TRUE){
  if (verbose) x <- as.matrix(x)
  distance(x, method = method, verbose = verbose)
})