setGeneric("binarize", function(.Object, ...) standardGeneric("binarize"))

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