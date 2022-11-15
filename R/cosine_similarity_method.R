#' Calculate cosine similarity.
#' 
#' Calculate cosine similarity of two vectors.
#' Note: Much faster for pairwise comparisons than proxy::simil.
#' @param y matrix from which the values are taken
#' @param y simple triplet matrix
#' @param tool 
#' @param method defaults to "cosine", no other method is implemented
#' @param progress logical
#' @param verbose logical
#' @param mc logical, or if numeric, providing number of cores
#' @export cosine_similarity
#' @importFrom slam simple_triplet_matrix
#' @importFrom proxy as.simil
#' @param ... further parameters (for use with blapply)
#' 
#' @rdname cosine_similarity
setGeneric("cosine_similarity", function(x, ...) standardGeneric("cosine_similarity"))

#' @rdname cosine_similarity
setMethod("cosine_similarity", "numeric", function(x, y){
  c_prod <- crossprod(x) * crossprod(y)
  c_prod_sqrt <- sqrt(c_prod)
  as.vector(crossprod(x, y) / c_prod_sqrt)
})

#' @rdname cosine_similarity
setMethod("cosine_similarity", "TermDocumentMatrix", function(x, y = NULL, progress = TRUE, verbose = TRUE, mc = FALSE){

  if (is.null(y)){
    combinations <- utils::combn(1:ncol(x), 2)
    y <- slam::simple_triplet_matrix(
      i = combinations[1,],
      j = combinations[2,],
      v = rep(FALSE, times = ncol(combinations)),
      dimnames = list(colnames(x), colnames(x))
    )
  }
  
  if (verbose) message("... calculating similarities")
  
  cosine_values <- pbapply::pblapply(
    1:length(y$i),
    FUN = function(i) cosine_similarity(as.vector(x[, y$i[i]]), as.vector(x[, y$j[i]])),
    cl = mc
  )

  if (verbose) message("... preparing matrix to be returned")
  
  y$v <- unlist(cosine_values)
  y
})

# setMethod("cosine_similarity", "TermDocumentMatrix", function(x, ...) callNextMethod())

setMethod("cosine_similarity", "matrix", function(x, how = c("proxy", "coop", "algebra"), chunks = 1, progress = TRUE, verbose = TRUE, mc = FALSE){
  stopifnot(how %in% c("proxy", "coop", "algebra"))
  if (how == "proxy"){
    if (chunks > 1){
      M <- blapply(
        x, f = proxy::simil,
        method = "cosine", chunks = chunks,
        progress = progress, mc = mc, verbose = FALSE
      )
    } else {
      M <- proxy::simil(x, method = "cosine", by_rows = TRUE)
    }
  } else if (how == "coop"){
    M <- coop::cosine(t(x))
  } else if (how == "algebra"){
    cp <- crossprod(t(x))
    rtdg <- sqrt(diag(cp))
    foo <- tcrossprod(rtdg)
    M <- cp / foo
  }
  proxy::as.simil(M)
})

#' @import bigmemory
#' @import bigalgebra
#' 
setMethod("cosine_similarity", "big.matrix", function(x){
  stop("defunct as bigpca pkg is not available any more")
  # M_transposed <- bigpca::big.t(x) 
  # cp <- x %*% M_transposed
  # diag <- sapply(1:ncol(cp), function(i) cp[i,i])
  # rtdg <- sqrt(diag)
  # tcross <- as.big.matrix(matrix(rtdg, ncol = 1)) %*% as.big.matrix(matrix(rtdg, nrow = 1))
  # pbapply::pblapply(
  #   1:nrow(cp),
  #   function(i) cp[i,] <- cp[i,] / tcross[i,] 
  # )
  # options(bigmemory.allow.dimnames = TRUE)
  # rownames(cp) <- rownames(x)
  # colnames(cp) <- rownames(x)
  # cp
})
