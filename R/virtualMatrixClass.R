#' virtualMatrixClass
#' 
#' This is a virtual class to make it possible do define methods with uniform 
#' behavior for the classes of the virtualMatrixClass.
#' @param chunks number of chunks to generate when splitting up an object for processing it in parallel
#' @param finalizer a list of functions that will be applied to the return value before the respective object is returned
#' @aliases simple_triplet_matrix-class virtualMatrixClass-class
#' @name virtualMatrixClass
#' @rdname virtualMatrixClass
#' @importClassesFrom bigmemory big.matrix
NULL


# setClassUnion(
#   name = "virtualMatrixClass",
#   members = c("matrix", "simple_triplet_matrix", "big.matrix")
# )

# setMethod("blapply", "virtualMatrixClass", function(x, f, chunks = 2, finalizer = list(as.matrix, as.dist, as.matrix), progress = TRUE, verbose = FALSE, mc = FALSE, ...){
#   chunkList <- polmineR:::divide(1:ncol(x), n=chunks)
#   chunkCombinations <- t(cbind(utils::combn(1:chunks, 2), matrix(rep(1:chunks, each=2), nrow = 2)))
#   .worker <- function(i, FUN, M, chunkList, chunkCombinations, ...){
#     furtherArgs <- list(...)
#     for (x in c("progress", "mc", "verbose")) furtherArgs[[x]] <- NULL
#     rowsToProcess <- chunkList[[ chunkCombinations[i,1] ]]
#     colsToProcess <- chunkList[[ chunkCombinations[i,2] ]]
#     almost <- do.call(
#       what = FUN,
#       args = c(list(x = as.matrix(M[,rowsToProcess]), y = as.matrix(M[,colsToProcess]), furtherArgs))
#     )
#     toReturn <- t(apply(almost, 1, function(row) row))
#     colnames(toReturn) <- as.character(colsToProcess)
#     rownames(toReturn) <- as.character(rowsToProcess)
#     data.table::melt(toReturn)
#   }
#   resultList <- blapply(
#     x = c(1:nrow(chunkCombinations)),
#     f = .worker,
#     M = x, FUN = f,
#     chunkList = chunkList, chunkCombinations = chunkCombinations,
#     progress = progress, mc = mc, verbose = FALSE,
#     ...
#   )
#   result <- do.call(rbind, resultList)
#   retval <- as.matrix(slam::simple_triplet_matrix(i=result$Var2, j = result$Var1, v = result$value))
#   for (i in 1:length(finalizer)) retval <- finalizer[[i]](retval)
#   retval
# })