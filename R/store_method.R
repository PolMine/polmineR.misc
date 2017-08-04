#' @exportMethod store
setGeneric("store", function(object, ...) standardGeneric("store"))


#' store mallet object
#' 
#' @rdname as.mallet-method
#' @importClassesFrom rJava jobjRef
setMethod("store", "jobjRef", function(object, filename = NULL){
  if (require("rJava", quietly = TRUE)){
    message("... rJava-package loaded")
  } else {
    warning("rJava package not available")
    stop()
  }
  if (is.null(filename)) filename <- tempfile()
  fileOutputStream <- new(rJava::J("java/io/FileOutputStream"), filename)
  objectStream <- new(rJava::J("java/io/ObjectOutputStream"), fileOutputStream)
  objectStream$writeObject(object)
  objectStream$close()
  filename
})






