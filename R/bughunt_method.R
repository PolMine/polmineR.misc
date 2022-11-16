#' find bugs in a corpus
#' 
#' This is utility to trace back the cause of errors that cwb-encode 
#' throws out when XML elements are nested.
#' 
#' @param .Object a character vector providing the name of a CWB corpus
#' @param nested name of an element that is nested in an unwanted fashion 
#' @param ... further parameters
#' @name bughunt
#' @exportMethod bughunt
#' @rdname bughunt-method
setGeneric("bughunt", function(.Object, ...) standardGeneric("bughunt"))

#' @rdname bughunt-method
#' @importFrom RcppCWB cl_struc2cpos cl_cpos2struc cl_struc2str
setMethod("bughunt", "character", function(.Object, nested = c("p"=10), ...){
  lapply(
    as.character(c(1:nested[1])),
    function(x){
      a <- cl_struc2cpos(paste(.Object, ".", names(nested)[1], x, sep=""), 1)
      b <- cl_cpos2struc(paste(.Object, ".text_id", sep=""), a[1])
      beginningNested <- cl_struc2str(paste(.Object, ".text_id", sep=""), b)
      previousSiblingNested <- cl_struc2str(paste(.Object, ".text_id", sep=""), b - 1)
      return(c(beginningNested, previousSiblingNested))
    })
})




