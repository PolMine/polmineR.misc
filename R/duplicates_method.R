setGeneric("getDuplicates", function(.Object, ...) standardGeneric("getDuplicates"))

#' @exportMethod getDuplicates
setMethod("getDuplicates", "partitionBundle", function(
  .Object, similarityMatrix, threshold = 0.9, date,
  mc = FALSE, progress = TRUE, verbose = TRUE
  ){
  if (verbose == TRUE) message("... applying threshold")
  if (mc == FALSE) mc <- 1
  dates <- unlist(lapply(
    setNames(.Object@objects, names(.Object)),
    function(x) sAttributes(x, date))
  )
  indexDuplicates <- which(similarityMatrix$v >= threshold)
  if (length(indexDuplicates) > 0){
    # keep only those values in similarity matrix that are above the threshold
    for (x in c("i", "j", "v")) similarityMatrix[[x]] <- similarityMatrix[[x]][indexDuplicates]  
    duplicateList <- lapply(
      c(1:length(similarityMatrix$i)),
      function(i){
        iName <- similarityMatrix$dimnames[[1]][similarityMatrix$i[i]]
        jName <- similarityMatrix$dimnames[[1]] [similarityMatrix$j[i]]
        iDate <- as.POSIXct(dates[[iName]])
        iSize <- .Object@objects[[iName]]@size
        jDate <- as.POSIXct(dates[[jName]])
        jSize <- .Object@objects[[jName]]@size
        value <- similarityMatrix$v[i]
        if(iDate == jDate){
          if (iSize >= jSize){
            return(c(name=iName, date=as.character(iDate), size=iSize, duplicate_name=jName, duplicate_date=as.character(jDate), duplicate_size=jSize, similarity=value))
          } else {
            return(c(name=jName, date=as.character(jDate), size=jSize, duplicate_name=iName, duplicate_date=as.character(iDate), duplicate_size=iSize, similarity=value))
          }
        } else if (iDate < jDate){
          return(c(name=iName, date=as.character(iDate), size=iSize, duplicate_name=jName, duplicate_date=as.character(jDate), duplicate_size=jSize, similarity=value))
        } else if (iDate > jDate){
          return(c(name=jName, date=as.character(jDate), size=jSize, duplicate_name=iName, duplicate_date=as.character(iDate), duplicate_size=iSize, similarity=value))
        }
      })
    duplicateDT <- data.table(do.call(rbind, duplicateList))
    count <- function(x) return(x)
    DT <- duplicateDT[, count(.N), by=.(name, date, size, duplicate_name, duplicate_date, duplicate_size, similarity)][, V1 := NULL]
    DT[, size := as.numeric(size)][, duplicate_size := as.numeric(duplicate_size)][, similarity := as.numeric(similarity)]
    return(DT)
  } else {
    message("... no duplicates found")
    return(NULL)
  }
})

