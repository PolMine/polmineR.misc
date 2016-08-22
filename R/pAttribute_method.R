# setGeneric("pAttribute", function(object, ...) standardGeneric("pAttribute"))
# setGeneric("pAttribute<-", function(object, value) standardGeneric("pAttribute<-"))



# #' pAttribute 
# #' 
# #' @rdname pAttribute-method
# #' @exportMethod pAttribute
# #' @exportMethod pAttribute<-
# #' @importMethodsFrom polmineR pAttribute
# setMethod("pAttribute", "partition", function(object, from, to, mc=TRUE, verbose=TRUE){
#   if (verbose == TRUE) message("... preparing cpos information")
#   cpos <- unlist(apply(object@cpos, 1, function(x) c(x[1]:x[2])))
#   if (verbose == TRUE) message("... extracting information from corpus")
#   bag <- matrix(
#     data=c(
#       cqi_cpos2id(paste(object@corpus, '.', to, sep=''), cpos),
#       pos=cqi_cpos2id(paste(object@corpus, '.', from, sep=''), cpos)
#     ),
#     ncol=2, nrow=length(cpos), dimnames=list(NULL,c(to, from))
#   )
#   if (verbose == TRUE) message("... splitting data for evaluation")
#   idVectorList <- split(x=bag[,to], f=bag[,from])
#   if (verbose == TRUE) message("... doing calculations")
#   .id2stat <- function(idVector){
#     tabulatedIdVector <- tabulate(idVector + 1)
#     decreasing <- order(tabulatedIdVector, decreasing = TRUE)
#     occurring <- which(tabulatedIdVector > 0)
#     decreasingAndOccurring <- decreasing[which(decreasing %in% occurring)]  
#     shares <- round(tabulatedIdVector[decreasingAndOccurring] / sum(tabulatedIdVector[decreasingAndOccurring]), 2)
#     token <- cqi_id2str(paste(object@corpus, '.', to, sep=''), decreasingAndOccurring - 1)
#     Encoding(token) <- object@encoding
#     setNames(shares, token)
#   }
#   if (mc == FALSE){
#     statList <- lapply(idVectorList, .id2stat)
#   } else {
#     statList <- mclapply(idVectorList, .id2stat)
#   }
#   if (verbose == TRUE) message("... id to string for keys")
#   keyAsString <- cqi_id2str(paste(object@corpus, '.', from, sep=''), as.integer(names(statList)))
#   Encoding(keyAsString) <- object@encoding
#   names(statList) <- keyAsString
#   statList
# })
# 
