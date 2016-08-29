#' Analyse hits
#' 
#' A story to be told.
#' 
#' @importFrom R6 R6Class
#' @rdname Hitanalysis
#' @aliases Hitanalysis
Hitanalysis <- R6Class(
  
  "Hitanalysis",
  
  public = list(
    
    hits = NULL,
    bundle = NULL,
    labels = NULL,
    categories = c("TRUE"=FALSE, "FALSE"=FALSE),
    comments = NULL,
    
    initialize = function(hits, bundle=NULL){
      self$hits <- hits
      self$labels <- as.character(rep(NA, times=nrow(hits@dt)))
      self$comments <- as.character(rep(NA, times=nrow(hits@dt)))
    },
    
    read = function(i, sAttribute=NULL, meta=NULL, highlight=NULL, type="plpr", cqp=TRUE){
      
      "documentation for the read method"
      
      if (!is.null(sAttribute)){
        partitionToShow <- partition(
          self$hits@corpus,
          def=as.list(setNames(self$hits@dt[[sAttribute]][i], sAttribute)),
          type=type,
          verbose=FALSE
        )
      } else {
        partitionToShow <- self$bundle[[self$hits[["partition"]][i]]]
      }
      read(.Object=partitionToShow, meta=meta, highlight=highlight, cqp=cqp, verbose=FALSE)
    },
    
    label = function(categories=NULL, closed=TRUE, sAttribute=NULL, type="plpr", highlight=NULL, continue=FALSE, cqp=FALSE){
      if (is.null(categories)){
        categories <- self$categories
      } else {
        self$categories <- categories
      }
      if (closed == FALSE) categories <- c(categories, "assign new label"=TRUE)
      
      if (continue == TRUE){
        i <- last(which(!is.na(self$labels)))
      } else {
        i <- 0 
      }
      
      while(TRUE){
        i <- i + 1
        message("QUERY: ", self$hits@dt[i, query])
        if (!is.null(highlight)){
          toHighlight <- c(highlight, list(yellow=self$hits@dt[["query"]][i]))
        } else {
          toHighlight <- list(yellow=self$hits@dt[["query"]][i])
        }
        self$read(i, sAttribute=sAttribute, type=type, highlight=toHighlight, cqp=cqp)
        userInput <- select.list(names(categories))
        if (userInput != ""){
          if (userInput == "assign new label"){
            userInput <- readline("new label: ")
            categories <- c(categories, setNames(TRUE, userInput))
            self$categories <- categories[-grep("assign new label", names(categories))]
          }
          self$labels[i] <- userInput
          if (categories[grep(userInput, names(categories))] == TRUE){
            self$comments[i] <- readline("comment: ")
          }
        } else {
          break
        }
      }
    }
  )
)