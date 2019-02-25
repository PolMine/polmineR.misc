#' Use word2vec.
#' 
#' http://dsnotes.com/articles/glove-enwiki
#' 
#' @examples 
#' \dontrun{
#' filename <- "/Users/blaette/Lab/tmp/word2vec/word2vec/data/plprbt_word2vec.csv"
#' W2V <- Word2Vec$new()
#' }
#' @export Word2Vec
Word2Vec <- R6Class(
  
  classname = "Word2vec",
  
  public = list(
    
    vector_matrix = NULL, # matrix
    
    load = function(filename, method = 1){
      if (method == 1){
        word2vecRawChar <- scan(filename, what = character(), sep = "\n")
        # the first line states the dimensions: get it, and remove it
        matrixDims <- as.integer(strsplit(word2vecRawChar[1], "\\s")[[1]])
        message("Vector length: ", matrixDims[2])
        message("Vocabulary: ", matrixDims[1])
        word2vecRawChar <- word2vecRawChar[-1]
        splitted <- strsplit(word2vecRawChar, " ")
        rownames <- sapply(splitted, function(row) row[1])
        self$vector_matrix <- do.call(rbind, lapply(splitted, function(row) as.numeric(row[2:length(row)])))
        rownames(self$vector_matrix) <- rownames
      } else {
        matrixDims <- as.integer(strsplit(readLines(con = file(filename), n = 1), "\\s")[[1]])
        message("Vector length: ", matrixDims[2])
        message("Vocabulary: ", matrixDims[1])
        word2vecRaw <- read.table(file = filename, header = FALSE, skip = 1, sep = " ", row.names = 1)
        self$vector_matrix <- as.matrix(word2vecRaw[,1:matrixDims[2]])
        colnames(self$vector_matrix) <- paste("V", 1:matrixDims[2], sep = "")
      }
      invisible(self$vector_matrix)
    },
    
    similarity = function(x, n = 50){
      similarities <- proxy::simil(self$vector_matrix, matrix(self$vector_matrix[x,], nrow = 1), method = "cosine")[,1]
      similarities2 <- similarities[order(similarities, decreasing = TRUE)[2:(n + 1)]]
      data.frame(
        word = names(similarities2), 
        similarity = unname(similarities2)
      )
    }
  )
)
