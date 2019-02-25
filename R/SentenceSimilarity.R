#' Get similar sentences.
#'
#' @field partition a partition
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{getSentenceVectors(method = c("pos", "openNLP"), f = mean, stopwords = tm::stopwords("de"), tfidf = FALSE, verbose = TRUE)}}{}
#'   \item{\code{}}{}
#' }
#' 
#' @examples
#' \dontrun{
#' library(R6)
#' library(NLP)
#' library(openNLP)
#' library(openNLPmodels.de)
#' library(polmineR)
#' 
#' filename <- "/Users/blaette/Lab/tmp/word2vec/word2vec/data/plprbt_word2vec.csv"
#' W2V <- Word2Vec$new()
#' W2V$load(filename)
#' merkel <- partition("PLPRBT", speaker_name = "Angela Merkel", speaker_year = "2015")
#' S <- SentenceSimilarity$new()
#' S$partition <- merkel
#' S$word2vec <- W2V$vector_matrix
#' S$threads <- 3
#' S$getSentenceVectors(method = "pos", f = sum)
#' S$getSimilarityMatrix()
#' 
#' i <- 549
#' S$sentences[[i]]
#' sentence <- c("Kriminelle", "Ausländer", "raus", "und", "zwar", "schnell")
#' S$getSentenceSimilarity(sentence)
#' }
#' @export SentenceSimilarity
SentenceSimilarity <- R6Class(
  
  "SentenceSimilarity",
  
  
  public = list(
    
    partition = NULL,
    sentences = NULL,
    word2vec = NULL,
    sentence_vectors = NULL,
    sentence_similarity_matrix = NULL,
    threads = 1,
    
    getSentenceVectors = function(method = c("pos", "openNLP"), f = mean, stopwords = tm::stopwords("de"), tfidf = FALSE, verbose = TRUE){
      
      if (method == "pos"){
        
        text_word <- getTokenStream(self$partition, "word")
        text_pos <- getTokenStream(self$partition, "pos")
        sentence_index <- as.integer(cut(1:length(text_word), breaks = c(0, grep("\\$[\\.!?]", text_pos))))
        no_sentences <- length(sentence_index)
        
        self$sentences <- lapply(split(text_word, sentence_index), function(x) paste(x, collapse = " "))
        
        word_available <- text_word %in% rownames(self$word2vec)
        text_word <- text_word[word_available]
        sentence_index <- sentence_index[word_available]
        
        
        if (is.vector(stopwords)){
          words_to_keep <- which(!tolower(text_word) %in% stopwords)
          text_word <- text_word[words_to_keep]
          sentence_index <- sentence_index[words_to_keep]
        }
        
        if (tfidf){
          words_unique <- unique(text_word)
          word_index <- setNames(1:length(words_unique), words_unique)
          mat <- simple_triplet_matrix(
            i = word_index[text_word],
            j = sentence_index,
            v = rep(1, times = length(text_word)),
            dimnames = list(names(word_index), as.character(1:max(sentence_index)))
          )
          mat$v <- mat$v/row_sums(mat)[mat$i] * log2(ncol(mat)/col_sums(mat > 0))[mat$j]  
          tfidf_weights <- sapply(split(mat$v, mat$i), mean)
          names(tfidf_weights) <- names(word_index)
        }
        
        M <- self$word2vec[text_word,]
        sentence_vectors <- pbapply::pblapply(
          split(M, sentence_index),
          function(tab) apply(matrix(tab, ncol = ncol(self$word2vec)), 2, FUN = f),
          cl = self$threads
        )
        
      } else if (method == "openNLP"){
        
        text <- getTokenStream(self$partition, "word", collapse = " ", beautify = TRUE)
        text <- as.String(text)
        
        sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = "de")
        word_token_annotator <- Maxent_Word_Token_Annotator(language = "de")
        
        boundaries <- annotate(
          text,
          list(sentence_token_annotator, word_token_annotator)
        )
        self$sentences <- text[
          annotations_in_spans(subset(boundaries, type == "word"), subset(boundaries, type == "sentence"))
          ]
        sentence_vectors <- pbapply::pblapply(
          self$sentences,
          self$getSentenceVector,
          cl = self$threads
        )
      }
      self$sentence_vectors <- do.call(rbind, sentence_vectors)
    },
    
    getSentenceVector = function(sentence){
      words <- sentence[sentence %in% rownames(self$word2vec)]
      if (length(words) == 0){
        return( matrix( rep(0, times = ncol(self$word2vec)), nrow = 1) )
      } else {
        vector_matrix <- self$word2vec[words,]
        if (is.vector(vector_matrix)) vector_matrix <- matrix(vector_matrix, nrow = 1)
        return( apply(vector_matrix, 2, mean) )
      }
    },

    getSentenceSimilarity = function(x, n = NULL, text = TRUE){
      if (is.character(x)){
        sentenceToCompare <- self$getSentenceVector(x)
        similarity <- proxy::simil(x = self$sentence_vectors, y = matrix(sentenceToCompare, ncol = ncol(self$word2vec)), method = "cosine")
      } else if (is.numeric(x)){
        similarity <- self$sentence_similarity_matrix[i, ]
      }
      if (is.null(n)) n <- length(similarity)
      newOrder <- order(similarity, decreasing = TRUE)[1:n]
      dt <- data.table(similarity = similarity[newOrder])
      if (text){
        dt[["sentence"]] = sapply(newOrder, function(i) paste(self$sentences[i], collapse = " "))
      }
      dt
    },
    
    getSimilarityMatrix = function(){
      self$sentence_similarity_matrix <- as.matrix(
        proxy::simil(self$sentence_vectors, method = "cosine")
      )
    },
    
    getSimilarSentences = function(i, n = 10){
    }
  )
)
