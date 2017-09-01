#' @examples 
#' \dontrun{
library(data.table)
library(magrittr)
library(xml2)
library(stringi)
library(rJava)
library(pbapply)
library(parallel)
library(readr)
library(text2vec)

filenames <- Sys.glob(sprintf("%s/*.xml", "~/Lab/gitlab/plprbttxt_tei"))
filenames <- filenames[1:2]

metadata <- c(
  lp = "//legislativePeriod",
  session = "//titleStmt/sessionNo",
  date = "//publicationStmt/date"
)
dtList <- pblapply(filenames, function(x) xmlToDT(x, meta = metadata), cl = 10)
dt <- rbindlist(dtList, fill = TRUE)
rm(dtList)

dt2 <- dt[is.na(speaker)][, speaker := NULL] # remove text in speaker tag
dt2[, chunk := 1:nrow(dt2)] # add column with chunks
for (x in c("div_what", "div_desc", "body", "TEI", "p")) dt2[[x]] <- NULL
dt2[["stage_type"]] <- ifelse(is.na(dt2[["stage_type"]]), "speech", "interjection")
rm(dt)

options(java.parameters = "-Xmx4g")
CNLP <- CoreNLP$new(method = "json", filename = "~/Lab/tmp/coreNLP.json")
dummy <- pbapply::pblapply(
  1:nrow(dt2),
  function(i) CNLP$annotate(dt2[["text"]][i], chunk = i) # add chunks for matching with metadata table
)

# processing 1100 plenary protocols ~ 2h 
cores <- 10
chunks <- text2vec::split_into(1:nrow(dt2), n = cores)
system.time(filenames <- mclapply(
  1:length(chunks),
  function(i){
    options(java.parameters = "-Xmx4g")
    filename <- sprintf("~/Lab/tmp/coreNLP_%d.json", i)
    A <- CoreNLP$new(method = "json", filename = filename)
    lapply(chunks[[i]], function(j) A$annotate(dt2[["text"]][j], chunk = j))
    return( filename )
  },
  mc.cores = 10
))

# with parallelization
J <- unlist(lapply(filenames, readr::read_lines))
            
# withou parallelization
J <- readr::read_lines(file = "~/Lab/tmp/coreNLP.json", progress = TRUE)
CNLP <- CoreNLP$new(method = "json", filename = "~/Lab/tmp/coreNLP.json")
dts <- pbapply::pblapply(J, CNLP$parseJson, cl = 10) # parallelization works nicely here
tokenStreamDT <- rbindlist(dts)


tokenStreamDT[, cpos := 0:(nrow(tokenStreamDT) - 1)]
encode(tokenStreamDT[["token"]], corpus = "FOO", pAttribute = "word", encoding = "UTF-8")
encode(tokenStreamDT[["pos"]], corpus = "FOO", pAttribute = "pos")

cpos <- tokenStreamDT[,{list(cpos_left = min(.SD[["cpos"]]), cpos_right = max(.SD[["cpos"]]))}, by = "chunk"]
setkeyv(cpos, cols = "chunk")
setkeyv(dt2, cols = "chunk")
dt3 <- dt2[cpos]
options("polmineR.cwb-regedit" = FALSE)
setnames(dt3, old = c("sp_party", "sp_name"), new = c("party", "name"))
for (col in c("party", "name", "lp", "session", "date")){
  dtEnc <- dt3[, c("cpos_left", "cpos_right", col), with = FALSE]
  encode(dtEnc, corpus = "FOO", sAttribute = col)
}
use()
#' @param filename if filename is not NULL (default), the object will be initialized with
#' a FileWriter, and new annotations will be appended
#' @param colsToKeep character vector with names of columens of the output data.table
#' @importFrom jsonlite fromJSON
#' @importFrom stringi stri_match
CoreNLP <- setRefClass(
  
  "CoreNLP",
  
  fields = list(
    
    tagger = "jobjRef",
    xmlifier = "jobjRef",
    jsonifier = "jobjRef",
    writer = "jobjRef",
    append = "logical",
    method = "character",
    colsToKeep = "character",
    destfile = "character",
    logfile = "character"
    
  ),
  
  methods = list(
    
    initialize = function(
      stanfordDir = NULL, propertiesFile = NULL, 
      method = c("txt", "json", "xml"),
      colsToKeep = c("sentence", "id", "token", "pos", "ner"),
      filename = NULL
    ){
      
      .self$colsToKeep <- colsToKeep
      
      rJava::.jinit(force.init = TRUE) # does it harm when called again?
      
      # add stanford jars to classpath
      if (is.null(stanfordDir)){
        stanfordDir <- file.path(
          system.file("extdata", package = "coreNLP"),
          "stanford-corenlp-full-2015-12-09"
        )
      }
      stanfordPath <- Sys.glob(paste0(stanfordDir,"/*.jar"))
      rJava::.jaddClassPath(stanfordPath)
      
      .self$method <- method
      if (.self$method == "xml") .self$xmlifier <- rJava::.jnew("edu.stanford.nlp.pipeline.XMLOutputter")
      if (.self$method == "json") .self$jsonifier <- rJava::.jnew("edu.stanford.nlp.pipeline.JSONOutputter")
      
      if (is.null(propertiesFile)){
        propertiesFile <- file.path(
          system.file(package = "coreNLP", "extdata"),
          "StanfordCoreNLP-german.properties"
        )
      }
      rJava::.jaddClassPath(dirname(propertiesFile))
      
      .self$tagger <- rJava::.jnew(
        "edu.stanford.nlp.pipeline.StanfordCoreNLP",
        basename(propertiesFile)
      )
      
      if (is.null(filename)){
        .self$append <- FALSE
      } else {
        .self$append <- TRUE
        .self$destfile <- filename
        if (method == "txt"){
          .self$writer <- new(
            J("java.io.PrintWriter"),
            .jnew("java.io.FileOutputStream",
                  .jnew("java.io.File", filename),
                  TRUE)
          )
        }
      }
      
    },
    
    annotationToXML = function(anno){
      doc <- .jcall(.self$xmlifier, "Lnu/xom/Document;", "annotationToDoc", anno, .self$tagger)
      xml <- .jcall(doc, "Ljava/lang/String;", "toXML")
      df <- coreNLP::getToken(coreNLP:::parseAnnoXML(xml))
      colnames(df) <- tolower(colnames(df))
      as.data.table(df[, colsToKeep])
    },
    
    annotationToJSON = function(anno, chunk = NULL){
      jsonString <- .jcall(.self$jsonifier, "Ljava/lang/String;", "print", anno)
      jsonString <- gsub("\\s+", " ", jsonString)
      if (!is.null(chunk)){
        stopifnot(is.numeric(chunk))
        jsonString <- sprintf('{"chunk": %d, %s', chunk, substr(jsonString, 2, nchar(jsonString)))
      }
      cat(jsonString, "\n", file = .self$destfile, append = .self$append)
      if (.self$append == FALSE){
        return(jsonString)
      } else {
        return( NULL )
      }
    },
    
    annotationToTXT = function(anno){
      if (.self$append == FALSE){
        .self$writer <- .jnew("java.io.PrintWriter", filename <- tempfile())
      }
      .jcall(.self$tagger, "V", "prettyPrint", anno, .self$writer)
      # .jmethods(writer, "V", "close")
      if (.self$append == FALSE){
        return( .self$parsePrettyPrint(filename) )
      } else {
        return( NULL )
      }
    },
    
    parseJson = function(x, colsToKeep = c("sentence", "id", "token", "pos", "ner")){
      # run the parsing within try - coding issues may cause problems
      dat <- try( jsonlite::fromJSON(x) )
      if (is(dat)[1] == "try-error") return( NULL )
      dt <- rbindlist(
        lapply(
          1:length(dat$sentences$tokens),
          function(i) as.data.table(dat$sentences$tokens[[i]])[, "sentence" := i]
        )
      )
      if ("chunk" %in% names(dat)){
        dt[, "chunk" := dat[["chunk"]]]
        cols <- c("chunk", colsToKeep)
      } else {
        cols <- colsToKeep
      }
      setnames(dt, old = c("index", "word"), new = c("id", "token"))
      dt[, colsToKeep, with = FALSE]
    },
    
    parsePrettyPrint = function(x = NULL, filename = NULL, mc = 1){
      if (is.null(x)) x <- readLines(filename)
      chunks <- cut(
        1:length(x),
        c(grep("^Sentence\\s#\\d+", x), length(x)),
        include.lowest = TRUE, right = FALSE
      )
      dts <- pbapply:pblapply(
        split(x, f = chunks),
        function(chunk){
          txt <- chunk[grepl("^\\[.*\\]$", chunk)] # get lines with annotation
          regex <- "^.*?Text=(.*?)\\s.*\\sPartOfSpeech=(.*?)\\sNamedEntityTag=(.*?)\\]"
          df <- stringi::stri_match(txt, regex = regex)
          dt <- as.data.table(df)[,2:4, with = FALSE]
          colnames(dt) <- c("token", "pos", "ner")
          dt
        },
        cl = mc
      )
      # rbindlist(dts)
      dts
    },
    
    purge = function(x){
      replacements <- list(
        c("\u202F", ""), # narrow no-break space
        c("\uFFFD", "")
      )
      for (i in 1:length(replacements)){
        x <- gsub(replacements[[i]][1], replacements[[i]][2], x)
      }
      x
    },
    
    annotate = function(txt, chunk = NULL, purge = TRUE){
      if (purge) txt <- .self$purge(txt)
      anno <- rJava::.jcall(.self$tagger, "Ledu/stanford/nlp/pipeline/Annotation;", "process", txt)
      switch(.self$method,
             xml = .self$annotationToXML(anno),
             json = .self$annotationToJSON(anno, chunk = chunk),
             txt = .self$annotationToTXT(anno)
      )
    }
  )
)